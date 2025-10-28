#' @title Sentiment Analysis using Grasp
#' @description This function performs sentiment analysis on a collection of sentences using the Grasp sentiment analysis model. It computes the polarity of each sentence and returns a table with sentiment scores.
#'
#' @param documents A character vector of document identifiers for which sentiment analysis needs to be performed.
#' @param sentences A data frame containing sentence-level data. This should include columns like `sentenceid`, `sentence`, `document`, `submissionid`, `competencyid`, and `type` (which indicates the type of feedback).
#' @param language A string indicating the language of the sentences to analyze. Supported languages include "nl" (Dutch), "de" (German), and "fr" (French).
#' @param grasp_folder The folder path where the Grasp Python module is located. This is required to run the Grasp sentiment analysis.
#'
#' @return A data table with the following columns:
#'   - `sentenceid`: The ID of the sentence.
#'   - `sentence`: The sentence being analyzed.
#'   - `polarity`: The sentiment polarity score for the sentence.
#'   - `document`: The document identifier to which the sentence belongs.
#'
#' @details
#' The function loads the Grasp sentiment analysis model via `reticulate` and computes sentiment polarity for each sentence. It performs a special handling for Dutch language sentences that start with certain keywords to prepend "suboptimal" to improve sentiment accuracy. The sentiment is calculated using the `pov` function from the Grasp model. To use the sentiment setting for Dutch medical education, copy file `nl_pov.json`, included in this package, and paste it in grasp_folder/lm.
#' @importFrom reticulate py_available source_python import_from_path
#' @keywords internal
#' @export

sentiment_analysis_grasp <- function(documents, sentences, language, grasp_folder){

  check_dataset(sentences, type_of_dataset = "sentences")


  if (missing(grasp_folder) || !dir.exists(grasp_folder)) {
    stop("Invalid grasp_folder path: ", grasp_folder)
  }

  grasp_file <- file.path(grasp_folder, "grasp.py")

  if (!file.exists(grasp_file)) {
    stop("grasp.py not found in folder: ", grasp_folder)
  }

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' must be installed to use this function.", call. = FALSE)
  }
  # Check Python availability
  if (!reticulate::py_available(initialize = FALSE)) {
    warning("Python not available; skipping GRASP loading.")
  }

  # Load and import the module
  try({
    reticulate::py_run_file(grasp_file)
    grasp <- reticulate::import_from_path("grasp", path = grasp_folder)
  }, silent = TRUE)

  if (!exists("grasp")) {
    stop("Failed to import GRASP module from: ", grasp_folder)
  }

  #reticulate::py_run_file(paste(grasp_folder, "\\grasp.py", sep=""))
  #grasp <- reticulate::import_from_path("grasp", path=grasp_folder)

  sentence_polarity <- data.table::data.table("sentenceid" =numeric(), "sentence" = character(), "polarity" = numeric(), "document" = numeric())

  for(cur_doc in documents) {
    # Sentiment per sentence
    filtered_data <- dplyr::filter(sentences, .data$document == cur_doc)

    if(nrow(filtered_data) > 0) {
  	  for(i in 1: nrow(filtered_data)) {
    		submissionid <- filtered_data$submissionid[[i]]
    		competencyid <- filtered_data$competencyid[[i]]
    		sentenceid <- filtered_data$sentenceid[[i]]

    		# Sentiment per sentence
    		polarity <- get_polarity(grasp, filtered_data$sentence[[i]], filtered_data$type[[i]], language)
    		sentence_polarity <- rbind(sentence_polarity, data.table::data.table("sentenceid" = sentenceid, "sentence" = filtered_data$sentence[[i]], "polarity" = polarity, "document" = cur_doc))
  	  }
  	}
  }

  return (sentence_polarity)
}

# ===================================
# get_polarity: Get Polarity from Grasp
# ===================================

#' @title Get Polarity from Grasp
#' @description This function computes the polarity score for a given sentence using the Grasp sentiment analysis model. It applies specific logic based on the comment type (e.g., improvement suggestions) and language (Dutch, English, German, or French).
#'
#' @param grasp The imported Grasp model, which is used to compute sentiment polarity.
#' @param sentence A character string representing the sentence for which sentiment polarity needs to be calculated.
#' @param comment_type A string that indicates the type of feedback (e.g., "verbeter" for improvement). This influences sentiment adjustment in Dutch language.
#' @param language A string indicating the language of the sentence ("nl" for Dutch, "en" for English, "de" for German, "fr" for French).
#'
#' @return A numeric value representing the sentiment polarity of the sentence. Positive values indicate positive sentiment, negative values indicate negative sentiment, and zero indicates neutral sentiment.
#'
#' @details
#' For Dutch sentences, the function applies specific transformations (such as removing certain words like "naar" and "gemaakt") before calculating sentiment. It also handles feedback sentences marked as "verbeter" (improvement suggestions) by adding the word "suboptimal" to adjust the sentiment analysis for improvement-type feedback.
#'
#' @export

get_polarity <- function(grasp, sentence, comment_type, language) {
  # escape: specifically optimized for Dutch
  if(language == "nl") {
    sentence<-stringr::str_replace_all(sentence, "naar", "")
    sentence<-stringr::str_replace_all(sentence, "gemaakt", "")
    sentence<-stringr::str_replace_all(sentence, "laat", "")
    #sentence<-stringr::str_replace_all(sentence, ",", "")
    sentence<-stringr::str_replace_all(sentence, ";", "")
    sentence<-stringr::str_replace_all(sentence, ":", "")
    sentence<-stringr::str_replace_all(sentence, " ' ", " ")

    # erg --> neemt de tijd voor patiënten, die voelen zich daardoor erg gehoord --> neg (--> "gehoord" zit er niet in)
    # te --> houding tijdens gesprek is te amicaal en te weinig met professionele distantie --> pos
    # maar "fijn om mee samen te werken" is terecht positief --> 'te' negatief sentiment geven werkt niet
    # opgelost door combinaties van 'te' met bijv naamwoord sentiment -0.3 te geven
  }

  improve_addition = "suboptimal - "
  if(language == "nl") {
    improve_addition = "suboptimal - "
  } else if(language == "de"){
    improve_addition = "suboptimal - "
  } else if(language == "fr"){
    improve_addition = "sous-optimal - "
  }

  if(comment_type == "verbeter"){
    added = FALSE

    # In Dutch, use the addional text when the feedback is presented as improvement point and starts with the following words:
    if(language == "nl") {
      if(stringr::str_detect(sentence, "\\bprobeer\\b") | stringr::str_detect(sentence, "\\bmag\\b")
         | stringr::str_detect(sentence, "\\bmeer\\b") | stringr::str_detect(sentence, "\\blet\\b")
         | stringr::str_detect(sentence, "\\bheb\\b")) {
        sentence <- paste(improve_addition,sentence,sep="")
        added = TRUE
      }
    }

    polarity<-grasp$pov(sentence, language=language)

    if((polarity != 0 | stringr::str_count(sentence, '\\w+') > 3) & added == FALSE) {
      sentence <- paste(improve_addition,sentence,sep="")
      polarity<-grasp$pov(sentence, language=language)
    }
  } else {
    polarity<-grasp$pov(sentence, language=language)
  }

  return(polarity)
}
