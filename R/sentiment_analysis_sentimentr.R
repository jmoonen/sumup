#' @title Sentiment Analysis using sentimentr
#' @description This function performs sentiment analysis on sentences using a predefined lexicon and the `sentimentr` package. The sentiment score is calculated based on a dictionary of words with associated sentiment values.
#'
#' @param documents A character vector of document identifiers for which sentiment analysis needs to be performed. This is included for consistency, but is not directly used in the analysis.
#' @param sentences A data frame containing sentence-level data. It should include the following columns:
#'   - `sentenceid`: The unique identifier of the sentence.
#'   - `sentence`: The sentence text that needs to be analyzed.
#'   - `document`: The identifier of the document to which the sentence belongs.
#' @param dictionary_file A string representing the path to a CSV file containing the sentiment dictionary. The file should have two columns:
#'   - `word`: The words in the lexicon.
#'   - `value`: The sentiment value associated with the word.
#' @param use_dictionary_file_in_sentimentr A boolean determining whether the dictionary file is used (TRUE) or the built-in lexicon (FALSE)
#'
#' @return A data table with the following columns:
#'   - `sentenceid`: The ID of the sentence.
#'   - `sentence`: The sentence being analyzed.
#'   - `polarity`: The sentiment polarity score for the sentence.
#'   - `document`: The document identifier to which the sentence belongs.
#'
#' @details
#' This function loads a sentiment dictionary, processes the sentences, and calculates a sentiment score for each sentence using the `sentimentr` package. The dictionary is expected to have words associated with sentiment values that influence the sentiment score calculation. Positive sentiment scores indicate a positive sentiment, while negative values indicate negative sentiment.
#'
#' The dictionary file (`SumUp_Dictionary.csv`) is read from the `data/` directory by default. The file should have a column of words and corresponding sentiment values.
#'
#' @export

sentiment_analysis_sentimentr <- function(documents, sentences, dictionary_file, use_dictionary_file_in_sentimentr) {

  check_dataset(sentences, type_of_dataset = "sentences")
  if (!requireNamespace("sentimentr", quietly = TRUE)) {
    stop("Package 'sentimentr' must be installed to use this function.", call. = FALSE)
  }
  my_sentences = sentimentr::get_sentences(sentences$sentence)


  if(use_dictionary_file_in_sentimentr) {
     # Load the dictonary
    my_dictionary = read.csv(dictionary_file, header=TRUE, sep=";")
    colnames(my_dictionary) = c("word", "value")
    my_lexicon = suppressWarnings(sentimentr::as_key(my_dictionary))

    sentences$polarity <- sentimentr::sentiment(my_sentences, polarity_dt = my_lexicon)$sentiment
  } else {
    sentences$polarity <- sentimentr::sentiment(my_sentences)$sentiment
  }

  sentence_polarity <- data.table::data.table("sentenceid" = sentences$sentenceid, "sentence" = sentences$sentence, "polarity" = sentences$polarity, "document" = sentences$document)

  return(sentence_polarity)
}





#   # escape: specifically optimized for Dutch
#   if(language == "nl") {
#     sentence<-stringr::str_replace_all(sentence, "naar", "")
#     sentence<-stringr::str_replace_all(sentence, "gemaakt", "")
#     sentence<-stringr::str_replace_all(sentence, "laat", "")
#     #sentence<-stringr::str_replace_all(sentence, ",", "")
#     sentence<-stringr::str_replace_all(sentence, ";", "")
#     sentence<-stringr::str_replace_all(sentence, ":", "")
#     sentence<-stringr::str_replace_all(sentence, " ' ", " ")
#
#     # erg --> neemt de tijd voor patiënten, die voelen zich daardoor erg gehoord --> neg (--> "gehoord" zit er niet in)
#     # te --> houding tijdens gesprek is te amicaal en te weinig met professionele distantie --> pos
#     # maar "fijn om mee samen te werken" is terecht positief --> 'te' negatief sentiment geven werkt niet
#     # opgelost door combinaties van 'te' met bijv naamwoord sentiment -0.3 te geven
#   }
#
#   improve_addition = "suboptimal - "
#   if(language == "nl") {
#     improve_addition = "suboptimal - "
#   } else if(language == "de"){
#     improve_addition = "suboptimal - "
#   } else if(language == "fr"){
#     improve_addition = "sous-optimal - "
#   }
#
#   if(comment_type == "verbeter"){
#     added = FALSE
#
#     # In Dutch, use the addional text when the feedback is presented as improvement point and starts with the following words:
#     if(language == "nl") {
#       if(stringr::str_detect(sentence, "\\bprobeer\\b") | stringr::str_detect(sentence, "\\bmag\\b")
#          | stringr::str_detect(sentence, "\\bmeer\\b") | stringr::str_detect(sentence, "\\blet\\b")
#          | stringr::str_detect(sentence, "\\bheb\\b")) {
#         sentence <- paste(improve_addition,sentence,sep="")
#         added = TRUE
#       }
#     }
#
#     polarity<-grasp$pov(sentence, language=language)
#
#     if((polarity != 0 | stringr::str_count(sentence, '\\w+') > 3) & added == FALSE) {
#       sentence <- paste(improve_addition,sentence,sep="")
#       polarity<-grasp$pov(sentence, language=language)
#     }
#   } else {
#     polarity<-grasp$pov(sentence, language=language)
#   }
#

