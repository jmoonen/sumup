#' @title Lemmatize Sentences Using a UDPipe Model
#' @description This function processes a dataset of sentences using a UDPipe model to perform
#' lemmatization, applies corrections to the lemmas, and associates metadata (e.g., submission ID, document)
#' with the processed sentences. If UDPipie is not available, package NLP is used.
#' @param sentences A data frame containing sentences, with at least a `sentenceid`, `sentence`, `document`,
#' and `submissionid` column.
#' @param udpipe_model_file A character string representing the path to the UDPipe model file used for annotation.
#' @param corrections_file A character string representing the path to a CSV file containing corrections
#' to be applied to the lemmas.
#' @param language A character string representing the language of the dataset ('nl', 'en', 'de' or 'fr')
#' @param use_seeds A logical value indicating whether to use the seeds (e.g. of an educational framework)
#' @return A data frame containing the lemmatized sentences with the following columns:
#' - `doc_id`: Document identifier.
#' - `lemma`: The lemmatized form of each word.
#' - `upos`: Universal part-of-speech tag.
#' - `sentenceid`: The sentence ID for the sentence from which the lemma was extracted.
#' - `document`: Document associated with the sentence.
#' - `submissionid`: Submission ID associated with the sentence.
#' @details This function loads a UDPipe model, annotates the input sentences, corrects lemmas based on
#' a provided corrections file, and optionally filters by noun tokens. It returns a data frame with the
#' sentence-level annotations.
#' @importFrom udpipe udpipe_download_model udpipe_load_model udpipe_annotate
#' @export

lemmatize <- function(sentences, udpipe_model_file, corrections_file, language, use_seeds = TRUE) {

  check_dataset(sentences, type_of_dataset = "sentences")

  ### Get and load the udpipe model
  ud_model_language = "dutch"
  ud_model = NULL

  if (!requireNamespace("udpipe", quietly = TRUE)) {
    stop("Package 'udpipe' must be installed to use this function.", call. = FALSE)
  }

  if(udpipe_model_file != "") {

    if (!file.exists(udpipe_model_file)) {


      if(language == "en"){
        ud_model_language = "english"
      } else if(language == "de"){
          ud_model_language = "german"
      } else if(language == "fr"){
        ud_model_language = "french"
      }

      message("Model not found locally. Trying to download...")
      udpipe_model_file <- file.path(tempdir(), paste0(language, ".udpipe"))

      # Try download
      try({
        model_info <- udpipe::udpipe_download_model(language = language)
        file.copy(model_info$file_model, udpipe_model_file, overwrite = TRUE)
      }, silent = TRUE)
    }
  }

  data_anotated = NULL
  # If file now exists, load it
  if (file.exists(udpipe_model_file)) {

    ud_model = udpipe::udpipe_load_model(udpipe_model_file)

    # Annotate the sentences
    data_annotated <- udpipe::udpipe_annotate(ud_model, x = sentences$sentence)
    data_annotated <- as.data.frame(data_annotated)
  } else {
    message("UDPipe model not available. Please download manually using udpipe_download_model() and set setting udpipe_model_file if you want to use that.")
    if(language == "en"){
        message("Sum-up continues running with an alternative tagging package.")
        data_annotated <- alternative_tagging(sentences)
        data_annotated$doc_id = paste("doc", data_annotated$sentenceid, sep = "")
        data_annotated$document = paste("doc", data_annotated$sentenceid, sep = "")
    }
    else{
      return(FALSE)
    }
  }

  if(is.null(data_annotated)) {
    stop("Tagging failed")
    return(FALSE)
  }

  # Correct lemmas (use efficient match and correction)
  corrections <- get_corrections(corrections_file)
  data_annotated$lemma <- correct_text(data_annotated$lemma, corrections)

  # Match document and submissionid directly using vectorized operations
  doc_ids <- paste("doc", sentences$sentenceid, sep = "")
  data_annotated$document <- sentences$document[match(data_annotated$doc_id, doc_ids)]
  data_annotated$submissionid <- sentences$submissionid[match(data_annotated$doc_id, doc_ids)]

  # Filter out "NA" values for document
  data_annotated <- dplyr::filter(data_annotated, !is.na(data_annotated$document))

  # Extract sentenceid from doc_id
  data_annotated$sentenceid <- stringr::str_replace(data_annotated$doc_id, "doc", "")

  # Apply seed condition if `use_seeds == FALSE`
  if (!use_seeds) {
    data_annotated <- dplyr::filter(data_annotated, data_annotated$upos == "NOUN")
  }

  return(data_annotated)
}

alternative_tagging = function(sentences) {
   if (!requireNamespace("tokenizers", quietly = TRUE) ||
       !requireNamespace("textstem", quietly = TRUE)) {
     stop("Please install tokenizers, textstem, and openNLP packages.")
   }

   results <- lapply(seq_len(nrow(sentences)), function(i) {
     s <- sentences[i, , drop = FALSE]
     sentence_text <- s$sentence

     # Skip empty or NA sentences
     if (is.na(sentence_text) || trimws(sentence_text) == "") return(NULL)

     # Tokenize and lemmatize
     if (!requireNamespace("tokenizers", quietly = TRUE)) {
       stop("Package 'tokenizers' must be installed to use this function.", call. = FALSE)
     }
     toks <- unlist(tokenizers::tokenize_words(sentence_text))
     if (!requireNamespace("textstem", quietly = TRUE)) {
       stop("Package 'textstem' must be installed to use this function.", call. = FALSE)
     }
     lemmas <- textstem::lemmatize_words(toks)

     # # Optional POS tagging (English only)
     # if (requireNamespace("parts.of.speech", quietly = TRUE)) {
     #   pos <- parts.of.speech::pos(toks)
     # } else {
     #   pos <- rep(NA_character_, length(toks))
     # }

     # Replicate metadata for each token
     meta <- s[rep(1, length(toks)), , drop = FALSE]

     # Combine into final data frame
     cbind(
       meta,
       data.frame(
         paragraph_id = 1,
         sentence_id = 1,
         token_id = seq_along(toks),
         token = toks,
         lemma = lemmas,
         upos = NA_character_,
         xpos = NA_character_,
         feats = NA_character_,
         head_token_id = NA_integer_,
         dep_rel = NA_character_,
         stringsAsFactors = FALSE,
         deps = NA_character_,
         misc = NA_character_
       )
     )
   })

   # Bind results safely
   do.call(rbind, results)
 }

