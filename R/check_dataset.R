
check_dataset <- function(dt, type_of_dataset = 'dataset', nr_topics = 0) {

    expected_cols <- c("submissionid", "competencyid", "formid", "templatename", "assistant", "authority",
                       "specialty", "hospital", "datereferenced", "portfolioid", "sterk", "verbeter",
                       "feedback", "score", "zorgpunten")

    if(type_of_dataset == 'narratives') {
      expected_cols <- c("document", "submissionid", "competencyid", "assistant", "portfolioid", "comment",
                         "sterk", "verbeter", "feedback", "type")
    } else if(type_of_dataset == "sentences") {
      expected_cols <- c("document", "submissionid", "competencyid", "assistant", "portfolioid",
                         "comment", "type", "sentence", "sentenceid")
    } else if(type_of_dataset == 'data_annotated') {
      expected_cols <- c("doc_id", "paragraph_id", "sentence_id", "sentence", "token_id", "token",
                         "lemma", "upos", "xpos", "feats", "head_token_id", "dep_rel",
                         "deps", "misc", "document", "submissionid", "sentenceid")
    } else if(type_of_dataset == "word_counts") {
      expected_cols <- c("mID", "word", "n")
    } else if(type_of_dataset == "sentence_polarity") {
      expected_cols <- c("sentenceid", "sentence", "polarity", "document")
    } else if(type_of_dataset == "sentence_complete_output") {
      required_cols <- c("submissionid", "sentenceid", "sentence", "polarity",
                         "max_probability", "document", "competencyid", "type",
                         "portfolioid", "feedbacktype", "datereferenced",
                         "templatename", "score", "comment", "zorgpunt")

      numeric_cols <- as.character(1:nr_topics)
      expected_cols <- c(required_cols, numeric_cols)
    }

    actual_cols <- colnames(dt)

    if (setequal(actual_cols, expected_cols)) {
      return(TRUE)
    } else {
      missing_cols <- setdiff(expected_cols, actual_cols)

      if (length(missing_cols) > 0) {
        message("Column mismatch detected for '%s'!", type_of_dataset)
        message("Missing columns: ", paste(missing_cols, collapse = ", "))
      }

      return(length(missing_cols) == 0)  # Return FALSE only if there are missing columns
    }
}

is_LDA <- function(obj) {
  if(inherits(obj, "LDA")){
    return(TRUE)
  } else {
    message("The object is not of class LDA!")
    return(FALSE)
  }
}
