#' @title Tidy and Split Narrative Text
#' @description This function processes narrative data by splitting the text into sentences or simply subsetting the data based on specific comment types. It ensures consistency across various comment types and removes unwanted columns and duplicates.
#'
#' @param narratives A data frame or data.table containing the narratives to be processed. The dataset should include columns representing different types of comments (e.g., `sterk`, `verbeter`, `feedback`).
#' @param split_in_sentences A logical value indicating whether to split the text into individual sentences. If `TRUE`, the function will split the narratives into sentences using regular expressions. If `FALSE`, it will only subset the data based on comment types.
#'
#' @return A data.table containing sentences (or narrative data) with the following columns:
#' - `document`: The document ID.
#' - `submissionid`: The submission ID.
#' - `competencyid`: The competency ID.
#' - `assistant`: The assistant information.
#' - `portfolioid`: The portfolio ID.
#' - `sentenceid`: A unique identifier for each sentence (or narrative entry).
#' - `sentence`: The cleaned-up sentence text.
#'
#' @details
#' The `tidy_text` function processes a dataset of narratives, splitting them into individual sentences (if `split_in_sentences = TRUE`) or subsetting them based on comment types (if `split_in_sentences = FALSE`). The comment types are predefined as `sterk`, `verbeter`, and `feedback`.
#' - When `split_in_sentences = TRUE`, the function unnests the narrative data into sentences using regular expressions to identify sentence boundaries (periods, question marks, and exclamation marks).
#' - When `split_in_sentences = FALSE`, the function subsets the data by comment type and ensures that the relevant columns are kept.
#' The function also performs text cleaning by squishing spaces and removing HTML tags.
#'
#' @export

tidy_text <- function(narratives, split_in_sentences = TRUE) {
  check_dataset(narratives, type_of_dataset = "narratives")

  # Define the column names and their corresponding comment types
  comment_types <- c("sterk", "verbeter", "feedback")

  # If splitting into sentences, use unnest_tokens; otherwise, subset the data
  if (split_in_sentences) {
    # Unnest sentences in one step for all comment types
    sentences_list <- lapply(comment_types, function(comment_type) {
      tidytext::unnest_tokens(narratives, .data$sentence, !!rlang::sym(comment_type), token = "regex", pattern = "(?<!\\b\\p{L}r)\\.|\\?|\\!")
    })

    # Remove the comment_types columns from each data.table
    sentences_list <- lapply(sentences_list, function(dt) {
      dt[, !(names(dt) %in% comment_types), with = FALSE]
    })

    # Combine all sentences
    sentences_all <- do.call(rbind, sentences_list)
  } else {
    # Subset data for each comment type
    sentences_list <- lapply(comment_types, function(comment_type) {
      subset(narratives, !!rlang::sym(comment_type) != "", select = c(narratives$document, narratives$submissionid, narratives$competencyid, narratives$assistant, narratives$portfolioid, narratives$comment, comment_types))
    })

    # Rename columns for consistency
    sentences_list <- lapply(sentences_list, function(df) {
      colnames(df)[colnames(df) == df$comment_type] <- "sentence"
      return(df)
    })

    # Remove the comment_types columns from each data.table
    sentences_list <- lapply(sentences_list, function(dt) {
      dt[, !(names(dt) %in% comment_types), with = FALSE]
    })

    # Combine all sentences
    sentences_all <- do.call(rbind, sentences_list)
  }

  # Add 'sentenceid' column and clean sentence text
  sentences_all <- data.table::data.table(sentences_all)
  sentences_all$sentenceid <- 1:nrow(sentences_all)
  sentences_all$sentence <- stringr::str_squish(textclean::replace_html(sentences_all$sentence, FALSE))

  # Remove duplicates
  sentences_all <- unique(sentences_all)

  return(sentences_all)
}
