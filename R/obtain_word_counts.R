#' @title Obtain Word Counts
#' @description This function calculates the word counts for each document in the annotated dataset, excluding stopwords.
#' It unites the annotated dataset, applies stopwords filtering, and counts the occurrences of each word.
#'
#' @param data_annotated A data frame containing the annotated text data. The data must include columns like `document`, `submissionid`, `sentenceid`, and `lemma`.
#' @param data_stopwords A vector of stopwords that will be excluded from the word count.
#' @param stopwords_to_append A vector of additional stopwords to be appended to `data_stopwords` before counting words.
#'
#' @return A tibble containing the word counts for each unique combination of `mID` (document, submissionid, sentenceid) and `word`. The tibble has columns:
#'   - `mID`: A unique identifier combining `document`, `submissionid`, and `sentenceid`.
#'   - `word`: The word itself.
#'   - `n`: The frequency of the word.
#'
#' @details This function works by first uniting the `document`, `submissionid`, and `sentenceid` columns into a new identifier `mID`. Then, it filters out stopwords from the word count, counts the frequency of words, and returns the result.
#'
#'
#' @export

obtain_word_counts <- function(data_annotated, data_stopwords, stopwords_to_append) {

  check_dataset(data_annotated, type_of_dataset = 'data_annotated')

  data_annotated_united <- tidyr::unite(
    data_annotated,
    col = "mID",
    .data$document,
    .data$submissionid,
    .data$sentenceid
  )

  by_data_word_annotated <- tibble::tibble(mID = data_annotated_united$mID, word = data_annotated_united$lemma)

  ### add specific stopwords only for topic modeling
  data_stopwords <- append_stopwords(data_stopwords, stopwords_to_append)

  # Count number of words by submissionid or competencyid or sentenceid
  # word_counts <- dplyr::filter(by_data_word_annotated, (!word %in% data_stopwords)) %>%
  #   plyr::filter(by_data_word_annotated, nchar(word) > 1) %>%
  #   dplyr::count(mID, word, sort=TRUE)

  word_counts <- dplyr::filter(by_data_word_annotated, (!.data$word %in% data_stopwords))
  word_counts <- dplyr::filter(word_counts, nchar(.data$word) > 1)
  word_counts <- dplyr::count(word_counts, .data$mID, .data$word, sort=TRUE)

  return (word_counts)
}
