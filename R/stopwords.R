# ===================================
# text_clean: Clean Text in Dataset
# ===================================
#' @title Load Stopwords
#' @description Load stopwords from a file
#'
#' @param stopwords_file A string representing the path to a file containing a list of stopwords, with one word per line. This file is expected to have tab-separated values with the stopwords in the first column.
#'
#' @return
#' Returns a character vector of stopwords read from the specified file.
#'
#' @details
#' The `default_stopwords` function loads a list of stopwords from a specified file, with each stopword being listed on a new line. It returns a character vector of these stopwords.
#' @export

default_stopwords <- function(stopwords_file) {
  # list of stopwords: stopwords("dutch")
  read_stopwords <-read.csv(stopwords_file, header=FALSE, sep="\t", stringsAsFactors=FALSE)
  data_stopwords <- read_stopwords$V1

    return (data_stopwords)
}

#' @title Append Stopwords
#' @description This function is used to append additional stopwords to an existing list.
#'
#' @param data_stopwords A character vector containing the initial set of stopwords to which more stopwords can be appended.
#' @param stopwords_to_append A character vector containing additional stopwords to be appended to the existing `data_stopwords` vector.
#'
#' @return
#' Returns a character vector of unique stopwords after appending the specified stopwords and additional stopwords from the `stopwords` package.
#'
#' @details
#' The `append_stopwords` function takes an existing list of stopwords (`data_stopwords`) and appends a new set of stopwords (`stopwords_to_append`) to it. Additionally, stopwords from the `stopwords` package (using the "dutch" language option) are also appended to the list. The final list is then returned with duplicate stopwords removed by calling `unique()`.
#'
#' @export

append_stopwords <- function(data_stopwords, stopwords_to_append) {
  if (!requireNamespace("stopwords", quietly = TRUE)) {
    stop("Package 'stopwords' must be installed to use this function.", call. = FALSE)
  }

  data_stopwords <- append(data_stopwords, stopwords_to_append)
  data_stopwords <- append(data_stopwords, stopwords::stopwords("dutch"))
  return (unique(data_stopwords))
}
