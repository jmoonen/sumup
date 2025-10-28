#' @title Text Cleaning and Processing Functions
#' @description This file contains helper functions for text processing and cleaning used in the 'sumup' package.
#' The functions provide capabilities for text correction, cleaning, dataset preparation, and handling abbreviation replacements.
#' @author Joyce M.W. Moonen - van Loon
#'
#' This file defines several functions:
#' - `text_clean()`: Cleans multiple columns in a dataset.
#' - `correct_text()`: Applies typo corrections using a predefined correction list.
#' - `clean_text()`: Cleans and formats input text, handling HTML tags, spacing, punctuation, and typos.
#' - `get_corrections()`: Reads a correction file and processes typo replacements.
#' - `create_dataset_narratives()`: Prepares a dataset by restructuring and normalizing its textual content.
#' - `replace_abbr()`: Corrects abbreviations.

# ===================================
# text_clean: Clean Text in Dataset
# ===================================
#' @title Clean Text in Dataset
#' @description Cleans narrative text columns (`sterk`, `verbeter`, `feedback`) in a dataset.
#' @param data A data.table or data.frame containing text data.
#' @param corrections_file Path to a CSV file containing typo corrections.
#' @return A cleaned dataset with standardized text.
#' @export

text_clean <- function(data, corrections_file) {

  new_dataset <- create_dataset_narratives(data)

  corrections <- get_corrections(corrections_file)

  if(nrow(corrections) > 0){
    # Clean all narrative data columns
    new_dataset$sterk 	<- clean_text(new_dataset$sterk, corrections)
    new_dataset$verbeter <- clean_text(new_dataset$verbeter, corrections)
    new_dataset$feedback <- clean_text(new_dataset$feedback, corrections)
  }

  return(new_dataset)
}

# ===================================
# create_dataset_narratives: Create Narratives Dataset
# ===================================

#' @title Create Narratives Dataset
#' @description Restructures a dataset by normalizing and transforming its textual content.
#' @param data A data.table or data.frame containing narrative data.
#' @return A structured data.table with cleaned text.
#' @export

create_dataset_narratives <- function(data) {

  check_dataset(data, type_of_dataset = "dataset")

  # Convert "NULL" to NA and ensure 'score' is numeric
  data$score[data$score == "NULL"] <- NA
  data$score <- as.numeric(data$score)

  sterk = verbeter = feedback = portfolioid = NULL # due to NSE notes in R CMD check

  # Create a 'comment' by combining 'sterk', 'verbeter', and 'feedback'
  #data$comment <- trimws(paste(data$sterk, data$verbeter, data$feedback, sep = ". "))
  data$comment <- apply(
    data[, c("sterk", "verbeter", "feedback")],
    1,
    function(x) {
      # keep only non-missing, non-empty parts
      parts <- x[!is.na(x) & x != ""]
      if (length(parts) == 0) "" else paste(parts, collapse = ". ")
    }
  )
  data$feedback <- as.character(data$feedback)
  #print(head(data))

  # Create a single data frame for all types (sterk, verbeter, feedback)
  data <- data %>%
    dplyr::mutate(
      sterk    = as.character(sterk),
      verbeter = as.character(verbeter),
      feedback = as.character(feedback)
    )

  data_long <- dplyr::bind_rows(
    data %>%
      dplyr::filter(!is.na(sterk), trimws(sterk) != "") %>%
      dplyr::mutate(
        comment   = sterk,
        verbeter  = as.character(""),
        feedback  = as.character(""),
        document  = portfolioid,
        type      = "sterk"
      ),

    data %>%
      dplyr::filter(!is.na(verbeter), trimws(verbeter) != "") %>%
      dplyr::mutate(
        comment   = verbeter,
        sterk     = as.character(""),
        feedback  = as.character(""),
        document  = portfolioid,
        type      = "verbeter"
      ),

    data %>%
      dplyr::filter(!is.na(feedback), trimws(feedback) != "") %>%
      dplyr::mutate(
        comment   = feedback,
        sterk     = as.character(""),
        verbeter  = as.character(""),
        document  = portfolioid,
        type      = "feedback"
      )
  )

  # Remove rows where comment is empty
  data_long <- dplyr::filter(data_long, comment != "")

  # Create the 'narratives' data.table with relevant columns
  narratives <- data.table::data.table(
    document = data_long$document,
    submissionid = data_long$submissionid,
    competencyid = data_long$competencyid,
    assistant = data_long$assistant,
    portfolioid = data_long$portfolioid,
    comment = data_long$comment,
    sterk = data_long$sterk,
    verbeter = data_long$verbeter,
    feedback = data_long$feedback,
    type = data_long$type
  )

  return(narratives)
}

# ===================================
# correct_text: Applies typo corrections to an input text using a given correction table
# ===================================

#' @title Correct Text
#' @description Applies typo corrections to an input text using a given correction table.
#' @param input_text A character vector containing the text to be corrected.
#' @param corrections A data frame containing two columns: `typoEdit` (pattern) and `replacement` (correction).
#' @return A character vector with corrected text.
#' @export

correct_text <- function(input_text, corrections) {
  #text_to_clean <- text_clean::mgsub_regex(input_text, corrections$typoEdit, corrections$replacement)
  if (!requireNamespace("stringi", quietly = TRUE)) {
    stop("Package 'stringi' must be installed to use this function.", call. = FALSE)
  }
  if(nrow(corrections) > 0){
    text_to_clean <- stringi::stri_replace_all_regex(input_text, corrections$typoEdit, corrections$replacement, vectorize_all = FALSE)
  } else {
    text_to_clean = input_text
  }

  return(text_to_clean)
}

# ===================================
# clean_text: Cleans and standardizes text by handling HTML tags, punctuation, spacing, and abbreviations
# ===================================

#' @title Clean Text
#' @description Cleans and standardizes text by handling HTML tags, punctuation, spacing, and abbreviations.
#' @param input_text A character vector containing the text to be cleaned.
#' @param corrections A data frame containing typo corrections.
#' @return A cleaned character vector.
#' @export

clean_text <- function(input_text, corrections) {
  # Define patterns and corresponding replacements
  replacements <- list(
    "</p>" = "\\.</p> ",
    "</P>" = "\\.</P> ",
    "<br>" = "\\. ",
    "\\b([a-zA-Z]\\.\\s*){2,}" = replace_abbr,  # Abbreviation correction
    "bijv.(\\s+[A-Z])" = "bijv..\\1",
    "bv.(\\s+[A-Z])" = "bv..\\1",
    "etc.(\\s+[A-Z])" = "etc..\\1",
    "enz.(\\s+[A-Z])" = "enz..\\1"
  )

  # Loop over the replacements and apply them one by one
  for (pattern in names(replacements)) {
    replacement <- replacements[[pattern]]
    input_text <- stringr::str_replace_all(input_text, pattern, replacement)
  }

  # Apply textclean's replace_html and replace_white only once
  text_to_clean <- textclean::replace_html(input_text, FALSE)
  text_to_clean <- textclean::replace_white(text_to_clean)

  # Replace tokens ('/' and '-') in a single step
  text_to_clean <- textclean::replace_tokens(text_to_clean, c('/', '-'))

  # Replace double periods followed by space and uppercase letter
  text_to_clean <- gsub("\\.{2,}(\\s+[A-Z])", ".\\1", text_to_clean)

  # Replace consecutive periods with an underscore
  text_to_clean <- gsub("\\.{2,}", "_", text_to_clean)

  # Squish extra spaces and trim text
  text_to_clean <- stringr::str_squish(text_to_clean)

  # Add missing endmark and remove standalone periods
  text_to_clean <- textclean::add_missing_endmark(text_to_clean, replacement = ".")
  text_to_clean <- gsub("^\\.$", "", text_to_clean)

  # Replace the remaining underscore with a period
  text_to_clean <- stringr::str_replace_all(text_to_clean, "_", "\\.")

  # Final typo correction with external correction function
  return(correct_text(text_to_clean, corrections))
}

# ===================================
# get_corrections: Reads a typo correction file and prepares it for use in text cleaning
# ===================================

#' @title Load Corrections
#' @description Reads a typo correction file and prepares it for use in text cleaning.
#' @param corrections_file Path to a CSV file with typo corrections.
#' @return A data frame containing `typoEdit` (pattern) and `replacement` (correction).
#' @export

get_corrections <- function(corrections_file) {
    corrections <- read.csv(corrections_file, header = TRUE, sep=",", stringsAsFactors = FALSE, encoding="UTF-8")

    corrections <- dplyr::mutate(corrections, typoEdit = dplyr::case_when(
    stringr::str_detect(typo, "\\.") ~ paste("\\b",stringr::str_replace(typo, "\\.", "\\\\."),sep=""),
    !stringr::str_detect(typo, "\\.") ~ paste("\\b",typo,"\\b",sep="")
  ))

    return (corrections)
}

# ===================================
# replace_abbr: eplaces periods in abbreviations to ensure correct spacing and formatting
# ===================================

#' @title Replace Abbreviations
#' @description Replaces periods in abbreviations to ensure correct spacing and formatting.
#' @param abbr A character string containing the abbreviation.
#' @return A corrected abbreviation string.
#' @export

replace_abbr <- function(abbr) {
  last_space <- stringr::str_replace(abbr, "^.*?(\\s*)$", "\\1")
  abbr <- stringr::str_replace_all(abbr, "(\\.\\s*)", "")
  abbr <- paste(abbr, last_space, sep="")
}

