#' @title Topic Classification for Narrative Data
#' @description This function classifies sentences from narrative data into topics based on Latent Dirichlet Allocation (LDA) topic modeling results. It integrates additional features like sentiment analysis, annotated data, and seeded topics to enhance the classification.
#'
#' @param data A data frame or data.table containing metadata and other relevant data for topic classification.
#' @param narratives A data frame or data.table containing the narrative data, including comments and feedback.
#' @param sentences A data frame or data.table containing the sentences to be classified, including the text and metadata.
#' @param sentences_lda A result object from LDA topic modeling, which contains the topic distribution for each term.
#' @param sentence_polarity A data frame or data.table containing sentence-level polarity scores (sentiment analysis results).
#' @param data_annotated A data frame or data.table containing the lemmatized text and other annotations.
#' @param use_beta A logical value indicating whether to use the `beta` values from the LDA model for topic classification.
#' @param use_seeds A logical value indicating whether to incorporate seeded topics for topic classification.
#' @param nr_topics An integer specifying the number of topics to classify.
#' @param seeded_topics A list of character vectors representing the topics with predefined seed words.
#' @param competencies A list of mames of the competencies per id as used in the data set
#'
#' @return A data.table containing the classified sentences with the following columns:
#' - `sentenceid`: Unique identifier for each sentence.
#' - `sentence`: The sentence text.
#' - `polarity`: The sentiment polarity score of the sentence.
#' - `max_probability`: The highest topic probability for each sentence.
#' - Additional columns corresponding to each topic, representing the probability of the sentence belonging to that topic.
#' - Metadata fields such as `document`, `submissionid`, `competencyid`, `feedbacktype`, `score`, and `comment`.
#'
#' @details
#' The `topic_classification` function integrates several sources of data to classify sentences into topics:
#' - It uses LDA topic modeling results to assign a probability for each topic.
#' - The function can incorporate seeded topics, which enhance the classification by matching predefined keywords to the topics.
#' - Sentiment analysis is used to add polarity scores to the sentence data.
#' - The output includes additional metadata for each sentence, such as feedback type, score, and associated comments.
#'
#' @export

topic_classification <- function(data, narratives, sentences, sentences_lda,
                                 sentence_polarity, data_annotated, use_beta,
                                 use_seeds, nr_topics, seeded_topics, competencies) {

  check_dataset(data, type_of_dataset = "dataset")
  check_dataset(narratives, type_of_dataset = "narratives")
  check_dataset(sentences, type_of_dataset = "sentences")
  check_dataset(sentence_polarity, type_of_dataset = "sentence_polarity")
  check_dataset(data_annotated, type_of_dataset = "data_annotated")

  .datatable.aware = TRUE
  comment_key = competencyid = datereferenced = document = feedbacktype = lemma = max_probability = polarity = score = score_key = sentence = sentenceid = submissionid = templatename = type = NULL # due to NSE notes in R CMD check

  is_LDA(sentences_lda)

  sentences_lda_td <- tidytext::tidy(sentences_lda)

  # Convert to data.table
  data.table::setDT(sentences_lda_td)
  data.table::setDT(data_annotated)
  data.table::setDT(sentence_polarity)
  data.table::setDT(sentences)
  data.table::setDT(narratives)
  data.table::setDT(data)

  data_annotated[, sentenceid := as.numeric(data_annotated$sentenceid)]
  sentence_polarity[, sentenceid := as.numeric(sentence_polarity$sentenceid)]
  sentences[, sentenceid := as.numeric(sentences$sentenceid)]

  # Ensure beta is numeric
  sentences_lda_td[, beta := as.numeric(sentences_lda_td$beta)]

  # Reshape into wide format
  if (!requireNamespace("reshape2", quietly = TRUE)) {
    stop("Package 'reshape2' must be installed to use this function.", call. = FALSE)
  }
  lda_td <- reshape2::dcast(sentences_lda_td, term ~ topic, value.var = "beta", fill = 0)

  # Join top_term_words with lda_td
  top_term_matrix <- data.table::merge.data.table(
    data_annotated[, .(sentenceid, lemma)],
    lda_td, by.x = "lemma", by.y = "term", all.x = TRUE
  )
  data.table::setnafill(top_term_matrix, fill = 0, cols = 3:ncol(top_term_matrix))  # Replace NA with 0

  # Aggregate by sentenceid to get max beta per topic
  sentence_complete_output <- top_term_matrix[, lapply(.SD, max), by = sentenceid, .SDcols = 3:ncol(top_term_matrix)]

  sentence_complete_output[, sentenceid := as.numeric(sentence_complete_output$sentenceid)]

  # Add sentence text
  sentence_complete_output <- data.table::merge.data.table(
    sentence_complete_output,
    sentence_polarity[, .(sentenceid, sentence, polarity)],
    by = "sentenceid", all.x = TRUE
  )

  if (use_seeds) {
    # Combine lemmas per sentence
    lemmas <- data_annotated[, .(lemmas = paste(lemma, collapse = " ")), by = sentenceid]
    sentence_complete_output <- data.table::merge.data.table(sentence_complete_output, lemmas, by = "sentenceid", all.x = TRUE)

    # Flatten seeded topics
    seeded_topics_flat <- unlist(unlist(seeded_topics, recursive = FALSE), recursive = FALSE)

    maxBeta <- max(sentences_lda_td$beta, na.rm = TRUE)

    # Vectorized lookup for seeded topic matches
    for (t in seq_len(nr_topics)) {
      topic_name <- as.character(t)
      pattern <- paste0("\\b", paste(seeded_topics_flat[[t]], collapse = "|"), "\\b")
      sentence_complete_output[, (topic_name) := ifelse(
        stringr::str_detect(paste(sentence, lemmas, sep = " "), pattern),
        get(topic_name) + maxBeta,
        get(topic_name)
      )]
    }
  }

  # Compute max probability per sentence
  if (!requireNamespace("matrixStats", quietly = TRUE)) {
    stop("Package 'matrixStats' must be installed to use this function.", call. = FALSE)
  }
  sentence_complete_output[, max_probability := matrixStats::rowMaxs(as.matrix(.SD)), .SDcols = 2:(nr_topics + 1)]

  # Match metadata fields
  sentence_complete_output <- data.table::merge.data.table(
    sentence_complete_output,
    sentences[, .(sentenceid, document, submissionid, competencyid, type)],
    by = "sentenceid", all.x = TRUE
  )

  # Assign metadata directly
  sentence_complete_output[, `:=`(
    portfolioid = document,
    feedbacktype = type
  )]

  # Match additional data fields
  sentence_complete_output <- data.table::merge.data.table(
    sentence_complete_output,
    data[, .(submissionid, datereferenced, templatename)],
    by = "submissionid", all.x = TRUE, allow.cartesian = TRUE
  )

  # Add zorgpunt = 1 if the feedback is denoted as a concern (zorgpunt) in the origial submission
  # Initialize column
  zorgpunt = 0L

  sentence_complete_output[, zorgpunt := 0L]

  # Loop over competencies
  for (i in seq_along(competencies[[1]])) {
    comp_obj <- competencies[[1]][i]             # each competency list
    comp_id <- names(comp_obj)                   # competencyid as string
    comp_name <- trimws(comp_obj[[1]])           # competency name

    # Trim and lowercase for robust comparison
    comp_name_clean <- tolower(trimws(comp_name))

    # Loop through data rows
    for(j in seq_len(nrow(data))) {
      submission_id <- data$submissionid[j]
      zorgpunten_vec <- unlist(strsplit(as.character(data$zorgpunten[j]), ","))
      zorgpunten_vec <- tolower(trimws(zorgpunten_vec))  # normalize

      # Check if this competency matches any zorgpunt for this submission
      if(comp_name_clean %in% zorgpunten_vec) {
        # Find matching rows in sentence_complete_output
        sentence_complete_output[
          submissionid == submission_id & competencyid == comp_id,
          zorgpunt := 1L
        ]
      }
    }
  }


###################

  # Match score using efficient key joins
  data[, score_key := paste(submissionid, competencyid, sep = "_")]
  sentence_complete_output[, score_key := paste(submissionid, competencyid, sep = "_")]

  sentence_complete_output <- data.table::merge.data.table(
    sentence_complete_output,
    data[, .(score_key, score)],
    by = "score_key", all.x = TRUE
  )
  sentence_complete_output[, score_key := NULL]

  # Process comments efficiently
  data_comment <- narratives[, .(comment = paste(comment, collapse = " ")),
                             by = .(submissionid, competencyid, type)]

  sentence_complete_output[, comment_key := paste(submissionid, competencyid, feedbacktype, sep = "_")]
  data_comment[, comment_key := paste(submissionid, competencyid, type, sep = "_")]

  sentence_complete_output <- data.table::merge.data.table(
    sentence_complete_output,
    data_comment[, .(comment_key, comment)],
    by = "comment_key", all.x = TRUE
  )
  sentence_complete_output[, comment_key := NULL]

  sentence_complete_output=unique(sentence_complete_output)
  return(sentence_complete_output)
}
