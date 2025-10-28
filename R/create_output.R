#' @title Create Output for Sum Up
#' @description Processes sentence-level Sum Up's output, assigns sentences to topics,
#' calculates topic strengths, and organizes data into structured feedback categories.
#' @param sentence_complete_output A data frame containing sentence-level topic polarity scores.
#' @param sentences_lda An LDA model object containing topic-term distributions.
#' @param settings A list of settings containing parameters such as the number of topics,
#' threshold values, scoring scale, and output format.
#' @return A structured list containing topic-wise feedback, strength, and quality assessments.
#' If `settings$output_json` is `TRUE`, returns a JSON string.
#' @details This function processes Sum Up's output to assign sentences to topics based on
#' a probability threshold. It calculates the topic strength based on sentence polarity and recency,
#' categorizes topics by feedback quantity and quality, and includes unassigned feedback in a separate category.
#' @export

create_output <- function(sentence_complete_output, sentences_lda, settings) {

  check_all_settings(settings)
  check_dataset(sentence_complete_output, type_of_dataset = "sentence_complete_output", nr_topics = settings$nr_topics)
  is_LDA(sentences_lda)

  all_topics <- list()
  selected_ids <- c()
  assigned_sentences <- 0

  if (!requireNamespace("matrixStats", quietly = TRUE)) {
    stop("Package 'matrixStats' must be installed to use this function.", call. = FALSE)
  }
  max_probability <- max(matrixStats::rowMaxs(as.matrix(sentence_complete_output[, 3:(settings$nr_topics+2)])))
  min_probability <- min(matrixStats::rowMaxs(as.matrix(sentence_complete_output[, 3:(settings$nr_topics+2)])))

  competencies <- unlist(settings$competencies)
  portfolio_names <- unlist(settings$portfolio_names)

  threshold <- settings$assignment_threshold * max_probability
  if (threshold < min_probability) {
    threshold <- (max_probability - min_probability) * settings$assignment_threshold + min_probability
  }

  seeded_topics <- settings$seeded_topics
  topic_names <- settings$topic_names
  topic_domain <- settings$topic_domain
  topic_addition <- settings$topic_addition

  if (!settings$use_seeds) {
    sentences_lda_td <- tidytext::tidy(sentences_lda)

    sentences_lda_td_grouped <- dplyr::group_by(sentences_lda_td, .data$topic)

    top_terms_filtered <- dplyr::top_n(sentences_lda_td_grouped, settings$nr_top_terms, .data$beta)

    top_terms_ungrouped <- dplyr::ungroup(top_terms_filtered)

    top_terms <- dplyr::arrange(top_terms_ungrouped, .data$topic, dplyr::desc(.data$beta))

    top_terms <- dplyr::filter(top_terms, .data$beta >= 0.01)

    d <- data.table::data.table(top_terms)

    d <- dplyr::mutate(d, topic = as.factor(.data$topic))

    d_grouped <- dplyr::group_by(d, .data$topic)

    d_wide <- dplyr::summarise(d_grouped, terms = paste0(.data$term, collapse = ", "), .groups = "drop")

    topic_names <- as.list(dplyr::pull(d_wide, terms))
    topic_domain <- list()
    topic_addition <- list()
    seeded_topics <- list()
  }

  for (t in 1:settings$nr_topics) {
    output <- data.frame()
    strength <- 0
    concerns <- 0
    nr_sentences_in_topic <- 0

    topic_column <- as.character(t)

    output <- sentence_complete_output[sentence_complete_output[[topic_column]] >= threshold &
                                         sentence_complete_output[[topic_column]] >= 0.01, ]

    if (nrow(output) > 0) {
      output$perc <- 100 * output[[topic_column]] / max_probability
      output$competency <- competencies[as.character(output$competencyid)]
      output$coschap <- portfolio_names[as.character(output$portfolioid)]

      output$score <- suppressWarnings(as.numeric(output$score))
      output$score <- ifelse(
        is.na(output$score),
        NA,
        sprintf(output$score / ((settings$scale_max - settings$scale_min) / (settings$score_max - settings$score_min)) +
                  (settings$score_min - settings$scale_min), fmt = '%#.2f')
      )

      output <- output[, c("sentenceid", "sentence", "portfolioid", "coschap", "feedbacktype", "datereferenced", "perc", "polarity", "score",
                           "competencyid", "competency", "comment", "submissionid", "zorgpunt")]

      selected_ids <- c(selected_ids, output$sentenceid)
      output <- dplyr::distinct_at(output, dplyr::vars(-.data$sentenceid))

      output <- output[order(output$perc, decreasing = TRUE), ]

      nr_sentences_in_topic <- nrow(output)

      output$datesnumeric <- as.numeric(as.POSIXct(output$datereferenced))
      max_date <- max(output$datesnumeric, na.rm = TRUE)
      min_date <- min(output$datesnumeric, max(output$datesnumeric) - settings$nominalDurationEducation)
      output$datedistance <- (output$datesnumeric - min_date) / (2 * (max_date - min_date)) + 1 / 2

      strength <- sum(output$perc * output$polarity * output$datedistance) / sum(output$perc * output$datedistance)
      concerns <- sum(output$zorgpunt)
    } else {

      output <- data.table("sentenceid"=numeric(), "sentence"=character(), "portfolioid"=numeric(),
                           "coschap"=character(), "feedbacktype"=character(), "datereferenced"=numeric(),
                           "perc"=numeric(), "polarity"=numeric(), "score"=numeric(),
                           "competencyid"=numeric(), "competency"=character(), "comment"=character(),
                           "submissionid"=numeric(), "zorgpunt"=numeric())
    }

    topic_list <- list(
      "topic_domain" = unlist(topic_domain)[t],
      "topic_name" = unlist(topic_names)[t],
      "topic_description" = unlist(topic_addition)[t],
      "topic_seeds" = unlist(unlist(seeded_topics, recursive = FALSE), recursive = FALSE)[t],
      "feedback_count" = nr_sentences_in_topic,
      "quantity_cat" = 0,
      "strength" = strength,
      "quality_cat" = '',
      "concerns" = concerns,
      "feedback" = output
    )

    assigned_sentences <- assigned_sentences + nr_sentences_in_topic
    all_topics[[t]] <- topic_list
  }

  avg_nr_sentences <- assigned_sentences / settings$nr_topics

  for (t in 1:settings$nr_topics) {
    if (all_topics[[t]]$feedback_count <= 5) {
      all_topics[[t]]$quality_cat <- ''
      all_topics[[t]]$quantity_cat <- 1
      if (all_topics[[t]]$feedback_count == 0) {
        all_topics[[t]]$quantity_cat <- 0
      }
    } else {
      if (all_topics[[t]]$feedback_count <= 0.5 * avg_nr_sentences) {
        all_topics[[t]]$quantity_cat <- 2
      } else if (all_topics[[t]]$feedback_count <= 1.5 * avg_nr_sentences) {
        all_topics[[t]]$quantity_cat <- 3
      } else {
        all_topics[[t]]$quantity_cat <- 4
      }

      if (all_topics[[t]]$strength >= 0.1) {
        all_topics[[t]]$quality_cat <- 'positive'
      } else if (all_topics[[t]]$strength < 0) {
        all_topics[[t]]$quality_cat <- 'negative'
      }
    }
  }

  additional_sentences <- sentence_complete_output[!(sentence_complete_output$sentenceid %in% selected_ids), ]
  if (nrow(additional_sentences) > 0) {
    additional_sentences$perc <- 100
    additional_sentences$competency <- competencies[as.character(additional_sentences$competencyid)]
    additional_sentences$coschap <- portfolio_names[as.character(additional_sentences$portfolioid)]
    additional_sentences$score <- suppressWarnings(as.numeric(additional_sentences$score))
    additional_sentences$score <- ifelse(
      is.na(additional_sentences$score),
      NA,
      sprintf(additional_sentences$score / ((settings$scale_max - settings$scale_min) / (settings$score_max - settings$score_min)) +
                (settings$score_min - settings$scale_min), fmt = '%#.2f')
    )

    additional_sentences <- additional_sentences[, c("sentenceid", "sentence", "portfolioid", "coschap", "feedbacktype", "datereferenced", "perc",
                                                     "polarity", "score", "competencyid", "competency", "comment", "submissionid", "zorgpunt")]
    additional_sentences <- dplyr::distinct_at(additional_sentences, dplyr::vars(-.data$sentenceid))

    topic_list <- list(
      "topic_domain" = "Overige feedback",
      "topic_name" = "Overige feedback",
      "topic_description" = "Feedback die niet gekoppeld is aan de vastgestelde onderwerpen",
      "topic_seeds" = "",
      "feedback_count" = "",
      "quantity_cat" = "",
      "strength" = "",
      "quality_cat" = "",
      "concerns" = 0,
      "feedback" = additional_sentences
    )
    all_topics[[settings$nr_topics + 1]] <- topic_list
  }

  if(settings$output_json == TRUE) {
    return (jsonlite::toJSON(list(studentid = settings$filter_student_ID, timestamp = format(Sys.time(), "%Y%m%d_%H%M%S"), topics = all_topics), pretty = TRUE))
  }

  return(all_topics)
}
