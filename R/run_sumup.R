#' @title Run Sum Up
#' @description This function runs a series of text processing and analysis steps including text cleaning, tokenization, lemmatization, topic modeling, and sentiment analysis. It then classifies sentences into topics and generates an output summarizing the results.
#'
#' @param dataset A data frame containing the text data to be analyzed. It should include at least the following columns: `sentenceid`, `sentence`, `portfolioid`, `competencyid`, `feedbacktype`, and `datereferenced`.
#' @param settings A list containing settings for various processing steps. If not provided, default settings are used.
#'
#' @return A list or JSON output (depending on settings) containing the processed text data classified by topics and sentiment, as well as various metrics related to topics, such as strength and feedback count.
#'
#' @details This function performs the following steps:
#'   - Cleans the input text data using `text_clean`.
#'   - Tokenizes the text into sentences and removes stopwords.
#'   - Lemmatizes and annotates the sentences using a UDPipe model.
#'   - Counts word frequencies and excludes stopwords.
#'   - Performs topic modeling on the word counts.
#'   - Runs sentiment analysis based on the specified method (Grasp or SentimentR).
#'   - Classifies sentences into topics using the topic classification model.
#'   - Generates output summarizing the topics and sentiment.
#'
#' @examples
#' data(example_data)
#' ex_data <- example_data
#' ex_settings  <- set_default_settings()
#' ex_settings  <- update_setting(ex_settings , "language", "en")
#' ex_settings  <- update_setting(ex_settings , "use_sentiment_analysis", "sentimentr")
#' result <- run_sumup(ex_data, ex_settings )
#'
#' @export

run_sumup <- function(dataset, settings = NULL) {
  # Settings
  if(is.null(settings)) {
    settings <- set_default_settings()
  } else {
    check_all_settings(settings)
  }

  # Clean the data
  narratives <- text_clean(dataset, settings$corrections_file)

   # Unnest tokens in sentences and words, and remove stopwords
  sentences <- tidy_text(narratives, settings$split_in_sentences)
  stopwords <- default_stopwords(settings$stopwords_file)

  # Lemmatize and annotate dataset
  data_annotated <- lemmatize(sentences, settings$udpipe_model_file, settings$corrections_file, settings$language, settings$use_seeds)
  word_counts <- obtain_word_counts(data_annotated, stopwords, settings$stopwords_to_append)

  # Topic modeling
  sentences_lda <- topic_modeling(word_counts, settings$seeded_topics, settings$seed_weight, settings$nr_topics, settings$set_seed, settings$lda_seed, settings$lda_alpha, settings$lda_best, settings$lda_burnin, settings$lda_verbose, settings$lda_iter, settings$lda_thin)

  # Sentiment analysis
  if(settings$use_sentiment_analysis == "grasp") {
    sentence_polarity <-sentiment_analysis_grasp(unique(narratives$document), sentences, settings$language, settings$grasp_folder)
  } else {
    sentence_polarity <-sentiment_analysis_sentimentr(unique(narratives$document), sentences, settings$dictionary_file, settings$use_dictionary_file_in_sentimentr)
  }

  # Assign sentences to topics and create the results
  sentence_complete_output <- topic_classification(dataset, narratives, sentences, sentences_lda, sentence_polarity, data_annotated, settings$use_beta, settings$use_seeds, settings$nr_topics, settings$seeded_topics, settings$competencies)

 return (create_output(sentence_complete_output, sentences_lda, settings))

}

