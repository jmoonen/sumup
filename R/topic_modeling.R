#' @title Topic Modeling with Latent Dirichlet Allocation (LDA)
#' @description This function performs topic modeling on word count data using Latent Dirichlet Allocation (LDA). It supports both standard LDA and seeded LDA, where predefined topics can guide the topic modeling process.
#'
#' @param word_counts A data frame or data.table containing word counts, with columns for the document ID (`mID`), word (`word`), and count (`n`).
#' @param seeded_topics A list of character vectors representing predefined terms for each seed topic. If provided, seeded LDA will be performed.
#' @param seed_weight A numeric value indicating the weight assigned to the seeded terms in the LDA model. This parameter influences how strongly the predefined seed topics affect the topic modeling.
#' @param nr_topics An integer specifying the number of topics to be modeled by the LDA algorithm.
#' @param set_seed A numeric value setting the seed for the topic modeling algorithm. Default set to 1234.
#' @param lda_seed A numeric seed to be set for Gibbs Sampling. Default set to 1000.
#' @param lda_alpha A numeric value that set the initial value for alpha.
#' @param lda_best If TRUE only the model with the maximum (posterior) likelihood is returned, by default equals TRUE.
#' @param lda_burnin A number of omitted Gibbs iterations at beginning, by default equals 0.
#' @param lda_verbose A numeric value. If a positive integer, then the progress is reported every verbose iterations. If 0 (default), no output is generated during model fitting.
#' @param lda_iter Number of Gibbs iterations (after omitting the burnin iterations), by default equals 2000.
#' @param lda_thin Number of omitted in-between Gibbs iterations, by default equals iter.
#'
#' @return A topicmodels LDA object containing the result of the topic modeling. This object includes the topic distribution for each document and the terms associated with each topic.
#'
#' @details
#' The `topic_modeling` function performs topic modeling using Latent Dirichlet Allocation (LDA) on a document-term matrix (DTM). If `seeded_topics` is provided, a seeded LDA approach is used where predefined topics help guide the model's generation of topics. The function supports:
#' - **Standard LDA**: Uses the traditional Gibbs sampling approach to estimate topics from word counts.
#' - **Seeded LDA**: Incorporates predefined seed terms into the LDA model by assigning a weight (`seed_weight`) to these terms.
#'
#' The function uses the `topicmodels` package for LDA and the `slam` package to manipulate sparse matrices. The results are captured in an LDA model object, which contains topic-word distributions and document-topic assignments.
#'
#' @export

topic_modeling <- function(word_counts, seeded_topics, seed_weight, nr_topics, set_seed, lda_seed, lda_alpha, lda_best, lda_burnin, lda_verbose, lda_iter, lda_thin) {

  check_dataset(word_counts, type_of_dataset = "word_counts")

  ### Latent Dirichlet Allocation
  sentences_dtm <- tidytext::cast_dtm(word_counts, .data$mID, .data$word, .data$n)

  if(length(seeded_topics) > 0) {
    seeded_topics = unlist(seeded_topics, recursive=FALSE)

    ### Seeded LDA
    terms <- colnames(sentences_dtm)

    i <- c()
    j <- c()
    for(index in 1:length(seeded_topics)) {
      all_matches <- match(seeded_topics[[index]], terms) # find the terms' indices in the columns of the DTM
      found_matches <- which(is.na(all_matches) == FALSE, arr.ind=TRUE) #only use the seeded topics found in terms
      i <- c(i, rep(index, length(found_matches))) # define the topic numbers
      j <- c(j, all_matches[found_matches])
    }

    if (!requireNamespace("slam", quietly = TRUE)) {
      stop("Package 'slam' must be installed to use this function.", call. = FALSE)
    }
    deltaS <- slam::simple_triplet_matrix(i, j, v = rep(seed_weight, length(i)), nrow = nr_topics, ncol = ncol(sentences_dtm))

    if(!is.null(lda_seed)){
      set.seed(lda_seed)
    }

    ### Create a k-topic LDA model
    lda_result <- capture.output({
      sentences_lda <- topicmodels::LDA(sentences_dtm, k = nr_topics, method = "Gibbs",
                                     seedwords = deltaS,
                                     control = list(alpha = lda_alpha, best = lda_best, verbose = lda_verbose, burnin = lda_burnin, iter = lda_iter, thin = lda_thin))
    })

  } else {
    ### Create a k-topic LDA model
    lda_result <- capture.output({
      sentences_lda <- topicmodels::LDA(sentences_dtm, k = nr_topics, control = list(seed = set_seed))
    })
  }

  return (sentences_lda)
}
