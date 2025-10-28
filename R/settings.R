#' @title Settings functionality for package 'sumup'
#' @description This file contains helper functions used throughout package 'sumup' to set and update the settings of the algorithm.
#' @author Joyce M.W. Moonen - van Loon
#'
#' This file defines  functions used to set and update the settings used in the 'sumup' algorithms
#' Functions in this file:
#' - set_default_settings(): Set the default settings list of the package (optimized for a Dutch Master in Medicine education using CanMeds competencies and professional activities)
#' - update_educational_framework_settings(settings): Update settings related to the educational framework
#' - update_setting(settings, setting_name, new_value): Updates setting 'setting_name' with value 'new_value', returning the new list of settings
#' - check_all_settings(settings): Check whether all settings are of the correct type and fit the required ranges
#' - check_setting(settings, setting_name, setting_value):  Check whether setting_value is allowed for setting 'setting_name'
#'
#' Usage:
#' Load this package and call the functions directly.
#'
#' Note: This file is part of the 'sumup' package.

# ===================================
# set_default_settings: Set the default settings
# ===================================

#' @title Set default settings
#' @description Create a list of settings for the algorithms in 'sumup'
#' @return A list containing all settings used in 'sumup'
#' @export

set_default_settings <- function(){
  settings <- list(

    ### Set the language of the feedback. Options: "nl", "en", "de", "fr"
    language = "nl",

    ### Locations of input files, location of folder in which Python-tool grasp is installed, and output folder
    corrections_file = system.file("extdata", "SumUp_Corrections.csv", package = "sumup"), #"inst/extdata/SumUp_Corrections.csv",
    stopwords_file = system.file("extdata", "SumUp_Stopwords.csv", package = "sumup"), #"inst/extdata/SumUp_Stopwords.csv",
    udpipe_model_file = "",
    grasp_folder = tempdir(), # Needed for sentiment analysis "grasp"
    dictionary_file = system.file("extdata", "SumUp_Dictionary_nl.csv", package = "sumup"), #"inst/extdata/SumUp_Dictionary.csv", # Needed for sentiment analysis "sentimentr"
    seeds_file = system.file("extdata", "SumUp_Seeds_Competencies.json", package = "sumup"), #"inst/extdata/SumUp_Seeds_Competencies.json", # (# Encoding = "UTF-8") SumUp_Seeds_ProfAct.json for pre-set professional activities
    out_dir = tempdir(),

    ### User settings (input from portfolio system). When using a data set containing data of multiple students, add the id of the student of interest.
    # Use value -1 to include all students in the data set
    filter_student_id = -1,

    ### Educational settings
    # The nominal duration of the education in seconds (3*365*24*60*60), used to give more recent feedback a larger weight
    nominal_duration_education = 94608000,
    # Names of the clerkships/rotations per id as used in the data set
    portfolio_names = list(list(
      "1" = "GEZP", "2" = "GEZP", "3" = "WESP", "4" = "WESP", "6" = "Beschouwend",
      "7" = "Snijdend", "8" = "Vrouw, Moeder en Kind", "9" = "Neurowetenschappen", "10" = "Huisartsgeneeskunde en Sociale geneeskunde",
      "15" = "Extra Programma 1", "16" = "Extra Programma 2"
    )),
    # Names of the competencies per id as used in the data set
    competencies = list(list(
      "10" = "Medische deskundigheid",
      "20" = "Communicatie",
      "30" = "Samenwerking",
      "40" = "Wetenschappelijk denken",
      "50" = "Maatschappelijk handelen",
      "60" = "Leiderschap",
      "70" = "Professionaliteit",
      "NULL" = "NA"
    )),
    # Score ranges, in which ranges in the data set may differ from the output data
    scale_max = 120, # Maximum score in the data set
    scale_min = 0, # Maximum score in the data set
    score_max = 5, # Maximum score of the presented data
    score_min = 1, # Maximum score of the presented data

    ### Topic modeling settings
    # List of stopwords to exclude from topic modeling. Default values set for Dutch feedback from medical education.
    # stopwords_to_append <- c("goede", "goed", "bijvoorbeeld", "soms", "wel", "alle", "co", "schap", "coschap", "tijdens", "assistent", "duidelijk", "duidelijke", "kort", "huisarts", "nvt"),
    # Either split the feedback comments in sentences (TRUE) or keep as comment (FALSE)
    split_in_sentences = TRUE,
    # Set to TRUE when you use seeds as input that match the education framework
    use_seeds = TRUE,

    # Control parameters for an object of class "LDAcontrol" when using seedwords
    # Weight of the seeds. Adjust to the size of the data set; small data sets need larger seed weight to implement the educational framework
    seed_weight = 10 - 0.1,
    # Seed to be set for Gibbs Sampling
    lda_seed = 1000,
    # Initial value for alpha (numeric)
    lda_alpha = 0.1,
    # If TRUE only the model with the maximum (posterior) likelihood is returned, by default equals TRUE.
    lda_best = TRUE,
    # Number of omitted Gibbs iterations at beginning, by default equals 0.
    lda_burnin = 500,
    # If a positive integer, then the progress is reported every verbose iterations. If 0 (default), no output is generated during model fitting.
    lda_verbose = 500,
    # Number of Gibbs iterations (after omitting the burnin iterations), by default equals 2000.
    lda_iter = 100,
    # Number of omitted in-between Gibbs iterations, by default equals iter.
    lda_thin = 100,

    # Set the default seed value for the LDA when no seedwords are used (use_seeds = FALSE). Set to NA is no seed is needed. Used to set the seed in the external code for VEM estimation and to call set.seed for Gibbs sampling. For Gibbs sampling it can also be set to NA (default) to avoid changing the seed of the random number generator in the model fitting call.
    set_seed = 1234,

    # Threshold of minimal relevance for assigning feedback to topic (between 0 and 1)
    assignment_threshold = 0.4,
    # The number of topics as output from topic modeling without using seeds
    nr_topics = 5L,
    # The number of top terms per topic as output from topic modeling without using seeds
    nr_top_terms = 5L,

    # Without using seeds, set these lists empty. When using seeds, add this information in 'seeds_file'
    topic_names = list(),
    topic_domain = list(),
    topic_addition = list(),
    seeded_topics = list(),  # Fill seeded_topics to seed the LDA (ensure terms are in the DTM and not part of stopwords)

    # Sentiment analysis settings. Options: "grasp", "sentimentr"
    use_sentiment_analysis = "grasp",
    use_dictionary_file_in_sentimentr = TRUE,

    # Output either json or Rmarkdown
    output_json = TRUE
  )

  # Based on values of use_all_competencies, use_seeds, and seeds_file, update the settings relating to the educational framework
  settings = update_educational_framework_settings(settings)

  # Return the settings
  return(settings)
}


# ===================================
# update_educational_framework_settings: Update settings related to the educational framework
# ===================================

#' @title Update settings related to the educational framework
#' @description When changing the seeds_file or use_seeds settings, update all parameters relating to the educational framework
#' @param settings A current list of settings (list)
#'
#' @return An updated list containing all settings used in 'sumup'
#' @export

update_educational_framework_settings <- function(settings){
  # Retrieve the educational model, if set to use
  json_seeds = NULL

  reset = FALSE
  if (settings[["use_seeds"]]) {
    if (!file.exists(settings[["seeds_file"]])) {
      warning(sprintf("Warning: File '%s' does not exist.", settings[["seeds_file"]]))
    } else {
      json_seeds = jsonlite::fromJSON(settings[["seeds_file"]])
    }
  }

  # Update settings if json_seeds exists
  if (settings[["use_seeds"]] && exists("json_seeds")) {
    settings[["seeded_topics"]] = list(list(as.list(json_seeds$seeded_topics)))
    settings[["topic_names"]] = list(list(as.character(json_seeds$topic_names)))
    settings[["topic_domain"]] = list(list(as.character(json_seeds$topic_domain)))
    settings[["topic_addition"]] = list(list(as.character(json_seeds$topic_addition)))
    settings[["nr_topics"]] = length(json_seeds$seeded_topics)
  } else {
    reset = TRUE
  }

  if(reset == TRUE) {
    if(settings[["use_seeds"]]) {
      warning("Setting 'use_seeds' is set to FALSE.")
    }
    settings[["use_seeds"]] = FALSE
    settings[["seeded_topics"]] = list()
    settings[["topic_names"]] = list()
    settings[["topic_domain"]] = list()
    settings[["topic_addition"]] = list()
  }
  return (settings)
}

# ===================================
# update_setting: Update one particular setting
# ===================================

#' @title Update setting
#' @description Replace the value of one particular setting value
#' @param settings A current list of settings (list)
#' @param setting_name The name of the setting that needs updating (string)
#' @param new_value The new value of the setting
#'
#' @return A list containing all settings used in 'sumup'
#' @export

update_setting <- function(settings, setting_name, new_value) {
  if(check_setting(settings, setting_name, new_value)) {
    settings[[setting_name]] <- new_value

    if (setting_name %in% c("seeds_file", "use_seeds", "seeded_topics", "topic_names", "topic_domain", "topic_addition")) {
      settings = update_educational_framework_settings(settings)
    }
  } else {
    stop("Setting could not be updated.")
  }
  return(settings)
}

# ===================================
# check_all_settings: Check whether all settings are of the correct type and fit the required ranges
# ===================================

#' @title Check all setting
#' @description Check for every setting the type and value
#' @param settings A current list of settings (list)
#'
#' @return Either an error or TRUE
#' @export

check_all_settings <- function(settings) {
  for(setting_name in names(settings)) {
    if(!check_setting(settings, setting_name, settings[[setting_name]])) {
      stop(paste("Setting incorrect:", setting_name))
    }
  }
  return(TRUE)
}

# ===================================
# check_setting: Check one particular setting
# ===================================

#' @title Check setting
#' @description Check the value of one particular setting
#' @param settings A current list of settings (list)
#' @param setting_name The name of the setting that needs updating (string)
#' @param setting_value The value of the setting
#'
#' @return TRUE if correct, FALSE if incorrect
#' @export

check_setting <- function(settings, setting_name, setting_value) {
  # Define expected types for each setting
  expected_types <- list(
    language = "character",
    corrections_file = "character",
    stopwords_file = "character",
    udpipe_model_file = "character",
    grasp_folder = "character",
    dictionary_file = "character",
    seeds_file = "character",
    out_dir = "character",
    filter_student_id = c("numeric", "character"),
    nominal_duration_education = "numeric",
    portfolio_names = "list",
    competencies = "list",
    scale_max = "numeric",
    scale_min = "numeric",
    score_max = "numeric",
    score_min = "numeric",
    stopwords_to_append = "character",
    split_in_sentences = "logical",
    use_seeds = "logical",

    seed_weight = "numeric",
    lda_seed = "numeric",
    lda_alpha = "numeric",
    lda_best = "logical",
    lda_burnin = "numeric",
    lda_verbose = "numeric",
    lda_iter = "numeric",
    lda_thin = "numeric",

    set_seed = "numeric",
    assignment_threshold = "numeric",
    nr_topics = "integer",
    nr_top_terms = "integer",
    topic_names = "list",
    topic_domain = "list",
    topic_addition = "list",
    seeded_topics = "list",

    use_sentiment_analysis = "character",
    use_dictionary_file_in_sentimentr = "logical",
    output_json = "logical"
  )

  # Check if the setting exists in the predefined list
  if (!setting_name %in% names(expected_types)) {
    stop(paste("Unknown setting:", setting_name))
  }

  # Check if the setting is present in `settings`
  if (!setting_name %in% names(settings)) {
    warning(paste("Missing setting:", setting_name))
    return(FALSE)
  }

  # Get expected type(s) and actual value
  expected_type <- expected_types[[setting_name]]

  # Check if the actual type matches the expected type(s)
  if (!any(sapply(expected_type, function(t) inherits(setting_value, t)))) {
    warning(paste("Incorrect type for setting:", setting_name,
                  "- Expected:", paste(expected_type, collapse = " or "),
                  "- Found:", class(setting_value)))
    return(FALSE)
  }

  # Additional value constraints
  if (setting_name == "language" && !setting_value %in% c("nl", "en", "de", "fr")) {
    warning("Invalid value for 'language'. Must be one of: 'nl', 'en', 'de', 'fr'.")
    return(FALSE)
  }

  if (setting_name %in% c("nominal_duration_education", "scale_max", "scale_min",
                          "score_max", "score_min", "nr_topics", "nr_top_terms", "seed_weight")) {
    if (setting_value < 0) {
      warning(paste("Value for", setting_name, "must be positive."))
      return(FALSE)
    }
  }

  if (setting_name == "assignment_threshold" && (setting_value < 0 || setting_value > 1)) {
    warning("Value for 'assignment_threshold' must be between 0 and 1.")
    return(FALSE)
  }

  if (setting_name == "scale_max" && setting_value <= settings$scale_min) {
    warning("Value for 'scale_max' must be larger than 'scale_min'.")
    return(FALSE)
  }

  if (setting_name == "score_max" && setting_value <= settings$score_min) {
    warning("Value for 'score_max' must be larger than 'score_min'.")
    return(FALSE)
  }

  if (setting_name == "use_sentiment_analysis" && !setting_value %in% c("grasp", "sentimentr")) {
    warning("Invalid value for 'use_sentiment_analysis'. Must be one of: 'grasp', 'sentimentr'.")
    return(FALSE)
  }

  file_vars <- c("corrections_file", "stopwords_file", "seeds_file", "dictionary_file")
  folder_vars <- c("grasp_folder", "out_dir")

  for (var in file_vars) {
    if (!file.exists(settings[[var]])) {
      warning(sprintf("Warning: File '%s' does not exist.", settings[[var]]))
      return(FALSE)
    }
  }

  for (var in folder_vars) {
    if (!dir.exists(settings[[var]])) {
      warning(sprintf("Warning: Folder '%s' does not exist.", settings[[var]]))
      return(FALSE)
    }
  }

  return(TRUE)  # Return TRUE if the setting has the correct type and valid value
}



