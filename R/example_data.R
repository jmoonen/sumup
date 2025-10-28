#' Example example_data
#'
#' `example_data` contains an example of narrative feedback data and scores of formative assessments
#' from a student's portfolio in the Master education in Medicine from a Dutch university, translated to English.
#'
#' @format A data frame with 6 rows and 15 columns:
#' \describe{
#'   \item{submissionid}{ID of the assessment form from which the data originates (NB: as the data is gathered per competency, the same submissionid can be used for multiple rows of data).}
#'   \item{formid}{ID of the type of assessment form (NB: set to 1 if not applicable).}
#'   \item{templatename}{Name of the type of assessment form (NB: set to same 'dummy' name for all rows, if not applicable).}
#'   \item{assistant}{Identifier of the student. Needed when data from multiple portfolios is loaded. Set all to same value if not applicable.}
#'   \item{authority}{Identifier of the assessor. NB: set all to same value if not applicable.}
#'   \item{specialty}{Textual identifier of the specialty from which the assessment originates (NB: set to same 'dummy' name for all rows if not applicable).}
#'   \item{hospital}{Textual identifier of the hospital from which the assessment originates (NB: set to same 'dummy' name for all rows if not applicable).}
#'   \item{portfolioid}{Identifier of the sub-portfolio, e.g. when using rotations/clerkships. Define the IDs in the settings. NB: set all to 1 if not applicable.}
#'   \item{competencyid}{Identifier of the competency used in the educational framework. Define the IDs in the settings. NB: set all to 1 if not applicable.}
#'   \item{sterk}{Narrative feedback in case it is described in a predefined area for 'strengths' in the performance of the student.}
#'   \item{verbeter}{Narrative feedback in case it is described in a predefined area for 'improvement areas' in the performance of the student.}
#'   \item{feedback}{Narrative feedback in case it is NOT described in a predefined area for 'strengths' or 'improvement area'.}
#'   \item{score}{Numeric score on the same assessment form. The scale can be set in settings. NB: set to NA or NULL if not applicable.}
#'   \item{datereferenced}{Date of the assessment in format yyyy-mm-dd.}
#'   \item{zorgpunten}{Comma-separated list of competencies that are marked as 'concerns' in the performance of the student. Make sure the names of competencies match the values in setting 'competencies'. NB: leave empty if not applicable.}
#' }
#'
#' @source Example data for package documentation
"example_data"
