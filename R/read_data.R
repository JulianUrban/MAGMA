#' MAGMA simulated data set
#'
#' The 'MAGMA.R' simulated data set contains 14 variables of 800 cases. It is
#' used as example in the vignette and the help pages.
#'
#' @format A data frame with 17 variables of 800 cases.
#' \describe{
#' \item{ID}{: Individual ID for each case}
#' \item{gender}{: Binary variable indicating gender of a participant}
#' \item{gifted_support}{: Binary variable that specifies whether a case received
#' giftedness support (1) or not (0)}
#' \item{teacher_ability_rating}{: Three-step ordinal variable ranging from 1 to
#' 3 indicating increasing eacher rated ability of a case}
#' \item{enrichment}{: Binary variable that indicates whether a case participated
#' in an afternoon enrichment program}
#' \item{parents_academic}{: Binary variable that indicates whether at least one
#' parent of a case has an academic background}
#' \item{GPA_school}{: Variable ranging from 1 to 6 that indicates a case's high
#' school GPA. Lower values indicate higher achievement}
#' \item{IQ_score}{: Variable indicating the normed IQ score of a case}
#' \item{Motivation}{: The scale score of a case in a motivational questionnaire}
#' \item{college_GPA}{: variable ranging from 1 to 6 that indicates a case's
#'  college GPA. Lower values indicate higher achievement}
#' \item{support_enrichment}{: Multinomial variable representing the combination
#' of gifted support and enrichment}
#' \item{ps_tar}{: Propensity score of twangs mnps function for
#' teacher_ability_rating}
#' \item{ps_2x2}{: Propensity score of twangs mnps function for
#' support_enrichment}
#' \item{ps_gifted}{: Propensity score of twangs ps function for gifted support}
#' \item{step_gifted}{: Indicates step of MAGMA-matching for gifted support}
#' \item{weight_gifted}{: Indicates weight of MAGMA-matching for gifted support}
#' \item{distance_gifted}{: Indicates distance of MAGMA-matching for gifted
#' support}
#' }
#' @source Simulated data
#'
"MAGMA_sim_data"
