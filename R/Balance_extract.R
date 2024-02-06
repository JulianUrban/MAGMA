#' Balance_extract
#'
#' This function extracts the balance criteria or pairwise effects of a
#' \code{\link{Balance_MAGMA}} result for a specified sample size.
#'
#' Given a previous computed \code{\link{Balance_MAGMA}} object, this function
#' enables the extraction of balance criteria or pairwise effects for any
#' desired sample size. This makes it possible to independently check the
#' balance for each possible sample size.
#'
#' @param Balance A result of \code{\link{Balance_MAGMA}} See the function
#' \code{\link{Balance_MAGMA}} for details.
#' @param samplesize An integer indicating the sample size for which the balance
#' criteria or pairwise effects should be extracted.
#' @param effects Indicates whether balance criteria or pairwise effects should
#' be extracted. The default value *FALSE* returns the balance criteria, while
#' *TRUE* leads to the extraction of pairwise effects.
#'
#'
#' @author Julian Urban
#'
#' @import tidyverse
#' @importFrom purrr set_names
#' @importFrom rlang is_list
#'
#' @return Depends on the effects argument. If *FALSE*, it returns in a vector
#' containing the balance criteria. If *TRUE*, it returns a vector containing
#' all possible pairwise effects.
#' @export
#'
#' @examples
#' 
#' # Defining the names of the metric and binary covariates
#' covariates_vector <- c("GPA_school", "IQ_score", "Motivation", "parents_academic", "gender")
#'
#'
#' # Estimating balance of a two-group matching using the data set
#' # 'MAGMA_sim_data'.
#' # Matching variable 'gifted_support' (received giftedness support yes or no)
#' Balance_gifted <- Balance_MAGMA(Data = MAGMA_sim_data,
#'                                 group = "gifted_support",
#'                                 covariates = covariates_vector,
#'                                 step = "step_gifted")
#'
#' # Balance criteria for 100 cases per group
#' # Balance criteria
#' Balance_100_gifted <- Balance_extract(Balance = Balance_gifted,
#'                                       samplesize = 100,
#'                                       effects = FALSE)
#' Balance_100_gifted
#'
#' # Pairwise effects
#' Balance_100_gifted_effects <- Balance_extract(Balance = Balance_gifted,
#'                                               samplesize = 100,
#'                                               effects = TRUE)
#' Balance_100_gifted_effects
#'
#' \donttest{
#' # 2x2 matching using the data set 'MAGMA_sim_data'
#' # Matching variables are 'gifted_support' (received giftedness support yes
#' # or no) and 'enrichment' (participated in enrichment or not)
#' # 'MAGMA_sim_data_gift_enrich' contains the result of the matching
#' # 2x2 matching is equivalent to four-group matching
#' MAGMA_sim_data_gift_enrich <- MAGMA(Data = MAGMA_sim_data,
#'                                    group = c("gifted_support", "enrichment"),
#'                                    dist = "ps_2x2",
#'                                    cores = 2)
#'
#'
#' # Estimating balance. Covariates same as above
#' Balance_2x2 <- Balance_MAGMA(Data = MAGMA_sim_data_gift_enrich,
#'                              group = c("gifted_support", "enrichment"),
#'                              covariates = covariates_vector,
#'                              step = "step") # Step created during matching
#' str(Balance_2x2)
#'
#' # Balance criteria for 125 cases per group
#' # Balance criteria
#' Balance_125_2x2 <- Balance_extract(Balance = Balance_2x2,
#'                                       samplesize = 125,
#'                                       effects = FALSE)
#' Balance_125_2x2
#'
#' # Pairwise effects
#' Balance_125_2x2_effects <- Balance_extract(Balance = Balance_2x2,
#'                                               samplesize = 125,
#'                                               effects = TRUE)
#' Balance_125_2x2_effects
#' }
#'
Balance_extract <- function(Balance, samplesize, effects = FALSE) {
  #Check input
  if (!rlang::is_list(Balance)) {
    stop("Balance needs to be a Balance_MAGMA object!")
  }
  if (samplesize > length(Balance$adjusted_d_ratio)) {
    stop(paste("Samplesize exceeds the maximum size of ", length(Balance$Pillai),
               ". Please specify a lower samplesize", sep = ""))
  }

  if(effects == FALSE) {
    if(!is.matrix(Balance$Pillai)) {
      Balance_criteria <- c(Balance$Pillai[samplesize],
                            Balance$d_ratio$d_rate[samplesize],
                            Balance$mean_effect[samplesize],
                            Balance$adjusted_d_ratio[samplesize]) %>%
        round(digits = 2) %>%
        purrr::set_names("Pillai's Trace",
                  "d-ratio",
                  "mean g",
                  "adj. d-ratio")

    } else if(is.matrix(Balance$Pillai)) {
      Balance_criteria <- c(Balance$Pillai[1, samplesize],
                            Balance$Pillai[2, samplesize],
                            Balance$Pillai[3, samplesize],
                            Balance$d_ratio$d_rate[samplesize],
                            Balance$mean_effect[samplesize],
                            Balance$adjusted_d_ratio[samplesize]) %>%
        round(digits = 2) %>%
        purrr::set_names("Pillai's Trace ME 1",
                  "Pillai's Trace ME 2",
                  "Pillai's Trace IA",
                  "d-ratio",
                  "mean g",
                  "adj. d-ratio")
    }

    return(Balance_criteria)

  } else if(effects == TRUE) {
    Balance_effects <- Balance$d_ratio$effects[ , samplesize] %>%
      round(digits = 2)
    return(Balance_effects)
  }

}
















