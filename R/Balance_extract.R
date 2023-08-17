#' Balance extraction
#'
#' This function extracts the balance criteria or pairwise effects of a 
#' Balance_MAGMA result for a specified sample size.
#'
#' Based on a previous computed Balance_MAGMA object this function enables the
#' extraction of balance criteria or pairwise effects for each desired sample 
#' size. Thus, checking the balance for each possible sample size independently
#'  is possible.
#'
#' @param Balance A Balance_MAGMA object. Compare the function Balance_MAGMA.
#' @param samplesize An integer indicating the sample size for which the balance
#' criteria or pairwise effects hould be extracted.
#' @param effects Indicates whether balance criteria or pairwise effects should
#' be extracted. The default *FALSE* returns the balance criteria, while *TRUE*
#' leads to the extraction of the pairwise effects.
#'
#'
#' @author Julian Urban
#'
#' @import tidyverse 
#'
#' @return Depends on effects argument. If *FALSE* it returns in a vector
#' containing the balance criteria. If *TRUE* it returns a vector containing
#' all possible pairwise effects.
#' @export
#'
#' @examples
#' \dontrun {
#' #Defining the names of the metric and binary covariates
#' covariates_vector <- c("GPA_school", "IQ_score", "Motivation", "parents_academic", "sex")
#'
#' #Matching data set for giftedness support with exact matching for enrichment.
#' MAGMA_sim_data_gifted_exact <- MAGMA_exact(Data = MAGMA_sim_data,
#'                                            group = "gifted_support",
#'                                            dist = "ps_gifted",
#'                                            exact = "enrichment",
#'                                            cores = 2)
#'
#'
#' #Estimating Balance
#' Balance_gifted_exact <- Balance_MAGMA(data = MAGMA_sim_data_gifted_exact,
#'                                       group = "gifted_support",
#'                                       covariates = covariates_vector,
#'                                       step = "step") #step created during matching
#' str(Balance_gifted_exact)
#'
#' #Balance criteria for a 100 cases per group
#' #Balance criteria
#' Balance_100_gifted <- Balance_extract(Balance = Balance_gifted_exact,
#'                                       samplesize = 100,
#'                                       effects = FALSE)
#' Balance_100_gifted
#' 
#' #Pairwise Effects
#' Balance_100_gifted_effects <- Balance_extract(Balance = Balance_gifted_exact,
#'                                               samplesize = 100,
#'                                               effects = TRUE)
#' Balance_100_gifted_effects
#'
#' #Computing 2x2 Matching for giftedness support and enrichment equivalent to
#' #a four group matching
#' MAGMA_sim_data_gift_enrich <- MAGMA(Data = MAGMA_sim_data,
#'                                    group = c("gifted_support", "enrichment"),
#'                                    dist = "ps_2x2",
#'                                    cores = 2)
#'
#'
#' #Estimating Balance. Covariates same as above
#' Balance_2x2 <- Balance_MAGMA(data = MAGMA_sim_data_gift_enrich,
#'                              group = c("gifted_support", "enrichment"),
#'                              covariates = covariates_vector,
#'                              step = "step") #step created during matching
#' str(Balance_2x2)
#'
#' #Balance criteria for a 125 cases per group
#' #Balance criteria
#' Balance_125_2x2 <- Balance_extract(Balance = Balance_2x2,
#'                                       samplesize = 125,
#'                                       effects = FALSE)
#' Balance_125_2x2
#' 
#' #Pairwise Effects
#' Balance_125_2x2_effects <- Balance_extract(Balance = Balance_2x2,
#'                                               samplesize = 125,
#'                                               effects = TRUE)
#' Balance_125_2x2_effects
#' }
#'
Balance_extract <- function(Balance, samplesize, effects = FALSE) {
  #Check input
  if (!is_list(Balance)) {
    stop("Balance needs to be a Balance_MAGMA object!")
  }
  if (samplesize > length(Balance$Pillai)) {
    stop(paste("Samplesize exceeds the maximum size of ", length(Balance$Pillai),
               ". Please specify a lower samplesize", sep = ""))
  }

  if(effects == FALSE) {
    if(!is.matrix(Balance$Pillai)) {
      Balance_criteria <- c(Balance$Pillai[samplesize],
                            Balance$d_ratio$d_rate[samplesize],
                            Balance$mean_effect[samplesize],
                            Balance$adjusted_d_ratio[samplesize]) %>%
        round(., 2) %>%
        purrr::set_names("Pillai's Trace",
                  "d-ratio",
                  "mean g",
                  "adj. d-ratio")
      
    } else if(is.matrix(Balance$Pillai)) {
      Balance_criteria <- c(Balance_2x2$Pillai[1, samplesize],
                            Balance_2x2$Pillai[2, samplesize],
                            Balance_2x2$Pillai[3, samplesize],
                            Balance$d_ratio$d_rate[samplesize],
                            Balance$mean_effect[samplesize],
                            Balance$adjusted_d_ratio[samplesize]) %>%
        round(., 2) %>%
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
      round(., 2)
    return(Balance_effects)  
  }

}
















