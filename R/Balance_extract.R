#' Balance extracttion
#'
#' This function computes all four Balance criteria of MAGMA, namely
#' *Pillai's Trace*, *d-ratio*, *mean g*, and *adjusted d-ratio*. It can
#' consider binary and metric variables for balance estimation. Balance
#' Estimation is conducted over a varying sample size. See Details for more
#' information.
#'
#' This function computes all four Balance criteria of MAGMA, namely Pillai's
#' Trace, d-ratio, mean g, and adjusted d-ratio. This is an iterative process
#' including more cases with each iteration according to the step variable.
#' Thus, starting with cases having a small within match distance, larger
#' distances are included with increasing iterations. As minimum the function
#' specifies a n >= 20 per group. This does not imply, that balance criteria
#' with such a small sample size can be estimated consistently. For Pillai's
#' Trace a higher minimum sample size can be possible. It depends on the number
#' of covariates to ensure a positive model identification.
#' Missing data for Pillai's Trace are excluded listwise, whiel for the other
#' balance criteria pairwise exclusion is applied.
#'
#' @param data A data frame containing at least the *grouping* variable, the
#' *step* variable from the main MAGMA-function (or other mathcing algorithms),
#'  and all *covariates* of interest.
#' @param group A character specifying the name of
#' your grouping variable in data. Note that MAGMA can only match your data for
#' a maximum of 4 groups. For matching over two groups (e.g., 2x2 Design) is
#' possible by specifying group as a character vector with a length of two. In
#' this case each or the 2 grouping variables can only have two levels.
#' @param covariates A character vector listing the names of all binary and
#' metric covariates of interest.
#' @param step A character naming the step variable of the matching. Per
#' default it specifies the name as *step*, which corresponds the resulting
#' name of the main MAGMA function.
#'
#'
#' @author Julian Urban
#'
#' @import tidyverse psych metafor robumeta
#'
#' @return A list of length four containing all balance criteria and all
#' pairwise effects with respect to group sample size.
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
#' Balance_100_gifted <- c(Balance_gifted_exact$Pillai[100],
#'                         Balance_gifted_exact$d_ratio$d_rate[100],
#'                         Balance_gifted_exact$mean_effect[100],
#'                         Balance_gifted_exact$adjusted_d_ratio[100])
#' Balance_100_gifted
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
#' Balance_100_2x2<- c(Balance_2x2$Pillai[1, 100], #Main Effect 1
#'                     Balance_2x2$Pillai[2, 100], #Main Effect 2
#'                     Balance_2x2$Pillai[3, 100], #Interaction
#'                     Balance_2x2$d_ratio$d_rate[100],
#'                     Balance_2x2$mean_effect[100],
#'                     Balance_2x2$adjusted_d_ratio[100])
#' Balance_100_2x2
#' }
#'
Balance_extract <- function(Balance, samplesize, effects = FALSE) {
  #Check input
  if (!is_list(Balance)) {
    stop("Balance needs to be a Balance_MAGMA object!")
  }
  if(effects == FALSE) {
    if(!is.matrix(Balance$Pillai)) {
      Balance_criteria <- c(Balance$Pillai[samplesize],
                            Balance$d_ratio$d_rate[samplesize],
                            Balance$mean_effect[samplesize],
                            Balance$adjusted_d_ratio[samplesize])
    } else {
      Balance_criteria <- c(Balance_2x2$Pillai[1, samplesize],
                            Balance_2x2$Pillai[2, samplesize],
                            Balance_2x2$Pillai[3, samplesize],
                            Balance$d_ratio$d_rate[samplesize],
                            Balance$mean_effect[samplesize],
                            Balance$adjusted_d_ratio[samplesize])
    }
    
    
  }
}
















