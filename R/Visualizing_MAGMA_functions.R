#' Balance_MAGMA
#'
#' This function computes all four balance criteria of 'MAGMA.R', namely
#' *Pillai's Trace*, *d-ratio*, *mean g*, and *adjusted d-ratio*. The
#' estimation involves both binary and metric variables. Balance
#' estimation is performed across various sample sizes. See Details for more
#' information.
#'
#' This function computes all four balance criteria of 'MAGMA.R', namely
#' Pillai's Trace, d-ratio, mean g, and adjusted d-ratio. This is an iterative
#' process including more cases with each iteration according to the step
#' variable. Thus, starting with cases having a small within-match distance,
#' larger distances are included with increasing iterations. As a minimum the
#' function specifies n >= 20 per group. This does not imply that balance
#' criteria with such a small sample size can be estimated consistently. For
#' Pillai's Trace a higher minimum sample size can be possible. It depends on
#' the number of covariates to ensure a positive model identification.
#' Missing data for Pillai's Trace are excluded listwise, while for the other
#' balance criteria pairwise exclusion is applied.
#'
#' @param Data A data frame containing at least the *grouping* variable, the
#' *step* variable from the main MAGMA-function (or other matching algorithms),
#'  and all *covariates* of interest.
#' @param group A character specifying the name of
#' your grouping variable in data. Note that MAGMA can only match your data for
#' a maximum of four groups. For matching over two grouping variables (e.g., 2x2
#' design) is possible by specifying group as a character vector with a length
#' of two. In this case, each of the two grouping variables can only have two
#' levels.
#' @param covariates A character vector listing the names of all binary and
#' metric covariates of interest.
#' @param step A character specifying the step variable of the matching. Per
#' default, it is set to *step*, which corresponds the resulting
#' name of the main MAGMA function.
#' @param verbose TRUE or FALSE indicating whether matching information should
#' be printed to the console.
#' @param covariates_ordinal A character vector listing the names of all ordinal
#' covariates of interest.
#' @param covariates A character vector listing the names of all nominal
#'  covariates of interest.
#'
#'
#' @author Julian Urban
#'
#' @import tidyverse metafor robumeta tibble dplyr tidyselect
#' @importFrom psych describe
#' @importFrom rlang sym
#'
#' @return A list of length four containing all balance criteria and all
#' pairwise effects with respect to group sample size.
#' @export
#'
#' @references {Pastore, M., Loro, P.A.D., Mingione, M., Calcagni, A. (2022). _overlapping: Estimation of Overlapping in Empirical Distributions_. R package version
#' 2.1, (\url{https://CRAN.R-project.org/package=overlapping}).
#'
#' Revelle, W. (2023). _psych: Procedures for Psychological, Psychometric, and Personality Research_. Northwestern University, Evanston, Illinois. R package version 2.3.6, (\url{https://CRAN.R-project.org/package=psych}).
#'
#' Viechtbauer, W. (2010). Conducting meta-analyses in R with the metafor package. Journal of Statistical Software, 36(3), 1-48. (\doi{10.18637/jss.v036.i03})
#'
#' Fisher, Z., Tipton, E., Zhipeng, H. (2023). _robumeta: Robust Variance Meta-Regression_. R package version 2.1,
#' (\url{https://CRAN.R-project.org/package=robumeta}).}
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
#' # Using subsample only for faster execution.
#' Balance_gifted <- Balance_MAGMA(Data = MAGMA_sim_data[MAGMA_sim_data$step_gifted < 200, ],
#'                                 group = "gifted_support",
#'                                 covariates = covariates_vector,
#'                                 step = "step_gifted")
#' str(Balance_gifted)
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
#'                              step = "step") # step created during matching
#' str(Balance_2x2)
#'
#' }
#'
Balance_MAGMA <- function(Data,
                          group,
                          covariates,
                          step = "step",
                          verbose = TRUE,
                          covariates_ordinal = NULL,
                          covariates_nominal = NULL) {
# Check input
  if (!is.data.frame(Data) && !tibble::is_tibble(Data)) {
    stop("data needs to be list, data frame, or tibble!")
  }

  if(!is.character(group) | length(group) > 2) {
    stop("group needs to be a character vector of maximum length 2!")
  }

  if(!is.character(covariates)) {
    stop("covariates needs to be a character or a character vector!")
  }

  if(!is.character(step) | length(step) > 1) {
    stop("step needs to be a character of length 1!")
  }
  
  if(is.character(covariates_ordinal)) {
    if(sum(covariates_ordinal %in% covariates) > 0) {
      stop("Some variables are specified as covariates and covariates_ordinal. You can only specify each variable to one of theese arguments!")
    }
  }
  
  if(!is.character(covariates_nominal) & !is.null(covariates_nominal)) {
    stop("covariates_nominal needs to be a character or a character vector!")
  }
  
  if(is.character(covariates_nominal)) {
    if(sum(covariates_nominal %in% covariates) > 0) {
      stop("Some variables are specified as covariates and covariates_nominal You can only specify each variable to one of theese arguments!")
    }
  }
  
  if(is.character(covariates_nominal) & is.character(covariates_ordinal))  {
    if(sum(covariates_nominal %in% covariates_ordinal) > 0) {
      stop("Some variables are specified as covariates_ordinal and covariates_nominal. You can only specify each variable to one of theese arguments!")
    }
  }

  if(verbose) {
    cat("\n", "Start estimating Pillai's Trace.")
    }
  ########################
  #####Pillai's Trace#####
  ########################

Pillai <- Pillai_iterativ(da = Data,
                         gr = group,
                         co = covariates,
                         st = step)

if(verbose) {
cat("\n", "Pillai's Trace finished. Starting to compute d-ratio.")
}

if(length(group) == 2) {
  values_1 <- unlist(unique(Data[group[1]]))
  values_2 <- unlist(unique(Data[group[2]]))

  Data <- Data %>%
    dplyr::mutate(group_d = dplyr::case_when(
      !!rlang::sym(group[1]) == values_1[1] &
        !!rlang::sym(group[2]) == values_2[1] ~ 1,
      !!rlang::sym(group[1]) == values_1[1] &
        !!rlang::sym(group[2]) == values_2[2] ~ 2,
      !!rlang::sym(group[1]) == values_1[2] &
        !!rlang::sym(group[2]) == values_2[1] ~ 3,
      !!rlang::sym(group[1]) == values_1[2] &
        !!rlang::sym(group[2]) == values_2[2] ~ 4
    ))

  group <- "group_d"
}


  ########################
  ########d ratio#########
  ########################
  d_effects <- inner_d(da = Data,
                       gr = group,
                       co = covariates,
                       st = step,
                       co_ord = covariates_ordinal,
                       co_nom = covariates_nominal)

  ########################
  ########mean g#########
  ########################
  if(verbose) {
    cat("\n", "d-ratio finished. Starting to compute mean-g.", "\n")
  }
  group_number <- Data %>%
    dplyr::select(tidyselect::all_of(group)) %>%
    table() %>%
    length()
  
  input_g <- d_effects$effects[rownames(d_effects$effects) %in% covariates, ]

  mean_g <- mean_g_meta(input = input_g,
                        number_groups = group_number)

  ########################
  ###likelihhod g < .20###
  ########################
  if(verbose) {
  cat("\n", "Mean g finished. Starting to compute adjusted d-ratio.")
  }
  adj_d_ratio_20 <- adj_d_ratio(input = d_effects$effects)

  #####################
  ###Output creation###
  ######################
  output <- list(Pillai = Pillai,
                 d_ratio = d_effects,
                 mean_effect = mean_g,
                 adjusted_d_ratio = adj_d_ratio_20)
  if(verbose) {
  cat("\n", "Adjusted d-ratio finished.")
  cat("\n", "Balance estimation finished.")
  }
  return(output)
}

#' initial_unbalance
#'
#' This function computes all four balance criteria of 'MAGMA.R,' namely
#' *Pillai's Trace*, *d-ratiO*, *mean g*, and *adjusted d-ratio* for the
#' unmatched data set. This enables comparison of initial unbalance with
#' the balance after matching.
#'
#' This function computes all four Balance criteria of 'MAGMA.R', namely
#' Pillai's Trace, d-ratio, mean g, and adjusted d-ratio for the overall
#' samples. Missing data for Pillai's Trace are excluded listwise, while for
#' the other balance criteria pairwise exclusion is applied.
#'
#' @param Data A data frame containing at least the *grouping* variable and all
#'  *covariates* of interest.
#' @param group A character specifying the name of
#' your grouping variable in data. Note that MAGMA can only match your data for
#' a maximum of 4 groups. For matching over two grouping variables (e.g., 2x2
#' design) is possible by specifying group as a character vector with a length
#' of two. In this case each or the two grouping variables can only have two
#' levels.
#' @param covariates A character vector listing the names of all binary and
#' metric covariates of interest.
#' @param verbose TRUE or FALSE indicating whether matching information should
#' be printed to the console.
#'
#' @author Julian Urban
#'
#' @import tidyverse metafor robumeta tibble dplyr tidyselect
#' @importFrom psych describe
#' @importFrom purrr set_names
#' @importFrom rlang sym
#' @importFrom stats pnorm
#'
#' @return A numeric vector of length 4 containing the balance
#' criteria for the unmatched sample.
#' @export
#'
#' @references {Pastore, M., Loro, P.A.D., Mingione, M., Calcagni, A. (2022). _overlapping: Estimation of Overlapping in Empirical Distributions_. R package version
#' 2.1, (\url{https://CRAN.R-project.org/package=overlapping}).
#'
#' Revelle, W. (2023). _psych: Procedures for Psychological, Psychometric, and Personality Research_. Northwestern University, Evanston, Illinois. R package version 2.3.6, (\url{https://CRAN.R-project.org/package=psych})
#'
#' Viechtbauer, W. (2010). Conducting meta-analyses in R with the metafor package. Journal of Statistical Software, 36(3), 1-48. (\doi{10.18637/jss.v036.i03})
#'
#' Fisher, Z., Tipton, E., Zhipeng, H. (2023). _robumeta: Robust Variance Meta-Regression_. R package version 2.1,
#' (\url{https://CRAN.R-project.org/package=robumeta}).}
#'
#' @examples
#' # Defining covariates for balance estimation
#' covariates_vector <- c("GPA_school", "IQ_score", "Motivation", "parents_academic", "gender")
#'
#' # Computing initial unbalance using the data set 'MAGMA_sim_data'
#' # Computing initial unbalance for the variable 'gifted_support' (received
#' # giftedness support yes or no)
#' unbalance_gifted <- initial_unbalance(Data = MAGMA_sim_data,
#'                                       group = "gifted_support",
#'                                       covariates = covariates_vector)
#' unbalance_gifted
#'
#' # Computing initial unbalance using the data set 'MAGMA_sim_data'
#' # Computing initial unbalance for the variable 'teacher_ability_rating'
#' # (ability rated from teacher as below average, average, or above average)
#' unbalance_tar <- initial_unbalance(Data = MAGMA_sim_data,
#'                                   group = "teacher_ability_rating",
#'                                   covariates = covariates_vector)
#' unbalance_tar
#'
#' # Computing initial unbalance using the data set 'MAGMA_sim_data'
#' # Computing initial unbalance for the variables 'gifted_support' (received
#' # giftedness support yes or no) and 'enrichment' (participated in enrichment
#' # or not)
#' unbalance_2x2 <- initial_unbalance(Data = MAGMA_sim_data,
#'                                   group = c("gifted_support", "enrichment"),
#'                                   covariates = covariates_vector)
#' unbalance_2x2
#' 
#'
initial_unbalance <- function(Data,
                              group,
                              covariates,
                              verbose = TRUE, 
                              covariates_ordinal = NULL,
                              covariates_nominal = NULL) {
  if (!is.data.frame(Data) && !tibble::is_tibble(Data)) {
    stop("Data needs to be a data frame, or tibble!")
  }

  if(!is.character(group) | length(group) > 2) {
    stop("group needs to be a character vector of maximum length 2!")
  }

  if(!is.character(covariates)) {
    stop("covariates needs to be a character or a character vector!")
  }
  
  class_covariates <- unique(sapply(Data[, covariates], class))
  
  if("factor" %in% class_covariates |
     "character" %in%  class_covariates) {
       stop("Some covariates variables are defined as factor or character.
            Covariates need to be numeric variables.")
  }
  
  if(!is.character(covariates_ordinal) & !is.null(covariates_ordinal)) {
    stop("covariates_ordinal needs to be a character or a character vector!")
  }
  
  if(is.character(covariates_ordinal)) {
    if(sum(covariates_ordinal %in% covariates) > 0) {
      stop("Some variables are specified as covariates and covariates_ordinal. You can only specify each variable to one of theese arguments!")
    }
  }
  
  if(!is.character(covariates_nominal) & !is.null(covariates_nominal)) {
    stop("covariates_nominal needs to be a character or a character vector!")
  }
  
  if(is.character(covariates_nominal)) {
    if(sum(covariates_nominal %in% covariates) > 0) {
      stop("Some variables are specified as covariates and covariates_nominal You can only specify each variable to one of theese arguments!")
    }
  }
  
  if(is.character(covariates_nominal) & is.character(covariates_ordinal))  {
    if(sum(covariates_nominal %in% covariates_ordinal) > 0) {
      stop("Some variables are specified as covariates_ordinal and covariates_nominal. You can only specify each variable to one of theese arguments!")
    }
  }
    
    Data[, covariates_ordinal] <- sapply(Data[, covariates_ordinal], as.numeric)

  ########################
  #####Pillai's Trace#####
  ########################
  if(length(group) == 1) {
    Pillai_input <- Data %>%
      dplyr::select(tidyselect::all_of(covariates),
                    tidyselect::all_of(group))
    
    if(sum(rowSums(is.na(Pillai_input)) == 0) < 50) {
      Pillai <- NA
      warning("Too few cases with valid data on all covariates to estimate Pillai's
              Trace. Setting Pillai's Trace to NA.")
    } else {
    
    formula <- paste0("cbind(", paste(covariates, collapse = ","), ") ~ as.factor(", group, ")")
    
    if(length(covariates) == 1) {
      Square_sums <- stats::aov(stats::as.formula(formula),
                                data = Pillai_input) %>%
        summary() %>%
        `[[`(1) %>%
        `[[`("Sum Sq")
      
      Pillai <- Square_sums[1] / sum(Square_sums)
    } else {
      Pillai <- stats::manova(stats::as.formula(formula),
                              data = Pillai_input) %>%
        summary() %>%
        `[[`("stats") %>%
        `[[`(1, 2) %>%
        unlist()
    }

  }} else {
    Pillai_input <- Data %>%
      dplyr::select(tidyselect::all_of(covariates),
                    tidyselect::all_of(group[1]),
                    tidyselect::all_of(group[2]))
    
    if(sum(rowSums(is.na(Pillai_input)) == 0) < 50) {
      Pillai <- NA
      warning("Too few cases with valid data on all covariates to estimate Pillai's
              Trace. Setting Pillai's Trace to NA.")
    } else {
    
    formula <- paste0("cbind(", paste(covariates, collapse = ","), ") ~ as.factor(", group[1],
                      ") + as.factor(", group[2],
                      ") + ", group[1], "*", group[2])
    
    if(length(covariates) == 1) {
      Square_sums <- stats::aov(stats::as.formula(formula),
                                data = Pillai_input) %>%
        summary() %>%
        `[[`(1) %>%
        `[[`("Sum Sq")
      
      Pillai <- c(Square_sums[1] / (Square_sums[1] + Square_sums[4]),
                  Square_sums[2] / (Square_sums[2] + Square_sums[4]),
                  Square_sums[3] / (Square_sums[3] + Square_sums[4]))
    } else {

    Pillai <- stats::manova(stats::as.formula(formula),
                     data = Pillai_input) %>%
      summary() %>%
      `[[`("stats") %>%
      `[`(c(1:3), 2) %>%
      unlist()
    }

  }}

group_test <- group

  ########################
  ########d ratio#########
  ########################
if(length(group) == 2) {
  values_1 <- unlist(unique(Data[group[1]]))
  values_2 <- unlist(unique(Data[group[2]]))

  Data <- Data %>%
    dplyr::mutate(group_d = dplyr::case_when(
      !!rlang::sym(group[1]) == values_1[1] &
        !!rlang::sym(group[2]) == values_2[1] ~ 1,
      !!rlang::sym(group[1]) == values_1[1] &
        !!rlang::sym(group[2]) == values_2[2] ~ 2,
      !!rlang::sym(group[1]) == values_1[2] &
        !!rlang::sym(group[2]) == values_2[1] ~ 3,
      !!rlang::sym(group[1]) == values_1[2] &
        !!rlang::sym(group[2]) == values_2[2] ~ 4
    ))

  group <- "group_d"
}



  group_stats <- Data %>%
    dplyr::select(tidyselect::all_of(group),
                  tidyselect::all_of(covariates)) %>%
    dplyr::group_by(!!rlang::sym(group)) %>%
    dplyr::summarise_at(.vars = covariates,
                        .funs = psych::describe) %>%
    as.list() %>%
    `[`(-1)

  group_factor <- length(table(Data[group]))
  if (group_factor == 2) {
    pairwise_matrix <- matrix(c(1, 2),
                              ncol = 2,
                              nrow = 1)
  } else if (group_factor == 3) {
    pairwise_matrix <- matrix(c(1, 1, 2, 2, 3, 3),
                              ncol = 2,
                              nrow = 3)
  } else if (group_factor == 4) {
    pairwise_matrix <- matrix(c(1, 1, 1, 2, 2, 3, 2, 3, 4, 3, 4, 4),
                              ncol = 2,
                              nrow = 6)
  } else { #function independent of group number. Currently breaking algorithm
    pairwise_matrix <- NA
    stop("To many groups (maximum possible 4) for current package version!")
  }

  d_effect <- matrix(NA,
                     nrow = nrow(pairwise_matrix),
                     ncol = length(group_stats))

  for(j in 1:nrow(pairwise_matrix)) {
    value_1 <- pairwise_matrix[j, 1]
    value_2 <- pairwise_matrix[j, 2]
    for(i in seq_along(group_stats)) {
      temp_stats <- group_stats[[i]][c("n", "mean", "sd")]
      mean_diff <- (temp_stats[value_1, 2] - temp_stats[value_2, 2])
      pooled_sd <- sqrt(
        ((temp_stats[value_1, 1] - 1) * temp_stats[value_1, 3]^2 +
           (temp_stats[value_2, 1] - 1) * temp_stats[value_2, 3]^2)/
          (temp_stats[value_1, 1] + temp_stats[value_2, 1] - 2))
      d_effect[j, i] <- mean_diff/pooled_sd
    }
  }
  
  effects <- unlist(d_effect)
  if(!is.null(covariates_ordinal)) {
    ordinal_effects <- effect_ordinal(Data = Data,
                                      group = group,
                                      variable = covariates_ordinal)
    effects <- c(effects, unlist(ordinal_effects))
    ordinal_N <-   lapply(covariates_ordinal,
                          function(var) {
                            rowSums(as.matrix(table(unlist(Data[ , group]),
                                                    unlist(Data[, var]))))
                          })
    }
  
  if(!is.null(covariates_nominal)) {
    nominal_effects <- effect_nominal(Data = Data,
                                      group = group,
                                      variable = covariates_nominal)
    effects <- c(effects, unlist(nominal_effects))
    nominal_N <-  lapply(covariates_nominal,
                         function(var) {
                           rowSums(as.matrix(table(unlist(Data[ , group]),
                                 unlist(Data[, var]))))
                           })
  }
  

  d_logic <- abs(effects) < .20
  d_ratio <- sum(d_logic)/length(d_logic)


  ########################
  ########mean g#########
  ########################
  n_matrix <- var_matrix <- d_effect
  metric_N <-  lapply(covariates,
                       function(var) {
                         rowSums(as.matrix(table(unlist(Data[ , group]),
                                                 unlist(Data[, var]))))
                       })

  for(j in 1:nrow(pairwise_matrix)) {
    value_1 <- pairwise_matrix[j, 1]
    value_2 <- pairwise_matrix[j, 2]
    for(i in seq_along(covariates)) {
      n_matrix[j, i] <- sum(metric_N[[i]][c(value_1, value_2)])
      var_matrix[j, i] <- d_effect[j, i]^2/(2 * sum(metric_N[[i]][c(value_1, value_2)])) +
                          sum(metric_N[[i]][c(value_1, value_2)])/prod(metric_N[[i]][c(value_1, value_2)])
    }
  }


  J_matrix <- 1 - (3/(4 * (2 * n_matrix - 2) - 1))

  effect_g <- abs(J_matrix * d_effect)

  var_g <- J_matrix^2 * var_matrix 

  if(nrow(effect_g) == 1) {
    mean_g <- metafor::rma(effect_g, var_g)[["b"]]
    if(verbose) {
    cat("Mean g was computed using random effects meta-analysis with metafor.")
    }
  } else {
    effect <- as.numeric(effect_g)
    variance <- as.numeric(var_g)
    ma_input <- data.frame(effect,
                           variance,
                           numb =  rep(c(1:length(covariates)),
                                       length(effect)/length(covariates)))

    mean_g <- robumeta::robu(ma_input[, 1] ~ 1, var.eff.size = ma_input[, 2],
                                studynum = ma_input[, 3], data = ma_input)[["b.r"]]
    if(verbose) {
    cat("Mean g was computed using robust variance meta-analysis with robumeta.")
    }
  }

  ########################
  ###adj. d ratioo########
  ########################
  matrix_n <- lapply(c(covariates,
                       covariates_ordinal,
                       covariates_nominal),
                     function(variable) {
                       matrix_n_temp <- unlist(table(Data[!is.na(Data[, variable]), group]))
                       sapply(c(1:nrow(pairwise_matrix)),
                              function(index) {
                                n <- c(matrix_n_temp[pairwise_matrix[index, 1]],
                                       matrix_n_temp[pairwise_matrix[index, 2]])
                              }) %>%
                         t()
                     })
    matrix_n <-  do.call(rbind.data.frame, matrix_n)

  matrix_J <- 1 - (3/(4 * (2 * rowSums(matrix_n) - 2) - 1))
  
  effects_g <- abs(matrix_J * effects)
  
  var_matrix <- effects_g^2 / (2 * rowSums(matrix_n)) +
    rowSums(matrix_n) / (matrix_n[, 1] * matrix_n[, 2])

  var_effects_g <- matrix_J^2 * var_matrix 


  adj_d_ratio_raw <- purrr::map2_dbl(effects_g, sqrt(var_effects_g), stats::pnorm, q = .20) 
  adj_d_ratio <-  sum(adj_d_ratio_raw) / length(adj_d_ratio_raw)

  #####################
  ###Output creation###
  ######################
  if(length(group_test) == 1) {
  output <- cbind(round(Pillai, 2),
                  round(d_ratio, 2),
                  round(mean_g, 2),
                  round(adj_d_ratio, 2))
  rownames(output) <- "Unbalance"
  colnames(output) <- c("Pillai's Trace", "d-ratio", "Mean g", "adj. d-ratio")
  } else {
    output <- c(round(Pillai, 2),
                round(d_ratio, 2),
                round(mean_g, 2),
                round(adj_d_ratio, 2)) %>%
      as.matrix() %>%
      t()
    rownames(output) <- "Unbalance"
    colnames(output) <- c(paste("Pillai's Trace", group_test[1]),
                          paste("Pillai's Trace", group_test[2]),
                          "Pillai's Trace IA",
                          "d-ratio",
                          "Mean g",
                          "adj. d-ratio")
  }

  return(output)

}


#' Table_MAGMA
#'
#' This function prints an APA Table of the Balance criteria. It displays the
#' balance criteria for four different sample sizes per group. In each scenario,
#' one balance criteria has its optimal value. Thus, the table is a 4x5 table
#' showing the four balance criteria and the respective sample size per group
#' for the four scenarios.
#'
#' This function creates an APA Table including the optimal models for each
#' balance criterion, the other criteria for the respective sample size per
#' group as well as the sample size itself. With an optional argument you can
#' save a the APA table in Word.
#'
#' @param Balance A result of \code{\link{Balance_MAGMA}} Compare the function
#' \code{\link{Balance_MAGMA}}.
#' @param filename Optional argument.  A character specifying the filename that
#' the resulting Word document with the table should have.
#' @param verbose TRUE or FALSE indicating whether matching information should
#' be printed to the console.
#'
#'
#' @author Julian Urban
#'
#' @import tidyverse
#' @importFrom janitor adorn_title
#' @importFrom flextable flextable
#' @importFrom flextable autofit
#' @importFrom flextable save_as_docx
#' @importFrom rlang is_list
#'
#' @return A 4x5 APA table showing the four balance
#' criteria and the respective sample sizes per group for four scenarios. In
#' each of these scenarios, one balance criteria has its optimal value. It can
#' print a Word Document with this table.
#' @export
#'
#' @examples
#' # This function bases on a MAGMA function as well as Balance_MAGMA
#' # Defining the names of the metric and binary covariates
#' covariates_vector <- c("GPA_school", "IQ_score", "Motivation", "parents_academic", "gender")
#'
#'
#' # Estimating balance of a two-group matching using the data set
#' # 'MAGMA_sim_data'.
#' # Matching variable 'gifted_support' (received giftedness support yes or no)
#' Balance_gifted <- Balance_MAGMA(Data = MAGMA_sim_data[MAGMA_sim_data$step_gifted < 200, ],
#'                                 group = "gifted_support",
#'                                 covariates = covariates_vector,
#'                                 step = "step_gifted")
#'
#' Table_MAGMA(Balance_gifted)
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
#' # Estimating Balance. Covariates same as above
#' Balance_2x2 <- Balance_MAGMA(Data = MAGMA_sim_data_gift_enrich,
#'                              group = c("gifted_support", "enrichment"),
#'                              covariates = covariates_vector,
#'                              step = "step") #step created during matching
#'
#' Table_MAGMA(Balance_2x2)
#' }
#'
Table_MAGMA <- function(Balance, filename = NULL, verbose = TRUE) {
  #Check input
  if (!rlang::is_list(Balance)) {
    stop("Balance needs to be a Balance_MAGMA object!")
  }
  #Creating index vector of models with optimalized criteria

  if(!is.matrix(Balance$Pillai)) {
  index_optimal <- c(max(which(Balance$Pillai == min(Balance$Pillai, na.rm = T), arr.ind = T)),
                   max(which(Balance$d_ratio$d_rate == max(Balance$d_ratio$d_rate, na.rm = T), arr.ind = T)),
                   max(which(Balance$mean_effect == min(Balance$mean_effect, na.rm = T), arr.ind = T)),
                   max(which(Balance$adjusted_d_ratio == max(Balance$adjusted_d_ratio, na.rm = T), arr.ind = T)))
  suppressMessages({
  balance_matrix <- tibble::as_tibble(matrix(NA, ncol = length(index_optimal),
                                     nrow = length(index_optimal)),
                              .name_repair = "unique") %>%
    dplyr::transmute(Criterion_optimized = c("Best Pillai",
                                      "Best d-ratio",
                                      "Best mean g",
                                      "Best adj. d-ratio"), #Row names for table
              #Extractig vakues for all three "optimal" models as well as their n er group
              Pillai_Trace = round(Balance$Pillai[index_optimal], 2),
              d_ratio = round(Balance$d_ratio$d_rate[index_optimal], 2),
              mean_g = round(Balance$mean_effect[index_optimal], 2),
              adjusted_d_ratio = round(Balance$adjusted_d_ratio[index_optimal], 2))
  
  balance_matrix$n_per_group <- index_optimal
  # Ordering table after n per group
  balance_matrix <- balance_matrix[order(balance_matrix$n_per_group), ]
  })
  }

  if(is.matrix(Balance$Pillai)) {
    index_optimal <- c(max(which(Balance$Pillai[1, ] == min(Balance$Pillai[1, ], na.rm = T), arr.ind = T)),
                       max(which(Balance$Pillai[2, ] == min(Balance$Pillai[2, ], na.rm = T), arr.ind = T)),
                       max(which(Balance$Pillai[3, ] == min(Balance$Pillai[3, ], na.rm = T), arr.ind = T)),
                       max(which(Balance$d_ratio$d_rate == max(Balance$d_ratio$d_rate, na.rm = T), arr.ind = T)),
                       max(which(Balance$mean_effect == min(Balance$mean_effect, na.rm = T), arr.ind = T)),
                       max(which(Balance$adjusted_d_ratio == max(Balance$adjusted_d_ratio, na.rm = T), arr.ind = T)))

    suppressMessages({
    balance_matrix <- tibble::as_tibble(matrix(NA, ncol = length(index_optimal),
                                       nrow = length(index_optimal)),
                                .name_repair = "unique") %>%
      dplyr::transmute(Criterion_optimized = c("Best Pillai ME 1",
                                        "Best Pillai ME 2",
                                        "Best Pillai IA",
                                        "Best d-ratio",
                                        "Best mean g",
                                        "Best adj. d-ratio"), #Row names for table
                #Extractig vakues for all three "optimal" models as well as their n er group
                Pillai_Trace_ME1 = round(Balance$Pillai[1, index_optimal], 2),
                Pillai_Trace_ME2 = round(Balance$Pillai[2, index_optimal], 2),
                Pillai_Trace_IA = round(Balance$Pillai[3, index_optimal], 2),
                d_ratio = round(Balance$d_ratio$d_rate[index_optimal], 2),
                mean_g = round(Balance$mean_effect[index_optimal], 2),
                adjusted_d_ratio = round(Balance$adjusted_d_ratio[index_optimal], 2))
                
    balance_matrix$n_per_group <- index_optimal
    # Ordering table after n per group
    balance_matrix <- balance_matrix[order(balance_matrix$n_per_group), ]
    })
  }

if(!is.null(filename)) {
  balance_matrix %>%
    #convert matrix into rough APA Table
    janitor::adorn_title(
      row_name = "Optimized",
      col_name = "Criterion",
      placement = "combined") %>%
    flextable::flextable() %>%
    flextable::autofit() %>%
    flextable::save_as_docx(path = paste("./",filename,sep=""))
}
  if(verbose) {
cat("Balance table successfully created!")
  }
return(balance_matrix)
}



#' Plot_MAGMA
#'
#' Plots for balance with respect to sample size.
#'
#' This function creates R-Plots using ggplot2 to show the balance trend over
#' sample size.
#'
#' @param Balance A result of Balance_MAGMA. Compare the function
#' \code{\link{Balance_MAGMA}}.
#' @param criterion A character vector specifying for which balance criteria
#' a plot should be created. Default is all criteria.
#'
#'
#' @author Julian Urban
#'
#' @import tidyverse ggplot2
#' @importFrom rlang is_list
#'
#' @return R Plots showing the balance trend over sample size.
#' @export
#'
#' @examples
#' 
#' # This function bases on a MAGMA function as well as Balance_MAGMA
#' # To run examples, copy them into your console or script
#' # Defining the names of the metric and binary covariates
#' covariates_vector <- c("GPA_school", "IQ_score", "Motivation", "parents_academic", "gender")
#'
#' #  Estimating balance of a two-group matching using the data set
#' # 'MAGMA_sim_data'.
#' # Matching variable 'gifted_support' (received giftedness support yes or no)
#' Balance_gifted <- Balance_MAGMA(Data = MAGMA_sim_data[MAGMA_sim_data$step_gifted < 150, ],
#'                                 group = "gifted_support",
#'                                 covariates = covariates_vector,
#'                                 step = "step_gifted") 
#'
#' Plot_MAGMA(Balance = Balance_gifted,
#'            criterion = "Adj_d_ratio") #Using default to plot all criteria
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
#'                              step = "step") #step created during matching
#'
#' Plot_MAGMA(Balance = Balance_2x2,
#'            criterion = c("d_ration", "Adj_d_ratio"))
#' }
#'
Plot_MAGMA <- function(Balance,
                       criterion = c("Pillai",
                                     "d_ratio",
                                     "mean_g",
                                     "Adj_d_ratio")) {
  #Check input
  if (!rlang::is_list(Balance)) {
    stop("Balance needs to be a Balance_MAGMA object!")
  }


  if (any(criterion == "Pillai")) {

    if(!is.matrix(Balance$Pillai)) {
    Balance_temp <- Balance$Pillai %>%
        unlist() %>%
        tibble::as_tibble(.name_repair = "minimal")
    Balance_temp$N <- c(1:nrow(Balance_temp))
    
    print(ggplot2::ggplot() +
            ggplot2::geom_point(aes(x = Balance_temp$N, y = Balance_temp$value)) +
            ggplot2::theme(panel.background = element_blank()) +
            ggplot2::scale_y_continuous(limits = c(0, .5),
                               breaks = seq(0, .5, .05)) +
            ggplot2::labs(y = "Pillai`s Trace", x ="\nN per group",
                 title = "Pillai`s Trace values for different sample sizes"))
    }

    if(is.matrix(Balance$Pillai)) {
      effects <- c("Main Effect 1",
                   "Main Effect 2",
                   "Interaction")
      for(i in 1:nrow(Balance$Pillai)) {
        Balance_temp <- Balance$Pillai[i, ] %>%
          unlist() %>%
          tibble::as_tibble(.name_repair = "minimal")
        Balance_temp$N <- c(1:nrow(Balance_temp))
        
      print(Balance_temp %>%
              ggplot2::ggplot() +
              ggplot2::geom_point(aes(x = Balance_temp$N, y = Balance_temp$value))+
              ggplot2::theme(panel.background = element_blank()) +
              ggplot2::scale_y_continuous(limits = c(0, .5),
                                 breaks = seq(0, .5, .05)) +
              ggplot2::labs(y = "Pillai`s Trace", x ="\nN per group",
                   title = paste("Pillai`s Trace values for",
                                 effects[i],
                                 "for different sample sizes",
                                 sep = " "))
            )
      }
    }

  }
  if (any(criterion == "d_ratio")) {
    Balance_temp <- Balance$d_ratio$d_rate %>%
      unlist() %>%
      tibble::as_tibble(.name_repair = "minimal")
    Balance_temp$N <- c(1:nrow(Balance_temp))
    
    print(ggplot2::ggplot() +
            ggplot2::geom_point(aes(x = Balance_temp$N, y = Balance_temp$value)) +
            ggplot2::theme(panel.background = element_blank()) +
            ggplot2::scale_y_continuous(limits = c(0, 1),
                               breaks = seq(0, 1, .2)) +
            ggplot2::labs(y = "d`s < 0.20", x ="\nN per group",
                 title = "Cohen`s d < .20` for different sample sizes"))
  }
  if (any(criterion == "mean_g")) {
    Balance_temp <- Balance$mean_effect %>%
      unlist() %>%
      tibble::as_tibble(.name_repair = "minimal")
    Balance_temp$N <- c(1:nrow(Balance_temp))
    
    print(ggplot2::ggplot() +
            ggplot2::geom_point(aes(x = Balance_temp$N, y = Balance_temp$value))+
            ggplot2::theme(panel.background = element_blank()) +
            ggplot2::scale_y_continuous(limits = c(0, 1),
                               breaks = seq(0, 1, .2)) +
            ggplot2::labs(y ="mean g", x ="\nN per group",
                 title = "Mean effect for different sample sizes"))
  }
  if (any(criterion == "Adj_d_ratio")) {
    Balance_temp <- Balance$adjusted_d_ratio %>%
      unlist() %>%
      tibble::as_tibble(.name_repair = "minimal")
    Balance_temp$N <- c(1:nrow(Balance_temp))
    
    print(ggplot2::ggplot() +
            ggplot2::geom_point(aes(x = Balance_temp$N, y = Balance_temp$value)) +
            ggplot2::theme(panel.background = element_blank()) +
            ggplot2::scale_y_continuous(limits = c(0, 1),
                               breaks = seq(0, 1, .2)) +
            ggplot2::labs(y = "Adjusted d-ratio", x = "\nN per group",
                 title = "Adjusted d-ratio for different sample sizes"))
  }
}










