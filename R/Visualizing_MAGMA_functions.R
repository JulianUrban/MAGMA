#' Balance estimation
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
#' Missing data for Pillai's Trace are excluded listwise, while for the other
#' balance criteria pairwise exclusion is applied.
#'
#' @param Data A data frame containing at least the *grouping* variable, the
#' *step* variable from the main MAGMA-function (or other matching algorithms),
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
#' @import tidyverse psych metafor robumeta tibble dplyr tidyselect
#'
#' @return A list of length four containing all balance criteria and all
#' pairwise effects with respect to group sample size.
#' @export
#'
#' @references {Pastore M, Loro PAD, Mingione M, Calcagni' A (2022). _overlapping: Estimation of Overlapping in Empirical Distributions_. R package version
#' 2.1, <https://CRAN.R-project.org/package=overlapping>.
#' William Revelle (2023). _psych: Procedures for Psychological, Psychometric, and Personality Research_. Northwestern University, Evanston,
#' Illinois. R package version 2.3.6, <https://CRAN.R-project.org/package=psych>.
#' Viechtbauer, W. (2010). Conducting meta-analyses in R with the metafor package. Journal of Statistical Software, 36(3), 1-48.
#' \doi{10.18637/jss.v036.i03}
#' Fisher Z, Tipton E, Zhipeng H (2023). _robumeta: Robust Variance Meta-Regression_. R package version 2.1,
#' <https://CRAN.R-project.org/package=robumeta>.}
#'
#' @examples{
#' \dontrun{
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
#' Balance_gifted_exact <- Balance_MAGMA(Data = MAGMA_sim_data_gifted_exact,
#'                                       group = "gifted_support",
#'                                       covariates = covariates_vector,
#'                                       step = "step") #step created during matching
#' str(Balance_gifted_exact)
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
#' Balance_2x2 <- Balance_MAGMA(Dta = MAGMA_sim_data_gift_enrich,
#'                              group = c("gifted_support", "enrichment"),
#'                              covariates = covariates_vector,
#'                              step = "step") #step created during matching
#' str(Balance_2x2)
#'
#' }
#' }
#'
Balance_MAGMA <- function(Data, group, covariates, step = "step") {
  #Fehlermeldungen
  #Prüfen ob data korrekt ist
  #TODO: tbl class > 1
  if (!is.data.frame(Data) && !tibble::is_tibble(Data)) {
    stop("Class data needs to be list, data frame, or tibble!")
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

cat("Start estimating Pillai's Trace.")

  ########################
  #####Pillai's Trace#####
  ########################

Pillai <- Pillai_iterativ(da = Data,
                         gr = group,
                         co = covariates,
                         st = step)


cat("\n", "Pillai's Trace finsihed. Starting to compute d-ratio.")

if(length(group) == 2) {
  values_1 <- unique(da[group[1]])
  values_2 <- unique(da[group[2]])

  da <- da %>%
    dplyr::mutate(group_d = dplyr::case_when(
      !!sym(group[1]) == values_1[1] &
        !!sym(group[2]) == values_2[1] ~ 1,
      !!sym(group[1]) == values_1[1] &
        !!sym(group[2]) == values_2[2] ~ 2,
      !!sym(group[1]) == values_1[2] &
        !!sym(group[2]) == values_2[1] ~ 3,
      !!sym(group[1]) == values_1[2] &
        !!sym(group[2]) == values_2[2] ~ 4
    ))

  group <- "group_d"
}


  ########################
  ########d ratio#########
  ########################
  d_effects <- inner_d(da = Data,
                       gr = group,
                       co = covariates,
                       st = step)

  ########################
  ########mean g#########
  ########################
cat("\n", "d-ratio finsihed. Starting to compute mean-g.", "\n")
  group_number <- Data %>%
    dplyr::select(tidyselect::all_of(group)) %>%
    table() %>%
    length()

  mean_g <- mean_g_meta(input = d_effects,
                        number_groups = group_number)

  ########################
  ###likelihhod g < .20###
  ########################
  cat("mean-g finsihed. Starting to compute adjusted d-ratio.")
  adj_d_ratio_20 <- adj_d_ratio(input = d_effects)

  #####################
  ###Output creation###
  ######################
  output <- list(Pillai = Pillai,
               d_ratio = d_effects,
               mean_effect = mean_g,
               adjusted_d_ratio = adj_d_ratio_20)
  cat("\n", "finsihed balance estimation")
  return(output)
}

#' Initial unbalance estimation
#'
#' This function computes all four Balance criteria of MAGMA, namely
#' *Pillai's Trace*, *d-ratiO*, *mean g*, and *adjusted d-ratio* for the
#' unmatched data set. This enables comparison of initial and after matching
#' unbalance.
#'
#' This function computes all four Balance criteria of MAGMA, namely Pillai's
#' Trace, d-ratio, mean g, and adjusted d-ratio for the overall samples. This
#' leads to different sample sizes per group. Missing data for Pillai's Trace
#' are excluded listwise, while for the other balance criteria pairwise
#' exclusion is applied.
#'
#' @param Data A data frame containing at least the *grouping* variable and all
#'  *covariates* of interest.
#' @param group A character specifying the name of
#' your grouping variable in data. Note that MAGMA can only match your data for
#' a maximum of 4 groups. For matching over two groups (e.g., 2x2 Design) is
#' possible by specifying group as a character vector with a length of two. In
#' this case each or the 2 grouping variables can only have two levels.
#' @param covariates A character vector listing the names of all binary and
#' metric covariates of interest.
#'
#' @author Julian Urban
#'
#' @import tidyverse psych metafor robumeta tibble dplyr tidyselect
#'
#' @return A numeric vector of length 4 containing the respective balance
#' criteria for the unmatched sample.
#' @export
#'
#' @references {Pastore M, Loro PAD, Mingione M, Calcagni' A (2022). _overlapping: Estimation of Overlapping in Empirical Distributions_. R package version
#' 2.1, <https://CRAN.R-project.org/package=overlapping>.
#' William Revelle (2023). _psych: Procedures for Psychological, Psychometric, and Personality Research_. Northwestern University, Evanston, Illinois. R package version 2.3.6, <https://CRAN.R-project.org/package=psych>.
#' Viechtbauer, W. (2010). Conducting meta-analyses in R with the metafor package. Journal of Statistical Software, 36(3), 1-48. \doi{10.18637/jss.v036.i03}
#' Fisher Z, Tipton E, Zhipeng H (2023). _robumeta: Robust Variance Meta-Regression_. R package version 2.1,
#' <https://CRAN.R-project.org/package=robumeta>.}
#'
#' @examples{
#' \dontrun{
#' #Defining covariates for balance estimation
#' covariates_vector <- c("GPA_school", "IQ_score", "Motivation", "parents_academic", "sex")
#'
#' #Computing initial unbalance for giftedness support
#' unbalance_gifted <- initial_unbalance(Data = MAGMA_sim_data,
#'                                       group = "gifted_support",
#'                                       covariates = covariates_vector)
#' unbalance_gifted
#'
#' #Computing initial unbalnce for teacher rated ability
#' unbalance_tar <- initial_unbalance(Data = MAGMA_sim_data,
#'                                   group = "teacher_ability_rating",
#'                                   covariates = covariates_vector)
#' unbalance_tar
#'
#' #Computing initial unbalnce for teacher rated ability
#' unbalance_2x2 <- initial_unbalance(Data = MAGMA_sim_data,
#'                                   group = c("gifted_support", "enrichment"),
#'                                   covariates = covariates_vector)
#' unbalance_2x2
#' }
#' }
#'
initial_unbalance <- function(Data, group, covariates) {
  if (!is.data.frame(Data) && !tibble::is_tibble(Data)) {
    stop("Data needs to be a data frame, or tibble!")
  }

  if(!is.character(group) | length(group) > 2) {
    stop("group needs to be a character vector of maximum length 2!")
  }

  if(!is.character(covariates)) {
    stop("covariates needs to be a character or a character vector!")
  }

  ########################
  #####Pillai's Trace#####
  ########################
  if(length(group) == 1) {
    Pillai_input <- Data %>%
      dplyr::select(tidyselect::all_of(covariates),
                    tidyselect::all_of(group)) %>%
      purrr::set_names(c(covariates, "IV"))

      Pillai <- stats::manova(Pillai_DV(Pillai_input, covariates) ~ IV,
                              data = Pillai_input) %>%
        summary() %>%
        `[[`("stats") %>%
        `[[`(1, 2) %>%
        unlist()

  } else {
    Pillai_input <- Data %>%
      dplyr::select(tidyselect::all_of(covariates),
                    tidyselect::all_of(group[1]),
                    tidyselect::all_of(group[2])) %>%
      purrr::set_names(c(covariates, "IV_1", "IV_2"))

    Pillai <- stats::manova(Pillai_DV(Pillai_input, covariates) ~ IV_1 + IV_2 + IV_1 * IV_2,
                     data = Pillai_input) %>%
      summary() %>%
      `[[`("stats") %>%
      `[`(c(1:3), 2) %>%
      unlist()

  }

group_test <- group

  ########################
  ########d ratio#########
  ########################
if(length(group) == 2) {
  values_1 <- unlist(unique(Data[group[1]]))
  values_2 <- unlist(unique(Data[group[2]]))

  Data <- Data %>%
    dplyr::mutate(group_d = dplyr::case_when(
      !!sym(group[1]) == values_1[1] &
        !!sym(group[2]) == values_2[1] ~ 1,
      !!sym(group[1]) == values_1[1] &
        !!sym(group[2]) == values_2[2] ~ 2,
      !!sym(group[1]) == values_1[2] &
        !!sym(group[2]) == values_2[1] ~ 3,
      !!sym(group[1]) == values_1[2] &
        !!sym(group[2]) == values_2[2] ~ 4
    ))

  group <- "group_d"
}



  group_stats <- Data %>%
    dplyr::select(IV = tidyselect::all_of(group),
                  tidyselect::all_of(covariates)) %>%
    dplyr::group_by(IV) %>%
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
      temp_stats <- group_stats[[i]] %>%
        dplyr::select(n, mean, sd)
      mean_diff <- (temp_stats[value_1, 2] - temp_stats[value_2, 2])
      pooled_sd <- sqrt(
        ((temp_stats[value_1, 1] - 1) * temp_stats[value_1, 3]^2 +
           (temp_stats[value_2, 1] - 1) * temp_stats[value_2, 3]^2)/
          (temp_stats[value_1, 1] + temp_stats[value_2, 1] - 2))
      d_effect[j, i] <- mean_diff/pooled_sd
    }
  }

  d_logic <- abs(d_effect) < .20
  d_ratio <- sum(d_logic)/length(d_logic)


  ########################
  ########mean g#########
  ########################
  n_matrix <- var_matrix <- d_effect

  for(j in 1:nrow(pairwise_matrix)) {
    value_1 <- pairwise_matrix[j, 1]
    value_2 <- pairwise_matrix[j, 2]
    for(i in seq_along(group_stats)) {
      temp_n <- group_stats[[i]] %>%
        dplyr::select(n) %>%
        unlist()
      n_matrix[j, i] <- sum(temp_n)
      var_matrix[j, i] <- d_effect[j, i]^2/(2 * sum(temp_n)) +
                          sum(temp_n)/prod(temp_n)
    }
  }


  J_matrix <- 1 - (3/(4 * (2 * n_matrix - 2) - 1))

  effect_g <- abs(J_matrix * d_effect)

  var_g <- J_matrix^2 * var_matrix #matrix multiplication?

  if(nrow(effect_g) == 1) {
    mean_g <- metafor::rma(effect_g, var_g)[["b"]]
    print("mean g was computed using random effects meta-analysis with metafor.")
  } else {
    effect <- as.numeric(effect_g)
    variance <- as.numeric(var_g)
    ma_input <- data.frame(effect,
                           variance,
                           numb =  rep(c(1:length(covariates)),
                                       length(effect)/length(covariates)))

    mean_g <- robumeta::robu(ma_input[, 1] ~ 1, var.eff.size = ma_input[, 2],
                                studynum = ma_input[, 3], data = ma_input)[["b.r"]]
    print("mean g was computed using robust variance meta-analysis with robumeta.")
  }

  ########################
  ###adj. d ratioo########
  ########################

  adj_d_ratio <- purrr::map2_dbl(effect_g, var_g, pnorm, q = .20) %>%
    matrix(ncol = ncol(effect_g), nrow = nrow(effect_g)) %>%
    sum() / (ncol(effect_g) * nrow(effect_g))

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


#' Balance table
#'
#' This function prints an APA Table of the Balance criteria. It displays the
#' balance criteria for four different sample sizes per group. In each scenario
#' one balance criteria has its optimal value. Thus, the table is a 4x5 table
#' showing the four balance criteria and the respective sample size per group
#' for the four scenarios.
#'
#' This function creates an APA Table including the optimal models for each
#' balance criterion, the other criteria for this sample size per group as well
#' as the sample size itself. With an optional argument you can save a the APA
#' table in Word.
#'
#' @param Balance A result of Balance_MAGMA. Compare the function
#' \code{\link{Balance_MAGMA}}.
#' @param filename Optional argument.  A character specifying the filename that
#' the resulting Word document with the Table should have.
#'
#'
#' @author Julian Urban
#'
#' @import tidyverse janitor flextable
#'
#' @return A 4x5 APA Table showing the four balance
#' criteria and the respective sample size per group for four scenarios. In
#' each of these scenario one balance criteria has its optimal value. It can
#' print a Word Document with this table, too.
#' @export
#'
#' @examples{
#' \dontrun{
#' #This function bases on a MAGMA function as well as Balance_MAGMA.
#' #To run examples, copy them into your console or script.
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
#' Balance_gifted_exact <- Balance_MAGMA(Data = MAGMA_sim_data_gifted_exact,
#'                                       group = "gifted_support",
#'                                       covariates = covariates_vector,
#'                                       step = "step") #step created during matching
#'
#' Table_MAGMA(Balance_gifted_exact, "Balance_gifted_exact.docx")
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
#' Balance_2x2 <- Balance_MAGMA(Data = MAGMA_sim_data_gift_enrich,
#'                              group = c("gifted_support", "enrichment"),
#'                              covariates = covariates_vector,
#'                              step = "step") #step created during matching
#'
#' Table_MAGMA(Balance_2x2, "Balance_2x2.docx")
#' }
#' }
#'
Table_MAGMA <- function(Balance, filename = NULL) {
  #Check input
  if (!is_list(Balance)) {
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
                                      "adjusted d-ratio"), #Row names for table
              #Extractig vakues for all three "optimal" models as well as their n er group
              Pillai_Trace = round(Balance$Pillai[index_optimal], 2),
              d_ratio = round(Balance$d_ratio$d_rate[index_optimal], 2),
              mean_g = round(Balance$mean_effect[index_optimal], 2),
              adjusted_d_ratio = round(Balance$adjusted_d_ratio[index_optimal], 2),
              n_per_group = index_optimal) %>%
    #Ordering table after n per group
    dplyr::arrange(., n_per_group)
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
                                        "adjusted d-ratio"), #Row names for table
                #Extractig vakues for all three "optimal" models as well as their n er group
                Pillai_Trace_ME1 = round(Balance$Pillai[1, index_optimal], 2),
                Pillai_Trace_ME2 = round(Balance$Pillai[2, index_optimal], 2),
                Pillai_Trace_IA = round(Balance$Pillai[3, index_optimal], 2),
                d_ratio = Balance$d_ratio$d_rate[index_optimal],
                mean_g = round(Balance$mean_effect[index_optimal], 2),
                adjusted_d_ratio = round(Balance$adjusted_d_ratio[index_optimal], 2),
                n_per_group = index_optimal) %>%
      #Ordering table after n per group
      dplyr::arrange(., n_per_group)
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

cat("Balance Table successfully created!")
return(balance_matrix)
}



#' Balance plots
#'
#' Plots for Balance with respect to sample size.
#'
#' This function creates R-Plots using ggplot to show the balance trend over
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
#'
#' @return R Plots showing the balance trend over sample size.
#' @export
#'
#' @examples{
#' \dontrun{
#' #This function bases on a MAGMA function as well as Balance_MAGMA.
#' #To run examples, copy them into your console or script.
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
#' Balance_gifted_exact <- Balance_MAGMA(Data = MAGMA_sim_data_gifted_exact,
#'                                       group = "gifted_support",
#'                                       covariates = covariates_vector,
#'                                       step = "step") #step created during matching
#'
#' Plot_MAGMA(Balance = Balance_gifted_exact) #Using default to plot all criteria
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
#' Balance_2x2 <- Balance_MAGMA(Data = MAGMA_sim_data_gift_enrich,
#'                              group = c("gifted_support", "enrichment"),
#'                              covariates = covariates_vector,
#'                              step = "step") #step created during matching
#'
#' Plot_MAGMA(Balance = Balance_2x2,
#'            criterion = c("d_ration", "Adj_d_ratio"))
#' }
#' }
#'
Plot_MAGMA <- function(Balance,
                       criterion = c("Pillai",
                                     "d_ratio",
                                     "mean_g",
                                     "Adj_d_ratio")) {
  #Check input
  if (!is_list(Balance)) {
    stop("Balance needs to be a Balance_MAGMA object!")
  }


  if (any(criterion == "Pillai")) {

    if(!is.matrix(Balance$Pillai)) {
    print(Balance$Pillai %>%
            unlist() %>%
            tibble::as_tibble(.name_repair = "minimal") %>%
            dplyr::mutate(N = dplyr::row_number()) %>%
            ggplot2::ggplot() +
            ggplot2::geom_point(aes(x = N, y = value)) +
            ggplot2::theme(panel.background = element_blank()) +
            ggplot2::scale_y_continuous(limits = c(0, .5),
                               breaks = seq(0, .5, .05)) +
            ggplot2::labs(y = "Pillai`s Trace", x ="\nN per group",
                 title = "Pillai`s Trace values for different sample sizes"))
    }

    if(is.matrix(Balance$Pillai)) {
      for(i in 1:nrow(Balance$Pillai))
      print(Balance$Pillai[i, ] %>%
              unlist() %>%
              tibble::as_tibble(.name_repair = "minimal") %>%
              dplyr::mutate(N = dplyr::row_number()) %>%
              ggplot2::ggplot() +
              ggplot2::geom_point(aes(x = N, y = value))+
              ggplot2::theme(panel.background = element_blank()) +
              ggplot2::scale_y_continuous(limits = c(0, .5),
                                 breaks = seq(0, .5, .05)) +
              ggplot2::labs(y = "Pillai`s Trace", x ="\nN per group",
                   title = "Pillai`s Trace values for different sample sizes"))
    }

  }
  if (any(criterion == "d_ratio")) {
    print(Balance$d_ratio$d_rate %>%
            tibble::as_tibble(.name_repair = "minimal") %>%
            dplyr::mutate(N = dplyr::row_number()) %>%
            ggplot2::ggplot() +
            ggplot2::geom_point(aes(x = N, y = value)) +
            ggplot2::theme(panel.background = element_blank()) +
            ggplot2::scale_y_continuous(limits = c(0, 1),
                               breaks = seq(0, 1, .2)) +
            ggplot2::labs(y = "d`s < 0.20", x ="\nN per group",
                 title = "Cohen`s d < .20` for different sample sizes"))
  }
  if (any(criterion == "mean_g")) {
    print(Balance$mean_effect %>%
            tibble::as_tibble(.name_repair = "minimal") %>%
            dplyr::mutate(N = dplyr::row_number()) %>%
            ggplot2::ggplot() +
            ggplot2::geom_point(aes(x = N, y = value))+
            ggplot2::theme(panel.background = element_blank()) +
            ggplot2::scale_y_continuous(limits = c(0, 1),
                               breaks = seq(0, 1, .2)) +
            ggplot2::labs(y ="mean g", x ="\nN per group",
                 title = "Mean effect for different sample sizes"))
  }
  if (any(criterion == "Adj_d_ratio")) {
    print(Balance$adjusted_d_ratio %>%
            tibble::as_tibble(.name_repair = "minimal") %>%
            dplyr::mutate(N = dplyr::row_number()) %>%
            ggplot2::ggplot() +
            ggplot2::geom_point(aes(x = N, y = value)) +
            ggplot2::theme(panel.background = element_blank()) +
            ggplot2::scale_y_continuous(limits = c(0, 1),
                               breaks = seq(0, 1, .2)) +
            ggplot2::labs(y = "Adjusted d-ratio", x = "\nN per group",
                 title = "Adjusted d-ratio for different sample sizes"))
  }
}










