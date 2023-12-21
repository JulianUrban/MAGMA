#' MAGMA_desc
#'
#' This function provides pre- and post-matching descriptive statistics and
#' effects.
#'
#' This function enables the computation of descriptive statistics of
#' continuous variables for the overall sample and specified groups. Additional,
#' pairwise effects (Cohen's d) are computed.
#'
#' @param Data A data frame that contains the desired variable for density
#' plotting as well as the specified grouping variable.
#' @param covariates A character vector specifying the variable names of the
#' continuous variables for which the descriptive statistics should be computed.
#' @param group A character (vector) specifying the groups for which
#' differentiated statistics should be computed.
#' @param step_num An integer specifying the number of cases to be
#' included per group in this post matching comparison (e.g., 100). If no value
#' is specified, pre-matching statistics are computed. Is based on the step
#' variable of MAGMA. Optional argument.
#' @param step_var A character specifying the name of the step
#' variable in the data set. If no value is specified, pre matching
#' statistics are computed. Optional argument.
#' @param filename A character specifying the filename that
#' the resulting Word document with the Table should have. Optional argument.
#'
#' @author Julian Urban
#'
#' @import tidyverse dplyr tidyselect
#' @importFrom purrr set_names
#' @importFrom purrr map2_dfc
#' @importFrom psych describe
#' @importFrom janitor adorn_title
#' @importFrom flextable flextable
#' @importFrom flextable autofit
#' @importFrom flextable save_as_docx
#' @importFrom rlang sym
#'
#' @return A table of descriptive statistics and pairwise effects for pre- or
#' post-matching samples.
#' @export
#'
#' @examples
#' \dontrun{
#' # Defining covariates
#' covariates_gifted <- c("GPA_school", "IQ_score", "Motivation", "parents_academic", "sex")
#'
#' # Estimating pre-matching descriptive statistics and pairwise effects using
#' # the data set 'MAGMA_sim_data'
#' # Estimating statistics for grouping variable 'gifted support' (received
#' # giftedness support yes or no)
#' MAGMA_desc(Data = MAGMA_sim_data,
#'            covariates = covariates_gifted,
#'            group =  "gifted_support")
#'
#' # Two-group exact matching using the data set 'MAGMA_sim_data'
#' # Matching variable 'gifted_support' (received giftedness support yes or no)
#' # 'MAGMA_sim_data_gifted' contains the result of the matching
#' MAGMA_sim_data_gifted <- MAGMA(Data = MAGMA_sim_data,
#'                                group = "gifted_support",
#'                                dist = "ps_gifted",
#'                                cores = 2)
#'
#' # Estimating post-matching descriptive statistics and pairwise effects using
#' # the data set 'MAGMA_sim_data'
#' # Estimating statistics for grouping variable 'gifted support' (received
#' # giftedness support yes or no)
#' # Estimating statistics for 100 cases per group
#' MAGMA_desc(Data = MAGMA_sim_data_gifted,
#'            covariates = covariates_gifted,
#'            group =  "gifted_support"
#'            step_num = 100,
#'            step_var = "step")
#'
#' }
#'
MAGMA_desc <- function(Data,
                       covariates,
                       group,
                       step_num = NULL,
                       step_var = NULL,
                       filename = NULL) {
  if (!is.data.frame(Data) && !tibble::is_tibble(Data)) {
    stop("Class data needs to be data frame, or tibble!")
  }

  if(!is.character(group) | length(group) > 2) {
    stop("group needs to be a character vector of maximum length 2!")
  }

  if(!is.character(covariates)) {
    stop("covariates needs to be a character or a character vector!")
  }

  if(!is.numeric(step_num) & !is.null(step_num)) {
    stop("step_num needs to be a numeric / integer or null!")
  }

  if(!is.null(step_num) & is.null(step_var) | is.null(step_num) & !is.null(step_var)) {
    stop("step_num and step_var need to be both NULL or both specified!")
  }

  if(!is.character(step_var) & !is.null(step_var)) {
    stop("step_var needs to be a character or null!")
  }

  if(!is.null(step_num)) {
    if(step_num >= max(Data[step_var], na.rm = T)) {
      stop("Step exceeded max step. Results equal to MAGMA_desc_post.")
    }
    Data <- Data %>%
      dplyr::filter(!!rlang::sym(step_var) <= step_num)
  }

  if(length(group) == 2) {
    Data <- Data %>%
      dplyr::mutate(group_long = dplyr::case_when(
        !!rlang::sym(group[1]) == unique(!!rlang::sym(group[1]))[1] &
          !!rlang::sym(group[2]) == unique(!!rlang::sym(group[2]))[1] ~ 1,
        !!rlang::sym(group[1]) == unique(!!rlang::sym(group[1]))[1] &
          !!rlang::sym(group[2]) == unique(!!rlang::sym(group[2]))[2] ~ 2,
        !!rlang::sym(group[1]) == unique(!!rlang::sym(group[1]))[2] &
          !!rlang::sym(group[2]) == unique(!!rlang::sym(group[2]))[1] ~ 3,
        !!rlang::sym(group[1]) == unique(!!rlang::sym(group[1]))[2] &
          !!rlang::sym(group[2]) == unique(!!rlang::sym(group[2]))[2] ~ 4
      ))
    group <- "group_long"
    cat("2x2 groups are represented as 4 groups.")
  }


  descs_overall <- Data %>%
    dplyr::select(tidyselect::all_of(group),
                  tidyselect::all_of(covariates)) %>%
    psych::describe() %>%
    dplyr::select(n, mean, sd) %>%
    tibble::as_tibble() %>%
    round(digits = 2)

  descs_group <- Data %>%
    dplyr::select(tidyselect::all_of(group),
                  tidyselect::all_of(covariates)) %>%
    split.data.frame(f = Data[group]) %>%
    lapply(FUN = function(data)
      psych::describe(data) %>%
        dplyr::select(n, mean, sd) %>%
        round(digits = 2)
    ) %>%
    do.call(what = cbind.data.frame) %>%
    purrr::set_names(paste(rep(
      seq(1:nrow(unique(Data[group]))) , each = 3),
      c("n", "mean", "sd")))

  if(ncol(descs_group) == 6) {
    index_matrix <- matrix(data = c("1", "2"),
                           ncol = 2)
  } else if(ncol(descs_group) == 9) {
    index_matrix <- matrix(data = c("1", "1", "2", "2", "3", "3"),
                           ncol = 2)
  } else if(ncol(descs_group) == 12) {
    index_matrix <- matrix(data = c("1", "1", "1", "2", "2", "3",
                                    "2", "3", "4", "3", "4", "4"),
                           ncol = 2)}

  effects_groups <- purrr::map2_dfc(index_matrix[, 1],
                                    index_matrix[, 2],
                                    cohen_d,
                                    Data = descs_group) %>%
    round(digits = 2)

  stats_overall <- cbind(descs_overall,
                         descs_group,
                         effects_groups)

  if(!is.null(filename)) {
    stats_overall %>%
      dplyr::mutate(names = rownames(stats_overall)) %>%
      dplyr::select(names, tidyselect::everything()) %>%
      #convert matrix into rough APA Table
      janitor::adorn_title(
        row_name = "Variable",
        col_name = "Statistic",
        placement = "combined") %>%
      flextable::flextable() %>%
      flextable::autofit() %>%
      flextable::save_as_docx(path = paste("./",filename,sep=""))
  }

  return(stats_overall)
}



#' cohen_d
#'
#' This function estimates Cohen's d in MAGMA_desc.
#'
#' Inner function of MAGMA_desc that computes Cohen's d using the pooled SD.
#'
#' @param Data A data frame that contains sample sizes, means, and standard
#' deviations.
#' @param index_1 Number of group 1.
#' @param index_2 Number of group_2
#'
#' @author Julian Urban
#'
#' @import tidyverse dplyr tidyselect
#' @importFrom purrr set_names
#' @importFrom rlang sym
#'
#' @return A vector of pairwise Cohen'ds.
#'
#'
cohen_d <- function(Data, index_1, index_2) {
  Data %>%
    dplyr::select(tidyselect::starts_with(index_1),
                  tidyselect::starts_with(index_2)) %>%
    dplyr::mutate(mean_diff = !!rlang::sym(paste(index_1, "mean", sep = " ")) - !!rlang::sym(paste(index_2, "mean", sep = " ")),
                  pooled_sd = sqrt(
                    ((!!rlang::sym(paste(index_1, "n", sep = " ")) - 1) * !!rlang::sym(paste(index_1, "sd", sep = " ")) *  !!rlang::sym(paste(index_1, "sd", sep = " ")) +
                       (!!rlang::sym(paste(index_2, "n", sep = " ")) - 1) * !!rlang::sym(paste(index_2, "sd", sep = " ")) *  !!rlang::sym(paste(index_2, "sd", sep = " "))) /
                      (!!rlang::sym(paste(index_1, "n", sep = " ")) + !!rlang::sym(paste(index_2, "n", sep = " ")))
                  ),
                  d = mean_diff/pooled_sd) %>%
    dplyr::select(d) %>%
    purrr::set_names(paste("d",
                           index_1,
                           index_2,
                           sep = "_"))
}
