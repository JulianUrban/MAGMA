#' J_group_size
#'
#' inner function of \code{\link{mean_g_meta}}.
#'
#' This function computes J over samples sizes necessary for Hedges' g.
#'
#' @param group_size A numeric defining the max sample size for which J
#' should be computed.
#'
#' @author Julian Urban
#'
#' @import tidyverse
#' @importFrom purrr set_names
#' @return A vector of J's in dependency of sample size.
#'
#'
J_group_size <- function(group_size) {
  N <- c(1:group_size)
  J <- 1 - (3/(4 * (2 * N - 2) - 1))
  return(J)
}



#' mean_g_meta
#'
#' Mean standardized effect.
#'
#' This function computes the mean effect. Method varies between two and
#' many group matchings.
#'
#' @param input An inner d object.
#' @param number_groups A numeric specifying the number of groups.
#'
#' @author Julian Urban
#'
#' @import tidyverse metafor robumeta
#' @importFrom psych describe
#' @importFrom rlang is_list
#' 
#' @return A vector containing the mean g in dependency of sample size.
#'
#'
mean_g_meta <- function(input, number_groups) {
    if (!is.numeric(number_groups) | number_groups < 2) {
    stop("number_groups needs to be an integer of at least two!") #Check that number of groups is an integer
  }
  J <- J_group_size(ncol(input))
  suppressMessages({
    g <- apply(input,
               MARGIN = 1,
               FUN = function(row) {
                 row * J
               }) %>%
      t() %>%
      abs()

  size_per_group <- c(1:ncol(g))

  variance_g <- apply(input,
                      MARGIN = 1,
                      FUN = function(row) {
                        (row^2 / (4 * size_per_group) + 2 * size_per_group / size_per_group^2) * J^2
                      }) %>%
    t()
  })

  #meta-analysis can not tak NAs as input. Defining starting value for analysis
  starting_number <- min(which(!is.na(g[1, ])))
  #Create vector to storre mean_g
  mean_g <- matrix(NA,
                   ncol =  ncol(g),
                   nrow = 1) %>%
                     as.numeric()

  if (number_groups == 2) {
    cat("\n", "Mean g was computed using random effects meta-analysis with metafor.")
    for (i in starting_number:ncol(g)) {
      ma_input <- cbind(g[, i], variance_g[, i])
      mean_g[i] <- metafor::rma(ma_input[, 1],
                                ma_input[, 2])[["b"]]
          }
  } else {
    cat("\n", "Mean g was computed using robust variance meta-analysis with robumeta.")
    number_covariates <- 1/2 * ((number_groups - 1)^2 + (number_groups - 1))
    for (i in starting_number:ncol(g)) {
      ma_input <- tibble::tibble(V1 = g[, i],
                                    V2 = variance_g[, i],
                                    V3 = rep(c(1:number_covariates),
                                              nrow(g)/number_covariates)) #create nesting variable
      mean_g[i] <- robumeta::robu(V1 ~ 1,
                                  var.eff.size = unlist(ma_input[, 2]),
                                  studynum = unlist(ma_input[, 3]),
                                  data = ma_input)[["b.r"]]
      }
  }
  return(mean_g)
}






