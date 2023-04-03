#' J per group size
#'
#' inner function of mean_g.
#'
#' This function computes J over samples sizes necessary for Hedges' g.
#'
#' @param group_size A numeric defining the max sample size for which J
#' should be computed.
#'
#' @author Julian Urban
#'
#' @import tidyverse
#' @return A vector of J's in dependency of sample size.
#'
#'
J_group_size <- function(group_size) {
  J <- tibble(N = c(1:group_size),
            J = NA) %>%
    transmute(J = 1 - (3/(4 * (2 * N - 2) - 1)))
  J <- as.numeric(J$J)
  return(J)
}


#' multiply
#'
#' inner function of mean_g.
#'
#' This function multiplies two numerics.
#'
#' @param x A numeric
#' @param y A numeric
#'
#' @author Julian Urban
#'
#' @return A product.
#'
#'
multiply <- function(x, y) {
  x * y
}

#' multiply for variance
#'
#' inner function of mean_g.
#'
#' This function multiplies a numeric with another squared numeric.
#'
#' @param x A numeric
#' @param y A numeric
#'
#' @author Julian Urban
#'
#' @return A product.
#'
multiply_variance <-function(x, y) {
  x * y^2
}



#' inner variance
#'
#' inner function of mean_g.
#'
#' This function computed the variance of a Cohen's d.
#'
#' @param d_square The squared Cohen's d.
#' @param size_per_group Specifies the group size.
#'
#' @author Julian Urban
#'
#' @return A numeric specifying the variance of an effect.
#'
inner_variance <- function(d_square, size_per_group) {
  d_square/(4 * size_per_group) + 2 * size_per_group/size_per_group^2
}

#' mean standardized effect
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
#' @import tidyverse psych metafor robumeta
#'
#' @return A vector containing the mean g in dependency of sample size.
#'
#'
mean_g_meta <- function(input, number_groups) {
  #Check, that input is an inner d object: addapting for more possibilites
  if (!is_list(input) &&
      length(input) != 2 &&
      names(input) != c("d_rate", "effects")) {
    stop("input needs to be an inner d object!")
  }
  if (!is.numeric(number_groups) | number_groups < 2) {
    stop("number of groups needs to be an integer of at least two!") #Check that number of groups is an integer
  }
  J <- J_group_size(ncol(input$effects))
  suppressMessages({
  g <- input$effects %>%
    t() %>%
    as_tibble(., .name_repair = "minimal") %>%
    sapply(., multiply, J) %>%
    as_tibble(., .name_repair = "minimal") %>%
    t() %>%
    abs() %>%
    as_tibble(., .name_repair = "minimal")

  size_per_group <- c(1:ncol(input$effects))
  variance_g <- input$effects^2 %>% #improve code, avoid t()
    t() %>%
    as_tibble(., .name_repair = "minimal") %>%
    sapply(., inner_variance, size_per_group) %>%
    as_tibble(., .name_repair = "minimal") %>%
    sapply(., multiply_variance, J) %>%
    as_tibble(., .name_repair = "minimal") %>%
    t()%>%
    as_tibble(., .name_repair = "minimal")
  })

  #meta-analysis can not tak NAs as input. Defining starting value for analysis
  starting_number <- min(which(!is.na(g[1, ])))
  #Create vector to storre mean_g
  mean_g <- matrix(NA,
                   ncol =  ncol(g),
                   nrow = 1) %>%
                     as.numeric()

  if (number_groups == 2) {
    print("mean g was computed using random effects meta-analysis with metafor.")
    for (i in starting_number:ncol(g)) {
      ma_input <- cbind(g[, i], variance_g[, i])
      mean_g[i] <- metafor::rma(ma_input[, 1],
                                ma_input[, 2]) %>%
        .[["b"]]
          }
  } else {
    print("mean g was computed using robust variance meta-analysis with robumeta.")
    number_covariates <- 1/2 * ((number_groups - 1)^2 + (number_groups - 1))
    for (i in starting_number:ncol(g)) {
      ma_input <- cbind(g[, i],
                      variance_g[, i],
                      rep(c(1:number_covariates),
                          nrow(g)/number_covariates)) #create nesting variable
      mean_g[i] <- robumeta::robu(ma_input[, 1] ~ 1, var.eff.size = ma_input[, 2],
                        studynum = ma_input[, 3], data = ma_input) %>%
        .[["b.r"]]}
  }
  return(mean_g)
}






