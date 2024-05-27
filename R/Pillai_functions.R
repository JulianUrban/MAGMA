#' Pillai_DV
#'
#' DV matrix creator.
#'
#' This function creates the DV matrix for Pillai iterativ.
#'
#' @param data Specified by Pillai iterativ equlas its data input.
#' @param input Specified by Pillai iterativ naming the covariates.
#'
#' @author Julian Urban
#'
#' @import tidyverse
#' @return A matrix of the covariates
#' @noRd
#'
Pillai_DV <- function(data, input) {
  DV <- data %>%
    dplyr::select(tidyselect::all_of(input)) %>%
    as.matrix()
}


#' Pillai_iterativ
#'
#' Pillai's Trace with respect to sample size.
#'
#' This function computes Pillai's Trace for increasing sample size.
#'
#' @param da Specifying the data frame or tibble with the data.
#' @param gr A character vector specifying the IVs.
#' @param co A character vector naming the DVs.
#' @param st A character naming the variable for iteratively inclusion
#'
#' @author Julian Urban
#'
#' @import tidyverse dplyr tidyselect tibble
#' @importFrom psych describe
#' @importFrom stats manova
#' @importFrom rlang sym
#' 
#' @return A vector containing Pillai's Trace in dependency of sample size. If two
#' grouping variables were specified, the output is a matrix containing
#' Pillai's Trace for both IVs and their ineraction.
#'
#'
Pillai_iterativ <- function(da, gr, co, st) {
  if(!is.data.frame(da) && !tibble::is_tibble(da)) {
    stop("da needs to be an object of class dataframe or tibble!")
  }

  if(!is.character(gr) | length(gr) > 2) {
    stop("gr needs to be a character vector of maximum length 2!")
  }

  if(!is.character(co)) {
    stop("co needs to be a character or a character vector!")
  }

  if(!is.character(st) | length(st) > 1) {
    stop("st needs to be a character of length 1!")
  }

  max_step <- da %>%
    dplyr::select(tidyselect::all_of(st)) %>%
    max(na.rm = T)
  iteration <- length(co) + 20

  if(length(gr) == 1) {

  #Creating a vector for Traces
  suppressWarnings(Pillai_temp <- double(length = max_step) %>%
                     dplyr::recode(default = NA) %>%
                     as.numeric())

  #Computing all Traces with increasing sample size
  while (iteration <= max_step){
    Pillai_input <- da %>%
      dplyr::select(tidyselect::all_of(co),
                    tidyselect::all_of(gr),
                    tidyselect::all_of(st)) %>%
      purrr::set_names(c(co, "IV", "It")) %>%
      dplyr::filter(!!rlang::sym("It") <= iteration)
    
    if(sum(rowSums(is.na(Pillai_input)) == 0) < 20 + length(co)) {
      Pillai_temp[iteration] <- NA
      warning("Too few cases with valid data on all covariates to estimate Pillai's
              Trace for current samples size. Setting Pillai's Trace to NA.")
    } else if(length(co) == 1) {
      Square_sums <- stats::aov(Pillai_DV(data = Pillai_input,
                                          input = co) ~ IV,
                                data = Pillai_input) %>%
        summary() %>%
        `[[`(1) %>%
        `[[`("Sum Sq")
      
      Pillai_temp[iteration] <- Square_sums[1] / sum(Square_sums)
    } else {

    Pillai_temp[iteration] <- stats::manova(Pillai_DV(data = Pillai_input,
                                                      input = co) ~ IV,
                                            data = Pillai_input) %>%
      summary() %>%
      `[[`("stats") %>%
      `[[`(1, 2)
    }

    iteration <- iteration + 1
  }
  }

  if(length(gr) == 2) {
    #Creating a vector for Traces
    Pillai_temp <- matrix(NA,
                          ncol = max_step,
                          nrow = 3)
    while (iteration <= max_step) {
      Pillai_input <- da %>%
        dplyr::select(tidyselect::all_of(co),
                      tidyselect::all_of(gr),
                      tidyselect::all_of(st)) %>%
        purrr::set_names(c(co, "IV1", "IV2", "It")) %>%
        dplyr::filter(!!rlang::sym("It") <= iteration)
      if(sum(rowSums(is.na(Pillai_input)) == 0) < 20 + length(co)) {
        Pillai_temp[iteration] <- NA
        warning("Too few cases with valid data on all covariates to estimate Pillai's
              Trace for current samples size. Setting Pillai's Trace to NA.")
      } else if(length(co) == 1) {
        Square_sums <- stats::aov(Pillai_DV(data = Pillai_input,
                                            input = co) ~ IV1 + IV2 + IV1 * IV2,
                                  data = Pillai_input) %>%
          summary() %>%
          `[[`(1) %>%
          `[[`("Sum Sq")
        
        Pillai_temp[, iteration] <- c(Square_sums[1] / (Square_sums[1] + Square_sums[4]),
                                      Square_sums[2] / (Square_sums[2] + Square_sums[4]),
                                      Square_sums[3] / (Square_sums[3] + Square_sums[4]))
      } else {

      Pillai_temp[, iteration] <- stats::manova(Pillai_DV(data = Pillai_input,
                                                          input = co) ~ IV1 + IV2 + IV1 * IV2,
                                                data = Pillai_input) %>%
        summary() %>%
        `[[`("stats") %>%
        `[`(c(1:3), 2)
      }

      iteration <- iteration + 1
  }
  }

  return(Pillai_temp)
}



