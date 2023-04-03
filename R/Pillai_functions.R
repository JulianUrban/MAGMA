#' Pillai DV creation
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
#' @import tidyverse psych
#' @return A matrix of the covariates
#'
#'
Pillai_DV <- function(data, input) {
  suppressMessages(attach(data))
  DV <- eval(parse(text = input[1]))
  for(i in 2:length(input)) {
    DV <- cbind(DV, eval(parse(text = input[i])))
  }
  return(DV)
  suppressMessages(detach(data))
}


#' Pillai's Trace
#'
#' Pillai's Trace with respect to sample size.
#'
#' This function computes Pillai's Trace for increasing sample size.
#'
#' @param da Specifying the data frame or tibble with the data.
#' @param gr A character vector specifying the IVs.
#' @param co A character vector naming the DVs.
#' @param st A character naming the variable for iteratively inclsuion.
#'
#' @author Julian Urban
#'
#' @import tidyverse psych
#' @return A vector containg Pillai's Trace in dependeny of sample size. If two
#' grouping variables were specified, the output is a matrix containing
#' Pillai's Trace for both IVs and their ineraction.
#'
#'
Pillai_iterativ <- function(da, gr, co, st) {
  if(!is.data.frame(da) && !is_tibble(da)) {
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
    select(all_of(st)) %>%
    max(., na.rm = T)
  iteration = length(co) + 20

  if(length(gr) == 1) {

  #Creating a vector for Traces
  suppressWarnings(Pillai_temp <- double(length = max_step) %>%
                     dplyr::recode(., default = NA) %>%
                     as.numeric())

  #Computing all Traces with increasing sample size
  while (iteration <= max_step){
    Pillai_input <- da %>%
      select(all_of(co),
             IV = gr,
             It = st) %>%
      filter(It <= iteration)

    Pillai_temp[iteration] <- manova(Pillai_DV(Pillai_input, co) ~ IV,
                                     data = Pillai_input) %>%
      summary(.) %>%
      .[["stats"]] %>%
      .[1, 2]

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
        select(all_of(co),
              IV1 = gr[1],
              IV2 = gr[2],
               It = st) %>%
        filter(It <= iteration)

      Pillai_temp[, iteration] <- Pillai_input %>%
        manova(Pillai_DV(., co) ~ IV1 + IV2 + IV1 * IV2, data = .) %>%
        summary(.) %>%
        .[["stats"]] %>%
        .[c(1:3), 2]

      iteration <- iteration + 1
  }
  }

  return(Pillai_temp)
}



