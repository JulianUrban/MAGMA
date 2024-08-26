#' inner_d
#'
#' d-ratio and pairwise Cohen's d with respect to sample size.
#'
#' This function computed the d-ratio and all pairwise effects
#'  with respect to sample size.
#'
#' @param da Specifying the data frame or tibble with the data.
#' @param gr A character vector specifying the IVs.
#' @param co A character vector naming the DVs.
#' @param st A character naming the variable for iteratively inclusion
#'
#' @author Julian Urban
#'
#' @import tidyverse tibble tidyselect
#' @importFrom rlang sym
#' 
#' @return A list of length two. The first element is a matrix including all
#' pairwise effects. The second is a vector expressing d-ratio
#' in dependency of sample size.
#'
#'
inner_d <- function(da, gr, co, st, co_ord = NULL, co_nom = NULL) {

  if(!is.data.frame(da) && !tibble::is_tibble(da)) {
    stop("da needs to be an object of class dataframe or tibble!")
  }

  if(!is.character(gr)) {
    stop("gr needs to be a character of maximum length 1!")
  }

  if(!is.character(co)) {
    stop("co needs to be a character or a character vector!")
  }

  if(!is.character(st) | length(st) > 1) {
    stop("st needs to be a character of length 1!")
  }
  #helpful: coding group variable as integers from 1 up
  #calculating factor for number of pairwise effects for nrow of effect matrix
  #function for pyramid numbers that are multplied with number of covariates
  #1/2*( (x-1)^2+(x-1)) with x being number of groups

  if(length(gr) == 2) {
    values_1 <- unlist(unique(da[gr[1]]))
    values_2 <- unlist(unique(da[gr[2]]))

    da <- da %>%
      dplyr::mutate(group_d = dplyr::case_when(
        !!rlang::sym(gr[1]) == values_1[1] &
          !!rlang::sym(gr[2]) == values_2[1] ~ 1,
        !!rlang::sym(gr[1]) == values_1[1] &
          !!rlang::sym(gr[2]) == values_2[2] ~ 2,
        !!rlang::sym(gr[1]) == values_1[2] &
          !!rlang::sym(gr[2]) == values_2[1] ~ 3,
        !!rlang::sym(gr[1]) == values_1[2] &
          !!rlang::sym(gr[2]) == values_2[2] ~ 4
      ))

    gr <- "group_d"
  }

  group_factor <- da %>%
    dplyr::select(tidyselect::all_of(gr)) %>%
    table() %>%
    length()

  #max step calculated equivalent to above
  max_step <- da %>%
    dplyr::select(tidyselect::all_of(st)) %>%
    max(na.rm = T)
  #spanning matrix for all pairwise effects
  d_matrix <- matrix(NA,
                     ncol = max_step,
                     nrow = (length(co) * (.5 * ((group_factor - 1)^2 +
                                                   (group_factor - 1)))))

  #defining pairwise comparisons depending on number of group
  #current version: only some predefined matrices (load reduction): add additiona function if number group exceeds max
  if (group_factor == 2) {
    pairwise_matrix <- matrix(c(1, 2),
                              ncol = 2,
                              nrow = 1)
    names_covariates <- co
  } else if (group_factor == 3) {
    pairwise_matrix <- matrix(c(1, 1, 2, 2, 3, 3),
                              ncol = 2,
                              nrow = 3)
    names_covariates <- purrr::map2(pairwise_matrix[, 1],
                                     pairwise_matrix[, 2],
                                     function(group_1, group_2)
                                       paste(co, group_1, group_2, sep = "_")) %>%
      unlist()
      } else if (group_factor == 4) {
    pairwise_matrix <- matrix(c(1, 1, 1, 2, 2, 3, 2, 3, 4, 3, 4, 4),
                              ncol = 2,
                              nrow = 6)
    names_covariates <- purrr::map2(pairwise_matrix[, 1],
                                     pairwise_matrix[, 2],
                                     function(group_1, group_2)
                                       paste(co, group_1, group_2, sep = "_")) %>%
      unlist()
  } else { #function independent of group number. Currently breaking algorithm
    pairwise_matrix <- NA
    stop("To many groups (maximum possible 4) for current package version!")
  }


  group_values <- unique(na.omit(da[, gr]))
  effects <- sapply(c(20:max_step),
                    function(iteration) {
                     sapply(c(1:nrow(pairwise_matrix)),
                             function(index) {
                               groups <- group_values[pairwise_matrix[index, ]]
                               data_temp <- da[da[, st] <= iteration & da[, gr] %in% groups, ]  
                               suppressWarnings({
                                 group_stats <- data_temp %>%
                                   dplyr::select(!!rlang::sym(gr),
                                                 tidyselect::all_of(co)) %>%
                                   dplyr::group_by(!!rlang::sym(gr)) %>%
                                   dplyr::summarise_at(.vars = co,
                                                       .funs = c(mean, var),
                                                       na.rm = T)
                               })
                               means <- seq(2, length(co) + 1, 1)
                               sds <- seq(length(co) + 2, ncol(group_stats), 1)
                               mean_diffs <- group_stats[1, means] - group_stats[2, means]
                               pooled_sds <- sqrt((group_stats[1, sds] + group_stats[2, sds]) / 2)
                               ds <- unlist(mean_diffs / pooled_sds)
                               names_effects <- co
                               
                               suppressWarnings({
                               if(!is.null(co_ord)) {
                                 ordinal_effects <- effect_ordinal(Data = data_temp,
                                                                   group = gr,
                                                                   variable = co_ord)
                                 names(ordinal_effects) <- co_ord
                                 ds <- unlist(c(ds, ordinal_effects))
                                 
                               }
                               if(!is.null(co_nom)) {
                                 nominal_effects <- effect_nominal(Data = data_temp,
                                                                   group = gr,
                                                                   variable = co_nom)
                                 names(nominal_effects) <- co_nom
                                 ds <- c(ds, nominal_effects)
                                 
                               }
                               })
                               return(ds)
                             })
                    }) 
  
names_effects <- co
if(!is.null(co_ord)) {names_effects <- c(names_effects, co_ord)}
if(!is.null(co_nom)) {names_effects <- c(names_effects, co_nom)}
rownames(effects) <- rep(names_effects, each = nrow(pairwise_matrix))
effects <- cbind(matrix(NA, nrow = nrow(effects), ncol = 19),
                 effects)
  


 
  d_logic <- abs(effects) < .20
  pairwise_effects <- list(d_rate = colSums(d_logic)/nrow(d_logic),
                         effects = effects)

  return(pairwise_effects)
}


#' adj_d_ratio
#'
#' adjusted d-ratio with respect to sample size.
#'
#' This function computed the adjusted d_ratio with respect to sample size.
#'
#' @param input An inner d object
#'
#' @author Julian Urban
#'
#' @import tidyverse tibble
#' @importFrom purrr map2
#' @importFrom purrr map2_dbl
#' @importFrom rlang is_list
#' 
#' @return A vector containing the adjusted d-ratio in dependency of
#' sample size.
#'
#'
adj_d_ratio <- function(input) {

  J <- J_group_size(ncol(input))

suppressMessages({
  
  g <- apply(input,
             MARGIN = 1,
             FUN = function(row) {
               row * J
             }) %>%
    t() %>%
    abs()
 
  size_per_group <- c(1:ncol(input))
  sd_g <- apply(input,
                      MARGIN = 1,
                      FUN = function(row) {
                        (row^2 / (4 * size_per_group) + 2 * size_per_group/size_per_group^2) * J^2
                      }) %>%
    t() %>%
    sqrt()
    })

  likelihood <- purrr::map2_dbl(g, sd_g, stats::pnorm, q = .20) %>%
    matrix(ncol = ncol(input), nrow = nrow(input)) %>%
    colSums()/nrow(input)
  return(likelihood)
}
