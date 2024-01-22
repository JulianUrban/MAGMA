#' Inner MAGMA-matching process function
#'
#' matches Cases iteratively during MAGMA
#'
#' This function conducts the matching process, by extracting the match with
#' the lowest distance
#'
#' @param distance_input distance matrix to extract lowest distance
#' @param output_list output where MAGMA results get stored
#' @param rep_elements sample sizes per group
#'
#' @return A matched sample including the variables step, weight & distance
#'
#'
match_iterative <- function(distance_input, output_list, rep_elements) {
  iteration_max <- min(rep_elements)
  iteration <- 1

  if(length(dim(distance_input)) == 2) {

    while (iteration <= iteration_max) {
      index <- which(distance_input == min(distance_input, na.rm = T), arr.ind = T)[1, ]

      output_list[[1]][["step"]][[index[1]]] <- iteration
      output_list[[2]][["step"]][[index[2]]] <- iteration
      output_list[[1]][["weight"]][[index[1]]] <- 1
      output_list[[2]][["weight"]][[index[2]]] <- 1
      output_list[[1]][["distance"]][[index[1]]] <- min(distance_input, na.rm = T)
      output_list[[2]][["distance"]][[index[2]]] <- min(distance_input, na.rm = T)

      distance_input[index[1], ] <- NA
      distance_input[, index[2]] <- NA

      iteration <- iteration + 1
    }
  } else if(length(dim(distance_input)) == 3) {

    while (iteration <= iteration_max) {
      index <- which(distance_input == min(distance_input, na.rm = T), arr.ind=T)[1, ] # Indizierung notwenig, wenn zufällig selbe Distanz zweimal vorhanden

      output_list[[1]][["step"]][[index[1]]] <- iteration
      output_list[[2]][["step"]][[index[2]]] <- iteration
      output_list[[3]][["step"]][[index[3]]] <- iteration
      output_list[[1]][["weight"]][[index[1]]] <- 1
      output_list[[2]][["weight"]][[index[2]]] <- 1
      output_list[[3]][["weight"]][[index[3]]] <- 1
      output_list[[1]][["distance"]][[index[1]]] <- min(distance_input, na.rm = T)
      output_list[[2]][["distance"]][[index[2]]] <- min(distance_input, na.rm = T)
      output_list[[3]][["distance"]][[index[3]]] <- min(distance_input, na.rm = T)


      distance_input[index[1], , ] <- NA
      distance_input[, index[2], ] <- NA
      distance_input[, , index[3]] <- NA

      iteration <- iteration + 1
    }
  } else if(length(dim(distance_input)) == 4) {
    while (iteration <= iteration_max) {
      index <- which(distance_input == min(distance_input, na.rm = T), arr.ind=T)[1, ]

      output_list[[1]][["step"]][[index[1]]] <- iteration
      output_list[[2]][["step"]][[index[2]]] <- iteration
      output_list[[3]][["step"]][[index[3]]] <- iteration
      output_list[[4]][["step"]][[index[4]]] <- iteration
      output_list[[1]][["weight"]][[index[1]]] <- 1
      output_list[[2]][["weight"]][[index[2]]] <- 1
      output_list[[3]][["weight"]][[index[3]]] <- 1
      output_list[[4]][["weight"]][[index[4]]] <- 1
      output_list[[1]][["distance"]][[index[1]]] <- min(distance_input, na.rm = T)
      output_list[[2]][["distance"]][[index[2]]] <- min(distance_input, na.rm = T)
      output_list[[3]][["distance"]][[index[3]]] <- min(distance_input, na.rm = T)
      output_list[[4]][["distance"]][[index[4]]] <- min(distance_input, na.rm = T)


      distance_input[index[1], , , ] <- NA
      distance_input[, index[2], , ] <- NA
      distance_input[, , index[3], ] <- NA
      distance_input[, , , index[4]] <- NA

      iteration <- iteration + 1
    }
  }

  return(output_list)
}


#' Inner MAGMA distance estimation preparation function
#'
#' prepares distance estimation
#'
#' This function uses the PS inputs to prepare them for distance estimation
#'
#' @param input_list Data with PS
#' @param rep_element sample sizes per group
#' @param name_ps names of PS in data
#'
#' @import tidyverse dplyr 
#' @importFrom purrr map
#'
#' @return the input for distance estimation
#'
#'
build_value_matrix <- function(input_list, rep_element, name_ps = "distance_ps") {
  value_matrix <- matrix(NA, nrow = prod(rep_element), ncol = length(rep_element))
  if(length(rep_element) == 2) {

    value_matrix[, 1] <- rep(input_list[[1]][[name_ps]], rep_element[2])
    value_matrix[, 2] <- purrr::map(input_list[[2]][[name_ps]], rep, rep_element[1]) %>%
      unlist()

  } else if(length(rep_element) == 3) {

    value_matrix[, 1] <- rep(input_list[[1]][[name_ps]], rep_element[2] * rep_element[3])
    value_matrix[, 2] <- purrr::map(input_list[[2]][[name_ps]], rep, rep_element[1]) %>%
      unlist() %>%
      rep(times = rep_element[3])
    value_matrix[, 3] <- purrr::map(input_list[[3]][[name_ps]], rep, rep_element[1] * rep_element[2]) %>%
      unlist()

  } else if(length(rep_element) == 4) {

    value_matrix[, 1] <- rep(input_list[[1]][[name_ps]], rep_element[2] * rep_element[3] * rep_element[4])
    value_matrix[, 2] <- purrr::map(input_list[[2]][[name_ps]], rep, rep_element[1]) %>%
      unlist() %>%
      rep(times = rep_element[3] * rep_element[4])
    value_matrix[, 3] <- purrr::map(input_list[[3]][[name_ps]], rep, rep_element[1] * rep_element[2]) %>%
      unlist() %>%
      rep(times = rep_element[4])
    value_matrix[, 4] <- purrr::map(input_list[[4]][[name_ps]], rep, rep_element[1] * rep_element[2] * rep_element[3]) %>%
      unlist()

  }
  return(value_matrix)
}
