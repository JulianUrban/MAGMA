#' Pairwise Cohen's and d-ratio
#'
#' d-ratio and pairwise Cohen's d with respect to sample size.
#'
#' This function computed the d-ratio and all pairwise effects
#'  with respect to sample size.
#'
#' @param da Specifying the data frame or tibble with the data.
#' @param gr A character vector specifying the IVs.
#' @param co A character vector naming the DVs.
#' @param st A character naming the variable for iteratively inclsuion.
#'
#' @author Julian Urban
#'
#' @import tidyverse psych
#' @return A list of length two. The first elemnt is a matrix including all
#' pairwise effects. The second is a vector expressing d-ratio
#' in dependency of sample size.
#'
#'
inner_d <- function(da, gr, co, st) {

  if(!is.data.frame(da) && !is_tibble(da)) {
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
    values_1 <- unique(da[gr[1]])
    values_2 <- unique(da[gr[2]])
    group_value <- 1
    da$group_d <- 99
    for(i in 1:nrow(values_1)) {
      for(j in 1:nrow(values_2)) {
        da <- da %>%
          mutate(group_d = case_when(
            .[gr[1]] == as.numeric(values_1[i, ]) &
               .[gr[2]] == as.numeric(values_2[j, ]) ~ group_value,
            TRUE ~ group_d
          ))
        group_value <- group_value + 1

      }
    }
    gr <- "group_d"
  }

  group_factor <- da %>%
    select(gr) %>%
    table(.) %>%
    length(.)

  #max step calculated equivalent to above
  max_step <- da %>%
    select(st) %>%
    max(., na.rm = T)
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


  iteration = 20

  while (iteration <= max_step){

    d_iteration <- double(nrow(d_matrix))
    suppressWarnings({
    group_stats <- da %>%
      filter(.[st] <= iteration) %>%
      select(IV = gr,
             all_of(co)) %>%
      group_by(IV) %>%
      summarise_at(., co, c(mean, var), na.rm = T)
    })

    for(i in 1:nrow(pairwise_matrix)) {
      for(j in 1:length(co)) {
        d_matrix[(i - 1) * length(co) + j, iteration] <-
          (as.numeric(group_stats[pairwise_matrix[i, 1], 1 + j]) -
           as.numeric(group_stats[pairwise_matrix[i, 2], 1 + j]))/
           sqrt((as.numeric(group_stats[pairwise_matrix[i, 1], 1 + j + length(co)]) +
              as.numeric(group_stats[pairwise_matrix[i, 2], 1 + j + length(co)]))/
               2)
      }
    }

    iteration <- iteration + 1
    }

  d_matrix <- abs(d_matrix)
  d_logic <- d_matrix < .20
  pairwise_effects <- list(d_rate = colSums(d_logic)/nrow(d_logic),
                         effects = abs(d_matrix))

  return(pairwise_effects)
}


#' Adjusted d-ratio
#'
#' adjusted d-ratio with respect to sample size.
#'
#' This function computed the adjusted d_ratio with respect to sample size.
#'
#' @param input An inner d object
#'
#' @author Julian Urban
#'
#' @import tidyverse psych
#' @return A vector containing the adjusted d-ratio in dependency of
#' sample size.
#'
#'
adj_d_ratio <- function(input) {

  if (!is_list(input) &&
      length(input) != 2 &&
      names(input) != c("d_rate", "effects")) {
    stop("input needs to be an inner d object!")
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
    matrix(., ncol = 1,
           nrow = ncol(input$effects) * nrow(input$effects))

  size_per_group <- c(1:ncol(input$effects))
  sd_g <- input$effects^2 %>%
    t() %>%
    as_tibble(., .name_repair = "minimal") %>%
    sapply(., inner_variance, size_per_group) %>%
    as_tibble(., .name_repair = "minimal") %>%
    sapply(., multiply_variance, J) %>%
    as_tibble(., .name_repair = "minimal") %>%
    t(.) %>%
    sqrt(.) %>%
    matrix(., ncol = 1,
           nrow = ncol(input$effects) * nrow(input$effects))
    })

  likelihood <- map2_dbl(g, sd_g, pnorm, q = .20) %>%
    matrix(., ncol = ncol(input$effects), nrow = nrow(input$effects)) %>%
    colSums()/nrow(input$effects)
  return(likelihood)
}
