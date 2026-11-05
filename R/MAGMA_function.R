#' distance_estimator
#'
#' estimates distance in \code{\link{MAGMA}}.
#'
#' This function is an inner function of \code{\link{MAGMA}}. It estimates the
#' distance of all possible matches.
#'
#' @param data A matrix containing all possible combinations.
#' @param means A matrix containing all row means of all possible matches.
#' @param variance A numeric indicating the variance of the propensity scores.
#' @param cores An integer defining the number of cores used for
#' parallel computation.
#' @param inp input parameter for parallel distance computation.
#'
#' @author Julian Urban
#'
#' @import tidyverse parallel doParallel foreach
#' @importFrom stats mahalanobis
#' 
#' @return A matrix of distance for each case of each possible match.
#' @noRd
#'
distance_estimator <- function(data, means, variance, cores, inp = NULL) {
  cl <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl, cores = cores)
  distance_matrix <- foreach::foreach(inp = data, .combine = cbind) %dopar%
    stats::mahalanobis(inp, center = means, cov = variance)
  parallel::stopCluster(cl)
  return(distance_matrix)
}

#' distance_estimator MD
#'
#' estimates distance in \code{\link{MAGMA}} for Mahalanobis Distance Matching.
#'
#' This function is an inner function of \code{\link{MAGMA}}. It estimates the
#' Mahalanobis distance of all possible matches.
#'
#' @param data A matrix containing all possible combinations.
#' @param variance The variance-covariance matrix of the covariates.
#' @param cores An integer defining the number of cores used for
#' parallel computation.
#' @param rows An integer defining the number of groups
#' @param weights Possible weights for estimating the Mahalanobis distance.
#' @param chunk input parameter for parallel distance computation.
#'
#' @author Julian Urban
#'
#' @import parallel doParallel foreach
#' 
#' @return A matrix of distance for each case of each possible match.
#' @noRd
#'
distance_estimator_MD <- function(data, 
                                  variance, cores, rows, weights = NULL, chunk = NULL) {
  if(cores > 1) {
    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)
    
    chunks <- split(seq_len(nrow(data)),
                    cut(seq_len(nrow(data)),
                        cores,
                        labels = FALSE))
    
    distance_matrix <- foreach::foreach(chunk = chunks,
                                        .combine = c) %dopar% {
                                          x <- sapply(chunk,
                                                      function(row) {
                                                        temp <- matrix(data[row, ],
                                                                       nrow = rows,
                                                                       byrow = TRUE)
                                                        means <- colMeans(temp)
                                                        
                                                        centered_temp <- sweep(temp, 2L, means)
                                                        
                                                        if(is.null(weights)) {
                                                          weights <- rep(1, length(means))
                                                        }
                                                        weight_mat <- diag(weights)
                                                        
                                                        mean(sqrt(rowSums(centered_temp %*% weight_mat %*% solve(variance) %*% weight_mat * centered_temp)))
                                                      })
                                        }
    parallel::stopCluster(cl) 
  } else {
    distance_matrix <- sapply(c(1:nrow(data)),
                              function(row) {
                                temp <- matrix(data[row, ],
                                               nrow = rows,
                                               byrow = TRUE)
                                means <- colMeans(temp)
                                
                                centered_temp <- sweep(temp, 2L, means)
                                
                                if(is.null(weights)) {
                                  weights <- rep(1, length(means))
                                }
                                weight_mat <- diag(weights)
                                
                                mean(sqrt(rowSums(centered_temp %*% weight_mat %*% solve(variance) %*% weight_mat * centered_temp)))
                                
                                
                              })
  }
  
  
  return(distance_matrix)
}


#' reference_matching
#'
#' Conducts reference-group matching for three or four groups.
#'
#' @param input Matching input data.
#' @param reference_group Value identifying the reference group. If NULL, the
#' smallest group is used.
#' @param number_candidates Number of nearest candidates retained per
#' non-reference group.
#' @param cores Number of cores used to evaluate reference cases.
#' @param variance Variance of the propensity score.
#'
#' @return Matching variables for all successfully matched cases.
#' @noRd
#'
reference_matching <- function(input,
                               reference_group = NULL,
                               number_candidates = 10L,
                               cores = 1L,
                               variance) {
  group_values <- unique(input$group)
  group_sizes <- vapply(group_values,
                        function(group_value) {
                          sum(input$group == group_value)
                        },
                        integer(1))

  if(is.null(reference_group)) {
    reference_group <- group_values[which.min(group_sizes)]
  }

  if(length(reference_group) != 1L || !reference_group %in% group_values) {
    stop("reference_group needs to identify one existing group!")
  }

  reference_indices <- which(input$group == reference_group)
  other_groups <- setdiff(group_values, reference_group)
  remaining_references <- reference_indices
  used_indices <- integer(0)

  input$match_id <- NA_integer_
  match_counter <- 0L

  find_best_match <- function(i) {
    reference_ps <- input$distance_ps[i]

    candidates <- lapply(other_groups,
                         function(group_index) {
                           group_indices <- which(
                             input$group == group_index &
                               !(seq_len(nrow(input)) %in% used_indices)
                           )

                           if(length(group_indices) == 0) {
                             return(integer(0))
                           }

                           group_ps <- input$distance_ps[group_indices]
                           differences <- abs(group_ps - reference_ps)

                           sorted_indices <- group_indices[order(differences,
                                                                 group_ps,
                                                                 input$ID[group_indices])]

                           head(sorted_indices, number_candidates)
                         })

    if(any(lengths(candidates) == 0L)) {
      return(NULL)
    }

    candidate_group_list <- c(list(input[i, , drop = FALSE]),
                              lapply(candidates,
                                     function(candidate_indices) {
                                       input[candidate_indices, , drop = FALSE]
                                     }))

    elements_temp <- vapply(candidate_group_list,
                            nrow,
                            integer(1))

    value_matrix <- build_value_matrix(input_list = candidate_group_list,
                                       rep_element = elements_temp,
                                       name_ps = "distance_ps")

    combination_means <- rowMeans(value_matrix)
    centered_values <- sweep(value_matrix,
                             MARGIN = 1L,
                             STATS = combination_means,
                             FUN = "-")

    combination_distances <- rowMeans(centered_values^2) / variance
    best_combination <- which.min(combination_distances)

    best_positions <- arrayInd(best_combination,
                               .dim = elements_temp)[1, ]

    selected_indices <- c(i,
                          vapply(seq_along(candidates),
                                 function(group_number) {
                                   candidate_position <- best_positions[group_number + 1L]
                                   candidates[[group_number]][candidate_position]
                                 },
                                 integer(1)))

    return(list(reference_index = i,
                selected_indices = selected_indices,
                distance = combination_distances[best_combination]))
  }

  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)

  number_workers <- min(cores,
                        length(reference_indices),
                        future::availableCores())

  if(number_workers > 1L) {
    future::plan(future::multisession,
                 workers = number_workers)
  }

  while(length(remaining_references) > 0) {
    if(number_workers > 1L) {
      possible_matches <- future.apply::future_lapply(
        remaining_references,
        find_best_match,
        future.seed = FALSE,
        future.scheduling = 1
      )
    } else {
      possible_matches <- lapply(remaining_references,
                                 find_best_match)
    }

    possible_matches <- Filter(Negate(is.null),
                               possible_matches)

    if(length(possible_matches) == 0) {
      warning("No further complete matches are possible.")
      break
    }

    possible_distances <- vapply(possible_matches,
                                 function(match) match$distance,
                                 numeric(1))

    best_match <- possible_matches[[which.min(possible_distances)]]
    match_counter <- match_counter + 1L
    selected_indices <- best_match$selected_indices

    input$match_id[selected_indices] <- match_counter
    input$weight[selected_indices] <- 1L
    input$distance[selected_indices] <- best_match$distance

    used_indices <- c(used_indices,
                      selected_indices)

    remaining_references <- setdiff(remaining_references,
                                    best_match$reference_index)
  }

  matched <- !is.na(input$match_id)

  if(any(matched)) {
    match_distances <- tapply(input$distance[matched],
                              input$match_id[matched],
                              mean)

    step_lookup <- setNames(rank(match_distances,
                                 ties.method = "first"),
                            names(match_distances))

    input$step[matched] <- unname(
      step_lookup[as.character(input$match_id[matched])]
    )
  }

  return(input[matched,
               c("ID", "step", "weight", "distance")])
}


#' MAGMA
#'
#' This function conducts many group matching for 2 to 4 groups. It augments
#' the original data set by the relevant 'MAGMA.R' variables. For details, see
#' below.
#'
#' This function conducts nearest neighbor many group matching. It is
#' applicable for two to four groups or a 2x2 design. As output, this function
#' augments your original data by the variables *weight*, *step*, *distance*,
#' and *ID*. Weight indicates whether a case was matched. Step specifies the
#' iteration in which a case was matched. It also shows which cases were matched
#' together. Distance indicates the mean difference within a match. Since
#' matches with a lower distance are matched in an earlier iteration, step and
#' distance are strongly correlated.
#' Matching can require substantial CPU time and memory because the number of
#' possible matches increases with the product of the group sample sizes. For
#' three- and four-group matching, the computational procedure is controlled by
#' *large_method*. With the default *large_method = "subgroup"*, MAGMA evaluates
#' all possible matches while their number is below *combination_threshold*. If
#' the threshold is exceeded, MAGMA switches to random quasi-systematic matching
#' within subgroups and informs the user. Because subgroup assignment is random,
#' results can depend on the random seed and balance may vary slightly between
#' runs.
#' Alternatively, *large_method = "reference"* conducts reference-group
#' matching independently of *combination_threshold*. For every case in the
#' specified reference group, MAGMA considers the nearest
#' *number_candidates* cases from each non-reference group and iteratively
#' selects the currently best complete match. If *reference_group* is not
#' specified, the smallest group is used. This procedure avoids constructing
#' the complete array of all possible matches and does not depend on random
#' subgroup assignment. 
#' Both procedures return the input data augmented by the same matching
#' variables, but the resulting matches can differ between procedures.
#'
#'
#' @param Data A data frame or tibble containing at least your grouping and
#' distance variable. Data needs to be specified in your environment.
#' @param group A character specifying the name of
#' your grouping variable in the data. Note that MAGMA can only match your data
#' for a maximum of 4 groups. Matching over two grouping variables (e.g., 2x2
#' Design) is possible by specifying group as a character vector with a length
#' of two. In this case, each or the 2 grouping variables can only have two
#' levels.
#' @param dist A character specifying the name of the propensity score
#' in data.
#' @param cores An integer defining the number of cores used for
#' parallel computation.
#' @param verbose TRUE or FALSE indicating whether matching information should
#' be printed to the console.
#' @param covs Only relevant for Mahalanobis distance matching. Specifies the
#' names of the variables for which Mahalanobis distance matching should be
#' conducted as a character vector.
#' @param weights Optional parameter for Mahalanobis distance matching. Should
#' be a numeric vector with the same length as covs. These weights are used to
#' estimate a weightes Mahalanobis distance. If not specified, all covariates
#' are weighted equally.
#' @param large_method A character that handles how matching is conducted for
#' three or four groups with large sample sizes. The default "subgroup" uses 
#' complete matching below *combination_threshold* and random subgroup matching 
#' above it. The option "reference" uses reference-group matching independently 
#' of the threshold.
#' @param reference_group A value defining which group is the reference group
#' for large_method = "reference". If NULL, the smallest group is used.
#' @param number_candidates An integer for large_method = "reference" defining
#' the number of candidates per non-reference group that are considered when
#' searching for the locally best complete match.
#' @param combination_threshold A positive number defining the maximum number
#' of possible combinations for complete matching when large_method =
#' "subgroup".
#' 
#'
#' @author Julian Urban
#'
#' @import tidyverse parallel doParallel foreach dplyr tibble tidyselect
#' @importFrom purrr set_names
#' @importFrom stats var
#' @importFrom stats cov
#' @importFrom rlang sym
#' 
#' @return Your input data frame augmented with matching
#' relevant variables, namely *weight*, *step*, *distance*, and *ID*. In case
#' of missing values on the distance or group variable, MAGMA excludes them for
#' the matching process. The returned data set does not contain those excluded
#' cases. For more information, see Details.
#' @export
#'
#' @examples
#' 
#' # Running this code will take a while
#' # Two-group exact matching using the data set 'MAGMA_sim_data'
#' # Matching variable 'gifted_support' (received giftedness support yes or no)
#' # 'MAGMA_sim_data_gifted' contains the result of the matching
#' MAGMA_sim_data_gifted <- MAGMA(Data = MAGMA_sim_data,
#'                                 group = "gifted_support",
#'                                 dist = "ps_gifted",
#'                                 cores = 1)
#' head(MAGMA_sim_data_gifted)
#' 
#' \donttest{
#' # Two-group exact matching using the data set 'MAGMA_sim_data'
#' # Matching variable 'teacher_ability_rating' (ability rated from teacher as
#' # below average, average, or above average)
#' # MAGMA_sim_data_tar' contains the result of the matching
#' # Cores per default = 1
#' MAGMA_sim_data_tar <- MAGMA(Data = MAGMA_sim_data,
#'                             group = "teacher_ability_rating",
#'                             dist = "ps_tar")
#' head(MAGMA_sim_data_tar)
#'
#' # 2x2 matching using the data set 'MAGMA_sim_data'
#' # Matching variables are 'gifted_support' (received giftedness support yes
#' # or no) and 'enrichment' (participated in enrichment or not)
#' # 'MAGMA_sim_data_gift_enrich' contains the result of the matching
#' # 2x2 matching is equal to four-group matching
#' MAGMA_sim_data_gift_enrich <- MAGMA(Data = MAGMA_sim_data,
#'                                    group = c("gifted_support", "enrichment"),
#'                                    dist = "ps_2x2",
#'                                    cores = 2)
#' head(MAGMA_sim_data_gift_enrich)
#' }
#'
MAGMA <- function(Data, 
                  group, 
                  dist = NULL, 
                  cores = 1, 
                  verbose = TRUE, 
                  covs = NULL, 
                  weights = NULL,
                  large_method = "subgroup",
                  reference_group = NULL,
                  number_candidates = 10L,
                  combination_threshold = 1.0e+09) {

  #Check for regular input
  if(!is.data.frame(Data) && !tibble::is_tibble(Data)) {
    stop("Data needs to be an object of class dataframe or tibble!")
  }

  if(!is.character(group) | length(group) > 2) {
    stop("group needs to be a character of length 1 or a character vector of length 2!")
  }
  
  if(length(group) == 2) {
    if(length(table(Data[[group[1]]])) > 2 | length(table(Data[[group[2]]])) > 2 ) {
      stop("For two factor designs only 2 levels per factor are currently possible!")
    }
  }

  if((!is.character(dist) | length(dist) > 1) & !is.null(dist)) {
    stop("dist needs to be a character of length 1!")
  }
  
  if(!is.integer(cores) & !is.numeric(cores) | length(cores) > 1) {
    stop("cores needs to be a single integer number")
  }
  
  if(!is.null(covs) & !is.character(covs)) {
    stop("covs needs to be a character vector!")
  }
  
  if(is.null(dist) == is.null(covs)) {
    stop("Specify exactly one of dist or covs!")
  }
  
  if(!is.null(weights) & !is.numeric(weights)) {
    stop("weights needs to be a numeric vector!")
  }
  if(!is.null(weights) & length(weights) != length(covs)) {
    stop("covs and weights need to have the same length!")
  }
  if(!is.character(large_method) ||
     length(large_method) != 1L ||
     is.na(large_method) ||
     !large_method %in% c("subgroup", "reference")) {
    stop("large_method needs to be either subgroup or reference!")
  }

  if(!is.numeric(number_candidates) ||
     length(number_candidates) != 1L ||
     !is.finite(number_candidates) ||
     number_candidates < 1 ||
     number_candidates != floor(number_candidates)) {
    stop("number_candidates needs to be one positive integer!")
  }
  number_candidates <- as.integer(number_candidates)

  if(!is.numeric(combination_threshold) ||
     length(combination_threshold) != 1L ||
     !is.finite(combination_threshold) ||
     combination_threshold <= 0) {
    stop("combination_threshold needs to be one positive number!")
  }
  
  
  use_covariate_distance <- FALSE
  if(is.null(dist)) {
    Data$dist_dummy <- ifelse(rowSums(is.na(Data[, covs])) == 0, 1, NA)
    dist <- "dist_dummy"
    use_covariate_distance <- TRUE
  }

  max_cores <- parallel::detectCores()

  if(cores > max_cores) {
    warning("specified cores exceeds available cores. Proceeding with all available cores.")
    cores <- max_cores
  }


  #Creating data set with relevant variables
  if(length(group) == 1) {
    data <- Data %>%
      dplyr::filter(as.logical(!is.na(!!rlang::sym(group))),
                    as.logical(!is.na(!!rlang::sym(dist)))) 
    data$ID <- c(1:nrow(data))
  }  else {
    values_1 <- unlist(unique(Data[group[1]]))
    values_2 <- unlist(unique(Data[group[2]]))

    data <- Data %>%
      dplyr::filter(!is.na(!!rlang::sym(group[1])),
                    !is.na(!!rlang::sym(group[2])),
                    !is.na(!!rlang::sym(dist)))
    data$ID <- c(1:nrow(data))
    data$group_long <- ifelse(data[[group[1]]] == values_1[1] & data[[group[2]]] == values_2[1], 1,
                              ifelse(data[[group[1]]] == values_1[1] & data[[group[2]]] == values_2[2], 2,
                                     ifelse(data[[group[1]]] == values_1[2] & data[[group[2]]] == values_2[1], 3,
                                            ifelse(data[[group[1]]] == values_1[2] & data[[group[2]]] == values_2[2], 4, NA))))
  }


  #Checking if cases were excluded for missing data reasons
  if(nrow(data) != nrow(Data)) {
    warning("Some cases were excluded due to missing values for group or distance variable. Matching proceeds with reduced dataset.")
  }

  if(is.null(covs)) {
    if(length(group) == 1) {
      input <- data.frame(data["ID"],
                          data[group],
                          data[dist])
      colnames(input) <- c("ID", "group", "distance_ps")
    } else {
      input <- data.frame(data["ID"],
                          data["group_long"],
                          data[dist])
      colnames(input) <- c("ID", "group", "distance_ps")
    }
  } else {
    if(length(group) == 1) {
      group_indicator <- group
    } else {
      group_indicator <- "group_long"
    }
    input <- data.frame(data["ID"],
                        data[group_indicator],
                        data[, covs])
    colnames(input) <- c("ID", "group", covs)
  }
  
  input <- transform(input,
                     group_id = stats::ave(1:nrow(input),
                                           input$group,
                                           FUN = seq_along))
  
  input$distance <- input$step <- input$weight <- NA

if(verbose) {
  cat("\n","Input correctly identified.")
}

  #######################
  #distance estimation##
  ######################
  if(is.null(covs)) {
      var_ma <- as.numeric(stats::var(input$distance_ps))
      name_ps <- "distance_ps"
  } else {
    var_ma <- stats::cov(input[, covs])
    name_ps <- covs
  }


    group_list <- split.data.frame(input, input$group)

    elements <- split(input$group_id, input$group) %>%
      sapply(FUN = max)

    if(length(elements) == 2) {
      value_matrix <- build_value_matrix(group_list, elements, name_ps = name_ps)

      if(is.null(covs)) {
        means <- rowMeans(value_matrix)
        
        distance_matrix <- distance_estimator(data = value_matrix,
                                              means = means,
                                              variance = var_ma,
                                              cores = cores)
        
        distance_mean <- rowMeans(distance_matrix)
        
        rm(distance_matrix)
        rm(value_matrix)
        rm(means)
      } else {
        distance_mean <- distance_estimator_MD(data = value_matrix,
                                               variance = var_ma,
                                               cores = cores,
                                               rows = length(elements),
                                               weights = weights)
        rm(value_matrix)
      }

      distance_array <- array(data = distance_mean, dim = elements)

      rm(distance_mean)
      gc()
      
      if(verbose) {
      cat("\n", "Distance computation finished. Starting matching.")
      }

      group_list <- match_iterative(distance_array, group_list, elements)

      output_temp <- do.call(rbind.data.frame, group_list)
      output_temp <- output_temp[c("ID", "step", "weight", "distance")] 

      data <- merge(data,
                    output_temp,
                    by = "ID")
    } else if(length(elements) == 3) {

      if(large_method == "reference") {
        if(!is.null(covs)) {
          stop("The reference method currently supports propensity-score matching only.")
        }

        data_temp <- reference_matching(input = input,
                                        reference_group = reference_group,
                                        number_candidates = number_candidates,
                                        cores = cores,
                                        variance = var_ma)

        data <- merge(data,
                      data_temp,
                      by = "ID",
                      all.x = TRUE)
      } else if (prod(as.double(elements)) < combination_threshold) {
        value_matrix <- build_value_matrix(group_list,
                                           elements,
                                           name_ps = name_ps)

        if(is.null(covs)) {
          means <- rowMeans(value_matrix)
          
          distance_matrix <- distance_estimator(data = value_matrix,
                                                means = means,
                                                variance = var_ma,
                                                cores = cores)
          
          distance_mean <- rowMeans(distance_matrix)
          rm(distance_matrix)
          rm(value_matrix)
          rm(means)
        } else {
          distance_mean <- distance_estimator_MD(data = value_matrix,
                                                 variance = var_ma,
                                                 cores = cores,
                                                 rows = length(elements),
                                                 weights = weights)
          rm(value_matrix)
        }

        distance_array <- array(data = distance_mean, dim = elements)

        rm(distance_mean)
        gc()

        if(verbose) {
        cat("\n", "Distance computation finished. Starting matching")
        }

        group_list <- match_iterative(distance_array, group_list, elements)

        output_temp <- do.call(rbind.data.frame, group_list)
        output_temp <- output_temp[c("ID", "step", "weight", "distance")]

        data <- merge(data,
                      output_temp,
                      by = "ID",
                      all.x = TRUE)
      } else {
        cores <- 2
        if(verbose) {
        cat("\n", "Large number of groups with large group sizes. Computing quasi-systematic matching. Cores were reduced to 2 to simplify node communication despite high RAM usage.")
        }
        
        number_split_groups <- ceiling(sqrt(prod(as.double(elements)) /
                                                    combination_threshold)) + 1


        input$random_group = sample(c(1:number_split_groups),
                                    size = nrow(input),
                                    replace = TRUE) 
        
        random_list <- split.data.frame(input, input$random_group)
        
        random_list <- lapply(seq_along(random_list),
                              function(i) {
                                group_list_temp <- split(random_list[[i]], random_list[[i]]$group)
                                
                                elements_temp <- sapply(group_list_temp, nrow)
                                
                                value_matrix <- build_value_matrix(group_list_temp,
                                                                   elements_temp,
                                                                   name_ps = name_ps)
                                
                                if (is.null(covs)) {
                                  means <- rowMeans(value_matrix)
                                  
                                  distance_matrix <- distance_estimator(data = value_matrix,
                                                                        means = means,
                                                                        variance = var_ma,
                                                                        cores = cores)
                                  
                                  distance_mean <- rowMeans(distance_matrix)
                                  rm(distance_matrix)
                                  rm(means)
                                  
                                } else {
                                  
                                  distance_mean <- distance_estimator_MD(data = value_matrix,
                                                                         variance = var_ma,
                                                                         cores = cores,     
                                                                         rows = length(elements_temp),
                                                                         weights = weights)
                                }
                                rm(value_matrix)
                                
                                distance_array <- array(distance_mean, dim = elements_temp)
                                
                                rm(distance_mean)
                                
                                group_list_temp <- match_iterative(distance_array,
                                                                   group_list_temp,
                                                                   elements_temp)
                                rm(distance_array)
                                gc()
                                
                                do.call(rbind.data.frame, group_list_temp)
                              })

        
        data_temp <- do.call(rbind.data.frame, random_list)
        data_temp <- data_temp[order(data_temp$distance, data_temp$step),]
        data_temp$step <- ceiling(c(1:nrow(input))/3)
        data_temp <- data_temp[!is.na(data_temp$weight), c("ID", "step", "weight", "distance")] 
        
        data <- merge(data,
                      data_temp,
                      by = "ID",
                      all.x = TRUE)

      }
    } else if(length(elements) == 4) {

      if(large_method == "reference") {
        if(!is.null(covs)) {
          stop("The reference method currently supports propensity-score matching only.")
        }

        data_temp <- reference_matching(input = input,
                                        reference_group = reference_group,
                                        number_candidates = number_candidates,
                                        cores = cores,
                                        variance = var_ma)

        data <- merge(data,
                      data_temp,
                      by = "ID",
                      all.x = TRUE)
      } else if (prod(as.double(elements)) < combination_threshold) {
        value_matrix <- build_value_matrix(group_list,
                                           elements,
                                           name_ps = name_ps)

        if(is.null(covs)) {
          means <- rowMeans(value_matrix)
          
          distance_matrix <- distance_estimator(data = value_matrix,
                                                means = means,
                                                variance = var_ma,
                                                cores = cores)
          
          distance_mean <- rowMeans(distance_matrix)
          rm(distance_matrix)
          rm(means)
        } else {
          distance_mean <- distance_estimator_MD(data = value_matrix,
                                                 variance = var_ma,
                                                 cores = cores,
                                                 rows = length(elements),
                                                 weights = weights)
        }
        
        distance_array <- array(data = distance_mean, dim = elements)
        
        rm(distance_mean)
        rm(value_matrix)
        gc()
        if(verbose) {
        cat("\n", "Distance computation finished. Starting matching")
        }

        group_list <- match_iterative(distance_array, group_list, elements)

        output_temp <- do.call(rbind.data.frame, group_list)
        output_temp <- output_temp[c("ID", "step", "weight", "distance")]

        data <- merge(data,
                      output_temp,
                      by = "ID",
                      all.x = TRUE)
      } else {
          cores <- 2
          if(verbose) {
            cat("\n", "Large number of groups with large group sizes. Computing quasi-systematic matching. Cores were reduced to 2 to simplify node communication despite high RAM usage.")
          }
          
          
          number_split_groups <- ceiling(sqrt(prod(as.double(elements)) /
                                                      combination_threshold)) + 1
          
          
          input$random_group = sample(c(1:number_split_groups),
                                      size = nrow(input),
                                      replace = TRUE) 
          
          random_list <- split.data.frame(input, input$random_group)
          
          random_list <- lapply(seq_along(random_list),
                                function(i) {
                                  group_list_temp <- split(random_list[[i]], random_list[[i]]$group)
                                  
                                  elements_temp <- sapply(group_list_temp, nrow)
                                  
                                  value_matrix <- build_value_matrix(group_list_temp,
                                                                     elements_temp,
                                                                     name_ps = name_ps)
                                  
                                  if (is.null(covs)) {
                                    means <- rowMeans(value_matrix)
                                    
                                    distance_matrix <- distance_estimator(data = value_matrix,
                                                                          means = means,
                                                                          variance = var_ma,
                                                                          cores = cores)
                                    
                                    distance_mean <- rowMeans(distance_matrix)
                                    rm(distance_matrix)
                                    rm(means)
                                    
                                  } else {
                                    
                                    distance_mean <- distance_estimator_MD(data = value_matrix,
                                                                           variance = var_ma,
                                                                           cores = cores,     
                                                                           rows = length(elements_temp),
                                                                           weights = weights)
                                  }
                                  rm(value_matrix)
                                  
                                  distance_array <- array(distance_mean, dim = elements_temp)
                                  
                                  rm(distance_mean)
                                  
                                  group_list_temp <- match_iterative(distance_array,
                                                                     group_list_temp,
                                                                     elements_temp)
                                  rm(distance_array)
                                  gc()
                                  
                                  do.call(rbind.data.frame, group_list_temp)
                                })
          
          
          data_temp <- do.call(rbind.data.frame, random_list)
          data_temp <- data_temp[order(data_temp$distance, data_temp$step),]
          data_temp$step <- ceiling(c(1:nrow(input))/4)
          data_temp <- data_temp[!is.na(data_temp$weight), c("ID", "step", "weight", "distance")] 
          
          data <- merge(data,
                        data_temp,
                        by = "ID",
                        all.x = TRUE)
        }
    } else {
      stop("Specify a grouping variable that distinguishes 2, 3, or 4 groups or represent a 2x2 Design!")
    }
    if(verbose) {
    cat("\n", "Matching complete!")
    }
    if(use_covariate_distance) {
      data$dist_dummy <- NULL
    }
    return(data)
}
