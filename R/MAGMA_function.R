#' distance estimation
#'
#' estimates distance in MAGMA_3.
#'
#' This function is an inner function of MAGMA. It estimates the distance
#' of all possible matches.
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
#'
#'
distance_estimator <- function(data, means, variance, cores, inp = NULL) {
  cl <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl, cores = cores)
  distance_matrix <- foreach::foreach(inp = data, .combine = cbind) %dopar%
    stats::mahalanobis(inp, center = means, cov = variance)
  parallel::stopCluster(cl)
  return(distance_matrix)
}

#' MAGMA
#'
#' This function conducts many group matching for 2 to 4 groups. It augments
#' the original data set by the relevant MAGMA variables. For details, see
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
#' This function has some CPU and RAM load. In most four-group applications and
#' three-group applications with large sample size, RAM may be not sufficient.
#' Therefore MAGMA switches to random quasi-systematic matching. If this is the
#' case, MAGMA informs you. The output of the function does not change, but
#' balance might be slightly affected.
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
#' @param dist A character specifying the name of your distance
#' variable in data.
#' @param cores An integer defining the number of cores used for
#' parallel computation.
#'
#' @author Julian Urban
#'
#' @import tidyverse parallel doParallel foreach dplyr tibble tidyselect
#' @importFrom purrr set_names
#' @importFrom stats var
#' @importFrom stats runif
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
#' \dontrun{
#' # Running this code will take a while
#' # Two-group exact matching using the data set 'MAGMA_sim_data'
#' # Matching variable 'gifted_support' (received giftedness support yes or no)
#' # 'MAGMA_sim_data_gifted' contains the result of the matching
#' MAGMA_sim_data_gifted <- MAGMA(Data = MAGMA_sim_data,
#'                                 group = "gifted_support",
#'                                 dist = "ps_gifted",
#'                                 cores = 2)
#' head(MAGMA_sim_data_gifted)
#'
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
MAGMA <- function(Data, group, dist, cores = 1) {

  #Check for regular input
  if(!is.data.frame(Data) && !tibble::is_tibble(Data)) {
    stop("Data needs to be an object of class dataframe or tibble!")
  }

  if(!is.character(group) | length(group) > 2) {
    stop("group needs to be a character of length 1 or a character vector of length 2!")
  }

  if(!is.character(dist) | length(dist) > 2) {
    stop("dist needs to be a character of length 1 or 2!")
  }

  if(!is.integer(cores) & !is.numeric(cores) | length(cores) > 1) {
    stop("cores needs to be a single integer number")
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

    if(length(dist) == 1) {
      data <- Data %>%
        dplyr::filter(!is.na(!!rlang::sym(group[1])),
                      !is.na(!!rlang::sym(group[2])),
                      !is.na(!!rlang::sym(dist))) 
    } else {
      data <- Data %>%
        dplyr::filter(!is.na(!!rlang::sym(group[1])),
                      !is.na(!!rlang::sym(group[2])),
                      !is.na(!!rlang::sym(dist[1])),
                      !is.na(!!rlang::sym(dist[2])))
    }
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

  if(length(group) == 1) {
    input <- data.frame(data["ID"],
                        data[group],
                        data[dist])
    colnames(input) <- c("ID", "group", "distance_ps")
  } else {
    if(length(dist) == 1) {
      input <- data.frame(data["ID"],
                          data["group_long"],
                          data[dist])
      colnames(input) <- c("ID", "group", "distance_ps")
    } else {
      input <- data.frame(data["ID"],
                          data["group_long"],
                          data[dist[1]],
                          data[dist[2]])
      colnames(input) <- c("ID", "group", "distance_ps_1", "distance_ps_2")
    }
  }
  
  input <- transform(input,
                     group_id = stats::ave(1:nrow(input),
                                           input$group,
                                           FUN = seq_along))
  
  input$distance <- input$step <- input$weight <- NA


  cat("\n","Input correctly identified.")

  #######################
  #distance estimation##
  ######################
  if(length(dist) == 1) {
    var_ma <- as.numeric(stats::var(input$distance_ps))


    group_list <- split.data.frame(input, input$group)

    elements <- split(input$group_id, input$group) %>%
      sapply(FUN = max)

    if(length(elements) == 2) {
      value_matrix <- build_value_matrix(group_list, elements)

      means <- rowMeans(value_matrix)

      distance_matrix <- distance_estimator(data = value_matrix,
                                            means = means,
                                            variance = var_ma,
                                            cores = cores)

      distance_mean <- rowMeans(distance_matrix)

      distance_array <- array(data = distance_mean, dim = elements)

      rm(distance_mean)
      rm(distance_matrix)
      rm(value_matrix)
      rm(means)
      gc()

      cat("\n", "Distance computation finished. Starting matching.")

      group_list <- match_iterative(distance_array, group_list, elements)

      output_temp <- do.call(rbind.data.frame, group_list)
      output_temp <- output_temp[c("ID", "step", "weight", "distance")] 

      data <- merge(data,
                    output_temp,
                    by = "ID")
    } else if(length(elements) == 3) {

      if (prod(elements) < 1.0e+09) {
        value_matrix <- build_value_matrix(group_list, elements)

        means <- rowMeans(value_matrix)

        distance_matrix <- distance_estimator(data = value_matrix,
                                              means = means,
                                              variance = var_ma,
                                              cores = cores)

        distance_mean <- rowMeans(distance_matrix)

        distance_array <- array(data = distance_mean, dim = elements)

        rm(distance_mean)
        rm(distance_matrix)
        rm(value_matrix)
        rm(means)
        gc()


        cat("\n", "Distance computation finished. Starting matching")

        group_list <- match_iterative(distance_array, group_list, elements)

        output_temp <- do.call(rbind.data.frame, group_list)
        output_temp <- output_temp[c("ID", "step", "weight", "distance")]

        data <- merge(data,
                      output_temp,
                      by = "ID",
                      all.x = TRUE)
      } else {
        cores <- 2
        cat("\n", "Large number of groups with large group sizes. Computing quasi-systematic matching. Cores were reduced to 2 to simplify node communication despite high RAM usage.")

        number_split_groups <- ceiling(sqrt(prod(elements) / 1.0e+09)) + 1

        initial_seed <- .Random.seed

        set.seed(28062022)

        input$random_group = floor(stats::runif(nrow(input),
                                                1, number_split_groups))
        
        random_list <- split.data.frame(input, input$random_group)

        .Random.seed <- initial_seed

        for(i in 1:length(random_list)) {

          group_list_temp <- random_list[[i]] %>%
            split.data.frame(f = random_list[[i]]$group)

          elements_temp <- sapply(group_list_temp, nrow)

          value_matrix <- build_value_matrix(group_list_temp, elements_temp)

          means <- rowMeans(value_matrix)


          distance_matrix <- distance_estimator(data = value_matrix,
                                                means = means,
                                                variance = var_ma,
                                                cores = cores)
          rm(value_matrix)
          rm(means)
          gc()

          distance_mean <- rowMeans(distance_matrix)
          distance_array <- array(data = distance_mean, dim = elements_temp)

          rm(distance_matrix)
          rm(distance_mean)
          gc()

          group_list_temp <- match_iterative(distance_array, group_list_temp, elements_temp)

          rm(distance_array)
          gc()

          random_list[[i]] <- do.call(rbind.data.frame, group_list_temp)
        }
        data_temp <- do.call(rbind.data.frame, random_list)
        data_temp <- data_temp[order(data_temp$step, data_temp$distance),]
        data_temp$step <- ceiling(c(1:nrow(input))/3)
        data_temp <- data_temp[!is.na(data_temp$weight), c("ID", "step", "weight", "distance")] 
        
        data <- merge(data,
                      data_temp,
                      by = "ID",
                      all.x = TRUE)

      }
    } else if(length(elements) == 4) {

      if (prod(elements) < 1.0e+09) {

        value_matrix <- build_value_matrix(group_list, elements)

        means <- rowMeans(value_matrix)

        distance_matrix <- distance_estimator(data = value_matrix,
                                              means = means,
                                              variance = var_ma,
                                              cores = cores)

        distance_mean <- rowMeans(distance_matrix)

        distance_array <- array(data = distance_mean, dim = elements)

        rm(distance_mean)
        rm(distance_matrix)
        rm(value_matrix)
        rm(means)
        gc()


        cat("\n", "Distance computation finished. Starting matching")

        group_list <- match_iterative(distance_array, group_list, elements)

        output_temp <- do.call(rbind.data.frame, group_list)
        output_temp <- output_temp[c("ID", "step", "weight", "distance")]

        data <- merge(data,
                      output_temp,
                      by = "ID",
                      all.x = TRUE)
      } else {
        cores <- 2
        cat("\n", "Large number of groups with large group sizes. Computing quasi-systematic matching. Cores were reduced to 2 to simplify node communication despite high RAM usage.")

        number_split_groups <- ceiling(sqrt(prod(elements) / 1.0e+09)) + 1
        if(number_split_groups == 1) {
          number_split_groups <- 2
        }

        initial_seed <- .Random.seed

        set.seed(28062022)

        input$random_group = floor(stats::runif(nrow(input),
                                                1, number_split_groups))
        random_list <- split.data.frame(input, input$random_group)

        .Random.seed <- initial_seed

        for(i in 1:length(random_list)) {

          group_list_temp <- random_list[[i]] %>%
            split.data.frame(f = random_list[[i]]$group)

          elements_temp <- sapply(group_list_temp, nrow)

          value_matrix <- build_value_matrix(group_list_temp, elements_temp)

          means <- rowMeans(value_matrix)


          distance_matrix <- distance_estimator(data = value_matrix,
                                                means = means,
                                                variance = var_ma,
                                                cores = cores)
          rm(value_matrix)
          rm(means)
          gc()

          distance_mean <- rowMeans(distance_matrix)
          distance_array <- array(data = distance_mean, dim = elements_temp)

          rm(distance_matrix)
          rm(distance_mean)
          gc()

          group_list_temp <- match_iterative(distance_array, group_list_temp, elements_temp)

          rm(distance_array)
          gc()

          random_list[[i]] <- do.call(rbind.data.frame, group_list_temp)
        }
        data_temp <- do.call(rbind.data.frame, random_list)
        data_temp <- data_temp[order(data_temp$step, data_temp$distance),]
        data_temp$step <- ceiling(c(1:nrow(input))/4)
        data_temp <- data_temp[!is.na(data_temp$weight), c("ID", "step", "weight", "distance")] 
        
        data <- merge(data,
                      data_temp,
                      by = "ID",
                      all.x = T)
      }
    } else {
      stop("Specify a grouping variable that distinguishes 2, 3, or 4 groups or represent a 2x2 Design!")
    }

    cat("\n", "Matching complete!")
    return(data)
  } else {
    var_ma <- input[c("distance_ps_1", "distance_ps_2")] %>%
      sapply(FUN = stats::var)


    group_list <- split.data.frame(input, input$group)

    elements <- split(input$group_id, input$group) %>%
      sapply(FUN = max)

    if (prod(elements) < 1.0e+09) {

      value_matrix <- build_value_matrix(group_list, elements)

      means <- rowMeans(value_matrix)

      distance_matrix_1 <- distance_estimator(data = value_matrix,
                                              means = means,
                                              variance = var_ma[1],
                                              cores = cores)

      value_matrix <- build_value_matrix(group_list, elements)

      means <- rowMeans(value_matrix)

      distance_matrix_2 <- distance_estimator(data = value_matrix,
                                              means = means,
                                              variance = var_ma[2],
                                              cores = cores)
      rm(means)
      rm(value_matrix)
      distance_array <- array(data = (distance_matrix_1 +
                                        distance_matrix_2 +
                                        distance_matrix_1 * distance_matrix_2),
                              dim = elements)
      rm(distance_matrix)
      rm(distance_matrix_2)
      gc()

      group_list <- match_iterative(distance_array, group_list, elements)

      rm(distance_array)
      gc()

      output_temp <- do.call(rbind.data.frame, group_list)
      output_temp <- output_temp[c("ID", "step", "weight", "distance")] 

      data <- merge(data,
                    output_temp,
                    by = "ID",
                    all.x = TRUE)
      cat("\n", "matching complete!")
      return(data)

    } else {
      cores <- 2
      cat("\n", "Large number of groups with large group sizes. Computing quasi-systematic matching. Cores were reduced to 2 to simplify node communication despite high RAM usage.")

      number_split_groups <- ceiling(sqrt(prod(elements) / 1.0e+09)) + 1
      if(number_split_groups == 1) {
        number_split_groups <- 2
      }

      initial_seed <- .Random.seed

      set.seed(28062022)

      input <- input$random_group <- floor(stats::runif(nrow(input),
                                                        1, number_split_groups))
      random_list <- split.data.frame(input, input$random_group)

      .Random.seed <- initial_seed

      for(i in 1:length(random_list)) {

        group_list_temp <- random_list[[i]] %>%
          split.data.frame(f = random_list[[i]]$group)

        elements_temp <- sapply(group_list_temp, nrow)

        value_matrix <- build_value_matrix(group_list_temp,
                                           elements_temp,
                                           name_ps = "distance_ps_1")

        means <- rowMeans(value_matrix)


        distance_matrix_1 <- distance_estimator(data = value_matrix,
                                                means = means,
                                                variance = var_ma[1],
                                                cores = cores)

        value_matrix <- build_value_matrix(group_list_temp,
                                           elements_temp,
                                           name_ps = "distance_ps_2")

        means <- rowMeans(value_matrix)

        distance_matrix_2 <- distance_estimator(data = value_matrix,
                                                means = means,
                                                variance = var_ma[2],
                                                cores = cores)
        rm(means)
        rm(value_matrix)
        distance_array <- array(data = ((distance_matrix_1 +
                                           distance_matrix_2 +
                                           distance_matrix_1 * distance_matrix_2)),
                                dim = elements_temp)
        rm(distance_matrix_1)
        rm(distance_matrix_2)
        gc()

        group_list_temp <- match_iterative(distance_array, group_list_temp, elements_temp)

        rm(distance_array)
        gc()

        random_list[[i]] <- do.call(rbind.data.frame, group_list_temp)
      }
      data_temp <- do.call(rbind.data.frame, random_list)
      data_temp <- data_temp[order(data_temp$step, data_temp$distance),]
      data_temp$step <- ceiling(c(1:nrow(input))/4)
      data_temp <- data_temp[!is.na(data_temp$weight), c("ID", "step", "weight", "distance")] 
      

      data <- merge(data,
                    data_temp,
                    by = "ID",
                    all.x = TRUE)
      cat("\n", "Matching complete!")
      return(data)
    }
  }
}
