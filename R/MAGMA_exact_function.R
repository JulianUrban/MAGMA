#' MAGMA exact
#'
#' This function conducts exact many group matching for 2 to 4 groups. Exact
#' means, that only cases with the same value on the exact variable can be
#' matched. It #' augments the original data set by relevant MAGMA variables.
#' For details, see below.
#'
#' This function conducts nearest neighbor exact many group matching. It is
#' applicable for 2 to 4 groups or a 2x2 Design. As output, this function
#' augment your original data by the variables *weight*, *step*, *distance*,
#' and *ID*. Weight indicates whether a case was matched. Step specifies the
#' iteration in which a case was matched. It also shows which cases were
#' matched together. Distance indicates the mean difference within a match.
#' Since matches with a lower distance are matched in an earlier iteration,
#' step and distance are strongly correlated.
#' Exact matching means that only cases with the same value on the exact
#' variable can be matched. As example, only person of the same gender, the
#' same school, or the same organization are possible matches.
#'
#'
#' @param Data A data frame or tibble containing at least your grouping and
#' distance variable. Data needs to be specified in your environment.
#' @param group A character specifying the name of
#' your grouping variable in data. Note that MAGMA can only match your data for
#' a maximum of 4 groups. For matching over two groups (e.g., 2x2 Design) is
#' possible by specifying group as a character vector with a length of two. In
#' this case each or the 2 grouping variables can only have two levels.
#' @param dist A character specifying the name of your distance
#' variable in data.
#' @param exact A character specifying the name of the exact variable.
#' Only cases with the same value on this variable can be matched.
#' @param cores An integer defining the number of cores used for
#' parallel computation.
#'
#' @author Julian Urban
#'
#' @import tidyverse parallel doParallel foreach
#' @return Your input data frame of valid cases augmented with matching
#' relevant variables, namely *weight*, *step*, *distance*, and *ID*. In case
#' of missing values on the distance or group variable, MAGMA excludes them for
#' th matching process. The returned data set does not contain those excluded
#' cases. For more information, see Details.
#' @export
#'
#' @examples
#' #Running this code will take a while
#' #Computing two-group Matching for giftedness support with exact for enrichment
#' MAGMA_sim_data_gifted_exact <- MAGMA_exact(Data = MAGMA_sim_data,
#'                                            group = "gifted_support",
#'                                            dist = "ps_gifted",
#'                                            exact = "enrichment",
#'                                            cores = 2)
#' head(MAGMA_sim_data_gifted_exact)
#'
#' #Conducting three-group matching for teacher ability rating exact for
#' #sex. Cores per default = 1
#' MAGMA_sim_data_tar_exact<- MAGMA_exact(Data = MAGMA_sim_data,
#'                                        group = "teacher_ability_rating",
#'                                        dist = "ps_tar",
#'                                        exact = "sex")
#' head(MAGMA_sim_data_tar_exact)
#'
#' #Computing 2x2 Matching for giftedness support and enrichment equivalent to
#' #a four group matching with exact MAGMA_exact for teacher rated ability
#' MAGMA_sim_data_gift_enrich_exact <- MAGMA_exact(Data = MAGMA_sim_data,
#'                                                 group = c("gifted_support", "enrichment"),
#'                                                 dist = "ps_2x2",
#'                                                 exact = "teacher_ability_rating",
#'                                                 cores = 2)
#' head(MAGMA_sim_data_gift_enrich_exact)
#'
MAGMA_exact <- function(Data, group, dist, exact, cores = 1) {

  #Check for regular input
  if(!is.data.frame(Data) && !is_tibble(Data)) {
    stop("Data needs to be an object of class dataframe or tibble!")
  }

  if(!is.character(group) | length(group) > 2) {
    stop("group needs to be a character of length 1!")
  }

  if(!is.character(exact) | length(exact) > 1) {
    stop("exact needs to be a character of length 1!")
  }

  if(!is.character(dist) | length(dist) > 1) {
    stop("dist needs to be a character of length 1!")
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
      filter(as.logical(!is.na(.[group])),
             as.logical(!is.na(.[dist]))) %>%
      mutate( ID = c(1:nrow(Data)))
  } else {
    values_1 <- unlist(unique(Data[group[1]]))
    values_2 <- unlist(unique(Data[group[2]]))
    data <- Data %>%
      filter(as.logical(!is.na(.[group[1]])),
             as.logical(!is.na(.[group[2]])),
             as.logical(!is.na(.[dist]))) %>%
      mutate(ID = c(1:nrow(Data)),
             group_long = case_when(
               as.logical(Data[group[1]] == values_1[1] & Data[group[2]] == values_2[1]) ~ 1,
               as.logical(Data[group[1]] == values_1[1] & Data[group[2]] == values_2[2]) ~ 2,
               as.logical(Data[group[2]] == values_1[2] & Data[group[2]] == values_2[1]) ~ 3,
               as.logical(Data[group[1]] == values_1[2] & Data[group[2]] == values_2[2]) ~ 4
             ))
  }


  #Checking if cases were excluded for missing data reasons
  if(nrow(data) != nrow(Data)) {
    warning("Some cases were excluded due to missing values for group or distance variable. Matching proceeds with reduced dataset.")
  }

  if(length(group) == 1) {
    input <- data.frame(ID = data["ID"],
                        group = data[group],
                        distance = data[dist],
                        exact = data[exact])
  } else {
    input <- data.frame(ID = data["ID"],
                        group = data["group_long"],
                        distance = data[dist],
                        exact = data[exact])
  }

  colnames(input) <- c("ID", "group", "distance_ps","exact")
  input <- input %>%
    as_tibble(.) %>%
    group_by(group) %>%
    mutate(group_id = row_number()) %>%
    ungroup() %>%
    mutate(weight = NA,
           step = NA,
           distance = NA)

  cat("input correctly identified")

  #######################
  #distance estimation##
  ######################
  var_ma <- as.numeric(var(input$distance_ps))

  elements <- input %>%
    group_by(group) %>%
    summarise(max(group_id)) %>%
    .[, 2] %>%
    unlist()

  if(length(elements) == 2) {

    exact_list <- split.data.frame(input, input$exact)

    for(i in 1:length(exact_list)) {

    group_list_temp <- exact_list[[i]] %>%
      split.data.frame(., .$group)

    elements_temp <- sapply(group_list_temp, nrow)

    value_matrix <- matrix(NA, nrow = prod(elements_temp), length(elements_temp))

    value_matrix[, 1] <- rep(group_list_temp[[1]][["distance_ps"]], elements_temp[2])
    value_matrix[, 2] <- purrr::map(group_list_temp[[2]][["distance_ps"]], rep, elements_temp[1]) %>%
      unlist()

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

    iteration <- 1
    iteration_max <- min(elements_temp)
    while (iteration <= iteration_max) {
      index <- which(distance_array == min(distance_array, na.rm = T), arr.ind=T)[1, ] # Indizierung notwenig, wenn zufällig selbe Distanz zweimal vorhanden

      group_list_temp[[1]][["step"]][[index[1]]] <- iteration
      group_list_temp[[2]][["step"]][[index[2]]] <- iteration
      group_list_temp[[1]][["weight"]][[index[1]]] <- 1
      group_list_temp[[2]][["weight"]][[index[2]]] <- 1
      group_list_temp[[1]][["distance"]][[index[1]]] <- min(distance_array, na.rm = T)
      group_list_temp[[2]][["distance"]][[index[2]]] <- min(distance_array, na.rm = T)


      distance_array[index[1], ] <- NA
      distance_array[, index[2]] <- NA

      iteration <- iteration + 1
    }
    rm(distance_array)
    gc()
    exact_list[[i]] <- do.call(rbind.data.frame, group_list_temp)
    }

  data_temp <- do.call(rbind.data.frame, exact_list) %>%
    arrange(distance, step) %>%
    mutate(step = ceiling(c(1:nrow(.))/2)) %>%
    select(ID, step, weight, distance) %>%
    filter(weight == 1)

  data <- merge(data,
                data_temp,
                by = "ID",
                all.x = TRUE)

  } else if(length(elements) == 3) {

      exact_list <- split.data.frame(input, input$exact)

      for(i in 1:length(exact_list)) {

        group_list_temp <- exact_list[[i]] %>%
          split.data.frame(., .$group)

        elements_temp <- sapply(group_list_temp, nrow)

        value_matrix <- matrix(NA, nrow = prod(elements_temp), length(elements_temp))

        value_matrix[, 1] <- rep(group_list_temp[[1]][["distance_ps"]], elements_temp[2] * elements_temp[3])
        value_matrix[, 2] <- purrr::map(group_list_temp[[2]][["distance_ps"]], rep, elements_temp[1]) %>%
          unlist() %>%
          rep(., elements_temp[3])
        value_matrix[, 3] <- purrr::map(group_list_temp[[3]][["distance_ps"]], rep, elements_temp[1] * elements_temp[2]) %>%
          unlist()

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

        iteration <- 1
        iteration_max <- min(elements_temp)
        while (iteration <= iteration_max) {
          index <- which(distance_array == min(distance_array, na.rm = T), arr.ind=T)[1, ] # Indizierung notwenig, wenn zufällig selbe Distanz zweimal vorhanden

          group_list_temp[[1]][["step"]][[index[1]]] <- iteration
          group_list_temp[[2]][["step"]][[index[2]]] <- iteration
          group_list_temp[[3]][["step"]][[index[3]]] <- iteration
          group_list_temp[[1]][["weight"]][[index[1]]] <- 1
          group_list_temp[[2]][["weight"]][[index[2]]] <- 1
          group_list_temp[[3]][["weight"]][[index[3]]] <- 1
          group_list_temp[[1]][["distance"]][[index[1]]] <- min(distance_array, na.rm = T)
          group_list_temp[[2]][["distance"]][[index[2]]] <- min(distance_array, na.rm = T)
          group_list_temp[[3]][["distance"]][[index[3]]] <- min(distance_array, na.rm = T)


          distance_array[index[1], , ] <- NA
          distance_array[, index[2], ] <- NA
          distance_array[, , index[3]] <- NA

          iteration <- iteration + 1
        }
        rm(distance_array)
        gc()
        exact_list[[i]] <- do.call(rbind.data.frame, group_list_temp)
      }
      data_temp <- do.call(rbind.data.frame, exact_list) %>%
        arrange(distance, step) %>%
        mutate(step = ceiling(c(1:nrow(.))/3)) %>%
        select(ID, step, weight, distance) %>%
        filter(weight == 1)

      data <- merge(data,
                    data_temp,
                    by = "ID")

  } else if(length(elements) == 4) {


      exact_list <- split.data.frame(input, input$exact)

      for(i in 1:length(exact_list)) {

        group_list_temp <- exact_list[[i]] %>%
          split.data.frame(., .$group)

        elements_temp <- sapply(group_list_temp, nrow)

        value_matrix <- matrix(NA, nrow = prod(elements_temp), length(elements_temp))

        value_matrix[, 1] <- rep(group_list_temp[[1]][["distance_ps"]], elements_temp[2] * elements_temp[3] * elements_temp[4])
        value_matrix[, 2] <- purrr::map(group_list_temp[[2]][["distance_ps"]], rep, elements_temp[1]) %>%
          unlist() %>%
          rep(., elements_temp[3] * elements_temp[4])
        value_matrix[, 3] <- purrr::map(group_list_temp[[3]][["distance_ps"]], rep, elements_temp[1] * elements_temp[2]) %>%
          unlist() %>%
          rep(., elements_temp[4])
        value_matrix[, 4] <- purrr::map(group_list_temp[[4]][["distance_ps"]], rep, elements_temp[1] * elements_temp[2] * elements_temp[3]) %>%
          unlist()

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

        iteration <- 1
        iteration_max <- min(elements_temp)
        while (iteration <= iteration_max) {
          index <- which(distance_array == min(distance_array, na.rm = T), arr.ind=T)[1, ]

          group_list_temp[[1]][["step"]][[index[1]]] <- iteration
          group_list_temp[[2]][["step"]][[index[2]]] <- iteration
          group_list_temp[[3]][["step"]][[index[3]]] <- iteration
          group_list_temp[[4]][["step"]][[index[4]]] <- iteration
          group_list_temp[[1]][["weight"]][[index[1]]] <- 1
          group_list_temp[[2]][["weight"]][[index[2]]] <- 1
          group_list_temp[[3]][["weight"]][[index[3]]] <- 1
          group_list_temp[[4]][["weight"]][[index[4]]] <- 1
          group_list_temp[[1]][["distance"]][[index[1]]] <- min(distance_array, na.rm = T)
          group_list_temp[[2]][["distance"]][[index[2]]] <- min(distance_array, na.rm = T)
          group_list_temp[[3]][["distance"]][[index[3]]] <- min(distance_array, na.rm = T)
          group_list_temp[[4]][["distance"]][[index[4]]] <- min(distance_array, na.rm = T)


          distance_array[index[1], , , ] <- NA
          distance_array[, index[2], , ] <- NA
          distance_array[, , index[3], ] <- NA
          distance_array[, , , index[4]] <- NA

          iteration <- iteration + 1
        }
        rm(distance_array)
        gc()
        exact_list[[i]] <- do.call(rbind.data.frame, group_list_temp)
      }
      data_temp <- do.call(rbind.data.frame, exact_list) %>%
        arrange(distance, step) %>%
        mutate(step = ceiling(c(1:nrow(.))/4)) %>%
        select(ID, step, weight, distance) %>%
        filter(weight == 1)

      data <- merge(data,
                    data_temp,
                    by = "ID")
  } else {
    stop("Specify a grouping variable that distinguishes 2, 3, or 4 groups or represent a 2x2 Design!")
  }

  cat("\n", "matching complete!")
  return(data)
}
