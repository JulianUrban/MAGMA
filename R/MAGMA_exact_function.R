#' MAGMA exact
#'
#' This function conducts exact many group matching for 2 to 4 groups. Exact
#' means, that only cases with the same value on the exact variable can be
#' matched. It augments the original data set by relevant MAGMA variables.
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
#' same school, or the same organization are possible matches. For standard
#' matching, see \code{\link{MAGMA}}
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
#' @import tidyverse parallel doParallel foreach tibble dplyr tidyselect stats
#' @return Your input data frame of valid cases augmented with matching
#' relevant variables, namely *weight*, *step*, *distance*, and *ID*. In case
#' of missing values on the distance or group variable, MAGMA excludes them for
#' the matching process. The returned data set does not contain those excluded
#' cases. For more information, see Details.
#' @export
#'
#' @examples{
#' \dontrun{
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
#' }
#' }
#'
MAGMA_exact <- function(Data, group, dist, exact, cores = 1) {

  #Check for regular input
  if(!is.data.frame(Data) && !tibble::is_tibble(Data)) {
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
      dplyr::filter(as.logical(!is.na(!!sym(group))),
                    as.logical(!is.na(!!sym(dist)))) %>%
      dplyr::mutate(ID = row_number())
  }  else {
    values_1 <- unlist(unique(Data[group[1]]))
    values_2 <- unlist(unique(Data[group[2]]))

    if(length(dist) == 1) {
      data <- Data %>%
        dplyr::filter(!is.na(!!sym(group[1])),
                      !is.na(!!sym(group[2])),
                      !is.na(!!sym(dist))) %>%
        dplyr::mutate(ID = row_number(),
                      group_long = dplyr::case_when(
                        !!sym(group[1]) == values_1[1] &
                          !!sym(group[2]) == values_2[1] ~ 1,
                        !!sym(group[1]) == values_1[1] &
                          !!sym(group[2]) == values_2[2] ~ 2,
                        !!sym(group[1]) == values_1[2] &
                          !!sym(group[2]) == values_2[1] ~ 3,
                        !!sym(group[1]) == values_1[2] &
                          !!sym(group[2]) == values_2[2] ~ 4
                      ))
    } else {
      data <- Data %>%
        dplyr::filter(!is.na(!!sym(group[1])),
                      !is.na(!!sym(group[2])),
                      !is.na(!!sym(dist[1])),
                      !is.na(!!sym(dist[2]))) %>%
        dplyr::mutate(ID = row_number(),
                      group_long = dplyr::case_when(
                        !!sym(group[1]) == values_1[1] &
                          !!sym(group[2]) == values_2[1] ~ 1,
                        !!sym(group[1]) == values_1[1] &
                          !!sym(group[2]) == values_2[2] ~ 2,
                        !!sym(group[1]) == values_1[2] &
                          !!sym(group[2]) == values_2[1] ~ 3,
                        !!sym(group[1]) == values_1[2] &
                          !!sym(group[2]) == values_2[2] ~ 4
                      ))
    }
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
    tibble::as_tibble() %>%
    dplyr::group_by(group) %>%
    dplyr::mutate(group_id = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(weight = NA,
           step = NA,
           distance = NA)

  cat("input correctly identified")

  #######################
  #distance estimation##
  ######################
  var_ma <- as.numeric(stats::var(input$distance_ps))

  elements <- input %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(max(group_id)) %>%
    `[`(, 2) %>%
    unlist()

  if(length(elements) == 2) {

    exact_list <- split.data.frame(input, input$exact)

    for(i in 1:length(exact_list)) {

    group_list_temp <- exact_list[[i]] %>%
      split.data.frame(f = exact_list[[i]]$group)

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

    exact_list[[i]] <- do.call(rbind.data.frame, group_list_temp)
    }

  data_temp <- do.call(rbind.data.frame, exact_list) %>%
    dplyr::arrange(distance, step) %>%
    dplyr::mutate(step = ceiling(c(1:nrow(input))/2)) %>%
    dplyr::select(ID, step, weight, distance) %>%
    dplyr::filter(weight == 1)

  data <- merge(data,
                data_temp,
                by = "ID",
                all.x = TRUE)

  } else if(length(elements) == 3) {

      exact_list <- split.data.frame(input, input$exact)

      for(i in 1:length(exact_list)) {

        group_list_temp <- exact_list[[i]] %>%
          split.data.frame(f = exact_list[[i]]$group)

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
        exact_list[[i]] <- do.call(rbind.data.frame, group_list_temp)
      }
      data_temp <- do.call(rbind.data.frame, exact_list) %>%
        dplyr::arrange(distance, step) %>%
        dplyr::mutate(step = ceiling(c(1:nrow(input))/3)) %>%
        dplyr::select(ID, step, weight, distance) %>%
        dplyr::filter(weight == 1)

      data <- merge(data,
                    data_temp,
                    by = "ID",
                    all.x = TRUE)

  } else if(length(elements) == 4) {


      exact_list <- split.data.frame(input, input$exact)

      for(i in 1:length(exact_list)) {

        group_list_temp <- exact_list[[i]] %>%
          split.data.frame(f = exact_list[[i]]$group)

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
        exact_list[[i]] <- do.call(rbind.data.frame, group_list_temp)
      }
      data_temp <- do.call(rbind.data.frame, exact_list) %>%
        dplyr::arrange(distance, step) %>%
        dplyr::mutate(step = ceiling(c(1:nrow(input))/4)) %>%
        dplyr::select(ID, step, weight, distance) %>%
        dplyr::filter(weight == 1)

      data <- merge(data,
                    data_temp,
                    by = "ID")
  } else {
    stop("Specify a grouping variable that distinguishes 2, 3, or 4 groups or represent a 2x2 Design!")
  }

  cat("\n", "matching complete!")
  return(data)
}
