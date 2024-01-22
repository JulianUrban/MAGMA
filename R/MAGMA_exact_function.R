#' MAGMA exact
#'
#' This function conducts exact many group matching for 2 to 4 groups. Exact
#' means that only cases with the same value on the exact variable can be
#' matched. It augments the original data set by relevant MAGMA variables.
#' For details, see below.
#'
#' This function conducts nearest neighbor exact many group matching. It is
#' applicable for two to four groups or a 2x2 design. As output, this function
#' augments your original data by the variables *weight*, *step*, *distance*,
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
#' your grouping variable in the data. Note that MAGMA can only match your data
#' for a maximum of 4 groups. Matching over two grouping variables (e.g., 2x2
#' Design) is possible by specifying group as a character vector with a length
#' of two. In this case, each or the 2 grouping variables can only have two
#' levels.
#' @param dist A character specifying the name of your distance
#' variable in data.
#' @param exact A character specifying the name of the exact variable.
#' Only cases with the same value on this variable can be matched.
#' @param cores An integer defining the number of cores used for
#' parallel computation.
#'
#' @author Julian Urban
#'
#' @import tidyverse parallel doParallel foreach dplyr tibble tidyselect
#' @importFrom stats var
#' @importFrom stats ave
#' @importFrom rlang sym
#' 
#' @return Your input data frame of valid cases augmented with matching
#' relevant variables, namely *weight*, *step*, *distance*, and *ID*. In case
#' of missing values on the distance or group variable, MAGMA_exact excludes
#' them for the matching process. The returned data set does not contain those 
#' excluded cases. For more information, see Details.
#' @export
#'
#' @examples
#' \dontrun{
#' # Running this code will take a while
#' # Two-group exact matching using the data set 'MAGMA_sim_data'
#' # Matching variable 'gifted_support' (received giftedness support yes or no)
#' # 'MAGMA_sim_data_gifted_exact' contains the result of the matching
#' # Exact matching for 'enrichment' (participated in enrichment or not)
#' # Students that participated can only be matched with other
#' # students that participated and vice versa
#' MAGMA_sim_data_gifted_exact <- MAGMA_exact(Data = MAGMA_sim_data,
#'                                            group = "gifted_support",
#'                                            dist = "ps_gifted",
#'                                            exact = "enrichment",
#'                                            cores = 2)
#' head(MAGMA_sim_data_gifted_exact)
#'
#' # Conducting three-group matching using the data set 'MAGMA_sim_data'
#' # Matching variable 'teacher_ability_rating' (ability rated from teacher as
#' # below average, average, or above average)
#' # 'MAGMA_sim_data_tar_exact' contains the result of the matching
#' # Exact matching for gender (male or female)
#' # Male students can only be matched to male students, female students can only
#' # be matched to female students
#' # Cores per default = 1
#' MAGMA_sim_data_tar_exact<- MAGMA_exact(Data = MAGMA_sim_data,
#'                                        group = "teacher_ability_rating",
#'                                        dist = "ps_tar",
#'                                        exact = "gender")
#' head(MAGMA_sim_data_tar_exact)
#'
#' # 2x2 matching using the data set 'MAGMA_sim_data'
#' # Matching variables are 'gifted_support' (received giftedness support yes
#' # or no) and 'enrichment' (participated in enrichment or not)
#' # 'MAGMA_sim_data_gift_enrich_exact' contains the result of the matching
#' # 2x2 matching is equal to four-group matching
#' # Exact matching for for teacher rated ability (ability rated from teacher as
#' # below average, average, or above average)
#' # Below average students can only be matched to other below average rated
#' # students, average rated students can be matched with other average rated
#' # students, and above average rated students can only be matched to other
#' # above average rated students
#' MAGMA_sim_data_gift_enrich_exact <- MAGMA_exact(Data = MAGMA_sim_data,
#'                                                 group = c("gifted_support", "enrichment"),
#'                                                 dist = "ps_2x2",
#'                                                 exact = "teacher_ability_rating",
#'                                                 cores = 2)
#' head(MAGMA_sim_data_gift_enrich_exact)
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
  
  if(length(group) == 2) {
    if(length(table(Data[[group[1]]])) > 2 | length(table(Data[[group[2]]])) > 2 ) {
      stop("For two factor designs only 2 levels per factor are currently possible!")
    }
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
    warning("Specified cores exceeds available cores. Proceeding with all available cores.")
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
  
  input <- transform(input,
                     group_id = stats::ave(1:nrow(input),
                                           input$group,
                                           FUN = seq_along))
  
  input$distance <- input$step <- input$weight <- NA

  cat("\n","Input correctly identified!")

  #######################
  #distance estimation##
  ######################
  var_ma <- as.numeric(stats::var(input$distance_ps))

  elements <- split(input$group_id, input$group) %>%
    sapply(FUN = max)


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

    if (i == 1) {
      cat("\n", "Distance computation finished. Starting matching")
    }

    group_list_temp <- match_iterative(distance_array, group_list_temp, elements_temp)
    rm(distance_array)
    gc()

    exact_list[[i]] <- do.call(rbind.data.frame, group_list_temp)
    }

  data_temp <- do.call(rbind.data.frame, exact_list)
  data_temp <- data_temp[order(data_temp$distance, data_temp$step),]
  data_temp$step <- ceiling(c(1:nrow(input))/2)
  data_temp <- data_temp[!is.na(data_temp$weight), c("ID", "step", "weight", "distance")] 

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

        if (i == 1) {
          cat("\n", "Distance computation finished. Starting matching")
        }

        group_list_temp <- match_iterative(distance_array, group_list_temp, elements_temp)
        rm(distance_array)
        gc()
        exact_list[[i]] <- do.call(rbind.data.frame, group_list_temp)
      }
      data_temp <- do.call(rbind.data.frame, exact_list)
      data_temp <- data_temp[order(data_temp$distance, data_temp$step),]
      data_temp$step <- ceiling(c(1:nrow(input))/3)
      data_temp <- data_temp[!is.na(data_temp$weight), c("ID", "step", "weight", "distance")] 

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

        if (i == 1) {
          cat("\n", "Distance computation finished. Starting matching")
        }

        group_list_temp <- match_iterative(distance_array, group_list_temp, elements_temp)
        rm(distance_array)
        gc()
        exact_list[[i]] <- do.call(rbind.data.frame, group_list_temp)
      }
      data_temp <- do.call(rbind.data.frame, exact_list) 
      data_temp <- data_temp[order(data_temp$distance, data_temp$step),]
      data_temp$step <- ceiling(c(1:nrow(input))/4)
      data_temp <- data_temp[!is.na(data_temp$weight), c("ID", "step", "weight", "distance")] 

      data <- merge(data,
                    data_temp,
                    by = "ID")
  } else {
    stop("Specify a grouping variable that distinguishes 2, 3, or 4 groups or represent a 2x2 Design!")
  }

  cat("\n", "Matching complete!")
  return(data)
}
