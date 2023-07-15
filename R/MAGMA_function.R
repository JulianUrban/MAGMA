#' distance estimation
#'
#' estimates distance in MAGMA_3.
#'
#' This function is an inner function of MAGMA_3. It estimates the distance
#' of all possible matches.
#'
#' @param data A matrix containing all possible combinations.
#' @param means A matrix containg all row means of all possible matches.
#' @param variance A numeric indicating the variance of the propensity scores.
#' @param cores An integer defining the number of cores used for
#' parallel computation.
#'
#' @author Julian Urban
#'
#' @import tidyverse parallel doParallel foreach
#' @return A matrix of distance for each case of each possible match.
#'
#'
distance_estimator <- function(data, means, variance, cores) {
  cl <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl, cores = cores)
  distance_matrix <- foreach(inp = data, .combine = cbind) %dopar%
    stats::mahalanobis(inp, means, variance)
  parallel::stopCluster(cl)
  return(distance_matrix)
}


#' MAGMA
#'
#' This function conducts many group matching for 2 to 4 groups. It augments
#' the original data set by relevant MAGMA variables. For details, see below.
#'
#' This function conducts nearest neighbor many group matching. It is
#' applicable for 2 to 4 groups or a 2x2 Design. As output, this function
#' augment your original data by the variables *weight*, *step*, *distance*,
#' and *ID*. Weight indicates whether a case was matched. Step specifies the
#' iteration in which a case was matched. It also shows which cases were matched
#' together. Distance indicates the mean difference within a match. Since
#' matches with a lower distance are matched in an earlier iteration, step and
#' distance are strongly correlated.
#' This function has some CPU and RAM load. In most four group applications and
#' three group applications with large sample size, RAM may be not sufficient.
#' Therefore MAGMA switches to random quasi-systematic matching. If this is the
#' case MAGMA informs you. Results does not change, but Balance might be
#' sligtly affected.
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
#' @param cores An integer defining the number of cores used for
#' parallel computation.
#'
#' @author Julian Urban
#'
#' @import tidyverse parallel doParallel foreach
#' @return Your input data frame augmented with matching
#' relevant variables, namely *weight*, *step*, *distance*, and *ID*. In case
#' of missing values on the distance or group variable, MAGMA excludes them for
#' th matching process. The returned data set does not contain those excluded
#' cases. For more information, see Details.
#' @export
#'
#' @examples
#' #Running this code will take a while
#' #Computing two-group Matching for giftedness support
#' MAGMA_sim_data_gifted<- MAGMA(Data = MAGMA_sim_data,
#'                                 group = "gifted_support",
#'                                 dist = "ps_gifted",
#'                                 cores = 2)
#' head(MAGMA_sim_data_gifted)
#'
#' #Conducting three-group matching for teacher ability rating. Cores per
#' #default = 1
#' MAGMA_sim_data_tar <- MAGMA(Data = MAGMA_sim_data,
#'                             group = "teacher_ability_rating",
#'                             dist = "ps_tar")
#' head(MAGMA_sim_data_tar)
#'
#' #Computing 2x2 Matching for giftedness support and enrichment equivalent to
#' #a four group matching
#' MAGMA_sim_data_gift_enrich <- MAGMA(Data = MAGMA_sim_data,
#'                                    group = c("gifted_support", "enrichment"),
#'                                    dist = "ps_2x2",
#'                                    cores = 2)
#' head(MAGMA_sim_data_gift_enrich)
#'
MAGMA <- function(Data, group, dist, cores = 1) {
  
  #Check for regular input
  if(!is.data.frame(Data) && !is_tibble(Data)) {
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
      dplyr::filter(as.logical(!is.na(.[group])),
                    as.logical(!is.na(.[dist]))) %>%
      mutate( ID = c(1:nrow(Data)))
  }  else {
    values_1 <- unlist(unique(Data[group[1]]))
    values_2 <- unlist(unique(Data[group[2]]))
    if(length(dist) == 1) {
      data <- Data %>%
        dplyr::filter(as.logical(!is.na(.[group[1]])),
                      as.logical(!is.na(.[group[2]])),
                      as.logical(!is.na(.[dist]))) %>%
        dplyr::mutate(ID = c(1:nrow(Data)),
                      group_long = case_when(
                        as.logical(.[[group[1]]] == values_1[1] & .[[group[2]]] == values_2[1]) ~ 1,
                        as.logical(.[[group[1]]] == values_1[1] & .[[group[2]]] == values_2[2]) ~ 2,
                        as.logical(.[[group[1]]] == values_1[2] & .[[group[2]]] == values_2[1]) ~ 3,
                        as.logical(.[[group[1]]] == values_1[2] & .[[group[2]]] == values_2[2]) ~ 4
                      ))
    } else {
      data <- Data %>%
        dplyr::filter(as.logical(!is.na(.[group[1]])),
                      as.logical(!is.na(.[group[2]])),
                      as.logical(!is.na(.[dist[1]])),
                      as.logical(!is.na(.[dist[2]]))) %>%
        dplyr::mutate(ID = c(1:nrow(Data)),
                      group_long = case_when(
                        as.logical(.[[group[1]]] == values_1[1] & .[[group[2]]] == values_2[1]) ~ 1,
                        as.logical(.[[group[1]]] == values_1[1] & .[[group[2]]] == values_2[2]) ~ 2,
                        as.logical(.[[group[1]]] == values_1[2] & .[[group[2]]] == values_2[1]) ~ 3,
                        as.logical(.[[group[1]]] == values_1[2] & .[[group[2]]] == values_2[2]) ~ 4
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
                        distance = data[dist]) %>%
      set_names(c("ID", "group", "distance_ps")) %>%
      tibble::as_tibble(., .name_repair = "minimal") %>%
      dplyr::group_by(group) %>%
      dplyr::mutate(group_id = row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(weight = NA,
                    step = NA,
                    distance = NA)
  } else {
    if(length(dist) == 1) {
      input <- data.frame(ID = data["ID"],
                          group = data["group_long"],
                          distance = data[dist]) %>%
        set_names(c("ID", "group", "distance_ps")) %>%
        tibble::as_tibble(., .name_repair = "minimal") %>%
        dplyr::group_by(group) %>%
        dplyr::mutate(group_id = row_number()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(weight = NA,
                      step = NA,
                      distance = NA)
    } else {
      input <- data.frame(ID = data["ID"],
                          group = data["group_long"],
                          distance_1 = data[dist[1]],
                          distance_2 = data[dist[2]]) %>%
        set_names(c("ID", "group", "distance_ps_1", "distance_ps_2")) %>%
        tibble::as_tibble(., .name_repair = "minimal") %>%
        dplyr::group_by(group) %>%
        dplyr::mutate(group_id = row_number()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(weight = NA,
                      step = NA,
                      distance = NA)
    }
  }
  
  
  cat("input correctly identified")
  
  #######################
  #distance estimation##
  ######################
  if(length(dist) == 1) {  
    var_ma <- as.numeric(var(input$distance_ps))
    
    
    group_list <- split.data.frame(input, input$group)
    
    elements <- input %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(max(group_id)) %>%
      .[, 2] %>%
      unlist()
    
    if(length(elements) == 2) {
      value_matrix <-build_value_matrix(group_list, elements)
      
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
      
      output_temp <- do.call(rbind.data.frame, group_list)%>%
        select(ID, step, weight, distance)
      
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
        
        output_temp <- do.call(rbind.data.frame, group_list)%>%
          select(ID, step, weight, distance)
        
        data <- merge(data,
                      output_temp,
                      by = "ID",
                      all.x = T)
      } else {
        cores <- 2
        cat("\n", "Large Number of groups with large group sizes. Computing quasi-systematic matching. Cores were reduced to 2 to simplify node comumunication despite high RAM usage.")
        
        number_split_groups <- ceiling(sqrt(prod(elements) / 1.0e+09)) + 1
        
        initial_seed <- .Random.seed
        
        set.seed(28062022)
        
        input <- input %>%
          dplyr::mutate(random_group = floor(runif(nrow(input),
                                                   1, number_split_groups)))
        random_list <- split.data.frame(input, input$random_group)
        
        .Random.seed <- initial_seed
        
        for(i in 1:length(random_list)) {
          
          group_list_temp <- random_list[[i]] %>%
            split.data.frame(., .$group)
          
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
        data_temp <- do.call(rbind.data.frame, random_list) %>%
          dplyr::arrange(distance, step) %>%
          dplyr::mutate(step = ceiling(c(1:nrow(.))/3)) %>%
          dplyr::select(ID, step, weight, distance) %>%
          dplyr::filter(weight == 1)
        
        data <- merge(data,
                      data_temp,
                      by = "ID",
                      all.x = T)
        
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
        
        output_temp <- do.call(rbind.data.frame, group_list)%>%
          dplyr::select(ID, step, weight, distance)
        
        data <- merge(data,
                      output_temp,
                      by = "ID",
                      all.x = T)
      } else {
        cores <- 2
        cat("\n", "Large Number of groups with large group sizes. Computing quasi-systematic matching. Cores were reduced to 2 to simplify node comumunication despite high RAM usage.")
        
        number_split_groups <- ceiling(sqrt(prod(elements) / 1.0e+09)) + 1
        if(number_split_groups == 1) {
          number_split_groups <- 2
        }
        
        initial_seed <- .Random.seed
        
        set.seed(28062022)
        
        input <- input %>%
          dplyr::mutate(random_group = floor(runif(nrow(input),
                                                   1, number_split_groups)))
        random_list <- split.data.frame(input, input$random_group)
        
        .Random.seed <- initial_seed
        
        for(i in 1:length(random_list)) {
          
          group_list_temp <- random_list[[i]] %>%
            split.data.frame(., .$group)
          
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
        data_temp <- do.call(rbind.data.frame, random_list) %>%
          dplyr::arrange(distance, step) %>%
          dplyr::mutate(step = ceiling(c(1:nrow(.))/4)) %>%
          dplyr::select(ID, step, weight, distance) %>%
          dplyr::filter(weight == 1)
        
        data <- merge(data,
                      data_temp,
                      by = "ID",
                      all.x = T)
      }
    } else {
      stop("Specify a grouping variable that distinguishes 2, 3, or 4 groups or represent a 2x2 Design!")
    }
    
    cat("\n", "matching complete!")
    return(data)
  } else {
    var_ma <- input %>%
      dplyr::select(
        dplyr::starts_with("distance_")
      ) %>%
      sapply(., var)
    
    
    group_list <- split.data.frame(input, input$group)
    
    elements <- input %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(max(group_id)) %>%
      .[, 2] %>%
      unlist()
    
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
      
      output_temp <- do.call(rbind.data.frame, group_list)%>%
        dplyr::select(ID, step, weight, distance)
      
      data <- merge(data,
                    output_temp,
                    by = "ID",
                    all.x = T)
      cat("\n", "matching complete!")
      return(data)
      
    } else {
      cores <- 2
      cat("\n", "Large Number of groups with large group sizes. Computing quasi-systematic matching. Cores were reduced to 2 to simplify node comumunication despite high RAM usage.")
      
      number_split_groups <- ceiling(sqrt(prod(elements) / 1.0e+09)) + 1
      if(number_split_groups == 1) {
        number_split_groups <- 2
      }
      
      initial_seed <- .Random.seed
      
      set.seed(28062022)
      
      input <- input %>%
        dplyr::mutate(random_group = floor(runif(nrow(input),
                                                 1, number_split_groups)))
      random_list <- split.data.frame(input, input$random_group)
      
      .Random.seed <- initial_seed
      
      for(i in 1:length(random_list)) {
        
        group_list_temp <- random_list[[i]] %>%
          split.data.frame(., .$group)
        
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
      data_temp <- do.call(rbind.data.frame, random_list) %>%
        dplyr::arrange(distance, step) %>%
        dplyr::mutate(step = ceiling(c(1:nrow(.))/4)) %>%
        dplyr::select(ID, step, weight, distance) %>%
        dplyr::filter(weight == 1)
      
      data <- merge(data,
                    data_temp,
                    by = "ID",
                    all.x = T)
      cat("\n", "matching complete!")
      return(data)
    }
  }
}
