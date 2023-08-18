#' Density_overlap
#'
#' This function shows and quantifies the kernel density overlap of a variable
#' for two or more groups.
#'
#' This function enables the comparison of the density of variables for two
#' or more groups. It plots the kernel density separately for each group and
#' quantifies the amount of overlap.
#'
#' @param Data A data frame that contains the desired variable for density
#' plotting as well as the specified grouping variable.
#' @param variable A character naming the variable for that the density should
#' be plotted.
#' @param group A character specifying the groups for which the density should
#' be plotted. Can be an independent group comparison (e.g., comparing matched
#' groups) or the comparison of pre and post matched samples.
#' @param variable_name A character specifying the name that should appear 
#' in the plot for variable.
#' @param group_labels A character vector specifying the labels for the groups 
#' that should appear in the legend of the plot.
#' @param group_name A character specifying the name of the grouping variable
#' tha should appear in the title of the legend.
#' @param step_num An integer specifying the number of cases that should be included
#' per group in this post matching comparison. Is based on the step variable of
#' MAGMA.
#' @param step_var A Character specifying the name of the step variable.
#'
#' @author Julian Urban
#'
#' @import tidyverse ggplot2 overlapping
#' 
#' @references \insertRef{Pastore M, Loro PAD, Mingione M, Calcagni' A (2022). _overlapping: Estimation of Overlapping in Empirical Distributions_. R package version
#' 2.1, <https://CRAN.R-project.org/package=overlapping>.}
#'
#' @return A Plot showing the kernel density for a specified variable separated
#' for specified groups and the quantification of this overlap.
#' @export
#'
#' @examples
#' \dontrun {
#' #Density overlap in Propensity scores for gifted before matching
#' Density_overlap(Data = MAGMA_sim_data,
#' variable = "ps_gifted", 
#' group = "gifted_support",
#' step = NULL,
#' variable_name = "Propensity Score",
#' group_labels = c("No Support", "Support"),
#' group_name = "Gifted Support")
#' 
#'  #Density overlap in Propensity scores for gifted after matching with 250 cases per group.
#' Density_overlap(Data = MAGMA_sim_data_gifted,
#' variable = "ps_gifted", 
#' group = "gifted_support",
#' step_num = 250,
#' step_var = "step",
#' variable_name = "Propensity Score",
#' group_labels = c("No Support", "Support"),
#' group_name = "Gifted Support")
#' 
#'#Density overlap in GPA for teacher ability rating before matching
#' Density_overlap(Data = MAGMA_sim_data,
#' variable = "GPA_school", 
#' group = "teacher_ability_rating",
#' variable_name = "School Achievement",
#' group_labels = c("Low", "Medium", "High"),
#' group_name = "Rating") 
#' 
#' }
#'
Density_overlap <- function(Data, variable, group,
                            variable_name = NULL,
                            group_labels = NULL,
                            group_name  = NULL,
                            step_num = NULL,
                            step_var = NULL) {
  if(!is.data.frame(Data) && !is_tibble(Data)) {
    stop("Data needs to be an object of class dataframe or tibble!")
  }
  if(!is.character(variable) | length(variable) > 1) {
    stop("Variable needs to be a character of length 1!")
  }
  if(!is.character(group) | length(group) > 2) {
    stop("Group needs to be a character of length 1 or 2!")
  }
  if((!is.character(variable_name) & !is.null(variable_name)) | length(variable_name) > 1) {
    stop("variable_name needs to be a character of length 1!")
  }
  if((!is.character(group_labels) & !is.null(group_labels))) {
    stop("group_labels needs to be a character vecetor!")
  }
  if((!is.character(group_name) & !is.null(group_name)) | length(group_name) > 1) {
    stop("group_name needs to be a character of length 1!")
  }
  if(!is.null(step_num) & is.null(step_var) | is.null(step_num) & !is.null(step_var)) {
    stop("step_num and step_var need to be both NULL or both specified!")
  }
  
  if(length(group) == 2) {
    Data <- Data %>%
      dplyr::mutate(group_long = dplyr::case_when(
        .[group[1]] == unique(Data[group[1]])[1, 1] &
          .[group[2]] == unique(Data[group[2]])[1, 1] ~ 1,
        .[group[1]] == unique(Data[group[1]])[1, 1] &
          .[group[2]] == unique(Data[group[2]])[2, 1] ~ 2,
        .[group[1]] == unique(Data[group[1]])[2, 1] &
          .[group[2]] == unique(Data[group[2]])[1, 1] ~ 3,
        .[group[1]] == unique(Data[group[1]])[2, 1] &
          .[group[2]] == unique(Data[group[2]])[2, 1] ~ 4
      ))
    group <- "group_long"
    cat("2x2 groups are represented as 4 groups.")
  }

  
  if(is.null(variable_name)) {
    names_variable <- variable
  } else {
    names_variable <- variable_name
  }
  
  if(is.null(group_name)) {
    name_group <- group
  } else {
    name_group <- group_name
  }
  
  if(!is.null(step_num) & !is.null(step_var)) {
    Data <- Data %>%
      dplyr::filter(.[step_var] <= step_num)
  }
  
  
  Data[, group] <- as.factor(Data[ ,group])
  
  if(is.null(group_labels)) {
    levels(Data[ , group]) <- c("group 1", "group 2")
  } else {
    levels(Data[ , group]) <- group_labels
  }
  
  Data_plot <- data.frame(var = Data[ , variable],
                          group = Data[ , group])

    print(
      ggplot2::ggplot(Data_plot, aes(x = var,
                   fill = group,
                   color = group)) +
      ggplot2::geom_density(alpha = 0.5) +
      ggplot2::theme(panel.background = element_blank()) +
      ggplot2::labs(title = paste("Density Plot for ", names_variable, sep = ""),
                    x = names_variable,
                    y = "Density",
                    fill = name_group,
                    color = name_group)
    )
    
overlap_areas <- split.data.frame(Data, Data[group]) %>%
  lapply(., function(data) data %>%
           select(all_of(variable)) %>%
           unlist()) %>%
  overlapping::overlap(., type= "1") %>%
  unlist()

print(overlap_areas)
  
}

