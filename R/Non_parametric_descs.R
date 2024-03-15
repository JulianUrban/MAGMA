#' row_ordinal
#'
#' Computes descriptive statistics and effect size for ordinal data
#'
#' This function computes descriptive statistics and effect size for ordinal data.
#'
#' @param Data A data set
#' @param group A character specifying the grouping variable
#' @param variable Variables for which the effect should be estimated
#'
#' @author Julian Urban
#'
#' @importFrom stats median
#' @importFrom stats IQR
#' 
#' @return A vector containing the adjusted d-ratio in dependency of
#' sample size.
#'
#'
row_ordinal<- function(Data,
                       group,
                       variable) {
  
  group_values <- sort(unique(Data[, group]))
  table_statistics <- lapply(variable,
                             function(var) {
                               table(Data[ , group], Data[, var])
                             })
  
  overall_stats <- sapply(c(1:length(variable)),
                          function(index) {
                            c(sum(table_statistics[[index]]),
                              stats::median(Data[, variable[index]], na.rm = TRUE),
                              stats::IQR(Data[, variable[index]], na.rm = TRUE)
                            )
                          })
  
  group_stats <- sapply(c(1:length(variable)),
                        function(index) {
                          sapply(group_values,
                                 function(gr) {
                                   c(sum(table_statistics[[index]][as.character(gr), ]),
                                     stats::median(Data[Data[, group] == gr, variable[index]], na.rm = TRUE),
                                     stats::IQR(Data[Data[, group] == gr, variable[index]], na.rm = TRUE)
                                   )
                                 })
                          
                        })
  effect_sizes <- round(effect_ordinal(Data, group, variable), digits = 2)
  
  if(length(variable) > 1) {
  output <- rbind(overall_stats, group_stats, t(effect_sizes)) %>%
    t()
  } else {
    output <- t(c(overall_stats, group_stats, effect_sizes))
  }
  
  return(output)
}

#' effect_ordinal
#'
#' Computes descriptive statistics and effect size for ordinal data
#'
#' This function computes descriptive statistics and effect size for ordinal
#' data.
#'
#' @param Data A data set
#' @param group A character specifying the grouping variable
#' @param variable Variables for which the effect should be estimated
#'
#' @author Julian Urban
#'
#' @importFrom stats qnorm
#' @importFrom stats wilcox.test
#' 
#' @return A vector or matrix containing ordinal effect sizes
#'
#'
effect_ordinal <- function(Data,
                           group,
                           variable) {
  
  group_values <- sort(unique(Data[, group]))
  
  if(length(group_values) == 2) {
    index_matrix <- matrix(data = c(1, 2),
                           ncol = 2)
  } else if(length(group_values) == 3) {
    index_matrix <- matrix(data = c(1, 1, 2, 2, 3, 3),
                           ncol = 2)
  } else if(length(group_values) == 4) {
    index_matrix <- matrix(data = c(1, 1, 1, 2, 2, 3,
                                    2, 3, 4, 3, 4, 4),
                           ncol = 2)}
  
  effects <- sapply(c(1:nrow(index_matrix)),
                    function(row) {
                      group_1 <- index_matrix[row, 1]
                      group_2 <- index_matrix[row, 2]
                      groups_temp <- group_values[c(group_1, group_2)]
                      Data_temp <- Data[Data[, group] %in% groups_temp, ]
                      sapply(variable,
                             function(var) {
                               p_value <- stats::wilcox.test(
                                 Data_temp[, var] ~ Data_temp[, group])[["p.value"]]
                               stats::qnorm(p_value / 2) / sqrt(nrow(Data_temp[!is.na(Data_temp[, var]), ]))
                             })
                    }) 
  
  return(effects)
}


#' row_nominal
#'
#' Computes descriptive statistics and effect size for nominal data
#'
#' This function computes descriptive statistics and effect size for nominal data.
#'
#' @param Data A data set
#' @param group A character specifying the grouping variable
#' @param variable Variables for which the effect should be estimated
#'
#' @author Julian Urban
#'
#' @importFrom stats median
#' @importFrom stats IQR
#' 
#' @return A vector containing the adjusted d-ratio in dependency of
#' sample size.
#'
#'
row_nominal <- function(Data,
                       group,
                       variable) {
  
  group_values <- sort(unique(Data[, group]))
  table_statistics <- lapply(variable,
                             function(var) {
                               table(Data[ , group], Data[, var])
                             })
  
  overall_stats <- sapply(c(1:length(variable)),
                          function(index) {
                            c(sum(table_statistics[[index]]),
                              as.numeric(names(which(colSums(table_statistics[[index]]) == max(colSums(table_statistics[[index]]))))),
                              ncol(table_statistics[[index]])
                            )
                          })
  
  group_stats <- sapply(c(1:length(variable)),
                        function(index) {
                          sapply(group_values,
                                 function(gr) {
                                   c(sum(table_statistics[[index]][as.character(gr), ]),
                                     as.numeric(
                                       names(
                                         which(
                                           table_statistics[[index]][as.character(gr), ]
                                              == max(
                                                 table_statistics[[index]][as.character(gr), ])
                                           ))),
                                     length(table_statistics[[index]][as.character(gr), ])
                                   )
                                 })
                          
                        })
  
  effect_sizes <- round(effect_nominal(Data, group, variable), digits = 2)
  
  if(length(variable) > 1) {
    output <- rbind(overall_stats, group_stats, t(effect_sizes)) %>%
      t()
  } else {
    output <- t(c(overall_stats, group_stats, effect_sizes))
  }
  
  return(output)
}

#' effect_ordinal
#'
#' Computes descriptive statistics and effect size for nominal data
#'
#' This function computes descriptive statistics and effect size for nominal
#' data.
#'
#' @param Data A data set
#' @param group A character specifying the grouping variable
#' @param variable Variables for which the effect should be estimated
#'
#' @author Julian Urban
#'
#' @importFrom stats chisq.test
#' 
#' @return A vector or matrix containing nominal effect sizes
#'
#'
effect_nominal <- function(Data,
                           group,
                           variable) {
  
  group_values <- sort(unique(Data[, group]))
  
  if(length(group_values) == 2) {
    index_matrix <- matrix(data = c(1, 2),
                           ncol = 2)
  } else if(length(group_values) == 3) {
    index_matrix <- matrix(data = c(1, 1, 2, 2, 3, 3),
                           ncol = 2)
  } else if(length(group_values) == 4) {
    index_matrix <- matrix(data = c(1, 1, 1, 2, 2, 3,
                                    2, 3, 4, 3, 4, 4),
                           ncol = 2)}
  
  effects <- sapply(c(1:nrow(index_matrix)),
                    function(row) {
                      group_1 <- index_matrix[row, 1]
                      group_2 <- index_matrix[row, 2]
                      groups_temp <- group_values[c(group_1, group_2)]
                      Data_temp <- Data[Data[, group] %in% groups_temp, ]
                      sapply(variable,
                             function(var) {
                               Chi_test <- stats::chisq.test(Data_temp[, var],
                                                             Data_temp[, group])
                               round(
                                 sqrt(Chi_test[["statistic"]][["X-squared"]] /
                                            sum(Chi_test[["observed"]])), 2)
                             })
                    }) 
  
  return(effects)
}
