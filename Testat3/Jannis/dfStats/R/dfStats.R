library(dplyr)
source('R/resultObject.R')

#' Calculates the average over all numeric columns in the given data frame.
#' @param data a data frame
#' @return The average of the data frame.
#' @export
#' @examples
#' average = df_average(test_data)
#' print(average)
df_average = function(data) {
  result = 0
  numbers = getNumbers(data)
  
  if (length(numbers) > 0) {
    result = sum(numbers) / length(numbers)
  }
  
  return(result)
}

#' Calculates the mean over all numeric columns in the given data frame.
#' @param data a data frame
#' @return The mean of the data frame.
#' @export
#' @examples
#' m = df_mean(test_data)
#' print(m)
df_mean = function(data) {
  result = 0
  numbers = sort(getNumbers(data))
  
  if (length(numbers) > 0) {
    result = numbers[(length(numbers) + 1) %/% 2]
  }
  
  return(result)
}

#' Calculates the variance over all numeric columns in the given data frame.
#' @param data a data frame
#' @return The variance of the data frame.
#' @export
#' @examples
#' variance = df_variance(test_data)
#' print(variance)
df_variance = function(data) {
  result = 0
  numbers = getNumbers(data)
  
  if (length(numbers) > 1) {
    av = df_average(data)
    result = sum(sapply(numbers, function(n) (n - av) * (n - av))) / (length(numbers) - 1)
  }
  
  return(result)
}

#' Calculates the standard deviation over all numeric columns in the given data frame.
#' @param data a data frame
#' @return The standard deviation of the data frame.
#' @export
#' @examples
#' standard_deviation = df_standard_deviation(test_data)
#' print(standard_deviation)
df_standard_deviation = function(data) {
  result = sqrt(df_variance(data))
  print(sd(getNumbers(data)))
  
  return(result)
}

#' Calculates all statistics over all numeric columns in the given data frame.
#' @param data a data frame
#' @return An object containing all stats for the given data frame.
#' @export
#' @examples
#' df_stats = df_all_stats(test_data)
#' print(df_stats)
df_all_stats = function(data) {
  
  av = df_average(data)
  m = df_mean(data)
  v = df_variance(data)
  s_d = df_standard_deviation(data)
  
  return(StatsResults$new(av, m, v, s_d))
}

#' Removes any row containing 'NA' in a numeric column and extracts all numbers.
#' @param data a data frame
#' @return A vector with the data frames numbers.
getNumbers = function(data) {
  numbers = data |> select(where(is.numeric)) |> na.omit()
  return(unlist(numbers, use.names = FALSE))
}