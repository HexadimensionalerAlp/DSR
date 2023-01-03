library(R6)


StatsResults = R6Class(
  private = list(average = NULL, mean = NULL, standard_deviation = NULL, variance = NULL),
  public = list(
    initialize = function(average = 0, mean = 0, standard_deviation = 0, variance = 0) {
      private$average = average
      private$mean = mean
      private$standard_deviation = standard_deviation
      private$variance = variance
    }
  ),
  active = list(
    toString = function() paste0('Average: ', round(private$average, 7),
                                 ', Mean: ', round(private$mean, 7),
                                 ', Variance: ', round(private$variance, 7),
                                 ', Standard Deviation: ', round(private$standard_deviation, 7))
  )
)
