library(testthat)

test_df_1 = data.frame(A = c('A', 'B', 'C', 'D', 'E'), B = c(1, 2, 3, 4, 5), C = c(7, 0, 5, NA, -2))
test_df_2 = data.frame(A = c('A', 'B', 'C', 'D', 'E'))

test_that('getNumbers from valid data frame', {
  expect_equal(getNumbers(test_df_1), c(1, 2, 3, 5, 7, 0, 5, -2))
})
test_that('getNumbers from a data frame without numbers', {
  expect_equal(getNumbers(test_df_2), c())
})

test_that('Average on valid data frame', {
  expect_equal(df_average(test_df_1), 2.625)
})
test_that('Average on data frame without numbers', {
  expect_equal(df_average(test_df_2), 0)
})

test_that('Mean on valid data frame', {
  expect_equal(df_mean(test_df_1), 2)
})
test_that('Mean on data frame without numbers', {
  expect_equal(df_mean(test_df_2), 0)
})

test_that('Variance on valid data frame', {
  expect_equal(df_variance(test_df_1), 8.8392857)
})
test_that('Variance on data frame without numbers', {
  expect_equal(df_variance(test_df_2), 0)
})

test_that('Standard deviation on valid data frame', {
  expect_equal(df_standard_deviation(test_df_1), 2.9730936)
})
test_that('Standard deviation on data frame without numbers', {
  expect_equal(df_standard_deviation(test_df_2), 0)
})

test_that('All stats on valid data frame', {
  expect_equal(df_all_stats(test_df_1), StatsResults$new(2.625, 2, 8.8392857, 2.9730936))
})
test_that('All stats on data frame without numbers', {
  expect_equal(df_all_stats(test_df_2), StatsResults$new())
})