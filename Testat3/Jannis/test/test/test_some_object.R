library(testthat)

context('SomeClass')

so = SomeClass$new('test')

test_that('test 1', {
  expect_that(so$toString(), equals('test'))
})