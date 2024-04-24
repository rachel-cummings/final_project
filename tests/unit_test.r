## This tests that there are no duplicate individuals before we do our mean collapse
# of the binary hour variables. Essentially, we are checking that no one will be double 
# (or triple, etc) counted. Note that TUCASEID starts with the year, so this is a unique identifier



test_that("This tests that there are no duplicates of individuals prior to mean collapse", {
  expect_unique(data = atus_filter, c(TUCASEID))
}) 