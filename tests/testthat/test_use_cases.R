context("stabpath::get_stability_paths(w/ subsample method) - test of use cases ")


library(leaps)
library(ElemStatLearn)

data(prostate)



model <- regsubsets(x = lpsa ~ (. - train), data = prostate, 
  nbest = 1, nvmax = 8, really.big = TRUE)
# note: using variable 'max_formula <- lpsa ~ (. - train)' will result in 
# check() run error message: 
#   max_formula is not found when this is run via check()

model$call <- quote(regsubsets(x = lpsa ~ (. - train), data = prostate, 
  nbest = 1, nvmax = 8, really.big = TRUE))
# 'model$call <- ...' needed, otherwise test cases are not running 
#  with devtools::test() or covr::package_coverage() => throwing error:
#  object 'code' not found
#   reason: model$call when refit.regsubsets is run under devtools::test 
#           does not have the expected structure

test_that("get_stability_paths with prostate data - subsample", {
  expect_is(
    get_stability_paths(model, data = prostate, reps = 10),
    c("stabpath", "matrix"))
  expect_named(
    get_stability_paths(model, data = prostate, reps = 10)[5, ],
    c("lcavol", "lweight", "age", "lbph", "svi", "lcp", "gleason", "pgg45"))
})

set.seed(20141020)
s <- get_stability_paths(model, data = prostate, reps = 10)[5, ]

test_that("get_stability_paths with prostate data - subsample, row values", {
  expect_equal(
    unname(s),
    c(1.0, 0.8, 0.3, 0.7, 0.9, 0.1, 0.0, 0.2))
})

context("stabpath::get_stability_paths (w/ bootstrap method) - test of use cases ")

test_that("get_stability_paths with prostate data - bootstrap", {
  expect_is(
    get_stability_paths(model, data = prostate, reps = 10, method = "bootstrap"),
    c("stabpath", "matrix"))
  expect_named(
    get_stability_paths(model, data = prostate, reps = 10, method = "bootstrap")[5, ],
    c("lcavol", "lweight", "age", "lbph", "svi", "lcp", "gleason", "pgg45"))
})

set.seed(20141020)
b <-   get_stability_paths(model, data = prostate, reps = 10, method = "bootstrap")[5, ]
test_that("get_stability_paths with prostate data - bootstrap, row values", {
  expect_equal(
    unname(b),
    c(1.0, 0.7, 0.4, 0.4, 0.9, 0.2, 0.2, 0.2))
})

