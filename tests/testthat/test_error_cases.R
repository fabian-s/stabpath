context("stabpath::get_stability_paths() - test of error cases ")

# Notee:  only very short part of  error message - otherwise 
#  German <=> English language issue with covr::package_coverage() 

test_that("Error cases", {
  expect_error(
    get_stability_paths(model = NULL, data = 1:10, reps = 100, method = "subsample",
      strata = NULL, fraction = 0.5)
    , "'resample'")     
  expect_error(
    get_stability_paths(model = NULL, data = 1:10, reps = 100, method = "bootstrap",
      strata = NULL, fraction = 0.5)
    , "'resample'")
  expect_error(
  get_stability_paths(model = NULL, data = mtcars, reps = 100, method = "bootstrap",
    strata = NULL, fraction = 0.5)
  , "'refit'")
})

