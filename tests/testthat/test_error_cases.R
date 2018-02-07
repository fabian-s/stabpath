context("stabpath::get_stability_paths() - test of error cases ")

# Note: for error messages 'or' linkage of English / German language text used
#       to avoide issue with covr::package_coverage() 

test_that("Error cases", {
  expect_error(
    get_stability_paths(model = NULL, data = 1:10, reps = 100, method = "subsample",
      strata = NULL, fraction = 0.5), 
    "(no applicable method for 'resample' applied to an object of class|nicht anwendbare Methode für 'resample' auf Objekt der Klasse)")     
  expect_error(
    get_stability_paths(model = NULL, data = 1:10, reps = 100, method = "bootstrap",
      strata = NULL, fraction = 0.5),
    "(no applicable method for 'resample' applied to an object of class|nicht anwendbare Methode für 'resample' auf Objekt der Klasse)")
  expect_error(
    get_stability_paths(model = NULL, data = mtcars, reps = 100, method = "bootstrap",
      strata = NULL, fraction = 0.5),
    "(no applicable method for 'refit' applied to an object of class|nicht anwendbare Methode für 'refit' auf Objekt der Klasse)")
})

