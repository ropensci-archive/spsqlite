context("initialize")

test_that("init spatial extension works", {

  library("RSQLite")
  con <- invisible(dbConnect(RSQLite::SQLite(), dbname = "inst/examples/test-2.3.sqlite", loadable.extensions = TRUE))
  init1 <- suppressWarnings(initSpatialExtension(con))
  expect_is(init1, "character")
  expect_match(init1, "Spatialite version")

  init2 <- tryCatch(initSpatialExtension(con), error = function(e) e)
  expect_is(init2, "error")

})
