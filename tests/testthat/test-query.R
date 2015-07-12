context("initialize")

test_that("spatial queries work", {

  library('RSQLite')
  con <- invisible(dbConnect(RSQLite::SQLite(), dbname = "data/test-2.3.sqlite", loadable.extensions = TRUE))
  suppressWarnings(initSpatialExtension(con))

  # List Tables
  tables <- dbListTables(con)
  expect_is(tables, "character")
  expect_equal(length(tables), 7)
  expect_true(any(grepl('HighWays', tables)))

  # Show the structure of Towns (point-shape)
  struct <- dbGetQuery(con,"SELECT * FROM Towns LIMIT 2")
  expect_is(struct, "data.frame")

  # Now query the same but format the geometry using spatialite functions (X,Y)
  splite1 <- dbGetQuery(con,"SELECT Name, X(Geometry) AS Longitude, Y(Geometry) AS Latitude, SRID(Geometry) as SRID, GeometryType(Geometry) FROM Towns LIMIT 2")
  expect_is(splite1, "data.frame")
  expect_named(splite1, c("Name", "Longitude", "Latitude", "SRID", "GeometryType(Geometry)"))

  # Calculate the length im m of the biggest 5 Highways
  splite2 <- dbGetQuery(con,"SELECT Name, Length(Geometry) as length FROM HighWays ORDER BY length DESC LIMIT 5")
  expect_is(splite2, "data.frame")
  expect_is(splite2$length, "integer")
  expect_named(splite2, c("Name", "length"))

})