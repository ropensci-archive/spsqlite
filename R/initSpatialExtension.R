#' Enable spatialite capabilities for an existing sqlite library
#'
#' Currently based on theloadable extension from Peter Schmiedeskamp
#' which has a 'Artistic-2.0' license
#'
#' @author Martin Jung
#' @param con A RSQLite connection link
#' @export

initSpatialExtension <- function(con) {
  # Test if RSQlite extension is available.
  # On the long-term implement own integration with plat-form independent
  # hardlinked library (current version is 4.2.0)
  if(!require(RSQLite.spatialite)){
    # Install devtools if necessary
    if(!require(devtools))install.packages("devtools");library(devtools)

    # Install from Github
    install_github(repo="RSQLite.spatialite", username="pschmied")
    library(RSQLite.spatialite)
  }

  # Check if connection object is valid
  if(!dbIsValid(con)) stop("Not a valid SQLiteConnection")

  # Test if spatial extension is already loaded
  res <- try(dbGetQuery(con,"SELECT spatialite_version()"),silent = TRUE)
  if(class(res) != "try-error") {
    stop("Spatial SQlite extension already loaded")
  } else {
    res <- try( dbGetQuery(con, sprintf("SELECT load_extension('%s')",lib_path())),silent = TRUE)
    # Check if correctly loaded
    if(class(res) != "try-error" && all(dim(res) == c(1, 1))) {
      paste0("Spatialite extension successfully loaded")
      res <- dbGetQuery(con,"SELECT spatialite_version() as spatialite_version")
      paste0("Spatialite version: ",res$spatialite_version)
    } else {
      stop("SQlite does not allow extensions")
    }
  }

}

#' Get the path of the RSQLite.spatialite dll
#' @keywords internal
lib_path <- function()
{
  getLoadedDLLs()[["RSQLite.spatialite"]][["path"]]
}