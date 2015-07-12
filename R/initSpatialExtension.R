#' Enable spatialite capabilities for an existing sqlite library
#'
#' Currently based on theloadable extension from Peter Schmiedeskamp
#' https://github.com/pschmied/RSQLite.spatialite
#'
#' @author Martin Jung
#' @import RSQLite
#' @param con A RSQLite connection link
#' @export

initSpatialExtension <- function(con) {
  # Check if connection object is valid
  if(!dbIsValid(con)) stop("Not a valid SQLiteConnection")

  # Test if spatial extension is already loaded
  res <- try(dbGetQuery(con,"SELECT spatialite_version()"),silent = TRUE)
  if(class(res) != "try-error") {
    stop("Spatial SQlite extension already loaded")
  } else {
    lib <- lib_path()
    if(is.null(lib)) warning("Spatialite library not dynamically loaded!")
    # Alternative lookup in local path
    lib <- paste0(path.package("spsqlite"),"/libs/spsqlite.so")
    if(!file.exists(lib))stop("Spatialite library could not be found!")
    res <- try( dbGetQuery(con, sprintf("SELECT load_extension('%s')",lib)),silent = TRUE)
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

#' Get the path of the spsqlite dll
#' @keywords internal
lib_path <- function()
{
  getLoadedDLLs()[["spsqlite"]][["path"]]
}


#' Quick query if spatiallite is loaded
#' @author Martin Jung
#' @param con A RSQLite connection link
#' @export
#' @keywords internal

is_spatial <- function(con) {
  res <- try(dbGetQuery(con,"SELECT spatialite_version()"),silent = TRUE)
  if(class(res) != "try-error") return(TRUE) else return(FALSE)
}
