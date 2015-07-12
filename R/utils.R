#' Check if SQlite driver is linked with GDAL
#'
#' @keywords internal

.SQLiteExists <- function(){
  og <- rgdal::ogrDrivers()
  return(og$write[grep("SQLite", og$name)])
}
