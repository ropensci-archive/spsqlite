#' Check if SQlite driver is linked with GDAL
#'
#' @export
#' @keywords internal

.SQLiteExists <- function(){
  og <- ogrDrivers()
  return(og$write[grep("SQLite",og$name)])
}
