\name{sqli2sp}
\alias{sqli2sp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Read SQLite geometry table into spatial }
\description{
   From SQLite database transforms the source geometries in WKT form to SpatalPolygons, SpatialLines or SpatialPoints object.
}
\usage{
sqli2sp(geoms, gcol, idcol)
}
\arguments{
  \item{geoms}{ table contains the WKT geometry field}
  \item{gcol}{ WKT field of \code{geoms} table, what can be transformed from spatialite Geometry type field by SQL statement Astext() }
  \item{idcol}{ field of \code{geoms} table for identification }
}

\value{
Returns SpatalPolygons, SpatialLines or SpatialPoints depending on the source geometry type.
}

\author{ Norbert Solymosi <solymosi.norbert@gmail.com> }

\seealso{ \code{\link{sqli.dump}}, \code{sp} package}
\examples{
sqli.db <- system.file("sqlimaps/maps.sqlite", package="SQLiteMap")
drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname = sqli.db, loadable.extensions = TRUE)
spatialite.init(con)

sql <- 'select SP_ID, NAME, BIR74, SID74, BIR79, SID79, 
  astext(geometry) as geom from sids'
sids <- dbGetQuery(con, sql)

sids.sp <- sqli2sp(geoms=sids, gcol='geom', idcol='NAME')
sids.attr <- data.frame(R74 = sids$SID74/sids$BIR74, 
  R79 = sids$SID79/sids$BIR79)
rownames(sids.attr) <- sids$NAME
sids.df <- SpatialPolygonsDataFrame(sids.sp, sids.attr)

spplot(sids.df)
}


\keyword{ spatial }
