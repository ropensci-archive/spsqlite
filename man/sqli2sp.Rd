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
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{geoms}{ table contains the WKT geometry field}
  \item{gcol}{ the geometry field of \code{geoms} table }
  \item{idcol}{ field of \code{geoms} table for identification }
}

\value{
Returns SpatalPolygons, SpatialLines or SpatialPoints depending on the source geometry type.
}

\author{ Norbert Solymosi <solymosi.norbert@gmail.com> }

\seealso{ \code{\link{sqli2map}}, \code{\link{sqli.dump}}, \code{sp} package}
\examples{
sqli.db <- system.file("sqlimaps/sids.db3", package="SQLiteMap")
drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname = sqli.db)


sql <- 'select sidsmap.gid, sidsmap.geom, sidsattr.* 
  from sidsmap Inner Join sidsattr On sidsattr.sp_id = sidsmap.sp_id 
  order by sidsattr.name'
rs <- dbSendQuery(con, sql)
join.data <- fetch(rs, n = -1)


sids.sp <- sqli2sp(geoms=join.data, gcol='geom', idcol='name')
sids.attr <- data.frame(R74 = join.data$sid74/join.data$bir74, 
  R79 = join.data$sid79/join.data$bir79)
rownames(sids.attr) <- join.data$name
sids.df <- SpatialPolygonsDataFrame(sids.sp, sids.attr)


library(RColorBrewer)
spplot(sids.df, 
  col.regions = colorRampPalette(brewer.pal(9,"OrRd")[1:9])(140))
}

\keyword{ spatial }
