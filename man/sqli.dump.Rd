\name{sqli.dump}
\alias{sqli.dump}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Save maps into SQLite database }
\description{
   Save Map or Spatial objects into SQLite database in two geometry and attribute tables.
}
\usage{
sqli.dump(db, mapobj, mn)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{db}{ path of SQLite database }
  \item{mapobj}{ the Map or Spatial object }
  \item{mn}{ save as name of object }
}

\author{ Norbert Solymosi <solymosi.norbert@gmail.com> }

\seealso{ \code{\link{sqli2map}}, \code{\link{sqli2sp}}, \code{maptools} package, \code{sp} package}
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


sqli.dump(db = 'test.db3', mapobj = sids.df, mn = 'sidsexport')
}
\keyword{ spatial }
