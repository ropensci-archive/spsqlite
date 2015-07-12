\name{sqli.dump}
\alias{sqli.dump}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Save maps into spatialite database }
\description{
   Save Spatial objects into SQLite database as a geometry and an attribute tables.
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

\seealso{ \code{\link{sqli2sp}}, \code{sp} package}
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

\dontrun{
t.db = '/home/user/wd/targetdb.sqlite'
t.con = dbConnect(SQLite(), dbname = t.db)
sqli.dump(t.con, mapobj = sids.df, mn = 'sidsexport')
}

}


\keyword{ spatial }
