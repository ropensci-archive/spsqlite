\name{sqli2map}
\alias{sqli2map}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Read SQLite geometry table into Map object }
\description{
   From SQLite database transforms the source geometries in WKT form to Map object.
}
\usage{
sqli2map(geoms, gcol)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{geoms}{ table contains the WKT geometry field }
  \item{gcol}{ the geometry field of \code{geoms} table }
}

\value{
Returns Map object depending on the source geometry type.
}

\author{ Norbert Solymosi <solymosi.norbert@gmail.com> }

\seealso{ \code{\link{sqli2sp}}, \code{\link{sqli.dump}}, \code{maptools} package }
\examples{
sqli.db <- system.file("sqlimaps/sids.db3", package="SQLiteMap")
drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname = sqli.db)


sql <- 'select * from sidsmap order by gid'
rs <- dbSendQuery(con, sql)
geom.tab <- fetch(rs, n = -1)


sids.map <- sqli2map(geoms=geom.tab, gcol='geom')
}
\keyword{ spatial }