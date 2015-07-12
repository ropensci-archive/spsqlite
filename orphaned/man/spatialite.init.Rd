\name{spatialite.init}
\alias{spatialite.init}
\title{Initialize the SQLite database as spatialite one}
\description{
   Load the spatialite extension into the database connection
}
\usage{
spatialite.init(con)
}
\arguments{
  \item{con}{ a SQLite connection }
}

\author{ Norbert Solymosi <solymosi.norbert@gmail.com> }

\seealso{ \code{\link[RSQLite.extfuns]{init_extensions}}}
\examples{
sqli.db <- system.file("sqlimaps/maps.sqlite", package="SQLiteMap")
drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname = sqli.db, loadable.extensions = TRUE)
spatialite.init(con)

}
\keyword{ spatial }

