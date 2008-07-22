\name{SQLiteMap-package}
\alias{SQLiteMap-package}
\alias{SQLiteMap}
\docType{package}
\title{A package to manage vector graphical maps using SQLite

}
\description{
Some server based database management systems implemented the OpenGIS "Simple
Features Specification for SQL". The OpenGIS specification defines two standard ways
of expressing spatial features: the Well-Known Text (WKT) form and the Well-Known
Binary (WKB) form. Both WKT and WKB include information about the type of the
feature and the coordinates which form the feature. These systems (e.g. PostgreSQLPostGIS,
MySQL, ORACLE, MSSQL) allow to store the topological features and the
descriptive data in the same database. \cr
But these systems assume that the user needs permission to a running service or to
install a server to use the spatial data. In some cases, it is useful if the user can use the
database stored maps on different computers and platforms. The SQLite is a good choice
for a portable database, it is platform-idendependent and there are some R packages to
manage SQLite databases. Unfortunately, it has no spatial extension, but there is an
SQLite extension for the SharpMap library.\cr
Following the idea of this solution this package was developed that may help the user read
and write spatial features from and to an SQLite database. Each table with geometry
field is treated as a layer. The tables contain the topological features in one geometry field in WKT form.\cr
For conversion of standard map files to SQLite table maps2WinBUGS can be used.
}
\details{
\tabular{ll}{
Package: \tab SQLiteMap\cr
Type: \tab Package\cr
Version: \tab 0.3\cr
Date: \tab 2008-07-28\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
}
\author{
Norbert Solymosi

Maintainer: Norbert Solymosi <solymosi.norbert@gmail.com>
}
\references{
http://www.opengeospatial.org/standards\cr
http://postgis.refractions.net/\cr
http://www.codeplex.com/SharpMap\cr
http://sourceforge.net/projects/maps2winbugs
}
\keyword{ spatial }
