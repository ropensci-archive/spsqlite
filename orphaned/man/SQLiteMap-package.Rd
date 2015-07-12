\name{SQLiteMap-package}
\alias{SQLiteMap-package}
\alias{SQLiteMap}
\docType{package}
\title{Wrapper to spatialite functions}
\description{
Provides bindings to spatialite functionalities. Through this bindings (due to OGC specifications)  numerous spatial functions can be performed by simple SQL statements.
}
\section{Main available SQL statements}{

\tabular{lll}{
  \tab  \tab  \cr
 AddGeometryColumn \tab ExteriorRing \tab MBRWithin \cr
 Area \tab FilterMbrWithin \tab NumInteriorRings \cr
 AsBinary \tab FilterMbrContains \tab NumGeometries \cr
 AsText \tab FilterMbrIntersects \tab NumPoints \cr
 Buffer \tab GeometryN \tab Overlaps \cr
 Centroid \tab GeometryType \tab PointN \cr
 Contains \tab GeomFromText \tab RecoverGeometryColumn \cr
 ConvexHull \tab GeomFromWKB \tab Relate \cr
 CreateMbrCache \tab GLength \tab SetSrid \cr
 CreateSpatialIndex \tab GUnion \tab Simplify \cr
 Crosses \tab InteriorRingN \tab SimplifyPreserveTopology \cr
 Difference \tab Intersection \tab SRID \cr
 Dimension \tab Intersects \tab StartPoint \cr
 DisableSpatialIndex \tab MBRContains \tab SymDifference \cr
 DiscardGeometryColumn \tab MBRDisjoint \tab Touches \cr
 Disjoint \tab MBREqual \tab Transform \cr
 EndPoint \tab MBRIntersects \tab Within \cr
 Envelope \tab MBROverlaps \tab X \cr
 Equal \tab MBRTouches \tab Y \cr
}
}

\details{
\tabular{ll}{
Package: \tab SQLiteMap\cr
Type: \tab Package\cr
Version: \tab 0.5\cr
Date: \tab 2011-04-05\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
}
\author{
Norbert Solymosi

Maintainer: Norbert Solymosi <solymosi.norbert@gmail.com>
}
\references{
http://www.sqlite.org/\cr
http://www.gaia-gis.it/spatialite/\cr
http://www.gaia-gis.it/spatialite-2.4.0-4/spatialite-cookbook/\cr
http://www.opengeospatial.org/standards\cr
http://www.qgis.org/\cr
http://sourceforge.net/projects/maps2winbugs\cr
}
\keyword{ spatial }
\keyword{ SQL }
