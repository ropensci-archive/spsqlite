sqli2map = function(geoms, gcol)
{
   splsp = function(x) strsplit(x, " ")
   shps = list()

   for(i in 1:dim(geoms)[1])
   {
		geomi = geoms[i,gcol]
		typ = sub(' ', '', strsplit(geomi, '\\(')[[1]][1])	 
		m = matrix(nc=2); m = m[is.na(m[, 2]) == FALSE, ]			  
		geom.i = as.character(geomi)
		
		if (typ == "POINT") {
			shptype  = "point"
			shptypen = 1
			pstart   = 0
			geomi    = sub(paste(typ, " " , sep=""), "", geomi)
			geomi    = sub(typ, "", geomi)
			geomi    = gsub("\\(", "", geomi)
			geomi    = gsub("\\)", "", geomi)
			geomi    = strsplit(geomi, ",")
			geomi    = strsplit(geomi[[1]], " ")
			verts    = matrix(as.numeric(geomi[[1]]), nc = 2)
			bbox     = rep(as.numeric(geomi[[1]]), 2)
			geomdim  = 1
			nParts   = 0
		}
		
        if (typ == "LINESTRING") {
            shptype  = "arc"
            shptypen = 3
			pstart   = 0
			geomi    = sub(paste(typ, " " , sep=""), "", geomi)
			geomi    = sub(typ, "", geomi)
			m        = poly.pars(geomi)
            bbox     = c(min(m[,1]), min(m[,2]), max(m[,1]), max(m[,2]))
            verts    = m
            geomdim  = c(dim(m)[1], 2)
            nParts   = 1
        }		
		
        if (typ == "MULTILINESTRING") {
            shptype  = "arc"
            shptypen = 3
	        geomi    = sub(paste(typ, " " , sep=""), "", geomi)
	        geomi    = sub(typ, "", geomi)
			geomi    = gsub("\\), \\(", "\\),\\(", geomi)
			geomi    = strsplit(geomi, "\\),\\(")[[1]]		 
			pstart   = c()
			for ( u in geomi)
			{
				pstart = c(pstart, dim(m)[1])
				r = poly.pars(u)
				m = rbind(m,r)
			}
			verts    = m
			bbox     = c(min(m[,1]), min(m[,2]), max(m[,1]), max(m[,2]))
			geomdim  = c(dim(m)[1], 2)
			nParts   = length(geomi)
        }		
		
	    if (typ == "POLYGON" | typ == "MULTIPOLYGON") {
	        shptype  = "poly"
	        shptypen = 5
	        geomi    = sub(paste(typ, " " , sep=""), "", geomi)
	        geomi    = sub(typ, "", geomi)
			geomi    = gsub("\\), \\(", "\\),\\(", geomi)
			geomi    = strsplit(geomi, "\\),\\(")[[1]]		 
			pstart   = c()
			for ( u in geomi)
			{
				pstart = c(pstart, dim(m)[1])
				r = poly.pars(u)
				m = rbind(m,r)
			}
			verts    = m
			bbox     = c(min(m[,1]), min(m[,2]), max(m[,1]), max(m[,2]))
			geomdim  = c(dim(m)[1], 2)
			nParts   = length(geomi)
	    }
		
		shp.obj                   = list()
		shp.obj$Pstart            = pstart
		shp.obj$verts             = verts
		shp.obj$shp.type          = as.integer(shptypen)
		shp.obj$nVerts            = as.integer(geomdim[1])
		shp.obj$nParts            = as.integer(nParts)
		shp.obj$bbox              = bbox
		shp.obj$shpID             = as.integer(i)
		attr(shp.obj, "nVerts")   = geomdim[1]
		attr(shp.obj, "nParts")   = nParts
		attr(shp.obj, "shp.type") = shptypen
		attr(shp.obj, "bbox")     = bbox
		shps[[i]]                 = shp.obj
   }

   attr(shps, "shp.type")    = shptype
   attr(shps, "nshps")       = i
   attr(shps, "minbb")       = c(min(geoms[,'minx']), min(geoms[,'miny']), 0, 0)
   attr(shps, "maxbb")       = c(max(geoms[,'maxx']), max(geoms[,'maxy']), 0, 0)
   class(shps)               = "ShapeList"

   ftrs = list()
   ftrs$Shapes = shps
   class(ftrs) = "Map"
   return(ftrs)
}

sqli2sp = function(geoms, gcol, idcol)
{   
	mt = sub(' ', '', strsplit(geoms[1,gcol], '\\(')[[1]][1])
	
	if (mt == "POINT")
	{
		xc = c()
		yc = c()
		for (geomi in geoms[,gcol])
		{
			xc = c(xc, pont.pars(geomi)[1])
			yc = c(yc, pont.pars(geomi)[2])
		}
		xy.sp = SpatialPoints(cbind(xc,yc))		
		return(xy.sp)
	}	
	
	if (mt == "LINESTRING" | mt == "MULTILINESTRING")
	{
		ids = as.character(geoms[,idcol])
		shps = list()
		n = 1
		for(i in 1:dim(geoms)[1])
		{
			geomi = geoms[i,gcol]
			typ = sub(' ', '', strsplit(geomi, '\\(')[[1]][1])
			geom.i = as.character(geomi)
		  
			if (typ == "LINESTRING") 
			{
				plist = list()
				geomi = sub(paste(typ, " " , sep=""), "", geomi); geomi = sub(typ, "", geomi)         
				geomi = gsub("\\), \\(", "\\),\\(", geomi)
				geomi = strsplit(geomi, "\\),\\(")[[1]]		 
				for (u in geomi)
				{
					m = poly.pars(u)
					plist[[1]] = Line(m)			   
				}
				srs = Lines(plist, ID= ids[i])
				shps[[n]] = srs
				n = n+1
			}	
			if (typ == "MULTILINESTRING") 
			{		 
				geomi = sub(paste(typ, " " , sep=""), "", geomi); geomi = sub(typ, "", geomi)      
				geomi = gsub("\\), \\(", "\\),\\(", geomi)
				polys = strsplit(geomi, "\\)\\),\\(\\(")[[1]]	
				plist = list()
				p = 1
				for (poly in polys)
				{		 			
					poly = strsplit(poly, "\\),\\(")[[1]]						
					for (u in poly)
					{
						m = poly.pars(u)
						plist[[p]] = Line(m)
						p = p+1
					} 						
				}
				srs = Lines(plist, ID= ids[i])
				shps[[n]] = srs
				n = n+1		 
			}
			
		}
		SpP = SpatialLines(shps)
		return(SpP)
	}	
	
	if (mt == "POLYGON" | mt == "MULTIPOLYGON")
	{
		ids = geoms[,idcol]
		shps = list()
		n = 1
		for(i in 1:dim(geoms)[1])
		{
			geomi = geoms[i,gcol]
			typ = sub(' ', '', strsplit(geomi, '\\(')[[1]][1])
			geom.i = as.character(geomi)			
		  
			if (typ == "POLYGON") 
			{
				plist = list()
				geomi = sub(paste(typ, " " , sep=""), "", geomi); geomi = sub(typ, "", geomi)         
				geomi = gsub("\\), \\(", "\\),\\(", geomi)
				geomi = strsplit(geomi, "\\),\\(")[[1]]		 
				j = 1
				for (u in geomi)
				{
					m = poly.pars(u)
					if (j==1) plist[[j]] = Polygon(m)
					if (j==2) plist[[j]] = Polygon(m, hole=TRUE)		   
					j = j+1
				}
				srs = Polygons(plist, ids[i])
				shps[[n]] = srs
				n = n+1
			}	  
		  
			if (typ == "MULTIPOLYGON") 
			{		 
				geomi = sub(paste(typ, " " , sep=""), "", geomi); geomi = sub(typ, "", geomi)      
				geomi = gsub("\\), \\(", "\\),\\(", geomi)
				polys = strsplit(geomi, "\\)\\),\\(\\(")[[1]]	
				plist = list()
				p = 1
				for (poly in polys)
				{		 			
					j = 1
					poly = strsplit(poly, "\\),\\(")[[1]]						
					for (u in poly)
					{
						m = poly.pars(u)
						if (j==1) plist[[p]] = Polygon(m)
						if (j==2) plist[[p]] = Polygon(m, hole=TRUE)
						j = j+1
						p = p+1
					} 						
				}
				srs = Polygons(plist, ids[i])
				shps[[n]] = srs
				n = n+1		 
			}		  	  
		}
		SpP = SpatialPolygons(shps, 1:length(shps))
		return(SpP)
	}
}

pont.pars = function(geomi)
{
	geomi = sub(paste("POINT " , sep=""), "", geomi)
	geomi = sub("POINT", "", geomi) 
	geomi = gsub("\\(", "", geomi)
	geomi = gsub("\\)", "", geomi)	
	geomi = strsplit(geomi, " ")   
	geomi = as.numeric(unlist(splsp(geomi[[1]])))
	return(geomi)
}

poly.pars = function(u)
{
	u = gsub("\\(", "", u)
	u = gsub("\\)", "", u)
	u = gsub(", ", ",", u)
	u = strsplit(u, ",")   
	u = splsp(u[[1]])
	x = as.numeric(unlist(u)[seq(1,length(unlist(u)),2)])
	y = as.numeric(unlist(u)[seq(2,length(unlist(u)),2)])   
	m = cbind(x,y)
	m = m[is.na(m[, 2]) == FALSE, ]
	m = m[is.na(m[, 1]) == FALSE, ]   
	return(m)
}

splsp = function(x) strsplit(x, " ")

spdfho = function(SrDf)
{
	if (class(SrDf)[1] == "SpatialPolygonsDataFrame")
	{
		lukas = c()
		for (i in 1:length(SrDf@polygons))
		{
			ps = SrDf@polygons[i][[1]]
			for (p in ps@Polygons)
			{
				if(p@hole==TRUE) lukas = c(lukas, i)
			}
		}
		SrDf@plotOrder = c(lukas,SrDf@plotOrder[-lukas])
		return(SrDf)
	}
}

sqli.dump = function(db, mapobj, mn)
{
	drv = dbDriver("SQLite")
	con = dbConnect(drv, dbname = db)
	
	sql = paste("select name from sqlite_master where name = '", mn,"' order by name", sep='')
	rs = dbSendQuery(con, sql)
	tnames = fetch(rs, n = -1)
	
	if (dim(tnames)[1]==0)
	{
		mapsql = paste('create table', mn, '(gid NUMERIC, geom TEXT, minx NUMERIC, miny NUMERIC, maxx NUMERIC, maxy NUMERIC)', sep=' ')
		obtyp = class(mapobj)[[1]]
		
		if (obtyp == "SpatialPoints")
		{		
			rs = dbSendQuery(con, mapsql)
			for(i in 1:summary(mapobj)$npoints)
			{
				mo = mapobj[i]
				sql = paste('INSERT INTO ', mn,' values(',i, ',',
					"'POINT(", as.numeric(coordinates(mo)[,1]), ' ', as.numeric(coordinates(mo)[,2]), ")'", 
					',', bbox(mo)[1,1], ',', bbox(mo)[2,1], ',', 
					bbox(mo)[1,2], ',', bbox(mo)[2,2], ')',
					sep = '')
				rs = dbSendQuery(con, sql)
			}
		}
		
		if (obtyp == "SpatialPointsDataFrame")
		{
			rs = dbSendQuery(con, mapsql)
			for(i in 1:summary(mapobj)$npoints)
			{
				mo = mapobj[i,]
				sql = paste('INSERT INTO ', mn,' values(',i, ',',
					"'POINT(", as.numeric(coordinates(mo)[,1]), ' ', as.numeric(coordinates(mo)[,2]), ")'", 
					',', bbox(mo)[1,1], ',', bbox(mo)[2,1], ',', 
					bbox(mo)[1,2], ',', bbox(mo)[2,2], ')',
					sep = '')
				rs = dbSendQuery(con, sql)
			}	
			attr.wrt(db, mapobj, mn)
		}
		
		if (obtyp == "SpatialLines")
		{
			mapsql = paste('create table', mn, '(gid NUMERIC, ID TEXT, geom TEXT, minx NUMERIC, miny NUMERIC, maxx NUMERIC, maxy NUMERIC)', sep=' ')
			rs = dbSendQuery(con, mapsql)
			
			for(i in 1:length(mapobj@lines))
			{
				mo = mapobj@lines[[i]]
				if (length(coordinates(mo))==1)
				{
					minx = min(coordinates(mo)[[1]][,1])
					maxx = max(coordinates(mo)[[1]][,1])
					miny = min(coordinates(mo)[[1]][,2])
					maxy = max(coordinates(mo)[[1]][,2])	
					sql = paste('INSERT INTO ', mn,' values(',i , ", '", mo@ID,"',",
						"'LINESTRING(", as.numeric(coordinates(mo)[[1]][1,1]), ' ', as.numeric(coordinates(mo)[[1]][1,2]), sep='')
					for (j in 1:dim(coordinates(mo)[[1]])[1])
					{
						sql = paste(sql, ', ', as.numeric(coordinates(mo)[[1]][j,1]), ' ', as.numeric(coordinates(mo)[[1]][j,2]), sep='')
					}
					sql = paste(sql, ")', ", minx, ', ', miny, ', ', maxx, ', ', maxy, ')', sep='')
				}
				rs = dbSendQuery(con, sql)
			}	
		}		
		
		if (obtyp == "SpatialLinesDataFrame")
		{
			mapsql = paste('create table', mn, '(gid NUMERIC, ID TEXT, geom TEXT, minx NUMERIC, miny NUMERIC, maxx NUMERIC, maxy NUMERIC)', sep=' ')
			rs = dbSendQuery(con, mapsql)
			
			for(i in 1:length(mapobj@lines))
			{
				mo = mapobj@lines[[i]]
				if (length(coordinates(mo))==1)
				{
					minx = min(coordinates(mo)[[1]][,1])
					maxx = max(coordinates(mo)[[1]][,1])
					miny = min(coordinates(mo)[[1]][,2])
					maxy = max(coordinates(mo)[[1]][,2])	
					sql = paste('INSERT INTO ', mn,' values(',i , ", '", mo@ID,"',",
						"'LINESTRING(", as.numeric(coordinates(mo)[[1]][1,1]), ' ', as.numeric(coordinates(mo)[[1]][1,2]), sep='')
					for (j in 1:dim(coordinates(mo)[[1]])[1])
					{
						sql = paste(sql, ', ', as.numeric(coordinates(mo)[[1]][j,1]), ' ', as.numeric(coordinates(mo)[[1]][j,2]), sep='')
					}
					sql = paste(sql, ")', ", minx, ', ', miny, ', ', maxx, ', ', maxy, ')', sep='')
				}
				rs = dbSendQuery(con, sql)
			}	
			attr.wrt(db, mapobj, mn)
		}	

		if (obtyp == "SpatialPolygons")
		{
			mapsql = paste('create table', mn, '(gid NUMERIC, ID TEXT, geom TEXT, minx NUMERIC, miny NUMERIC, maxx NUMERIC, maxy NUMERIC)', 
			sep=' ')
			rs = dbSendQuery(con, mapsql)
			
			for(i in 1:length(mapobj@polygons))
			{
				minx = slot(mapobj[i], 'bbox')[1,1]
				maxx = slot(mapobj[i], 'bbox')[1,2]
				miny = slot(mapobj[i], 'bbox')[2,1]
				maxy = slot(mapobj[i], 'bbox')[2,2]
				mo = mapobj@polygons[[i]]
				id = slot(mo, 'ID')
				
				if (length(slot(mo, "Polygons"))==1)
				{
					m = slot(slot(mo, 'Polygons')[[1]], 'coords')
					poli = paste("'POLYGON ((", m[1,1], " ", m[1,2], sep='')
					for(j in 2:dim(m)[1])
					{
						poli = paste(poli, ', ', m[j,1], " ", m[j,2], sep='')
					}
					poli = paste(poli, "))'", sep='')
				}
				
				if (length(slot(mo, "Polygons"))>1)
				{	
					polys = list()
					holes = list()
					for(n in 1:length(slot(mo, "Polygons")))
					{
						nm = slot(mo, 'Polygons')[[n]]
						m = slot(nm, 'coords')
						pol = paste("(", m[1,1], " ", m[1,2], sep='')
						for(j in 2:dim(m)[1])
						{
							pol = paste(pol, ', ', m[j,1], " ", m[j,2], sep='')
						}
						pol = paste(pol, ")", sep='')
						polys[[n]] = pol
						holes[[n]] = slot(nm, 'hole')
					}
					
					if (sum(unlist(holes)==TRUE)==0)
					{
						poli = "'MULTIPOLYGON (("
						for(p in polys)
						{
							poli = paste(poli, p, '), (', sep='')
						}
						poli = paste(substr(poli, 1, nchar(poli)-3), ")'", sep='')
					} else {
						holpol = list()
						polyb = unlist(polys)
						hs = which(unlist(holes)==TRUE)
						for(h in hs)
						{
							holpol[[length(holpol)+1]] = paste("(", polys[[h-1]], ", " , polys[[h]], ")", sep='')
							polyb = polyb[-c(h-1,h)]
						}
						if (length(holpol)==1 & length(polyb)==0)
						{
							poli = paste("'POLYGON ", holpol[[1]], "'", sep='')
						}
						if (length(holpol)==1 & length(polyb)>0)
						{
							poli = "'MULTIPOLYGON ("
							for(a in holpol)
							{
								poli = paste(poli, a, ', ', sep='')
							}
							for(v in polyb)
							{
								poli = paste(poli, '(', v, '), ', sep='')
							}
							poli = paste(substr(poli, 1, nchar(poli)-2), ")'", sep='')
						}						
						if (length(holpol)>1 & length(polyb)==0)
						{
							poli = "'MULTIPOLYGON ("
							for(a in holpol)
							{
								poli = paste(poli, a, ', ', sep='')
							}
							poli = paste(substr(poli, 1, nchar(poli)-2), ")'", sep='')
						}	
						if (length(holpol)>1 & length(polyb)>0)
						{
							poli = "'MULTIPOLYGON ("
							for(a in holpol)
							{
								poli = paste(poli, a, ', ', sep='')
							}
							for(v in polyb)
							{
								poli = paste(poli, v, ', ', sep='')
							}
							poli = paste(substr(poli, 1, nchar(poli)-2), ")'", sep='')
						}						
					}
				}				
			sql = paste('INSERT INTO ', mn,' values(',i , ", '", id, "', ", poli, ', ', minx, ', ', miny, ', ', maxx, ', ', maxy, ')', sep='')
			rs = dbSendQuery(con, sql)
			}
		}
		
		if (obtyp == "SpatialPolygonsDataFrame")
		{
			mapsql = paste('create table', mn, '(gid NUMERIC, ID TEXT, geom TEXT, minx NUMERIC, miny NUMERIC, maxx NUMERIC, maxy NUMERIC)', 
			sep=' ')
			rs = dbSendQuery(con, mapsql)
			
			for(i in 1:length(mapobj@polygons))
			{				
				mo = mapobj@polygons[[i]]
				id = slot(mo, 'ID')
				
				mm = coordinates(slot(mo, 'Polygons')[[1]])
				if (length(slot(mo, 'Polygons'))>1)
				{
					for (e in 2:length(slot(mo, 'Polygons')))
					{
						mm = rbind(mm,coordinates(slot(mo, 'Polygons')[[1]]))
					}
				}
				minx = min(mm[,1])
				maxx = max(mm[,1])
				miny = min(mm[,2])
				maxy = max(mm[,2])				
				
				if (length(slot(mo, "Polygons"))==1)
				{
					m = slot(slot(mo, 'Polygons')[[1]], 'coords')
					poli = paste("'POLYGON ((", m[1,1], " ", m[1,2], sep='')
					for(j in 2:dim(m)[1])
					{
						poli = paste(poli, ', ', m[j,1], " ", m[j,2], sep='')
					}
					poli = paste(poli, "))'", sep='')
				}
				
				if (length(slot(mo, "Polygons"))>1)
				{	
					polys = list()
					holes = list()
					for(n in 1:length(slot(mo, "Polygons")))
					{
						nm = slot(mo, 'Polygons')[[n]]
						m = slot(nm, 'coords')
						pol = paste("(", m[1,1], " ", m[1,2], sep='')
						for(j in 2:dim(m)[1])
						{
							pol = paste(pol, ', ', m[j,1], " ", m[j,2], sep='')
						}
						pol = paste(pol, ")", sep='')
						polys[[n]] = pol
						holes[[n]] = slot(nm, 'hole')
					}
					
					if (sum(unlist(holes)==TRUE)==0)
					{
						poli = "'MULTIPOLYGON (("
						for(p in polys)
						{
							poli = paste(poli, p, '), (', sep='')
						}
						poli = paste(substr(poli, 1, nchar(poli)-3), ")'", sep='')
					} else {
						holpol = list()
						polyb = unlist(polys)
						hs = which(unlist(holes)==TRUE)
						for(h in hs)
						{
							holpol[[length(holpol)+1]] = paste("(", polys[[h-1]], ", " , polys[[h]], ")", sep='')
							polyb = polyb[-c(h-1,h)]
						}
						if (length(holpol)==1 & length(polyb)==0)
						{
							poli = paste("'POLYGON ", holpol[[1]], "'", sep='')
						}
						if (length(holpol)==1 & length(polyb)>0)
						{
							poli = "'MULTIPOLYGON ("
							for(a in holpol)
							{
								poli = paste(poli, a, ', ', sep='')
							}
							for(v in polyb)
							{
								poli = paste(poli, '(', v, '), ', sep='')
							}
							poli = paste(substr(poli, 1, nchar(poli)-2), ")'", sep='')
						}						
						if (length(holpol)>1 & length(polyb)==0)
						{
							poli = "'MULTIPOLYGON ("
							for(a in holpol)
							{
								poli = paste(poli, a, ', ', sep='')
							}
							poli = paste(substr(poli, 1, nchar(poli)-2), ")'", sep='')
						}	
						if (length(holpol)>1 & length(polyb)>0)
						{
							poli = "'MULTIPOLYGON ("
							for(a in holpol)
							{
								poli = paste(poli, a, ', ', sep='')
							}
							for(v in polyb)
							{
								poli = paste(poli, v, ', ', sep='')
							}
							poli = paste(substr(poli, 1, nchar(poli)-2), ")'", sep='')
						}						
					}
				}				
			sql = paste('INSERT INTO ', mn,' values(',i , ", '", id, "', ", poli, ', ', minx, ', ', miny, ', ', maxx, ', ', maxy, ')', sep='')
			rs = dbSendQuery(con, sql)
			}
			attr.wrt(db, mapobj, mn)
		}		
		
		if (obtyp == "Map")
		{
			if (attr(mapobj$Shapes,"shp.type")=="point" | attr(mapobj$Shapes,"shp.type")=="poly")
			{
				mapsql = paste('create table', mn, '(gid NUMERIC, geom TEXT, minx NUMERIC, miny NUMERIC, maxx NUMERIC, maxy NUMERIC)', 
				sep=' ')
				rs = dbSendQuery(con, mapsql)			
				
				if (attr(mapobj$Shapes, "shp.type")=="point")
				{
					for(shp in mapobj$Shapes)
					{
						id = shp$shpID
						minx = shp$bbox[1]
						miny = shp$bbox[2]
						maxx = shp$bbox[3]
						maxy = shp$bbox[4]		
						
						ftr = paste("'POINT (", shp$verts[,1], ' ', shp$verts[,2], ")'", sep='')
						
						sql = paste('INSERT INTO ', mn,' values(', id, ', ', ftr, ', ', minx, ', ', miny, ', ', maxx, ', ', maxy, ')', sep='')
					}
				}
				if (attr(mapobj$Shapes, "shp.type")=="poly")
				{
					for(shp in mapobj$Shapes)
					{
						id = shp$shpID
						minx = shp$bbox[1]
						miny = shp$bbox[2]
						maxx = shp$bbox[3]
						maxy = shp$bbox[4]	
						ps = shp$Pstart+1
						if(is.null(shp$Pstart)==TRUE)
						{
							m = shp$verts
							ftr = paste("'POLYGON ((", m[1,1], ' ', m[1,2], sep='')
							for (i in 2:dim(m)[1])
							{
								ftr = paste(ftr, ', ', m[i,1], ' ', m[i,2], sep='')
							}
							ftr = paste(ftr, "))'", sep='')
						}
						
						sql = paste('INSERT INTO ', mn,' values(', id, ', ', ftr, ', ', minx, ', ', miny, ', ', maxx, ', ', maxy, ')', sep='')
						rs = dbSendQuery(con, sql)
					}
				}
				
				
			}
		}
		
	} else {
		warning(paste("Database has already ", mn, " table!", sep=''))
	}
}

attr.wrt = function(db, mapobj, mn)
{
	drv = dbDriver("SQLite")
	con = dbConnect(drv, dbname = db)
	
	df = as.data.frame(mapobj)
	for (q in colnames(coordinates(mapobj)))
	{
		df = df[,-which(colnames(df)==q)]
	}
	dtsql = paste('create table ', mn, 'dt (gid NUMERIC ', sep='')
	for (o in 1:dim(df)[2])
	{
		if(is.numeric(df[,o])==TRUE) sqs = paste(', ', colnames(df)[o], ' NUMERIC', sep='')
		if(is.numeric(df[,o])!=TRUE) sqs = paste(', ', colnames(df)[o], ' TEXT', sep='')
		dtsql = paste(dtsql, sqs, sep='')
	}
	dtsql = paste(dtsql, ')', sep='')
	rs = dbSendQuery(con, dtsql)
	for (i in 1:dim(df)[1])
	{
		dtsql = paste('insert into ', mn, 'dt values(', i, sep='')
		for (o in 1:dim(df)[2])
		{
			dtsql = paste(dtsql, ',', df[i,o], sep='')					
		}
		dtsql = paste(dtsql, ')', sep='')
		rs = dbSendQuery(con, dtsql)
	}
}



