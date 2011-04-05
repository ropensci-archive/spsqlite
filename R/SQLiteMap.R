sqli2sp = function(geoms, gcol, idcol)
{   
   mt = sub(' ', '', strsplit(geoms[1,gcol], '\\(')[[1]][1])
   
   if (mt == "POINT")
   {
      xc = c()
      yc = c()
      for (geomi in geoms[,gcol])
      {
         xc = c(xc, .pont.pars(geomi)[1])
         yc = c(yc, .pont.pars(geomi)[2])
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
               m = .poly.pars(u)
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
                  m = .poly.pars(u)
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
               m = .poly.pars(u)
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
                  m = .poly.pars(u)
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

.pont.pars = function(geomi)
{
   geomi = sub(paste("POINT " , sep=""), "", geomi)
   geomi = sub("POINT", "", geomi) 
   geomi = gsub("\\(", "", geomi)
   geomi = gsub("\\)", "", geomi)   
   geomi = strsplit(geomi, " ")   
   geomi = as.numeric(unlist(.splsp(geomi[[1]])))
   return(geomi)
}

.poly.pars = function(u)
{
   u = gsub("\\(", "", u)
   u = gsub("\\)", "", u)
   u = gsub(", ", ",", u)
   u = strsplit(u, ",")   
   u = .splsp(u[[1]])
   x = as.numeric(unlist(u)[seq(1,length(unlist(u)),2)])
   y = as.numeric(unlist(u)[seq(2,length(unlist(u)),2)])   
   m = cbind(x,y)
   m = m[is.na(m[, 2]) == FALSE, ]
   m = m[is.na(m[, 1]) == FALSE, ]   
   return(m)
}

.splsp = function(x) strsplit(x, " ")

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
   sql = paste("select name from sqlite_master where name = '", mn,"' order by name", sep='')
   rs = dbSendQuery(db, sql)
   tnames = fetch(rs, n = -1)
   dbHasCompleted(rs)
   dbClearResult(rs)   
   if (dim(tnames)[1]==0)
   {
      dbBeginTransaction(db)
      mapsql = paste('create table', mn, '(PK_UID INTEGER PRIMARY KEY AUTOINCREMENT, geom TEXT)', sep=' ')
      obtyp = class(mapobj)[[1]]
      
      if (obtyp == "SpatialPoints")
      {      
         rs = dbSendQuery(db, mapsql)
         dbHasCompleted(rs)
         dbClearResult(rs)
         for(i in 1:summary(mapobj)$npoints)
         {
            mo = mapobj[i]
            sql = paste('INSERT INTO ', mn,' values(',i, ',',
               "'POINT(", as.numeric(coordinates(mo)[,1]), ' ', as.numeric(coordinates(mo)[,2]), ")')", sep = '')
            rs = dbSendQuery(db, sql)
            dbHasCompleted(rs)
            dbClearResult(rs)
         }
      }
      
      if (obtyp == "SpatialPointsDataFrame")
      {
         rs = dbSendQuery(db, mapsql)
         dbHasCompleted(rs)
         dbClearResult(rs)
         for(i in 1:summary(mapobj)$npoints)
         {
            mo = mapobj[i,]
            sql = paste('INSERT INTO ', mn,' values(',i, ',',
               "'POINT(", as.numeric(coordinates(mo)[,1]), ' ', as.numeric(coordinates(mo)[,2]), ")')", sep = '')
            rs = dbSendQuery(db, sql)
            dbHasCompleted(rs)
            dbClearResult(rs)
         }   
         .attr.wrt(db, mapobj, mn)
      }
      
      if (obtyp == "SpatialLines")
      {
         mapsql = paste('create table', mn, '(PK_UID INTEGER PRIMARY KEY AUTOINCREMENT, ID TEXT, geom TEXT)', sep=' ')
         rs = dbSendQuery(db, mapsql)
         dbHasCompleted(rs)
         dbClearResult(rs)
         for(i in 1:length(mapobj@lines))
         {
            mo = mapobj@lines[[i]]
            if (length(coordinates(mo))==1)
            {
               sql = paste('INSERT INTO ', mn,' values(',i , ", '", mo@ID,"',",
                  "'LINESTRING(", as.numeric(coordinates(mo)[[1]][1,1]), ' ', as.numeric(coordinates(mo)[[1]][1,2]), sep='')
               for (j in 1:dim(coordinates(mo)[[1]])[1])
               {
                  sql = paste(sql, ', ', as.numeric(coordinates(mo)[[1]][j,1]), ' ', as.numeric(coordinates(mo)[[1]][j,2]), sep='')
               }
               sql = paste(sql, ")')", sep='')
            }
            rs = dbSendQuery(db, sql)
            dbHasCompleted(rs)
            dbClearResult(rs)
         }   
      }      
      
      if (obtyp == "SpatialLinesDataFrame")
      {
         mapsql = paste('create table', mn, '(PK_UID INTEGER PRIMARY KEY AUTOINCREMENT, ID TEXT, geom TEXT)', sep=' ')
         rs = dbSendQuery(db, mapsql)
         dbHasCompleted(rs)
         dbClearResult(rs)
        
         for(i in 1:length(mapobj@lines))
         {
            mo = mapobj@lines[[i]]
            if (length(coordinates(mo))==1)
            {
               sql = paste('INSERT INTO ', mn,' values(',i , ", '", mo@ID,"',",
                  "'LINESTRING(", as.numeric(coordinates(mo)[[1]][1,1]), ' ', as.numeric(coordinates(mo)[[1]][1,2]), sep='')
               for (j in 1:dim(coordinates(mo)[[1]])[1])
               {
                  sql = paste(sql, ', ', as.numeric(coordinates(mo)[[1]][j,1]), ' ', as.numeric(coordinates(mo)[[1]][j,2]), sep='')
               }
               sql = paste(sql, ")')", sep='')
            }
            rs = dbSendQuery(db, sql)
            dbHasCompleted(rs)
            dbClearResult(rs)
         }   
         .attr.wrt(db, mapobj, mn)
      }   

      if (obtyp == "SpatialPolygons")
      {
         mapsql = paste('create table', mn, '(PK_UID INTEGER PRIMARY KEY AUTOINCREMENT, ID TEXT, geom TEXT)', sep=' ')
         rs = dbSendQuery(db, mapsql)
         dbHasCompleted(rs)
         dbClearResult(rs)
         
         for(i in 1:length(mapobj@polygons))
         {
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
            sql = paste('INSERT INTO ', mn,' values(',i , ", '", id, "', ", poli, ')', sep='')
            rs = dbSendQuery(db, sql)
            dbHasCompleted(rs)
            dbClearResult(rs)
         }
      }
      
      if (obtyp == "SpatialPolygonsDataFrame")
      {
         mapsql = paste('create table', mn, '(PK_UID INTEGER PRIMARY KEY AUTOINCREMENT, ID TEXT, geom TEXT)', 
         sep=' ')
         rs = dbSendQuery(db, mapsql)
         dbHasCompleted(rs)
         dbClearResult(rs)
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
                     tipusN = tipusN + 1
                  }                  
               }
            }            
            sql = paste('INSERT INTO ', mn,' values(',i , ", '", id, "', ", poli, ')', sep='')
            rs = dbSendQuery(db, sql)
            dbHasCompleted(rs)
            dbClearResult(rs)
         }
         .attr.wrt(db, mapobj, mn)                   
      }
      dbCommit(db)
   } else {
      warning(paste("Database has already ", mn, " table!", sep=''))
   }
}

.attr.wrt = function(db, mapobj, mn)
{      
   df = as.data.frame(mapobj)
   for (q in colnames(coordinates(mapobj)))
   {
      df = df[,-which(colnames(df)==q)]
   }
   dtsql = paste('create table ', mn, 'dt (PK_UID INTEGER PRIMARY KEY AUTOINCREMENT ', sep='')
   for (o in 1:dim(df)[2])
   {
      if(is.numeric(df[,o])==TRUE) sqs = paste(', ', colnames(df)[o], ' NUMERIC', sep='')
      if(is.numeric(df[,o])!=TRUE) sqs = paste(', ', colnames(df)[o], ' TEXT', sep='')
      dtsql = paste(dtsql, sqs, sep='')
   }
   dtsql = paste(dtsql, ')', sep='')
   rs = dbSendQuery(db, dtsql)
   dbHasCompleted(rs)
   dbClearResult(rs)
   for (i in 1:dim(df)[1])
   {
      dtsql = paste('insert into ', mn, 'dt values(', i, sep='')
      for (o in 1:dim(df)[2])
      {
         dtsql = paste(dtsql, ',', df[i,o], sep='')               
      }
      dtsql = paste(dtsql, ')', sep='')
      rs = dbSendQuery(db, dtsql)
      dbHasCompleted(rs)
      dbClearResult(rs)
   }   
}


spatialite.init = function(con){
   res = dbGetQuery(con, paste("SELECT load_extension('", getLoadedDLLs()$SQLiteMap[['path']], "')", sep=''))
}

