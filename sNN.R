library(data.table)
library(cluster)
#install.packages('dismo')
# install.packages('deldir')
library(deldir)
library(dismo)
library(rgeos)
library(rgdal)
#library(dbscan)
library(leaflet)
library(geosphere)
library(MetroTransitr)
#install.packages('readxl')
library(readxl)
library(RANN)
library(igraph)


# download stops from shapefile for most up to date info
stopsURL = 'ftp://ftp.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/trans_transit_stops/shp_trans_transit_stops.zip'
# download street segments to temp dir
loc <- file.path(tempdir(), 'stops.zip')
download.file(stopsURL, loc)
unzip(loc, exdir = file.path(tempdir(), 'stops'), overwrite = TRUE)
file.remove(loc)
stopsshp <- readOGR(file.path(tempdir(), 'stops'), layer = 'TransitStops', stringsAsFactors = FALSE)
stopshp <- subset(stopsshp, busstop_yn == 'Y')

stopgeo <- spTransform(stopshp, CRS("+proj=longlat +datum=WGS84"))

stations <- grep('Station', stopgeo@data$site_on)
stopcoords <- data.table(stopgeo@coords)
stopcoords[, site_id := stopgeo@data$site_id]
stopcoords[, idx := .I]

dist_mat <- distm(stopcoords[, .(coords.x1, coords.x2)], stopcoords[, .(coords.x1, coords.x2)], fun = distHaversine)

snn_10_5_1 <- sNNclust(dist_mat, k = 10, eps = 5, minPts = 1)
stopcoords[, V2 := NULL]
stopcoords <- cbind(stopcoords, snn_10_5_1$cluster)

hull_list <- lapply(stopcoords[V2 != 0, unique(V2)], 
                    function(x) {
                      #print(x)
                      gConvexHull(SpatialPoints(coords = stopcoords[V2 == x, .(coords.x1, coords.x2)], proj4string = CRS('+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs')), id = paste0(x))
                    })

hull_polys <- hull_list[sapply(hull_list, function(x) class(x) == 'SpatialPolygons')]
hull_polys <- do.call(rbind, hull_polys)

hull_lines <- hull_list[sapply(hull_list, function(x) class(x) == 'SpatialLines')]
hull_lines <- do.call(rbind, hull_lines)

hull_points <- hull_list[sapply(hull_list, function(x) class(x) == 'SpatialPoints')]
hull_points <- do.call(rbind, hull_points)

pal = colorNumeric(palette = 'Dark2', domain = stopcoords[, V2])
leaflet()%>%
  addTiles()%>%
  addCircleMarkers(data = stopcoords, lng = ~coords.x1, lat = ~coords.x2, fillColor = ~pal(V2), stroke = FALSE, radius = 5, fillOpacity = 1) %>%
  addPolygons(data = hull_polys, weight = 2, fillOpacity = 0)

snn_10_7_1 <- sNNclust(dist_mat, k = 10, eps = 7, minPts = 1)
stopcoords[, V2 := NULL]
stopcoords <- cbind(stopcoords, snn_10_7_1$cluster)

hull_list <- lapply(stopcoords[V2 != 0, unique(V2)], 
                    function(x) {
                      #print(x)
                      gConvexHull(SpatialPoints(coords = stopcoords[V2 == x, .(coords.x1, coords.x2)], proj4string = CRS('+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs')), id = paste0(x))
                    })

hull_polys <- hull_list[sapply(hull_list, function(x) class(x) == 'SpatialPolygons')]
hull_polys <- do.call(rbind, hull_polys)

hull_lines <- hull_list[sapply(hull_list, function(x) class(x) == 'SpatialLines')]
hull_lines <- do.call(rbind, hull_lines)

hull_points <- hull_list[sapply(hull_list, function(x) class(x) == 'SpatialPoints')]
hull_points <- do.call(rbind, hull_points)

pal = colorNumeric(palette = 'Dark2', domain = stopcoords[, V2])
leaflet()%>%
  addTiles()%>%
  addCircleMarkers(data = stopcoords, lng = ~coords.x1, lat = ~coords.x2, fillColor = ~pal(V2), stroke = FALSE, radius = 5, fillOpacity = 1) %>%
  addPolygons(data = hull_polys, weight = 2, fillOpacity = 0)

#Don't love the fact that this algorithm gives weird shapes and then ttaz get assigned based on voronoi diagram.
#Want some way to grow convex hulls into a partition of space
centers <- gCentroid(hull_polys, byid = TRUE)
ttaz <- voronoi(centers)

pal = colorNumeric(palette = 'Dark2', domain = stopcoords[, V2])
leaflet()%>%
  addTiles()%>%
  addCircleMarkers(data = stopcoords, lng = ~coords.x1, lat = ~coords.x2, fillColor = ~pal(V2), stroke = FALSE, radius = 5, fillOpacity = 1) %>%
  addPolygons(data = ttaz, weight = 2, fillOpacity = 0)
