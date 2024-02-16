# Install/load packages
packages <- c("raster", "rgdal", "htmltab")
for(i in 1:NROW(packages)){
  if(!require(packages[i], character.only = TRUE)){
    install.packages(packages[i])
    library(packages[i], character.only = TRUE)
  }
}
install.packages("raster")
install.packages("rgdal")
install.packages("remotes")
remotes::install_github("htmltab/htmltab")

library(sp)
library(raster)
library(rgdal)
library(htmltab)

# file paths
GPS_logs_path <- "C:/Users/uwpnc/Downloads/tracker_data.xls"
blocks_path <- "./data/shapefiles/LELEPlotOutlines.shp"
borders_path <- "./data/shapefiles/LapalalaBorders.shp"
mainroads_path <- "./data/shapefiles/MainRoads.shp"
out_path <- "C:/Users/uwpnc/Downloads/GPS_Tags.pdf"

# colours
cols <- c(
  rgb(0, 150, 130, alpha = 254.99, maxColorValue = 255), #kit colour
  rgb(70, 100, 170, alpha = 254.99, maxColorValue = 255), #kit blue
  rgb(223, 155, 27, alpha = 254.99, maxColorValue = 255), #kit orange
  rgb(140, 182, 60, alpha = 254.99, maxColorValue = 255), #kit Mai green
  rgb(162, 34, 35, alpha = 254.99, maxColorValue = 255), #kit red
  rgb(35, 161, 224, alpha = 254.99, maxColorValue = 255),
  rgb(163, 16, 124, alpha = 254.99, maxColorValue = 255), #kit violet
  rgb(167, 130, 46, alpha = 254.99, maxColorValue = 255) #kit braun
)

# read data
GPS_html <- htmltab::htmltab(GPS_logs_path, which = 1)
GPS_html$Latitude <- as.numeric(sub("Â°", "", GPS_html$Latitude))
GPS_html$Longitude <- as.numeric(sub("Â°", "", GPS_html$Longitude))

GPS_locs <- sp::SpatialMultiPoints(split(GPS_html[, c("Longitude", "Latitude")],
                                         seq(nrow(GPS_html))))
GPS_tags <- sp::SpatialPointsDataFrame(GPS_locs, data = GPS_html[, c("Index", "Tag", "Time Stamp")])
names(GPS_tags)[3] <- "Time_Stamp"
proj4string(GPS_tags) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

get_spec <- function(x){
  return(strsplit(x, ":")[[1]][1])
}

GPS_tags$Species <- as.vector(unlist(lapply(GPS_tags$Tag, FUN = get_spec)))
GPS_tags$Species <- as.factor(GPS_tags$Species)

blocks_shp <- raster::shapefile(blocks_path)
borders_shp <- raster::shapefile(borders_path)
mainroads_shp <- raster::shapefile(mainroads_path)

# transform crs
target_crs <- crs(borders_shp)
blocks_shp <- sp::spTransform(blocks_shp, target_crs)
mainroads_shp <- sp::spTransform(mainroads_shp, target_crs)
GPS_tags <- sp::spTransform(GPS_tags, target_crs)

# plot
ext <- raster::extent(borders_shp)

pdf(file = out_path, width = 7, height = 6)
raster::plot(borders_shp, col = cols[1], axes = TRUE, main = paste("GPS Collar Locations", Sys.time()))
raster::plot(mainroads_shp, col = cols[3], add = TRUE)
raster::plot(blocks_shp, col = "black", add = TRUE)
raster::plot(GPS_tags,
             col = cols[as.numeric(GPS_tags$Species) + 3],
             pch = as.numeric(GPS_tags$Species),
             add = TRUE)
raster::text(GPS_tags, GPS_tags$Time_Stamp, cex = 0.25, pos = 4)
legend("topleft", legend = c(levels(GPS_tags$Species), "CRS: UTM zone 35S"),
       col = c(cols[(1:length(levels(GPS_tags$Species))) + 3], NA),
       pch = c(seq(1, length(levels(GPS_tags$Species)))), NA)
dev.off()

file.remove(GPS_logs_path)
