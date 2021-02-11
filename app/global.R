library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyBS)
library(shinyLP)
library(leaflet)
library(maptools)
library(rgdal)
library(downloader)
library(grDevices)
library(utils)
library(rgeos)
library(raster)
library(GISTools)
library(gstat)
library(lubridate)
library(stringi)


load("vector_data.RData")

heat.index.colors <- colorRampPalette(c("green", "orange"))(13) 
heat.index.colors.2030 <- colorRampPalette(c("green", "orange"))(length(unique(index_2030$INDEX))) 
heat.index.colors.quartile <- colorRampPalette(c("green", "orange"))(4) 
heat.index.colors.quartile.labels <- as.list(c("Lowest Quartile", "Second Quartile", "Third Quartile", "Highest Quartile"))
heat.index.colors.tercile <- colorRampPalette(c("green", "orange"))(3) 
heat.index.colors.tercile.labels <- as.list(c("Low", "Medium", "High"))
pal.sd <- colorFactor(heat.index.colors, levels=c(seq(9, 21, 1)), na.color = "#808080", alpha = FALSE)
hotspot.colors <- c("royalblue", "cornflowerblue", "pink", "red")
hotspot.labels <- as.list(c("Low Vulnerability Cluster - 99% Conf.", "Low Vulnerability Cluster - 95% Conf.", "High Vulnerability Cluster - 95% Conf.", "High Vulnerability Cluster - 99% Conf."))
pal.hotspot <- colorFactor(hotspot.colors, c(-3, -2, 2, 3)) 
sd.colors <- c("green", "lightgreen", "yellow", "orange")

data.sub$Date <- base::as.Date(data.sub$Date)
deaths$Date <- base::as.Date(deaths$BEGIN_DATE)
deaths$Year <- lubridate::year(deaths$Date)
valid.years <- sort(unique(as.numeric(deaths$Year)))

get.interp <- function(x) {
  match.date <- x
  
  x.range <- as.numeric(c(-88.895365, -87.367214))  # min/max longitude of the interpolation area
  y.range <- as.numeric(c(41.464838, 42.481357))  # min/max latitude of the interpolation area
  grd.2 <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.01), y = seq(from = y.range[1], to = y.range[2], by = 0.01))  # expand points to grid
  coordinates(grd.2) <- ~x+y
  gridded(grd.2) <- TRUE
  crs(grd.2) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  to.plot.2 <- subset(data.sub, base::as.Date(data.sub$Date) == match.date, select=c(HIMax, Lon, Lat, Date))
  to.plot.date <- subset(to.plot.2, select=Date)
  to.plot.2 <- subset(to.plot.2, select = -Date)
  xy.2 <- to.plot.2[,c(2,3)]
  spdf.2 <- SpatialPointsDataFrame(coords = xy.2, data = to.plot.2,
                                   proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  idw.2 <- gstat::idw(formula = spdf.2$HIMax ~ 1, locations = spdf.2, newdata = grd.2)  # apply idw model for the data
  idw.output.2 = as.data.frame(idw.2)  # output is defined as a data table
  names(idw.output.2)[1:3] <- c("lon", "lat", "HIMax")
  idw.output.2 <- idw.output.2[,1:3]
  idw.to.plot.2 <- rasterFromXYZ(idw.output.2, crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  crs(idw.to.plot.2) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  r <- raster(idw.to.plot.2)
  idw.to.plot.2.vals <- getValues(idw.to.plot.2)
  idw.to.plot.2.vals <- range(idw.to.plot.2.vals)
  return(idw.to.plot.2)
}

the.year <- 1990

