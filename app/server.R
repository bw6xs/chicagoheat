
# Define server logic for the web application

shinyServer(
  function(input, output, session) {
    
### 
### BEGIN CODE FOR THE SURFACE TEMPERATURES PAGE
### 
    
    colorpal <- reactive({
      colorFactor(heat.index.colors, levels=c(seq(9, 21, 1)), na.color = "#808080", alpha = FALSE)
    })
    
    colorpal_2013 <- reactive({
      colorQuantile(heat.index.colors, domain=tract.polys$PERCENT, n = 4, na.color = "#808080", alpha = FALSE)
    })
    
# Default map to display in the SURFACE TEMPERATURES tab is entire summer of 1990
    
    stmap <- createLeafletMap(session, "stmap")
    
    tract.polys <- index_1990
    temp.polys <- summer_95pct_1990
    
    output$stmap <- renderLeaflet({

      my.pal <- colorpal()
      
      leaflet("stmap") %>% 
        addProviderTiles("OpenStreetMap.HOT", group = "OpenStreetMap") %>%
        addProviderTiles("Esri.WorldImagery", group = "ESRI World Imagery") %>%
        addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
        setView(-87.787411, 41.887812, zoom = 11) %>% 
        addPolygons(data = index_1990, layerId = index_1990$INDEX, fillOpacity = 0.3, 
                    stroke = TRUE, weight = 0.5,
                    color = ~colorFactor(sd.colors, index_1990$INDEX)(INDEX),
                    popup=paste0("<b>Heat Vulnerability  Index: ", index_1990@data$INDEX),
                    group = "Heat Vulnerability Index - 1990") %>%
        addPolygons(data = summer_95pct_1990, layerId = summer_95pct_1990$SP_ID, fillOpacity = 0.3, 
                    stroke = TRUE, weight = 0.5,
                    color = "red",
                    group = "Max. Temp. > 95th Percentile - 1990") %>%
        addLegend(position = 'topright',
                  pal = my.pal,
                  values = index_1990$INDEX,
                  opacity = 0.3,
                  layerId = "bghvi",
                  title = "HVI in 1990") %>%
        addLayersControl(
          baseGroups = c("OpenStreetMap", "ESRI World Imagery", "CartoDB"),
          overlayGroups = c("Heat Vulnerability Index - 1990", "Max. Temp. > 95th Percentile - 1990"),
          options = layersControlOptions(collapsed = TRUE)         
        ) %>%
         hideGroup("Max. Temp. > 95th Percentile - 1990")
   })


# Monitor buttons for user input
     
  observeEvent(input$index, {
        if (input$var == "summer") {
          if (input$index == "2000") {
            tract.polys <- index_2000
            temp.polys <- summer_95pct_2000
          }
          else if (input$index == "2010") {
            tract.polys <- index_2010
            temp.polys <- summer_95pct_2010
          }
          else {
            tract.polys <- index_1990
            temp.polys <- summer_95pct_1990
          }
        }
    
    if (input$var == "monthly") {
      if (input$index == "1990")  {
        if (input$chosenMonth == "july") {
          temp.polys <- july_95pct_1990
          tract.polys <- index_1990
        }
        else if (input$chosenMonth == "august") {
          temp.polys <- august_95pct_1990
          tract.polys <- index_1990
        }
        else {
          temp.polys <- june_95pct_1990
          tract.polys <- index_1990
        }
      }
      else if (input$index == "2000")  {
        if (input$chosenMonth == "july") {
          temp.polys <- july_95pct_2000
          tract.polys <- index_2000
        }
        else if (input$chosenMonth == "august") {
          temp.polys <- august_95pct_2000
          tract.polys <- index_2000
        }
        else {
          temp.polys <- june_95pct_2000
          tract.polys <- index_2000
        }
      }
      else if (input$index == "2010")  {
        if (input$chosenMonth == "july") {
          temp.polys <- july_95pct_2010
          tract.polys <- index_2010
        }
        else if (input$chosenMonth == "august") {
          temp.polys <- august_95pct_2010
          tract.polys <- index_2010
        }
        else {
          temp.polys <- june_95pct_2010
          tract.polys <- index_2010
        }
      }
    }

          my.pal <- colorpal()
          
          leafletProxy("stmap") %>%
            clearImages() %>%
            clearControls() %>%
            clearShapes() %>%
            addProviderTiles("OpenStreetMap.HOT", group = "OpenStreetMap") %>%
            addProviderTiles("Esri.WorldImagery", group = "ESRI World Imagery") %>%
            addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
            setView(-87.787411, 41.887812, zoom = 11) %>%
            addPolygons(data = tract.polys, layerId = tract.polys$INDEX, fillOpacity = 0.3, 
                        stroke = TRUE, weight = 0.5,
                        color = ~colorFactor(sd.colors, tract.polys$INDEX)(INDEX),
                        popup=paste0("<b>Heat Vulnerability  Index: ", tract.polys@data$INDEX),
                        group = paste0("Heat Vulnerability Index - ", input$index)) %>%
            addPolygons(data = temp.polys, layerId = temp.polys$SP_ID, fillOpacity = 0.3,
                        stroke = TRUE, weight = 0.5,
                        color = "red",
                        group = paste0("Max. Temp. > 95th Percentile - ", input$index)) %>%
            addLegend(position = 'topright',
                      pal = my.pal,
                      values = tract.polys[[10]],
                      opacity = 0.3,
                      layerId = "bghvi",
                      title = paste0("HVI in ", input$index)) %>%
            addLayersControl(
              baseGroups = c("OpenStreetMap", "ESRI World Imagery", "CartoDB"),
              overlayGroups = c(paste0("Heat Vulnerability Index - ", input$index), paste0("Max. Temp. > 95th Percentile - ", input$index)),
              options = layersControlOptions(collapsed = TRUE)
            ) %>%
            hideGroup(paste0("Max. Temp. > 95th Percentile - ", input$index))
    })
  
 
  observeEvent(input$chosenMonth, { 
    if (input$index == "1990")  {
      if (input$chosenMonth == "july") {
        temp.polys <- july_95pct_1990
        tract.polys <- index_1990
      }
      else if (input$chosenMonth == "august") {
        temp.polys <- august_95pct_1990
        tract.polys <- index_1990
      }
      else {
        temp.polys <- june_95pct_1990
        tract.polys <- index_1990
      }
    }
    else if (input$index == "2000")  {
      if (input$chosenMonth == "july") {
        temp.polys <- july_95pct_2000
        tract.polys <- index_2000
      }
      else if (input$chosenMonth == "august") {
        temp.polys <- august_95pct_2000
        tract.polys <- index_2000
      }
      else {
        temp.polys <- june_95pct_2000
        tract.polys <- index_2000
      }
    }
    else if (input$index == "2010")  {
      if (input$chosenMonth == "july") {
        temp.polys <- july_95pct_2010
        tract.polys <- index_2010
      }
      else if (input$chosenMonth == "august") {
        temp.polys <- august_95pct_2010
        tract.polys <- index_2010
      }
      else {
        temp.polys <- june_95pct_2010
        tract.polys <- index_2010
      }
    }
    
    my.pal <- colorpal()
    
    leafletProxy("stmap") %>%
      clearImages() %>%
      clearControls() %>%
      clearShapes() %>%
      addProviderTiles("OpenStreetMap.HOT", group = "OpenStreetMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "ESRI World Imagery") %>%
      addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
      setView(-87.787411, 41.887812, zoom = 11) %>%
      addPolygons(data = tract.polys, layerId = tract.polys$INDEX, fillOpacity = 0.3, 
                  stroke = TRUE, weight = 0.5,
                  color = ~colorFactor(sd.colors, tract.polys$INDEX)(INDEX),
                  popup=paste0("<b>Heat Vulnerability  Index: ", tract.polys@data$INDEX),
                  group = paste0("Heat Vulnerability Index - ", input$index)) %>%
      addPolygons(data = temp.polys, layerId = temp.polys$SP_ID, fillOpacity = 0.3,
                  stroke = TRUE, weight = 0.5,
                  color = "red",
                  group = paste0("Max. Temp. > 95th Percentile - ", input$index)) %>%
      addLegend(position = 'topright',
                pal = my.pal,
                values = tract.polys[[10]],
                opacity = 0.3,
                layerId = "bghvi",
                title = paste0("HVI in ", input$index)) %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "ESRI World Imagery", "CartoDB"),
        overlayGroups = c(paste0("Heat Vulnerability Index - ", input$index), paste0("Max. Temp. > 95th Percentile - ", input$index)),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      hideGroup(paste0("Max. Temp. > 95th Percentile - ", input$index))
  })
    
  
# Monitor for user request to visualize 2013 data
  
  observeEvent(input$rf_2013, {
    
    if (input$rf_2013 == "oldalone") {
      tract.polys <- elderly_alone_2013
      temp.polys <- summer_95pct_2013
      popup.label <- "Percent Elderly Living Alone"
    }
    else if (input$rf_2013 == "language") {
      tract.polys <- language_2013
      temp.polys <- summer_95pct_2013
      popup.label <- "Percent Limited English Speaking Households"
    }
    else if (input$rf_2013 == "oldhousing") {
      tract.polys <- older_housing_2013
      temp.polys <- summer_95pct_2013
      popup.label <- "Percent Older Housing Units"
    }
    else if (input$rf_2013 == "poverty") {
      tract.polys <- poverty_rate_2013
      temp.polys <- summer_95pct_2013
      popup.label <- "Percent Below Poverty Line"
    }
    else {
      tract.polys <- disability_2013
      temp.polys <- summer_95pct_2013
      popup.label <- "Percent With A Disability"
    }
    
    my.pal_2013 <- colorpal_2013()

    leafletProxy("stmap") %>%
      clearImages() %>%
      clearControls() %>%
      clearShapes() %>%
      addProviderTiles("OpenStreetMap.HOT", group = "OpenStreetMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "ESRI World Imagery") %>%
      addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
      setView(-87.787411, 41.887812, zoom = 11) %>%
      addPolygons(data = tract.polys, layerId = tract.polys$PERCENT, fillOpacity = 0.4, 
                  stroke = TRUE, weight = 0.5,
                  color = ~colorQuantile(heat.index.colors, tract.polys$PERCENT)(PERCENT),
                  popup=paste0(popup.label, ": ", round(tract.polys@data$PERCENT, digits = 0)),
                  group = paste0(popup.label, " in 2013")) %>%
      addPolygons(data = temp.polys, layerId = temp.polys$SP_ID, fillOpacity = 0.3,
                  stroke = TRUE, weight = 0.5,
                  color = "red",
                  group = "Max. Temp. > 95th Percentile - Summer 2013") %>%
      addLegend(position = 'topright',
                colors = heat.index.colors.quartile,
                labels = heat.index.colors.quartile.labels,
                opacity = 0.4,
                layerId = "bghvi",
                title = "Selected Risk Factor in 2013") %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "ESRI World Imagery", "CartoDB"),
        overlayGroups = c(paste0(popup.label, " in 2013"), "Max. Temp. > 95th Percentile - Summer 2013"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      hideGroup("Max. Temp. > 95th Percentile - Summer 2013")
  })
  
# Monitor for user request to intersect vulnerability and temperature data
  
  observeEvent(input$intersect, {
    if (input$checkbox_2013_data == TRUE) {
      
      if (input$rf_2013 == "oldalone") {
        tract.polys <- elderly_alone_2013
        temp.polys <- summer_95pct_2013
        popup.label <- "Percent Elderly Living Alone"
      }
      else if (input$rf_2013 == "language") {
        tract.polys <- language_2013
        temp.polys <- summer_95pct_2013
        popup.label <- "Percent Limited English Speaking Households"
      }
      else if (input$rf_2013 == "oldhousing") {
        tract.polys <- older_housing_2013
        temp.polys <- summer_95pct_2013
        popup.label <- "Percent Older Housing Units"
      }
      else if (input$rf_2013 == "poverty") {
        tract.polys <- poverty_rate_2013
        temp.polys <- summer_95pct_2013
        popup.label <- "Percent Below Poverty Line"
      }
      else {
        tract.polys <- disability_2013
        temp.polys <- summer_95pct_2013
        popup.label <- "Percent With A Disability"
      }

      poly.intersect <- quantile(tract.polys$PERCENT, probs = seq(0, 1, 0.25), na.rm = TRUE) 
      poly.upper <- tract.polys[tract.polys$PERCENT > poly.intersect[3],]   
      intersect <- gIntersection(poly.upper, summer_95pct_2013, byid=TRUE)
      intersect.popup <- paste0(popup.label, ": ", round(poly.upper@data$PERCENT, digits = 0))
      
      leafletProxy("stmap") %>%
        clearImages() %>%
        clearControls() %>%
        clearShapes() %>%
        addProviderTiles("OpenStreetMap.HOT", group = "OpenStreetMap") %>%
        addProviderTiles("Esri.WorldImagery", group = "ESRI World Imagery") %>%
        addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
        setView(-87.787411, 41.887812, zoom = 11) %>%
        
# Add a slightly thicker red polygon on top of the selected one
        
        addPolygons(data = intersect, layerId = intersect, fillOpacity = 0.3, 
                    stroke = TRUE, weight = 0.5, color = "#03F", fillColor = "red",
                    popup=intersect.popup,
                    group = "Intersection - Upper Quartile of HVI & 95th Percentile of Max. Daily Temp.") %>%
        addLayersControl(
          baseGroups = c("OpenStreetMap", "ESRI World Imagery", "CartoDB"),
          overlayGroups = paste0("Intersection - Upper Quartile of ", popup.label, " & 95th Percentile of Max. Daily Temp."),
          options = layersControlOptions(collapsed = TRUE)
        ) %>%
        hideGroup("Max. Temp. > 95th Percentile - Summer 2013")
    }
    
    else if (input$checkbox_2013_data == FALSE) {
      
      poly.intersect <- quantile(tract.polys$INDEX, probs = seq(0, 1, 0.25), na.rm = TRUE) 
      poly.upper <- tract.polys[tract.polys$INDEX > poly.intersect[3],]   
      intersect <- gIntersection(poly.upper, temp.polys, byid=TRUE)
      intersect.popup <- paste0("<b>Heat Vulnerability  Index: ", round(poly.upper@data$INDEX, digits = 0))
      
      leafletProxy("stmap") %>%
        clearImages() %>%
        clearControls() %>%
        clearShapes() %>%
        addProviderTiles("OpenStreetMap.HOT", group = "OpenStreetMap") %>%
        addProviderTiles("Esri.WorldImagery", group = "ESRI World Imagery") %>%
        addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
        setView(-87.787411, 41.887812, zoom = 11) %>%
        # Add a slightly thicker red polygon on top of the selected one
        addPolygons(data = intersect, layerId = intersect, fillOpacity = 0.3, 
                    stroke = TRUE, weight = 0.5, color = "#03F", fillColor = "red",
                    popup=intersect.popup,
                    group = "Intersection - Upper Quartile of HVI & 95th Percentile of Max. Temp.") %>%
        addLayersControl(
          baseGroups = c("OpenStreetMap", "ESRI World Imagery", "CartoDB"),
          overlayGroups = "Intersection - Upper Quartile of HVI & 95th Percentile of Max. Temp.",
          options = layersControlOptions(collapsed = TRUE)
        )
    }
  })
  
### 
### BEGIN CODE FOR THE VULNERABLE POPULATIONS PAGE
### 
  
# Default map to display in the VULNERABLE POPULATIONS tab is HVI for 1990, 2000, and 2010
  
  vpmap <- createLeafletMap(session, "vpmap")
  
  output$vpmap <- renderLeaflet({
    
    my.pal <- colorpal()
    
    leaflet("vpmap") %>% 
      addProviderTiles("OpenStreetMap.HOT", group = "OpenStreetMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "ESRI World Imagery") %>%
      addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
      setView(-87.787411, 41.887812, zoom = 11) %>% 
      addPolygons(data = hotspot_1990, layerId = hotspot_1990$SOURCE_ID, fillOpacity = 0.3, 
                  stroke = TRUE, weight = 0.5,
                  color = ~pal.hotspot(hotspot_1990$Gi_Bin),
                  group = "Getis-Ord Gi - 1990") %>%
      addPolygons(data = hotspot_2000, layerId = hotspot_2000$SOURCE_ID, fillOpacity = 0.3, 
                  stroke = TRUE, weight = 0.5,
                  color = ~pal.hotspot(hotspot_2000$Gi_Bin),
                  group = "Getis-Ord Gi - 2000") %>%
      addPolygons(data = hotspot_2010, layerId = hotspot_2010$SOURCE_ID, fillOpacity = 0.3, 
                  stroke = TRUE, weight = 0.5,
                  color = ~pal.hotspot(hotspot_2010$Gi_Bin),
                  group = "Getis-Ord Gi - 2010") %>%
      addLegend(position = 'topright',
                colors = hotspot.colors,
                labels = hotspot.labels,
                opacity = 0.3,
                layerId = "bghvi",
                title = "Vulnerability Clusters") %>%
      addLayersControl(
        baseGroups = c("CartoDB", "OpenStreetMap", "ESRI World Imagery"),
        overlayGroups = c("Getis-Ord Gi - 1990", "Getis-Ord Gi - 2000","Getis-Ord Gi - 2010"),
        options = layersControlOptions(collapsed = TRUE)
      )
 })
  
# Monitor for user request to visualize 2013 data
  
  observeEvent(input$hotspot, {
    
    if (input$hotspot == "oldalone") {
      tract.polys <- hotspot_2013_Elderly
      popup.label <- "Percent Elderly Living Alone"
    }
    else if (input$hotspot == "language") {
      tract.polys <- hotspot_2013_Language
      popup.label <- "Percent Limited English Speaking Households"
    }
    else if (input$hotspot == "poverty") {
      tract.polys <- hotspot_2013_Poverty
      popup.label <- "Percent Below Poverty Line"
    }
    else {
      tract.polys <- hotspot_2013_Disability
      popup.label <- "Percent With A Disability"
    }
    
    leafletProxy("vpmap") %>% 
      clearImages() %>%
      clearControls() %>%
      clearShapes() %>%
      addProviderTiles("OpenStreetMap.HOT", group = "OpenStreetMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "ESRI World Imagery") %>%
      addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
      setView(-87.787411, 41.887812, zoom = 11) %>% 
      addPolygons(data = tract.polys, layerId = tract.polys$SOURCE_ID, fillOpacity = 0.3, 
                  stroke = TRUE, weight = 0.5,
                  color = ~pal.hotspot(tract.polys$Gi_Bin),
                  group = paste0("Getis-Ord Gi - ", popup.label)) %>%
      addLegend(position = 'topright',
                colors = hotspot.colors,
                labels = hotspot.labels,
                opacity = 0.3,
                layerId = "bghvi",
                title = "Vulnerability Clusters") %>%
      addLayersControl(
        baseGroups = c("CartoDB", "OpenStreetMap", "ESRI World Imagery"),
        overlayGroups = paste0("Getis-Ord Gi - ", popup.label),
        options = layersControlOptions(collapsed = TRUE)
      )
  })
 
  
  
  
  
  
  
  
  
### 
### BEGIN CODE FOR THE PAST EVENTS PAGE
### 
  
# Default map to display in the PAST EVENTS tab is July 12 1995 with 1990 HVI
  
  pemap <- createLeafletMap(session, "pemap")
  
  output$pemap <- renderLeaflet({
  idw.to.plot.values <- getValues(idw.to.plot)
  
  tract.polys <- index_1990
  
  leaflet("pemap") %>%
    addProviderTiles("OpenStreetMap.HOT", group = "OpenStreetMap") %>%
    addProviderTiles("Esri.WorldImagery", group = "ESRI World Imagery") %>%
    addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
    setView(-87.627565, 41.881076, zoom = 10) %>% 
    addPolygons(data = index_1990, layerId = index_1990$INDEX, fillOpacity = 0.3, 
                stroke = TRUE, weight = 0.5,
                color = ~colorFactor(sd.colors, index_1990$INDEX)(INDEX),
                popup= paste0("<b>Heat Vulnerability Index: ", index_1990@data$INDEX),
                group = "Heat Vulnerability Index - 1990") %>%
    addLegend(position = 'topleft',
              pal = pal.sd,
              values = index_1990$INDEX,
              opacity = 0.3,
              layerId = "bghvi",
              title = "HVI in 1990") %>%
    addLayersControl(
      baseGroups = c("OpenStreetMap", "ESRI World Imagery", "CartoDB"),
      overlayGroups = c("Heat Vulnerability Index - 1990"),
      options = layersControlOptions(collapsed = TRUE)
    ) 
  })  
  
the.raster <- reactive({
  match.date <- input$heatevent
  
  if (input$heatevent == "No Heat Events Found") {
    x.range <- as.numeric(c(-88.895365, -87.367214))  # min/max longitude of the interpolation area
    y.range <- as.numeric(c(41.464838, 42.481357))  # min/max latitude of the interpolation area
    grd.2 <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.01), y = seq(from = y.range[1], to = y.range[2], by = 0.01))  # expand points to grid
    coordinates(grd.2) <- ~x+y
    gridded(grd.2) <- TRUE
    crs(grd.2) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    
    to.plot.2 <- subset(data.sub, base::as.Date(data.sub$Date) == "1995-07-12", select=c(HIMax, Lon, Lat))
    
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
    isolate(local(idw.to.plot.2))
    updateSelectInput(session, "heatevent", choices = "1995-07-12")
    updateSliderInput(session, "integer", value = 1995)
    tract.polys <- index_1990
    the.year <- 1990
    output$selectUI <- renderText({ 
      updateSelectInput(session, "heatevent", choices = "1995-07-12")
    })
  }
  else {
    x.range <- as.numeric(c(-88.895365, -87.367214))  # min/max longitude of the interpolation area
    y.range <- as.numeric(c(41.464838, 42.481357))  # min/max latitude of the interpolation area
    grd.2 <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.01), y = seq(from = y.range[1], to = y.range[2], by = 0.01))  # expand points to grid
    coordinates(grd.2) <- ~x+y
    gridded(grd.2) <- TRUE
    crs(grd.2) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    
    to.plot.2 <- subset(data.sub, base::as.Date(data.sub$Date) == match.date, select=c(HIMax, Lon, Lat))
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
    isolate(local(idw.to.plot.2))
  }
})

observeEvent(input$button, {
  if (input$heatevent == "No Heat Events Found") {
    updateSelectInput(session, "heatevent", choices = "1995-07-12")
    updateSelectInput(session, "integer", value = 1995)
    output$selectUI <- renderText({ 
      updateSelectInput(session, "heatevent", choices = "1995-07-12")
    })
    r <- idw.to.plot
    tract.polys <- index_1990
    the.year <- 1990
    leafletProxy("pemap") %>%
      clearImages() %>%
      clearControls() %>%
      clearShapes() %>%
      addProviderTiles("OpenStreetMap.HOT", group = "OpenStreetMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "ESRI World Imagery") %>%
      addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
      setView(-87.627565, 41.881076, zoom = 10) %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "ESRI World Imagery", "CartoDB"),
        options = layersControlOptions(collapsed = TRUE)
      ) 
  }
  else {
    r <- the.raster()
    r.values <- getValues(r)
    p <- colorNumeric(palette = rev(heat.colors(4,  alpha = 0.5)), domain = r.values)
    if (input$integer < 2000) {
      tract.polys <- index_1990
      the.year <- 1990
    }
    else if (input$integer > 1999 & input$integer < 2010) {
      tract.polys <- index_2000
      the.year <- 2000
    }
    else {
      tract.polys <- index_2010
      the.year <- 2010
    }
    leafletProxy("pemap") %>%
      clearImages() %>%
      clearControls() %>%
      clearShapes() %>%
      addProviderTiles("OpenStreetMap.HOT", group = "OpenStreetMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "ESRI World Imagery") %>%
      addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
      setView(-87.627565, 41.881076, zoom = 10) %>% 
      addPolygons(data = tract.polys, layerId = tract.polys$INDEX, fillOpacity = 0.3, 
                  stroke = TRUE, weight = 0.5,
                  color = ~colorFactor(sd.colors, tract.polys$INDEX)(INDEX),
                  popup= paste0("<b>Heat Vulnerability Index: ", tract.polys@data$INDEX),
                  group = paste0("Heat Vulnerability Index - ", the.year)) %>%
      addRasterImage(r, colors = p, opacity = 0.5, 
                     group = paste0("Interpolated Max. Heat Index - ", input$heatevent)) %>%
      addLegend(pal =  p, values = quantile(r, probs = seq(0, 1, 0.25))) %>%
      addLegend(position = 'topleft',
                pal = pal.sd,
                values = tract.polys$INDEX,
                opacity = 0.3,
                layerId = "bghvi",
                title = paste0("HVI in ", the.year)) %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "ESRI World Imagery", "CartoDB"),
        overlayGroups = c(paste0("Heat Vulnerability Index - ", the.year), paste0("Interpolated Max. Heat Index - ", input$heatevent)),
        options = layersControlOptions(collapsed = TRUE)
      ) 
  }
  
  
  
  the.raster <- reactive({
    match.date <- input$heatevent
    
    if (input$heatevent == "No Heat Events Found") {
      x.range <- as.numeric(c(-88.895365, -87.367214))  # min/max longitude of the interpolation area
      y.range <- as.numeric(c(41.464838, 42.481357))  # min/max latitude of the interpolation area
      grd.2 <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.01), y = seq(from = y.range[1], to = y.range[2], by = 0.01))  # expand points to grid
      coordinates(grd.2) <- ~x+y
      gridded(grd.2) <- TRUE
      crs(grd.2) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
      
      to.plot.2 <- subset(data.sub, base::as.Date(data.sub$Date) == "1995-07-12", select=c(HIMax, Lon, Lat))
      
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
      isolate(local(idw.to.plot.2))
      updateSelectInput(session, "heatevent", choices = "1995-07-12")
      updateSelectInput(session, "integer", value = 1995)
      tract.polys <- index_1990
      the.year <- 1990
      output$selectUI <- renderText({ 
        updateSelectInput(session, "heatevent", choices = "1995-07-12")
      })
    }
    else {
      x.range <- as.numeric(c(-88.895365, -87.367214))  # min/max longitude of the interpolation area
      y.range <- as.numeric(c(41.464838, 42.481357))  # min/max latitude of the interpolation area
      grd.2 <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.01), y = seq(from = y.range[1], to = y.range[2], by = 0.01))  # expand points to grid
      coordinates(grd.2) <- ~x+y
      gridded(grd.2) <- TRUE
      crs(grd.2) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
      
      to.plot.2 <- subset(data.sub, base::as.Date(data.sub$Date) == match.date, select=c(HIMax, Lon, Lat))
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
      isolate(local(idw.to.plot.2))
    }
  })
})


observe({
#  if (input$integer > 1995) {
  deaths.sub <- subset(deaths, as.numeric(deaths$Year) == input$integer)
  if (dim(deaths.sub)[1] > 0) {
    choices.out <- as.character(deaths.sub$Date)
    updateSelectInput(session, "heatevent", choices = choices.out, selected = choices.out[1])
    output$selectUI <- renderText({ 
      choices.out
    })
  }
  else {
    choices.out <- as.character("No Heat Events Found")
    output$selectUI <- renderText({ 
      updateSelectInput(session, "heatevent", choices = choices.out, selected = choices.out[1])
    })
    updateSelectInput(session, "heatevent", choices = choices.out, selected = choices.out[1])
    # tract.polys <- index_1990
    # the.year <- 1990
    # r <- idw.to.plot
#    }
  }
})

# Monitor for user request to intersect HVI with interpolated NWS heat index data

observeEvent(input$intersect_pe, {
  
  poly.intersect <- quantile(tract.polys$INDEX, probs = seq(0, 1, 0.25), na.rm = TRUE) 
  poly.upper <- tract.polys[tract.polys$INDEX > poly.intersect[3],]  
  r <- the.raster()
  the.raster.values <- getValues(r)
  to.get <- quantile(r, probs = seq(0, 1, 0.05))
  thresh <- to.get[20]
  p <- rasterToPolygons(r, fun=function(x) {x > thresh}, dissolve=FALSE)
  intersect <- gIntersection(poly.upper, p, byid=TRUE)
  intersect.popup <- paste0("<b>Heat Vulnerability  Index: ", round(poly.upper@data$INDEX, digits = 0))
  
  leafletProxy("pemap") %>%
    clearImages() %>%
    clearControls() %>%
    clearShapes() %>%
    addProviderTiles("OpenStreetMap.HOT", group = "OpenStreetMap") %>%
    addProviderTiles("Esri.WorldImagery", group = "ESRI World Imagery") %>%
    addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
    setView(-87.787411, 41.887812, zoom = 11) %>%
# Add a slightly thicker red polygon on top of the selected one
    addPolygons(data = intersect, layerId = intersect, fillOpacity = 0.5, 
                stroke = TRUE, weight = 0.5, color = "#03F", fillColor = "red",
                popup=intersect.popup,
                group = paste0("Intersection - Upper Quartile of ", the.year, " HVI & 95th Percentile of NWS Heat Index")) %>%
    addLayersControl(
      baseGroups = c("OpenStreetMap", "ESRI World Imagery", "CartoDB"),
      overlayGroups = paste0("Intersection - Upper Quartile of ", input$integer, " HVI & 95th Percentile of NWS Heat Index"),
      options = layersControlOptions(collapsed = TRUE)
    )
})
  
  
### 
### BEGIN CODE FOR THE FUTURE SCENARIOS PAGE
### 
  
# Default map to display in the FUTURE SCENARIOS tab is 2030 HVI with RCP 4.5

  
  colorpal_2030 <- reactive({
    colorFactor(heat.index.colors, levels=c(seq(4, 33, 1)), na.color = "#808080", alpha = FALSE)
  })
  
  fsmap <- createLeafletMap(session, "fsmap")
  
  output$fsmap <- renderLeaflet({
    
    
    
    my.pal_2030 <- colorpal_2030()
    
    tract.polys <- index_2030
    
    leaflet("fsmap") %>% 
      addProviderTiles("OpenStreetMap.HOT", group = "OpenStreetMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "ESRI World Imagery") %>%
      addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
      setView(-87.787411, 41.887812, zoom = 11) %>% 
      addPolygons(data = index_2030, layerId = index_2030$INDEX, fillOpacity = 0.3, 
                  stroke = TRUE, weight = 0.5,
                  color = ~colorFactor(sd.colors, index_2030$INDEX)(INDEX),
                  popup=paste0("<b>Heat Vulnerability  Index: ", index_2030@data$INDEX),
                  group = "Heat Vulnerability Index - 2030") %>%
      
      addPolygons(data = summer_95pct_2030.rcp45, layerId = summer_95pct_2030.rcp45$SP_ID, fillOpacity = 0.5, 
                  stroke = TRUE, weight = 0.5,
                  color = "red",
                  group = "Max. Temp. > 95th Percentile - 2030 - RCP 4.5") %>%
      addLegend(position = 'topright',
                pal = my.pal_2030,
                values = index_2030$INDEX,
                opacity = 0.3,
                layerId = "bghvi",
                title = "HVI in 2030") %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "ESRI World Imagery", "CartoDB"),
        overlayGroups = c("Heat Vulnerability Index - 2030", "Max. Temp. > 95th Percentile - 2030 - RCP 4.5"),
        options = layersControlOptions(collapsed = TRUE)         
      )
  })
  
  observeEvent(input$future, {
  
  	if (input$future == "RCP 2.6") {
            temp.polys <- summer_95pct_2030.rcp26
        }
    else if (input$future == "RCP 6.0") {
            temp.polys <- summer_95pct_2030.rcp60
        }
    else if (input$future == "RCP 8.5") {
            temp.polys <- summer_95pct_2030.rcp85
        }
    else {
            temp.polys <- summer_95pct_2030.rcp45
        }
  
  my.pal_2030 <- colorpal_2030()
    
  leafletProxy("fsmap") %>% 
    clearImages() %>%
    clearControls() %>%
    clearShapes() %>%
    addProviderTiles("OpenStreetMap.HOT", group = "OpenStreetMap") %>%
    addProviderTiles("Esri.WorldImagery", group = "ESRI World Imagery") %>%
    addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
    setView(-87.787411, 41.887812, zoom = 11) %>% 
    addPolygons(data = index_2030, layerId = index_2030$INDEX, fillOpacity = 0.3, 
                stroke = TRUE, weight = 0.5,
                color = ~colorFactor(sd.colors, index_2030$INDEX)(INDEX),
                popup=paste0("<b>Heat Vulnerability  Index: ", index_2030@data$INDEX),
                group = "Heat Vulnerability Index - 2030") %>%
    
    addPolygons(data = temp.polys, layerId = temp.polys$SP_ID, fillOpacity = 0.5, 
                stroke = TRUE, weight = 0.5,
                color = "red",
                group = paste0("Max. Temp. > 95th Percentile - 2030 - ", input$future)) %>%
    addLegend(position = 'topright',
              pal = my.pal_2030,
              values = index_2030$INDEX,
              opacity = 0.3,
              layerId = "bghvi",
              title = "HVI in 2030") %>%
    addLayersControl(
      baseGroups = c("OpenStreetMap", "ESRI World Imagery", "CartoDB"),
      overlayGroups = c("Heat Vulnerability Index - 2030", paste0("Max. Temp. > 95th Percentile - 2030 - ", input$future)),
      options = layersControlOptions(collapsed = TRUE)         
    )  
  })
  
# Monitor for user request to intersect 2030 HVI and temperature data
  
  observeEvent(input$intersect_fs, {
    
    tract.polys <- index_2030
    
    if (input$future == "RCP 2.6") {
      temp.polys <- summer_95pct_2030.rcp26
    }
    else if (input$future == "RCP 6.0") {
      temp.polys <- summer_95pct_2030.rcp60
    }
    else if (input$future == "RCP 8.5") {
      temp.polys <- summer_95pct_2030.rcp85
    }
    else {
      temp.polys <- summer_95pct_2030.rcp45
    }

      poly.intersect <- quantile(tract.polys$INDEX, probs = seq(0, 1, 0.25), na.rm = TRUE) 
      poly.upper <- tract.polys[tract.polys$INDEX > poly.intersect[3],]   
      intersect <- gIntersection(poly.upper, temp.polys, byid=TRUE)
      intersect.popup <- paste0("<b>Heat Vulnerability  Index: ", round(poly.upper@data$INDEX, digits = 0))
      
      leafletProxy("fsmap") %>%
        clearImages() %>%
        clearControls() %>%
        clearShapes() %>%
        addProviderTiles("OpenStreetMap.HOT", group = "OpenStreetMap") %>%
        addProviderTiles("Esri.WorldImagery", group = "ESRI World Imagery") %>%
        addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
        setView(-87.787411, 41.887812, zoom = 11) %>%
# Add a slightly thicker red polygon on top of the selected one
        addPolygons(data = intersect, layerId = intersect, fillOpacity = 0.3, 
                    stroke = TRUE, weight = 0.5, color = "#03F", fillColor = "red",
                    popup=intersect.popup,
                    group = paste0("Intersection - Upper Quartile of 2030 HVI & 95th Percentile of Max. Temp. - ", input$future)) %>%
        addLayersControl(
          baseGroups = c("OpenStreetMap", "ESRI World Imagery", "CartoDB"),
          overlayGroups = paste0("Intersection - Upper Quartile of 2030 HVI & 95th Percentile of Max. Temp. - ", input$future),
          options = layersControlOptions(collapsed = TRUE)
        )
  })
})

###
### END OF THE SERVER SIDE CODE
###
