MaroonPalette <- c(rgb(1,1,1),rgb(114,30,53,maxColorValue=255)) #or "Blues" or whatever...
#MaroonPalette <- "Blues"

# Helper functions for HTML tables...
cell_html <- function(table_cell) paste0('<td>', table_cell, '</td>')

head_html <- function(table_cell) paste0('<th>', table_cell, '</th>')

row_html <- function(table_row) {
  cells <- sapply(table_row, cell_html)
  collapse_cells <- paste0(cells, collapse='')
  paste0('<tr>', collapse_cells, '</tr>')
}

shinyServer(function(input, output, session){
  # Default selection for navigation page
  updateNavbarPage(session, "menu", selected = "Introduction")
  
  observeEvent(input$goButton, {
    updateNavbarPage(session, "menu", selected = "Data Visualisation")
    })
  
  output$DemoTable <- renderUI({
    if (is.null(DemoTableData())) return('')
    HTML(paste0('<table>',
                paste(sapply(names(DemoTableData()), head_html), collapse = ''),
                paste(apply(DemoTableData(), 1, row_html), collapse = ''),
                '</table>'))
  })
  
  Get_Selected_Area <- reactive({
    if (input$filterreg != '') return(input$filterreg)
    if (input$filterta != '') return(input$filterta)
    return(NULL)
  })
  
  output$Area <- renderText({ 
    if (is.null(Get_Selected_Area())) {
      return(NULL)
    }
    paste("Comparison of", 
          Get_Selected_Area(),
          "to rest of N.Z.")
  })
  
  Get_Selected_Age <- reactive({
    if (input$mapagegrp != '') return(input$mapagegrp)
    # This shouldn't be possible in this version...
    return(NULL)
  })
  
  Get_Selected_Risk <- reactive({
    if (input$mapagegrp == '00-05' || input$mapagegrp == '06-14'){
      return(names(Risk0)[sapply(Risk0, FUN=function(X) input$maprisktype1 %in% X)])
    } else if (input$mapagegrp == '15-19'){
      return(names(Risk15)[sapply(Risk15, FUN=function(X) input$maprisktype2 %in% X)])
    } else if (input$mapagegrp == '20-24'){
      return(names(Risk20)[sapply(Risk20, FUN=function(X) input$maprisktype3 %in% X)])
    } else return(NULL)
  })
  
  output$DemoDesc <- renderText({ 
    if (is.null(Get_Selected_Age()) || is.null(Get_Selected_Area()) || is.null(Get_Selected_Risk())) {
      return(NULL)
    }
    Age_Readable <- names(age.list)[sapply(age.list, FUN=function(X) Get_Selected_Age() %in% X)]
    paste("Gender and Multiple Response Ethnic profile of children aged", 
          Age_Readable, "with", Get_Selected_Risk(), "in", Get_Selected_Area())
  })

  DemoTableData <- reactive({
    # Construct Male/Female counts
    if(input$dispgeo == 'region'){
      Area <- input$filterreg
      d <- copy(rc.tab)
    } else if (input$dispgeo == 'ta'){
      Area <- input$filterta
      d <- copy(ta.tab)
    } else return(NULL)
    if (input$mapagegrp == '00-05' || input$mapagegrp == '06-14'){
      var <- input$maprisktype1
    } else if (input$mapagegrp == '15-19'){
      var <- input$maprisktype2
    } else if (input$mapagegrp == '20-24'){
      var <- input$maprisktype3
    }
    if (Area == '') return(NULL)
    
    # Extract the risk category
    var <- strsplit(var,"_")[[1]][3]
    d %<>%  filter(agegrp==input$mapagegrp) %>%
      select(description, sex, ends_with(var)) %>%
      group_by(sex, description) %>%
      summarise_each(funs(sum))
    G_Area <- d %>% filter(description == Area) %>%
      select(-description) %>%
      group_by(sex) %>%
      summarise_each(funs(sum))

    Gender <- data.frame(c("Total", "Male", "Female"), G_Area[1:3,2])
    names(Gender) <- c("", "Number")
    Ethnicity <- data.frame(c("Maori", "NZ/European", "Asian", "Pacific", "Other"),
                            t(G_Area[1,3:7]))
    names(Ethnicity) <- c("", "Number")
    Final <- rbind(Gender,Ethnicity)
    Final$Percent <- sprintf("%1.2f%%", 100*c(Final$Number[1:3]/sum(Final$Number[2:3]), Final$Number[4:8]/sum(Final$Number[2:3])))
    Final$Number <- format(Final$Number,big.mark=",",scientific=FALSE)
    Final <- Final[-1,]
    return(Final)
  })
  
  #output$ComparisonTable <- renderTable({
  #  df <- copy(ComparisonTableData())
  #})
  
  output$NationalTable <- renderUI({
    if (input$mapagegrp == '00-05'){
      var <- input$maprisktype1
      Table <- Table_0005
      var <- strsplit(var,"_")[[1]][3]
      Table$At_Risk = Table[, strtoi(var) + 1]
      Table$P1 <- sprintf("%1.2f%%",100*Table$At_Risk/Table$At_Risk[1])
      Table$Not_At_Risk = Table$All - Table$At_Risk
      # Fix Avg Cost.
      Table$Not_At_Risk[7] <- (Table$All[7]*Table$All[1] - Table$At_Risk[7]*Table$At_Risk[1])/Table$Not_At_Risk[1]
      Table %<>% select(X,At_Risk, P1, Not_At_Risk)
      Table$P2 <- sprintf("%1.2f%%",100*Table$Not_At_Risk/Table$Not_At_Risk[1])
      Table$P1[1] = ''
      Table$P2[1] = ''
      Table$P1[7] = ''
      Table$P2[7] = ''
      Table$At_Risk <- format(round(Table$At_Risk) , big.mark=",", scientific=FALSE)
      Table$Not_At_Risk <- format(round(Table$Not_At_Risk) , big.mark=",", scientific=FALSE)
      Table$Not_At_Risk[7] <- paste0('$', Table$Not_At_Risk[7])
      Table$At_Risk[7] <- paste0('$', Table$At_Risk[7])
      names(Table) <- c("Outcome", "At Risk", "", "Not At Risk", "")
    } else if(input$mapagegrp == '06-14'){
      var <- input$maprisktype1
      Table <- Table_0614
      var <- strsplit(var,"_")[[1]][3]
      Table$At_Risk = Table[, strtoi(var) + 1]
      Table$P1 <- sprintf("%1.2f%%",100*Table$At_Risk/Table$At_Risk[1])
      Table$Not_At_Risk = Table$All - Table$At_Risk
      # Fix Avg Cost.
      Table$Not_At_Risk[7] <- (Table$All[7]*Table$All[1] - Table$At_Risk[7]*Table$At_Risk[1])/Table$Not_At_Risk[1]
      Table %<>% select(X,At_Risk, P1, Not_At_Risk)
      Table$P2 <- sprintf("%1.2f%%",100*Table$Not_At_Risk/Table$Not_At_Risk[1])
      Table$P1[1] = ''
      Table$P2[1] = ''
      Table$P1[7] = ''
      Table$P2[7] = ''
      Table$At_Risk <- format(round(Table$At_Risk) , big.mark=",", scientific=FALSE)
      Table$Not_At_Risk <- format(round(Table$Not_At_Risk) , big.mark=",", scientific=FALSE)
      Table$Not_At_Risk[7] <- paste0('$', Table$Not_At_Risk[7])
      Table$At_Risk[7] <- paste0('$', Table$At_Risk[7])
      names(Table) <- c("Outcome", "At Risk", "", "Not At Risk", "")
    } else if (input$mapagegrp == '15-19'){
      var <- input$maprisktype2
      Table <- Table_1519
      var <- strsplit(var,"_")[[1]][3]
      Table$At_Risk = Table[, strtoi(var) + 1]
      Table$P1 <- sprintf("%1.2f%%",100*Table$At_Risk/Table$At_Risk[1])
      Table$Not_At_Risk = Table$All - Table$At_Risk
      # Fix Avg Cost.
      Table$Not_At_Risk[5] <- (Table$All[5]*Table$All[1] - Table$At_Risk[5]*Table$At_Risk[1])/Table$Not_At_Risk[1]
      Table %<>% select(X,At_Risk, P1, Not_At_Risk)
      Table$P2 <- sprintf("%1.2f%%",100*Table$Not_At_Risk/Table$Not_At_Risk[1])
      Table$P1[1] = ''
      Table$P2[1] = ''
      Table$P1[5] = ''
      Table$P2[5] = ''
      Table$At_Risk <- format(round(Table$At_Risk) , big.mark=",", scientific=FALSE)
      Table$Not_At_Risk <- format(round(Table$Not_At_Risk) , big.mark=",", scientific=FALSE)
      Table$Not_At_Risk[5] <- paste0('$', Table$Not_At_Risk[5])
      Table$At_Risk[5] <- paste0('$', Table$At_Risk[5])
      names(Table) <- c("Outcome", "At Risk", "", "Not At Risk", "")
    } else if (input$mapagegrp == '20-24'){
      var <- input$maprisktype3
      Table <- Table_2024
      var <- strsplit(var,"_")[[1]][3]
      Table$At_Risk = Table[, strtoi(var) + 1]
      Table$P1 <- sprintf("%1.2f%%",100*Table$At_Risk/Table$At_Risk[1])
      Table$Not_At_Risk = Table$All - Table$At_Risk
      # Fix Avg Cost.
      Table$Not_At_Risk[5] <- (Table$All[5]*Table$All[1] - Table$At_Risk[5]*Table$At_Risk[1])/Table$Not_At_Risk[1]
      Table %<>% select(X,At_Risk, P1, Not_At_Risk)
      Table$P2 <- sprintf("%1.2f%%",100*Table$Not_At_Risk/Table$Not_At_Risk[1])
      Table$P1[1] = ''
      Table$P2[1] = ''
      Table$P1[5] = ''
      Table$P2[5] = ''
      Table$At_Risk <- format(round(Table$At_Risk) , big.mark=",", scientific=FALSE)
      Table$Not_At_Risk <- format(round(Table$Not_At_Risk) , big.mark=",", scientific=FALSE)
      Table$Not_At_Risk[5] <- paste0('$', Table$Not_At_Risk[5])
      Table$At_Risk[5] <- paste0('$', Table$At_Risk[5])
      names(Table) <- c("Outcome", "At Risk", "", "Not At Risk", "")
    } else return(NULL)
    HTML(paste0('<table>',
                paste(sapply(names(Table), head_html), collapse = ''),
                paste(apply(Table, 1, row_html), collapse = ''),
                '</table>'))
  })
  
  output$National <- renderText({
    Age_Readable <- names(age.list)[sapply(age.list, FUN=function(X) Get_Selected_Age() %in% X)]
    return(paste('Projected outcomes for', Get_Selected_Risk(), 
                 ', aged', Age_Readable, '(National level)'))
  })
  
  output$ComparisonTable <- renderUI({
    if (is.null(ComparisonTableData())) return('')
    HTML(paste0('<table>',
                paste(sapply(names(ComparisonTableData()), head_html), collapse = ''),
                paste(apply(ComparisonTableData(), 1, row_html), collapse = ''),
                '</table>'))
  })
  
  ComparisonTableData <- reactive({
    # Import the relevant geographic linked data.
    if(input$dispgeo == 'region'){
      Area <- input$filterreg
      d <- copy(rc.tab)
    } else if (input$dispgeo == 'ta'){
      Area <- input$filterta
      d <- copy(ta.tab)
    } else return(NULL)
    if (input$mapagegrp == '00-05' || input$mapagegrp == '06-14'){
      var <- input$maprisktype1
    } else if (input$mapagegrp == '15-19'){
      var <- input$maprisktype2
    } else if (input$mapagegrp == '20-24'){
      var <- input$maprisktype3
    }
    if (Area == '') return(NULL)
    
    # Find the information we are interested in.
    d %<>%  filter(agegrp == input$mapagegrp, sex == "All") %>%
      select(description, all, ends_with(var)) %>%
      mutate(description = ifelse(description == Area, Area, "Rest of NZ")) %>%
      group_by(description) %>%
      summarise_each(funs(sum))
    
    d$description <- gsub("Local Board Area", "", d$description)
    d$description <- gsub("District", "", d$description)
    d$description <- gsub("City", "", d$description)
    d$description <- gsub("Region", "", d$description)
    
    d$Percent <- sprintf('%1.2f%%', 100*d[[var]]/d$all)
    names(d) <- c("", "Total", "At Risk", "Percent")
    d[["Total"]] <-  format(d[["Total"]],big.mark=",",scientific=FALSE)
    d[["At Risk"]] <-  format(d[["At Risk"]],big.mark=",",scientific=FALSE)
    return(d)
  })
  
  DownloadData <- reactive({

    if (input$mapgeonz=="regc" || input$mapgeoreg=="regc"){
      map_data <- copy(rc.tab)
    } else if (input$mapgeonz=="ta" || input$mapgeoreg=="ta" || input$mapgeota=="ta"){
      map_data <- copy(ta.tab)
    } else if (input$mapgeoreg=="au" || input$mapgeota=="au"){
      map_data <- copy(au.tab)
    } else return(NULL)
    
    map_data %<>% filter(sex == input$mapsex, agegrp==input$mapagegrp) %>%
      select(-sex, -agegrp)
    
    if (input$filterreg != ''){
      map_data %<>% filter(reg == input$filterreg)
    } else if (input$filterta != ''){
      map_data %<>% filter(tla == input$filterta)
    }
    
    if (input$mapagegrp == '00-05' || input$mapagegrp == '06-14'){
      RiskList <- Risk0
      map_data[[Get_Selected_Risk()]] <- map_data[[input$maprisktype1]]
    } else if (input$mapagegrp == '15-19'){
      RiskList <- Risk15
      map_data[[Get_Selected_Risk()]] <- map_data[[input$maprisktype2]]
    } else if (input$mapagegrp == '20-24'){
      RiskList <- Risk20
      map_data[[Get_Selected_Risk()]] <- map_data[[input$maprisktype3]]
    } else return(NULL)
    
    map_data %<>% select(description, all, ends_with(Get_Selected_Risk())) %>%
      arrange(description)
    return(map_data)
  })

  output$downloadData <- downloadHandler(
    # Add in better name generation...
    filename = function() { paste0('Output', '.csv') },
    content = function(file) { write.csv( DownloadData(), file) }
  )

  
  # Register event to update geogrpahy consistently.
  observeEvent(input$dispgeo, {
    if (input$dispgeo == 'nz'){
      updateSelectInput(session, "mapgeonz",selected="regc")
      updateSelectInput(session, "mapgeoreg",selected="")
      updateSelectInput(session, "mapgeota",selected="")
      updateSelectizeInput(session, "filterreg",selected="")
      updateSelectizeInput(session, "filterta",selected="")
    } else if(input$dispgeo == 'region'){
      updateSelectInput(session, "mapgeoreg",selected="regc")
      updateSelectInput(session, "mapgeonz",selected="")
      updateSelectInput(session, "mapgeota",selected="")
      updateSelectizeInput(session, "filterta",selected="")
    } else if(input$dispgeo == 'ta'){
      updateSelectInput(session, "mapgeota",selected="ta")
      updateSelectInput(session, "mapgeonz",selected="")
      updateSelectInput(session, "mapgeoreg",selected="")
      updateSelectizeInput(session, "filterreg",selected="")
    }
  })
  
  # Register event for updating the region selected.
  observeEvent(input$filterreg, {
    if (input$filterreg != '') {
      dgeom <- copy(Region_Shape_16)
      if (input$filterreg %in% dgeom@data$description) dgeom <- dgeom[dgeom$description == input$filterreg,]
      view_box <- bbox(dgeom)
      leafletProxy("map") %>% fitBounds(view_box[1], view_box[4], view_box[3], view_box[2])
    }
    })
  
  # Register event for updating the territorial authority selected.
  observeEvent(input$filterta, {
    if (input$filterta != '') {
      dgeom <- copy(TA_Shape_16)
      if (input$filterta %in% dgeom@data$description) dgeom <- dgeom[dgeom$description == input$filterta,]
      view_box <- bbox(dgeom)
      leafletProxy("map") %>% fitBounds(view_box[1], view_box[4], view_box[3], view_box[2])
    }
  })
  
  

output$map <- renderLeaflet({
  #  Display a progress bar.
  progress <- shiny::Progress$new()
  on.exit({progress$close()})
  progress$set(message = "Updating Plot - Be Patient", #detail = 'This may take a while...', 
               value = 0)
  
  # First work out the level of display geography
  if (input$dispgeo == 'nz'){
    # If NZ then use mapgeonz variable
    if (input$mapgeonz=="regc") geom <- Region_Shape_16
    else if (input$mapgeonz=="ta") geom <- TA_Shape_16
    else return(NULL)
  } else if(input$dispgeo == 'region'){
    # If region then use mapgeoreg variable
    if (input$mapgeoreg=="regc") geom <- Region_Shape_16
    else if (input$mapgeoreg=="ta") geom <- TA_Shape_16
    else if (input$mapgeoreg=="au") geom <- Area_Unit_Shape_16
    else return(NULL)
  } else if(input$dispgeo == 'ta'){
    # If ta then use mapgeota variable
    if (input$mapgeota=="ta") geom <- TA_Shape_16
    else if (input$mapgeota=="au") geom <- Area_Unit_Shape_16
    else return(NULL)
  }
  progress$set(value = 0.2)
  
  if (input$dispgeo == 'nz'){
    view_box <- bbox(geom)
    map <- leaflet() %>% clearShapes() %>%
      mapOptions(zoomToLimits = 'never') %>%
      fitBounds(view_box[1], view_box[4], view_box[3], view_box[2])
  } else {
    map <- leafletProxy("map") %>% clearShapes() %>% clearMarkerClusters() %>%
      mapOptions(zoomToLimits = 'never')
  }
  
  # Import the map data
  if (input$mapgeonz=="regc" || input$mapgeoreg=="regc"){
    map_data <- copy(rc.tab) %>%
      filter(sex == input$mapsex, agegrp==input$mapagegrp) %>%
      select(description, starts_with("all")) %>%
      group_by(description) %>%
      summarise_each(funs(sum))
  } else if (input$mapgeonz=="ta" || input$mapgeoreg=="ta" || input$mapgeota=="ta"){
    if(input$dispgeo == 'region'){
      map_data <- copy(ta.tab) %>% 
        filter(sex == input$mapsex, agegrp == input$mapagegrp, reg == input$filterreg) %>%
        select(description, starts_with("all")) %>%
        group_by(description) %>%
        summarise_each(funs(sum))
    } else{
      map_data <- copy(ta.tab) %>%
        filter(sex == input$mapsex, agegrp==input$mapagegrp) %>%
        select(description, starts_with("all")) %>%
        group_by(description) %>%
        summarise_each(funs(sum))
    }
  } else if (input$mapgeoreg=="au" || input$mapgeota=="au"){
    if(input$dispgeo == 'region'){
      map_data <- copy(au.tab) %>% 
        filter(sex == input$mapsex, agegrp == input$mapagegrp, reg == input$filterreg) %>%
        select(description, starts_with("all")) %>%
        group_by(description) %>%
        summarise_each(funs(sum))
    } else if(input$dispgeo == 'ta'){
      map_data <- copy(au.tab) %>% 
        filter(sex == input$mapsex, agegrp == input$mapagegrp, tla == input$filterta) %>%
        select(description, starts_with("all")) %>%
        group_by(description) %>%
        summarise_each(funs(sum))
    }
  } else return(NULL)
  
  progress$set(value = 0.4)
  
  # Some further data manipulation
  if (input$mapagegrp == '00-05' || input$mapagegrp == '06-14'){
    risk <- input$maprisktype1
    RiskList <- Risk0
    map_data$at_risk <- map_data[[input$maprisktype1]]
  } else if (input$mapagegrp == '15-19'){
    risk <- input$maprisktype2
    RiskList <- Risk15
    map_data$at_risk <- map_data[[input$maprisktype2]]
  } else if (input$mapagegrp == '20-24'){
    risk <- input$maprisktype3
    RiskList <- Risk20
    map_data$at_risk <- map_data[[input$maprisktype3]]
  } else return(NULL)
  map_data$plot_var <- map_data$at_risk/map_data$all
  map_data$plot_var_num <- map_data$all
  progress$set(value = 0.6)
  
  # Import the shape data
  if (input$mapgeonz=="regc" || input$mapgeoreg=="regc") map_layer <- Region_Shape_16
  else if (input$mapgeonz=="ta" || input$mapgeoreg=="ta" || input$mapgeota=="ta") map_layer <- TA_Shape_16
  else if (input$mapgeonz=="au" || input$mapgeoreg=="au" || input$mapgeota=="au") map_layer <- Area_Unit_Shape_16
  else return(NULL)
  
  if (is.null(map_layer) || is.null(map_data) || nrow(map_data) == 0) return(NULL)
  
  map_layer <- sp::merge(map_layer, map_data, by = c("description"),
                         all.x = T)
  map_layer <- map_layer[!is.na(map_layer$all),]
  map_layer@data$plot_var[is.na(map_layer@data$plot_var)] <- 0 

  # Create the popup information
  popup <- ~paste0(description,
                   "<dl><dt># at risk </dt><dd>",
                   at_risk,
                   "</dd><dt>Population </dt><dd>",
                   all, 
                   "</dd><dt>Percentage at risk </dt><dd>", 
                   sprintf("%1.2f%%", 100*plot_var),"</dd></dl>")
  
  # Update the color palette for this variable
  if (min(map_layer@data$plot_var) == max(map_layer@data$plot_var)){
    pal <- colorNumeric(MaroonPalette, domain = c(0,map_layer@data$plot_var,1), na.color="black")
    Legend_Palette <- colorBin(MaroonPalette, c(0,map_layer@data$plot_var,1))
  } else {
    pal <- colorNumeric(MaroonPalette, domain = c(map_layer@data$plot_var), na.color="black")
    Legend_Palette <- colorBin(MaroonPalette, map_layer@data$plot_var)
  }
  
  if (input$mapgeoreg=="au"){
    High_layer <- Region_Shape_16
    High_layer <- High_layer[High_layer$description != input$filterreg,]
  } else if(input$mapgeoreg=="ta"){
    High_layer <- Region_Shape_16
    High_layer <- High_layer[High_layer$description != input$filterreg,]
  } else if(input$mapgeota=="au"){
    High_layer <- TA_Shape_16
    High_layer <- High_layer[High_layer$description != input$filterta,]
  }
  # Create custom legend...
  bins <- sapply(attr(Legend_Palette,"colorArgs")$bins, 
                 function(x) paste0(sprintf('%1.1f',100*x),"%"))
  cols <- Legend_Palette(attr(Legend_Palette,"colorArgs")$bins)
  
  # Finally add the filled polygons and the legend to the map
  if (input$mapgeoreg=="au" || input$mapgeota=="au" || 
        input$mapgeoreg=="ta"){
    map %<>% addPolygons(data=High_layer,
                         opacity=1, weight=1, color="black", fillColor='lightgrey')
  }
  
  map %<>% addTiles(group = 'None') %>%
    addPolygons(data=map_layer,
                opacity=1, weight=0.75, color="black",
                fillOpacity=0.8, fillColor=~pal(plot_var),popup=popup
                ) %>%
    addLegend(position = "bottomleft",# pal = pal, values=map_layer@data$plot_var
              colors = rev(cols),
              labels = rev(bins), opacity = 0.8, 
              title = "At risk"#names(RiskList)[sapply(RiskList, FUN=function(X) risk %in% X)]
    )
  if (input$mapgeoreg=="au" || input$mapgeota=="au"){
    if (input$filterta != ''){
      GV <- Gaming_Venues %>% filter(TA == input$filterta)
      FF <- Fast_Food %>% filter(TA == input$filterta)
      CC <- Car_Crashes %>% filter(TA == input$filterta)
    } else if(input$filterreg != ''){
      GV <- Gaming_Venues %>% filter(Reg == input$filterreg)
      FF <- Fast_Food %>% filter(Reg == input$filterreg)
      CC <- Car_Crashes %>% filter(Reg == input$filterreg)
    }
    map %<>% 
      addMarkers(
      data = GV, lng = ~GEOMETRY_X, lat = ~GEOMETRY_Y,
      layerId = rownames(Gaming_Venues),
      clusterOptions = markerClusterOptions(), clusterId = 'cluster1',
      group = 'Gaming Machines'
    )  %>%
      addMarkers(
        data = FF, lng = ~GEOMETRY_X, lat = ~GEOMETRY_Y,
        layerId = rownames(Fast_Food),
        clusterOptions = markerClusterOptions(), clusterId = 'cluster2',
        group = 'Fast Food'
      )  %>%
      addMarkers(
        data = CC,
        clusterOptions = markerClusterOptions(), clusterId = 'cluster3',
        group = 'Car Crashes'
      )  %>%
      addLayersControl(
        overlayGroups = c("Gaming Machines","Fast Food",'Car Crashes')
      ) %>% hideGroup("Gaming Machines") %>% 
      hideGroup("Fast Food") %>% hideGroup('Car Crashes')
  }
    

  progress$set(value = 0.8)
  
  
  map
})


})


#title = 'An Obvious Legend'
#pal=pal, 
#values=map_layer@data$plot_var#, 
#               labFormat = function(type, cuts, p){
#       print(type)
#       print(cuts)
#       print(p)
#       n = length(cuts)
#       paste0(100*cuts[-n], " &ndash; ", 100*cuts[-1])
#     }