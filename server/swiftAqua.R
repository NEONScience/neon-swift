observeEvent(input$menu, {
  if(input$menu == "swft_aquatics_tab"){
    
    theme_set(theme_bw()) 
    
    startTimeTIS <- Sys.time()
    swiftAquaData <- readRDS(paste0(swft.server.folder.path, "data/swiftAqua.rds"))
    library(stringr)
    swiftAquaData$Location[str_detect(string = swiftAquaData$sensorID, pattern ="S1") == TRUE] <- "S1"
    swiftAquaData$Location[str_detect(string = swiftAquaData$sensorID, pattern ="S2") == TRUE] <- "S2"
    swiftAquaData$Location[str_detect(string = swiftAquaData$sensorID, pattern ="STAFFTROLLTEMP") == TRUE] <- "S2"
    swiftAquaData$Location[str_detect(string = swiftAquaData$sensorID, pattern ="AQUATROLL") == TRUE] <- "AQUATROLL"
    swiftAquaData$Location[str_detect(string = swiftAquaData$sensorID, pattern ="MET") == TRUE] <- "MET"
    swiftAquaData$Location[str_detect(string = swiftAquaData$sensorID, pattern ="Belfort") == TRUE] <- "Belfort"
    swiftAquaData$Location[str_detect(string = swiftAquaData$sensorID, pattern ="BUOY") == TRUE] <- "BUOY"
    swiftAquaData$Location[str_detect(string = swiftAquaData$sensorID, pattern ="NADP") == TRUE] <- "NADP"
    
    # AIS PLOTS BEGUN # AIS PLOTS BEGUN # AIS PLOTS BEGUN # AIS PLOTS BEGUN # AIS PLOTS BEGUN # AIS PLOTS BEGUN # AIS PLOTS BEGUN # AIS PLOTS BEGUN	
    
    ## PRT Calcs	
    PRTvars <- c("METPRT","METNR01PRT","S1PRTTEMP","S2PRTTEMP")	
    PRT99 <- swiftAquaData %>%	
      dplyr::filter(sensorID %in% PRTvars) %>%	
      dplyr::filter(reading < 100)%>%	
      dplyr::mutate(PRT = (reading -100) / (.00392 *100))	
    PRT100 <- swiftAquaData %>%	
      dplyr::filter(sensorID %in% PRTvars) %>%	
      dplyr::filter(reading >= 100) %>%	
      dplyr::mutate(PRT = ((reading/100)-1)/(.00385))	
    metTempVars <- c("METHMPDew","METHMPTemp","MET2DWindTemp","METBaroTemp","S1SONDETEMP","S2SONDETEMP","STAFFTROLLTEMP")	
    metTempData <- swiftAquaData %>%	
      dplyr::filter(sensorID %in% metTempVars) %>%	
      dplyr::mutate(PRT = reading)	
    
    prt = rbind(PRT99,PRT100,metTempData)	
    
    # Conductivity Data	
    swiftAISCondData <- reactive({	
      swiftAquaData %>%	
        dplyr::filter(dateID> input$swiftAISDateRange[1] & dateID < input$swiftAISDateRange[2]) %>% # Filter by date selected	
        dplyr::filter(siteID %in% input$swiftAISSiteID) %>% # Filter by SiteID selected	
        dplyr::filter(sensorID %in% c("S1SONDECOND","S2SONDECOND")) # Filter to SensorID	
    })	
    # Conductivity Plot	
    swiftAISCondPlot <- reactive({	
      ggplot(data = swiftAISCondData(), aes(color = sensorID))+	
        geom_line(aes(x=dateID,y=reading),size = 1, na.rm = TRUE) +	
        scale_x_date(date_labels = "%b-%d") +	
        labs(x = "", y="")+	
        geom_point(aes(x=dateID,y=reading))+	
        facet_grid(siteID~sensorID,scales = "free_y")+	
        theme_light()	
    })	
    output$swiftAISCondPlot <- renderPlotly({	
      swiftAISCondPlot()	
    })	
    # End Conductivity 	
    # Start Dissolved Oxygen	
    
    # DO Data	
    swiftAISDOData <- reactive({	
      swiftAquaData %>%	
        dplyr::filter(dateID> input$swiftAISDateRange[1] & dateID < input$swiftAISDateRange[2]) %>% # Filter by date selected	
        dplyr::filter(siteID %in% input$swiftAISSiteID) %>% # Filter by SiteID selected	
        dplyr::filter(sensorID %in% c("S1SONDEDO","S2SONDEDO")) # Filter to SensorID	
    })	
    swiftAISDOPlot <- reactive({	
      ggplot(data = swiftAISDOData(), aes(color = sensorID))+	
        geom_line(aes(x=dateID,y=reading),size = 1, na.rm = TRUE) +	
        scale_x_date(date_labels = "%b-%d") +	
        labs(x = "", y="")+ geom_point(aes(x=dateID,y=reading))+	
        facet_grid(siteID~sensorID,scales = "free_y")+	
        theme_light()	
    })	
    output$swiftAISDOPlot <- renderPlotly({	
      swiftAISDOPlot()	
    })	
    # End DO Data	
    # Start pH Data	
    ## InputData	
    swiftAISpHData <- reactive({	
      swiftAquaData %>%	
        dplyr::filter(dateID> input$swiftAISDateRange[1] & dateID < input$swiftAISDateRange[2]) %>% # Filter by date selected	
        dplyr::filter(siteID %in% input$swiftAISSiteID) %>% # Filter by SiteID selected	
        dplyr::filter(unit == "pH") # Filter to SensorID	
    })	
    ## Plot	
    swiftAISpHPlot <- reactive({	
      ggplot(data = swiftAISpHData(), aes(color = sensorID))+	
        geom_line(aes(x=dateID,y=reading),size = 1, na.rm = TRUE) +	
        scale_x_date(date_labels = "%b-%d") +	
        labs(x = "", y="")+ geom_point(aes(x=dateID,y=reading))+	
        facet_grid(siteID~sensorID,scales = "free_y")+	
        theme_light()	
    })	
    output$swiftAISpHPlot <- renderPlotly({	
      swiftAISpHPlot()	
    })	
    # End pH Data	
    # Start Algae	
    # Input Data	
    swiftAISAlgaeData <- reactive({	
      swiftAquaData %>%	
        dplyr::filter(dateID> input$swiftAISDateRange[1] & dateID < input$swiftAISDateRange[2]) %>% # Filter by date selected	
        dplyr::filter(siteID %in% input$swiftAISSiteID) %>% # Filter by SiteID selected	
        dplyr::filter(sensorID %in% c("S1SONDEALGAE","S2SONDEALGAE")) # Filter to SensorID	
    })	
    ## Plot	
    swiftAISAlgaePlot <- reactive({	
      ggplot(data = swiftAISAlgaeData(), aes(color = sensorID))+	
        geom_line(aes(x=dateID,y=reading),size = 1, na.rm = TRUE) +	
        scale_x_date(date_labels = "%b-%d") +	
        labs(x = "", y="")+ geom_point(aes(x=dateID,y=reading))+	
        facet_grid(siteID~sensorID,scales = "free_y")+	
        theme_light()	
    })	
    output$swiftAISAlgaePlot <- renderPlotly({	
      swiftAISAlgaePlot()	
    })	
    # End Algae	
    # Start Turbity	
    
    ## InputData	
    swiftAISTurbData <- reactive({	
      swiftAquaData %>%	
        dplyr::filter(dateID> input$swiftAISDateRange[1] & dateID < input$swiftAISDateRange[2]) %>% # Filter by date selected	
        dplyr::filter(siteID %in% input$swiftAISSiteID) %>% # Filter by SiteID selected	
        dplyr::filter(sensorID %in% c("S1SONDETURBIDITY","S2SONDETURBIDITY")) # Filter to SensorID	
    })	
    ## Plot	
    swiftAISTurbPlot <- reactive({	
      ggplot(data = swiftAISTurbData(), aes(color = sensorID))+	
        geom_line(aes(x=dateID,y=reading),size = 1, na.rm = TRUE) +	
        scale_x_date(date_labels = "%b-%d") +	
        labs(x = "", y="")+ geom_point(aes(x=dateID,y=reading))+	
        facet_grid(siteID~sensorID,scales = "free_y")+	
        theme_light()	
    })	
    output$swiftAISTurbPlot <- renderPlotly({	
      swiftAISTurbPlot()	
    })	
    # End Turbidity	
    # Start FDOM	
    # Input Data	
    swiftAISFDOMData <- reactive({	
      swiftAquaData %>%	
        dplyr::filter(dateID> input$swiftAISDateRange[1] & dateID < input$swiftAISDateRange[2]) %>% # Filter by date selected	
        dplyr::filter(siteID %in% input$swiftAISSiteID) %>% # Filter by SiteID selected	
        dplyr::filter(unit == "quinineSulfateUnit") # Filter to SensorID	
    })	
    # Plot	
    swiftAISFDOMPlot <-reactive({	
      ggplot(data = swiftAISFDOMData(), aes(color = sensorID))+	
        geom_line(aes(x=dateID,y=reading),size = 1, na.rm = TRUE) +	
        scale_x_date(date_labels = "%b-%d") +	
        labs(x = "", y="")+ geom_point(aes(x=dateID,y=reading))+	
        facet_grid(siteID~sensorID,scales = "free_y")+	
        theme_light()	
    })	
    output$swiftAISFDOMPlot <- renderPlotly({	
      swiftAISFDOMPlot()	
    })	
    # End FDOM 	
    # Start SUNA Nitrate	
    ## InputData	
    swiftSUNAData <- reactive({	
      swiftAquaData %>%	
        dplyr::filter(dateID> input$swiftAISDateRange[1] & dateID < input$swiftAISDateRange[2]) %>% # Filter by date selected	
        dplyr::filter(siteID %in% input$swiftAISSiteID) %>% # Filter by SiteID selected	
        dplyr::filter(sensorID %in% "S2SUNAVALUE") # Filter to SensorID	
    })	
    ## Plot	
    swiftAISSUNAPlot <- reactive({	
      ggplot(data = swiftSUNAData(), aes(color = sensorID))+	
        geom_line(aes(x=dateID,y=reading),size = 1, na.rm = TRUE) +	
        scale_x_date(date_labels = "%b-%d") +	
        labs(x = "", y="")+ geom_point(aes(x=dateID,y=reading))+	
        facet_grid(siteID~sensorID,scales = "free_y")+	
        theme_light()	
    })	
    output$swiftAISSUNAPlot <- renderPlotly({	
      swiftAISSUNAPlot()	
    })	
    # End SUNA Nitrate	
    # Start Tempurature Data	
    ## InputData	
    swiftAISTempData <- reactive({	
      prt %>%	
        dplyr::filter(dateID> input$swiftAISDateRange[1] & dateID < input$swiftAISDateRange[2]) %>% # Filter by date selected	
        dplyr::filter(siteID %in% input$swiftAISSiteID) %>% # Filter by SiteID selected	
        dplyr::filter(sensorID %in% c("S1SONDETEMP","S2SONDETEMP","S1PRTTEMP","S2PRTTEMP","STAFFTROLLTEMP")) # Filter to SensorID	
    })	
    ## Plot	
    swiftAISTempPlot <- reactive({	
      ggplot(data = swiftAISTempData(), aes(color = sensorID))+	
        geom_line(aes(x=dateID,y=PRT),size = 1, na.rm = TRUE) +	
        scale_x_date(date_labels = "%b-%d") +	
        labs(x = "", y="")+ geom_point(aes(x=dateID,y=PRT))+	
        facet_grid(siteID~Location,scales = "free_y")+	
        theme_light()	
    })	
    output$swiftAISTempPlot <- renderPlotly({	
      swiftAISTempPlot()	
    })	
    # End Temperature Data	
    # Start Par Chunk	
    ## InputData	
    swiftAISTrollPARData <- reactive({	
      swiftAquaData %>%	
        dplyr::filter(dateID> input$swiftAISDateRange[1] & dateID < input$swiftAISDateRange[2]) %>% # Filter by date selected	
        dplyr::filter(siteID %in% input$swiftAISSiteID) %>% # Filter by SiteID selected	
        dplyr::filter(sensorID %in% c("S1PARVALUE","S2PARVALUE")) # Filter to SensorID	
    })	
    ## Plot	
    swiftAISTrollPARPlot <- reactive({	
      ggplot(data = swiftAISTrollPARData(), aes(color = sensorID))+	
        geom_line(aes(x=dateID,y=reading),size = 1, na.rm = TRUE) +	
        scale_x_date(date_labels = "%b-%d") +	
        labs(x = "", y="")+ geom_point(aes(x=dateID,y=reading))+	
        facet_grid(siteID~sensorID,scales = "free_y")+	
        theme_light()	
    })	
    output$swiftAISTrollPARPlot <- renderPlotly({	
      swiftAISTrollPARPlot()	
    })	
    
    ## End Troll PAR	
    ## Start Troll Level	
    ## InputData	
    swiftAISlevelData <- reactive({	
      swiftAquaData %>%	
        dplyr::filter(dateID> input$swiftAISDateRange[1] & dateID < input$swiftAISDateRange[2]) %>% # Filter by date selected	
        dplyr::filter(siteID %in% input$swiftAISSiteID) %>% # Filter by SiteID selected	
        dplyr::filter(sensorID %in% c("S1LEVELTROLLLEVEL","S2LEVELTROLLLEVEL")) # Filter to SensorID	
    })	
    ## Plot	
    swiftAISLevelPlot <- reactive({	
      ggplot(data = swiftAISlevelData(), aes(color = sensorID))+	
        geom_line(aes(x=dateID,y=reading),size = 1, na.rm = TRUE) +	
        scale_x_date(date_labels = "%b-%d") +	
        labs(x = "", y="")+ geom_point(aes(x=dateID,y=reading))+	
        facet_grid(siteID~sensorID,scales = "free_y")+	
        theme_light()	
    })	
    output$swiftAISLevelPlot <- renderPlotly({	
      swiftAISLevelPlot()	
    })	
    # End Level Troll	
    # Start Troll Temp	
    ## InputData	
    swiftAISTrollTempData <- reactive({	
      swiftAquaData %>%	
        dplyr::filter(dateID> input$swiftAISDateRange[1] & dateID < input$swiftAISDateRange[2]) %>% # Filter by date selected	
        dplyr::filter(siteID %in% input$swiftAISSiteID) %>% # Filter by SiteID selected	
        dplyr::filter(sensorID %in% c("S1LEVELTROLLTEMP","S2LEVELTROLLTEMP")) # Filter to SensorID	
    })	
    ## Plot	
    swiftAISTrollTempPlot <- reactive({	
      ggplot(data = swiftAISTrollTempData(), aes(color = sensorID))+	
        geom_line(aes(x=dateID,y=reading),size = 1, na.rm = TRUE) +	
        scale_x_date(date_labels = "%b-%d") +	
        labs(x = "", y="")+ geom_point(aes(x=dateID,y=reading))+	
        facet_grid(siteID~sensorID,scales = "free_y")+	
        theme_light()	
    })	
    output$swiftAISTrollTempPlot <- renderPlotly({	
      swiftAISTrollTempPlot()	
    })	
    # End Troll Temp	
    
    # Start Well Temp	
    ## InputData	
    swiftAISWellTempData <- reactive({	
      swiftAquaData %>%	
        dplyr::filter(dateID> input$swiftAISDateRange[1] & dateID < input$swiftAISDateRange[2]) %>% # Filter by date selected	
        dplyr::filter(siteID %in% input$swiftAISSiteID) %>% # Filter by SiteID selected	
        dplyr::filter(sensorID %in% c("AQUATROLLTEMPWELL1","AQUATROLLTEMPWELL2","AQUATROLLTEMPWELL3","AQUATROLLTEMPWELL4", # Filter to SensorID	
                                      "AQUATROLLTEMPWELL5","AQUATROLLTEMPWELL6","AQUATROLLTEMPWELL7","AQUATROLLTEMPWELL8"))	
    })	
    ## Plot	
    swiftAISWellTempPlot <- reactive({	
      ggplot(data = swiftAISWellTempData(), aes(color = sensorID))+	
        geom_line(aes(x=dateID,y=reading),size = 1, na.rm = TRUE) +	
        scale_x_date(date_labels = "%b-%d") +	
        labs(x = "", y="")+ geom_point(aes(x=dateID,y=reading))+	
        facet_grid(~siteID,scales = "free_y")+	
        theme_light()	
    })	
    output$swiftAISWellTempPlot <- renderPlotly({	
      swiftAISWellTempPlot()	
    })	
    # End Well Temp	
    # Start Well Cond	
    swiftAISWellCondData <- reactive({	
      swiftAquaData %>%	
        dplyr::filter(dateID> input$swiftAISDateRange[1] & dateID < input$swiftAISDateRange[2]) %>% # Filter by date selected	
        dplyr::filter(siteID %in% input$swiftAISSiteID) %>% # Filter by SiteID selected	
        dplyr::filter(sensorID %in% c("AQUATROLLCONDWELL1","AQUATROLLCONDWELL2","AQUATROLLCONDWELL3","AQUATROLLCONDWELL4", # Filter to SensorID	
                                      "AQUATROLLCONDWELL5","AQUATROLLCONDWELL6","AQUATROLLCONDWELL7","AQUATROLLCONDWELL8"))	
    })	
    ## Plot	
    swiftAISWellCondPlot <- reactive({	
      ggplot(data = swiftAISWellCondData(), aes(color = sensorID))+	
        geom_line(aes(x=dateID,y=reading),size = 1, na.rm = TRUE) +	
        scale_x_date(date_labels = "%b-%d") +	
        labs(x = "", y="")+ geom_point(aes(x=dateID,y=reading))+	
        facet_grid(~siteID,scales = "free_y")+	
        theme_light()	
    })	
    output$swiftAISWellCondPlot <- renderPlotly({	
      swiftAISWellCondPlot()	
    })	
    # End Well Cond	
    # Start Well Pressure	
    swiftAISWellPressureData <- reactive({	
      swiftAquaData %>%	
        dplyr::filter(dateID> input$swiftAISDateRange[1] & dateID < input$swiftAISDateRange[2]) %>% # Filter by date selected	
        dplyr::filter(siteID %in% input$swiftAISSiteID) %>% # Filter by SiteID selected	
        filter(sensorID %in% c("AQUATROLLPRESSUREWELL1","AQUATROLLPRESSUREWELL2","AQUATROLLPRESSUREWELL3","AQUATROLLPRESSUREWELL4", # Filter to SensorID	
                               "AQUATROLLPRESSUREWELL5","AQUATROLLPRESSUREWELL6","AQUATROLLPRESSUREWELL7","AQUATROLLPRESSUREWELL8"))	
    })	
    ## Plot	
    swiftAISWellPressurePlot <- reactive({	
      ggplot(data = swiftAISWellPressureData(), aes(color = sensorID))+	
        geom_line(aes(x=dateID,y=reading),size = 1, na.rm = TRUE) +	
        scale_x_date(date_labels = "%b-%d") +	
        labs(x = "", y="")+ geom_point(aes(x=dateID,y=reading))+	
        facet_grid(~siteID,scales = "free_y")+	
        theme_light()	
    })	
    output$swiftAISWellPressurePlot <- renderPlotly({	
      swiftAISWellPressurePlot()	
    })	
    # End Well Pressure	
    # Start Met Station Fan Speed	
    metSpeedVars <- c("METATSFanSpeed","METATSTach")	
    swiftAISMetSpeedData <- reactive({	
      swiftAquaData %>%	
        dplyr::filter(dateID> input$swiftAISDateRange[1] & dateID < input$swiftAISDateRange[2]) %>% # Filter by date selected	
        dplyr::filter(siteID %in% input$swiftAISSiteID) %>% # Filter by SiteID selected	
        dplyr::filter(sensorID %in% c("METATSFanSpeed","METATSTach")) # Filter to SensorID	
    })	
    ## Plot	
    swiftAISMetSpeedPlot <- reactive({	
      ggplot(data = swiftAISMetSpeedData(), aes(color = sensorID))+	
        geom_line(aes(x=dateID,y=reading),size = 1, na.rm = TRUE) +	
        scale_x_date(date_labels = "%b-%d") +	
        labs(x = "", y="")+ geom_point(aes(x=dateID,y=reading))+	
        facet_wrap(~siteID,scales = "free_y")+	
        theme_light()	
    })	
    output$swiftAISMetSpeedPlot <- renderPlotly({	
      swiftAISMetSpeedPlot()	
    })	
    # End Fan Speed	
    # Start PRT	
    
    
    ## InputData	
    swiftAISMetTempData <- reactive({	
      prt %>%	
        dplyr::filter(dateID> input$swiftAISDateRange[1] & dateID < input$swiftAISDateRange[2]) %>% # Filter by date selected	
        dplyr::filter(siteID %in% input$swiftAISSiteID) %>% # Filter by SiteID selected	
        dplyr::filter(sensorID %in% c("METHMPDew","METHMPTemp","MET2DWindTemp","METBaroTemp","METPRT","METNR01PRT"))
    })	
    ## Plot	
    swiftAISMetTempPlot <-reactive({	
      ggplot(data = swiftAISMetTempData(), aes(color = sensorID))+	
        geom_line(aes(x=dateID,y=PRT),size = 1, na.rm = TRUE) +	
        scale_x_date(date_labels = "%b-%d") +	
        labs(x = "", y="")+ geom_point(aes(x=dateID,y=PRT))+	
        facet_wrap(~siteID, scales = "free_y")+	
        theme_light()	
    })	
    output$swiftAISMetTempPlot <- renderPlotly({	
      swiftAISMetTempPlot()	
    })	
    
    # End PRT Data	
    # Start	
    # Met Rel Hum Input Data	
    swiftMetRHData <- reactive({	
      swiftAquaData %>%	
        dplyr::filter(dateID> input$swiftAISDateRange[1] & dateID < input$swiftAISDateRange[2]) %>% # Filter by date selected	
        dplyr::filter(siteID %in% input$swiftAISSiteID) %>% # Filter by SiteID selected	
        dplyr::filter(sensorID == "METHMPRH") # Filter to SensorID	
    })	
    ## Plot	
    swiftMetRHPlot <-reactive({	
      ggplot(data = swiftMetRHData(), aes(color = siteID))+	
        geom_line(aes(x=dateID,y=reading),size = 1, na.rm = TRUE) +	
        scale_x_date(date_labels = "%b-%d") +	
        labs(x = "", y="")+ 	
        geom_point(aes(x=dateID,y=reading))+	
        facet_wrap(~siteID)+	
        theme_light()	
    })	
    output$swiftMetRHPlot <- renderPlotly({	
      swiftMetRHPlot()	
    })	
    # End Rel Hum 	
    # Start Wind Vectors	
    # Wind Var Input Data	
    swiftMetWindData <- reactive({	
      swiftAquaData %>%	
        dplyr::filter(dateID> input$swiftAISDateRange[1] & dateID < input$swiftAISDateRange[2]) %>% # Filter by date selected	
        dplyr::filter(siteID %in% input$swiftAISSiteID) %>% # Filter by SiteID selected	
        dplyr::filter(sensorID %in% c("MET2DWindX","MET2DWindY")) # Filter to SensorID	
    })	
    ## Plot	
    swiftMetWindPlot <- reactive({	
      ggplot(data = swiftMetWindData(), aes(color = sensorID))+	
        geom_line(aes(x=dateID,y=reading),size = 1, na.rm = TRUE) +	
        scale_x_date(date_labels = "%b-%d") +	
        labs(x = "", y="")+ geom_point(aes(x=dateID,y=reading))+	
        facet_wrap(~siteID, scales = "free_y")+	
        theme_light()	
    })	
    output$swiftMetWindPlot <- renderPlotly ({	
      swiftMetWindPlot()	
    })	
    # End Wind Var	
    # Start Wind Speed of Sound	
    # Wind Speed of Sound Input Data	
    swiftMetWindSSData <- reactive({	
      swiftAquaData %>%	
        dplyr::filter(dateID> input$swiftAISDateRange[1] & dateID < input$swiftAISDateRange[2]) %>% # Filter by date selected	
        dplyr::filter(siteID %in% input$swiftAISSiteID) %>% # Filter by SiteID selected	
        dplyr::filter(sensorID %in% c("MET2DWindSS")) # Filter to SensorID	
    })	
    ## Plot	
    swiftMetWindSSPlot <- reactive({	
      ggplot(data = swiftMetWindSSData(), aes(color = sensorID))+	
        geom_line(aes(x=dateID,y=reading),size = 1, na.rm = TRUE) +	
        scale_x_date(date_labels = "%b-%d") +	
        labs(x = "", y="")+ geom_point(aes(x=dateID,y=reading))+	
        facet_wrap(~siteID, scales = "free_y")+	
        theme_light()	
    })	
    output$swiftMetWindSSPlot <- renderPlotly({	
      swiftMetWindSSPlot()	
    })	
    # End Speed of Sound plot	
    # Start Wind Status Code	
    # Wind Status Code Input Data	
    swiftMetWindSHData <- reactive({ 	
      swiftAquaData %>%	
        dplyr::filter(dateID> input$swiftAISDateRange[1] & dateID < input$swiftAISDateRange[2]) %>% # Filter by date selected	
        dplyr::filter(siteID %in% input$swiftAISSiteID) %>% # Filter by SiteID selected	
        dplyr::filter(sensorID %in% c("MET2DWindSH")) # Filter to SensorID	
    })	
    ## Plot	
    swiftMetWindSHPlot <- reactive({	
      ggplot(data = swiftMetWindSHData(), aes(color = sensorID))+	
        geom_line(aes(x=dateID,y=reading),size = 1, na.rm = TRUE) +	
        scale_x_date(date_labels = "%b-%d") +	
        labs(x = "", y="")+ geom_point(aes(x=dateID,y=reading))+	
        facet_wrap(~siteID, ncol = 1, scales = "free_y")+	
        theme_light()	
    })	
    output$swiftMetWindSHPlot <- renderPlotly({	
      swiftMetWindSHPlot()	
    })	
    # End Wind Health Code	
    # Start NR01 values	
    # NR01 Input Data	
    swiftMetNR01Data <- reactive({	
      swiftAquaData %>%	
        dplyr::filter(dateID> input$swiftAISDateRange[1] & dateID < input$swiftAISDateRange[2]) %>% # Filter by date selected	
        dplyr::filter(siteID %in% input$swiftAISSiteID) %>% # Filter by SiteID selected	
        dplyr::filter(sensorID %in% c("METNR01Up","METNR01Down","METNR01UpSW","METNR01DownSW")) # Filter to SensorID	
    })	
    ## Plot	
    swiftMetNR01Plot <- reactive({	
      ggplot(data = swiftMetNR01Data(), aes(color = sensorID))+	
        geom_line(aes(x=dateID,y=reading),size = 1, na.rm = TRUE) +	
        scale_x_date(date_labels = "%b-%d") +	
        labs(x = "", y="")+ geom_point(aes(x=dateID,y=reading))+	
        facet_wrap(~siteID, ncol = 1, scales = "free_y")+	
        theme_light()	
    })	
    output$swiftMetNR01Plot <- renderPlotly({	
      swiftMetNR01Plot()	
    })	
    # End NR01	
    # Start Met PAR	
    swiftMetPARData <- reactive({	
      swiftAquaData %>%	
        dplyr::filter(dateID> input$swiftAISDateRange[1] & dateID < input$swiftAISDateRange[2]) %>% # Filter by date selected	
        dplyr::filter(siteID %in% input$swiftAISSiteID) %>% # Filter by SiteID selected	
        dplyr::filter(sensorID %in% c("METPAR")) # Filter to SensorID	
    })	
    ## Plot	
    swiftMetPARPlot <- reactive({	
      ggplot(data = swiftMetPARData(), aes(color = sensorID))+	
        geom_line(aes(x=dateID,y=reading),size = 1, na.rm = TRUE) +	
        scale_x_date(date_labels = "%b-%d") +	
        labs(x = "", y="")+ geom_point(aes(x=dateID,y=reading))+	
        facet_wrap(~siteID, ncol = 1, scales = "free_y")+	
        theme_light()	
    })	
    output$swiftMetPARPlot <- renderPlotly({	
      swiftMetPARPlot()	
    })	
    # End Met PAR	
    # Start Met Barometer	
    # Met Baro Input data	
    swiftMetBaroPressureData <- reactive({	
      swiftAquaData %>%	
        dplyr::filter(dateID> input$swiftAISDateRange[1] & dateID < input$swiftAISDateRange[2]) %>% # Filter by date selected	
        dplyr::filter(siteID %in% input$swiftAISSiteID) %>% # Filter by SiteID selected	
        dplyr::filter(sensorID %in% c("METBaroPressure")) # Filter to SensorID	
    })	
    ## Plot	
    swiftMetBaroPressurePlot <- reactive({	
      ggplot(data = swiftMetBaroPressureData(), aes(color = sensorID))+	
        geom_line(aes(x=dateID,y=reading),size = 1, na.rm = TRUE) +	
        scale_x_date(date_labels = "%b-%d") +	
        labs(x = "", y="")+ geom_point(aes(x=dateID,y=reading))+	
        facet_wrap(~siteID, ncol = 1, scales = "free_y")+	
        theme_light()	
    })	
    output$swiftMetBaroPressurePlot <- renderPlotly({	
      swiftMetBaroPressurePlot()	
    })	
    # End Met Barometer	
    # Start Met Baro Status Code	
    # Met Baro Status Input	
    swiftMetBaroStatusData <- reactive({	
      swiftAquaData %>%	
        dplyr::filter(dateID> input$swiftAISDateRange[1] & dateID < input$swiftAISDateRange[2]) %>% # Filter by date selected	
        dplyr::filter(siteID %in% input$swiftAISSiteID) %>% # Filter by SiteID selected	
        dplyr::filter(sensorID %in% c("METBaroStatus")) # Filter to SensorID	
    })	
    ## Plot	
    swiftMetBaroStatusPlot <- reactive({	
      ggplot(data = swiftMetBaroStatusData(), aes(color = sensorID))+	
        geom_line(aes(x=dateID,y=reading),size = 1, na.rm = TRUE) +	
        scale_x_date(date_labels = "%b-%d") +	
        labs(x = "", y="")+ geom_point(aes(x=dateID,y=reading))+	
        facet_wrap(~siteID, ncol = 1, scales = "free_y")+	
        theme_light()	
    })	
    output$swiftMetBaroStatusPlot <- renderPlotly({	
      swiftMetBaroStatusPlot()	
    })	
    
    
    # Start Belfort 	
    # Belfort Data Input	
    swiftAISBelfort <- reactive({	
      swiftCO2 %>% 	
        dplyr::filter(sensorID %in% c("Belfort Heated", "Belfort")) %>% # Filter to SensorID	
        dplyr::filter(dateID> input$swiftAISDateRange[1] & dateID < input$swiftAISDateRange[2]) %>% # Filter by date selected	
        dplyr::filter(siteID %in% input$swiftAISSiteID) %>% # Filter by SiteID selected	
        dplyr::filter(readings > 0)  # Subset readings to reasonable values	
    })	
    # Belfort Plot	
    swiftAISBelPlot <- reactive({	
      ggplot(swiftAISBelfort(), aes(x=dateID,y=readings, color=as.factor(streamID)))  +	
        geom_point() +	
        geom_line()+	
        labs(title = '', x = "", y="", color = "Stream ID")+	
        facet_wrap(~siteID, ncol = 2)+	
        theme_light()	
    })	
    output$swiftAISBelPlot <- renderPlotly({	
      swiftAISBelPlot()	
    })
    
    ## WetDep
    swiftWetDep <- reactive({	
      swiftAquaData %>% 	
        dplyr::filter(sensorID %in% c("NADPPRTC", "METHMPTemp")) %>% # Filter to SensorID	
        dplyr::filter(dateID> input$swiftAISDateRange[1] & dateID < input$swiftAISDateRange[2]) %>% # Filter by date selected	
        dplyr::filter(siteID %in% input$swiftAISSiteID) %>% # Filter by SiteID selected	
        dplyr::filter(reading > 0)  # Subset readings to reasonable values	
    })	
    # Belfort Plot	
    swiftWetDepPlot <- reactive({	
      ggplot(swiftWetDep(), aes(x=dateID,y=reading, color=as.factor(sensorID)))  +	
        geom_point() +	
        geom_line()+	
        labs(title = '', x = "", y="", color = "Stream ID")+	
        facet_wrap(~siteID, ncol = 2)+	
        theme_light()	
    })	
    output$swiftWetDepPlot <- renderPlotly({	
      swiftWetDepPlot()	
    })
  }
})
