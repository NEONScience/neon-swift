shiny::observeEvent(input$menu, {
  if(input$menu == "swft_cvalfast_tab"){
    
    base::Sys.setenv(
      "AWS_S3_ENDPOINT"       = "neonscience.org",
      "AWS_DEFAULT_REGION"    = "s3.data"
    )
    
    base::source(paste0(swft.server.folder.path, "R/read.eddy.inquiry.swift.R"))
    
    swiftCylAssay = read.eddy.inquiry(dataType = "meta", sensor = "spanGas")  %>%
      reshape2::dcast(date + siteID ~ name, value.var = "conc") %>%
      dplyr::select(date,siteID,`ECSE-LOW`,`ECSE-MEDIUM`,`ECSE-HIGH`,`ECSE-Archive`,`ECTE-LOW`,`ECTE-MEDIUM`,`ECTE-HIGH`,`ECTE-Archive`)
    
    toListen <- shiny::reactive({
      list(input$swft_cval_site,input$cvalDateRange,input$swft_cval_sensor, input$cvalShape)
  })
  colorBlindPal <- c('#d73027','#f46d43','#fdae61','#fee090','#ffffbf','#e0f3f8','#abd9e9','#74add1','#4575b4') # Actual Colorblind Safe Pallette
  colorRegular <- c('#e41a1c', # Red
                    '#377eb8', # Blue
                    '#4daf4a', # Green
                    '#ff5100', # Burnd Orange
                    '#b700ff', # Paint Purple
                    '#242e42', # Dark Blue Gray
                    '#a65628', # Brown
                    '#ff52ad', # Pink
                    '#7a7a7a'  # Gray
  ) 
  shiny::observeEvent(toListen(), {
    
    if(input$swft_cval_sensor %in% c("Li7200", "G2131i", "Li840")){
      
      theme_set(theme_bw()) 
      
      cvalReadTime <- Sys.time()
      
      ### Grab the DATES!
      dateData <- shiny::reactive({
        all.cvals.meta = read.eddy.inquiry(dataType = "meta", sensor = "cval") %>%
          dplyr::filter(Sensor == "Li7200") %>%
          dplyr::filter(Site == "UNDE")
        
        read.eddy.inquiry(dataType = "meta", sensor = "cval") %>%
          dplyr::filter(Sensor == input$swft_cval_sensor) %>%
          dplyr::filter(Site == input$swft_cval_site)
      }) 
      
      output$swft_cval_react_unique_cvals = shiny::renderUI({
        shiny::selectInput('swft_cval_react_unique_cvals', 'Reactive Dates', base::sort(unique(dateData()$StartDateTime),decreasing = TRUE))
      })
    
        cvalFstInput <- shiny::reactive({
          req(input$swft_cval_react_unique_cvals, input$swft_cval_sensor, input$swft_cval_site)

          file.pull = dateData() %>%
            dplyr::filter(StartDateTime == input$swft_cval_react_unique_cvals)
          
          message(paste0(file.pull$Key[1]))
          if(is.na(paste0(file.pull$Key[1])) == TRUE){
            Sys.sleep(.5)
          }
          if(input$swft_cval_sensor == "G2131i"){
            cval <- aws.s3::s3read_using(FUN = fst::read.fst, bucket = "research-eddy-inquiry", object = paste0(file.pull$Key[1])) %>%
              dplyr::filter(strm_name == "G2131_fwMoleCo2") %>%
              dplyr::mutate(readout_val_double = round(readout_val_double, 2))
          }
          if(input$swft_cval_sensor == "Li840A"){
            cval <- aws.s3::s3read_using(FUN = fst::read.fst, bucket = "research-eddy-inquiry", object = paste0(file.pull$Key[1])) %>%
              dplyr::filter(strm_name == "Li840_CO2_fwMole") %>%
              dplyr::mutate(readout_val_double = round(readout_val_double, 2))
          }
          if(input$swft_cval_sensor == "L2130i"){
            # Read in all the data here, no need for filtering
            cval <- aws.s3::s3read_using(FUN = fst::read.fst, bucket = "research-eddy-inquiry", object = paste0(file.pull$Key[1]))  %>%
              dplyr::mutate(readout_val_double = round(readout_val_double, 2))
          }
          if(input$swft_cval_sensor == "Li7200"){
            # Read in all the data here, no need for filtering
            cval <- aws.s3::s3read_using(FUN = fst::read.fst, bucket = "research-eddy-inquiry", object = paste0(file.pull$Key[1])) %>%
              dplyr::filter(strm_name %in% c("Li7200_CO2","Li7200_MFCSampleFlow", "Li7200_leakCheckValve")) %>%
              dplyr::mutate(mean = round(mean, 2))
          }
          
          # If there is no data, make a blank table to graph.
          if(nrow(cval)>1){
            cval
          } else {
            # Create blank data frame to join all the site data together
            blankDataframe <- data.frame(matrix(ncol=8,nrow=0, dimnames=list(NULL, 
                                         c("SiteID","CvalType","TimeStamp","Co2","H2o","Temp","Pressure","Date"))))
            names(blankDataframe)<- c("SiteID","CvalType","TimeStamp","Co2","H2o","Temp","Pressure","Date")
            blankDataframe
          }
        })
        
        swiftCylAssayWideEcse <- shiny::reactive({
          # TODO Make this dynamically select the cylinder that was likely installed.
          # Will have to create a new data frame that creates a likely range the cylinders were installed
          swiftCylAssay %>%
            dplyr::filter(date == max(date) &
                          siteID == input$swft_cval_site) 
        })
        #### ECSE Span Gas Value Boxes
        output$swiftEcseLow <- shinydashboard::renderValueBox({
          shinydashboard::valueBox(
            value = paste0("Low: ", swiftCylAssayWideEcse()$`ECSE-LOW`[1]),
            subtitle = paste0("ECSE - ", swiftCylAssayWideEcse()$date[1]),
            width = 12,
            color = "orange"
          )
        })
        output$swiftEcseInt <- shinydashboard::renderValueBox({
          shinydashboard::valueBox(
            value = paste0("Int: ", swiftCylAssayWideEcse()$`ECSE-MEDIUM`[1]),
            subtitle = paste0("ECSE - ", swiftCylAssayWideEcse()$date[1]),
            width = 12,
            color = "red"
          )
        })
        output$swiftEcseHigh <- shinydashboard::renderValueBox({
          shinydashboard::valueBox(
            value = paste0("High: ", swiftCylAssayWideEcse()$`ECSE-HIGH`[1]),
            subtitle = paste0("ECSE - ", swiftCylAssayWideEcse()$date[1]),
            width = 12,
            color = "blue"
          )
        }) 
        
        #### ECTE Validation Value boxes
        
        swiftCylAssayWideEcte <- shiny::reactive({
          swiftCylAssay %>%
            dplyr::filter(date == max(date) &
                            siteID == input$swft_cval_site)
        })
        #### Ecte Span Gas Value Boxes
        output$swiftEcteLow <- shinydashboard::renderValueBox({
          shinydashboard::valueBox(
            value = paste0("Low: ", swiftCylAssayWideEcte()$`ECTE-LOW`),
            subtitle = paste0("ECTE - ", swiftCylAssayWideEcte()$date[1]),
            width = 12,
            color = "orange"
          )
        })
        output$swiftEcteInt <- shinydashboard::renderValueBox({
          shinydashboard::valueBox(
            value = paste0("Int: ", swiftCylAssayWideEcte()$`ECTE-MEDIUM`),
            subtitle = paste0("ECTE - ", swiftCylAssayWideEcte()$date[1]),
            width = 12,
            color = "red"
          )
        })
        output$swiftEcteHigh <- shinydashboard::renderValueBox({
          shinydashboard::valueBox(
            value = paste0("High: ", swiftCylAssayWideEcte()$`ECTE-HIGH`),
            subtitle = paste0("ECTE - ", swiftCylAssayWideEcte()$date[1]),
            width = 12,
            color = "blue"
          )
        })
        
        #### Co2 Analyzer Plots
        
        plot_co2_ecse  <- shiny::reactive({
          req(cvalFstInput())
          if(nrow(cvalFstInput())>1){

            # ALL Storage Analyzers
            if(input$swft_cval_sensor != "Li7200"){
            
              ggplot(cvalFstInput())+
                geom_hline(data = swiftCylAssayWideEcse(),  aes(yintercept = `ECSE-HIGH`),   alpha = .65, linetype = 3,size= 1.1, color = "blue")+
                geom_hline(data = swiftCylAssayWideEcse(),  aes(yintercept = `ECSE-MEDIUM`), alpha = .65, linetype = 3,size= 1.1, color = "red")+
                geom_hline(data = swiftCylAssayWideEcse(),  aes(yintercept = `ECSE-LOW`),    alpha = .65, linetype = 3,size= 1.1, color = "orange")+
                scale_x_datetime(date_breaks = "2 mins", date_labels = "%H:%M")+
                scale_y_continuous(breaks = scales::pretty_breaks(n = 9)) +
                geom_line(aes(x=readout_time, y=readout_val_double), color = "black") +
                labs(x="",y="Co2 (PPM)",color = "Stream Type",title = paste0(input$swft_cval_site," ", input$swft_cval_sensor, " ", input$swft_cval_react_unique_cvals))
            } else {
              # Turbulent Analyzer
              ggplot(cvalFstInput() %>% dplyr::filter(strm_name %in% c("Li7200_CO2")))+
                geom_hline(data = swiftCylAssayWideEcte(),  aes(yintercept = `ECTE-HIGH`),   alpha = .65, linetype = 3,size= 1.1, color = "blue")+
                geom_hline(data = swiftCylAssayWideEcte(),  aes(yintercept = `ECTE-MEDIUM`), alpha = .65, linetype = 3,size= 1.1, color = "red")+
                geom_hline(data = swiftCylAssayWideEcte(),  aes(yintercept = `ECTE-LOW`),    alpha = .65, linetype = 3,size= 1.1, color = "orange")+
                geom_line(aes(x=timestamp, y=mean),color = "black") +
                scale_x_datetime(date_breaks = "2 mins", date_labels = "%H:%M")+
                scale_y_continuous(breaks = scales::pretty_breaks(n = 9)) +
                labs(x="",y="Co2 (PPM)",color = "Stream Type",title = paste0(input$swft_cval_site," ", input$swft_cval_sensor, " Validation on ", as.Date(input$swft_cval_react_unique_cvals, format = "%Y-%m-%d", origin = "1970-01-01")))
            }

          } else {
            ggplot()+
              geom_text(label = "text")+
              annotate("text", label = "NO DATA\n(Input plot terms above)", x = 0, y = 0, color = "black")+
              theme_minimal()
          } 
          
        })
        
        plot_co2_ecte_leak_check = shiny::reactive({
          # ALL Storage Analyzers
          if(input$swft_cval_sensor == "Li7200"){
            
            if(input$swft_cval_react_unique_cvals > as.Date("2021-02-11")){
              cvalFstInput() %>%
                dplyr::filter(strm_name %in% c("Li7200_MFCSampleFlow","Li7200_leakCheckValve")) %>%
                reshape2::dcast(timestamp ~ strm_name, value.var = "mean") %>%
                dplyr::filter(Li7200_leakCheckValve == 1) %>%
                reshape2::melt(id.vars = "timestamp",value.name = "mean") %>%
                ggplot()+
                  geom_line(aes(x=timestamp, y=mean, color = variable)) +
                  scale_x_datetime(date_breaks = "2 mins", date_labels = "%H:%M")+
                  scale_y_continuous(breaks = scales::pretty_breaks(n = 9)) +
                  theme(legend.position = "none") +
                  labs(x="",y="Flow Rate (SLPM) \t\t Valve Status",color = "Stream Type",title = paste0(input$swft_cval_site," ", input$swft_cval_sensor, " Leak Check ", input$swft_cval_react_unique_cvals)) +
                  facet_wrap(~variable, scales = "free")
            } else {
              ggplot()+
                geom_text(label = "text")+
                annotate("text", label = "No Validation Check data for this time period", x = 0, y = 0, color = "black")+
                theme_minimal()
            }
            
            
          } else {
            ggplot()+
              geom_text(label = "text")+
              annotate("text", label = "NO DATA\n(Input plot terms above)", x = 0, y = 0, color = "black")+
              theme_minimal()
          }
          
        })
        
        output$plot_co2_ecse <- plotly::renderPlotly({
          plot_co2_ecse()
        })
        
        output$plot_co2_ecte_leak_check <- plotly::renderPlotly({
          plot_co2_ecte_leak_check()
        })
        
        output$table_co2_ecse <- DT::renderDT(
          DT::datatable(
            data = cvalFstInput(),
            filter = "top",
            options = list(
              deferRender = TRUE,
              scrollY = 300,
              scrollCollapse = TRUE,
              scrollX = TRUE,
              paging = TRUE),rownames = FALSE) 
        )
        output$table_ecse_span <- DT::renderDT(
          DT::datatable(
            data = swiftCylAssayWideEcse(),
            filter = "none",
            options = list(
              deferRender = FALSE,
              scrollY = 300,
              scrollCollapse = TRUE,
              scrollX = FALSE,
              paging = FALSE),rownames = FALSE) 
        )
      cvalEndTime <- Sys.time()
      cvalLoadTime <- round(difftime(cvalEndTime, cvalReadTime, units = "secs") *1000,2)
    }
  })
  }
})
