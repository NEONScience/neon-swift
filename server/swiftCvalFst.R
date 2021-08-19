shiny::observeEvent(input$menu, {
  if(input$menu == "swft_cvalfast_tab"){
    
    # Set S3 Endpoints for safety
    base::Sys.setenv(
      "AWS_S3_ENDPOINT"       = "neonscience.org",
      "AWS_DEFAULT_REGION"    = "s3.data"
    )
    
    # Source the base data pulling function
    base::source(paste0(swft.server.folder.path, "R/read.eddy.inquiry.swift.R"))
    
    # Cylinder Metadata
    swiftCylAssay = read.eddy.inquiry(dataType = "meta", sensor = "spanGas")  %>%
      reshape2::dcast(date + siteID ~ name, value.var = "conc") %>%
      dplyr::select(date,siteID,`ECSE-LOW`,`ECSE-MEDIUM`,`ECSE-HIGH`,`ECSE-Archive`,`ECTE-LOW`,`ECTE-MEDIUM`,`ECTE-HIGH`,`ECTE-Archive`)
    
    # Aesthetics
    ggplot2::theme_set(ggdark::dark_theme_gray()) 
    # Custom color palettes' for cylinders
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
    
    
    toListen <- shiny::reactive({
      list(input$swft_cval_site,input$cvalDateRange,input$swft_cval_sensor, input$cvalShape)
    })
    
    # this Observe event looks for any changes to the list of input in the toListen call
    shiny::observeEvent(toListen(), {
      message(input$swft_cval_sensor)
    
    
    ###                                     Span Gas Value Boxes and Associated Logics                                    ###
    
    
    
    ###                                     Total CVAL Counts and Associated Logics                                    ###
    
    # Display total CVAL's on file
    uniqueDates = read.eddy.inquiry(dataType = "meta", sensor = "cval")
    
    #### Total CVALS on record
    output$swft_cval_total_records <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        subtitle = "Total CVALs on record",
        value = paste0(prettyNum(nrow(uniqueDates), big.mark=",",scientific=FALSE)),
        width = 12,
        color = "yellow"
      )
    })
    #### Total G2131 CVALS on record
    output$swft_cval_g2131_records <- shinydashboard::renderValueBox({
      uniqueDates.G2131i = uniqueDates %>%
        dplyr::filter(Sensor == "G2131i")
      shinydashboard::valueBox(
        subtitle = "Total G2131i CVALs",
        value = paste0(prettyNum(nrow(uniqueDates.G2131i), big.mark=",",scientific=FALSE)),
        width = 12,
        color = "black"
      )
    })
    #### Total Li840 CVALS on record
    output$swft_cval_li840_records <- shinydashboard::renderValueBox({
      uniqueDates.Li840A = uniqueDates %>%
        dplyr::filter(Sensor == "Li840A")
      shinydashboard::valueBox(
        subtitle = "Total Li840A CVALs",
        value = paste0(prettyNum(nrow(uniqueDates.Li840A), big.mark=",",scientific=FALSE)),
        width = 12,
        color = "black"
      )
    })
    #### Total Li7200 CVALS on record
    output$swft_cval_li7200_records <- shinydashboard::renderValueBox({
      uniqueDates.Li7200 = uniqueDates %>%
        dplyr::filter(Sensor == "Li7200")
      shinydashboard::valueBox(
        subtitle = "Total Li7200 CVALs",
        value = paste0(prettyNum(nrow(uniqueDates.Li7200), big.mark=",",scientific=FALSE)),
        width = 12,
        color = "black"
      )
    })
    #### Total L2130 CVALS on record
    output$swft_cval_l2130_records <- shinydashboard::renderValueBox({
      uniqueDates.L2130 = uniqueDates %>%
        dplyr::filter(Sensor == "L2130i")
      shinydashboard::valueBox(
        subtitle = "Total L2130i CVALs",
        value = paste0(prettyNum(nrow(uniqueDates.L2130), big.mark=",",scientific=FALSE)),
        width = 12,
        color = "black"
      )
    })
    
    
    uniqueDates_site = shiny::reactive({
      uniqueDates %>% dplyr::filter(Site == input$swft_cval_site)
    })
    
    #### ECSE Span Gas Value Boxes
    output$swft_cval_site_records <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        subtitle = paste0("Total ", input$swft_cval_site, " CVALs"),
        value = paste0(prettyNum(nrow(uniqueDates_site()), big.mark=",",scientific=FALSE)),
        width = 12,
        color = "maroon"
      )
    })
    

      
      # This function checks the Site/Sensor inputs and returns a list of available CVAL's to the end users
      dateData <- shiny::reactive({
        # Read in CVAL's filtered by Sensor/Site
        uniqueDates.specific = uniqueDates %>%
          dplyr::filter(Sensor == input$swft_cval_sensor) %>%
          dplyr::filter(Site == input$swft_cval_site)
        
        message(paste0(input$swft_cval_site, " has ", input$swft_cval_sensor, " ", nrow(uniqueDates.specific)))
        
        # Return the data to the reactive call dateData
        uniqueDates.specific
        
      }) 

      
      # Output the available cval dates for the site/sensor
      output$swft_cval_react_unique_cvals = shiny::renderUI({
        shiny::req(dateData())
        if(nrow(dateData()) > 1){
          shiny::selectInput('swft_cval_react_unique_cvals', 'Reactive CVAL Times', base::sort(unique(dateData()$StartDateTime),decreasing = TRUE))
        } else if(input$swft_cval_sensor == "L2130i"){
          shiny::textInput('swft_cval_react_unique_cvals', 'Reactive CVAL Times', placeholder =  paste0("This site has 0 CVALS for ", input$swft_cval_sensor ," on file. Only Core sites have this dataset."))
        } else {
          shiny::textInput('swft_cval_react_unique_cvals', 'Reactive CVAL Times', placeholder =  paste0("This site has 0 CVALS for ", input$swft_cval_sensor ," on file..."))
        }
      })
      
      
      # Load in Value boxes based upon the Site/Sensor (ECSEvsECTE)
    swiftCylAssayWideEcse <- shiny::reactive({
      shiny::req(input$swft_cval_site, input$swft_cval_react_unique_cvals)
      
      # Get the closet date for which we have cval span gas values
      swift_cyl_assay = swiftCylAssay %>%
        dplyr::filter(siteID == input$swft_cval_site) %>% 
        dplyr::mutate(closeness = abs(difftime(base::as.Date(input$swft_cval_react_unique_cvals, orgin = "1970-01-01"), date, units = "days"))) %>% 
        dplyr::filter(closeness == min(closeness, na.rm = TRUE)) %>% 
        dplyr::select(date, siteID, `ECSE-LOW`, `ECSE-MEDIUM`, `ECSE-HIGH`, `ECSE-Archive`)
      
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
    #### ECSE Span Gas Value Boxes
    output$swiftEcseLow2 <- shinydashboard::renderValueBox({
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
    output$swiftEcseInt2 <- shinydashboard::renderValueBox({
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
    output$swiftEcseHigh2 <- shinydashboard::renderValueBox({
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
        dplyr::filter(siteID == input$swft_cval_site) %>% 
        dplyr::mutate(closeness = abs(difftime(base::as.Date(input$swft_cval_react_unique_cvals, orgin = "1970-01-01"), date, units = "days"))) %>% 
        dplyr::filter(closeness == min(closeness, na.rm = TRUE)) %>% 
        dplyr::select(date, siteID, `ECTE-LOW`, `ECTE-MEDIUM`, `ECTE-HIGH`, `ECTE-Archive`)
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
    output$table_ecte_span <- DT::renderDT(
      DT::datatable(
        data = swiftCylAssayWideEcte(),
        filter = "none",
        options = list(
          deferRender = FALSE,
          scrollY = 300,
          scrollCollapse = TRUE,
          paging = FALSE),rownames = FALSE) 
    )
      
      #### ECSE Span Gas Value Boxes
      output$swft_cval_site_sensor_records <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(
          subtitle = paste0("Total ",input$swft_cval_site, " ",input$swft_cval_sensor, " CVALs"),
          value = paste0(prettyNum(nrow(dateData()), big.mark=",",scientific=FALSE)),
          width = 12,
          color = "maroon"
        )
      })
      
      cvalReadTime <- Sys.time()
      
      ###                                     Data Input From User and Associated Logics                                    ###
      
      cvalFstInput <- shiny::reactive({
        req(input$swft_cval_react_unique_cvals, input$swft_cval_sensor, input$swft_cval_site)

        file.pull = dateData() %>%
          dplyr::filter(StartDateTime == input$swft_cval_react_unique_cvals) %>% 
          dplyr::distinct()
        
        message(paste0(file.pull$Key[1]))
        
        if(input$swft_cval_sensor == "G2131i"){
          if(is.na(file.pull$Key[1]) == FALSE){
            cval <- aws.s3::s3read_using(FUN = fst::read.fst, bucket = "research-eddy-inquiry", object = paste0(file.pull$Key[1])) %>%
              dplyr::mutate(strm_name = ifelse(test = strm_name == "G2131_13Co2_wet", yes = "G2131_fwMoleCo2", no = strm_name)) %>% 
              dplyr::filter(strm_name == "G2131_fwMoleCo2") %>%
              dplyr::mutate(readout_val_double = round(readout_val_double, 3)) %>%
              dplyr::select(meas_strm_name, strm_name, readout_time, readout_val_double)
          } else {
            # Key is NA, giving empty table
            cval = data.table::data.table()
          }
        }
        if(input$swft_cval_sensor == "Li840A"){
          if(is.na(file.pull$Key[1]) == FALSE){
            cval <- aws.s3::s3read_using(FUN = fst::read.fst, bucket = "research-eddy-inquiry", object = paste0(file.pull$Key[1])) %>%
              dplyr::filter(strm_name == "Li840_CO2_fwMole") %>%
              dplyr::mutate(readout_val_double = round(readout_val_double, 2))%>%
              dplyr::select(meas_strm_name, strm_name, readout_time, readout_val_double)
          } else {
            # Key is NA, giving empty table
            cval = data.table::data.table()
          }
        }
        if(input$swft_cval_sensor == "L2130i"){
          if(is.na(file.pull$Key[1]) == FALSE){
            # Read in all the data here, no need for filtering
            cval <- aws.s3::s3read_using(FUN = fst::read.fst, bucket = "research-eddy-inquiry", object = paste0(file.pull$Key[1])) %>%
              dplyr::mutate(readout_time = cut(readout_time, breaks = "10 secs")) %>%
              dplyr::mutate(readout_time = lubridate::ymd_hms(readout_time)) %>%
              dplyr::mutate(readout_val_double = round(readout_val_double, 2)) %>%
              dplyr::group_by(strm_name, readout_time) %>%
              dplyr::summarise(
                readout_val_double = mean(readout_val_double)
              )%>%
              dplyr::select(strm_name, readout_time, readout_val_double)
          } else {
            # Key is NA, giving empty table
            cval = data.table::data.table()
          }
        }
        if(input$swft_cval_sensor == "Li7200"){
          if(is.na(file.pull$Key[1]) == FALSE){
            # Read in all the data here, no need for filtering
            cval <- aws.s3::s3read_using(FUN = fst::read.fst, bucket = "research-eddy-inquiry", object = paste0(file.pull$Key[1])) %>%
              dplyr::filter(strm_name %in% c("Li7200_CO2","Li7200_MFCSampleFlow", "Li7200_leakCheckValve")) %>%
              dplyr::mutate(readout_val_double = round(mean, 2)) %>%
              dplyr::select(-mean)
          } else {
            # Key is NA, giving empty table
            cval = data.table::data.table()
          }
        }
        # If there is no data, make a blank table to graph.
        if(nrow(cval)>1){
          cval
        } else {
          # Create blank data frame to pass to the next logic step where it will be kicked out
          data.table::data.table()
        }
      })
          
      ###                                     Plotting Reactive Data from Above and Associated Logics                                    ###  
      
      plot_co2_ecse  <- shiny::reactive({
        req(cvalFstInput())
        # Begin logical edge casing
        if(nrow(cvalFstInput()) > 1){

          # ALL Storage Analyzers
          if(input$swft_cval_sensor %in% c("Li840A", "G2131i")){
          
            ggplot2::ggplot(cvalFstInput())+
              ggplot2::geom_hline(data = swiftCylAssayWideEcse(),  aes(yintercept = `ECSE-HIGH`),   alpha = .65, linetype = 3,size= 1.1, color = "blue")+
              ggplot2::geom_hline(data = swiftCylAssayWideEcse(),  aes(yintercept = `ECSE-MEDIUM`), alpha = .65, linetype = 3,size= 1.1, color = "red")+
              ggplot2::geom_hline(data = swiftCylAssayWideEcse(),  aes(yintercept = `ECSE-LOW`),    alpha = .65, linetype = 3,size= 1.1, color = "orange")+
              ggplot2::scale_x_datetime(date_breaks = "2 mins", date_labels = "%H:%M")+
              ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 9)) +
              ggplot2::geom_line(aes(x=readout_time, y=readout_val_double), color = "white") +
              ggplot2::labs(x="",y="Co2 (PPM)",color = "Stream Type",title = paste0(input$swft_cval_site," ", input$swft_cval_sensor, " ", input$swft_cval_react_unique_cvals))
          
          # Turbulent Analyzer
          } else if(input$swft_cval_sensor == "Li7200"){
            ggplot2::ggplot(cvalFstInput() %>% dplyr::filter(strm_name %in% c("Li7200_CO2")))+
              ggplot2::geom_hline(data = swiftCylAssayWideEcte(),  aes(yintercept = `ECTE-HIGH`),   alpha = .65, linetype = 3,size= 1.1, color = "blue")+
              ggplot2::geom_hline(data = swiftCylAssayWideEcte(),  aes(yintercept = `ECTE-MEDIUM`), alpha = .65, linetype = 3,size= 1.1, color = "red")+
              ggplot2::geom_hline(data = swiftCylAssayWideEcte(),  aes(yintercept = `ECTE-LOW`),    alpha = .65, linetype = 3,size= 1.1, color = "orange")+
              ggplot2::geom_line(aes(x=timestamp, y=readout_val_double),color = "white") +
              ggplot2::scale_x_datetime(date_breaks = "2 mins", date_labels = "%H:%M")+
              ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 9)) +
              ggplot2::labs(x="",y="Co2 (PPM)",color = "Stream Type",title = paste0(input$swft_cval_site," ", input$swft_cval_sensor, " Validation on ", as.Date(input$swft_cval_react_unique_cvals, format = "%Y-%m-%d", origin = "1970-01-01")))
          } else if(input$swft_cval_sensor == "L2130i"){
          # Water Isotopes
            if(input$swft_cval_l2130_options == "L2130_H2O"){
              # H2O
              ggplot2::ggplot(cvalFstInput() %>% dplyr::filter(strm_name == input$swft_cval_l2130_options), aes(x = readout_time, y = readout_val_double)) +
                ggplot2::geom_hline(aes(yintercept= 20000),linetype = 5, size = 1, color = "green") + 
                ggplot2::geom_hline(aes(yintercept= 18000),linetype = 5, size = 1, color = "#ffea00") + 
                ggplot2::geom_hline(aes(yintercept= 22000),linetype = 5, size = 1, color = "#ffea00") + 
                ggplot2::geom_point(size = 1, alpha = .8, shape = 1, color = "cyan") +
                ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 8.5)) +
                ggplot2::scale_x_datetime(date_breaks = "15 mins", date_labels = ("%m-%d\n%H:%M")) +
                ggplot2::theme(legend.position = "none")+
                ggplot2::labs(title = paste0(input$swft_cval_site,": ", input$swft_cval_l2130_options, " Validation " ,as.Date(cvalFstInput()$readout_time[1], format = "%Y-%m-%d")), subtitle = expression(paste(H[2],O)), x = "", y = "Concentration (ppm)")
              
            } else if(input$swft_cval_l2130_options == "L2130_18O_isotope") {
              # 18O Isotope
              ggplot2::ggplot(cvalFstInput() %>% dplyr::filter(strm_name == input$swft_cval_l2130_options), aes(x = readout_time, y = readout_val_double)) +
                ggplot2::geom_hline(aes(yintercept=  -4.22407709444),linetype = 5, size = 1, color = "red") +     # Red #1 18-O Vial
                ggplot2::geom_hline(aes(yintercept= -18.38043365275),linetype = 5, size = 1, color = "#ffea00") + # Yellow #2 18-O Vial
                ggplot2::geom_hline(aes(yintercept= -27.77007105211),linetype = 5, size = 1, color = "blue") +    # Blue #4 18-O Vial
                ggplot2::geom_point(size = 1, alpha = .8, shape = 1, color = "pink") + 
                ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 10), sec.axis = dup_axis(name = "", breaks = c(-4.22, -18.38, -27.77))) +
                ggplot2::scale_x_datetime(date_breaks = "15 mins", date_labels = ("%m-%d\n%H:%M")) +
                ggplot2::theme(legend.position = "none")+
                ggplot2::labs(title = paste0(input$swft_cval_site,": ", input$swft_cval_l2130_options, " Validation " ,as.Date(cvalFstInput()$readout_time[1], format = "%Y-%m-%d")), subtitle = expression(paste(H[2],O)), x = "", y = "Concentration (per mil)")
              
            } else if(input$swft_cval_l2130_options == "L2130_2H_isotope") {
              # 2H Isotope
              ggplot2::ggplot(cvalFstInput() %>% dplyr::filter(strm_name == input$swft_cval_l2130_options) , aes(x = readout_time, y = readout_val_double)) +
                ggplot2::geom_hline(aes(yintercept=  -29.17558629625),linetype = 5, size = 1, color = "red") +     # Red #1 18-O Vial
                ggplot2::geom_hline(aes(yintercept= -136.51575175443),linetype = 5, size = 1, color = "#ffea00") + # Yellow #2 18-O Vial
                ggplot2::geom_hline(aes(yintercept= -217.66690584745),linetype = 5, size = 1, color = "blue") +    # Blue #4 18-O Vial
                ggplot2::geom_point(size = 1, alpha = .8, shape = 1, color = "white") +
                ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 10), sec.axis = dup_axis(name = "", breaks = c(-29.17, -136.51, -217.66))) +
                ggplot2::scale_x_datetime(date_breaks = "15 mins", date_labels = ("%m-%d\n%H:%M")) +
                ggplot2::theme(legend.position = "none")+
                ggplot2::labs(title = paste0(input$swft_cval_site,": ", input$swft_cval_l2130_options, " Validation " , as.Date(cvalFstInput()$readout_time[1], format = "%Y-%m-%d")), subtitle = expression(paste(H[2],O)), x = "", y = "Concentration (per mil)")
              
            } else {
              # message("doing nothing just like you asked")
            }
          }
        } else {
          # message("doing nothing just like you asked")
        } 
      })
      # Immediately output plot after reactive is completed
      output$plot_co2_ecse <- plotly::renderPlotly({
        plot_co2_ecse()
      })
      
      # Reactive Plot for ECTE Leak Checks
      plot_co2_ecte_leak_check = shiny::reactive({
        # Only for ECTE Leak Checks
        if(input$swft_cval_sensor == "Li7200"){
          # Data must be available and greater than 0
          if(input$swft_cval_react_unique_cvals > as.Date("2021-01-01") & nrow(cvalFstInput()) > 0){
            cval.test.take.1 = cvalFstInput() %>%
              dplyr::filter(strm_name %in% c("Li7200_MFCSampleFlow","Li7200_leakCheckValve")) %>%
              reshape2::dcast(timestamp ~ strm_name, value.var = "readout_val_double") %>%
              dplyr::filter(Li7200_leakCheckValve == 1) %>%
              reshape2::melt(id.vars = "timestamp",value.name = "readout_val_double") 
            
            ggplot(cval.test.take.1)+
              geom_line(aes(x=timestamp, y=readout_val_double, color = variable)) +
              scale_x_datetime(date_breaks = "2 mins", date_labels = "%H:%M")+
              scale_y_continuous(breaks = scales::pretty_breaks(n = 9)) +
              geom_hline(yintercept = 0, color = "white", linetype = "dashed") +
              theme(legend.position = "none") +
              labs(x="",y="Flow Rate (SLPM) \t\t Valve Status",color = "Stream Type",title = paste0(input$swft_cval_site," ", input$swft_cval_sensor, " Leak Check ", input$swft_cval_react_unique_cvals)) +
              facet_wrap(~variable, scales = "free")
          } else {
            # If data is not available or = to zero, provide this plot
            ggplot()+
              geom_text(label = "text")+
              annotate("text", label = "No Validation Check data for this time period", x = 0, y = 0, color = "white", size = 12)
          }
        } else {
          # message("doing nothing just like you asked")
        }
        
      })
      # Immediately out put this plot once reactive plot is completed
      output$plotly_co2_ecte_leak_check <- plotly::renderPlotly({
        plot_co2_ecte_leak_check()
      })
      
      # Since we are done with the cvalFstInput() reactive lets output the table
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
    
      # Now we are finished let calculate how long it took.
      cvalEndTime <- Sys.time()
      cvalLoadTime <- round(difftime(cvalEndTime, cvalReadTime, units = "secs") *1000,2)
    }) # End of Listening
  }
})