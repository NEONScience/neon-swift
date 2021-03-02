# swftEddyCo

shiny::observeEvent(input$menu, {
  if(input$menu == "swft_ecfast_tab"){
    
    ## Guiding Principles
    # We will plot 2-min data. 
    
    # We dynamically auto aggregate if user inputs date range
    
    # Lets make this as simple as possible
    
    base::source(paste0(swft.server.folder.path, "R/read.eddy.inquiry.swift.R"))
    
    swft.tis.site.meta <- data.table::fread(paste0(swft.server.folder.path, "data/lookup/tis.site.meta.csv"))

    # Plot Data
    swft_ec_fast_plot = shiny::eventReactive(input$swft_ec_fast_actionButton,{
      shiny::req(input$swft_EddyCo_data_type, input$swft_EddyCo_site, input$swft_EddyCo_date_range)
      
      #################################################                            Collecting Data Logic                            #################################################
      swft_ec_fast_collect_data_time_start = Sys.time()
      
      
      if(input$swft_EddyCo_data_type != "CO2" & input$swft_EddyCo_data_type != "H2O"){
      # Pull data from S3
      message(input$swft_EddyCo_site, " from ", input$swft_EddyCo_date_range[1], " - ", input$swft_EddyCo_date_range[2], paste0(" Swft Data Gather Starting ... "))
      swft.data.in = read.eddy.inquiry(dataType  = "2min", 
                                       sensor    = input$swft_EddyCo_data_type, 
                                       siteID    = input$swft_EddyCo_site,
                                       startDate = as.Date(input$swft_EddyCo_date_range[1]), 
                                       endDate   = as.Date(input$swft_EddyCo_date_range[2]),
                                       silent    = TRUE
      ) 
      
      if(nrow(swft.data.in) > 0 & input$swft_EddyCo_data_type != "CSAT3") { 
        swft.data.in = swft.data.in %>% dplyr::mutate(`Stream Name` = strm_name) %>% dplyr::select(-strm_name) 
      }
      message(input$swft_EddyCo_site, " - ", input$swft_EddyCo_data_type, " data from ", input$swft_EddyCo_date_range[1], " - ", input$swft_EddyCo_date_range[2], paste0(" Swft Data Gathered: ", nrow(swft.data.in)))
      message("Data pull Finished")
      } else if(input$swft_EddyCo_data_type == "CO2") {
        
        li840.data <- read.eddy.inquiry(dataType = "2min", sensor = "Li840", siteID = input$swft_EddyCo_site, startDate = input$swft_EddyCo_date_range[1], endDate = input$swft_EddyCo_date_range[2], silent = TRUE)
        if(nrow(li840.data) > 0){
          li840.data <- li840.data %>% dplyr::mutate(`Stream Name` = strm_name) %>% dplyr::select(-strm_name) %>%
            dplyr::filter(`Stream Name` == "Li840_CO2_fwMole") 
          
        }
        g2131.data <- read.eddy.inquiry(dataType = "2min", sensor = "G2131", siteID = input$swft_EddyCo_site, startDate = input$swft_EddyCo_date_range[1], endDate = input$swft_EddyCo_date_range[2], silent = TRUE)
        if(nrow(g2131.data) > 0){
          g2131.data <- g2131.data %>% dplyr::mutate(`Stream Name` = strm_name) %>% dplyr::select(-strm_name) %>%
            dplyr::filter(`Stream Name` == "G2131_fwMoleCo2")
        }
        li7200.data <- read.eddy.inquiry(dataType = "2min", sensor = "Li7200", siteID = input$swft_EddyCo_site, startDate = input$swft_EddyCo_date_range[1], endDate = input$swft_EddyCo_date_range[2], silent = TRUE)
        if(nrow(li7200.data) > 0){
          li7200.data <- li7200.data %>% dplyr::mutate(`Stream Name` = strm_name) %>% dplyr::select(-strm_name) %>%
            dplyr::filter(`Stream Name` == "Li7200_CO2")
        }
        swft.data.in <- rbindlist(l = list(li840.data, g2131.data, li7200.data))
        rm(li840.data, g2131.data, li7200.data)
        
      } else if(input$swft_EddyCo_data_type == "H2O") {
        
        message("\t\t   Is anybody out ther! ?1")
        
        li840.h2o.data <- read.eddy.inquiry(dataType = "2min", sensor = "Li840", siteID = input$swft_EddyCo_site, startDate = input$swft_EddyCo_date_range[1], endDate = input$swft_EddyCo_date_range[2], silent = TRUE)
        # li840.h2o.data <- read.eddy.inquiry(dataType = "2min", sensor = "Li840", siteID = "UNDE", startDate = Sys.Date()-7, endDate = Sys.Date(), silent = TRUE)
        if(nrow(li840.h2o.data) > 0){
          li840.h2o.data <- li840.h2o.data %>% dplyr::mutate(`Stream Name` = strm_name) %>% dplyr::select(-strm_name) %>%
            dplyr::filter(`Stream Name` == "Li840_H2O_fwMole")
        }
        # g2131.h2o.data <- read.eddy.inquiry(dataType = "2min", sensor = "G2131", siteID = "UNDE", startDate = Sys.Date()-7, endDate = Sys.Date(), silent = TRUE)
        g2131.h2o.data <- read.eddy.inquiry(dataType = "2min", sensor = "G2131", siteID = input$swft_EddyCo_site, startDate = input$swft_EddyCo_date_range[1], endDate = input$swft_EddyCo_date_range[2], silent = TRUE)
        if(nrow(g2131.h2o.data) > 0){
          g2131.h2o.data <- g2131.h2o.data %>% dplyr::mutate(`Stream Name` = strm_name) %>% dplyr::select(-strm_name) %>%
            dplyr::filter(`Stream Name` == "G2131_percentFwMoleH2O")
        }
        # li7200.h2o.data <- read.eddy.inquiry(dataType = "2min", sensor = "Li7200", siteID = "UNDE", startDate = Sys.Date()-7, endDate = Sys.Date(), silent = TRUE)
        li7200.h2o.data <- read.eddy.inquiry(dataType = "2min", sensor = "Li7200", siteID = input$swft_EddyCo_site, startDate = input$swft_EddyCo_date_range[1], endDate = input$swft_EddyCo_date_range[2], silent = TRUE)
        if(nrow(li7200.h2o.data) > 0){
          li7200.h2o.data <- li7200.h2o.data %>% dplyr::mutate(`Stream Name` = strm_name) %>% dplyr::select(-strm_name) %>%
            dplyr::filter(`Stream Name` == "Li7200_fdMoleH2O")
        }
        
        # l2130.h2o.data <- read.eddy.inquiry(dataType = "2min", sensor = "L2130", siteID = "UNDE", startDate = Sys.Date()-7, endDate = Sys.Date(), silent = TRUE)
        l2130.h2o.data <- read.eddy.inquiry(dataType = "2min", sensor = "L2130", siteID = input$swft_EddyCo_site, startDate = input$swft_EddyCo_date_range[1], endDate = input$swft_EddyCo_date_range[2], silent = TRUE)
        if(nrow(l2130.h2o.data) > 0){
          l2130.h2o.data <- l2130.h2o.data %>% dplyr::mutate(`Stream Name` = strm_name) %>% dplyr::select(-strm_name) %>%
            dplyr::filter(`Stream Name` == "L2130_H2O")
        }
        
        swft.data.in <- data.table::rbindlist(l = list(li840.h2o.data, g2131.h2o.data, li7200.h2o.data, l2130.h2o.data)) %>%
          dplyr::mutate(readout_val_double = ifelse(test = `Stream Name` == "G2131_percentFwMoleH2O", yes = readout_val_double*10, no = readout_val_double)) %>%
          dplyr::mutate(readout_val_double = ifelse(test = `Stream Name` == "L2130_H2O", yes = readout_val_double/1000, no = readout_val_double)) 
        
        rm(li840.h2o.data, g2131.h2o.data, li7200.h2o.data)
      }
      
      # # For Function Testing
      # swft.data.in = read.eddy.inquiry(dataType  = "2min",
      #                                  sensor    = "CSAT3",
      #                                  siteID    = "WREF",
      #                                  startDate = Sys.Date()-7,
      #                                  endDate   = Sys.Date(),
      #                                  silent    = TRUE
      # )
      # message(paste0("Ultimate row check: ", nrow(swft.data.in)))
      # First check to see there was any data in the pull.
      if(nrow(swft.data.in) > 0){
        
        # Storage Isotope Analyzer - G2131-i
        if(input$swft_EddyCo_data_type == "G2131"){
          
          # Used in every permuation, so lets make this Valve Data frame first! By joining this to the subsequent data frame, we can see what level was being sampled
          swft.g2131.valves = swft.data.in %>%
            dplyr::filter(stringr::str_detect(string = `Stream Name`, pattern = "_Valve_") == TRUE) %>%
            tidyr::separate(col = `Stream Name`, into = c("Sensor", "Delete", "SampleLevel"), sep = "_") %>% 
            dplyr::filter(readout_val_double == 1) %>%
            dplyr::select(readout_time, SampleLevel) %>%
            dplyr::mutate(SampleLevel = ifelse(test = is.na(SampleLevel) == TRUE, yes = "Validation", no = SampleLevel)) 
          
          # Sub Data Type: CO2
          if(input$swft_EddyCo_sub_data_type_G2131 == "CO2"){
            
            swft.g2131.co2 = swft.data.in %>% 
              dplyr::filter(`Stream Name` %in% c("G2131_fwMoleCo2")) 
            
            swft.data.out = dplyr::left_join(x = swft.g2131.co2, y = swft.g2131.valves, by = "readout_time")  %>%
              dplyr::mutate(SampleLevel = ifelse(test = is.na(SampleLevel) == TRUE, yes = "Validation", no = SampleLevel)) 
            
            message(nrow(swft.data.out))
            
          }
          # Sub Data Type: H2O
          else if(input$swft_EddyCo_sub_data_type_G2131 == "H2O"){
            
            swft.g2131.co2 = swft.data.in %>% 
              dplyr::filter(`Stream Name` %in% c("G2131_percentFwMoleH2O")) 
            
            swft.data.out = dplyr::left_join(x = swft.g2131.co2, y = swft.g2131.valves, by = "readout_time")  %>%
              dplyr::mutate(SampleLevel = ifelse(test = is.na(SampleLevel) == TRUE, yes = "Validation", no = SampleLevel)) 
            
          }
          
          # Sub Data Type: Isotopes
          else if(input$swft_EddyCo_sub_data_type_G2131 == "Isotopes"){ 
            
            swift.g2131.isotopes = swft.data.in %>%
              dplyr::filter(`Stream Name` %in% "G2131_13C_isotope")
            
            swft.data.out = dplyr::left_join(x = swift.g2131.isotopes, y = swft.g2131.valves, by = "readout_time") %>%
              dplyr::mutate(SampleLevel = ifelse(test = is.na(SampleLevel) == TRUE, yes = "Validation", no = SampleLevel)) %>%
              dplyr::filter(readout_val_double <= 0 & readout_val_double >= -20)
            
          }
          
          # Sub Data Type: Sample Valves
          else if(input$swft_EddyCo_sub_data_type_G2131 == "Sample Valves"){
            
            swft.data.out = swft.data.in %>%
              dplyr::filter(stringr::str_detect(string = `Stream Name`, pattern = "_Valve_")) %>%
              tidyr::separate(col = `Stream Name`, into = c("Sensor", "Delete", "SampleLevel"), sep = "_") %>%
              dplyr::mutate(`Stream Name` = paste0("ML Solenoid ", SampleLevel) )  %>%
              dplyr::group_by(SiteID, `Stream Name`) %>%
              dplyr::add_tally() %>%
              dplyr::summarise(
                n = n[1],
                Open  = sum(readout_val_double, na.rm = TRUE),
                Closed = n - Open
              ) %>% 
              dplyr::mutate(Open =  paste0(100*round((Open/n),3), "%")) %>% 
              dplyr::mutate(Closed = paste0(100*round((Closed/n),3), "%")) %>% 
              reshape2::melt(id.vars = c("SiteID","Stream Name")) %>% 
              dplyr::filter(variable != "n") %>%
              dplyr::mutate(Percentage = as.numeric(gsub(x = value, pattern = "%", replacement = ""))) %>% 
              dplyr::mutate(`Valve Status` = variable) %>% 
              dplyr::select(SiteID, `Stream Name`, `Valve Status`, Percentage) %>%
              dplyr::arrange(`Stream Name`)
            
            names(swft.data.out) = c("SiteID", "Stream Name", "Valve Status", "Percentage")
            
          } else {
            swft.data.out = data.table::data.table()
          }
        # Storage CO2/H2O Analyzer - Li840A
        } else if(input$swft_EddyCo_data_type == "Li840"){
          
          if(input$swft_EddyCo_sub_data_type_Li840 %in% c("CO2","H2O")){
            
            # Pull Valve Data Too
            swft.li840.valves = read.eddy.inquiry(dataType  = "2min", 
                                                  sensor    = "Li840.valves", 
                                                  siteID    = input$swft_EddyCo_site,
                                                  startDate = input$swft_EddyCo_date_range[1], 
                                                  endDate   = input$swft_EddyCo_date_range[2],
                                                  silent    = TRUE
            )
            
            if(nrow(swft.li840.valves) > 0 ){
              swft.li840.valves.clean = swft.li840.valves %>%
                dplyr::mutate(readout_time = cut(readout_time, breaks = "2 min")) %>%
                dplyr::mutate(readout_time = lubridate::ymd_hms(readout_time)) %>%
                dplyr::distinct() %>%
                tidyr::separate(col = `Stream Name`, into = c("Sensor", "Delete", "SampleLevel"), sep = "_")
              
              # Join Clean Valve Data to d
              swft.li840.valves.clean.join = swft.li840.valves.clean %>% 
                dplyr::filter(readout_val_double == 1) %>%
                dplyr::select(readout_time, SampleLevel)
              
              
              if(input$swft_EddyCo_sub_data_type_Li840 == "CO2"){
              
                swft.data.out = swft.data.in %>%
                  dplyr::left_join(y = swft.li840.valves.clean.join, by = "readout_time") %>%
                  dplyr::mutate(SampleLevel = ifelse(test = is.na(SampleLevel) == TRUE, yes = "Unknown", no = SampleLevel)) %>%
                  dplyr::filter(`Stream Name` == "Li840_CO2_fwMole")
                
                swft.li840.summary = swft.data.out %>%
                  dplyr::group_by(`Stream Name`) %>%
                  dplyr::summarise(
                    median = stats::median(readout_val_double, na.rm = TRUE)
                  )
                
                swft.li840.co2.med = swft.li840.summary %>% dplyr::filter(`Stream Name` == "Li840_CO2_fwMole")
                swft.li840.co2.med = swft.li840.co2.med$median[1]
                
              }
              
              if(input$swft_EddyCo_sub_data_type_Li840 == "H2O"){
                
                swft.data.out = swft.data.in %>%
                  dplyr::left_join(y = swft.li840.valves.clean.join, by = "readout_time") %>%
                  dplyr::mutate(SampleLevel = ifelse(test = is.na(SampleLevel) == TRUE, yes = "Unknown", no = SampleLevel)) %>%
                  dplyr::filter(`Stream Name` == "Li840_H2O_fwMole")
              }
            }
          } else if(input$swft_EddyCo_sub_data_type_Li840 == "Sample Valves"){
            
            # WARNING There is some redundancy here, as we pull the Li840 Data too... Could make an excpetion...
            # However oddly enough there is not much difference in overall plotting time, go figure.
            
            swft.li840.valves = read.eddy.inquiry(dataType  = "2min", 
                                                  sensor    = "Li840.valves", 
                                                  siteID    = input$swft_EddyCo_site,
                                                  startDate = input$swft_EddyCo_date_range[1], 
                                                  endDate   = input$swft_EddyCo_date_range[2],
                                                  silent    = TRUE
            )
            # Check data was found!
            if(nrow(swft.li840.valves) > 0){
              # Make valve summary data frame for plot
              swft.data.out = swft.li840.valves %>%
                plyr::mutate(readout_time = cut(readout_time, breaks = "2 min")) %>%
                dplyr::mutate(readout_time = lubridate::ymd_hms(readout_time)) %>%
                dplyr::distinct() %>%
                tidyr::separate(col = `Stream Name`, into = c("Sensor", "Delete", "SampleLevel"), sep = "_") %>%
                dplyr::mutate(`Stream Name` = paste0("ML Solenoid ", SampleLevel) )  %>%
                dplyr::group_by(SiteID, `Stream Name`) %>%
                dplyr::add_tally() %>%
                dplyr::summarise(
                  n = n[1],
                  Open  = sum(readout_val_double, na.rm = TRUE),
                  Closed = n - Open
                ) %>% 
                dplyr::mutate(Open =  paste0(100*round((Open/n),3), "%")) %>% 
                dplyr::mutate(Closed = paste0(100*round((Closed/n),3), "%")) %>% 
                reshape2::melt(id.vars = c("SiteID","Stream Name")) %>% 
                dplyr::filter(variable != "n") %>%
                dplyr::mutate(Percentage = as.numeric(gsub(x = value, pattern = "%", replacement = ""))) %>% 
                dplyr::mutate(`Valve Status` = variable) %>% 
                dplyr::select(SiteID, `Stream Name`, `Valve Status`, Percentage) %>%
                dplyr::arrange(`Stream Name`)
              
              names(swft.data.out) = c("SiteID", "Stream Name", "Valve Status", "Percentage")
            } else {
              # Else create blank table to tell future logic to create a blank plot 
              swft.data.out = data.table::data.table()
              
            }
            
          } else if(input$swft_EddyCo_sub_data_type_Li840 == "Flow Rate"){
            
            # Read flow data
            swft.data.out = read.eddy.inquiry(dataType  = "2min", 
                                              sensor    = "ecse.sample.mfc", 
                                              siteID    = input$swft_EddyCo_site,
                                              startDate = input$swft_EddyCo_date_range[1], 
                                              endDate   = input$swft_EddyCo_date_range[2],
                                              silent    = TRUE
            )
            
          } else {
            swft.data.out = data.table::data.table()
          }
          
        } else if(input$swft_EddyCo_data_type == "L2130"){
          
          swft.l2130.valves = swft.data.in %>%
            dplyr::filter(stringr::str_detect(string = `Stream Name`, pattern = "_Valve") )%>%
            dplyr::distinct() %>%
            tidyr::separate(col = `Stream Name`, into = c("Sensor", "Delete", "SampleLevel"), sep = "_") %>%
            dplyr::mutate(Sensor = ifelse (Sensor == "L2130", yes = "L2130-I", no = Sensor))  %>%
            dplyr::mutate(SampleLevel = ifelse (SampleLevel == "Valve", yes = "Validation", no = SampleLevel)) %>%
            dplyr::filter(readout_val_double == 1) %>%
            dplyr::select(readout_time, SampleLevel)
          
          if(input$swft_EddyCo_sub_data_type_L2130 == "Isotope - 2H"){
            swft.data.out = swft.data.in %>%
              dplyr::filter(`Stream Name` == "L2130_2H_isotope") %>%
              dplyr::left_join(y = swft.l2130.valves, by = "readout_time") %>%
              dplyr::filter(readout_val_double <= 0 & readout_val_double > -1000)
            
          }
          if(input$swft_EddyCo_sub_data_type_L2130 == "Isotope - 18O"){
            swft.data.out = swft.data.in %>%
              dplyr::filter(`Stream Name` == "L2130_18O_isotope") %>%
              dplyr::left_join(y = swft.l2130.valves, by = "readout_time") %>%
              dplyr::filter(readout_val_double <= 0 & readout_val_double > -50)
          }
          if(input$swft_EddyCo_sub_data_type_L2130 == "H2O"){
            swft.data.out = swft.data.in %>%
              dplyr::filter(`Stream Name` == "L2130_H2O") %>%
              dplyr::left_join(y = swft.l2130.valves, by = "readout_time")
          }
          if(input$swft_EddyCo_sub_data_type_L2130 == "Sample Valves"){
            swft.data.out = swft.data.in %>%
              dplyr::filter(stringr::str_detect(string = `Stream Name`, pattern = "_Valve"))%>%
              tidyr::separate(col = `Stream Name`, into = c("Sensor", "Delete", "SampleLevel"), sep = "_") %>%
              dplyr::mutate(Sensor = ifelse (Sensor == "L2130", yes = "L2130-I", no = Sensor))  %>%
              dplyr::mutate(SampleLevel = ifelse (SampleLevel == "Valve", yes = "Validation", no = SampleLevel)) %>%
              dplyr::mutate(`Stream Name` = paste0("ML ", SampleLevel, " Solenoid ") )  %>%
              dplyr::mutate(`Stream Name` = ifelse(test = `Stream Name` == "ML Validation Solenoid ", yes = "Validation Solenoid", no = `Stream Name`)) %>% 
              dplyr::group_by(SiteID, `Stream Name`) %>%
              dplyr::add_tally() %>%
              dplyr::summarise(
                n = n[1],
                Open  = sum(readout_val_double, na.rm = TRUE),
                Closed = n - Open
              ) %>% 
              dplyr::mutate(Open =  paste0(100*round((Open/n),3), "%")) %>% 
              dplyr::mutate(Closed = paste0(100*round((Closed/n),3), "%")) %>% 
              reshape2::melt(id.vars = c("SiteID","Stream Name")) %>% 
              dplyr::filter(variable != "n") %>%
              dplyr::mutate(Percentage = as.numeric(gsub(x = value, pattern = "%", replacement = ""))) %>% 
              dplyr::mutate(`Valve Status` = variable) %>% 
              dplyr::select(SiteID, `Stream Name`, `Valve Status`, Percentage) %>%
              dplyr::arrange(`Stream Name`)
            
            names(swft.data.out) = c("SiteID", "Stream Name", "Valve Status", "Percentage")
          }
          
        } else if(input$swft_EddyCo_data_type == "Li7200"){
          
          if(input$swft_EddyCo_sub_data_type_Li7200 == "CO2"){
            
            swft.data.out = swft.data.in %>%
              dplyr::filter(`Stream Name` == "Li7200_CO2")
            
          }
          if(input$swft_EddyCo_sub_data_type_Li7200 == "H2O"){
            
            swft.data.out = swft.data.in %>%
              dplyr::filter(`Stream Name` == "Li7200_fdMoleH2O")
            
          }
          if(input$swft_EddyCo_sub_data_type_Li7200 == "Flow"){
            
            swft.data.out = swft.data.in %>%
              dplyr::filter(`Stream Name` == "Li7200_MFCSampleFlow")
            
          }
          if(input$swft_EddyCo_sub_data_type_Li7200 == "Signal Strength"){
            
            swft.data.out = swft.data.in %>%
              dplyr::filter(`Stream Name` %in% c("Li7200_CO2SglStr","Li7200_H2oSglStr"))
            
          }
          if(input$swft_EddyCo_sub_data_type_Li7200 == "Cell Temp"){
            
            swft.data.out = swft.data.in %>%
              dplyr::filter(`Stream Name` %in% c("Li7200_cellTempIn", "Li7200_cellTempOut")) %>%
              reshape2::dcast(readout_time + SiteID ~ `Stream Name`, value.var = "readout_val_double",fun.aggregate = mean) %>%
              dplyr::mutate(temp_diff = Li7200_cellTempIn - Li7200_cellTempOut)
            
          }
          if(input$swft_EddyCo_sub_data_type_Li7200 == "Pressure Differential"){
            
            swft.data.out = swft.data.in %>%
              dplyr::filter(`Stream Name` == "Li7200_pDiff")
            
          }
          if(input$swft_EddyCo_sub_data_type_Li7200 == "Diagnostic"){
            
            swft.data.out = swft.data.in %>%
              dplyr::filter(`Stream Name` == "Li7200_Diag") %>%
              dplyr::mutate(Diagnostic = as.factor(readout_val_double))
            
          }
          
          
        } else if(input$swft_EddyCo_data_type == "CO2"){
          
          swft.data.out <- swft.data.in %>%
            dplyr::mutate(`Stream Name` = base::trimws(`Stream Name`)) %>%
            dplyr::filter(readout_val_double < 1000 & readout_val_double >= -5)
          
        } else if(input$swft_EddyCo_data_type == "H2O"){
          message("\t\t   2")
          
          swft.data.out = swft.data.in
          
        } else if(input$swft_EddyCo_data_type == "CSAT3"){
          
          swft.data.out = swft.data.in 
          
          
        } else if(input$swft_EddyCo_data_type == "amrs"){
          
          swft.data.out <- swft.data.in %>%
            dplyr::mutate(day = lubridate::ymd(base::substr(x = readout_time, start = 1, stop = 11))) %>%
            dplyr::group_by(`Stream Name`, day) %>%
            dplyr::summarise(
              SiteID = SiteID[1],
              mean_daily = mean(readout_val_double)
            ) %>%
            reshape2::dcast(SiteID + day ~ `Stream Name`, value.var = "mean_daily", fun.aggregate = mean) 
          
          if(swft.data.out$SiteID[1] == "SCBI"){
            yInt = 11
            xInt = -14
          } else if(swft.data.out$SiteID[1] == "GUAN") {
            yInt = 0
            xInt = 11
          } else if(swft.data.out$SiteID[1] == "UKFS") {
            yInt = 7
            xInt = -10
          } else if(swft.data.out$SiteID[1] == "TALL") {
            yInt = 6
            xInt = 0
          } else if(swft.data.out$SiteID[1] == "NIWO") {
            yInt = 8
            xInt = -9
          } else if(swft.data.out$SiteID[1] == "RMNP") {
            yInt = -7
            xInt = 8
          } else if(swft.data.out$SiteID[1] == "SOAP") {
            yInt = 18
            xInt = -7
          } else if(swft.data.out$SiteID[1] == "TEAK") {
            yInt = 6
            xInt = 7
          } else {
            yInt = 0
            xInt = 0
          }
          
        } else if(input$swft_EddyCo_data_type == "ecse.mfm"){
          swft.ecse.ml.flow <- swft.data.in %>%
            dplyr::mutate(`Stream Name` = trimws(`Stream Name`)) %>%
            dplyr::filter(readout_val_double < 12)
          
          # Read in expected flow rate table
          swft.flowRateLookup <- data.table::fread(paste0(swft.server.folder.path, "data/lookup/flowRateLookup.csv"))
          
          swft.flowRateLookup.table <- swft.flowRateLookup %>%
            dplyr::filter(SiteID %in% input$swft_EddyCo_site)
          # dplyr::filter(SiteID == "WREF")
          
          flow.text <- paste0(swft.flowRateLookup.table$SiteID[1],"'s expected flowrate based upon environmental conditions is: ",
                              round(swft.flowRateLookup.table$FlowRateE[1],2)," (SLPM).\n The 70% threshold is: ",
                              round(swft.flowRateLookup.table$FlowRateE70[1],2)," (SLPM)." )
          swft.data.out <- dplyr::left_join(x = swft.ecse.ml.flow, y = swft.flowRateLookup, by = "SiteID") %>%
            dplyr::mutate(aggTime = cut(readout_time, breaks = "1 hours")) %>%
            dplyr::mutate(aggTime = lubridate::ymd_hms(aggTime)) %>%
            dplyr::group_by(SiteID, `Stream Name`, aggTime,FlowRateE,FlowRateE70) %>%
            dplyr::summarise(
              mean = mean(readout_val_double, na.rm = TRUE)
            ) 
          
        } else if(input$swft_EddyCo_data_type == "HMP155"){
          
          if(input$swft_EddyCo_sub_data_type_HMP155 == "Relative Humidity"){
            swft.data.out = swft.data.in %>%
              dplyr::filter(`Stream Name` == "HMP155_RH")
          }
          if(input$swft_EddyCo_sub_data_type_HMP155 == "Temperature"){
            swft.data.out = swft.data.in %>%
              dplyr::filter(`Stream Name` == "HMP155_temp")
          }
          if(input$swft_EddyCo_sub_data_type_HMP155 == "Dew Point"){
            swft.data.out = swft.data.in %>%
              dplyr::filter(`Stream Name` == "HMP155_DewPoint")
          }
          
        } else if(input$swft_EddyCo_data_type == "ecse.voltage"){
          
          swft.data.out = swft.data.in %>%
            dplyr::mutate(aggTime = cut(by15, breaks = "1 hours")) %>%
            dplyr::mutate(aggTime = lubridate::ymd_hms(aggTime)) %>%
            dplyr::group_by(SiteID, `Stream Name`, aggTime) %>%
            dplyr::summarise(
              mean = mean(mean)
            )
          
        } else if(input$swft_EddyCo_data_type == "ec.temps"){
          
          swft.data.out = swft.data.in %>%
            dplyr::group_by(`Stream Name`) %>%
            dplyr::filter(`Stream Name` != "ecse_comet_H2OMixRatio") %>%
            dplyr::mutate(by30 = cut(readout_time, breaks = "30 min")) %>%
            dplyr::ungroup() %>%
            dplyr::group_by(SiteID, `Stream Name`, by30) %>%
            dplyr::summarise(
              mean = mean(readout_val_double, na.rm = TRUE)
            ) %>%
            dplyr::mutate(by30 = lubridate::ymd_hms(by30)) %>%
            tidyr::separate(col = `Stream Name`, into = c("ec.system", "sensor","type","na"), sep = "_", remove = FALSE) %>%
            dplyr::select(-na) %>%
            dplyr::mutate(type = ifelse(test = type == "tempHut", yes = "comet", no = type)) %>%
            dplyr::mutate(type = ifelse(test = type == "tempC", yes = "comet", no = type)) %>%
            dplyr::mutate(ec.system = ifelse(test = ec.system == "ecse", yes = "Instrument Hut", no = "Environmental Enclosure")) %>%
            dplyr::mutate(ec.system = ifelse(test = stringr::str_detect(string = `Stream Name`, pattern = "HMP155_temp"), yes = "Ambient", no = ec.system)) %>%
            dplyr::mutate(`Stream Name` = gsub(x = `Stream Name`, pattern = "_temp", replacement = "")) %>%
            dplyr::mutate(`Stream Name` = gsub(x = `Stream Name`, pattern = "C", replacement = "")) %>%
            dplyr::mutate(`Stream Name` = gsub(x = `Stream Name`, pattern = "ecse_cometHut", replacement = "ecse_comet")) %>%
            dplyr::mutate(type = ifelse(test = `Stream Name` == "HMP155", yes = "tower.top", no = type))
          
        } else {
          swft.data.out = data.table::data.table()
        }
      } else {
        swft.data.out = data.table::data.table()
      }
      
      #################################################                            Plotting Logic                            #################################################
      
      if(nrow(swft.data.out) > 0){
        
        # Aesthetics
        ggplot2::theme_set(ggdark::dark_theme_gray()) 
        
        # Storage Isotope Analyzer - G2131-i
        if(input$swft_EddyCo_data_type == "G2131"){
          if(input$swft_EddyCo_sub_data_type_G2131 == "CO2"){
            message(paste0("Plot: ", input$swft_EddyCo_data_type, " - ", input$swft_EddyCo_sub_data_type_G2131, " for ", input$swft_EddyCo_site, " from ", input$swft_EddyCo_date_range[1], " - ", input$swft_EddyCo_date_range[2]))
            
            swft.data.out = swft.data.out %>%
              dplyr::mutate(`Stream Name` = SampleLevel)
            
            swft.plot = ggplot(data = swft.data.out, aes(x = readout_time, y = readout_val_double, color = SampleLevel)) +
              geom_point() +
              scale_x_datetime(breaks = scales::pretty_breaks(n = 10), date_labels = "%Y-%m-%d") +
              scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
              labs(x = "", y = "Concentration (ppm)", title = paste0(input$swft_EddyCo_site, " - ", input$swft_EddyCo_data_type, " ", input$swft_EddyCo_sub_data_type), 
                   subtitle = paste0(input$swft_EddyCo_date_range[1], " to ", input$swft_EddyCo_date_range[2])) +
              theme(strip.text.x = element_text(size = 14), axis.text.x = element_text(angle = 270, size = 14), text = element_text(color = "white", face = "bold", size = 20)) 
            
          }
          
          if(input$swft_EddyCo_sub_data_type_G2131 == "H2O"){
            message(paste0("Plot: ", input$swft_EddyCo_data_type, " - ", input$swft_EddyCo_sub_data_type_G2131, " for ", input$swft_EddyCo_site, " from ", input$swft_EddyCo_date_range[1], " - ", input$swft_EddyCo_date_range[2]))
            
            swft.data.out = swft.data.out %>%
              dplyr::mutate(`Stream Name` = SampleLevel)
            
            swft.plot = ggplot(data = swft.data.out, aes(x = readout_time, y = readout_val_double, color = SampleLevel)) +
              geom_point() +
              scale_x_datetime(breaks = scales::pretty_breaks(n = 10), date_labels = "%Y-%m-%d") +
              scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
              labs(x = "", y = "", title = paste0(input$swft_EddyCo_site, " - ", input$swft_EddyCo_data_type, " ", input$swft_EddyCo_sub_data_type), 
                   subtitle = paste0(input$swft_EddyCo_date_range[1], " to ", input$swft_EddyCo_date_range[2])) +
              theme(strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 270), text = element_text(color = "white", face = "bold", size = 20)) 
            
          }
          
          if(input$swft_EddyCo_sub_data_type_G2131 == "Isotopes"){
            message(paste0("Plot: ", input$swft_EddyCo_data_type, " - ", input$swft_EddyCo_sub_data_type_G2131, " for ", input$swft_EddyCo_site, " from ", input$swft_EddyCo_date_range[1], " - ", input$swft_EddyCo_date_range[2]))
            
            swft.data.out = swft.data.out %>%
              dplyr::mutate(`Stream Name` = SampleLevel)
            
            swft.plot = ggplot(data = swft.data.out, aes(x = readout_time, y = readout_val_double, color = SampleLevel)) +
              geom_point() +
              scale_x_datetime(breaks = scales::pretty_breaks(n = 10), date_labels = "%Y-%m-%d") +
              scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
              labs(x = "", y = "Concentration", title = paste0(input$swft_EddyCo_site, " - ", input$swft_EddyCo_data_type, " ", input$swft_EddyCo_sub_data_type), 
                   subtitle = paste0(input$swft_EddyCo_date_range[1], " to ", input$swft_EddyCo_date_range[2])) +
              theme(strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 270), text = element_text(color = "white", face = "bold", size = 20)) 
            
          }
          
          if(input$swft_EddyCo_sub_data_type_G2131 == "Sample Valves"){
            message(paste0("Plot: ", input$swft_EddyCo_data_type, " - ", input$swft_EddyCo_sub_data_type_G2131, " for ", input$swft_EddyCo_site, " from ", input$swft_EddyCo_date_range[1], " - ", input$swft_EddyCo_date_range[2]))
            
            swft.plot = ggplot() +
              geom_col(data = swft.data.out, aes(x = `Valve Status`, y = Percentage, fill = `Valve Status`)) +
              geom_text(data = swft.data.out, aes(x = `Valve Status`, y = Percentage, label = paste0(Percentage, "%"), vjust = -1), color = "white", size = 6) +
              scale_y_continuous(breaks = seq(from = 0, to = 100, by = 10), limits = c(0,115)) +
              scale_fill_manual(values = c("blue", "#ff69b4")) +
              labs(x = "", y = "Counts", fill = "Valve\nStatus", title = paste0(input$swft_EddyCo_site, " - ", input$swft_EddyCo_data_type, " ", input$swft_EddyCo_sub_data_type),
                   subtitle = paste0(input$swft_EddyCo_date_range[1], " to ", input$swft_EddyCo_date_range[2])) +
              theme(text = element_text(color = "white", face = "bold", size = 20)) +
              facet_wrap(~`Stream Name`)
          }
        }
        
        # Storage CO2/H2O Analyzer - Li840A
        if(input$swft_EddyCo_data_type == "Li840"){
          if(input$swft_EddyCo_sub_data_type_Li840 == "CO2"){
            message(paste0("Plot: ", input$swft_EddyCo_data_type, " - ", input$swft_EddyCo_sub_data_type_Li840, " for ", input$swft_EddyCo_site, " from ", input$swft_EddyCo_date_range[1], " - ", input$swft_EddyCo_date_range[2]))
            
            swft.data.out = swft.data.out %>%
              dplyr::mutate(`Stream Name` = SampleLevel)
            
            swft.plot = ggplot(data = swft.data.out, aes(x = readout_time, y = readout_val_double, color = SampleLevel)) +
              geom_point() +
              scale_y_continuous(limits = c(swft.li840.co2.med - 100, swft.li840.co2.med + 100), breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
              scale_x_datetime(breaks = scales::pretty_breaks(n = 10), date_labels = "%Y-%m-%d") +
              labs(x = "", y = "Concentration (ppm)", title = paste0(input$swft_EddyCo_site, " - ", input$swft_EddyCo_data_type, " ", input$swft_EddyCo_sub_data_type), 
                   subtitle = paste0(input$swft_EddyCo_date_range[1], " to ", input$swft_EddyCo_date_range[2])) +
              theme(strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 270), text = element_text(color = "white", face = "bold", size = 20)) 
            
          }
          if(input$swft_EddyCo_sub_data_type_Li840 == "H2O"){
            message(paste0("Plot: ", input$swft_EddyCo_data_type, " - ", input$swft_EddyCo_sub_data_type_Li840, " for ", input$swft_EddyCo_site, " from ", input$swft_EddyCo_date_range[1], " - ", input$swft_EddyCo_date_range[2]))
            
            swft.data.out = swft.data.out %>%
              dplyr::mutate(`Stream Name` = SampleLevel)
            
            swft.plot = ggplot(data = swft.data.out, aes(x = readout_time, y = readout_val_double, color = SampleLevel)) +
              geom_point() +
              scale_x_datetime(breaks = scales::pretty_breaks(n = 10), date_labels = "%Y-%m-%d") +
              scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
              labs(x = "", y = "Concentration (ppm)", title = paste0(input$swft_EddyCo_site, " - ", input$swft_EddyCo_data_type, " ", input$swft_EddyCo_sub_data_type), 
                   subtitle = paste0(input$swft_EddyCo_date_range[1], " to ", input$swft_EddyCo_date_range[2])) +
              theme(strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 270), text = element_text(color = "white", face = "bold", size = 20)) 
            
          }
          if(input$swft_EddyCo_sub_data_type_Li840 == "Sample Valves"){
            message(paste0("Plot: ", input$swft_EddyCo_data_type, " - ", input$swft_EddyCo_sub_data_type_Li840, " for ", input$swft_EddyCo_site, " from ", input$swft_EddyCo_date_range[1], " - ", input$swft_EddyCo_date_range[2]))
            
            swft.plot = ggplot() +
              geom_col(data = swft.data.out, aes(x = `Valve Status`, y = Percentage, fill = `Valve Status`)) +
              geom_text(data = swft.data.out, aes(x = `Valve Status`, y = Percentage, label = paste0(Percentage, "%"), vjust = -1), color = "white", size = 6) +
              scale_y_continuous(breaks = seq(from = 0, to = 100, by = 10), limits = c(0,115)) +
              scale_fill_manual(values = c("blue", "#ff69b4")) +
              labs(x = "", y = "Counts", fill = "Valve\nStatus", title = paste0(input$swft_EddyCo_site, " - ", input$swft_EddyCo_data_type, " ", input$swft_EddyCo_sub_data_type),
                   subtitle = paste0(input$swft_EddyCo_date_range[1], " to ", input$swft_EddyCo_date_range[2])) +
              theme(text = element_text(color = "white", face = "bold", size = 20)) +
              facet_wrap(~`Stream Name`)
          }
          if(input$swft_EddyCo_sub_data_type_Li840 == "Flow Rate"){
            message(paste0("Plot: ", input$swft_EddyCo_data_type, " - ", input$swft_EddyCo_sub_data_type_Li840, " for ", input$swft_EddyCo_site, " from ", input$swft_EddyCo_date_range[1], " - ", input$swft_EddyCo_date_range[2]))
            
            plot.min <- min(swft.data.out$readout_time, na.rm = TRUE)
            plot.max <- max(swft.data.out$readout_time, na.rm = TRUE)
            
            swft.plot <- ggplot(swft.data.out, aes(x=readout_time, y= readout_val_double))+
              annotate("rect", xmin = plot.min, xmax = plot.max, ymin = .8, ymax = 1.2, alpha = 0.4, fill = "#00cc00")+
              geom_point(shape = 1, alpha = .6, color = "cyan") +
              geom_hline(yintercept = 1.0, linetype = "dashed") +
              scale_color_manual(values = c("#e41a1c","#377eb8","#4daf4a")) +
              scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
              scale_x_datetime(date_labels = "%Y-%m-%d", breaks = scales::pretty_breaks(n = 10)) +
              labs(title = paste0(swft.data.out$SiteID[1], ": Li840 MFC Flow Rate 2-minute point data"),
                   x = "", y = "Flow (SLPM)",
                   color = "Sensor") +
              theme(strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 270), text = element_text(color = "white", face = "bold", size = 20))
            
          }
        } # End Li840 if
        if(input$swft_EddyCo_data_type == "L2130"){
          if(input$swft_EddyCo_sub_data_type_L2130 %in% c("Isotope - 2H", "Isotope - 18O")){
            message(paste0("Plot: ", input$swft_EddyCo_data_type, " - ", input$swft_EddyCo_sub_data_type_L2130, " for ", input$swft_EddyCo_site, " from ", input$swft_EddyCo_date_range[1], " - ", input$swft_EddyCo_date_range[2]))
            
            swft.plot = ggplot(swft.data.out, aes(x = readout_time, y = readout_val_double, color = SampleLevel)) +
              geom_point(alpha = .6) +
              scale_x_datetime(breaks = scales::pretty_breaks(n = 10), date_labels = "%Y-%m-%d") +
              scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
              theme(strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 270), text = element_text(color = "white", face = "bold", size = 20)) +
              labs(x = "", y = "per mil", title = paste0(input$swft_EddyCo_site, " - ", input$swft_EddyCo_data_type, " ", input$swft_EddyCo_sub_data_type), 
                   subtitle = paste0(input$swft_EddyCo_date_range[1], " to ", input$swft_EddyCo_date_range[2]))
          }
          if(input$swft_EddyCo_sub_data_type_L2130 == "H2O"){
            message(paste0("Plot: ", input$swft_EddyCo_data_type, " - ", input$swft_EddyCo_sub_data_type_L2130, " for ", input$swft_EddyCo_site, " from ", input$swft_EddyCo_date_range[1], " - ", input$swft_EddyCo_date_range[2]))

            swft.plot = ggplot(swft.data.out, aes(x = readout_time, y = readout_val_double, color = SampleLevel)) +
              geom_point(alpha = .6) +
              scale_x_datetime(breaks = scales::pretty_breaks(n = 10), date_labels = "%Y-%m-%d") +
              scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
              theme(strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 270), text = element_text(color = "white", face = "bold", size = 20)) +
              labs(x = "", y = "Micromoles Per Mole", title = paste0(input$swft_EddyCo_site, " - ", input$swft_EddyCo_data_type, " ", input$swft_EddyCo_sub_data_type), 
                   subtitle = paste0(input$swft_EddyCo_date_range[1], " to ", input$swft_EddyCo_date_range[2]))
          }
          if(input$swft_EddyCo_sub_data_type_L2130 == "Sample Valves"){
            message(paste0("Plot: ", input$swft_EddyCo_data_type, " - ", input$swft_EddyCo_sub_data_type_L2130, " for ", input$swft_EddyCo_site, " from ", input$swft_EddyCo_date_range[1], " - ", input$swft_EddyCo_date_range[2]))
            
            swft.plot = ggplot() +
              geom_col(data = swft.data.out, aes(x = `Valve Status`, y = Percentage, fill = `Valve Status`)) +
              geom_text(data = swft.data.out, aes(x = `Valve Status`, y = Percentage, label = paste0(Percentage, "%"), vjust = -1), color = "white", size = 6) +
              scale_y_continuous(breaks = seq(from = 0, to = 100, by = 10), limits = c(0,115)) +
              scale_fill_manual(values = c("blue", "#ff69b4")) +
              labs(x = "", y = "Counts", fill = "Valve\nStatus", title = paste0(input$swft_EddyCo_site, " - ", input$swft_EddyCo_data_type, " ", input$swft_EddyCo_sub_data_type),
                   subtitle = paste0(input$swft_EddyCo_date_range[1], " to ", input$swft_EddyCo_date_range[2])) +
              theme(text = element_text(color = "white", face = "bold", size = 20)) +
              facet_wrap(~`Stream Name`)
          }
        }
        
        if(input$swft_EddyCo_data_type == "Li7200"){
          if(input$swft_EddyCo_sub_data_type_Li7200 == "CO2"){
            message(paste0("Plot: ", input$swft_EddyCo_data_type, " - ", input$swft_EddyCo_sub_data_type_Li7200, " for ", input$swft_EddyCo_site, " from ", input$swft_EddyCo_date_range[1], " - ", input$swft_EddyCo_date_range[2]))
            
            swft.plot = ggplot(swft.data.out, aes(x = readout_time, y = readout_val_double)) +
              geom_point(alpha = .8, color = "red") +
              scale_x_datetime(breaks = scales::pretty_breaks(n = 10), date_labels = "%Y-%m-%d") +
              scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
              theme(strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 270), text = element_text(color = "white", face = "bold", size = 20)) +
              labs(x = "", y = "Concentration (ppm)", title = paste0(input$swft_EddyCo_site, " - ", input$swft_EddyCo_data_type, " ", input$swft_EddyCo_sub_data_type), 
                   subtitle = paste0(input$swft_EddyCo_date_range[1], " to ", input$swft_EddyCo_date_range[2]))+
              facet_wrap(~`Stream Name`)
          }
          if(input$swft_EddyCo_sub_data_type_Li7200 == "H2O"){
            message(paste0("Plot: ", input$swft_EddyCo_data_type, " - ", input$swft_EddyCo_sub_data_type_Li7200, " for ", input$swft_EddyCo_site, " from ", input$swft_EddyCo_date_range[1], " - ", input$swft_EddyCo_date_range[2]))
            
            swft.plot = ggplot(swft.data.out, aes(x = readout_time, y = readout_val_double)) +
              geom_point(alpha = .8, color = "blue") +
              scale_x_datetime(breaks = scales::pretty_breaks(n = 10), date_labels = "%Y-%m-%d") +
              scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
              theme(strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 270), text = element_text(color = "white", face = "bold", size = 20)) +
              labs(x = "", y = "Concentration (ppm)", title = paste0(input$swft_EddyCo_site, " - ", input$swft_EddyCo_data_type, " ", input$swft_EddyCo_sub_data_type), 
                   subtitle = paste0(input$swft_EddyCo_date_range[1], " to ", input$swft_EddyCo_date_range[2]))+
              facet_wrap(~`Stream Name`)
          }
          if(input$swft_EddyCo_sub_data_type_Li7200 == "Flow"){
            message(paste0("Plot: ", input$swft_EddyCo_data_type, " - ", input$swft_EddyCo_sub_data_type_Li7200, " for ", input$swft_EddyCo_site, " from ", input$swft_EddyCo_date_range[1], " - ", input$swft_EddyCo_date_range[2]))
            
            plot.min <- min(swft.data.out$readout_time, na.rm = TRUE)
            plot.max <- max(swft.data.out$readout_time, na.rm = TRUE)
            
            swft.plot = ggplot(swft.data.out, aes(x = readout_time, y = readout_val_double)) +
              annotate("rect", xmin = plot.min, xmax = plot.max, ymin = 11.5, ymax = 12.5, alpha = 0.4, fill = "#00cc00")+
              geom_point(alpha = .8, color = "cyan") +
              geom_hline(yintercept = 12, linetype = "dashed") +
              scale_x_datetime(breaks = scales::pretty_breaks(n = 10), date_labels = "%Y-%m-%d") +
              scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
              theme(strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 270), text = element_text(color = "white", face = "bold", size = 20)) +
              labs(x = "", y = "", title = paste0(input$swft_EddyCo_site, " - ", input$swft_EddyCo_data_type, " ", input$swft_EddyCo_sub_data_type), 
                   subtitle = paste0(input$swft_EddyCo_date_range[1], " to ", input$swft_EddyCo_date_range[2]))+
              facet_wrap(~`Stream Name`)
          }
          if(input$swft_EddyCo_sub_data_type_Li7200 == "Signal Strength"){
            message(paste0("Plot: ", input$swft_EddyCo_data_type, " - ", input$swft_EddyCo_sub_data_type_Li7200, " for ", input$swft_EddyCo_site, " from ", input$swft_EddyCo_date_range[1], " - ", input$swft_EddyCo_date_range[2]))
            
            plot.min <- min(swft.data.out$readout_time, na.rm = TRUE)
            plot.max <- max(swft.data.out$readout_time, na.rm = TRUE)
            
            swft.plot = ggplot(swft.data.out, aes(x = readout_time, y = readout_val_double, color = `Stream Name`)) +
              geom_point(alpha = .8) +
              annotate("rect", xmin = plot.min, xmax = plot.max, ymin = 95, ymax = 102, alpha = 0.2, fill = "#00cc00")+
              scale_color_manual(values = c("red","blue","grey"))+
              scale_x_datetime(breaks = scales::pretty_breaks(n = 10), date_labels = "%Y-%m-%d") +
              scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
              theme(strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 270), text = element_text(color = "white", face = "bold", size = 20)) +
              labs(x = "", y = "", title = paste0(input$swft_EddyCo_site, " - ", input$swft_EddyCo_data_type, " ", input$swft_EddyCo_sub_data_type), 
                   subtitle = paste0(input$swft_EddyCo_date_range[1], " to ", input$swft_EddyCo_date_range[2]))+
              facet_wrap(~`Stream Name`)
          }
          if(input$swft_EddyCo_sub_data_type_Li7200 == "Cell Temp"){
            message(paste0("Plot: ", input$swft_EddyCo_data_type, " - ", input$swft_EddyCo_sub_data_type_Li7200, " for ", input$swft_EddyCo_site, " from ", input$swft_EddyCo_date_range[1], " - ", input$swft_EddyCo_date_range[2]))
            
            plot.min <- min(swft.data.out$readout_time, na.rm = TRUE)
            plot.max <- max(swft.data.out$readout_time, na.rm = TRUE)
            
            swft.data.out = swft.data.out %>%
              dplyr::mutate(`Stream Name` = "Cell Tempurature Difference")
            
            swft.plot = ggplot(swft.data.out, aes(x = readout_time, y = temp_diff)) +
              geom_point(alpha = .8, color = "cyan") +
              annotate("rect", xmin = plot.min, xmax = plot.max, ymin = 8,    ymax = Inf, alpha = 0.2, fill = "red")+
              annotate("rect", xmin = plot.min, xmax = plot.max, ymin = 6,    ymax =   8, alpha = 0.2, fill = "#ff8c00")+
              annotate("rect", xmin = plot.min, xmax = plot.max, ymin = 2,    ymax =   6, alpha = 0.2, fill = "#00cc00")+
              annotate("rect", xmin = plot.min, xmax = plot.max, ymin = .2,   ymax =   2, alpha = 0.2, fill = "#ff8c00")+
              annotate("rect", xmin = plot.min, xmax = plot.max, ymin = -Inf, ymax =  .2, alpha = 0.2, fill = "red")+
              scale_x_datetime(breaks = scales::pretty_breaks(n = 10), date_labels = "%Y-%m-%d") +
              scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
              theme(strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 270), text = element_text(color = "white", face = "bold", size = 20)) +
              labs(x = "", y = "", title = paste0(input$swft_EddyCo_site, " - ", input$swft_EddyCo_data_type, " ", input$swft_EddyCo_sub_data_type), 
                   subtitle = paste0(input$swft_EddyCo_date_range[1], " to ", input$swft_EddyCo_date_range[2]))+
              facet_wrap(~`Stream Name`)
          }
          if(input$swft_EddyCo_sub_data_type_Li7200 == "Pressure Differential"){
            message(paste0("Plot: ", input$swft_EddyCo_data_type, " - ", input$swft_EddyCo_sub_data_type_Li7200, " for ", input$swft_EddyCo_site, " from ", input$swft_EddyCo_date_range[1], " - ", input$swft_EddyCo_date_range[2]))
            
            plot.min <- min(swft.data.out$readout_time, na.rm = TRUE)
            plot.max <- max(swft.data.out$readout_time, na.rm = TRUE)
            
            swft.plot = ggplot(swft.data.out, aes(x = readout_time, y = readout_val_double)) +
              geom_point(alpha = .6, color = "cyan") +
              annotate("rect", xmin = plot.min, xmax = plot.max, ymin = -3, ymax = -.7, alpha = 0.2, fill = "#00cc00")+
              annotate("rect", xmin = plot.min, xmax = plot.max, ymin = -5, ymax = -3, alpha = 0.2, fill = "#ff8c00")+
              annotate("rect", xmin = plot.min, xmax = plot.max, ymin = -10, ymax = -5, alpha = 0.2, fill = "red")+
              scale_x_datetime(breaks = scales::pretty_breaks(n = 10), date_labels = "%Y-%m-%d") +
              scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
              theme(strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 270), text = element_text(color = "white", face = "bold", size = 20)) +
              labs(x = "", y = "", title = paste0(input$swft_EddyCo_site, " - ", input$swft_EddyCo_data_type, " ", input$swft_EddyCo_sub_data_type), 
                   subtitle = paste0(input$swft_EddyCo_date_range[1], " to ", input$swft_EddyCo_date_range[2]))
          }
          if(input$swft_EddyCo_sub_data_type_Li7200 == "Diagnostic"){
            message(paste0("Plot: ", input$swft_EddyCo_data_type, " - ", input$swft_EddyCo_sub_data_type_Li7200, " for ", input$swft_EddyCo_site, " from ", input$swft_EddyCo_date_range[1], " - ", input$swft_EddyCo_date_range[2]))
            
            swft.plot = ggplot(swft.data.out, aes(x = readout_time, y= as.integer(readout_val_double), color = Diagnostic))+
              geom_point(alpha = .5) +
              geom_hline(yintercept = 8191, linetype = "dashed", color = "black") +
              scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
              scale_x_datetime(breaks = scales::pretty_breaks(n = 10), date_labels = "%Y-%m-%d") +
              labs(title = paste0(swft.data.out$SiteID[1], ": Li7200 Diagnostic 2-minute point data"),
                   x = "", y = "Diagnostic",
                   color = "Diagnostic Code") +
              theme(strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 270), text = element_text(color = "white", face = "bold", size = 20)) +
              labs(x = "", y = "", title = paste0(input$swft_EddyCo_site, " - ", input$swft_EddyCo_data_type, " ", input$swft_EddyCo_sub_data_type), 
                   subtitle = paste0(input$swft_EddyCo_date_range[1], " to ", input$swft_EddyCo_date_range[2]))
          } 
        }
        if(input$swft_EddyCo_data_type == "CO2"){
          message(paste0("Plot: ", input$swft_EddyCo_data_type, "-All for ", input$swft_EddyCo_site, " from ", input$swft_EddyCo_date_range[1], " - ", input$swft_EddyCo_date_range[2]))
          
          swft.co2.colors <- c("Li7200_CO2" = "#005824",
                               "Li840_CO2_fwMole" = "#e31a1c", 
                               "G2131_fwMoleCo2" = "#0c2c84")
          
          swft.plot <- ggplot(swft.data.out, aes(x=readout_time, y= readout_val_double, color = `Stream Name`))+
            geom_jitter(alpha = .6) +
            scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
            scale_x_datetime(breaks = scales::pretty_breaks(n = 10), date_labels = "%Y-%m-%d") +
            scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +            scale_color_manual(values = swft.co2.colors)+
            labs(title = paste0(swft.data.out$SiteID[1], ": CO2 2-minute point data"),
                 x = "", y = "CO2 (ppm)",
                 color = "Sensor") +
            theme(axis.text.x = element_text(angle = 270), legend.text=element_text(size=14), legend.title = element_text(size = 18))+ guides(colour = guide_legend(override.aes = list(size=4, alpha = 1)))  # Option to angle the facet grid y panel text
        }
        if(input$swft_EddyCo_data_type == "H2O"){
          message("\t\t   3")
          message(paste0("Plot: ", input$swft_EddyCo_data_type, "-All for ", input$swft_EddyCo_site, " from ", input$swft_EddyCo_date_range[1], " - ", input$swft_EddyCo_date_range[2]))
          
          swft.co2.colors <- c("Li7200_fdMoleH2O" = "#005824",
                               "Li840_H2O_fwMole" = "#e31a1c", 
                               "G2131_percentFwMoleH2O" = "#0c2c84",
                               "L2130_H2O"        = "#1c9099")
          
          swft.plot <- ggplot(swft.data.out, aes(x=readout_time, y= readout_val_double, color = `Stream Name`))+
            geom_jitter(alpha = .6) +
            scale_x_datetime(breaks = scales::pretty_breaks(n = 10), date_labels = "%Y-%m-%d") +
            scale_y_continuous(breaks = scales::pretty_breaks(n = 10), sec.axis = dup_axis(name = "")) +            
            scale_color_manual(values = swft.co2.colors)+
            labs(title = paste0(swft.data.out$SiteID[1], ": H2O 2-minute point data"),
                 x = "", y = "undetermined",
                 color = "Sensor") +
            theme(axis.text.x = element_text(angle = 270), legend.text=element_text(size=14), legend.title = element_text(size = 18))+ guides(colour = guide_legend(override.aes = list(size=4, alpha = 1)))  # Option to angle the facet grid y panel text
          
        }
        if(input$swft_EddyCo_data_type == "CSAT3"){
          message(paste0("Plot: ", input$swft_EddyCo_data_type, " for ", input$swft_EddyCo_site, " from ", input$swft_EddyCo_date_range[1], " - ", input$swft_EddyCo_date_range[2]))
          
          colors <- c("NA" = "grey","0" = "limegreen", "1" = "gold",
                      "2" = "yellow", "3" = "orange","4" = "red")
          
          swft.data.out = swft.data.out %>% 
            dplyr::mutate(`Stream Name` = CodeSum)
          
          swft.plot <- ggplot(swft.data.out, aes(x=readout_time,y=CSAT3_WindVector,color = CodeSum))+
            geom_point(alpha = 0.6, shape = 1)+
            scale_x_datetime(breaks = scales::pretty_breaks(n = 10), date_labels = "%Y-%m-%d") +
            scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
            scale_color_manual(values = colors)+
            theme(text = element_text(color = "white", face = "bold", size = 20)) +
            labs(title = paste0(swft.data.out$SiteID[1], ": CSAT3 Wind Speed 2-minute point data"), x = "Timestamp", y = "Wind Speed (m/s)")
          
        }
        if(input$swft_EddyCo_data_type == "amrs"){
          message(paste0("Plot: ", input$swft_EddyCo_data_type, " for ", input$swft_EddyCo_site, " from ", input$swft_EddyCo_date_range[1], " - ", input$swft_EddyCo_date_range[2]))
          
          swft.data.out = swft.data.out %>%
            dplyr::mutate(`Stream Name` = day)
          
          swft.plot <-  ggplot(swft.data.out, aes(x = AMRS_x, y= AMRS_y, Time = day, color = as.factor(day)))+
            geom_hline(yintercept = yInt,linetype = "dashed",alpha = 0.75)+
            geom_vline(xintercept = xInt,linetype = "dashed",alpha = 0.75)+
            geom_point(size = 3.5)+ # Make Points, alpha gets lighter the older the date
            scale_color_viridis_d()+
            stat_summary(fun=median, geom="line", alpha = .35) + # Draw lines to same sites
            geom_segment(x=xInt-1,y=yInt+1, xend=xInt+1, yend=yInt+1, color="white", aes(color = "1x1 box"))+   # Create 1x1 box TOP
            geom_segment(x=xInt-1,y=yInt-1, xend=xInt+1, yend=yInt-1, color="white", aes(color = "1x1 box"))+ # Create 1x1 box Bottom
            geom_segment(x=xInt-1,y=yInt-1, xend=xInt-1, yend=yInt+1, color="white", aes(color = "1x1 box"))+ # Create 1x1 box Left
            geom_segment(x=xInt+1,y=yInt+1, xend=xInt+1, yend=yInt-1, color="white", aes(color = "1x1 box"))+   # Create 1x1 box Right
            scale_x_continuous(limits=c(xInt-5, xInt+5), breaks = c(xInt,-5,-3,-1,0,1,3,5)) + # Set limits based upon max potential values
            scale_y_continuous(limits=c(yInt-5, yInt+5), breaks = c(yInt,-5,-3,-1,0,1,3,5)) + # Set limits based upon max potential values
            geom_hline(yintercept = 0)+ # Set y-axis
            geom_vline(xintercept = 0)+ # Set x-axis
            theme(text = element_text(color = "white", face = "bold", size = 20)) +
            labs(title = paste0("AMRS Pitch and Roll at ", swft.data.out$SiteID[1]),x="AMRS Roll",y="AMRS Pitch", color = "Date") # Create labels to make plot legible
        }
        if(input$swft_EddyCo_data_type == "HMP155"){
          if(input$swft_EddyCo_sub_data_type_HMP155 == "Relative Humidity"){
            message(paste0("Plot: ", input$swft_EddyCo_data_type, " - ", input$swft_EddyCo_sub_data_type_HMP155, " for ", input$swft_EddyCo_site, " from ", input$swft_EddyCo_date_range[1], " - ", input$swft_EddyCo_date_range[2]))
            
            swft.plot = ggplot(swft.data.out, aes(x = readout_time, y= readout_val_double))+
              geom_point(alpha = .6, color = "#73C0EC") +
              scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
              scale_x_datetime(breaks = scales::pretty_breaks(n = 10), date_labels = "%Y-%m-%d") +
              theme(strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 270), text = element_text(color = "white", face = "bold", size = 20)) +
              labs(x = "", y = "%", title = paste0(input$swft_EddyCo_site, " - ", input$swft_EddyCo_data_type, " ", input$swft_EddyCo_sub_data_type), 
                   subtitle = paste0(input$swft_EddyCo_date_range[1], " to ", input$swft_EddyCo_date_range[2]))
          }
          if(input$swft_EddyCo_sub_data_type_HMP155 == "Temperature"){
            message(paste0("Plot: ", input$swft_EddyCo_data_type, " - ", input$swft_EddyCo_sub_data_type_HMP155, " for ", input$swft_EddyCo_site, " from ", input$swft_EddyCo_date_range[1], " - ", input$swft_EddyCo_date_range[2]))
            
            swft.plot = ggplot(swft.data.out, aes(x = readout_time, y= readout_val_double))+
              geom_point(alpha = .6, color = "#00b200")+
              scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
              scale_x_datetime(breaks = scales::pretty_breaks(n = 10), date_labels = "%Y-%m-%d") +
              theme(strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 270), text = element_text(color = "white", face = "bold", size = 20)) +
              labs(x = "", y = "Temperature (C)", title = paste0(input$swft_EddyCo_site, " - ", input$swft_EddyCo_data_type, " ", input$swft_EddyCo_sub_data_type), 
                   subtitle = paste0(input$swft_EddyCo_date_range[1], " to ", input$swft_EddyCo_date_range[2]))
          }
          if(input$swft_EddyCo_sub_data_type_HMP155 == "Dew Point"){
            message(paste0("Plot: ", input$swft_EddyCo_data_type, " - ", input$swft_EddyCo_sub_data_type_HMP155, " for ", input$swft_EddyCo_site, " from ", input$swft_EddyCo_date_range[1], " - ", input$swft_EddyCo_date_range[2]))
            
            swft.plot = ggplot(swft.data.out, aes(x = readout_time, y= readout_val_double))+
              geom_point(alpha = .6, color = "red")+
              scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
              scale_x_datetime(breaks = scales::pretty_breaks(n = 10), date_labels = "%Y-%m-%d") +
              theme(strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 270), text = element_text(color = "white", face = "bold", size = 20)) +
              labs(x = "", y = "Temperature (C)", title = paste0(input$swft_EddyCo_site, " - ", input$swft_EddyCo_data_type, " ", input$swft_EddyCo_sub_data_type), 
                   subtitle = paste0(input$swft_EddyCo_date_range[1], " to ", input$swft_EddyCo_date_range[2]))
          }
        }
        if(input$swft_EddyCo_data_type == "ecse.mfm"){
          message(paste0("Plot: ", input$swft_EddyCo_data_type, " for ", input$swft_EddyCo_site, " from ", input$swft_EddyCo_date_range[1], " - ", input$swft_EddyCo_date_range[2]))
          
          plot.min <- min(swft.data.out$aggTime, na.rm = TRUE)
          plot.max <- max(swft.data.out$aggTime, na.rm = TRUE)
          
          swft.plot <- ggplot(swft.data.out, aes(x=aggTime, y= mean, color = `Stream Name`))+
            geom_hline(yintercept = swft.data.out$FlowRateE[1], linetype = "dashed", color = "#006d2c")+
            geom_hline(yintercept = swft.data.out$FlowRateE70[1], linetype = "dashed", color = "#b30000")+
            annotate("rect", xmin = plot.min, xmax = plot.max, ymin = swft.data.out$FlowRateE70[1],     ymax = swft.data.out$FlowRateE[1],   alpha = 0.4, fill = "#00cc00")+
            annotate("rect", xmin = plot.min, xmax = plot.max, ymin = swft.data.out$FlowRateE70[1] - 2.5, ymax = swft.data.out$FlowRateE70[1], alpha = 0.4, fill = "#ff8c00")+
            annotate("rect", xmin = plot.min, xmax = plot.max, ymin = 0, ymax = swft.data.out$FlowRateE70[1]- 2.5, alpha = 0.4, fill = "red")+
            geom_point() +
            geom_line() + 
            scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "",breaks = c(round(swft.data.out$FlowRateE[1],2), round(swft.data.out$FlowRateE70[1],2)))) +
            scale_x_datetime(breaks = scales::pretty_breaks(n = 10), date_labels = "%Y-%m-%d") +
            labs(title = paste0(swft.data.out$SiteID[1], ": ECSE MFM 2-minute point data"),
                 x = "", y = "Flow Rate (SLPM)",
                 color = "Sensor", caption = flow.text) +
            theme(strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 270), text = element_text(color = "white", face = "bold", size = 20))
        }
        if(input$swft_EddyCo_data_type == "ecse.voltage") {
          message(paste0("Plot: ", input$swft_EddyCo_data_type, " for ", input$swft_EddyCo_site, " from ", input$swft_EddyCo_date_range[1], " - ", input$swft_EddyCo_date_range[2]))
          
          swft.plot <- ggplot(swft.data.out, aes(x=aggTime, y= mean, color = `Stream Name`))+
            geom_point() +
            geom_line() + 
            scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
            scale_x_datetime(breaks = scales::pretty_breaks(n = 10), date_labels = "%Y-%m-%d") +
            labs(title = paste0(swft.data.out$SiteID[1], ": ECSE Pump Voltage 2-minute point data"),
                 x = "", y = "Voltage",
                 color = "Sensor") +
            theme(strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 270), text = element_text(color = "white", face = "bold", size = 20))
          
        }
        if(input$swft_EddyCo_data_type == "ec.temps") {
          message(paste0("Plot: ", input$swft_EddyCo_data_type, " for ", input$swft_EddyCo_site, " from ", input$swft_EddyCo_date_range[1], " - ", input$swft_EddyCo_date_range[2]))
          
          plot.min <- min(swft.data.out$by30, na.rm = TRUE)
          plot.max <- max(swft.data.out$by30, na.rm = TRUE)
          
          # swft.data.out = swft.data.out %>%
          #   dplyr::mutate(`Stream Name`_test = ec.system)
          
          swft.plot <- ggplot(swft.data.out, aes(x = by30, y= mean, color = `Stream Name`))+
            annotate("rect", xmin = plot.min, xmax = plot.max, ymin = 35, ymax = 50, alpha = 0.4, fill = "red")+
            annotate("rect", xmin = plot.min, xmax = plot.max, ymin =  6, ymax = 0, alpha = 0.4, fill = "cyan")+
            geom_point(alpha = .63) +
            geom_line(alpha = .63) +
            scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
            scale_x_datetime(breaks = scales::pretty_breaks(n = 10), date_labels = "%Y-%m-%d") +
            labs(title = paste0(swft.data.out$SiteID[1], ": Hut Temperature 2-minute point data"), subtitle = "Averaged hourly",
                 x = "", y = "Temp in Celcius", color = "Temp Sensor") +
            theme(axis.text.x = element_text(angle = 270), text = element_text(color = "white", face = "bold", size = 20)) + # Option to angle the facet grid y panel text
            facet_wrap(~ec.system)
          
        }
      } else { 
        # If there are not rows in swft.data.out generate blank plot
        swft.plot = ggplot()+
          geom_text(label = "text")+
          annotate("text", label = paste0("No data found: ", input$swft_EddyCo_site, "\nData not available..."), x = 0, y = 0, color = "white", size = 12)
      }
      
      swft_ec_fast_collect_data_time_finish = Sys.time()
      
      # Check how log the whole process took
      output$swft_ec_fast_collect_data_time <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(
          value = paste0("Data collected in: ", round(difftime(swft_ec_fast_collect_data_time_finish, swft_ec_fast_collect_data_time_start, units = "secs"), 2)," s"),
          subtitle = "",
          width = 12,
          color = "black"
        )
      })
      
      # Return the holy plot and data files!!
      swft.ec.list = list(swft_plot = swft.plot, swft_data = swft.data.out)
      swft.ec.list
      
    })
    
    # Output Plot
    output$swft_ec_fast_plot <- shiny::renderPlot({
      if(input$swft_EddyCo_radioButton == "Enabled"){
        
        if(input$swft_EddyCo_radio_y_custom == "Yes"){
          
          if(input$swft_EddyCo_facet_wrap == "Yes"){
            swft_ec_fast_plot()$swft_plot +
              ylim(input$swft_EddyCo_y_lower, input$swft_EddyCo_y_upper) +
              facet_wrap(~`Stream Name`)+
              theme(legend.position = "none")
          } else {
            swft_ec_fast_plot()$swft_plot +
              ylim(input$swft_EddyCo_y_lower, input$swft_EddyCo_y_upper) 
            
          } 
          
        } else {
          
          if(input$swft_EddyCo_facet_wrap == "Yes"){
            swft_ec_fast_plot()$swft_plot +
              facet_wrap(~`Stream Name`)+
              theme(legend.position = "none")
          } else {
            swft_ec_fast_plot()$swft_plot 
            
          }
          
        }
        
      } else {
        swft_ec_fast_plot()$swft_plot
      }
    })
    
    # Output Table
    output$swft_ec_fast_table = DT::renderDataTable({
      data.table::data.table(swft_ec_fast_plot()$swft_data)
    })
    
  }
})