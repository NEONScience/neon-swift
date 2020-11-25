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
      
      # Pull data from S3
      message(input$swft_EddyCo_site, "from ", input$swft_EddyCo_date_range[1], " - ", input$swft_EddyCo_date_range[2], paste0(" Swft Data Gather Starting ... "))
      swft.data.in = read.eddy.inquiry(dataType  = "2min", 
                                       sensor    = input$swft_EddyCo_data_type, 
                                       siteID    = input$swft_EddyCo_site,
                                       startDate = as.Date(input$swft_EddyCo_date_range[1]), 
                                       endDate   = as.Date(input$swft_EddyCo_date_range[2]),
                                       silent    = FALSE
      )
      message(input$swft_EddyCo_site, "from ", input$swft_EddyCo_date_range[1], " - ", input$swft_EddyCo_date_range[2], paste0(" Swft Data Gathered: ", nrow(swft.data.in)))
      message("Data pull Finished")
      
      # For Function Testing
      # swft.data.in = read.eddy.inquiry(dataType  = "2min",
      #                                  sensor    = "ec.temps",
      #                                  siteID    = "WREF",
      #                                  startDate = Sys.Date()-7,
      #                                  endDate   = Sys.Date(),
      #                                  silent    = TRUE
      # )
      
      # First check to see there was any data in the pull.
      if(nrow(swft.data.in) > 0){
        
        # Storage Isotope Analyzer - G2131-i
        if(input$swft_EddyCo_data_type == "G2131"){
          
          # Used in every permuation, so lets make this Valve Data frame first! By joining this to the subsequent data frame, we can see what level was being sampled
          swft.g2131.valves = swft.data.in %>%
            dplyr::filter(stringr::str_detect(string = strm_name, pattern = "_Valve_")) %>%
            tidyr::separate(col = strm_name, into = c("Sensor", "Delete", "SampleLevel"), sep = "_") %>% 
            dplyr::filter(readout_val_double == 1) %>%
            dplyr::select(readout_time, SampleLevel) %>%
            dplyr::mutate(SampleLevel = ifelse(test = is.na(SampleLevel) == TRUE, yes = "Validation", no = SampleLevel)) 
          
          # Sub Data Type: CO2
          if(input$swft_EddyCo_sub_data_type_G2131 == "CO2"){
            
            swft.g2131.co2 = swft.data.in %>% 
              dplyr::filter(strm_name %in% c("G2131_fwMoleCo2")) 
            
            swft.data.out = dplyr::left_join(x = swft.g2131.co2, y = swft.g2131.valves, by = "readout_time")  %>%
              dplyr::mutate(SampleLevel = ifelse(test = is.na(SampleLevel) == TRUE, yes = "Validation", no = SampleLevel)) 
            
          }
          
          # Sub Data Type: Isotopes
          else if(input$swft_EddyCo_sub_data_type_G2131 == "Isotopes"){ 
            
            swift.g2131.isotopes = swft.data.in %>%
              dplyr::filter(strm_name %in% "G2131_13C_isotope")
            
            swft.data.out = dplyr::left_join(x = swift.g2131.isotopes, y = swft.g2131.valves, by = "readout_time") %>%
              dplyr::mutate(SampleLevel = ifelse(test = is.na(SampleLevel) == TRUE, yes = "Validation", no = SampleLevel)) %>%
              dplyr::filter(readout_val_double <= 0 & readout_val_double >= -20)
            
          }
          
          # Sub Data Type: Sample Valves
          else if(input$swft_EddyCo_sub_data_type_G2131 == "Sample Valves"){
            
            swft.data.out = swft.data.in %>%
              dplyr::filter(stringr::str_detect(string = strm_name, pattern = "_Valve_")) %>%
              tidyr::separate(col = strm_name, into = c("Sensor", "Delete", "SampleLevel"), sep = "_") %>%
              dplyr::mutate(strm_name = paste0("ML Solenoid ", SampleLevel) )  %>%
              dplyr::group_by(SiteID, strm_name) %>%
              dplyr::add_tally() %>%
              dplyr::summarise(
                n = n[1],
                Open  = sum(readout_val_double, na.rm = TRUE),
                Closed = n - Open,
                percentage = paste0(100*round(mean(readout_val_double, na.rm = TRUE),3), "%")
              ) %>%
              reshape2::melt(id.vars = c("SiteID","strm_name", "percentage")) %>%
              dplyr::filter(variable != "n")
          } else {
            swft.data.out = data.table::data.table()
          }
        # Storage CO2/H2O Analyzer - Li840A
        } else if(input$swft_EddyCo_data_type == "Li840"){
          
          message("Data: Li840")
          # The following logic steps are split out based upon which S3 metadata folder we will pull from
          # Since CO2 and H2O are housed in the same file, it only makes sense to pair these together to reduce redundant coding
          # The Li840 Valves are stored in their own file so these are seperate
          # The Li840 Flowrate is also stored in it's own file so this was pulled out seperately.
          
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
                tidyr::separate(col = strm_name, into = c("Sensor", "Delete", "SampleLevel"), sep = "_")
              
              # Join Clean Valve Data to d
              swft.li840.valves.clean.join = swft.li840.valves.clean %>% 
                dplyr::filter(readout_val_double == 1) %>%
                dplyr::select(readout_time, SampleLevel)
              
              
              if(input$swft_EddyCo_sub_data_type_Li840 == "CO2"){
              
                swft.data.out = swft.data.in %>%
                  dplyr::left_join(y = swft.li840.valves.clean.join, by = "readout_time") %>%
                  dplyr::mutate(SampleLevel = ifelse(test = is.na(SampleLevel) == TRUE, yes = "Unknown", no = SampleLevel)) %>%
                  dplyr::filter(strm_name == "Li840_CO2_fwMole")
                
                swft.li840.summary = swft.data.out %>%
                  dplyr::group_by(strm_name) %>%
                  dplyr::summarise(
                    median = stats::median(readout_val_double, na.rm = TRUE)
                  )
                
                swft.li840.co2.med = swft.li840.summary %>% dplyr::filter(strm_name == "Li840_CO2_fwMole")
                swft.li840.co2.med = swft.li840.co2.med$median[1]
                
              }
              
              if(input$swft_EddyCo_sub_data_type_Li840 == "H2O"){
                
                swft.data.out = swft.data.in %>%
                  dplyr::left_join(y = swft.li840.valves.clean.join, by = "readout_time") %>%
                  dplyr::mutate(SampleLevel = ifelse(test = is.na(SampleLevel) == TRUE, yes = "Unknown", no = SampleLevel)) %>%
                  dplyr::filter(strm_name == "Li840_H2O_fwMole")
                
                
              }
              
              
            }
            
          # If swft_EddyCo_sub_data_type_Li840 does == "Sample Valves"
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
                tidyr::separate(col = strm_name, into = c("Sensor", "Delete", "SampleLevel"), sep = "_") %>%
                dplyr::mutate(strm_name = paste0("ML Solenoid ", SampleLevel) )  %>%
                dplyr::group_by(SiteID, strm_name) %>%
                dplyr::add_tally() %>%
                dplyr::summarise(
                  n = n[1],
                  Open  = sum(readout_val_double, na.rm = TRUE),
                  Closed = n - Open,
                  percentage = paste0(100*round(mean(readout_val_double, na.rm = TRUE),3), "%")
                ) %>%
                reshape2::melt(id.vars = c("SiteID","strm_name", "percentage")) %>%
                dplyr::filter(variable != "n")
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
            dplyr::filter(stringr::str_detect(string = strm_name, pattern = "_Valve") )%>%
            dplyr::distinct() %>%
            tidyr::separate(col = strm_name, into = c("Sensor", "Delete", "SampleLevel"), sep = "_") %>%
            dplyr::mutate(Sensor = ifelse (Sensor == "L2130", yes = "L2130-I", no = Sensor))  %>%
            dplyr::mutate(SampleLevel = ifelse (SampleLevel == "Valve", yes = "Validation", no = SampleLevel)) %>%
            dplyr::filter(readout_val_double == 1) %>%
            dplyr::select(readout_time, SampleLevel)
          
          if(input$swft_EddyCo_sub_data_type_L2130 == "Isotope - 2H"){
            swft.data.out = swft.data.in %>%
              dplyr::filter(strm_name == "L2130_2H_isotope") %>%
              dplyr::left_join(y = swft.l2130.valves, by = "readout_time") %>%
              dplyr::filter(readout_val_double <= 0 & readout_val_double > -300)
              
          }
          if(input$swft_EddyCo_sub_data_type_L2130 == "Isotope - 18O"){
            swft.data.out = swft.data.in %>%
              dplyr::filter(strm_name == "L2130_18O_isotope") %>%
              dplyr::left_join(y = swft.l2130.valves, by = "readout_time") %>%
              dplyr::filter(readout_val_double <= 0 & readout_val_double > -50)
          }
          if(input$swft_EddyCo_sub_data_type_L2130 == "H2O"){
            swft.data.out = swft.data.in %>%
              dplyr::filter(strm_name == "L2130_H2O") %>%
              dplyr::left_join(y = swft.l2130.valves, by = "readout_time")
          }
          if(input$swft_EddyCo_sub_data_type_L2130 == "Sample Valves"){
            swft.data.out = swft.data.in %>%
              dplyr::filter(stringr::str_detect(string = strm_name, pattern = "_Valve"))%>%
              tidyr::separate(col = strm_name, into = c("Sensor", "Delete", "SampleLevel"), sep = "_") %>%
              dplyr::mutate(Sensor = ifelse (Sensor == "L2130", yes = "L2130-I", no = Sensor))  %>%
              dplyr::mutate(SampleLevel = ifelse (SampleLevel == "Valve", yes = "Validation", no = SampleLevel)) %>%
              dplyr::mutate(strm_name = paste0("ML Solenoid ", SampleLevel) )  %>%
              dplyr::group_by(SiteID, strm_name) %>%
              dplyr::add_tally() %>%
              dplyr::summarise(
                n = n[1],
                Open  = sum(readout_val_double, na.rm = TRUE),
                Closed = n - Open,
                percentage = paste0(100*round(mean(readout_val_double, na.rm = TRUE),3), "%")
              ) %>%
              reshape2::melt(id.vars = c("SiteID","strm_name", "percentage")) %>%
              dplyr::filter(variable != "n")
          }
          
        } else if(input$swft_EddyCo_data_type == "Li7200"){
          
          if(input$swft_EddyCo_sub_data_type_Li7200 == "CO2"){
            
            swft.data.out = swft.data.in %>%
              dplyr::filter(strm_name == "Li7200_CO2")
            
          }
          if(input$swft_EddyCo_sub_data_type_Li7200 == "H2O"){
            
            swft.data.out = swft.data.in %>%
              dplyr::filter(strm_name == "Li7200_fdMoleH2O")
            
          }
          if(input$swft_EddyCo_sub_data_type_Li7200 == "Flow"){
            
            swft.data.out = swft.data.in %>%
              dplyr::filter(strm_name == "Li7200_MFCSampleFlow")
            
          }
          if(input$swft_EddyCo_sub_data_type_Li7200 == "Signal Strength"){
            
            swft.data.out = swft.data.in %>%
              dplyr::filter(strm_name %in% c("Li7200_CO2SglStr","Li7200_H2oSglStr"))
            
          }
          if(input$swft_EddyCo_sub_data_type_Li7200 == "Cell Temp"){
            
            swft.data.out = swft.data.in %>%
              dplyr::filter(strm_name %in% c("Li7200_cellTempIn", "Li7200_cellTempOut")) %>%
              reshape2::dcast(readout_time + SiteID ~ strm_name, value.var = "readout_val_double",fun.aggregate = mean) %>%
              dplyr::mutate(temp_diff = Li7200_cellTempIn - Li7200_cellTempOut)
            
          }
          if(input$swft_EddyCo_sub_data_type_Li7200 == "Pressure Differential"){
            
            swft.data.out = swft.data.in %>%
              dplyr::filter(strm_name == "Li7200_pDiff")
            
          }
          if(input$swft_EddyCo_sub_data_type_Li7200 == "Diagnostic"){
            
            swft.data.out = swft.data.in %>%
              dplyr::filter(strm_name == "Li7200_Diag") %>%
              dplyr::mutate(Diagnostic = as.factor(readout_val_double))
            
          }
          # if(input$swft_EddyCo_sub_data_type_Li7200 == ""){
          #   
          # }
          
        } else if(input$swft_EddyCo_data_type == "CSAT3"){
          

          message("Data CSAT3")
          swft.data.out = swft.data.in
          
          
          
        } else if(input$swft_EddyCo_data_type == "amrs"){
          
          swft.data.out <- swft.data.in %>%
            dplyr::mutate(day = lubridate::ymd(base::substr(x = readout_time, start = 1, stop = 11))) %>%
            dplyr::group_by(strm_name, day) %>%
            dplyr::summarise(
              SiteID = SiteID[1],
              mean_daily = mean(readout_val_double)
            ) %>%
            reshape2::dcast(SiteID + day ~ strm_name, value.var = "mean_daily", fun.aggregate = mean) 
          
          if(swft.data.out$SiteID == "SCBI"){
              yInt = 11
              xInt = -14
            } else if(swft.data.out$SiteID == "GUAN") {
              yInt = 0
              xInt = 11
            } else if(swft.data.out$SiteID == "UKFS") {
              yInt = 7
              xInt = -10
            } else if(swft.data.out$SiteID == "TALL") {
              yInt = 6
              xInt = 0
            } else if(swft.data.out$SiteID == "NIWO") {
              yInt = 8
              xInt = -9
            } else if(swft.data.out$SiteID == "RMNP") {
              yInt = -7
              xInt = 8
            } else if(swft.data.out$SiteID == "SOAP") {
              yInt = 18
              xInt = -7
            } else if(swft.data.out$SiteID == "TEAK") {
              yInt = 6
              xInt = 7
            } else {
              yInt = 0
              xInt = 0
            }
          
          } else if(input$swft_EddyCo_data_type == "ecse.mfm"){
            message("Silly")
            swft.ecse.ml.flow <- swft.data.in %>%
              dplyr::mutate(strm_name = trimws(strm_name)) %>%
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
              dplyr::group_by(SiteID, strm_name, aggTime,FlowRateE,FlowRateE70) %>%
              dplyr::summarise(
                mean = mean(readout_val_double, na.rm = TRUE)
              ) 
            
            
            
          } else if(input$swft_EddyCo_data_type == "HMP155"){
            

            
            if(input$swft_EddyCo_sub_data_type_HMP155 == "Relative Humidity"){
              swft.data.out = swft.data.in %>%
                dplyr::filter(strm_name == "HMP155_RH")
            }
            if(input$swft_EddyCo_sub_data_type_HMP155 == "Temperature"){
              swft.data.out = swft.data.in %>%
                dplyr::filter(strm_name == "HMP155_temp")
            }
            if(input$swft_EddyCo_sub_data_type_HMP155 == "Dew Point"){
              swft.data.out = swft.data.in %>%
                dplyr::filter(strm_name == "HMP155_DewPoint")
            }

        } else if(input$swft_EddyCo_data_type == "ecse.voltage"){
          
          swft.data.out = swft.data.in %>%
            dplyr::mutate(aggTime = cut(by15, breaks = "1 hours")) %>%
            dplyr::mutate(aggTime = lubridate::ymd_hms(aggTime)) %>%
            dplyr::group_by(SiteID, strm_name, aggTime) %>%
            dplyr::summarise(
              mean = mean(mean)
            )
            
        
        } else if(input$swft_EddyCo_data_type == "ec.temps"){
          
          swft.data.out = swft.data.in %>%
            dplyr::filter(strm_name %in% c("ecte_mfc_sample_tempC", "ecte_mfc_valid_tempC",
                                           "ecse_comet_tempC","ecse_comet_tempHut", "ecse_mfc_sample_tempC","ecse_mfc_valid_tempC" #,
                                           # "HMP155_temp","HMP155_temp_sp3"
            )) %>%
            dplyr::mutate(day_hour = substr(x = readout_time, start = 1, stop = 13))%>%
            dplyr::group_by(strm_name, day_hour) %>%
            dplyr::summarise(
              SiteID = SiteID[1],
              readout_temp = mean(readout_val_double, na.rm = TRUE)
            ) %>%
            tidyr::separate(col = strm_name, into = c("ec.system", "sensor","type","na"), sep = "_", remove = FALSE) %>%
            dplyr::select(-na) %>%
            dplyr::mutate(type = ifelse(test = type == "tempHut", yes = "comet", no = type)) %>%
            dplyr::mutate(type = ifelse(test = type == "tempC", yes = "comet", no = type)) %>%
            dplyr::mutate(ec.system = ifelse(test = ec.system == "ecse", yes = "Instrument Hut", no = "Environmental Enclosure")) %>%
            dplyr::mutate(day_hour = lubridate::ymd_h(day_hour))
          
        
        } else {
          swft.data.out = data.table::data.table()
        }
      } else {
        swft.data.out = data.table::data.table()
      }

      #################################################                            Plotting Logic                            #################################################

      if(nrow(swft.data.out) > 0){
        
        theme_set(theme_bw()) 

        # Storage Isotope Analyzer - G2131-i
        if(input$swft_EddyCo_data_type == "G2131"){
          if(input$swft_EddyCo_sub_data_type_G2131 == "CO2"){
            message("Plot: G2131 - CO2")
            swft.plot = ggplot(data = swft.data.out, aes(x = readout_time, y = readout_val_double, color = SampleLevel)) +
              geom_point() +
              scale_x_datetime(breaks = scales::pretty_breaks(n = 10)) +
              scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
              labs(x = "", y = "Concentration (ppm)", title = paste0(input$swft_EddyCo_site, " - ", input$swft_EddyCo_data_type, " ", input$swft_EddyCo_sub_data_type), 
                   subtitle = paste0(input$swft_EddyCo_date_range[1], " to ", input$swft_EddyCo_date_range[2])) +
              theme(strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 270), text = element_text(size = 20)) +
              facet_wrap(~strm_name)
            
          }
          
          if(input$swft_EddyCo_sub_data_type_G2131 == "Isotopes"){
            message("Plot: G2131 - Isotopes")
            swft.plot = ggplot(data = swft.data.out, aes(x = readout_time, y = readout_val_double, color = SampleLevel)) +
              geom_point() +
              scale_x_datetime(breaks = scales::pretty_breaks(n = 10)) +
              scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
              labs(x = "", y = "Concentration", title = paste0(input$swft_EddyCo_site, " - ", input$swft_EddyCo_data_type, " ", input$swft_EddyCo_sub_data_type), 
                   subtitle = paste0(input$swft_EddyCo_date_range[1], " to ", input$swft_EddyCo_date_range[2])) +
              theme(strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 270), text = element_text(size = 20)) +
              facet_wrap(~strm_name)
      
          }
          
          if(input$swft_EddyCo_sub_data_type_G2131 == "Sample Valves"){
            message("Plot: G2131 - Sample Valves")
            swft.plot = ggplot() +
              geom_col(data = swft.data.out, aes(x = variable, y = value, fill = variable)) +
              geom_text(data = swft.data.out %>% dplyr::filter(variable == "Open"), aes(x = variable, y = value, label = percentage, vjust = -1)) +
              scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
              labs(x = "", y = "Counts", title = paste0(input$swft_EddyCo_site, " - ", input$swft_EddyCo_data_type, " ", input$swft_EddyCo_sub_data_type), 
                   subtitle = paste0(input$swft_EddyCo_date_range[1], " to ", input$swft_EddyCo_date_range[2])) +
              theme(strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 270), text = element_text(size = 20)) +
              facet_wrap(~strm_name)
          }
        }
        
        # Storage CO2/H2O Analyzer - Li840A
        if(input$swft_EddyCo_data_type == "Li840"){
          if(input$swft_EddyCo_sub_data_type_Li840 == "CO2"){
            message("Plot: Li840 - CO2")
            swft.plot = ggplot(data = swft.data.out, aes(x = readout_time, y = readout_val_double, color = SampleLevel)) +
              geom_point() +
              scale_y_continuous(limits = c(swft.li840.co2.med - 100, swft.li840.co2.med + 100), breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
              scale_x_datetime(breaks = scales::pretty_breaks(n = 10)) +
              labs(x = "", y = "Concentration (ppm)", title = paste0(input$swft_EddyCo_site, " - ", input$swft_EddyCo_data_type, " ", input$swft_EddyCo_sub_data_type), 
                   subtitle = paste0(input$swft_EddyCo_date_range[1], " to ", input$swft_EddyCo_date_range[2])) +
              theme(strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 270), text = element_text(size = 20)) +
              facet_wrap(~strm_name)
         
          }
          if(input$swft_EddyCo_sub_data_type_Li840 == "H2O"){
            message("Plot: Li840 - H2O")
            swft.plot = ggplot(data = swft.data.out, aes(x = readout_time, y = readout_val_double, color = SampleLevel)) +
              geom_point() +
              scale_x_datetime(breaks = scales::pretty_breaks(n = 10)) +
              scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
              labs(x = "", y = "Concentration (ppm)", title = paste0(input$swft_EddyCo_site, " - ", input$swft_EddyCo_data_type, " ", input$swft_EddyCo_sub_data_type), 
                   subtitle = paste0(input$swft_EddyCo_date_range[1], " to ", input$swft_EddyCo_date_range[2])) +
              theme(strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 270), text = element_text(size = 20)) +
              facet_wrap(~strm_name)
            
          }
          if(input$swft_EddyCo_sub_data_type_Li840 == "Sample Valves"){
            message("Plot: Li840 - Sample Valves")
            swft.plot = ggplot() +
              geom_col(data = swft.data.out, aes(x = variable, y = value, fill = variable)) +
              geom_text(data = swft.data.out %>% dplyr::filter(variable == "Open"), aes(x = variable, y = value, label = percentage, vjust = -1)) +
              scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
              labs(x = "", y = "Counts", title = paste0(input$swft_EddyCo_site, " - ", input$swft_EddyCo_data_type, " ", input$swft_EddyCo_sub_data_type), 
                   subtitle = paste0(input$swft_EddyCo_date_range[1], " to ", input$swft_EddyCo_date_range[2])) +
              theme(strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 270), text = element_text(size = 20)) +
              facet_wrap(~strm_name)
          }
          if(input$swft_EddyCo_sub_data_type_Li840 == "Flow Rate"){
            swft.plot <- ggplot(swft.data.out, aes(x=readout_time, y= readout_val_double))+
              geom_point(shape = 1, alpha = .25, color = "#c51b7d") +
              geom_hline(yintercept = 1.0, linetype = "dashed") +
              scale_color_manual(values = c("#e41a1c","#377eb8","#4daf4a")) +
              scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
              scale_x_datetime(date_labels = "%m-%d", date_breaks = "1 day") +
              labs(title = paste0(swft.data.out$SiteID[1], ": Li840 MFC Flow Rate 2-minute point data"),
                   x = "", y = "Flow (SLPM)",
                   color = "Sensor") +
              theme(strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 270), text = element_text(size = 20))

          }
        } # End Li840 if
        if(input$swft_EddyCo_data_type == "L2130"){
          if(input$swft_EddyCo_sub_data_type_L2130 %in% c("Isotope - 2H", "Isotope - 18O")){
            swft.plot = ggplot(swft.data.out, aes(x = readout_time, y = readout_val_double, color = SampleLevel)) +
              geom_point(alpha = .6) +
              scale_x_datetime(breaks = scales::pretty_breaks(n = 10)) +
              scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
              theme(strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 270), text = element_text(size = 20)) +
              labs(x = "", y = "per mil", title = paste0(input$swft_EddyCo_site, " - ", input$swft_EddyCo_data_type, " ", input$swft_EddyCo_sub_data_type), 
                   subtitle = paste0(input$swft_EddyCo_date_range[1], " to ", input$swft_EddyCo_date_range[2]))
          }
          if(input$swft_EddyCo_sub_data_type_L2130 == "H2O"){
            swft.plot = ggplot(swft.data.out, aes(x = readout_time, y = readout_val_double, color = SampleLevel)) +
              geom_point(alpha = .6) +
              scale_x_datetime(breaks = scales::pretty_breaks(n = 10)) +
              scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
              theme(strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 270), text = element_text(size = 20)) +
              labs(x = "", y = "Micromoles Per Mole", title = paste0(input$swft_EddyCo_site, " - ", input$swft_EddyCo_data_type, " ", input$swft_EddyCo_sub_data_type), 
                   subtitle = paste0(input$swft_EddyCo_date_range[1], " to ", input$swft_EddyCo_date_range[2]))
          }
          if(input$swft_EddyCo_sub_data_type_L2130 == "Sample Valves"){
            swft.plot = ggplot() +
              geom_col(data = swft.data.out, aes(x = variable, y = value, fill = variable)) +
              geom_text(data = swft.data.out %>% dplyr::filter(variable == "Open"), aes(x = variable, y = value, label = percentage, vjust = -1)) +
              scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
              theme(strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 270), text = element_text(size = 20)) +
              labs(x = "", y = "Counts", title = paste0(input$swft_EddyCo_site, " - ", input$swft_EddyCo_data_type, " ", input$swft_EddyCo_sub_data_type), 
                   subtitle = paste0(input$swft_EddyCo_date_range[1], " to ", input$swft_EddyCo_date_range[2])) +
              facet_wrap(~strm_name)
          }
        }
        
        if(input$swft_EddyCo_data_type == "Li7200"){
          if(input$swft_EddyCo_sub_data_type_Li7200 == "CO2"){
            message("swft_plot_Li7200_CO2")
            swft.plot = ggplot(swft.data.out, aes(x = readout_time, y = readout_val_double)) +
              geom_point(alpha = .6, color = "#4d9221") +
              scale_x_datetime(breaks = scales::pretty_breaks(n = 10)) +
              scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
              theme(strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 270), text = element_text(size = 20)) +
              labs(x = "", y = "Concentration (ppm)", title = paste0(input$swft_EddyCo_site, " - ", input$swft_EddyCo_data_type, " ", input$swft_EddyCo_sub_data_type), 
                   subtitle = paste0(input$swft_EddyCo_date_range[1], " to ", input$swft_EddyCo_date_range[2]))
          }
          if(input$swft_EddyCo_sub_data_type_Li7200 == "H2O"){
            message("swft_plot_Li7200_H2O")
            swft.plot = ggplot(swft.data.out, aes(x = readout_time, y = readout_val_double)) +
              geom_point(alpha = .6, color = "#386cb0") +
              scale_x_datetime(breaks = scales::pretty_breaks(n = 10)) +
              scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
              theme(strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 270), text = element_text(size = 20)) +
              labs(x = "", y = "Concentration (ppm)", title = paste0(input$swft_EddyCo_site, " - ", input$swft_EddyCo_data_type, " ", input$swft_EddyCo_sub_data_type), 
                   subtitle = paste0(input$swft_EddyCo_date_range[1], " to ", input$swft_EddyCo_date_range[2]))
          }
          if(input$swft_EddyCo_sub_data_type_Li7200 == "Flow"){
            message("swft_plot_Li7200_Flow")
            swft.plot = ggplot(swft.data.out, aes(x = readout_time, y = readout_val_double)) +
              geom_point(alpha = .6, color = "#c51b7d") +
              geom_hline(yintercept = 12, linetype = "dashed") +
              scale_x_datetime(breaks = scales::pretty_breaks(n = 10)) +
              scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
              theme(strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 270), text = element_text(size = 20)) +
              labs(x = "", y = "", title = paste0(input$swft_EddyCo_site, " - ", input$swft_EddyCo_data_type, " ", input$swft_EddyCo_sub_data_type), 
                   subtitle = paste0(input$swft_EddyCo_date_range[1], " to ", input$swft_EddyCo_date_range[2]))
          }
          if(input$swft_EddyCo_sub_data_type_Li7200 == "Signal Strength"){
            message("swft_plot_Li7200_Signal Strength")
            swft.plot = ggplot(swft.data.out, aes(x = readout_time, y = readout_val_double, color = strm_name)) +
              geom_point(alpha = .6) +
              scale_color_manual(values = c("#4d9221","#386cb0","grey"))+
              scale_x_datetime(breaks = scales::pretty_breaks(n = 10)) +
              scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
              theme(strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 270), text = element_text(size = 20)) +
              labs(x = "", y = "", title = paste0(input$swft_EddyCo_site, " - ", input$swft_EddyCo_data_type, " ", input$swft_EddyCo_sub_data_type), 
                   subtitle = paste0(input$swft_EddyCo_date_range[1], " to ", input$swft_EddyCo_date_range[2]))
          }
          if(input$swft_EddyCo_sub_data_type_Li7200 == "Cell Temp"){
            message("swft_plot_Li7200_Cell Temp")
            swft.plot = ggplot(swft.data.out, aes(x = readout_time, y = temp_diff)) +
              geom_point(alpha = .6, color = "black") +
              scale_x_datetime(breaks = scales::pretty_breaks(n = 10)) +
              scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
              theme(strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 270), text = element_text(size = 20)) +
              labs(x = "", y = "", title = paste0(input$swft_EddyCo_site, " - ", input$swft_EddyCo_data_type, " ", input$swft_EddyCo_sub_data_type), 
                   subtitle = paste0(input$swft_EddyCo_date_range[1], " to ", input$swft_EddyCo_date_range[2]))
          }
          if(input$swft_EddyCo_sub_data_type_Li7200 == "Pressure Differential"){
            message("swft_plot_Li7200_Pressure Differential")
            swft.plot = ggplot(swft.data.out, aes(x = readout_time, y = readout_val_double)) +
              geom_point(alpha = .6, color = "#2A3439") +
              scale_x_datetime(breaks = scales::pretty_breaks(n = 10)) +
              scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
              theme(strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 270), text = element_text(size = 20)) +
              labs(x = "", y = "", title = paste0(input$swft_EddyCo_site, " - ", input$swft_EddyCo_data_type, " ", input$swft_EddyCo_sub_data_type), 
                   subtitle = paste0(input$swft_EddyCo_date_range[1], " to ", input$swft_EddyCo_date_range[2]))
          }
          if(input$swft_EddyCo_sub_data_type_Li7200 == "Diagnostic"){
            message("swft_plot_Li7200_Diagnostic")
            swft.plot = ggplot(swft.data.out, aes(x = readout_time, y= as.integer(readout_val_double), color = Diagnostic))+
              geom_hline(yintercept = 8191, linetype = "dashed", color = "black") +
              scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
              scale_x_datetime(breaks = scales::pretty_breaks(n = 10)) +
              labs(title = paste0(swft.data.out$SiteID[1], ": Li7200 Diagnostic 2-minute point data"),
                   x = "", y = "Diagnostic",
                   color = "Diagnostic Code") +
              theme(strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 270), text = element_text(size = 20)) +
              labs(x = "", y = "", title = paste0(input$swft_EddyCo_site, " - ", input$swft_EddyCo_data_type, " ", input$swft_EddyCo_sub_data_type), 
                   subtitle = paste0(input$swft_EddyCo_date_range[1], " to ", input$swft_EddyCo_date_range[2]))
          } 
        }
        
        if(input$swft_EddyCo_data_type == "CSAT3"){
          message("Plot CSAT3")
          colors <- c("NA" = "grey","0" = "limegreen", "1" = "gold",
                      "2" = "yellow", "3" = "orange","4" = "red")
          
          swft.plot <- ggplot(swft.data.out, aes(x=readout_time,y=CSAT3_WindVector,color = CodeSum,`SpeedOfSoundAgreement` = SpdSndAgreement,`PoorSignalLock` = PoorSignalLock,
                                         `AmplitudeTooHigh` = AmplitudeHigh,`AmplitudeTooLow` = AmplitudeLow))+
            geom_point(alpha = 0.5, shape = 1)+
            scale_x_datetime(breaks = scales::pretty_breaks(n = 10)) +
            scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
            scale_color_manual(values = colors)+
            theme(text = element_text(size = 20)) +
            labs(title = paste0(swft.data.out$SiteID[1], ": CSAT3 Wind Speed 2-minute point data"), x = "Timestamp", y = "Wind Speed (m/s)")
          
        }
        
        if(input$swft_EddyCo_data_type == "amrs"){
          swft.plot <-  ggplot(swft.data.out, aes(x = AMRS_x, y= AMRS_y, Time = day, color = as.factor(day)))+
            geom_hline(yintercept = yInt,linetype = "dashed",alpha = 0.75)+
            geom_vline(xintercept = xInt,linetype = "dashed",alpha = 0.75)+
            geom_point(size = 3.5)+ # Make Points, alpha gets lighter the older the date
            scale_color_viridis_d()+
            stat_summary(fun.y=median, geom="line", alpha = .35) + # Draw lines to same sites
            geom_segment(x=xInt-1,y=yInt+1, xend=xInt+1, yend=yInt+1, color="black", aes(color = "1x1 box"))+   # Create 1x1 box TOP
            geom_segment(x=xInt-1,y=yInt-1, xend=xInt+1, yend=yInt-1, color="black", aes(color = "1x1 box"))+ # Create 1x1 box Bottom
            geom_segment(x=xInt-1,y=yInt-1, xend=xInt-1, yend=yInt+1, color="black", aes(color = "1x1 box"))+ # Create 1x1 box Left
            geom_segment(x=xInt+1,y=yInt+1, xend=xInt+1, yend=yInt-1, color="black", aes(color = "1x1 box"))+   # Create 1x1 box Right
            scale_x_continuous(limits=c(xInt-5, xInt+5), breaks = c(xInt,-5,-3,-1,0,1,3,5)) + # Set limits based upon max potential values
            scale_y_continuous(limits=c(yInt-5, yInt+5), breaks = c(yInt,-5,-3,-1,0,1,3,5)) + # Set limits based upon max potential values
            geom_hline(yintercept = 0)+ # Set y-axis
            geom_vline(xintercept = 0)+ # Set x-axis
            theme(text = element_text(size = 20)) +
            labs(title = paste0("AMRS Pitch and Roll at ", swft.data.out$SiteID[1]),x="AMRS Roll",y="AMRS Pitch", color = "Date") # Create labels to make plot legible
        }
        
        if(input$swft_EddyCo_data_type == "HMP155"){
          if(input$swft_EddyCo_sub_data_type_HMP155 == "Relative Humidity"){
            
            swft.plot = ggplot(swft.data.out, aes(x = readout_time, y= readout_val_double))+
              geom_point(alpha = .4, color = "#73C0EC") +
              scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
              scale_x_datetime(breaks = scales::pretty_breaks(n = 10)) +
              labs(title = paste0(swft.data.out$SiteID[1], ": HMP155 Relative Humidity 2-minute point data"),
                   x = "", y = "%") +
              theme(strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 270), text = element_text(size = 20)) +
              labs(x = "", y = "", title = paste0(input$swft_EddyCo_site, " - ", input$swft_EddyCo_data_type, " ", input$swft_EddyCo_sub_data_type), 
                   subtitle = paste0(input$swft_EddyCo_date_range[1], " to ", input$swft_EddyCo_date_range[2]))
          }
          if(input$swft_EddyCo_sub_data_type_HMP155 == "Temperature"){
            
            swft.plot = ggplot(swft.data.out, aes(x = readout_time, y= readout_val_double))+
              geom_point(alpha = .4, color = "#00b200")+
              scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
              scale_x_datetime(breaks = scales::pretty_breaks(n = 10)) +
              labs(title = paste0(swft.data.out$SiteID[1], ": HMP155 Temperature 2-minute point data"),
                   x = "", y = "Temperature (C)") +
              theme(strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 270), text = element_text(size = 20)) +
              labs(x = "", y = "", title = paste0(input$swft_EddyCo_site, " - ", input$swft_EddyCo_data_type, " ", input$swft_EddyCo_sub_data_type), 
                   subtitle = paste0(input$swft_EddyCo_date_range[1], " to ", input$swft_EddyCo_date_range[2]))
          }
          if(input$swft_EddyCo_sub_data_type_HMP155 == "Dew Point"){
            
            swft.plot = ggplot(swft.data.out, aes(x = readout_time, y= readout_val_double))+
              geom_point(alpha = .4, color = "red")+
              scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
              scale_x_datetime(breaks = scales::pretty_breaks(n = 10)) +
              labs(title = paste0(swft.data.out$SiteID[1], ": HMP155 Dew Point 2-minute point data"),
                   x = "", y = "Temperature (C)") +
              theme(strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 270), text = element_text(size = 20)) +
              labs(x = "", y = "", title = paste0(input$swft_EddyCo_site, " - ", input$swft_EddyCo_data_type, " ", input$swft_EddyCo_sub_data_type), 
                   subtitle = paste0(input$swft_EddyCo_date_range[1], " to ", input$swft_EddyCo_date_range[2]))
          }
        }
          
        if(input$swft_EddyCo_data_type == "ecse.mfm"){
          
          swft.plot <- ggplot(swft.data.out, aes(x=aggTime, y= mean, color = strm_name))+
            geom_point() +
            geom_line() + 
            geom_hline(yintercept = swft.data.out$FlowRateE, linetype = "dashed", color = "#006d2c")+
            geom_hline(yintercept = swft.data.out$FlowRateE70, linetype = "dashed", color = "#b30000")+
            scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
            scale_x_datetime(breaks = scales::pretty_breaks(n = 10)) +
            labs(title = paste0(swft.data.out$SiteID[1], ": ECSE MFM 2-minute point data"),
                 x = "", y = "Flow Rate (SLPM)",
                 color = "Sensor", caption = flow.text) +
            theme(strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 270), legend.position = "none", text = element_text(size = 20)) +
            facet_wrap(~strm_name, nrow = 3)
        }
        
        if(input$swft_EddyCo_data_type == "ecse.voltage") {
          
          swft.plot <- ggplot(swft.data.out, aes(x=aggTime, y= mean, color = strm_name))+
            geom_point() +
            geom_line() + 
            scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
            scale_x_datetime(breaks = scales::pretty_breaks(n = 10)) +
            labs(title = paste0(swft.data.out$SiteID[1], ": ECSE Pump Voltage 2-minute point data"),
                 x = "", y = "Voltage",
                 color = "Sensor") +
            theme(strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 270), legend.position = "none", text = element_text(size = 20)) +
            facet_wrap(~strm_name, nrow = 3)
          
        }
        
        if(input$swft_EddyCo_data_type == "ec.temps") {
          
          swft.plot <- ggplot(swft.data.out, aes(x=day_hour, y= readout_temp, color = type))+
            geom_point(alpha = .63) +
            geom_line(alpha = .63) +
            scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
            scale_x_datetime(breaks = scales::pretty_breaks(n = 10)) +
            labs(title = paste0(swft.data.out$SiteID[1], ": Hut Temperature 2-minute point data"), subtitle = "Averaged hourly",
                 x = "", y = "Temp in Celcius", color = "Temp Sensor") +
            theme(axis.text.x = element_text(angle = 270), text = element_text(size = 20)) + # Option to angle the facet grid y panel text
            facet_grid(~ec.system)
          
        }
        
        
      } else { 
        # If there are not rows in swft.data.out generate blank plot
        swft.plot = ggplot()+
          geom_text(label = "text")+
          annotate("text", label = paste0("No data found: ", input$swft_EddyCo_site, "\n(Site communications may be down or data transitions have failed)."), x = 0, y = 0, color = "black")+
          theme_minimal()
      }
      
      swft_ec_fast_collect_data_time_finish = Sys.time()
      
      # Check how log the whole process took
      output$swft_ec_fast_collect_data_time <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(
          value = paste0("Data Collected in: ", round(difftime(swft_ec_fast_collect_data_time_finish, swft_ec_fast_collect_data_time_start, units = "secs"), 2)," seconds."),
          subtitle = "",
          width = 12,
          color = "navy"
        )
      })
      # Check how many rows of data were pulled
      output$swft_ec_fast_data_points <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(
          value = paste0("Data points: ", nrow(swft.data.out)),
          subtitle = "",
          width = 12,
          color = "navy"
        )
      })
      
      # Return the holy plot file!
      swft.plot
      
    })

    # Output Plot
    output$swft_ec_fast_plot <- shiny::renderPlot({
      
      
      if(input$swft_EddyCo_radioButton == "Enabled"){
        
        # Debounce would be nice, failed to get it working though.
        swft_ec_fast_plot() +
          ylim(input$swft_EddyCo_y_lower, input$swft_EddyCo_y_upper)
        
      } else {
        swft_ec_fast_plot()
      }
      
        
    })
    
    
    # Output Table
    # output$swft_ec_fast.table = DT::renderDataTable({
    #   swft.data.out
    # })
    
  }
})