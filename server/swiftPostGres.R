shiny::observeEvent(input$menu, {
  if(input$menu == "swft_postgres_tab"){
    
    swft_postgres_plot = shiny::eventReactive(input$swft_postgres_actionButton,{
      
      db_con = eddycopipe::build_postgres_connection()
      
      
      
      ################################################################################################
      ################################################################################################
      ################################################################################################
      ########                                                                                ########
      ########                                                                                ########
      ########                                                                                ########
      ########                            Format the query!                                   ########
      ########                                                                                ########
      ########                                                                                ########
      ########                                                                                ########
      ################################################################################################
      ################################################################################################
      ################################################################################################
      
      #######              ECSE MFM Flows              ####### 
      if(input$swft_postgres_data_type == "ecse.mfm"){
        streams_to_pull = c('ML1_MFM_FlowRate', 'ML2_MFM_FlowRate', 'ML3_MFM_FlowRate', 'ML4_MFM_FlowRate','ML5_MFM_FlowRate','ML6_MFM_FlowRate','ML7_MFM_FlowRate','ML8_MFM_FlowRate')
        valves_are_available = FALSE
        plot_labs = labs(x = "", y = "Flowrate (SLPM)", title = paste0(input$swft_postgres_site, " - ", input$swft_postgres_data_type), subtitle = paste0(input$swft_postgres_date_range[1], " to ", input$swft_postgres_date_range[2])) 
      }
      #######              G2131 Picarro               ####### 
      if(input$swft_postgres_data_type == "G2131-I"){
        if(input$swft_postgres_sub_data_type_G2131 == "CO2"){                                                                 
          streams_to_pull = c('G2131_fwMoleCo2')
          valves_are_available = TRUE
          plot_labs = labs(x = "", y = "Concentration (ppm)", title = paste0(input$swft_postgres_site, " - ", input$swft_postgres_data_type, " ", input$swft_postgres_sub_data_type_G2131), subtitle = paste0(input$swft_postgres_date_range[1], " to ", input$swft_postgres_date_range[2])) 
        }
        if(input$swft_postgres_sub_data_type_G2131 == "H2O"){                                                                 
          streams_to_pull = c('G2131_percentFwMoleH2O')
          valves_are_available = TRUE
          plot_labs = labs(x = "", y = "", title = paste0(input$swft_postgres_site, " - ", input$swft_postgres_data_type, " ", input$swft_postgres_sub_data_type_G2131), subtitle = paste0(input$swft_postgres_date_range[1], " to ", input$swft_postgres_date_range[2])) 
        }
        if(input$swft_postgres_sub_data_type_G2131 == "Isotopes"){                                                                 
          streams_to_pull = c('G2131_13C_isotope')
          valves_are_available = TRUE
          plot_labs = labs(x = "", y = "Concentration", title = paste0(input$swft_postgres_site, " - ", input$swft_postgres_data_type, " ", input$swft_postgres_sub_data_type_G2131), subtitle = paste0(input$swft_postgres_date_range[1], " to ", input$swft_postgres_date_range[2])) 
        }
        if(input$swft_postgres_sub_data_type_G2131 == "Sample Valves"){                                                                 
          streams_to_pull = c('G2131-I_Valve_1', 'G2131-I_Valve_2', 'G2131-I_Valve_3' , 'G2131-I_Valve_4', 'G2131-I_Valve_5', 'G2131-I_Valve_6', 'G2131-I_Valve_7', 'G2131-I_Valve_8')
          valves_are_available = FALSE
          plot_labs = labs(x = "", y = "Valve Status (1 = open, 0 = closed)", title = paste0(input$swft_postgres_site, " - ", input$swft_postgres_data_type, " ", input$swft_postgres_sub_data_type_G2131), subtitle = paste0(input$swft_postgres_date_range[1], " to ", input$swft_postgres_date_range[2])) 
        }
      }
      #######              L2130 Picarro               ####### 
      if(input$swft_postgres_data_type == "L2130-I"){
        if(input$swft_postgres_sub_data_type_L2130 == "Isotope - 2H"){                                                                 
          streams_to_pull = c('L2130_2H_isotope')
          valves_are_available = TRUE
          plot_labs = labs(x = "", y = "per mil", title = paste0(input$swft_postgres_site, " - ", input$swft_postgres_data_type, " ", input$swft_postgres_sub_data_type_L2130), subtitle = paste0(input$swft_postgres_date_range[1], " to ", input$swft_postgres_date_range[2])) 
        }
        if(input$swft_postgres_sub_data_type_L2130 == "Isotope - 18O"){                                                                 
          streams_to_pull = c('L2130_18O_isotope')
          valves_are_available = TRUE
          plot_labs = labs(x = "", y = "per mil", title = paste0(input$swft_postgres_site, " - ", input$swft_postgres_data_type, " ", input$swft_postgres_sub_data_type_L2130), subtitle = paste0(input$swft_postgres_date_range[1], " to ", input$swft_postgres_date_range[2])) 
        }
        if(input$swft_postgres_sub_data_type_L2130 == "H2O"){                                                                 
          streams_to_pull = c('L2130_H2O')
          valves_are_available = TRUE
          plot_labs = labs(x = "", y = "Micromoles Per Mole", title = paste0(input$swft_postgres_site, " - ", input$swft_postgres_data_type, " ", input$swft_postgres_sub_data_type_L2130), subtitle = paste0(input$swft_postgres_date_range[1], " to ", input$swft_postgres_date_range[2])) 
        }
        if(input$swft_postgres_sub_data_type_L2130 == "Sample Valves"){                                                                 
          streams_to_pull = c('L2130-I_Valve_1', 'L2130-I_Valve_2', 'L2130-I_Valve_3' , 'L2130-I_Valve_4', 'L2130-I_Valve_5', 'L2130-I_Valve_6', 'L2130-I_Valve_7', 'L2130-I_Valve_8')
          valves_are_available = FALSE
          plot_labs = labs(x = "", y = "Valve Status (1 = open, 0 = closed)", title = paste0(input$swft_postgres_site, " - ", input$swft_postgres_data_type, " ", input$swft_postgres_sub_data_type_L2130), subtitle = paste0(input$swft_postgres_date_range[1], " to ", input$swft_postgres_date_range[2])) 
        }
      }
      
      #######              Li840 ECSE IRGA               ####### 
      if(input$swft_postgres_data_type == "Li-840"){
        if(input$swft_postgres_sub_data_type_Li840 == "CO2"){                                                                 
          streams_to_pull = c('Li840_CO2_fwMole')
          valves_are_available = TRUE
          plot_labs = labs(x = "", y = "Concentration (ppm)", title = paste0(input$swft_postgres_site, " - ", input$swft_postgres_data_type, " ", input$swft_postgres_sub_data_type_Li840), subtitle = paste0(input$swft_postgres_date_range[1], " to ", input$swft_postgres_date_range[2])) 
        }
        if(input$swft_postgres_sub_data_type_Li840 == "H2O"){                                                                 
          streams_to_pull = c('Li840_H2O_fwMole')
          valves_are_available = TRUE
          plot_labs = labs(x = "", y = "per mil", title = paste0(input$swft_postgres_site, " - ", input$swft_postgres_data_type, " ", input$swft_postgres_sub_data_type_Li840), subtitle = paste0(input$swft_postgres_date_range[1], " to ", input$swft_postgres_date_range[2])) 
        }
        if(input$swft_postgres_sub_data_type_Li840 == "Sample Valves"){      
          streams_to_pull = c('Li-840_Valve_1', 'Li-840_Valve_2', 'Li-840_Valve_3' , 'Li-840_Valve_4', 'Li-840_Valve_5', 'Li-840_Valve_6', 'Li-840_Valve_7', 'Li-840_Valve_8')
          valves_are_available = FALSE
          plot_labs = labs(x = "", y = "Micromoles Per Mole", title = paste0(input$swft_postgres_site, " - ", input$swft_postgres_data_type, " ", input$swft_postgres_sub_data_type_Li840), subtitle = paste0(input$swft_postgres_date_range[1], " to ", input$swft_postgres_date_range[2])) 
        }
        if(input$swft_postgres_sub_data_type_Li840 == "Flow Rate"){                                                                 
          streams_to_pull = c('ECSE_SampleMFC_FlowRate')
          valves_are_available = TRUE
          plot_labs = labs(x = "", y = "Valve Status (1 = open, 0 = closed)", title = paste0(input$swft_postgres_site, " - ", input$swft_postgres_data_type, " ", input$swft_postgres_sub_data_type_Li840), subtitle = paste0(input$swft_postgres_date_range[1], " to ", input$swft_postgres_date_range[2])) 
        }
      }
      #######              Li7200 ECTE IRGA               ####### 
      if(input$swft_postgres_data_type == "Li7200"){
        if(input$swft_postgres_sub_data_type_Li7200 == "CO2"){                                                                 
          streams_to_pull = c('Li7200_CO2')
          valves_are_available = FALSE
          plot_labs = labs(x = "", y = "Concentration (ppm)", title = paste0(input$swft_postgres_site, " - ", input$swft_postgres_data_type, " ", input$swft_postgres_sub_data_type_Li7200), subtitle = paste0(input$swft_postgres_date_range[1], " to ", input$swft_postgres_date_range[2])) 
        }
        if(input$swft_postgres_sub_data_type_Li7200 == "H2O"){                                                                 
          streams_to_pull = c('Li7200_fdMoleH2O')
          valves_are_available = FALSE
          plot_labs = labs(x = "", y = "per mil", title = paste0(input$swft_postgres_site, " - ", input$swft_postgres_data_type, " ", input$swft_postgres_sub_data_type_Li7200), subtitle = paste0(input$swft_postgres_date_range[1], " to ", input$swft_postgres_date_range[2])) 
        }
        if(input$swft_postgres_sub_data_type_Li7200 == "Flow"){      
          streams_to_pull = c('Li7200_MFCSampleFlow')
          valves_are_available = FALSE
          plot_labs = labs(x = "", y = "Flowrate (SLPM)", title = paste0(input$swft_postgres_site, " - ", input$swft_postgres_data_type, " ", input$swft_postgres_sub_data_type_Li7200), subtitle = paste0(input$swft_postgres_date_range[1], " to ", input$swft_postgres_date_range[2])) 
        }
        if(input$swft_postgres_sub_data_type_Li7200 == "Signal Strength"){                                                                 
          streams_to_pull = c('Li7200_CO2SglStr','Li7200_H2oSglStr')
          valves_are_available = FALSE
          plot_labs = labs(x = "", y = "Strength %", title = paste0(input$swft_postgres_site, " - ", input$swft_postgres_data_type, " ", input$swft_postgres_sub_data_type_Li7200), subtitle = paste0(input$swft_postgres_date_range[1], " to ", input$swft_postgres_date_range[2])) 
        }
        if(input$swft_postgres_sub_data_type_Li7200 == "Cell Temp"){                                                                 
          streams_to_pull = c('Li7200_cellTempIn', 'Li7200_cellTempOut')
          valves_are_available = FALSE
          plot_labs = labs(x = "", y = "Temperature C", title = paste0(input$swft_postgres_site, " - ", input$swft_postgres_data_type, " ", input$swft_postgres_sub_data_type_Li7200), subtitle = paste0(input$swft_postgres_date_range[1], " to ", input$swft_postgres_date_range[2])) 
        }
        if(input$swft_postgres_sub_data_type_Li7200 == "Pressure Differential"){                                                                 
          streams_to_pull = c('Li7200_pDiff')
          valves_are_available = FALSE
          plot_labs = labs(x = "", y = "Pressure (kPa)", title = paste0(input$swft_postgres_site, " - ", input$swft_postgres_data_type, " ", input$swft_postgres_sub_data_type_Li7200), subtitle = paste0(input$swft_postgres_date_range[1], " to ", input$swft_postgres_date_range[2])) 
        }
        if(input$swft_postgres_sub_data_type_Li7200 == "Diagnostic"){                                                                 
          streams_to_pull = c('Li7200_Diag')
          valves_are_available = FALSE
          plot_labs = labs(x = "", y = "Diagnostic", title = paste0(input$swft_postgres_site, " - ", input$swft_postgres_data_type, " ", input$swft_postgres_sub_data_type_Li7200), subtitle = paste0(input$swft_postgres_date_range[1], " to ", input$swft_postgres_date_range[2])) 
        }
      }
      
      #######              ALL CO2                 ####### 
      if(input$swft_postgres_data_type == "CO2"){
        streams_to_pull = c('Li7200_CO2', 'G2131_fwMoleCo2', 'Li840_CO2_fwMole')
        valves_are_available = FALSE
        plot_labs = labs(x = "", y = "CO2 (ppm)", title = paste0(input$swft_postgres_site, " - ", input$swft_postgres_data_type), subtitle = paste0(input$swft_postgres_date_range[1], " to ", input$swft_postgres_date_range[2]))
      }
      if(input$swft_postgres_data_type == "H2O"){
        streams_to_pull = c('Li7200_H2O', 'G2131_percentFwMoleH2O', 'Li840_H2O_fwMole', 'L2130_H2O')
        valves_are_available = FALSE
        plot_labs = labs(x = "", y = "H2O (ppm)", title = paste0(input$swft_postgres_site, " - ", input$swft_postgres_data_type), subtitle = paste0(input$swft_postgres_date_range[1], " to ", input$swft_postgres_date_range[2]))
      }
      
      
      if(input$swft_postgres_data_type == ""){
      }
      if(input$swft_postgres_data_type == ""){
      }
      if(input$swft_postgres_data_type == ""){
      }
      if(input$swft_postgres_data_type == ""){
      }
      if(input$swft_postgres_data_type == ""){
      }
      if(input$swft_postgres_data_type == ""){
      }
      
      
      ################################################################################################
      ################################################################################################
      ################################################################################################
      ########                                                                                ########
      ########                                                                                ########
      ########                                                                                ########
      ########                            Query the  data!                                    ########
      ########                                                                                ########
      ########                                                                                ########
      ########                                                                                ########
      ################################################################################################
      ################################################################################################
      ################################################################################################
      
      # Determine if aggregation is needed
      days_requested_for_plotting = as.numeric(difftime(input$swft_postgres_date_range[2], input$swft_postgres_date_range[1], units = "days"))
      
      aggregate_the_data = days_requested_for_plotting >= 62

      if(aggregate_the_data){
        
        # Transform streams into an SQL acceptable list
        idDpChar = paste0(streams_to_pull, collapse="', '") 
        
        # Write aggregation query
        so_query = glue::glue_sql(
          "SELECT 
            strm_name,
            avg(readout_val_double) as readout_val_double, 
            extract(year from twomin.readout_time) as year, 
            extract(month from twomin.readout_time) as month,
            extract(day from twomin.readout_time) as day, 
            extract(hour from twomin.readout_time) as hour
          FROM twomin
          WHERE(twomin.strm_name IN ({idDpChar}) AND siteid = {input$swft_postgres_site} AND twomin.readout_time  BETWEEN {input$swft_postgres_date_range[1]} AND {input$swft_postgres_date_range[2]})
          GROUP BY strm_name, year, month, day, hour
          ORDER BY strm_name, year, month, day, hour;", .con = db_con
        ) %>%
          gsub(pattern = "''", replacement = "'") %>%
          gsub(pattern = '""', replacement = '"')
        
      } else {
        
        # Grab valve data
        if(valves_are_available){
          streams_to_pull = c(
            streams_to_pull, 
            paste0(input$swft_postgres_data_type, '_Valve_1'),
            paste0(input$swft_postgres_data_type, '_Valve_2'),
            paste0(input$swft_postgres_data_type, '_Valve_3'),
            paste0(input$swft_postgres_data_type, '_Valve_4'),
            paste0(input$swft_postgres_data_type, '_Valve_5'),
            paste0(input$swft_postgres_data_type, '_Valve_6'),
            paste0(input$swft_postgres_data_type, '_Valve_7'),
            paste0(input$swft_postgres_data_type, '_Valve_8')
          )
        }
        
        # Transform streams into an SQL acceptable list
        idDpChar = paste0(streams_to_pull, collapse="', '") 
        
        # Write aggregation query
        so_query = glue::glue_sql(
          "SELECT * FROM twomin
          WHERE(twomin.strm_name IN ({idDpChar}) AND siteid = {input$swft_postgres_site} AND twomin.readout_time  BETWEEN {input$swft_postgres_date_range[1]} AND {input$swft_postgres_date_range[2]});", .con = db_con
        ) %>%
          gsub(pattern = "''", replacement = "'") %>%
          gsub(pattern = '""', replacement = '"')
        
      }

      # Start query timer
      start_time = Sys.time() 
      
      ################            QUERY            ################
      res = RPostgres::dbSendQuery(conn = db_con, statement = so_query)
      output = RPostgres::dbFetch(res)
      ################            QUERY            ################
      
      # Stop query timer
      end_time = Sys.time()
      postgres_query_time = difftime(end_time, start_time, units = "secs")
      message(paste0(Sys.time(), ": ", postgres_query_time))
      
      
      ################################################################################################
      ################################################################################################
      ################################################################################################
      ########                                                                                ########
      ########                                                                                ########
      ########                                                                                ########
      ########                            Clean the  data!                                    ########
      ########                                                                                ########
      ########                                                                                ########
      ########                                                                                ########
      ################################################################################################
      ################################################################################################
      ################################################################################################
      
      # Special case here for H2O values from L2130 are not in same units as other h2o streams
      if(input$swft_postgres_data_type == "H2O"){
        output = output %>%
          dplyr::mutate(strm_name = base::trimws(strm_name)) %>%
          dplyr::mutate(readout_val_double = ifelse(test = strm_name == "G2131_percentFwMoleH2O", yes = readout_val_double*10, no = readout_val_double)) %>%
          dplyr::mutate(readout_val_double = ifelse(test = strm_name == "L2130_H2O", yes = readout_val_double/1000, no = readout_val_double)) 
      }
      
      if(aggregate_the_data){ # Should we aggregate the data based upon how many days were requested
        
        output_cleaned = output %>%
          tidyr::unite(col = "date", c(year, month, day), sep = "-") %>% 
          dplyr::mutate(date = as.Date(date, origin = "1970-01-01")) %>%
          tidyr::unite(col = timestamp, c(date, hour), sep = " ") %>%
          dplyr::mutate(timestamp = lubridate::ymd_h(timestamp)) %>%
          dplyr::rename(Stream = strm_name, Value = readout_val_double)
        
      } else {
        
        if(valves_are_available){ # If valves are available, join them to the table (non-aggregation only)
          
          valve_data = output %>%
            dplyr::filter(stringr::str_detect(string = strm_name, pattern = "_Valve_") == TRUE) %>%
            tidyr::separate(col = strm_name, into = c("Sensor", "Delete", "SampleLevel"), sep = "_") %>% 
            dplyr::filter(readout_val_double == 1) %>%
            dplyr::select(readout_time, SampleLevel) %>%
            dplyr::mutate(SampleLevel = ifelse(test = is.na(SampleLevel) == TRUE, yes = "Validation", no = SampleLevel)) 
          
          non_valve_data = output %>%
            dplyr::filter(grepl(pattern = "Valve", x = strm_name) == FALSE)
          
          rm(output)
            
          output_cleaned = dplyr::left_join(x = non_valve_data, y = valve_data, by = "readout_time")  %>%
            dplyr::rename(timestamp = readout_time) %>%
            dplyr::mutate(SampleLevel = ifelse(test = is.na(SampleLevel) == TRUE, yes = "Validation", no = SampleLevel))  %>%
            dplyr::rename(Stream = strm_name, Value = readout_val_double)
          
        } else { # If valves are not available, just clean the data
          
          output_cleaned = output %>%
            dplyr::rename(timestamp = readout_time) %>%
            dplyr::rename(Stream = strm_name, Value = readout_val_double)
          
        }
        
      }
      
      
      ################################################################################################
      ################################################################################################
      ################################################################################################
      ########                                                                                ########
      ########                                                                                ########
      ########                                                                                ########
      ########                            Plot the the data!                                  ########
      ########                                                                                ########
      ########                                                                                ########
      ########                                                                                ########
      ################################################################################################
      ################################################################################################
      ################################################################################################
      
      if(nrow(output_cleaned) > 0){
        
        if(aggregate_the_data){ # Aggregated data plot
          plot_output = ggplot(output_cleaned, aes(x = timestamp, y = Value, color = Stream)) +
            geom_point() +
            # geom_line() +
            scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
            scale_x_datetime(breaks = scales::pretty_breaks(n = 10), date_labels = "%Y\n%m-%d") +
            plot_labs +
            ggdark::dark_theme_bw() + 
            theme(
              text = element_text(color = "white", face = "bold", size = 20), 
              legend.position = "top", 
              axis.text.x = element_text(color = "white", face = "bold", size = 20),
              axis.text.y = element_text(color = "white", face = "bold", size = 20)
            ) +
            guides(color = guide_legend(override.aes = list(size = 5, alpha = 1))) + 
            facet_wrap(~Stream)
        } else {
          if(valves_are_available){ # Non-aggregated plots with valves
            plot_output = ggplot(output_cleaned, aes(x = timestamp, y = Value, color = SampleLevel)) +
              geom_point() +
              # geom_line() +
              scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
              scale_x_datetime(breaks = scales::pretty_breaks(n = 10), date_labels = "%Y\n%m-%d") +
              plot_labs +
              ggdark::dark_theme_bw() + 
              theme(
                text = element_text(color = "white", face = "bold", size = 20), 
                legend.position = "top", 
                axis.text.x = element_text(color = "white", face = "bold", size = 20),
                axis.text.y = element_text(color = "white", face = "bold", size = 20)
              ) +
              guides(color = guide_legend(override.aes = list(size = 5, alpha = 1))) +
              facet_wrap(~Stream)
          } else { # Non-aggregated plots withOUT valves
            plot_output = ggplot(output_cleaned, aes(x = timestamp, y = Value, color = Stream)) +
              geom_point() +
              # geom_line() +
              scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
              scale_x_datetime(breaks = scales::pretty_breaks(n = 10), date_labels = "%Y\n%m-%d") +
              plot_labs +
              ggdark::dark_theme_bw() + 
              theme(
                text = element_text(color = "white", face = "bold", size = 20), 
                legend.position = "top", 
                axis.text.x = element_text(color = "white", face = "bold", size = 20),
                axis.text.y = element_text(color = "white", face = "bold", size = 20)
              ) +
              guides(color = guide_legend(override.aes = list(size = 5, alpha = 1))) + 
              facet_wrap(~Stream)
          }
        }
      } else { # If no data, produce a blank plot
        plot_output = ggplot()+
          geom_text(label = "text")+
          ggdark::dark_theme_bw() +
          scale_x_continuous(breaks = 0)+
          scale_y_continuous(breaks = 0)+
          labs(y = "", x = '') +
          theme(
            axis.text.x = element_text(color = "black", size = 0),
            axis.text.y = element_text(color = "black", size = 0)
          )+ 
          annotate("text", label = paste0("No data found for ", input$swft_postgres_site, "\nData not available"), x = 0, y = 0, color = "white", size = 12)
      }
  
      end_time = Sys.time()
      message(paste0(difftime(end_time, start_time, units = "secs")))
      
      # Output shiny object to Reactive
      plot_output
    })
    
    # Output shiny object to UI
    output$swft_postgres_plot = shiny::renderPlot({
      swft_postgres_plot()
    })
    
  }
})