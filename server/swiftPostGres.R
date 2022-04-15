shiny::observeEvent(input$menu, {
  if(input$menu == "swft_postgres_tab"){
    
    swft_postgres_plot = shiny::eventReactive(input$swft_postgres_actionButton,{
      db_con = eddycopipe::build_postgres_connection()
      
      
      if(input$swft_postgres_data_type == "ecse.mfm"){
        streams_to_pull = c('ML1_MFM_FlowRate', 'ML2_MFM_FlowRate', 'ML3_MFM_FlowRate', 'ML4_MFM_FlowRate','ML5_MFM_FlowRate','ML6_MFM_FlowRate','ML7_MFM_FlowRate','ML8_MFM_FlowRate')
      }
      if(input$swft_postgres_data_type == "G2131"){

        if(input$swft_postgres_sub_data_type_G2131 == "CO2"){                                                                 
          streams_to_pull = c('G2131_fwMoleCo2')
          plot_labs = labs(x = "", y = "Concentration (ppm)", title = paste0(input$swft_postgres_site, " - ", input$swft_postgres_data_type, " ", input$swft_postgres_sub_data_type_G2131), subtitle = paste0(input$swft_postgres_date_range[1], " to ", input$swft_postgres_date_range[2])) 
        }
        if(input$swft_postgres_sub_data_type_G2131 == "H2O"){                                                                 
          streams_to_pull = c('G2131_percentFwMoleH2O')
          plot_labs = labs(x = "", y = "", title = paste0(input$swft_postgres_site, " - ", input$swft_postgres_data_type, " ", input$swft_postgres_sub_data_type_G2131), subtitle = paste0(input$swft_postgres_date_range[1], " to ", input$swft_postgres_date_range[2])) 
        }
        if(input$swft_postgres_sub_data_type_G2131 == "Isotopes"){                                                                 
          streams_to_pull = c('G2131_13C_isotope')
          plot_labs = labs(x = "", y = "Concentration", title = paste0(input$swft_postgres_site, " - ", input$swft_postgres_data_type, " ", input$swft_postgres_sub_data_type_G2131), subtitle = paste0(input$swft_postgres_date_range[1], " to ", input$swft_postgres_date_range[2])) 
        }
        if(input$swft_postgres_sub_data_type_G2131 == "Sample Valves"){                                                                 
          streams_to_pull = c('G2131-I_Valve_1', 'G2131-I_Valve_2', 'G2131-I_Valve_3' , 'G2131-I_Valve_4', 'G2131-I_Valve_5', 'G2131-I_Valve_6', 'G2131-I_Valve_7', 'G2131-I_Valve_8')
          plot_labs = labs(x = "", y = "Valve Status (1 = open, 0 = closed)", title = paste0(input$swft_postgres_site, " - ", input$swft_postgres_data_type, " ", input$swft_postgres_sub_data_type_G2131), subtitle = paste0(input$swft_postgres_date_range[1], " to ", input$swft_postgres_date_range[2])) 
        }

      }
      if(input$swft_postgres_data_type == ""){
      }
      if(input$swft_postgres_data_type == ""){
      }
      if(input$swft_postgres_data_type == ""){
      }
      if(input$swft_postgres_data_type == ""){
      }
      
      
      # Transform streams into an SQL acceptable list
      idDpChar = paste0(streams_to_pull, collapse="', '") 
      
      # Write query
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
      
      start_time = Sys.time()
      ################            QUERY            ################
      res = RPostgres::dbSendQuery(conn = db_con, statement = so_query)
      output = RPostgres::dbFetch(res)
      ################            QUERY            ################
      end_time = Sys.time()
      message(paste0(difftime(end_time, start_time, units = "secs")))
      
      # Quickly clean the data
      output_cleaned = output %>%
        tidyr::unite(col = "date", c(year, month, day), sep = "-") %>% 
        dplyr::mutate(date = as.Date(date, origin = "1970-01-01")) %>%
        tidyr::unite(col = timestamp, c(date, hour), sep = " ") %>%
        dplyr::mutate(timestamp = lubridate::ymd_h(timestamp))
      
      plot_output = ggplot(output_cleaned, aes(x = timestamp, y = readout_val_double, color = strm_name)) +
        geom_point() +
        geom_line() +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 6), sec.axis = dup_axis(name = "")) +
        scale_x_datetime(breaks = scales::pretty_breaks(n = 10), date_labels = "%Y-%m-%d") +
        plot_labs +
        theme(axis.text.x = element_text(angle = 270), text = element_text(color = "white", face = "bold", size = 20)) + # Option to angle the facet grid y panel text
        facet_wrap(~strm_name)

  
      end_time = Sys.time()
      message(paste0(difftime(end_time, start_time, units = "secs")))
      
      plot_output
    })
    
    output$swft_postgres_plot = shiny::renderPlot({
      swft_postgres_plot()
    })
    
  }
})