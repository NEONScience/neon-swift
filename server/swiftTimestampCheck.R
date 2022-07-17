# Server Code for Timestamp Checker
shiny::observeEvent(input$menu, {
  if(input$menu == "swft_timestamp_tab"){
    message(paste0(Sys.time(), ": User selected swft_timestamp_tab"))
    # Libraries
    library(dplyr)
    library(data.table)
    library(ggplot2)
    
    
    library(lubridate)
    library(plotly)
    
    # Aesthetics
    ggplot2::theme_set(ggdark::dark_theme_bw()) 
    
    # S3 Bucket
    timestamp_bucket = "neon-eddy-inquiry"
    
    # List files in the timestamp docker folder
    timestamp_files_lookup = eddycopipe::neon_gcs_list_objects(bucket = timestamp_bucket, prefix = "sensor_timestamp_check_docker/main", max = Inf) %>% 
      dplyr::mutate(date = base::as.Date(base::substr(Key, 36, 45), origin = "1970-01-01"))
    

    # Reactive datatable
    reactive_timestamp_data = shiny::reactive({

      # Filter to last x days
      timestamp_files_lookup_filtered = timestamp_files_lookup %>% 
        # dplyr::filter(date >= Sys.Date()-4)
        dplyr::filter(date >= input$swft_timestamp_date_range[1] & date <= input$swft_timestamp_date_range[2])
      
      timestamp_data = data.table::data.table()
      for(i in base::seq_along(timestamp_files_lookup_filtered$Key)){
        timestamp_data_in = eddycopipe::neon_gcs_get_rds(object = timestamp_files_lookup_filtered$Key[i], bucket = timestamp_bucket) 
        
        timestamp_data = data.table::rbindlist(l = base::list(timestamp_data, timestamp_data_in), fill = TRUE)
      }
      
      if(base::nrow(timestamp_data) > 0){

        # Read in and join the look up table, give each mac address the name of the sensor
        smart_sensor_lookup = base::readRDS("./data/lookup/smart_sensor_lookup.RDS")
        
        busted_thresholds = timestamp_data %>% 
          dplyr::filter(timestamp_drift > 10) %>% 
          dplyr::distinct(siteID, MacAddress)
        
        busted_sensors = timestamp_data %>% 
          dplyr::filter(MacAddress %in% busted_thresholds$MacAddress) %>% 
          dplyr::mutate(site_mac = base::paste0(siteID, " ", MacAddress))
        
        # If the last 4 hours are fine, don't alert?
        if(nrow(busted_sensors) > 0){
          check_issue_resolved = busted_sensors %>% 
            dplyr::mutate(cut_time = lubridate::ymd_hms(cut(TimeStamp, breaks = "4 hours"))) %>% 
            dplyr::group_by(site_mac, cut_time) %>% 
            dplyr::summarise(.groups = "drop",
              PullDate = PullDate[1],
              busted_threshold = base::ifelse(test = timestamp_drift >= 10, yes = TRUE, no = FALSE),
              percent_busted =  sum (busted_threshold) / length(busted_threshold) 
            ) %>% 
            dplyr::group_by(PullDate, site_mac) %>% 
            dplyr::summarise(.groups = "drop",
              percent_busted =  sum (busted_threshold) / length(busted_threshold) 
            ) %>%
            reshape2::dcast(PullDate ~ site_mac, value.var = "percent_busted")
                  
          timestamp_data_named = busted_sensors %>%
            dplyr::left_join(y = smart_sensor_lookup, by = "MacAddress") %>% 
            dplyr::filter(site_mac %in% names(check_issue_resolved)) %>% 
            dplyr::mutate(SiteID = siteID) %>%
            dplyr::mutate(`Raw Time Difference` = Actual_Time_Difference) %>% dplyr::select(-Actual_Time_Difference) %>% 
            dplyr::mutate(`Median Site Time Difference` = median_site_time_diff) %>% dplyr::select(-median_site_time_diff) %>% 
            dplyr::mutate(`Calculated Timestamp Drift` = timestamp_drift) %>% dplyr::select(-timestamp_drift) %>% 
            dplyr::mutate(SurveyTime = TimeStamp) %>% dplyr::select(-TimeStamp) %>% 
            dplyr::mutate(Sensor_UID = base::paste0(SiteID, " ", Sensor)) %>% 
            dplyr::select(PullDate, SurveyTime, SiteID, Sensor, Sensor_UID, MacAddress, `Raw Time Difference`, `Median Site Time Difference`, `Calculated Timestamp Drift`)
        } else {
          timestamp_data_named = data.table::data.table()
        }
      
      } else {
        timestamp_data_named = data.table::data.table()
      }
      
      timestamp_data_named
    })
    
    # Check how many rows of data were pulled
    output$swft_timestamp_last_update_box <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = lubridate::ymd_hms(base::max(timestamp_files_lookup$updated, na.rm = TRUE)),
        subtitle = "Last Updated (UTC)",
        width = 12,
        color = "black"
      )
    })
    
    swft_timestamp_plot = shiny::reactive({
      
      if(nrow(reactive_timestamp_data()) > 0) {
        
        # Calculate bounds for green/red boxes
        max_y = max(reactive_timestamp_data()$`Calculated Timestamp Drift`, na.rm = TRUE)
        min_x = min(reactive_timestamp_data()$SurveyTime, na.rm = TRUE)
        max_x = Sys.time()
        
        analysisPlot = ggplot2::ggplot(reactive_timestamp_data(), ggplot2::aes(x = SurveyTime, y = `Calculated Timestamp Drift`, color = Sensor_UID))+
          # ggplot2::annotate("rect", xmin = min_x, xmax = max_x, ymin = 0, ymax = 10, alpha = 0.2, fill = "#00cc00")+
          # ggplot2::annotate("rect", xmin = min_x, xmax = max_x, ymin = 10, ymax = max_y+1, alpha = 0.2, fill = "red")+
          ggplot2::geom_point(size = 2) +
          ggplot2::geom_line(linetype = "dashed")+
          ggplot2::geom_vline(xintercept = base::Sys.time(), show.legend = TRUE, color = "white", linetype = "dashed", size = 1.5) +
          # ggplot2::geom_text(ggplot2::aes(x= base::Sys.time() + 9000, label="Current\nTime", y = 20),  color = "white", angle = 0, size = 5) + 
          ggplot2::geom_hline(yintercept = 10, color = "red") +
          ggplot2::geom_hline(yintercept = -1, color = "black") +
          ggplot2::scale_y_continuous(sec.axis = ggplot2::dup_axis(name = "", breaks = 10))+
          ggplot2::scale_x_datetime(breaks = scales::pretty_breaks(n = 10), date_labels = "%m-%d\n%H:%M", limits = c(min_x, base::Sys.time() + 10000))+ 
          ggplot2::labs(x = "Survey Time\n(UTC)", y = "Timestamp Drift") +
          ggplot2::theme(text = ggplot2::element_text(size = 16, color = "white", face = "bold"))
      } else {
        analysisPlot <- ggplot2::ggplot()+
          ggplot2::geom_text(label = "text")+
          ggplot2::annotate("text", label = base::paste0("NO DATA: \n(No Timestamp Issues Identified)"), x = 0, y = 0, color = "white", size = 17) +
          ggplot2::labs(x = "", y = "")
          ggplot2::theme(text = ggplot2::element_text(size = 20))
      }
      analysisPlot
      
    })
    
    
    # Shiny Output of plot
    # output$swft_timestamp_plot <- shiny::renderPlot({
    #     swft_timestamp_plot()
    # })
    output$swft_timestamp_plot <- plotly::renderPlotly({
        swft_timestamp_plot()
    })
    
    
    
    # Table data from plot
    swft_reactive_timestamp_table = shiny::reactive({
     if(base::nrow(reactive_timestamp_data()) > 0){
        swft_timestamp_table = reactive_timestamp_data() %>%
          dplyr::arrange(dplyr::desc(SurveyTime)) 
      } else{
        swft_timestamp_table = data.table::data.table()
      }
      swft_timestamp_table
    })
    
    
    output$swft_timestamp_table <- DT::renderDT({
     
      # Return the cleaned up timestamp table
      DT::datatable(swft_reactive_timestamp_table()
                    ,  escape = FALSE,filter='top', options = base::list(pageLength = 10, autoWidth = FALSE))
    })
  }
})
