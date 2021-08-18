# Server Code for Timestamp Checker
shiny::observeEvent(input$menu, {
  if(input$menu == "swft_timestamp_tab"){
    # Libraries
    library(dplyr)
    library(data.table)
    library(ggplot2)
    library(aws.s3)
    library(aws.signature)
    library(lubridate)
    library(plotly)
    
    # Aesthetics
    ggplot2::theme_set(ggdark::dark_theme_bw()) 
    
    # S3 Bucket
    timestamp_bucket = "research-eddy-inquiry"
    
    # S3 Environment
    base::Sys.setenv(
      "AWS_ACCESS_KEY_ID"     = timestamp_bucket,
      "AWS_S3_ENDPOINT"       = "neonscience.org",
      "AWS_DEFAULT_REGION"    = "s3.data"
    )
    
    # List files in the timestamp docker folder
    timestamp_files_lookup = aws.s3::get_bucket_df(bucket = timestamp_bucket, prefix = "sensor_timestamp_check_docker/") %>% 
      dplyr::mutate(date = base::as.Date(base::substr(Key, 31, 40), origin = "1970-01-01"))
    # Filter to last x days
    timestamp_files_lookup_filtered = timestamp_files_lookup %>% 
      dplyr::filter(date > base::Sys.Date()-7)
    
    timestamp_data = data.table::data.table()
    for(i in base::seq_along(timestamp_files_lookup_filtered$Key)){
      timestamp_data_in = aws.s3::s3readRDS(object = timestamp_files_lookup_filtered$Key[i], bucket = timestamp_bucket)
      
      timestamp_data = data.table::rbindlist(l = base::list(timestamp_data, timestamp_data_in), fill = TRUE)
    }
    
    if(base::nrow(timestamp_data) > 0){
      # Read in and join the look up table, give each mac address the name of the sensor
      smart_sensor_lookup = base::readRDS("./data/lookup/smart_sensor_lookup.RDS")
      
      busted_thresholds = timestamp_data %>% 
        dplyr::filter(timestamp_drift > 10) 
      
      busted_sensors = timestamp_data %>% 
        dplyr::filter(MacAddress %in% busted_thresholds$MacAddress)
      
      # If the last 4 hours are fine, don't alert?
      check_issue_resolved = busted_sensors%>% 
        dplyr::mutate(site_mac = base::paste0(siteID, " ", MacAddress)) %>%
        dplyr::mutate(cut_time = cut(TimeStamp, breaks = "4 hours")) %>% 
        dplyr::group_by(site_mac, cut_time) %>% 
        dplyr::summarise(.groups = "keep",
          busted_threshold = base::ifelse(test = timestamp_drift >= 10, yes = TRUE, no = FALSE)
        ) 
      
      timestamp_data_named = busted_sensors %>% 
        dplyr::left_join(y = smart_sensor_lookup, by = "MacAddress") %>% 
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
    
    # Check how many rows of data were pulled
    output$swft_timestamp_last_update_box <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = lubridate::ymd_hms(base::max(timestamp_files_lookup$LastModified, na.rm = TRUE)),
        subtitle = "Last Updated (UTC)",
        width = 12,
        color = "black"
      )
    })
      
    # Check if there are any issues, if not, give a blank plot
    if(nrow(timestamp_data_named) > 0) {
      
      # Calculate bounds for green/red boxes
      max_y = max(timestamp_data_named$`Calculated Timestamp Drift`, na.rm = TRUE)
      min_x = min(timestamp_data_named$SurveyTime, na.rm = TRUE)
      max_x = Sys.time()
      
      analysisPlot = ggplot2::ggplot(timestamp_data_named, ggplot2::aes(x = SurveyTime, y = `Calculated Timestamp Drift`, color = Sensor_UID))+
        ggplot2::annotate("rect", xmin = min_x, xmax = max_x, ymin = 0, ymax = 10, alpha = 0.2, fill = "#00cc00")+
        ggplot2::annotate("rect", xmin = min_x, xmax = max_x, ymin = 10, ymax = max_y+1, alpha = 0.2, fill = "red")+
        ggplot2::geom_point(size = 4) +
        ggplot2::geom_line(linetype = "dashed")+
        ggplot2::geom_vline(xintercept = base::Sys.time(), show.legend = TRUE, color = "white", linetype = "dashed", size = 1.5) +
        ggplot2::geom_text(ggplot2::aes(x= base::Sys.time() + 5000, label="Current\nTime", y = 8),  color = "white", angle = 0, size = 5) + 
        ggplot2::geom_hline(yintercept = -1, color = "black") +
        ggplot2::geom_hline(yintercept = 10, color = "white") +
        ggplot2::scale_y_continuous(sec.axis = ggplot2::dup_axis(name = "", breaks = 10))+
        ggplot2::scale_x_datetime(date_breaks = "4 hours", date_labels = "%Y-%m-%d\n%H:%M")+ 
        ggplot2::labs(x = "Survey Time\n(UTC)", y = "Timestamp Drift") +
        ggplot2::theme(text = ggplot2::element_text(size = 16, color = "white", face = "bold"))
    } else {
      analysisPlot <- ggplot2::ggplot()+
        ggplot2::geom_text(label = "text")+
        ggplot2::annotate("text", label = base::paste0("NO DATA: \n(No Timestamp Issues Identified)"), x = 0, y = 0, color = "white")
    }
    
    # Shiny Output of plot
    output$swft_timestamp_plot <- shiny::renderPlot({
        analysisPlot
    })
    # output$swft_timestamp_plot <- plotly::renderPlotly({
    #     analysisPlot
    # })
    # Table data from plot
    output$swft_timestamp_table <- DT::renderDT({
      # Format the data table output
      if(base::nrow(timestamp_data_named)){
        timestampData.table = timestamp_data_named %>%
          dplyr::arrange(dplyr::desc(SurveyTime)) 
      } else{
        timestampData.table = data.table::data.table()
      }
      # Return the cleaned up timestamp table
      DT::datatable(timestampData.table
                    ,  escape = FALSE,filter='top', options = base::list(pageLength = 10, autoWidth = FALSE))
    })
  }
})