# Server Code for Timestamp Checker
shiny::observeEvent(input$menu, {
  if(input$menu == "swft_timestamp_tab"){
    library(fst)
    library(shiny)
    library(dplyr)
    library(ggplot2)
    library(aws.s3)
    library(aws.signature)
    library(lubridate)
    
    # S3 Bucket
    timestamp_bucket = "research-eddy-inquiry"
    
    # Aesthetics
    ggplot2::theme_set(ggdark::dark_theme_gray()) 
    
    Sys.setenv("AWS_ACCESS_KEY_ID"     = timestamp_bucket,
               "AWS_S3_ENDPOINT"       = "neonscience.org",
               "AWS_DEFAULT_REGION"    = "s3.data")
    
    
    # List files in the timestamp docker folder
    timestamp_files_lookup = aws.s3::get_bucket_df(bucket = timestamp_bucket, prefix = "sensor_timestamp_check_docker/") %>% 
      dplyr::mutate(date = as.Date(base::substr(Key, 31, 40), origin = "1970-01-01"))
    # Filter to last x days
    timestamp_files_lookup_filtered = timestamp_files_lookup %>% 
      dplyr::filter(date > Sys.Date()-7)
    
    timestamp_data = data.table::data.table()
    for(i in seq_along(timestamp_files_lookup_filtered$Key)){
      timestamp_data_in = aws.s3::s3readRDS(object = timestamp_files_lookup_filtered$Key[i], bucket = timestamp_bucket)
      
      timestamp_data = data.table::rbindlist(l = list(timestamp_data, timestamp_data_in), fill = TRUE)
    }
    
    if(nrow(timestamp_data) > 0){
      # Read in and join the look up table, give each mac address the name of the sensor
      smart_sensor_lookup = base::readRDS("./data/lookup/smart_sensor_lookup.RDS")
      timestamp_data_named = timestamp_data %>% 
        dplyr::filter(timestamp_drift > 10) %>% 
        dplyr::left_join(y = smart_sensor_lookup, by = "MacAddress") %>% 
        dplyr::mutate(SiteID = siteID) %>%
        dplyr::mutate(`Raw Time Difference` = Actual_Time_Difference) %>% dplyr::select(-Actual_Time_Difference) %>% 
        dplyr::mutate(`Median Site Time Difference` = median_site_time_diff) %>% dplyr::select(-median_site_time_diff) %>% 
        dplyr::mutate(`Calculated Timestamp Drift` = timestamp_drift) %>% dplyr::select(-timestamp_drift) %>% 
        dplyr::mutate(SurveyTime = TimeStamp) %>% dplyr::select(-TimeStamp) %>% 
        dplyr::select(PullDate, SurveyTime, SiteID, Sensor, MacAddress, `Raw Time Difference`, `Median Site Time Difference`, `Calculated Timestamp Drift`)
    } else {
      timestamp_data_named = data.table::data.table()
    }
    
    # Check how many rows of data were pulled
    output$swft_timestamp_last_update_box <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = lubridate::ymd_hms(max(timestamp_files_lookup$LastModified, na.rm = TRUE)),
        subtitle = "Last Updated (UTC)",
        width = 12,
        color = "black"
      )
    })
    
      
    # Check if there are any issues, if not, give a blank plot
    if(nrow(timestamp_data_named) > 0) {
      analysisPlot = ggplot(timestamp_data_named, aes(x = SurveyTime, y = `Calculated Timestamp Drift`, color = Sensor))+
        geom_point(size = 3) +
        geom_smooth(method = 'loess', formula = 'y ~ x') +
        geom_vline(xintercept = Sys.time(), show.legend = TRUE, color = "white", linetype = "dashed", size = 1.5) +
        geom_text(aes(x= Sys.time() + 1500, label="Current\nTime", y = 10),  color = "white", angle = 0, size = 5) + 
        geom_hline(yintercept = 0, color = "black") +
        geom_hline(yintercept = 10, color = "white") +
        scale_y_continuous(sec.axis = dup_axis(name = "", breaks = 10))+
        scale_x_datetime(date_breaks = "4 hours", date_labels = "%Y-%m-%d\n%H:%M")+ 
        labs(x = "Survey Time\n(UTC)", y = "Timestamp Drift", subtitle = "If your site is on here, but the there are no points close to the 'Current Time' line, the issue has been resolved") +
        facet_wrap(~SiteID, scales = "free") +
        theme(text = element_text(size = 16, color = "white", face = "bold"))
    } else {
      analysisPlot <- ggplot2::ggplot()+
        ggplot2::geom_text(label = "text")+
        ggplot2::annotate("text", label = paste0("NO DATA: \n(No Timestamp Issues Identified)"), x = 0, y = 0, color = "white")
    }
    
    # Shiny Output of plot
    # output$swft_timestamp_plot <- plotly::renderPlotly({
    #     analysisPlot
    # })
    output$swft_timestamp_plot <- shiny::renderPlot({
        analysisPlot
    })
    # Table data from plot
    output$swft_timestamp_table <- DT::renderDT({
      # Format the data table output
      if(nrow(timestamp_data_named)){
        timestampData.table = timestamp_data_named %>%
          dplyr::arrange(desc(`Calculated Timestamp Drift`)) 
      } else{
        timestampData.table = data.table::data.table()
      }
      # Return the cleaned up timestamp table
      DT::datatable(timestampData.table
                    ,  escape = FALSE,filter='top', options = list(pageLength = 10, autoWidth = FALSE))
    })
  }
})