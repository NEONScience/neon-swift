# Server Code for Timestamp Checker
shiny::observeEvent(input$menu, {
  if(input$menu == "swft_timestamp_tab"){
    library(fst)
    library(shiny)
    library(dplyr)
    library(ggplot2)
    
    # Aesthetics
    ggplot2::theme_set(ggdark::dark_theme_gray()) 
    
    Sys.setenv("AWS_ACCESS_KEY_ID"     = "research-eddy-inquiry",
               "AWS_S3_ENDPOINT"       = "neonscience.org",
               "AWS_DEFAULT_REGION"    = "s3.data")
    
    # Read in Timestamp data
    swft_timestamp_date_list = seq.Date(from = Sys.Date()-14, to = Sys.Date(), by = 1)
    
    timestampData = data.table::data.table()
    for(days in swft_timestamp_date_list){
      days = as.Date(days, origin = "1970-01-01")
      if(aws.s3::object_exists(object = paste0("sensor_timestamp_check/", days, ".RDS"), bucket = "research-eddy-inquiry") == TRUE){
        swft_timestamp_data.in = aws.s3::s3readRDS(object = paste0("sensor_timestamp_check/", days, ".RDS"), bucket = "research-eddy-inquiry")
        timestampData = data.table::rbindlist(l = list(timestampData, swft_timestamp_data.in))

      }
    }
    
    timestampData = timestampData %>%
      dplyr::group_by(siteID, PullDate) %>%
      dplyr::mutate(`Site's Median Timestamp Difference` = median(diffTime)) %>%
      dplyr::mutate(`Sensor's Deviation from the Site's Median Timestamp` = abs(`Site's Median Timestamp Difference`-`diffTime` )) 
    
    swft_timestamp_last_update = max(timestampData$PullDate, na.rm = TRUE)
    
    # Check how many rows of data were pulled
    output$swft_timestamp_last_update_box <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = paste0("Last Updated: ", swft_timestamp_last_update),
        subtitle = "",
        width = 12,
        color = "black"
      )
    })
    
    # Give table `readable` names
    # Filter the data down a little and select only certain columns sensible to users
    timestampData.plot <- timestampData %>%
      dplyr::filter(`Sensor's Deviation from the Site's Median Timestamp` > 10 &
                    `Sensor's Deviation from the Site's Median Timestamp` < 1000  ) %>%
      dplyr::mutate(`Sensor's Deviation from the Site's Median Timestamp` = as.numeric(`Sensor's Deviation from the Site's Median Timestamp`)) %>%
      dplyr::mutate(`Site's Median Timestamp Difference` = as.numeric(`Site's Median Timestamp Difference`)) %>%
      dplyr::select(siteID, MacAddress, Eeprom, PullDate, `Site's Median Timestamp Difference`, `Sensor's Deviation from the Site's Median Timestamp`)
    
      
    # Check if there are any issues, if not, give a blank plot
    if(nrow(timestampData.plot) > 0) {
      
      
      swft_timestamp_plot = timestampData %>%
        dplyr::group_by(siteID, PullDate) %>%
        dplyr::mutate(`Site's Median Timestamp Difference` = median(diffTime)) %>%
        dplyr::mutate(`Sensor's Deviation from the Site's Median Timestamp` = -as.numeric(`Site's Median Timestamp Difference`-`diffTime` )) %>%
        dplyr::filter(`Sensor's Deviation from the Site's Median Timestamp` >= 10 | `Sensor's Deviation from the Site's Median Timestamp` <= -10) %>% 
        dplyr::mutate(`Sensor's Deviation from the Site's Median Timestamp` = round(as.numeric(`Sensor's Deviation from the Site's Median Timestamp`),0)) %>%
        dplyr::mutate(`Site's Median Timestamp Difference` = as.numeric(`Site's Median Timestamp Difference`)) %>%
        dplyr::select(siteID, MacAddress, Eeprom, PullDate, `Site's Median Timestamp Difference`, `Sensor's Deviation from the Site's Median Timestamp`)%>%
        dplyr::arrange(desc(`Sensor's Deviation from the Site's Median Timestamp`)) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(SensorID = paste0(siteID, " ", PullDate, "\n", MacAddress)) %>%
        dplyr::mutate(SensorID_2 = paste0(siteID, "\n", MacAddress))
      
      swft_timestamp_plot$SensorID = factor(swft_timestamp_plot$SensorID, levels = swft_timestamp_plot$SensorID)
      
      plot.y.max = swft_timestamp_plot$SensorID[1]
      plot.y.min = swft_timestamp_plot$SensorID[length(unique(swft_timestamp_plot$SensorID))]
      
      analysisPlot = ggplot(swft_timestamp_plot, aes(y = `Sensor's Deviation from the Site's Median Timestamp`, x = SensorID_2, fill = SensorID_2)) +
        geom_col(position = "identity", size = 1) + 
        geom_text(aes(x=, y=`Sensor's Deviation from the Site's Median Timestamp`, ymax=`Sensor's Deviation from the Site's Median Timestamp`, label=`Sensor's Deviation from the Site's Median Timestamp`, 
                      hjust=ifelse(sign(`Sensor's Deviation from the Site's Median Timestamp`)>0, 1, 1)), size = 6.5,
                  position = position_dodge(width=1)) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
        geom_hline(yintercept = 10, size = 1, linetype = "dashed") +
        geom_hline(yintercept = 0, size = 1, linetype = "solid") +
        geom_hline(yintercept = -10, size = 1, linetype = "dashed") +
        # annotate("rect", xmin = -10, xmax = 10, ymin = .8, ymax = 1.2, alpha = 0.62, fill = "#00cc00")+
        labs(y = "Time Delay from Actual UTC", x = "SiteID and MacAddress")+
        theme(legend.position = "none", text = element_text(size = 15)) +
        facet_wrap(~PullDate, scales = "free_y") + 
        coord_flip()
      
      
      # analysisPlot <- ggplot2::ggplot(data=timestampData.plot,aes(x=`Sensor's Deviation from the Site's Median Timestamp`,SensorMac=MacAddress,fill=siteID, SiteMedianDifferenceTime = `Site's Median Timestamp Difference`))+
      #   ggplot2::geom_histogram(binwidth = 5, color = "grey")+
      #   ggplot2::theme(axis.text.x = element_text(angle = 325))+ # Change axis text to Battelle Blue
      #   ggplot2::labs(x = "Sensor Delay (s)", 
      #                 y = "Count of Streams in Bin",
      #                 "Number of Streams", 
      #                 title = paste0("Time Difference Histogram Analysis"))+
      #   ggplot2::facet_grid(~PullDate)
    } else {
      analysisPlot <- ggplot2::ggplot()+
        ggplot2::geom_text(label = "text")+
        ggplot2::annotate("text", label = paste0("NO DATA: \n(No Timestamp Issues Identified)"), x = 0, y = 0, color = "white")
    }
    
    # Shiny Output of plot
    output$swft_timestamp_plot <- shiny::renderPlot({
        analysisPlot
    })
    
    # Table data from plot
    output$swft_timestamp_table <- DT::renderDT({
      
      timestampData.table = timestampData %>%
        dplyr::group_by(PullDate, siteID) %>%
        dplyr::filter(`Sensor's Deviation from the Site's Median Timestamp` > 10) %>%
        dplyr::filter(`Sensor's Deviation from the Site's Median Timestamp` == max(`Sensor's Deviation from the Site's Median Timestamp`)) %>%
        dplyr::arrange(desc(`Sensor's Deviation from the Site's Median Timestamp`)) %>%
        dplyr::select(siteID, PullDate, MacAddress, StartTime, EndTime, diffTime, `Site's Median Timestamp Difference`, `Sensor's Deviation from the Site's Median Timestamp`)
      
      DT::datatable(timestampData.table
                    ,  escape = FALSE,filter='top', options = list(pageLength = 10, autoWidth = FALSE))
    })
  }
})