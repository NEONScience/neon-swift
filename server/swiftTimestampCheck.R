# Server Code for LC Services Tab

# R script to spread out server code into more well defined chunks
library(shiny)
shiny::observeEvent(input$menu, {
  if(input$menu == "swft_timestamp_tab"){
    library(fst)
    library(shiny)
    library(dplyr)
    library(ggplot2)
    
    theme_set(theme_bw()) 
    
    Sys.setenv("AWS_ACCESS_KEY_ID"     = "research-eddy-inquiry",
               "AWS_S3_ENDPOINT"       = "neonscience.org",
               "AWS_DEFAULT_REGION"    = "s3.data")
    
    # Read in Timestamp data
    swft_timestamp_date_list = seq.Date(from = Sys.Date()-14, to = Sys.Date(), by = 1)
    
    timestampData = data.table::data.table()
    for(days in swft_timestamp_date_list){
      days = as.Date(days, origin = "1970-01-01")
      message(days)


      if(aws.s3::object_exists(object = paste0("sensor_timestamp_check/", days, ".RDS"), bucket = "research-eddy-inquiry") == TRUE){
        message("\t Exists!!!")
        
        swft_timestamp_data.in = aws.s3::s3readRDS(object = paste0("sensor_timestamp_check/", days, ".RDS"), bucket = "research-eddy-inquiry")

        # swft_timestamp_data.in = aws.s3::s3readRDS(object = paste0("sensor_timestamp_check/", days, ".RDS"), bucket = "research-eddy-inquiry")
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
        color = "navy"
      )
    })
    
    # Give table `readable` names
    # names(timestampData) <- c(
    #   "TimeStamp","MacAddress","Eeprom","StreamNumber","Data","StartTime","EndTime","PullDate","Sensor's Timestamp Difference2","Sensor's Timestamp Difference",
    #   "Site's Median Timestamp Difference", "Sensor's Deviation from the Site's Median Timestamp"
    # )
    
    # Filter the data down a little and select only certain columns sensible to users
    timestampData.plot <- timestampData %>%
      dplyr::filter(`Sensor's Deviation from the Site's Median Timestamp` > 10 &
                    `Sensor's Deviation from the Site's Median Timestamp` < 1000  ) %>%
      dplyr::select(siteID, MacAddress, Eeprom, PullDate, `Site's Median Timestamp Difference`, `Sensor's Deviation from the Site's Median Timestamp`)
    
      
    # Check if there are any issues, if not, give a blank plot
    if(nrow(timestampData.plot) > 0) {
      analysisPlot <- ggplot2::ggplot(data=timestampData.plot,aes(x=`Sensor's Deviation from the Site's Median Timestamp`,SensorMac=MacAddress,fill=siteID, SiteMedianDifferenceTime = `Site's Median Timestamp Difference`))+
        ggplot2::geom_histogram(binwidth = 1, color = "grey")+
        ggplot2::theme(axis.text.x = element_text(angle = 325))+ # Change axis text to Battelle Blue
        ggplot2::labs(x = "Sensor Delay (s)", 
                      y = "Count of Streams in Bin",
                      "Number of Streams", 
                      title = paste0("Time Difference Histogram Analysis"))+
        ggplot2::facet_grid(~PullDate)
    } else {
      analysisPlot <- ggplot()+
        geom_text(label = "text")+
        annotate("text", label = paste0("NO DATA: \n(No Timestamp Issues Identified)"), x = 0, y = 0, color = "black")+
        theme_minimal()
    }
    
    # Shiny Output of plot
    output$swft_timestamp_plot <- renderPlot({
      analysisPlot
    })
    
    # Table data from plot
    output$swft_timestamp_table <- renderDT({
      
      timestampData.table = timestampData %>%
        dplyr::group_by(PullDate, siteID) %>%
        dplyr::filter(`Sensor's Deviation from the Site's Median Timestamp` > 2) %>%
        dplyr::filter(`Sensor's Deviation from the Site's Median Timestamp` == max(`Sensor's Deviation from the Site's Median Timestamp`)) %>%
        dplyr::arrange(desc(`Sensor's Deviation from the Site's Median Timestamp`)) %>%
        dplyr::select(siteID, PullDate, MacAddress, StartTime, EndTime, diffTime, `Site's Median Timestamp Difference`, `Sensor's Deviation from the Site's Median Timestamp`)
      
      DT::datatable(timestampData.table
                    ,  escape = FALSE,filter='top', options = list(pageLength = 10, autoWidth = FALSE))
    })
    
    
    # This is the end.
    }
})