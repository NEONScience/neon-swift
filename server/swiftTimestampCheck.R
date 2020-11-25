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
    
    # Read in Timestamp data
    timestampData <- readRDS(paste0(swft.server.folder.path, "data/swiftTimeCheck.rds"))
    
    
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
    names(timestampData) <- c(
      "TimeStamp","MacAddress","Eeprom","StreamNumber","Data","StartTime","EndTime","PullDate","Sensor's Timestamp Difference2","siteID","Sensor's Timestamp Difference",
      "Site's Median Timestamp Difference", "Sensor's Deviation from the Site's Median Timestamp"
    )
    
    # Filter the data down a little and select only certain columns sensible to users
    timestampData <- timestampData %>%
      dplyr::filter(`Sensor's Deviation from the Site's Median Timestamp` > 10 &
                    `Sensor's Deviation from the Site's Median Timestamp` < 1000  ) %>%
      dplyr::filter(PullDate > Sys.Date()-21) %>%
      dplyr::filter(stringr::str_detect(string = MacAddress, pattern = "7c:e0") == FALSE) %>%
      dplyr::select(siteID, MacAddress, Eeprom, PullDate, `Sensor's Timestamp Difference`, `Site's Median Timestamp Difference`, `Sensor's Deviation from the Site's Median Timestamp`)
    
      
    # Check if there are any issues, if not, give a blank plot
    if(nrow(timestampData) > 0) {
      analysisPlot <- ggplot2::ggplot(data=timestampData,aes(x=`Sensor's Deviation from the Site's Median Timestamp`,SensorMac=MacAddress,fill=siteID, SiteMedianDifferenceTime = `Site's Median Timestamp Difference`))+
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
      DT::datatable(timestampData
                    ,  escape = FALSE,filter='top', options = list(pageLength = 10, autoWidth = FALSE))
    })
    
    
    # This is the end.
    }
})