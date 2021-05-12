shiny::observeEvent(input$menu, {
  if(input$menu == "swft_hidden_tab"){

    
    swft_hidden_data = base::readRDS(file = "./data/user_log/user_log.RDS") %>% 
      dplyr::distinct() %>% 
      dplyr::mutate(day = cut(Start, breaks = "1 day")) %>% 
      dplyr::mutate(`Total Time` = round(as.numeric(`Total Time`),2))
     
    output$swft_hidden_plot_connections = plotly::renderPlotly({
      
      ggplot(swft_hidden_data, aes(x = day, fill = day))+
        geom_bar() +
        labs(y = "Connections to Swift", x = "") +
        theme(legend.position = "none")


    })
    
    
    
    output$swft_hidden_plot_durations = plotly::renderPlotly({
        
      ggplot(swft_hidden_data, aes(x = `Total Time`/60, fill = day))+
        geom_histogram(binwidth = 1) +
        labs(y = "Count", x = "Duration of Connection (m)", fill = "Date") 

    })

    
    

    
    
    
    
        
  }
})