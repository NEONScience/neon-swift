shiny::observeEvent(input$menu, {
  if(input$menu == "swft_hidden_tab"){

    
    swft_hidden_data = base::readRDS(file = "./data/user_log/user_log.RDS") %>% 
      dplyr::distinct() %>% 
      dplyr::mutate(day = cut(Start, breaks = "1 day"))
     
    output$swft_hidden_plot = plotly::renderPlotly({
      
      ggplot(swft_hidden_data, aes(x = day))+
        geom_bar()

    })
      
    
    
    

    
    
    
    
        
  }
})