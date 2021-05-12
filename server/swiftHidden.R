shiny::observeEvent(input$menu, {
  if(input$menu == "swft_hidden_tab"){
    # Read/clean data
    swft_hidden_data = base::readRDS(file = "./data/user_log/user_log.RDS") %>% 
      dplyr::distinct() %>%
      dplyr::mutate(day = as.Date(cut(Start, breaks = "1 day"), origin = "1970-01-01")) %>%
      dplyr::filter(day > Sys.Date() - 14) %>% 
      dplyr::mutate(`Total Time` = round(as.numeric(`Total Time`),2))
    # First plot
    output$swft_hidden_plot_connections = plotly::renderPlotly({
      ggplot(swft_hidden_data, aes(x = day, fill = day, color = day))+
        geom_bar() +
        labs(y = "Connections to Swift", x = "") +
        theme(legend.position = "none")
    })
    # Second plot
    output$swft_hidden_plot_durations = plotly::renderPlotly({
      ggplot(swft_hidden_data %>%  dplyr::filter(`Total Time` < 60*60), aes(x = `Total Time`/60, fill = day))+
        geom_histogram(binwidth = 1) +
        labs(y = "Count", x = "Duration of Connection (m)", fill = "Date") 
    })
  }
})