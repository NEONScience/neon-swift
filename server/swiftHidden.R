shiny::observeEvent(input$menu, {
  if(input$menu == "swft_hidden_tab"){
    # S3 Read creds
    Sys.setenv(
      "AWS_ACCESS_KEY_ID"     = "research-eddy-inquiry",
      "AWS_S3_ENDPOINT"       = "neonscience.org",
      "AWS_DEFAULT_REGION"    = "s3.data"
    )
    
    # Read/clean data
    swft_hidden_data = aws.s3::s3readRDS(object = "neon-swift/user_log.RDS", bucket = "research-eddy-inquiry") %>% 
      dplyr::distinct() %>%
      dplyr::mutate(day = as.Date(cut(Start, breaks = "1 day"), origin = "1970-01-01")) %>%
      dplyr::filter(day > Sys.Date() - 14) %>% 
      dplyr::mutate(`Total Time` = round(as.numeric(`Total Time`),2))
    # First plot
    output$swft_hidden_plot_connections = plotly::renderPlotly({
      ggplot(swft_hidden_data, aes(x = day, fill = as.factor(day)))+
        geom_bar() +
        labs(y = "Connections to Swift", x = "") +
        ggdark::dark_theme_bw() +
        theme(legend.position = "none")
    })
    # Second plot
    output$swft_hidden_plot_durations = plotly::renderPlotly({
      ggplot(swft_hidden_data %>%  dplyr::filter(`Total Time` < 60*60), aes(x = `Total Time`/60, fill = as.factor(day))) +
        geom_histogram(binwidth = 1) +
        ggdark::dark_theme_bw() +
        labs(y = "Count", x = "Duration of Connection (m)", fill = "Date") 
    })
  }
})