# swiftQfQmMacro

swft_ei_bucket = "research-eddy-inquiry"

qfqm_macro_avail_data = shiny::reactive({
  shiny::req(input$swft_qfqm_macro_site_select)
  data_in = aws.s3::get_bucket_df(bucket = swft_ei_bucket, prefix = paste0("qfqm_flux_shiny/v20210223/", input$swft_qfqm_macro_site_select, "/"))
  
  if(nrow(data_in) > 1){
    
    data_in %>%    
      tidyr::separate(col = Key, remove = FALSE, into = c("folder", "version", "site", "file"), sep =  "/") %>% 
      tidyr::separate(col = file, sep = "_", into = c("year", "type", "stream", "id")) %>% 
      dplyr::filter(type == "plot")
    
  } else {
    data.table::data.table()
  }
  
  ### Testing Code
  # aws.s3::get_bucket_df(bucket = swft_ei_bucket, prefix = paste0("qfqm_flux_shiny/v20210223/YELL/"), max = Inf) %>% 
  #   tidyr::separate(col = Key, remove = FALSE, into = c("folder", "version", "site", "file"), sep =  "/") %>% 
  #   tidyr::separate(col = file, sep = "_", into = c("year", "type", "stream", "id")) %>% 
  #   dplyr::filter(type == "plot")
  ### Testing Code
})

output$swft_qfqm_macro_year_select = shiny::renderUI({
  shiny::req(qfqm_macro_avail_data())
  if(nrow(qfqm_macro_avail_data()) > 1){
    shiny::selectInput('swft_qfqm_macro_year_select', 'Year', choices = base::sort(unique(qfqm_macro_avail_data()$year),decreasing = TRUE))
  } else {
    shiny::textInput('swft_qfqm_macro_year_select', 'Year', placeholder = "No data found in the database...")
  }
})

# Commented out because we really don't need this to be reactive, static choices implemented in UI, additionally when you'd select a new site it would render the previous selection then buffer and load the top selection
# output$swft_qfqm_macro_terms = shiny::renderUI({
#   shiny::req(qfqm_macro_avail_data())
#   if(nrow(qfqm_macro_avail_data()) > 1){
#     shiny::selectInput('swft_qfqm_macro_terms', 'Terms', choices = base::sort(unique(qfqm_macro_avail_data()$stream),decreasing = TRUE))
#   } else {
#     shiny::textInput('swft_qfqm_macro_terms', 'Terms', placeholder = "No data found in the database...")
#   }
# })

swft_qfqm_macro_plot = shiny::reactive({
  
  shiny::req(input$swft_qfqm_macro_site_select, input$swft_qfqm_macro_year_select, input$swft_qfqm_macro_terms)
  
  aws.s3::s3readRDS(
    object = paste0("qfqm_flux_shiny/v20210223/", input$swft_qfqm_macro_site_select, "/", input$swft_qfqm_macro_year_select, "_plot_", input$swft_qfqm_macro_terms, "_dqmp.RDS"),
    bucket = swft_ei_bucket
  ) #+
    # ggplot2::theme(
    #   text        = ggplot2::element_text(size = 20),
    #   # axis.text.y = ggplot2::element_text(size = 16),
    #   axis.text.x = ggplot2::element_text(size = 12),
    #   plot.title  = ggplot2::element_text(size = 28)
    # ) 
})

# output$swft_qfqm_macro_plot = shiny::renderPlot({
#   swft_qfqm_macro_plot()
# })

output$swft_qfqm_macro_plot = plotly::renderPlotly({
  plotly::ggplotly(
    swft_qfqm_macro_plot()
  )
})
