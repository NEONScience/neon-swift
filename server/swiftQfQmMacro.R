# swiftQfQmMacro

swft_ei_bucket = "neon-eddy-inquiry"
## Commented out because we don't really need reactivate here either
# qfqm_macro_avail_data = shiny::reactive({
#   shiny::req(input$swft_qfqm_macro_site_select)
#   system.time(
#     data_in <- eddycopipe::neon_gcs_list_objects(bucket = swft_ei_bucket, prefix = paste0("qfqm_flux_shiny/v", input$swft_qfqm_macro_code_version_select,"/", input$swft_qfqm_macro_site_select, "/"), max = Inf)
#   )
#   
#   browser()
#   if(nrow(data_in) > 1){
#     
#     data_in %>%    
#       tidyr::separate(col = Key, remove = FALSE, into = c("folder", "version", "site", "file"), sep =  "/") %>% 
#       tidyr::separate(col = file, sep = "_", into = c("year", "type", "stream", "id")) %>% 
#       dplyr::filter(type == "plot")
#     
#   } else {
#     data.table::data.table()
#   }
#   
#   ### Testing Code
#   # eddycopipe::neon_gcs_list_objects(bucket = swft_ei_bucket, prefix = paste0("qfqm_flux_shiny/v20210223/YELL/"), max = Inf) %>% 
#   #   tidyr::separate(col = Key, remove = FALSE, into = c("folder", "version", "site", "file"), sep =  "/") %>% 
#   #   tidyr::separate(col = file, sep = "_", into = c("year", "type", "stream", "id")) %>% 
#   #   dplyr::filter(type == "plot")
#   ### Testing Code
# })


## Commented out because we don't really need reactivate here either
# output$swft_qfqm_macro_year_select = shiny::renderUI({
#   shiny::req(qfqm_macro_avail_data())
#   if(nrow(qfqm_macro_avail_data()) > 1){
#     shiny::selectInput('swft_qfqm_macro_year_select', 'Year', choices = base::sort(unique(qfqm_macro_avail_data()$year),decreasing = TRUE))
#   } else {
#     shiny::textInput('swft_qfqm_macro_year_select', 'Year', placeholder = "No data found in the database...")
#   }
# })

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
  
  macro_plot_object = paste0("qfqm_flux_shiny/v", input$swft_qfqm_macro_code_version_select ,"/", input$swft_qfqm_macro_site_select, "/", input$swft_qfqm_macro_year_select, "_plot_", input$swft_qfqm_macro_terms, "_dqmp.RDS")
  
  if(eddycopipe::neon_gcs_object_exists(object = macro_plot_object, bucket = swft_ei_bucket)){
  
    eddycopipe::neon_gcs_get_rds(
      object =macro_plot_object ,
      bucket = swft_ei_bucket
    ) +
      ggdark::dark_theme_bw()
    
  } else {
    # Create a blank plot
    ggplot()+
      geom_text(label = "text")+
      ggdark::dark_theme_bw() +
      scale_x_continuous(breaks = 0)+
      scale_y_continuous(breaks = 0)+
      labs(y = "", x = '') +
      theme(
        axis.text.x = element_text(color = "black", size = 0),
        axis.text.y = element_text(color = "black", size = 0)
      )+
      annotate("text", label = paste0("No data found for ", input$swft_qfqm_macro_site_select, "\n",  input$swft_qfqm_macro_year_select, " data not available"), x = 0, y = 0, color = "white", size = 12)
  }
})

# output$swft_qfqm_macro_plot = shiny::renderPlot({
#   swft_qfqm_macro_plot()
# })

output$swft_qfqm_macro_plot = plotly::renderPlotly({
  plotly::ggplotly(
    swft_qfqm_macro_plot()
  )
})
