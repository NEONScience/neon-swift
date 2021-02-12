shiny::observeEvent(input$menu, {
  if(input$menu == "swft_qfqm_tab"){
    
    Sys.setenv("AWS_ACCESS_KEY_ID"     = "research-eddy-inquiry",
               "AWS_S3_ENDPOINT"       = "neonscience.org",
               "AWS_DEFAULT_REGION"    = "s3.data")
    
    
    data.in = aws.s3::s3read_using(FUN = fst::read.fst, object = "qfqm_report_data/qfqm.report.fst", bucket = "research-eddy-inquiry")
    
    if(nrow(data.in) > 0) {
      message("Data Read in Succesfully")
      output$qfqm_data_loaded = shiny::renderText("True")
    } else {
      message("Data read in poorly!")
    }
    outputOptions(output, "qfqm_data_loaded", suspendWhenHidden = FALSE)
    
    
    
    swft_qfqm_plot = shiny::reactive({
      
      # Sys.sleep(.5)
      
        data.in %>%
          dplyr::filter(site == input$swft_qfqm_site) %>%
          dplyr::filter(metric %in% c("qmAlph","qmBeta")) %>%
          dplyr::group_by(site, dp, var, metric, date, levlTowr) %>%
          dplyr::summarise(
            qfFinlTotl = sum(qfFinlTotl)
          ) %>%
          dplyr::filter(date == max(date)) %>%
          dplyr::filter(dp == input$swift_qfqm_dp)
      
    })
    
    output$swft_qfqm_vars = shiny::renderUI({
      shiny::selectInput('swft_qfqm_vars', 'Available Variables', base::sort(unique(swft_qfqm_plot()$var),decreasing = TRUE))
    })
    
    
    output$swft_qfqm_plot <- shiny::renderPlot({
      Sys.sleep(1)
      if(nrow(swft_qfqm_plot()) > 0 ){
        if(input$swft_qfqm_focus_in == "Yes" ){
          
          ggplot() +
            geom_histogram(data = swft_qfqm_plot() %>% dplyr::filter(var == input$swft_qfqm_vars), aes(x = qfFinlTotl, fill = metric), bins = 30) +
            facet_grid(~var) +
            labs(title = paste0(swft_qfqm_plot()$site[1], "'s ", swft_qfqm_plot()$dp[1], " QFQM Alpha and Beta Report"))+
            theme(legend.position = "none")
          
        } else {
        
          ggplot() +
            geom_histogram(data = swft_qfqm_plot(), aes(x = qfFinlTotl, fill = metric), bins = 30) +
            facet_grid(~var) +
            labs(title = paste0(swft_qfqm_plot()$site[1], "'s ", swft_qfqm_plot()$dp[1], " QFQM Alpha and Beta Report"))+
            theme(legend.position = "none")
          
        }
      } else {
        ggplot()+
          geom_text(label = "text")+
          annotate("text", label = paste0("No data found: ", input$swft_qfqm_site, "\n(Site communications may be down or data transitions have failed)."), x = 0, y = 0, color = "black")+
          theme_minimal()
      }

      
    })
    
    
     
  }
})