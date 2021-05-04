shiny::observeEvent(input$menu, {
  if(input$menu == "swft_qfqm_tab"){
    
    Sys.setenv("AWS_ACCESS_KEY_ID"     = "research-eddy-inquiry",
               "AWS_S3_ENDPOINT"       = "neonscience.org",
               "AWS_DEFAULT_REGION"    = "s3.data")
    
  }
  
  swft.dave.files = aws.s3::s3readRDS(object = "qfqm_dave_reports/dave_reports_meta.RDS", bucket = "research-eddy-inquiry")
  
  swft.site.files = shiny::reactive({
    swft.dave.files %>% 
      dplyr::filter(stringr::str_detect(string = file_rename, pattern = ".rds") == FALSE) %>%
      # dplyr::filter(site == "ABBY")
      dplyr::filter(site == input$swft_qfqm_dev_site)
  })
  
  shiny::renderUI({
    shiny::selectizeInput(inputId = "swft_qfqm_dev_selection", label = "Select File", choices = swft.site.files()$file_rename)

  })
  
  
  swft_qfqm_dev_data = shiny::reactive({
    aws.s3::get_object(object = paste0("qfqm_dave_reports/", input$swft_qfqm_dev_selection), bucket = "research-eddy-inquiry")
    picture = aws.s3::get_object(object = paste0("qfqm_dave_reports/", swft.site.files$file_rename[2]), bucket = "research-eddy-inquiry")
    # aws.s3::save_object(object = paste0("qfqm_dave_reports/", swft.site.files$file_rename[2]), bucket = "research-eddy-inquiry", file = "/1_Data/file.png")
    
    
    
    
  })

  
})