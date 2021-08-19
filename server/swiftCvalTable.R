shiny::observeEvent(input$menu, {
  if(input$menu == "swft_historic_span_table_tab"){
    
    # Set S3 Endpoints for safety
    base::Sys.setenv(
      "AWS_S3_ENDPOINT"       = "neonscience.org",
      "AWS_DEFAULT_REGION"    = "s3.data"
    )
  
    # Source the base data pulling function
    base::source(paste0(swft.server.folder.path, "R/read.eddy.inquiry.swift.R"))
    
    # Cylinder Metadata
    swft_historic_span = read.eddy.inquiry(dataType = "meta", sensor = "spanGas")
    
    swft_historic_span_summary = swft_historic_span %>% 
      dplyr::group_by(siteID, assetTag, name) %>% 
      dplyr::summarise(
        CertificateNumber = CertificateNumber[1],
        aTag = aTag[1],
        concentration = conc[1],
        concDELTA = concDELTA[1],
        concCH4 = concCH4[1],
        start_date = min(as.Date(date, origin = "1970-01-01")), 
        end_date = max(date),
        install_period = as.numeric(difftime(end_date, start_date, units = "days")),
        still_installed = ifelse(end_date %in% c(Sys.Date()-3,Sys.Date()-2,Sys.Date()-1, Sys.Date(), Sys.Date()+1,Sys.Date()+2,Sys.Date()+3 ), yes = TRUE, no = FALSE)
      ) %>% 
      dplyr::mutate(notes = ifelse(test = end_date == "2020-06-30", yes = "Uninstall period unknown, check Maximo with the asset tag for more accurate info.", no = NA)) %>% 
      dplyr::mutate(notes = ifelse(test = start_date == "2020-09-21", yes = "Install period unknown, check Maximo with the asset tag for more accurate info.", no = notes)) %>% 
      dplyr::arrange(desc(start_date))
    
    output$swft_historic_span_table <- DT::renderDT(
      DT::datatable(
        data = swft_historic_span_summary,
          style = "bootstrap",
          filter = "top",
          options = list(
            pageLength = 50,
            deferRender = TRUE,
            scrollY = 600,
            scrollCollapse = TRUE,
            scrollX = FALSE,
            paging = TRUE),rownames = FALSE) %>% 
      DT::formatStyle(table = .,
        columns = "still_installed", 
        backgroundColor = DT::styleEqual(c(NA,FALSE,TRUE), c("gray", "#f96161", "#35fc60")), 
        color = "black"
      ) %>% 
      DT::formatStyle(table = .,
        columns = "end_date", 
        backgroundColor = DT::styleInterval(c("2019-06-19","2020-06-29", "2020-06-30","2020-06-31", as.character(Sys.Date())), c( "#35fc60","#35fc60", "#f96161","#35fc60", "#35fc60", "#35fc60")), 
        color = "black"
      ) %>% 
      DT::formatStyle(table = .,
        columns = "start_date", 
        backgroundColor = DT::styleInterval(c("2019-06-18","2020-09-20","2020-09-21","2020-09-22", as.character(Sys.Date())), c( "#35fc60","#35fc60", "#f96161","#35fc60", "#35fc60", "#35fc60")), 
        color = "black"
      ) %>% 
      DT::formatStyle(table = .,
        columns = "name", 
        backgroundColor = DT::styleEqual(c("ECSE-LOW","ECSE-MEDIUM","ECSE-HIGH","ECSE-Archive","ECTE-LOW","ECTE-MEDIUM","ECTE-HIGH","ECTE-Archive"), 
                                     c("grey",    "#35fc60",     "#9400D3","#8B4513",        "gold","#f96161","#617ff9","#a0522d")), 
        color = "black"
      ) 
      

      )
    
  }
})