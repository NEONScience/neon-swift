shiny::observeEvent(input$menu, {
  if(input$menu == "swft_wet_dep_tab"){
    
    # Libraries  
    library(dplyr)
    library(tidyr)
    
    library(fst)
    library(ggplot2)
    library(ggdark)
  
  
    # Read Data from File
    wet_dep_raw = eddycopipe::neon_gcs_get_fst(object = "fulcurm/wet_dep/main.fst", bucket = ei_bucket)
    
    # Input date reactive wet dep data
    reactive_wet_dep_data = shiny::reactive({
      
      wet_dep_raw %>% 
        dplyr::select(domainid, siteid, set_date, set_time, end_date, end_time, asetby, bsetby, sampleid, sampleduration, samplefate, tidyr::everything()) %>%
        dplyr::filter(set_date >= input$swft_wet_dep_date_select[1] & set_date <= input$swft_wet_dep_date_select[2])
        
    })
    
    # Data for plot 1
    reactive_wet_dep_record_counts = shiny::reactive({
      wet_dep_tidy = reactive_wet_dep_data() %>% 
        dplyr::group_by(domainid, siteid) %>% 
        dplyr::count()
    
      wet_sites = data.table::data.table("siteid" = eddycopipe::neon_gcs_get_rds(object = "lookup/wet_dep_sites.RDS", bucket = ei_bucket))
      
      
      meta.data.1 = eddycopipe::neon_gcs_get_rds(object = "lookup/swft.full.site.lookup.RDS", bucket = "neon-eddy-inquiry") %>%
        dplyr::mutate(domainid = Domain) %>%
        dplyr::mutate(siteid = factor(SiteID, levels = SiteID)) %>%
        dplyr::select(domainid, siteid, Type) %>%
        dplyr::distinct()
      
      meta.data.2 = meta.data.1 %>%
        dplyr::select(siteid, Type)
      
      dplyr::full_join(x = wet_sites, y = wet_dep_tidy, by = "siteid") %>% 
        dplyr::mutate(siteid = factor(siteid, levels = meta.data.1$siteid)) %>%
        dplyr::left_join(y = meta.data.2, by = "siteid") %>% 
        dplyr::mutate(domainid = ifelse(siteid == "BARR", yes = "D18", no = domainid)) %>%
        dplyr::mutate(n = base::ifelse(test = is.na(n), yes = 0, no = n))
    })
    
    # Data for plot 2
    reactive_wet_dep_sample_durations = shiny::reactive({
      
      
      
    })
        
    # Reactive Plot
    reactive_wet_dep_plot_1 = shiny::reactive({
      ggplot(reactive_wet_dep_record_counts(), aes(x = siteid, y = n, fill = domainid)) +
        geom_col() +
        labs(title = "Wet Deposition Collection Records", subtitle = paste0( input$swft_wet_dep_date_select[1], " to ",  input$swft_wet_dep_date_select[2]), x = "", y = "# of records", fill = "Domain")+
        ggdark::dark_theme_bw() +
        theme(
          text = element_text(color = "white", face = "bold", size = 14),
          legend.position = "none",
          axis.text.x = element_text(angle = 270)
        )
    })
    
    reactive_wet_dep_plot_2 = shiny::reactive({
      
      ggplot(reactive_wet_dep_data() %>% dplyr::mutate(days = round(sampleduration/24,0)) %>% dplyr::mutate(SiteID = siteid), 
             aes(x = days, fill = SiteID)) +
        geom_histogram(binwidth = 1) +
        scale_x_continuous() +
        labs(x = "Days", y = "Count", title = "Sample Durations", subtitle = "Sample Duration should last roughly 2 weeks") +
        ggdark::dark_theme_bw() +
        theme(
          text = element_text(color = "white", face = "bold", size = 14),
          legend.position = "none",
          axis.text.x = element_text(angle = 270)
        )
      
    })
    
    reactive_wet_dep_plot_3 = shiny::reactive({
      
      ggplot(reactive_wet_dep_data() %>% dplyr::mutate(`Isotope Subsample Mass` = round(isosubsamplebottleendmassmeas,2)) %>% dplyr::mutate(SiteID = siteid), 
             aes(x = `Isotope Subsample Mass`, fill = SiteID)) +
        geom_histogram(bins = 30) +
        scale_x_continuous() +
        labs(x = "Grams", y = "Count", title = "Isotope Subsample Mass", subtitle = "Histogram") +
        ggdark::dark_theme_bw() +
        theme(
          text = element_text(color = "white", face = "bold", size = 14),
          legend.position = "none",
          axis.text.x = element_text(angle = 270)
        )
      
    })
        
    reactive_wet_dep_plot_4 = shiny::reactive({
      
      ggplot(reactive_wet_dep_data() %>% dplyr::mutate(`Chem Subsample Mass` = round(chemsubsamplebottleendmassmeas,2)) %>% dplyr::mutate(SiteID = siteid), 
             aes(x = `Chem Subsample Mass`, fill = SiteID)) +
        geom_histogram(bins = 30) +
        scale_x_continuous() +
        labs(x = "Grams", y = "Count", title = "Chemical Subsample Mass", caption = "Histogram") +
        ggdark::dark_theme_bw() +
        theme(
          text = element_text(color = "white", face = "bold", size = 14),
          legend.position = "none",
          axis.text.x = element_text(angle = 270)
        )
      
    })
    
    # Render Reactive Plot
     output$swft_wet_dep_plot_1 = plotly::renderPlotly(
      reactive_wet_dep_plot_1()
    )
    output$swft_wet_dep_plot_2 = plotly::renderPlotly(
      reactive_wet_dep_plot_2()
    )
    output$swft_wet_dep_plot_3 = plotly::renderPlotly(
      reactive_wet_dep_plot_3()
    )
    output$swft_wet_dep_plot_4 = plotly::renderPlotly(
      reactive_wet_dep_plot_4()
    )

  }
})
