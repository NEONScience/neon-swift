# swiftObsMaintenance.R

shiny::observeEvent(input$menu, {
  if(input$menu == "swft_obs_maintenance_tab"){
    message("User selected swft_obs_maintenance_tab")
    # Read in Fulcrum TIS Maintenance Data
    library(ggplot2)
    library(dplyr)
    
    library(data.table)
    library(fst)
    library(plotly)
    
    
    ggplot2::theme_set(theme_bw())
    ei_bucket = "neon-eddy-inquiry"
    
    last_updated = lubridate::ymd_hms(max(eddycopipe::neon_gcs_list_objects(bucket = ei_bucket, prefix = "maintenance_app")$LastModified))
    
    output$swft_obsMaintenance_last_updated = shinydashboard::renderValueBox({
      shinydashboard::valueBox(value = last_updated, subtitle = "Last Updated (UTC)", color = "aqua", width = 12)
    })
    
    data.in = eddycopipe::neon_gcs_get_fst(object = "maintenance_app/all_data.fst", bucket = ei_bucket)%>%
      dplyr::mutate(date = as.Date(date, origin = "1970-01-01")) %>% 
      dplyr::mutate(cut = cut(date, breaks = "1 month")) %>% 
      dplyr::arrange(dplyr::desc(cut))
    
    meta.data = eddycopipe::neon_gcs_get_rds(object = "lookup/swft.full.site.lookup.RDS", bucket = "neon-eddy-inquiry") %>%
      dplyr::filter(Type == "TIS") %>%
      dplyr::mutate(domainid = Domain) %>%
      dplyr::mutate(siteid = factor(SiteID, levels = SiteID)) %>%
      dplyr::select(domainid, siteid) %>%
      dplyr::distinct()
    
    output$swft_obsMaintenance_month_pick = shiny::renderUI(
      shiny::selectInput(inputId = "swft_obsMaintenance_month_pick", label = "Choose Month", choices = unique(data.in$cut))
    )
    
    
    
    shiny::observeEvent( input$swft_obsMaintenance_month_pick,{
      test_plot = shiny::reactive({
        
        data.domain.sum = data.in %>%
          dplyr::filter(cut == input$swft_obsMaintenance_month_pick) %>% 
          dplyr::filter(is.na(cut) == FALSE) %>% 
          dplyr::filter(domainid != "18") %>% 
          dplyr::group_by(siteid,cut) %>%
          dplyr::summarise(.groups = "drop",
            count = n()
          )
      

          
        data.domain.plot = left_join(meta.data, data.domain.sum, by = "siteid") %>%
          dplyr::mutate(siteid = factor(siteid, levels = meta.data$siteid)) %>%
          dplyr::filter(is.na(cut) == FALSE) %>% 
          dplyr::mutate(count = ifelse(test = is.na(count) == TRUE, yes = 0, no = count)) %>%
          dplyr::arrange(-desc(siteid))
        
        plot_data = data.table::data.table() 
        for(i in seq_along(meta.data$siteid)){
          
          if(meta.data$siteid[i] %in% data.domain.plot$siteid){
            add_data = data.domain.plot %>% 
              dplyr::filter(siteid == meta.data$siteid[i])
            
          } else {
            add_data = data.table::data.table(
              domainid = meta.data$domainid[i],
              siteid   = meta.data$siteid[i],
              cut = input$swft_obsMaintenance_month_pick,
              count    = 0
            )
          }
          
          plot_data = data.table::rbindlist(l = list(plot_data, add_data))
          
        }
        
        ggplot(data = plot_data, aes(x = siteid, y = count, fill = domainid)) +
          geom_col() +
          labs(title = paste0("Total Maintenance Records during ", input$swft_obsMaintenance_month_pick),tag = "test", x = "", y = "") +
          ggdark::dark_theme_bw() +
          theme(
            legend.position = "none",
            text = element_text(size = 16),
            axis.text.x = element_text(angle = 270)
          )
      })
      
          
      output$swftObsMaintenance_plot = plotly::renderPlotly({
        test_plot()
      })
    })
    
    
    
    
    first_record = min(data.in$date)
    shiny::observeEvent(input$swft_obsMaintenance_rate_range,{
      swft_obsMaintenance_rate_plot = shiny::reactive({
        
        first_rate_date = Sys.Date()-30
        first_rate_date  = Sys.Date() - as.numeric(input$swft_obsMaintenance_rate_range)

        maintenance_rate = data.in %>%
          dplyr::filter(date >=first_rate_date) %>% 
          dplyr::group_by(siteid) %>%
          dplyr::summarise(
            length = length(siteid),
            start  = first_rate_date,
            end    = Sys.Date(),
            diff   = as.numeric(difftime(end, start, units = "days")/14),
            rate   = round(length/diff,0)
          )
    
        maintenance_rate_complete = dplyr::left_join(x = meta.data, y = maintenance_rate, by = "siteid") %>%
          dplyr::mutate(siteid = factor(siteid, levels = meta.data$siteid))
    
        ggplot(maintenance_rate_complete, aes(x = siteid, y = rate, fill = domainid))+
          geom_col()+
          geom_hline(yintercept = 1, color = "green", size = 2)+
          labs(x = "", y = "Rate") +
          ggdark::dark_theme_bw() +
          theme(
                legend.position = "none",
                text = element_text(size = 16),
                axis.text.x = element_text(angle = 270)
              )
      })
      
      output$swft_obsMaintenance_rate_plot = plotly::renderPlotly({
        swft_obsMaintenance_rate_plot()
      })
    
      
    })
    

  } # End initial if statement
}) # End tab observerEvent
