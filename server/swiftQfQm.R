# swiftQFQM
shiny::observeEvent(input$menu, {
  if(input$menu == "swft_qfqm_tab"){
    
    # Upon hitting "Gather QFQM data" pull the data from S3
    swft_qfqm_input_data = shiny::eventReactive(input$swft_qfqm_actionButton, {
      base::source("./R/qfqm_reporting.R")
      
      qfqm_reporting(site = input$swft_qfqm_site_select, start_date = input$swft_qfqm_date_select[1], end_date = input$swft_qfqm_date_select[2])
      # test = qfqm_reporting(site = "WREF", start_date = Sys.Date()-30, end_date = Sys.Date())
    })
    
    # Dynamically render the terms the user can select based upon what was found
    output$swft_qfqm_eddy4R_terms = shiny::renderUI({
      if(length(swft_qfqm_input_data()) != 0){
        shiny::selectInput(inputId = "swft_qfqm_eddy4R_terms", label = "eddy4R Terms", choices = names(swft_qfqm_input_data()), selected = "co2Stor")
      } else {
        shiny::selectInput(inputId = "swft_qfqm_eddy4R_terms", label = "eddy4R Terms", choices = "No data")
      }
      
    })
    
    output$swft_qfqm_table = DT::renderDataTable({
      if(length(swft_qfqm_input_data()) != 0){
        if(nrow(swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]]) > 0){
          swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]] %>% dplyr::select(-uid)
        } else{
          swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]]
        }
      } else{
        data.table::data.table()
      }
    })
    
    # Observe the input the user selects
    shiny::observeEvent(input$swft_qfqm_eddy4R_terms, {
      
      # Tower Maintenance
      if(length(swft_qfqm_input_data()) != 0){
      
        if(nrow(swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]]) > 0){
          
          message(input$swft_qfqm_eddy4R_terms)
        
          if(input$swft_qfqm_eddy4R_terms == "amrs"){
            
            output$swft_qfqm_plot = plotly::renderPlotly({
              
              ggplot(swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]], aes(x = date, y = qfFinlTotl, color = var)) +
                geom_point()+
                geom_line()+
                scale_y_continuous(limits = c(-8,48), breaks = c(0,10,20,30,40,48)) +
                scale_x_date(date_labels = "%m\n%d", breaks = scales::pretty_breaks(n = 8)) +
                labs(title = paste0(swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]]$site[1],"'s Summary QFQM Report for ", swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]]$dp[1]), x = "", color = "Variable")+
                ggdark::dark_theme_bw()
              
            })
            
            
          } else if(input$swft_qfqm_eddy4R_terms == "co2Stor"){
            
            output$swft_qfqm_plot = plotly::renderPlotly({
              ggplot(swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]], aes(x = date, y = qfFinlTotl, color = levlTowr)) +
                geom_point()+
                geom_line()+
                scale_y_continuous(limits = c(-8,48), breaks = c(0,10,20,30,40,48)) +
                scale_x_date(date_labels = "%m\n%d", breaks = scales::pretty_breaks(n = 8)) +
                facet_wrap(~var) +
                labs(title = paste0(swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]]$site[1],"'s Summary QFQM Report for ", swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]]$dp[1], "\n"), x = "", color = "Variable")+
                ggdark::dark_theme_bw()
              
            })
            
          } else if(input$swft_qfqm_eddy4R_terms == "co2Turb"){
            
            output$swft_qfqm_plot = plotly::renderPlotly({
              ggplot(swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]], aes(x = date, y = qfFinlTotl, color = var)) +
                geom_point()+
                geom_line()+
                scale_y_continuous(limits = c(-8,48), breaks = c(0,10,20,30,40,48)) +
                scale_x_date(date_labels = "%m\n%d", breaks = scales::pretty_breaks(n = 8)) +
                facet_wrap(~var) +
                labs(title = paste0(swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]]$site[1],"'s Summary QFQM Report for ", swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]]$dp[1]), x = "", color = "Variable")+
                ggdark::dark_theme_bw()
              
            })
            
          } else if(input$swft_qfqm_eddy4R_terms == "fluxHeatSoil"){
            
            output$swft_qfqm_plot = plotly::renderPlotly({
              
              ggplot(swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]], aes(x = date, y = qfFinlTotl, color = var)) +
                geom_point()+
                geom_line()+
                scale_y_continuous(limits = c(-8,48), breaks = c(0,10,20,30,40,48)) +
                scale_x_date(date_labels = "%m\n%d", breaks = scales::pretty_breaks(n = 8)) +
                facet_grid(sp~location) +
                labs(title = paste0(swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]]$site[1],"'s Summary QFQM Report for ", swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]]$dp[1]), x = "", color = "Variable")+
                ggdark::dark_theme_bw()
              
            })
            
          } else if(input$swft_qfqm_eddy4R_terms == "h2oSoilVol"){
            
            output$swft_qfqm_plot = plotly::renderPlotly({
              
              ggplot(swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]], aes(x = date, y = qfFinlTotl, color = var)) +
                geom_point()+
                geom_line()+
                scale_y_continuous(limits = c(-8,48), breaks = c(0,10,20,30,40,48)) +
                scale_x_date(date_labels = "%m\n%d", breaks = scales::pretty_breaks(n = 8)) +
                facet_grid(sp~location) +
                labs(title = paste0(swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]]$site[1],"'s Summary QFQM Report for ", swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]]$dp[1]), x = "", color = "Variable")+
                ggdark::dark_theme_bw()
              
            })
            
          } else if(input$swft_qfqm_eddy4R_terms == "h2oStor"){
            
            output$swft_qfqm_plot = plotly::renderPlotly({
              
              ggplot(swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]], aes(x = date, y = qfFinlTotl, color = levlTowr, group = levlTowr)) +
                geom_point()+
                geom_line()+
                scale_y_continuous(limits = c(-8,48), breaks = c(0,10,20,30,40,48)) +
                scale_x_date(date_labels = "%m\n%d", breaks = scales::pretty_breaks(n = 8)) +
                facet_wrap(~var) +
                labs(title = paste0(swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]]$site[1],"'s Summary QFQM Report for ", swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]]$dp[1]), x = "", color = "Variable")+
                ggdark::dark_theme_bw()
              
            })
            
          } else if(input$swft_qfqm_eddy4R_terms == "h2oTurb"){
            
            output$swft_qfqm_plot = plotly::renderPlotly({
              
              ggplot(swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]], aes(x = date, y = qfFinlTotl, color = var)) +
                geom_point()+
                geom_line()+
                scale_y_continuous(limits = c(-8,48), breaks = c(0,10,20,30,40,48)) +
                scale_x_date(date_labels = "%m\n%d", breaks = scales::pretty_breaks(n = 8)) +
                facet_wrap(~var) +
                labs(title = paste0(swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]]$site[1],"'s Summary QFQM Report for ", swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]]$dp[1]), x = "", color = "Variable")+
                ggdark::dark_theme_bw()
              
            })
            
          } else if(input$swft_qfqm_eddy4R_terms == "isoCo2"){
            
            output$swft_qfqm_plot = plotly::renderPlotly({
              
              ggplot(swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]], aes(x = date, y = qfFinlTotl, color = levlTowr, group = levlTowr)) +
                geom_point()+
                geom_line()+
                scale_y_continuous(limits = c(-8,48), breaks = c(0,10,20,30,40,48)) +
                scale_x_date(date_labels = "%m\n%d", breaks = scales::pretty_breaks(n = 8)) +
                facet_wrap(~var) +
                labs(title = paste0(swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]]$site[1],"'s Summary QFQM Report for ", swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]]$dp[1]), x = "", color = "Variable")+
                ggdark::dark_theme_bw()
              
            })
            
          } else if(input$swft_qfqm_eddy4R_terms == "isoH2o"){
            
            output$swft_qfqm_plot = plotly::renderPlotly({
              
              ggplot(swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]], aes(x = date, y = qfFinlTotl, color = levlTowr, group = levlTowr)) +
                geom_point()+
                geom_line()+
                scale_y_continuous(limits = c(-8,48), breaks = c(0,10,20,30,40,48)) +
                scale_x_date(date_labels = "%m\n%d", breaks = scales::pretty_breaks(n = 8)) +
                facet_wrap(~var) +
                labs(title = paste0(swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]]$site[1],"'s Summary QFQM Report for ", swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]]$dp[1]), x = "", color = "Variable")+
                ggdark::dark_theme_bw()
              
            })
            
          } else if(input$swft_qfqm_eddy4R_terms == "radiNet"){
            
            output$swft_qfqm_plot = plotly::renderPlotly({
              
              ggplot(swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]], aes(x = date, y = qfFinlTotl, color = var)) +
                geom_point()+
                geom_line()+
                scale_y_continuous(limits = c(-8,48), breaks = c(0,10,20,30,40,48)) +
                scale_x_date(date_labels = "%m\n%d", breaks = scales::pretty_breaks(n = 8)) +
                facet_wrap(~levlTowr) +
                labs(title = paste0(swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]]$site[1],"'s Summary QFQM Report for ", swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]]$dp[1]), x = "", color = "Variable")+
                ggdark::dark_theme_bw()
              
            })
            
            
          } else if(input$swft_qfqm_eddy4R_terms == "soni"){
            
            output$swft_qfqm_plot = plotly::renderPlotly({
              
              ggplot(swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]], aes(x = date, y = qfFinlTotl, color = var)) +
                geom_point()+
                geom_line()+
                scale_y_continuous(limits = c(-8,48), breaks = c(0,10,20,30,40,48)) +
                scale_x_date(date_labels = "%m\n%d", breaks = scales::pretty_breaks(n = 8)) +
                facet_wrap(~levlTowr) +
                labs(title = paste0(swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]]$site[1],"'s Summary QFQM Report for ", swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]]$dp[1]), x = "", color = "Variable")+
                ggdark::dark_theme_bw()
              
            })
            
          } else if(input$swft_qfqm_eddy4R_terms == "tempAirLvl"){
            
            output$swft_qfqm_plot = plotly::renderPlotly({
              
              ggplot(swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]], aes(x = date, y = qfFinlTotl, color = var)) +
                geom_point()+
                geom_line()+
                scale_y_continuous(limits = c(-8,48), breaks = c(0,10,20,30,40,48)) +
                scale_x_date(date_labels = "%m\n%d", breaks = scales::pretty_breaks(n = 8)) +
                facet_wrap(~levlTowr) +
                labs(title = paste0(swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]]$site[1],"'s Summary QFQM Report for ", swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]]$dp[1]), x = "", color = "Variable")+
                ggdark::dark_theme_bw()
              
            })
            
          } else if(input$swft_qfqm_eddy4R_terms == "tempAirTop"){
            
            output$swft_qfqm_plot = plotly::renderPlotly({
              
              ggplot(swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]], aes(x = date, y = qfFinlTotl, color = var)) +
                geom_point()+
                geom_line()+
                scale_y_continuous(limits = c(-8,48), breaks = c(0,10,20,30,40,48)) +
                scale_x_date(date_labels = "%m\n%d", breaks = scales::pretty_breaks(n = 8)) +
                facet_wrap(~levlTowr) +
                labs(title = paste0(swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]]$site[1],"'s Summary QFQM Report for ", swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]]$dp[1]), x = "", color = "Variable")+
                ggdark::dark_theme_bw()
              
            })
            
          } else if(input$swft_qfqm_eddy4R_terms == "tempSoil"){
            
            output$swft_qfqm_plot = plotly::renderPlotly({
              
              ggplot(swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]], aes(x = date, y = qfFinlTotl, color = var)) +
                geom_point()+
                geom_line()+
                scale_y_continuous(limits = c(-8,48), breaks = c(0,10,20,30,40,48)) +
                scale_x_date(date_labels = "%m\n%d", breaks = scales::pretty_breaks(n = 8)) +
                facet_grid(sp~location) +
                labs(title = paste0(swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]]$site[1],"'s Summary QFQM Report for ", swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]]$dp[1]), x = "", color = "Variable")+
                ggdark::dark_theme_bw()
              
            })
            
          } else {
            
          output$swft_qfqm_plot = plotly::renderPlotly({
            ggplot()+
              geom_text(label = "text")+
              annotate("text", label = paste0("No data found: ", input$swft_qfqm_eddy4R_terms, "\nTry a different eddy Term"), x = 0, y = 0, color = "white", size = 12) +
              ggdark::dark_theme_bw()
            
          })
            
             
          }
          
        } else {
        
          output$swft_qfqm_plot = plotly::renderPlotly({
            ggplot()+
              geom_text(label = "text")+
              annotate("text", label = paste0("No data found: ", input$swft_qfqm_eddy4R_terms, "\nTry increasing the date range"), x = 0, y = 0, color = "white", size = 12) +
              ggdark::dark_theme_bw()
            
          })
        }
        
      } else {
        
          output$swft_qfqm_plot = plotly::renderPlotly({
            ggplot()+
              geom_text(label = "text")+
              annotate("text", label = paste0("No data found: ", input$swft_qfqm_eddy4R_terms, "\nTry increasing the date range"), x = 0, y = 0, color = "white", size = 12) +
              ggdark::dark_theme_bw()
            
          })
      }
      
      
    })
    
    
    
    
    
    
    
    
  }


# End of the line buster
})