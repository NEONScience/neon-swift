# swiftQFQM
# shiny::observeEvent(input$menu, {
#   if(input$menu == "swft_qfqm_tab"){
    
    #### Data ingest Logic
    # Upon hitting "Gather QFQM data" pull the data from S3
    swft_qfqm_input_data = shiny::eventReactive(input$swft_qfqm_actionButton, {
      message(paste0(input$swft_qfqm_site_select, " ", input$swft_qfqm_eddy4R_terms, ": ",input$swft_qfqm_date_select[1], " - ", input$swft_qfqm_date_select[2]))
      
      base::source("./R/qfqm_reporting.R")
      qfqm_reporting(site = input$swft_qfqm_site_select, start_date = input$swft_qfqm_date_select[1], end_date = input$swft_qfqm_date_select[2])
    })
    
    ### Render what variables are present for further filtering
    # Dynamically render the var's the user can select based upon what was found
    output$swft_qfqm_eddy4R_vars = shiny::renderUI({
      req(input$swft_qfqm_eddy4R_terms)
      if(length(swft_qfqm_input_data()) != 0){
        shinyWidgets::pickerInput(inputId = "swft_qfqm_eddy4R_vars", label = "Variables", 
                                  choices = unique(swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]]$var), multiple = TRUE, options = list(`actions-box` = TRUE), 
                                  selected = unique(swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]]$var))
      } else {
        shiny::selectInput(inputId = "swft_qfqm_eddy4R_vars", label = "Variables", choices = "No data")
      }
    })
    
    # Filter data down to just the var selected, default is all vars
    swft_qfqm_data = shiny::reactive({
      swft_qfqm_data_reac = swft_qfqm_input_data()[[input$swft_qfqm_eddy4R_terms]] 
      if(nrow(swft_qfqm_data_reac) > 0){
        swft_qfqm_data_reac %>% dplyr::filter(var %in% input$swft_qfqm_eddy4R_vars)
      } else {
        swft_qfqm_data_reac
      }
    })
    
    # Plotting Logic
    swft_qfqm_plot_reactive = shiny::reactive({
      shiny::req(input$swft_qfqm_eddy4R_terms)
      if(length(swft_qfqm_input_data()) != 0){
        if(nrow(swft_qfqm_data()) > 0){
          if(input$swft_qfqm_facet == "Yes"){
            if(input$swft_qfqm_eddy4R_terms == "amrs"){
                ggplot(swft_qfqm_data(), aes(x = date, y = qfFinlTotl, color = var)) +
                  geom_point()+
                  geom_line()+
                  scale_y_continuous(limits = c(-8,48), breaks = c(0,10,20,30,40,48)) +
                  scale_x_date(date_labels = "%m\n%d", breaks = scales::pretty_breaks(n = 8)) +
                  facet_wrap(~var) +
                  labs(title = paste0(swft_qfqm_data()$site[1],"'s Summary QFQM Report for ", input$swft_qfqm_eddy4R_terms), x = "", color = "Variable")+
                  ggdark::dark_theme_bw()
            } else if(input$swft_qfqm_eddy4R_terms == "co2Stor"){
                ggplot(swft_qfqm_data(), aes(x = date, y = qfFinlTotl, color = levlTowr)) +
                  geom_point()+
                  geom_line()+
                  scale_y_continuous(limits = c(-8,48), breaks = c(0,10,20,30,40,48)) +
                  scale_x_date(date_labels = "%m\n%d", breaks = scales::pretty_breaks(n = 8)) +
                  facet_wrap(~var) +
                  labs(title = paste0(swft_qfqm_data()$site[1],"'s Summary QFQM Report for ", input$swft_qfqm_eddy4R_terms, "\n"), x = "", color = "Variable")+
                  ggdark::dark_theme_bw()
            } else if(input$swft_qfqm_eddy4R_terms == "co2Turb"){
                ggplot(swft_qfqm_data(), aes(x = date, y = qfFinlTotl, color = var)) +
                  geom_point()+
                  geom_line()+
                  scale_y_continuous(limits = c(-8,48), breaks = c(0,10,20,30,40,48)) +
                  scale_x_date(date_labels = "%m\n%d", breaks = scales::pretty_breaks(n = 8)) +
                  facet_wrap(~var) +
                  labs(title = paste0(swft_qfqm_data()$site[1],"'s Summary QFQM Report for ", input$swft_qfqm_eddy4R_terms), x = "", color = "Variable")+
                  ggdark::dark_theme_bw()
            } else if(input$swft_qfqm_eddy4R_terms == "fluxHeatSoil"){
                ggplot(swft_qfqm_data(), aes(x = date, y = qfFinlTotl, color = var)) +
                  geom_point()+
                  geom_line()+
                  scale_y_continuous(limits = c(-8,48), breaks = c(0,10,20,30,40,48)) +
                  scale_x_date(date_labels = "%m\n%d", breaks = scales::pretty_breaks(n = 8)) +
                  facet_grid(sp~location) +
                  labs(title = paste0(swft_qfqm_data()$site[1],"'s Summary QFQM Report for ", input$swft_qfqm_eddy4R_terms), x = "", color = "Variable")+
                  ggdark::dark_theme_bw()
            } else if(input$swft_qfqm_eddy4R_terms == "h2oSoilVol"){
                ggplot(swft_qfqm_data(), aes(x = date, y = qfFinlTotl, color = var)) +
                  geom_point()+
                  geom_line()+
                  scale_y_continuous(limits = c(-8,48), breaks = c(0,10,20,30,40,48)) +
                  scale_x_date(date_labels = "%m\n%d", breaks = scales::pretty_breaks(n = 8)) +
                  facet_grid(sp~location) +
                  labs(title = paste0(swft_qfqm_data()$site[1],"'s Summary QFQM Report for ", input$swft_qfqm_eddy4R_terms), x = "", color = "Variable")+
                  ggdark::dark_theme_bw()
            } else if(input$swft_qfqm_eddy4R_terms == "h2oStor"){
                ggplot(swft_qfqm_data(), aes(x = date, y = qfFinlTotl, color = levlTowr, group = levlTowr)) +
                  geom_point()+
                  geom_line()+
                  scale_y_continuous(limits = c(-8,48), breaks = c(0,10,20,30,40,48)) +
                  scale_x_date(date_labels = "%m\n%d", breaks = scales::pretty_breaks(n = 8)) +
                  facet_wrap(~var) +
                  labs(title = paste0(swft_qfqm_data()$site[1],"'s Summary QFQM Report for ", input$swft_qfqm_eddy4R_terms), x = "", color = "Variable")+
                  ggdark::dark_theme_bw()
            } else if(input$swft_qfqm_eddy4R_terms == "h2oTurb"){
                ggplot(swft_qfqm_data(), aes(x = date, y = qfFinlTotl, color = var)) +
                  geom_point()+
                  geom_line()+
                  scale_y_continuous(limits = c(-8,48), breaks = c(0,10,20,30,40,48)) +
                  scale_x_date(date_labels = "%m\n%d", breaks = scales::pretty_breaks(n = 8)) +
                  facet_wrap(~var) +
                  labs(title = paste0(swft_qfqm_data()$site[1],"'s Summary QFQM Report for ", input$swft_qfqm_eddy4R_terms), x = "", color = "Variable")+
                  ggdark::dark_theme_bw()
            } else if(input$swft_qfqm_eddy4R_terms == "isoCo2"){
                ggplot(swft_qfqm_data(), aes(x = date, y = qfFinlTotl, color = levlTowr, group = levlTowr)) +
                  geom_point()+
                  geom_line()+
                  scale_y_continuous(limits = c(-8,48), breaks = c(0,10,20,30,40,48)) +
                  scale_x_date(date_labels = "%m\n%d", breaks = scales::pretty_breaks(n = 8)) +
                  facet_wrap(~var) +
                  labs(title = paste0(swft_qfqm_data()$site[1],"'s Summary QFQM Report for ", input$swft_qfqm_eddy4R_terms), x = "", color = "Variable")+
                  ggdark::dark_theme_bw()
            } else if(input$swft_qfqm_eddy4R_terms == "isoH2o"){
                ggplot(swft_qfqm_data(), aes(x = date, y = qfFinlTotl, color = levlTowr, group = levlTowr)) +
                  geom_point()+
                  geom_line()+
                  scale_y_continuous(limits = c(-8,48), breaks = c(0,10,20,30,40,48)) +
                  scale_x_date(date_labels = "%m\n%d", breaks = scales::pretty_breaks(n = 8)) +
                  facet_wrap(~var) +
                  labs(title = paste0(swft_qfqm_data()$site[1],"'s Summary QFQM Report for ", input$swft_qfqm_eddy4R_terms), x = "", color = "Variable")+
                  ggdark::dark_theme_bw()
            } else if(input$swft_qfqm_eddy4R_terms == "radiNet"){
                ggplot(swft_qfqm_data(), aes(x = date, y = qfFinlTotl, color = var)) +
                  geom_point()+
                  geom_line()+
                  scale_y_continuous(limits = c(-8,48), breaks = c(0,10,20,30,40,48)) +
                  scale_x_date(date_labels = "%m\n%d", breaks = scales::pretty_breaks(n = 8)) +
                  facet_wrap(~var) +
                  labs(title = paste0(swft_qfqm_data()$site[1],"'s Summary QFQM Report for ", input$swft_qfqm_eddy4R_terms), x = "", color = "Variable")+
                  ggdark::dark_theme_bw()
            } else if(input$swft_qfqm_eddy4R_terms == "soni"){
                ggplot(swft_qfqm_data(), aes(x = date, y = qfFinlTotl, color = var)) +
                  geom_point()+
                  geom_line()+
                  scale_y_continuous(limits = c(-8,48), breaks = c(0,10,20,30,40,48)) +
                  scale_x_date(date_labels = "%m\n%d", breaks = scales::pretty_breaks(n = 8)) +
                  facet_wrap(~var) +
                  labs(title = paste0(swft_qfqm_data()$site[1],"'s Summary QFQM Report for ", input$swft_qfqm_eddy4R_terms), x = "", color = "Variable")+
                  ggdark::dark_theme_bw()
            } else if(input$swft_qfqm_eddy4R_terms == "tempAirLvl"){
                ggplot(swft_qfqm_data(), aes(x = date, y = qfFinlTotl, color = var)) +
                  geom_point()+
                  geom_line()+
                  scale_y_continuous(limits = c(-8,48), breaks = c(0,10,20,30,40,48)) +
                  scale_x_date(date_labels = "%m\n%d", breaks = scales::pretty_breaks(n = 8)) +
                  facet_wrap(~levlTowr) +
                  labs(title = paste0(swft_qfqm_data()$site[1],"'s Summary QFQM Report for ", input$swft_qfqm_eddy4R_terms), x = "", color = "Variable")+
                  ggdark::dark_theme_bw()
            } else if(input$swft_qfqm_eddy4R_terms == "tempAirTop"){
                ggplot(swft_qfqm_data(), aes(x = date, y = qfFinlTotl, color = var)) +
                  geom_point()+
                  geom_line()+
                  scale_y_continuous(limits = c(-8,48), breaks = c(0,10,20,30,40,48)) +
                  scale_x_date(date_labels = "%m\n%d", breaks = scales::pretty_breaks(n = 8)) +
                  facet_wrap(~levlTowr) +
                  labs(title = paste0(swft_qfqm_data()$site[1],"'s Summary QFQM Report for ", input$swft_qfqm_eddy4R_terms), x = "", color = "Variable")+
                  ggdark::dark_theme_bw()
            } else if(input$swft_qfqm_eddy4R_terms == "tempSoil"){
                ggplot(swft_qfqm_data(), aes(x = date, y = qfFinlTotl, color = var)) +
                  geom_point()+
                  geom_line()+
                  scale_y_continuous(limits = c(-8,48), breaks = c(0,10,20,30,40,48)) +
                  scale_x_date(date_labels = "%m\n%d", breaks = scales::pretty_breaks(n = 8)) +
                  facet_grid(sp~location) +
                  labs(title = paste0(swft_qfqm_data()$site[1],"'s Summary QFQM Report for ", input$swft_qfqm_eddy4R_terms), x = "", color = "Variable")+
                  ggdark::dark_theme_bw()
            } else {
              ggplot()+
                geom_text(label = "text")+
                annotate("text", label = paste0("No QFQM data found: ", input$swft_qfqm_eddy4R_terms, "\nData during time period selected\n is not flagged"), x = 0, y = 0, color = "white", size = 12) +
                ggdark::dark_theme_bw()
            }
          } else {
            if(input$swft_qfqm_eddy4R_terms == "amrs"){
              ggplot(swft_qfqm_data(), aes(x = date, y = qfFinlTotl, color = var)) +
                geom_point()+
                geom_line()+
                scale_y_continuous(limits = c(-8,48), breaks = c(0,10,20,30,40,48)) +
                scale_x_date(date_labels = "%m\n%d", breaks = scales::pretty_breaks(n = 8)) +
                labs(title = paste0(swft_qfqm_data()$site[1],"'s Summary QFQM Report for ", input$swft_qfqm_eddy4R_terms), x = "", color = "Variable")+
                ggdark::dark_theme_bw()
            } else if(input$swft_qfqm_eddy4R_terms == "co2Stor"){
              ggplot(swft_qfqm_data(), aes(x = date, y = qfFinlTotl, color = var, group = levlTowr)) +
                geom_point()+
                geom_line()+
                scale_y_continuous(limits = c(-8,48), breaks = c(0,10,20,30,40,48)) +
                scale_x_date(date_labels = "%m\n%d", breaks = scales::pretty_breaks(n = 8)) +
                labs(title = paste0(swft_qfqm_data()$site[1],"'s Summary QFQM Report for ", input$swft_qfqm_eddy4R_terms, "\n"), x = "", color = "Variable")+
                ggdark::dark_theme_bw()
            } else if(input$swft_qfqm_eddy4R_terms == "co2Turb"){
              ggplot(swft_qfqm_data(), aes(x = date, y = qfFinlTotl, color = var, group = levlTowr)) +
                geom_point()+
                geom_line()+
                scale_y_continuous(limits = c(-8,48), breaks = c(0,10,20,30,40,48)) +
                scale_x_date(date_labels = "%m\n%d", breaks = scales::pretty_breaks(n = 8)) +
                labs(title = paste0(swft_qfqm_data()$site[1],"'s Summary QFQM Report for ", input$swft_qfqm_eddy4R_terms), x = "", color = "Variable")+
                ggdark::dark_theme_bw()
            } else if(input$swft_qfqm_eddy4R_terms == "fluxHeatSoil"){
              ggplot(swft_qfqm_data(), aes(x = date, y = qfFinlTotl, color = var, group = levlTowr)) +
                geom_point()+
                geom_line()+
                scale_y_continuous(limits = c(-8,48), breaks = c(0,10,20,30,40,48)) +
                scale_x_date(date_labels = "%m\n%d", breaks = scales::pretty_breaks(n = 8)) +
                facet_grid(sp~location) +
                labs(title = paste0(swft_qfqm_data()$site[1],"'s Summary QFQM Report for ", input$swft_qfqm_eddy4R_terms), x = "", color = "Variable")+
                ggdark::dark_theme_bw()
            } else if(input$swft_qfqm_eddy4R_terms == "h2oSoilVol"){
              ggplot(swft_qfqm_data(), aes(x = date, y = qfFinlTotl, color = var)) +
                geom_point()+
                geom_line()+
                scale_y_continuous(limits = c(-8,48), breaks = c(0,10,20,30,40,48)) +
                scale_x_date(date_labels = "%m\n%d", breaks = scales::pretty_breaks(n = 8)) +
                facet_grid(sp~location) +
                labs(title = paste0(swft_qfqm_data()$site[1],"'s Summary QFQM Report for ", input$swft_qfqm_eddy4R_terms), x = "", color = "Variable")+
                ggdark::dark_theme_bw()
            } else if(input$swft_qfqm_eddy4R_terms == "h2oStor"){
              ggplot(swft_qfqm_data(), aes(x = date, y = qfFinlTotl, color = levlTowr, group = levlTowr)) +
                geom_point()+
                geom_line()+
                scale_y_continuous(limits = c(-8,48), breaks = c(0,10,20,30,40,48)) +
                scale_x_date(date_labels = "%m\n%d", breaks = scales::pretty_breaks(n = 8)) +
                labs(title = paste0(swft_qfqm_data()$site[1],"'s Summary QFQM Report for ", input$swft_qfqm_eddy4R_terms), x = "", color = "Variable")+
                ggdark::dark_theme_bw()
            } else if(input$swft_qfqm_eddy4R_terms == "h2oTurb"){
              ggplot(swft_qfqm_data(), aes(x = date, y = qfFinlTotl, color = var, group = levlTowr)) +
                geom_point()+
                geom_line()+
                scale_y_continuous(limits = c(-8,48), breaks = c(0,10,20,30,40,48)) +
                scale_x_date(date_labels = "%m\n%d", breaks = scales::pretty_breaks(n = 8)) +
                labs(title = paste0(swft_qfqm_data()$site[1],"'s Summary QFQM Report for ", input$swft_qfqm_eddy4R_terms), x = "", color = "Variable")+
                ggdark::dark_theme_bw()
            } else if(input$swft_qfqm_eddy4R_terms == "isoCo2"){
              ggplot(swft_qfqm_data(), aes(x = date, y = qfFinlTotl, color = levlTowr, group = levlTowr)) +
                geom_point()+
                geom_line()+
                scale_y_continuous(limits = c(-8,48), breaks = c(0,10,20,30,40,48)) +
                scale_x_date(date_labels = "%m\n%d", breaks = scales::pretty_breaks(n = 8)) +
                labs(title = paste0(swft_qfqm_data()$site[1],"'s Summary QFQM Report for ", input$swft_qfqm_eddy4R_terms), x = "", color = "Variable")+
                ggdark::dark_theme_bw()
            } else if(input$swft_qfqm_eddy4R_terms == "isoH2o"){
              ggplot(swft_qfqm_data(), aes(x = date, y = qfFinlTotl, color = levlTowr, group = levlTowr)) +
                geom_point()+
                geom_line()+
                scale_y_continuous(limits = c(-8,48), breaks = c(0,10,20,30,40,48)) +
                scale_x_date(date_labels = "%m\n%d", breaks = scales::pretty_breaks(n = 8)) +
                labs(title = paste0(swft_qfqm_data()$site[1],"'s Summary QFQM Report for ", input$swft_qfqm_eddy4R_terms), x = "", color = "Variable")+
                ggdark::dark_theme_bw()
            } else if(input$swft_qfqm_eddy4R_terms == "radiNet"){
              ggplot(swft_qfqm_data(), aes(x = date, y = qfFinlTotl, color = var, group = levlTowr)) +
                geom_point()+
                geom_line()+
                scale_y_continuous(limits = c(-8,48), breaks = c(0,10,20,30,40,48)) +
                scale_x_date(date_labels = "%m\n%d", breaks = scales::pretty_breaks(n = 8)) +
                labs(title = paste0(swft_qfqm_data()$site[1],"'s Summary QFQM Report for ", input$swft_qfqm_eddy4R_terms), x = "", color = "Variable")+
                ggdark::dark_theme_bw()
            } else if(input$swft_qfqm_eddy4R_terms == "soni"){
              ggplot(swft_qfqm_data(), aes(x = date, y = qfFinlTotl, color = var, group = levlTowr)) +
                geom_point()+
                geom_line()+
                scale_y_continuous(limits = c(-8,48), breaks = c(0,10,20,30,40,48)) +
                scale_x_date(date_labels = "%m\n%d", breaks = scales::pretty_breaks(n = 8)) +
                labs(title = paste0(swft_qfqm_data()$site[1],"'s Summary QFQM Report for ", input$swft_qfqm_eddy4R_terms), x = "", color = "Variable")+
                ggdark::dark_theme_bw()
            } else if(input$swft_qfqm_eddy4R_terms == "tempAirLvl"){
              ggplot(swft_qfqm_data(), aes(x = date, y = qfFinlTotl, color = var)) +
                geom_point()+
                geom_line()+
                scale_y_continuous(limits = c(-8,48), breaks = c(0,10,20,30,40,48)) +
                scale_x_date(date_labels = "%m\n%d", breaks = scales::pretty_breaks(n = 8)) +
                facet_wrap(~levlTowr) +
                labs(title = paste0(swft_qfqm_data()$site[1],"'s Summary QFQM Report for ", input$swft_qfqm_eddy4R_terms), x = "", color = "Variable")+
                ggdark::dark_theme_bw()
            } else if(input$swft_qfqm_eddy4R_terms == "tempAirTop"){
              ggplot(swft_qfqm_data(), aes(x = date, y = qfFinlTotl, color = var)) +
                geom_point()+
                geom_line()+
                scale_y_continuous(limits = c(-8,48), breaks = c(0,10,20,30,40,48)) +
                scale_x_date(date_labels = "%m\n%d", breaks = scales::pretty_breaks(n = 8)) +
                facet_wrap(~levlTowr) +
                labs(title = paste0(swft_qfqm_data()$site[1],"'s Summary QFQM Report for ", input$swft_qfqm_eddy4R_terms), x = "", color = "Variable")+
                ggdark::dark_theme_bw()
            } else if(input$swft_qfqm_eddy4R_terms == "tempSoil"){
              ggplot(swft_qfqm_data(), aes(x = date, y = qfFinlTotl, color = var)) +
                geom_point()+
                geom_line()+
                scale_y_continuous(limits = c(-8,48), breaks = c(0,10,20,30,40,48)) +
                scale_x_date(date_labels = "%m\n%d", breaks = scales::pretty_breaks(n = 8)) +
                facet_grid(sp~location) +
                labs(title = paste0(swft_qfqm_data()$site[1],"'s Summary QFQM Report for ", input$swft_qfqm_eddy4R_terms), x = "", color = "Variable")+
                ggdark::dark_theme_bw()
            } else {
            ggplot()+
              geom_text(label = "text")+
              annotate("text", label = paste0("No QFQM data found: ", input$swft_qfqm_eddy4R_terms, "\nData during time period selected\n is not flagged"), x = 0, y = 0, color = "white", size = 12) +
              ggdark::dark_theme_bw()
            }
          }
          
          
        } else {
            ggplot()+
              geom_text(label = "text")+
              annotate("text", label = paste0("No QFQM data found: ", input$swft_qfqm_eddy4R_terms, "\nData during time period selected\n is not flagged"), x = 0, y = 0, color = "white", size = 12) +
              ggdark::dark_theme_bw()
        }
      } else {
            ggplot()+
              geom_text(label = "text")+
              annotate("text", label = paste0("No data found: ", input$swft_qfqm_eddy4R_terms, "\nTry increasing the date range"), x = 0, y = 0, color = "white", size = 12) +
              ggdark::dark_theme_bw()
      }
    }) # End of reactive
    
    # Debouncing Logic
    swft_qfqm_plot_reactive_d = swft_qfqm_plot_reactive %>% shiny::debounce(1000) # This is primarily to give the code a second to process while the user updates what variables they want to plot
    
    # Rendering Plot Logic
    output$swft_qfqm_plot = plotly::renderPlotly({
      swft_qfqm_plot_reactive_d()
    })
    
#     } # End of if tab is equal to this one
# # End of the line buster
# })