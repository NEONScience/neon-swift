shiny::observeEvent(input$menu, {
  if(input$menu == "swft_maintenance_tab"){
    
    swft_mntc_input_data = shiny::reactive({
      base::source("./R/check_pm.R")
      
      check_pm(site = input$swft_mntc_site_select) 
      
      # test = check_pm(site = "BART")
      # test$tower_comments %>% dplyr::filter(`PM Date` > Sys.Date()-14 %>% dplyr::arrange(dplyr::desc(`PM Date`))
    })
    
    # Value Boxes
    
    shiny::observeEvent(input$swft_mntc_site_select, {
    
      output$swft_mtnc_bout_freq_recent = shinydashboard::renderValueBox({
        
        shinydashboard::valueBox(
          value = swft_mntc_input_data()$meta_statistics$`2 Week Bout Frequency`,
          subtitle = paste0("Average PM Bouts performed every 2 weeks (past 2-months)"),
          width = 12,
          color = "green"
        )
        
      })
      
      output$swft_mtnc_bout_freq_all_time = shinydashboard::renderValueBox({
        
        shinydashboard::valueBox(
          value = swft_mntc_input_data()$meta_statistics$`2 Week Bout Frequency - All Time`,
          subtitle = paste0("Average PM Bouts performed every 2 weeks (All-Time"),
          width = 12,
          color = "green"
        )
        
      })
      
      output$swft_mtnc_last_bout = shinydashboard::renderValueBox({
        
        shinydashboard::valueBox(
          value = swft_mntc_input_data()$meta_statistics$`Last Bout`,
          subtitle = paste0("Last PM Bout:"),
          width = 12,
          color = "navy"
        )
        
      })
      
      output$swft_mtnc_total_bouts = shinydashboard::renderValueBox({
        
        shinydashboard::valueBox(
          value = swft_mntc_input_data()$meta_statistics$`Total Bouts`,
          subtitle = paste0("Total PM Bouts:"),
          width = 12,
          color = "red"
        )
        
      })
      
      output$swft_mtnc_first_bout = shinydashboard::renderValueBox({
        
        shinydashboard::valueBox(
          value = swft_mntc_input_data()$meta_statistics$`First Bout`,
          subtitle = paste0("First PM Bout:"),
          width = 12,
          color = "navy"
        )
        
      })
    
    })
    
    
    
    
    
    
    
    
    
    
    shiny::observeEvent(input$swft_mntc_data_select, {
      
      # Tower Maintenance
      
      if(input$swft_mntc_data_select == "Tower"){
      
        output$swft_mtnc_tower_pheno = DT::renderDataTable(
          swft_mntc_input_data()$ml_pheno_data      %>% dplyr::filter(`PM Date` > input$swft_mntc_date_select[1]) %>% dplyr::arrange(dplyr::desc(`PM Date`)), escape = FALSE, options = list(pageLength = 26, autoWidth = TRUE, dom = 't')
        )
        output$swft_mtnc_tower_aat = DT::renderDataTable(
          swft_mntc_input_data()$ml_aat_data        %>% dplyr::filter(`PM Date` > input$swft_mntc_date_select[1]) %>% dplyr::arrange(dplyr::desc(`PM Date`)), escape = FALSE, options = list(pageLength = 26, autoWidth = TRUE, dom = 't')
        )
        output$swft_mtnc_tower_wind = DT::renderDataTable(
          swft_mntc_input_data()$ml_wind_data       %>% dplyr::filter(`PM Date` > input$swft_mntc_date_select[1]) %>% dplyr::arrange(dplyr::desc(`PM Date`)), escape = FALSE, options = list(pageLength = 26, autoWidth = TRUE, dom = 't')
        )
        output$swft_mtnc_tower_ir = DT::renderDataTable(
          swft_mntc_input_data()$ml_ir_biotemp_data %>% dplyr::filter(`PM Date` > input$swft_mntc_date_select[1]) %>% dplyr::arrange(dplyr::desc(`PM Date`)), escape = FALSE, options = list(pageLength = 26, autoWidth = TRUE, dom = 't')
        )
        output$swft_mtnc_tower_par = DT::renderDataTable(
          swft_mntc_input_data()$ml_par_data        %>% dplyr::filter(`PM Date` > input$swft_mntc_date_select[1]) %>% dplyr::arrange(dplyr::desc(`PM Date`)), escape = TRUE, options = list(pageLength = 26, autoWidth = TRUE, dom = 't')
        )
        output$swft_mtnc_tt_sensors = DT::renderDataTable(
          swft_mntc_input_data()$tt_sensor_data        %>% dplyr::filter(`PM Date` > input$swft_mntc_date_select[1]) %>% dplyr::arrange(dplyr::desc(`PM Date`)), escape = TRUE, options = list(pageLength = 26, autoWidth = TRUE, dom = 't')
        )
        output$swft_mtnc_tower_comments = DT::renderDataTable(
          swft_mntc_input_data()$tower_comments     %>% dplyr::filter(`PM Date` > input$swft_mntc_date_select[1]) %>% dplyr::arrange(dplyr::desc(`PM Date`)), escape = TRUE, options = list(pageLength = 26, autoWidth = TRUE, dom = 't')
        )
        
      }
      
      # Soil Plot Maintenance
      
      if(input$swft_mntc_data_select == "Soil"){
        
        output$swft_mtnc_sp_co2 = DT::renderDataTable(
          swft_mntc_input_data()$sp_co2_data      %>% dplyr::filter(`PM Date` > input$swft_mntc_date_select[1]) %>% dplyr::arrange(dplyr::desc(`PM Date`)), escape = FALSE, options = list(pageLength = 26, autoWidth = TRUE, dom = 't')
        )
        output$swft_mtnc_sp_soil_heat_flux = DT::renderDataTable(
          swft_mntc_input_data()$sp_soil_heat_flux        %>% dplyr::filter(`PM Date` > input$swft_mntc_date_select[1]) %>% dplyr::arrange(dplyr::desc(`PM Date`)), escape = FALSE, options = list(pageLength = 26, autoWidth = TRUE, dom = 't')
        )
        output$swft_mtnc_sp_soil_qline = DT::renderDataTable(
          swft_mntc_input_data()$sp_soil_qline       %>% dplyr::filter(`PM Date` > input$swft_mntc_date_select[1]) %>% dplyr::arrange(dplyr::desc(`PM Date`)), escape = FALSE, options = list(pageLength = 26, autoWidth = TRUE, dom = 't')
        )
        output$swft_mtnc_sp_soil_temperature = DT::renderDataTable(
          swft_mntc_input_data()$sp_soil_temperature %>% dplyr::filter(`PM Date` > input$swft_mntc_date_select[1]) %>% dplyr::arrange(dplyr::desc(`PM Date`)), escape = FALSE, options = list(pageLength = 26, autoWidth = TRUE, dom = 't')
        )
        output$swft_mtnc_sp_soil_thrufall = DT::renderDataTable(
          swft_mntc_input_data()$sp_soil_thrufall        %>% dplyr::filter(`PM Date` > input$swft_mntc_date_select[1]) %>% dplyr::arrange(dplyr::desc(`PM Date`)), escape = TRUE, options = list(pageLength = 26, autoWidth = TRUE, dom = 't')
        )
        output$swft_mtnc_sp_soil_water_content = DT::renderDataTable(
          swft_mntc_input_data()$sp_soil_water_content     %>% dplyr::filter(`PM Date` > input$swft_mntc_date_select[1]) %>% dplyr::arrange(dplyr::desc(`PM Date`)), escape = TRUE, options = list(pageLength = 26, autoWidth = TRUE, dom = 't')
        )
        output$swft_mtnc_sp3_sensors = DT::renderDataTable(
          swft_mntc_input_data()$sp3_sensors     %>% dplyr::filter(`PM Date` > input$swft_mntc_date_select[1]) %>% dplyr::arrange(dplyr::desc(`PM Date`)), escape = TRUE, options = list(pageLength = 26, autoWidth = TRUE, dom = 't')
        )
        output$swft_mtnc_sp_comments = DT::renderDataTable(
          swft_mntc_input_data()$sp_comments     %>% dplyr::filter(`PM Date` > input$swft_mntc_date_select[1]) %>% dplyr::arrange(dplyr::desc(`PM Date`)), escape = TRUE, options = list(pageLength = 26, autoWidth = TRUE, dom = 't')
        )
        
      }
      
      # Eddy Co Maintenance
      
      if(input$swft_mntc_data_select == "Eddy"){
        
        output$swft_mtnc_eddy_gas_analyzer = DT::renderDataTable(
          swft_mntc_input_data()$eddy_gas_analyzer_data      %>% dplyr::filter(`PM Date` > input$swft_mntc_date_select[1]) %>% dplyr::arrange(dplyr::desc(`PM Date`)), escape = FALSE, options = list(pageLength = 26, autoWidth = TRUE, dom = 't')
        )
        output$swft_mtnc_eddy_tt = DT::renderDataTable(
          swft_mntc_input_data()$eddy_tt_data        %>% dplyr::filter(`PM Date` > input$swft_mntc_date_select[1]) %>% dplyr::arrange(dplyr::desc(`PM Date`)), escape = TRUE, options = list(pageLength = 26, autoWidth = TRUE, dom = 't')
        )
        output$swft_mtnc_eddy_inlet = DT::renderDataTable(
          swft_mntc_input_data()$eddy_inlet_data        %>% dplyr::filter(`PM Date` > input$swft_mntc_date_select[1]) %>% dplyr::arrange(dplyr::desc(`PM Date`)), escape = FALSE, options = list(pageLength = 26, autoWidth = TRUE, dom = 't')
        )
        output$swft_mtnc_eddy_pump = DT::renderDataTable(
          swft_mntc_input_data()$eddy_pump_data %>% dplyr::filter(`PM Date` > input$swft_mntc_date_select[1]) %>% dplyr::arrange(dplyr::desc(`PM Date`)), escape = FALSE, options = list(pageLength = 26, autoWidth = TRUE, dom = 't')
        )
        output$swft_mtnc_eddy_gas_cylinder = DT::renderDataTable(
          swft_mntc_input_data()$eddy_gas_cylinder_data     %>% dplyr::filter(`PM Date` > input$swft_mntc_date_select[1]) %>% dplyr::arrange(dplyr::desc(`PM Date`)), escape = TRUE, options = list(pageLength = 26, autoWidth = TRUE, dom = 't')
        )
        output$swft_mtnc_eddy_pducer = DT::renderDataTable(
          swft_mntc_input_data()$eddy_pducer_data       %>% dplyr::filter(`PM Date` > input$swft_mntc_date_select[1]) %>% dplyr::arrange(dplyr::desc(`PM Date`)), escape = FALSE, options = list(pageLength = 26, autoWidth = TRUE, dom = 't')
        )
        output$swft_mtnc_eddy_fep_tube = DT::renderDataTable(
          swft_mntc_input_data()$eddy_fep_tube_data     %>% dplyr::filter(`PM Date` > input$swft_mntc_date_select[1]) %>% dplyr::arrange(dplyr::desc(`PM Date`)), escape = TRUE, options = list(pageLength = 26, autoWidth = TRUE, dom = 't')
        )
        output$swft_mtnc_eddy_comments = DT::renderDataTable(
          swft_mntc_input_data()$eddy_comments     %>% dplyr::filter(`PM Date` > input$swft_mntc_date_select[1]) %>% dplyr::arrange(dplyr::desc(`PM Date`)), escape = TRUE, options = list(pageLength = 26, autoWidth = TRUE, dom = 't')
        )
        
      }
      
      # DFIR Maintenance
      
      if(input$swft_mntc_data_select == "DFIR"){
        
        output$swft_mtnc_dfir = DT::renderDataTable(
          swft_mntc_input_data()$dfir_data      %>% dplyr::filter(`PM Date` > input$swft_mntc_date_select[1]) %>% dplyr::arrange(dplyr::desc(`PM Date`)), escape = FALSE, options = list(pageLength = 26, autoWidth = TRUE, dom = 't')
        )
        
      }
      
      if(input$swft_mntc_data_select == "Comments"){
        
        output$swft_mtnc_comments = DT::renderDataTable(
          swft_mntc_input_data()$all_comments      %>% dplyr::filter(`PM Date` > input$swft_mntc_date_select[1]) %>% dplyr::arrange(dplyr::desc(`PM Date`)), escape = TRUE, options = list(pageLength = 26, autoWidth = TRUE, dom = 't', scrollX = TRUE)
        )
        
      }
      
      
      
      
      
    })


    
  
    
    
    
    
    
    
  }
})