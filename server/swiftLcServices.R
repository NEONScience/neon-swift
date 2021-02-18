# Server Code for LC Services Tab

# R script to spread out server code into more well defined chunks
shiny::observeEvent(input$menu, {
  if(input$menu == "swft_lcservices_tab"){
    
    # Aesthetics
    ggplot2::theme_set(ggdark::dark_theme_gray()) 
    
    ######################## Start of LC Services Tab ######################## 

    # Filter Site Plot by Site Selected, LC Number, and date range
    swft.lcservices.user.data <- shiny::reactive({
      shiny::req(input$swft_lcservices_date_range, input$swft_lcservices_site)
      
      base::source(paste0(swft.server.folder.path, "R/read.eddy.inquiry.swift.R"))
      
      read.eddy.inquiry(dataType = "CnC", siteID = input$swft_lcservices_site, startDate = input$swft_lcservices_date_range[1], endDate = input$swft_lcservices_date_range[2]) %>%
        dplyr::filter(LCNumber == input$swft_lcservices_lc_number) %>%
        dplyr::mutate(`CnC Status` = as.integer(cnc)) %>%
        dplyr::mutate(`RTU Status` = as.integer(rtu)) %>%
        dplyr::mutate(`HornetQ Status` = as.integer(hornetq)) %>%
        dplyr::mutate(`CnC` = as.factor(cnc)) %>%
        dplyr::mutate(`RTU` = as.factor(rtu)) %>%
        dplyr::mutate(`HornetQ` = as.factor(hornetq)) 
    })
    
    #### Plot Data ####
    #### CnC ####
    
      cncPlot <- shiny::reactive({
        # Check if there is data, else make blank plot
        if(nrow(swft.lcservices.user.data()) != 0){
          ggplot2::ggplot(swft.lcservices.user.data(), ggplot2::aes(x = TimeStamp, y = `CnC Status`)) +
            ggplot2::geom_bar(stat = "identity", ggplot2::aes(fill = `CnC`)) +               # Create up/down bars for Cnc up/down time
            ggplot2::scale_fill_manual(values=c("1" = "limegreen","0" = "grey", "-1" = "red")) +   # Assign colors for down/up100%/up respectively
            ggplot2::scale_x_datetime(breaks = scales::pretty_breaks(n = 15), date_labels = "%m-%d\n%H:%M") +
            ggplot2::scale_y_continuous(breaks = c(1,0,-1), limits = c(-1,1)) +                                           # Set Breaks for Plot limited by potential values
            ggplot2::facet_wrap(~Name, ncol=1) +                                                        # using Facet for easy plot labeling
            ggplot2::theme(legend.position = "none") +                                                  # Remove Legend
            ggplot2::expand_limits(y = -1) +                                                            # Expand limits to show bottom of plot
            ggplot2::expand_limits(y =  1) +                                                            # Expand limits to show top of plot
            ggplot2::labs(x= "", y= "")                                                                 # Remove GenericAxis Labels
        } else {
          ggplot2::ggplot()+
            ggplot2::geom_text(label = "text")+
            ggplot2::annotate("text", label = paste0("NO CnC DATA: ",input$swft_lcservices_site ,"\n(No Data Within Date Range Specified)"), x = 0, y = 0, color = "black")+
            ggplot2::theme_minimal()
          }
      })
    
    output$CnCPlot <- plotly::renderPlotly({
      cncPlot()
    })
    
    #### RTU ####
    
    RTUPlot <- shiny::reactive({
      if(nrow(swft.lcservices.user.data()) != 0){
        ggplot2::ggplot(swft.lcservices.user.data(), ggplot2::aes(x=TimeStamp, y = `RTU Status`)) +
          ggplot2::geom_bar(stat = "identity", ggplot2::aes(fill = `RTU`)) +                   # Create up/down bars for Cnc up/down time
          ggplot2::scale_fill_manual(values=c("1" = "limegreen","0" = "grey", "-1" = "red")) +   # Assign colors for down/up100%/up respectively
          ggplot2::scale_x_datetime(breaks = scales::pretty_breaks(n = 15), date_labels = "%m-%d\n%H:%M") +
          ggplot2::scale_y_continuous(breaks = c(1,0,-1), limits = c(-1,1)) +                                               # Set Breaks for Plot limited by potential values
          ggplot2::facet_wrap(~Name, ncol=1) +                                                            # Using Facet for easy plot labeling
          ggplot2::theme(legend.position = "none") +                                                      # Remove Legend
          ggplot2::expand_limits(y = -1) +                                                                # Expand limits to show bottom of plot
          ggplot2::expand_limits(y =  1) +                                                                # Expand limits to show top of plot
          ggplot2::labs(x= "", y= "")                                                                     # Remove GenericAxis Labels 
      } else {
        ggplot2::ggplot()+
          ggplot2::geom_text(label = "text")+
          ggplot2::annotate("text", label = paste0("NO RTU DATA: ",input$swft_lcservices_site ,"\n(No Data Within Date Range Specified)"), x = 0, y = 0, color = "black")+
          ggplot2::theme_minimal()
      }
    })
    output$RTUPlot <- plotly::renderPlotly({
      RTUPlot()
    })
    
    #### HornetQ ####
    
    HornetQPlot <- shiny::reactive({
      if(nrow(swft.lcservices.user.data()) != 0){
        ggplot2::ggplot(swft.lcservices.user.data(), ggplot2::aes(x = TimeStamp, y = `HornetQ Status`)) +
          ggplot2::geom_bar(stat = "identity", ggplot2::aes(fill = `HornetQ`)) +           # Create up/down bars for Cnc up/down time
          ggplot2::scale_fill_manual(values=c("1" = "limegreen","0" = "grey", "-1" = "red")) +   # Assign colors for down/up100%/up respectively
          ggplot2::scale_x_datetime(breaks = scales::pretty_breaks(n = 15), date_labels = "%m-%d\n%H:%M") +
          ggplot2::scale_y_continuous(breaks = c(1,0,-1), limits = c(-1,1)) +                                               # Set Breaks for Plot limited by potential values
          ggplot2::facet_wrap(~Name, ncol=1) +                                                            # Using Facet for easy plot labeling
          ggplot2::theme(legend.position = "none") +                                                      # Remove Legend
          ggplot2::expand_limits(y = -1) +                                                                # Expand limits to show bottom of plot
          ggplot2::expand_limits(y =  1) +                                                                # Expand limits to show top of plot
          ggplot2::labs(x= "", y= "")                                                                     # Remove GenericAxis Labels 
      } else {
        ggplot2::ggplot()+
          ggplot2::geom_text(label = "text")+
          ggplot2::annotate("text", label = paste0("NO HornetQ DATA: ",input$swft_lcservices_site ,"\n(No Data Within Date Range Specified)"), x = 0, y = 0, color = "black")+
          ggplot2::theme_minimal()
      }
    })
    output$HornetQPlot <- plotly::renderPlotly({
      HornetQPlot()
    })
    
    
    ## Summary Panel Plots
    
    # Plot TIS CnC Summary
    CnCUptimePlot <- aws.s3::s3readRDS(object = "CnC/plots/swft.tis.cnc.plot.RDS", bucket = "research-eddy-inquiry")
    
    output$CnCUptimePlot <- plotly::renderPlotly({
      suppressWarnings(CnCUptimePlot)
    })

    # Plot TIS RTU Summary
    RTUUptimePlot <- aws.s3::s3readRDS(object = "CnC/plots/swft.tis.rtu.plot.RDS", bucket = "research-eddy-inquiry")
    output$RTUUptimePlot <- plotly::renderPlotly({
      suppressWarnings(RTUUptimePlot)
    })

    # Plot TIS HornetQ Summary
    HQUptimePlot <- aws.s3::s3readRDS(object = "CnC/plots/swft.tis.hornetq.plot.RDS", bucket = "research-eddy-inquiry")
    output$HornetQUptimePlot <- plotly::renderPlotly({
      suppressWarnings(HQUptimePlot)
    })

    # Plot AIS CnC Summary
    CnCUptimePlotAquatics <- aws.s3::s3readRDS(object = "CnC/plots/swft.ais.cnc.plot.RDS", bucket = "research-eddy-inquiry")
    output$CnCUptimePlotAquatics <- plotly::renderPlotly({
      suppressWarnings(CnCUptimePlotAquatics)
    })
  
    # Plot AIS RTU Summary
    RTUUptimePlotAquatics <- aws.s3::s3readRDS(object = "CnC/plots/swft.ais.rtu.plot.RDS", bucket = "research-eddy-inquiry")
    output$RTUUptimePlotAquatics <- plotly::renderPlotly({
      suppressWarnings(RTUUptimePlotAquatics)
    })
    
    # Summarize AIS HornetQ
    HQUptimePlotAquatics <- aws.s3::s3readRDS(object = "CnC/plots/swft.ais.hornetq.plot.RDS", bucket = "research-eddy-inquiry")
    output$HornetQUptimePlotAquatics <- plotly::renderPlotly({
      suppressWarnings(HQUptimePlotAquatics)
    })
  }
})