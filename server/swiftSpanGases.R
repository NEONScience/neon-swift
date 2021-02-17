# Server Code for LC Services Tab

# R script to spread out server code into more well defined chunks
shiny::observeEvent(input$menu, {
  if(input$menu == "swft_spangas_tab"){

    ### Aesthetics ######
    ggplot2::theme_set(theme_dark())
    # Specify Colors and Linetypes for Overall Pressure Gas Plots
    swft.spangas.linefills <- c("ECTE High"     = "blue",     "ECTE Zero"     = "cyan", 
                                "ECTE Int"      = "red",      "ECSE High"     = "#9400D3",
                                "ECSE Int"      = "#00cc00",  "ECSE Low"      = "grey",
                                "ECSE H2O Zero" = "cyan",     "ECSE CO2 Zero" = "cyan",
                                "ECTE Low"      = "gold",     "ECSE Archive"  = "#8B4513", 
                                "ECTE Archive"  = "#8B4513")
    
    swft.spangas.linetypes <- c("ECTE High"     = "solid",  "ECTE Zero"     = "solid", 
                                "ECTE Int"      = "solid",  "ECSE High"     = "solid",
                                "ECSE Int"      = "solid",  "ECSE Low"      = "solid",
                                "ECSE H2O Zero" = "dotted", "ECSE CO2 Zero" = "dashed",
                                "ECTE Low"      = "solid",  "ECSE Archive"  = "dashed", 
                                "ECTE Archive"  = "solid")

    # Specify Colors and Linetypes for Delivery Pressure Gas Plots
    swft.spangas.linefillsd <- c("ECTE High Delivery"     = "blue",    "ECTE Zero Delivery"     = "cyan", 
                                 "ECTE Int Delivery"      = "red",     "ECSE High Delivery"     = "#9400D3",
                                 "ECSE Int Delivery"      = "#00cc00", "ECSE Low Delivery"      = "grey",
                                 "ECSE H2O Zero Delivery" = "cyan",    "ECSE CO2 Zero Delivery" = "cyan",
                                 "ECTE Low Delivery"      = "gold",    "ECSE Archive Delivery"  = "#8B4513", 
                                 "ECTE Archive Delivery"  = "#8B4513")
    
    swft.spangas.linetypesd <- c("ECTE High Delivery"     = "solid",  "ECTE Zero Delivery"     = "solid", 
                                 "ECTE Int Delivery"      = "solid",  "ECSE High Delivery"     = "solid",
                                 "ECSE Int Delivery"      = "solid",  "ECSE Low Delivery"      = "solid",
                                 "ECSE H2O Zero Delivery" = "dotted", "ECSE CO2 Zero Delivery" = "dashed",
                                 "ECTE Low Delivery"      = "solid",  "ECSE Archive Delivery"  = "dashed", 
                                 "ECTE Archive Delivery"  = "solid")

    ###### Initializing data selected by user ######

    # Read in all Gas Cylinder Data
    # swft.spangas.overall.in <- fst::read.fst(paste0(swft.server.folder.path, "data/spanGas/swft.span.gas.master.fst"))
    # swft.spangas.differential.in <- fst::read.fst(paste0(swft.server.folder.path, "data/spanGas/swft.span.gas.differntial.master.fst"))
    
    swft.spangas.overall.in <- aws.s3::s3read_using(FUN = fst::read.fst, bucket = "research-eddy-inquiry", object = "spanGas/master/swft.span.gas.master.fst")
    swft.spangas.differential.in <- aws.s3::s3read_using(FUN = fst::read.fst, bucket = "research-eddy-inquiry", object = "spanGas/master/swft.span.gas.differntial.master.fst")
    
    
    
    swft_spangas_overall_plot <- shiny::reactive({
      shiny::req(input$swft_spangas_site)
      
      message(paste0("Span Gas - ", input$swft_spangas_site, " from ", input$swft_spangas_date_range[1], " to ", input$swft_spangas_date_range[2], "\n"))

      if(is.null(input$swft_spangas_site) == FALSE){
      
        swft.spangas.overall.out = swft.spangas.overall.in %>%
          dplyr::filter(SiteID %in% input$swft_spangas_site) %>%
          dplyr::filter(date >= input$swft_spangas_date_range[1]
                      & date <= input$swft_spangas_date_range[2]) %>%
          dplyr::filter(cylType == "Overall Pressure")
         
        if(nrow(swft.spangas.overall.out) > 0){
          ggplot2::ggplot(swft.spangas.overall.out, ggplot2::aes( color = Cylinder, linetype = Cylinder)) +
            ggplot2::geom_line( size = 1, ggplot2::aes(x = date, y = meanVal)) +                                          # Make Lines
            ggplot2::geom_point(size = 1, ggplot2::aes(x = date, y = meanVal)) +
            ggplot2::scale_y_continuous(breaks = c(0,250,400,800,1000,1250,1500,1750,2000,2100), sec.axis = dup_axis(name = "")) +
            ggplot2::scale_x_date(breaks = scales::pretty_breaks(n = 20), date_labels = "%Y\n%b-%d") +
            ggplot2::labs(x= "", y= "Cylinder Pressure (PSI)", color = "Cylinder Type", linetype="")+                                    # Labels to make plot legible
            ggplot2::geom_hline(aes(yintercept = 800),linetype = 3, size = 1.1, color = "red") +                 # Upper limit for Cylinder Pressure
            ggplot2::geom_hline(aes(yintercept = 400), linetype = 3, size = 1.1, color = "green") +              # Lower Limit for Cylinder Pressure
            ggplot2::scale_color_manual(values = swft.spangas.linefills) +                                       # Change line color to cylinder color
            ggplot2::scale_linetype_manual(values = swft.spangas.linetypes) +   
            ggplot2::theme(text = ggplot2::element_text(size = 16))+
            ggplot2::facet_wrap(~SiteID)
        } else {
          ggplot2::ggplot()+
            ggplot2::geom_text(label = "text")+
            ggplot2::annotate("text", label = paste0("NO DATA: \n(No Data Within Date Range Specified)"), x = 0, y = 0, color = "black")+
            ggplot2::theme_minimal()
        } 
      } else {
        ggplot2::ggplot()+
          ggplot2::geom_text(label = "text")+
          ggplot2::annotate("text", label = paste0("NO DATA: \n(No Data Within Date Range Specified)"), x = 0, y = 0, color = "black")+
          ggplot2::theme_minimal()
      }
    })
    
    swft_spangas_overall_plot_d <- shiny::debounce(swft_spangas_overall_plot, 1000)
    # Output the data
    output$swft_spangas_overall_plot <- plotly::renderPlotly({
      swft_spangas_overall_plot_d()
    })
    
    swft_spangas_delivery_plot <- shiny::reactive({
      
      if(is.null(input$swft_spangas_site) == FALSE){
      
        swft.spangas.overall.out = swft.spangas.overall.in %>%
          dplyr::filter(SiteID %in% input$swft_spangas_site) %>%
          dplyr::filter(date >= input$swft_spangas_date_range[1]
                        & date <= input$swft_spangas_date_range[2]) %>%
          dplyr::filter(cylType == "Delivery Pressure")
        
        if(nrow(swft.spangas.overall.out) > 0){
          ggplot2::ggplot(swft.spangas.overall.out, ggplot2::aes(color = Cylinder, linetype = Cylinder)) +
            ggplot2::geom_line( size = 1,  ggplot2::aes(x = date, y = meanVal)) +                                            # Make Lines
            ggplot2::geom_point(size = 1,  ggplot2::aes(x = date, y = meanVal)) +
            ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 12), sec.axis = dup_axis(name = "")) +
            ggplot2::scale_x_date(breaks = scales::pretty_breaks(n = 20), date_labels = "%Y\n%b-%d") +
            ggplot2::labs(x= "", y="Cylinder Pressure Loss (PSI)", color = "Cylinder Type", linetype="") +                                     # Labels to make plot legible
            ggplot2::geom_hline(aes(yintercept = 13),linetype = 3, size = 1.1, color = "firebrick") +              # Upper limit for Cylinder Pressure
            ggplot2::geom_hline(aes(yintercept = 9), linetype = 3, size = 1.1, color = "firebrick") +              # Lower Limit for Cylinder Pressure
            ggplot2::scale_color_manual(values = swft.spangas.linefillsd) +                                        # Change line color to cylinder color
            ggplot2::scale_linetype_manual(values = swft.spangas.linetypesd) +                                     # Change line type based upon Zero or assayed
            ggplot2::theme(text = ggplot2::element_text(size = 16))+
            ggplot2::facet_wrap(~SiteID)
        } else {
          ggplot2::ggplot()+
            ggplot2::geom_text(label = "text")+
            ggplot2::annotate("text", label = paste0("NO DATA: \n(No Data Within Date Range Specified)"), x = 0, y = 0, color = "black")+
            ggplot2::theme_minimal()
        }
      } else {
        ggplot2::ggplot()+
          ggplot2::geom_text(label = "text")+
          ggplot2::annotate("text", label = paste0("NO DATA: \n(No Data Within Date Range Specified)"), x = 0, y = 0, color = "black")+
          ggplot2::theme_minimal()
      }
    })
    
    swft_spangas_delivery_plot_d <- shiny::debounce(swft_spangas_delivery_plot, 1000)
    # Output the data
    output$swft_spangas_delivery_plot <- plotly::renderPlotly({
      swft_spangas_delivery_plot()
    })
    
    swft_spangas_loss_plot <- shiny::reactive({
      
      if(is.null(input$swft_spangas_site) == FALSE){
        swft.spangas.loss.out = swft.spangas.differential.in %>%
          dplyr::filter(SiteID %in% input$swft_spangas_site) %>%
          dplyr::group_by(SiteID,Cylinder) %>%
          dplyr::mutate(Average_Pressure_Loss = sum(meanDifferential,na.rm = TRUE)/(as.numeric(difftime(max(date,na.rm = TRUE),min(date, na.rm = TRUE))))) %>% 
          dplyr::filter(date == max(date))
  
        if(nrow(swft.spangas.loss.out) > 0){
          swft.spangas.loss.plot = ggplot2::ggplot(data = swft.spangas.loss.out, 
            ggplot2::aes(x = Cylinder, y = Average_Pressure_Loss, text = paste0(Cylinder, " is losing: ", round(-Average_Pressure_Loss, 2), " PSI Per Day"))) +
            ggplot2::geom_bar(aes(color = Cylinder, fill = Cylinder), stat = "identity") +                                                # Make bars
            ggplot2::scale_fill_manual(values= swft.spangas.linefills, aesthetics = "color") +                                            # Make color same as cylinder color
            ggplot2::scale_fill_manual(values= swft.spangas.linefills, aesthetics = "fill") +                                             # Make fill same as cylinder color
            ggplot2::geom_hline(yintercept = -4, linetype = "dashed") +
            ggplot2::labs(x= "",  y="PSI", color = "Cylinder Type", fill = "", linetype = "",
                          title = paste0("Cylinder Pressure Loss from: ", input$swft_spangas_date_range[1], " to ", input$swft_spangas_date_range[2])) +    
            ggplot2::facet_grid(~SiteID) +                                                                                                # Facet by site ID
            ggplot2::theme(axis.text.x = element_text(angle = 270, size = 12))                                       # Change axis text to Battelle Blue
          plotly::ggplotly(swft.spangas.loss.plot, tooltip = c("text")) %>% plotly::config(displayModeBar = F)                                  # Specify Tool tips and remove display bar
        } else {
          ggplot2::ggplot()+
            ggplot2::geom_text(label = "text")+
            ggplot2::annotate("text", label = "NO DATA\n(Input plot terms above)", x = 0, y = 0, color = "black")+
            ggplot2::theme_minimal()
        }
      } else {
        ggplot2::ggplot()+
          ggplot2::geom_text(label = "text")+
          ggplot2::annotate("text", label = paste0("NO DATA: \n(No Data Within Date Range Specified)"), x = 0, y = 0, color = "black")+
          ggplot2::theme_minimal()
      }
    }) 
    
    swft_spangas_loss_plot_d <- shiny::debounce(swft_spangas_loss_plot, 1000)
    # Output the data
    output$swft_spangas_loss_plot <- plotly::renderPlotly({
      swft_spangas_loss_plot_d()
    })

    ########################################################## COVID 19 Table  ########################################################################### 
   
     swft.covid19.table <- base::readRDS(paste0(swft.server.folder.path, "data/plots/avgLossTable.RDS"))
     output$swft.covid19.table <- DT::renderDT({
       DT::datatable(swft.covid19.table,escape = FALSE,filter='top', options = list(pageLength = 10, autoWidth = FALSE))
     })
    
  } # End If statement
}) # End observeEvent