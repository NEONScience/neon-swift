r.library = ("/home/NEON/kstyers/R/x86_64-redhat-linux-gnu-library/4.0/")

library(fst,   lib.loc = r.library)
library(shiny, lib.loc = r.library)
library(plyr, lib.loc = r.library)
library(vctrs, lib.loc = r.library)
library(glue, lib.loc = r.library)
library(tidyselect, lib.loc = r.library)
library(dplyr, lib.loc = r.library)
library(htmlwidgets, lib.loc = r.library)
library(plotly, lib.loc = r.library)
library(ggplot2, lib.loc = r.library)
library(DT, lib.loc = r.library)
library(tidyr, lib.loc = r.library)
library(data.table, lib.loc = r.library)
library(shinycssloaders, lib.loc = r.library)
library(shinydashboard, lib.loc = r.library)
library(viridis, lib.loc = r.library)
library(stringr, lib.loc = r.library)
library(scales, lib.loc = r.library)
library(aws.signature, lib.loc = r.library)
library(xml2, lib.loc = r.library)
library(aws.s3, lib.loc = r.library)
library(lubridate, lib.loc = r.library)

swft.server.folder.path = "/srv/shiny-server/neon-swift/"

# Essential Site Lookup Tables
swft.full.site.lookup <- data.table::fread(paste0(swft.server.folder.path, "data/lookup/swft.full.site.lookup.csv"))
swft.ais.site.lookup <-  data.table::fread(paste0(swft.server.folder.path, "data/lookup/swft.ais.site.lookup.csv"))
swft.tis.site.lookup <-  data.table::fread(paste0(swft.server.folder.path, "data/lookup/swft.tis.site.lookup.csv"))


# Define UI for application that draws a histogram
shiny::shinyUI(
  shinydashboard::dashboardPage(skin = "black",
    # Header
    shinydashboard::dashboardHeader(title = 'Swift',
                                    titleWidth = 170
                                    ),
    # Menu bar
    shinydashboard::dashboardSidebar(
      width = 150,
      shinydashboard::sidebarMenu( id = "menu",
        shinydashboard::menuItem("Home Page",       tabName = "swft_home_tab"                                                                  ),
        shinydashboard::menuItem("LC Services",     tabName = "swft_lcservices_tab", icon = shiny::icon("signal",         lib = "font-awesome")),
        shinydashboard::menuItem("Timestamp Check", tabName = "swft_timestamp_tab",  icon = shiny::icon("hourglass-half", lib = "font-awesome")),
        shinydashboard::menuItem("Gas Cylinders",   tabName = "swft_spangas_tab",    icon = shiny::icon("adjust",         lib = "font-awesome")),
        shinydashboard::menuItem("Cvals Fast",      tabName = "swft_cvalfast_tab",   icon = shiny::icon("atom",           lib = "font-awesome")),
        shinydashboard::menuItem("Eddy-Co Fast",    tabName = "swft_ecfast_tab",     icon = shiny::icon("sun",            lib = "font-awesome"))
      )
    ),
    # Body
    shinydashboard::dashboardBody(
      tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
      shinydashboard::tabItems(
        # ----------- Swift Tab ---------
        shinydashboard::tabItem(tabName = "swft_home_tab",
          shinydashboard::box(width = 12, 
              shiny::column(width = 7,
              shiny::h1("An Eddy-Covariance State of Health Dashboard (and Aquatics)"),
              # shiny::tags$h3(
              #   shiny::helpText(a("How to use this application!",href="http://den-devshiny-1.ci.neoninternal.org/TheAviary/pages/SwiftHowTo.html",target="_blank"))
              # ),
              shiny::h4("Users can use this application to plot data from all TIS sites to identify Eddy-Co issues, view trends, and verify calibrations/validations."),
              shiny::icon("signal", lib = "font-awesome"),
              shiny::tags$b("LC Sevices"),
              shiny::h4("Raw LC Service uptimes! This tab features an overview plot and site specific LC Service plots. You can use the LC Services tab to quickly identify sites with LC Service issues and investigate site specific LC Service outages."),
              shiny::icon("hourglass-half", lib = "font-awesome"),
              shiny::tags$b("Timestamp Check"),
              shiny::h4("Some IS sensors can have timestamp drift due to a variety of causes. Technicians/Engineers can use tab to see if there are any IS sensors that have large timestamp differentials (the difference between actual time and the sensor's time)."),
              shiny::icon("adjust", lib = "font-awesome"),
              shiny::tags$b("Gas Cylinders"),
              shiny::h4("Check current cylinder pressures, delivery pressures, and average pressure loss overtime. Also features a table showing expected cylinder depletion rates based upon average pressure loss."),
              shiny::icon("atom", lib = "font-awesome"),
              shiny::tags$b("Cvals Fast"),
              shiny::h4("This tab that uses `fst` files that renders all daily CVALs and visualizes all CVALs phase shifted by month!"),
              shiny::icon("sun", lib = "font-awesome"),
              shiny::tags$b("Eddy-Co Fast"),
              shiny::h4("A tab that uses `fst` data files to render Eddy-Co data; from Co2 measurements, CSAT3 wind data, and measurement level flows.")
            ),
            shiny::column(width = 4, offset = 1,
              shiny::img(src = 'swiftlogo.jpg', width = '300px', height = '240px')
            )
          ) # End Box
        ), # End home tabName
        
        ############################################                       LC Services                            ############################################
        
        shinydashboard::tabItem(tabName = "swft_lcservices_tab",
          shinydashboard::box(width = 12,
              shiny::column(width = 3,
                  shiny::selectInput(inputId = 'swft_lcservices_site',label = 'Select SiteID',
                    choices = as.vector(swft.full.site.lookup$SiteID),
                    selected = "HARV",
                    width='100%'
                  ),
                  shiny::dateRangeInput(inputId = "swft_lcservices_date_range", label = "Select Date Range for Plot [dev]",
                        start = Sys.Date()-14, end = Sys.Date()+2, max = Sys.Date()+2
                  ),
                  shiny::selectInput(inputId = "swft_lcservices_lc_number", label = "Select LC Number",
                                     choices = c(1,2)
                  ),
                  shiny::radioButtons("swft_lcservices_radio_lcnumber","Select LC Service",
                                     choices = list("CnC" = "cnc", "RTU" = "rtu", "HornetQ" = "hornetq"), inline = TRUE
                  ),
                  shiny::p("LC Services Data is collected hourly and displays CnC, RTU, and HornetQ data.")
              ), # End Column
              shiny::column(width = 9,
                            shiny::fluidRow(
                            shiny::radioButtons("swft_lcservices_radio_overall", "Select IS System",
                                              choices = list("TIS" = 1, "AIS" = 2),selected = 1, inline = TRUE),
                            shiny::conditionalPanel(condition =  "input.swft_lcservices_radio_lcnumber == 'cnc'",
                                shiny::conditionalPanel(condition =  "input.swft_lcservices_radio_overall == 1",                                              
                                  plotly::plotlyOutput("CnCUptimePlot") %>% shinycssloaders::withSpinner(color="#012D74",type="8",color.background = "white"),
                                ),
                                shiny::conditionalPanel(condition =  "input.swft_lcservices_radio_overall == 2",
                                  plotly::plotlyOutput("CnCUptimePlotAquatics") %>% shinycssloaders::withSpinner(color="#012D74",type="8",color.background = "white"),
                                )
                              ),
                              shiny::conditionalPanel(condition =  "input.swft_lcservices_radio_lcnumber == 'rtu'",
                                shiny::conditionalPanel(condition =  "input.swft_lcservices_radio_overall == 1",                                              
                                  plotly::plotlyOutput("RTUUptimePlot") %>% shinycssloaders::withSpinner(color="#012D74",type="8",color.background = "white")
                                ),
                                shiny::conditionalPanel(condition =  "input.swft_lcservices_radio_overall == 2",
                                  plotly::plotlyOutput("RTUUptimePlotAquatics") %>% shinycssloaders::withSpinner(color="#012D74",type="8",color.background = "white")
                                )
                              ),
                              shiny::conditionalPanel(condition =  "input.swft_lcservices_radio_lcnumber == 'hornetq'",
                                shiny::conditionalPanel(condition =  "input.swft_lcservices_radio_overall == 1",                                              
                                  plotly::plotlyOutput("HornetQUptimePlot") %>% shinycssloaders::withSpinner(color="#012D74",type="8",color.background = "white")
                                ),
                                shiny::conditionalPanel(condition =  "input.swft_lcservices_radio_overall == 2",
                                  plotly::plotlyOutput("HornetQUptimePlotAquatics") %>% shinycssloaders::withSpinner(color="#012D74",type="8",color.background = "white")
                                )
                              )
                            )
                            ),
              shinydashboard::tabBox(width = 12,
                shiny::tabPanel("Site Plot",width=12,
                  shiny::fluidRow(width = "100%",
                    shiny::fluidRow(
                      shiny::conditionalPanel(condition =  "input.swft_lcservices_radio_lcnumber == 'cnc'",
                        plotly::plotlyOutput("CnCPlot", height = "600px") %>% shinycssloaders::withSpinner(color="#012D74",type="8",color.background = "white")
                      ), # End Conditional Panel
                      shiny::conditionalPanel(condition =  "input.swft_lcservices_radio_lcnumber == 'rtu'",
                        plotly::plotlyOutput("RTUPlot", height = "600px") %>% shinycssloaders::withSpinner(color="#012D74",type="8",color.background = "white")
                      ), # End Conditional Panel
                      shiny::conditionalPanel(condition =  "input.swft_lcservices_radio_lcnumber == 'hornetq'",
                        plotly::plotlyOutput("HornetQPlot", height = "600px") %>% shinycssloaders::withSpinner(color="#012D74",type="8",color.background = "white")
                      ) # End Conditional Panel
                    )
                  )
                ) # End tabBox
              ) # End tabPanel
          ) # End Cval Fst box
        ), # End Cval Fst
        
        ############################################                       Timestamp Checker                      ############################################
  
        shinydashboard::tabItem(tabName = "swft_timestamp_tab",
          shinydashboard::box(width = 12,
            shiny::column(width = 4,
              shiny::fluidRow(
                shiny::h1("Timestamp Checker"),
                shiny::br(),
                shiny::p("This tool checks the timestamp difference between 'Actual Time' and the LC's Timestamp for a sensor."),
                shiny::p("Difference between actual UTC and the sensor's timestamp must be smaller than 10 seconds to appear in this reports.")
              )
            ), # End Column 7
            shiny::column(width = 1),
            shiny::column(width = 7,
              shiny::fluidRow(
                shinydashboard::valueBoxOutput("swft_timestamp_last_update_box", width = 6),
                shiny::br(),
                shiny::br()
              )
            ),
            shinydashboard::box(width = 12,
              shiny::fluidRow(width = "100%",
                shiny::fluidRow(
                  shiny::plotOutput("swft_timestamp_plot") %>% shinycssloaders::withSpinner(color="#012D74",type="8",color.background = "white"),
                  DT::dataTableOutput("swft_timestamp_table") %>% shinycssloaders::withSpinner(color="#012D74",type="8",color.background = "white")
                ) # End fluidRow
              ) # End fluidRow
            ) # End box
          ) # End Cval Fst box
        ), # End Cval Fst
        
        ############################################                       Span Gas Cylinder Plotter                      ############################################
        
        shinydashboard::tabItem(tabName = "swft_spangas_tab",
          shinydashboard::box(width = 12,
            shiny::fluidRow(
              shiny::column(width = 2,
                shiny::selectizeInput(inputId = 'swft_spangas_site',label = 'Select SiteID',
                                   choices = swft.tis.site.lookup$SiteID, multiple = TRUE, options = list(maxItems = 3),
                                   selected = c("HARV"),
                                   width='100%'
                )
              ),
              shiny::column(width = 3,    
                  shiny::dateRangeInput(inputId = "swft_spangas_date_range", label = "Select Date Range for Plot",
                                        start = Sys.Date() - 14, end = Sys.Date() + 1, max = Sys.Date() + 1
                  )
              )
            ), 
            # ), # End Column 7
            shinydashboard::tabBox(width = 12,
              shiny::tabPanel("Total Pressure",width=12,
                shiny::fluidRow(width = "100%",
                  plotly::plotlyOutput("swft_spangas_overall_plot", height = "600px") %>% shinycssloaders::withSpinner(color="#012D74",type="8",color.background = "white")
                ) # End fluidRow 
              ), # End tabPanel
              shiny::tabPanel("Delivery Pressure",width=12,
                shiny::p("Ideal delivery pressure is 11.6 PSI, but an acceptable range is between 9.5 and 13 PSI."),
                shiny::fluidRow(width = "100%",
                  plotly::plotlyOutput("swft_spangas_delivery_plot", height = "600px") %>% shinycssloaders::withSpinner(color="#012D74",type="8",color.background = "white")
                ) # End fluidRow 
              ), # End tabPanel
              shiny::tabPanel("Avg Pressure Loss",width=12,
                shiny::p("Ideal pressure loss in a fully functional Cval system is ~ 4 PSI."),
                shiny::fluidRow(width = "100%",
                  plotly::plotlyOutput("swft_spangas_loss_plot", height = "600px") %>% shinycssloaders::withSpinner(color="#012D74",type="8",color.background = "white")
                ) # End fluidRow
              ), # End tabPanel
              shiny::tabPanel("Covid-19 Cylinder Data",width=12,
                shiny::fluidRow(width = "100%",
                  DT::dataTableOutput("swft.covid19.table") %>% shinycssloaders::withSpinner(color="#012D74",type="8",color.background = "white")
                ) # End fluidRow 
              ) # End tabPanel
            ) # End tabBox
          ) # End Cval Fst box
        ), # End Cval Fst
        
        ############################################                            Cval Fast                         ############################################
        
        shinydashboard::tabItem(tabName = "swft_cvalfast_tab",
          shinydashboard::box(width = 12,
              shiny::column(width = 7,
                shiny::fluidRow(
                  shiny::h1("CVAL Fast Plots [Dev]"),
                  shiny::br(),
                  shiny::p("Calibration data is collected daily via a SQL query to the L0 database. The start and end times are determined by an LC file. "),
                  shiny::p("Use the data here to verify Cvals are occuring as expected. Check that span gases are being delivered properly. Zero*, Low, Int, High")
                ),
                  shiny::fluidRow(
                    shiny::column(width = 4,
                      shiny::selectizeInput(inputId = "swft_cval_site", multiple = FALSE,
                                            label = "Select Site",
                                            choices = swft.tis.site.lookup$SiteID,
                                            selected = "HARV"),
                      shiny::selectInput(inputId = "swft_cval_sensor",
                                         label = "Select Cal/Val to Render",
                                         choices = c("G2131 Validations" = "G2131i","Li840 CVALs" = "Li840A", "Li7200 Validations" = "Li7200")),
                      shiny::uiOutput('swft_cval_react_unique_cvals')
                    ),
                    shiny::column(width = 4,       
                      shiny::fluidRow(
                        shiny::tags$ol(
                          shiny::tags$li("Check that span gases are being delivered properly. Zero*, Low, Int, High"),
                          shiny::tags$li("Verify Cval's are occuring regularly. Daily for validations and weekly for Li840 calibrations."),
                          shiny::tags$li("Verify span gases are installed in the correct maximo location. Low, Int, High are all occuring in the correct order and have the correct span concentrations.")
                        ) # End OrderList
                      )                              
                    ),
                    shiny::column(width = 4) # End Column
                  ) # End fluidRow
              ) # End Top Master column
          ), # End tabBox
          shinydashboard::box(width = 12,
            shiny::column(width = 12,
              shiny::tabPanel("",width=12,
                shiny::fluidRow(
                  shiny::column(width =12,
                    # Co2 Analyzers Data
                    shiny::fluidRow(
                      shinydashboard::valueBoxOutput("swiftEcseLow", width = 4),shinydashboard::valueBoxOutput("swiftEcseInt", width = 4),shinydashboard::valueBoxOutput("swiftEcseHigh", width = 4)
                    ),
                    shiny::fluidRow(
                      plotly::plotlyOutput("plot_co2_ecse", height = "600px") %>% shinycssloaders::withSpinner(color="#012D74",type="8",color.background = "white"),
                      DT::dataTableOutput("table_co2_ecse") %>% shinycssloaders::withSpinner(color="#012D74",type="8",color.background = "white"),
                      DT::dataTableOutput("table_ecse_span") %>% shinycssloaders::withSpinner(color="#012D74",type="8",color.background = "white")
                    )
                  ) # End column
                ) # End Cval Fast shiny::column for Conditional Panels
              ) # End Cval Fst shiny::fluidRow for Condtional panels
            ) # End Cval Fst Tab Panel for Condtional panels
          ) # End Cval Fst box
        ), # End Cval Fst
        
        ############################################                            EC Fast                           ############################################
        
        shinydashboard::tabItem(tabName = "swft_ecfast_tab",
          shinydashboard::box(width = 12,
            shiny::column(width = 12,
              shiny::tabPanel("", width = 12,
                shiny::fluidRow(
                  shiny::fluidRow(
                    shiny::column(width = 4,
                      shiny::h1("Eddy Covariance Plots"),
                      shiny::br(),
                      shiny::p("Data is collected from an automated Presto pulled designed by IS Science and CI. Data is updated in the early morning (10:00:00 UTC)."),
                      shiny::p("Daily sensor files are then stored at an S3 bucket, and retrieved by a IS Science designed function. All data are 2-minute point data, meaning that what ever the values was at the 2 minute interval, is the value that is stored."),
                      shiny::a("Open the Eddy-Co Architecture Map",target="_blank",href="EC_ArchMap.pdf"),
                      shiny::br(),
                      shiny::br(),
                      shiny::br()
                    ),
                    shiny::column(width = 4),
                    shiny::column(width = 4,
                      shinydashboard::valueBoxOutput("swft_ec_fast_collect_data_time", width = 12),
                      shinydashboard::valueBoxOutput("swft_ec_fast_data_points", width = 12),
                    )
                  ),
                  shiny::column(width = 3,
                    shiny::fluidRow(
                      shiny::dateRangeInput(inputId = "swft_EddyCo_date_range", label = "Select a Date Range",
                                            min = "2017-01-01",
                                            max = Sys.Date(),
                                            start = Sys.Date()-7,
                                            end = Sys.Date()
                      )
                    )
                  ),
                  shiny::column(width = 3,
                    shiny::fluidRow(
                      shiny::selectizeInput(inputId = "swft_EddyCo_site", multiple = FALSE,
                                        label = "Select Site",
                                        choices = swft.tis.site.lookup$SiteID,
                                        selected = sample(swft.tis.site.lookup$SiteID, 1)
                      )
                    )
                  ),
                  shiny::column(width = 3,
                    shiny::fluidRow(
                      shiny::selectInput(inputId = "swft_EddyCo_data_type",
                                         label = "Select Data",
                                         choice = c("ECSE Isotopic Analyzer - G2131i"        = "G2131",
                                                    "ECSE Isotopic Analyzer - L2130i"        = "L2130",
                                                    "ECSE CO2/H2O Analyzer - Li840A"         = "Li840",
                                                    "ECTE CO2/H2O Analyzer - Li7200"         = "Li7200",
                                                    "All CO2"                                = "CO2",
                                                    "All H2O"                                = "H2O",
                                                    "ECTE 3D Wind Sensor - CSAT3"            = "CSAT3",
                                                    "ECTE Roll/Pitch/Azimuth Sensor - AMRS"  = "amrs",
                                                    "ECTE Relative Humidity Sensor - HMP155" = "HMP155",
                                                    "ECSE ML Flow Rate - MFM"                = "ecse.mfm",
                                                    "ECSE Pump Voltages - NEON Pump"         = "ecse.voltage",
                                                    "EC Temperatures - Comet/Others"         = "ec.temps"
                                                    ),
                                         selected = "G2131"
                      )
                    )
                  ),
                  shiny::column(width = 3,
                    shiny::fluidRow(
                      shiny::conditionalPanel(condition = "input.swft_EddyCo_data_type == 'G2131'",
                        shiny::selectInput(inputId = 'swft_EddyCo_sub_data_type_G2131', label = 'Sub Data Type', choices = c("CO2", "Isotopes", "Sample Valves"))
                      ),
                      shiny::conditionalPanel(condition = "input.swft_EddyCo_data_type == 'Li840'",
                        shiny::selectInput(inputId = 'swft_EddyCo_sub_data_type_Li840', label = 'Sub Data Type', choices = c("CO2", "H2O", "Sample Valves", "Flow Rate"))
                      ),
                      shiny::conditionalPanel(condition = "input.swft_EddyCo_data_type == 'L2130'",
                        shiny::selectInput(inputId = 'swft_EddyCo_sub_data_type_L2130', label = 'Sub Data Type', choices = c("Isotope - 2H", "Isotope - 18O", "H2O", "Sample Valves"))
                      ),
                      shiny::conditionalPanel(condition = "input.swft_EddyCo_data_type == 'Li7200'",
                        shiny::selectInput(inputId = 'swft_EddyCo_sub_data_type_Li7200', label = 'Sub Data Type', choices = c("CO2", "H2O", "Flow", "Signal Strength", "Cell Temp", "Pressure Differential", "Diagnostic" ))
                      ),
                      shiny::conditionalPanel(condition = "input.swft_EddyCo_data_type == 'HMP155'",
                        shiny::selectInput(inputId = 'swft_EddyCo_sub_data_type_HMP155', label = 'Sub Data Type', choices = c("Relative Humidity", "Temperature", "Dew Point"))
                      )
                    )
                  )
                ),
                shiny::fluidRow(
                  shiny::column(width = 5,
                    shiny::column(width = 1,
                      shiny::radioButtons(inputId = "swft_EddyCo_radioButton", label = "Advanced Options",choices = c("Enabled", "Disabled"), selected = "Disabled")
                    ),
                    shiny::column(width = 1),
                    shiny::column(width = 8,
                      shiny::conditionalPanel(condition = "input.swft_EddyCo_radioButton == 'Enabled' ",
                                              
                        shiny::column(width = 3,
                          shiny::selectInput(inputId = "swft_EddyCo_radio_y_custom", label = "Customize Y-axis", choices = c("Yes", "No"), selected = "No")
                        ),
                        shiny::conditionalPanel(condition = "input.swft_EddyCo_radio_y_custom == 'Yes'",
                          shiny::column(width = 3,
                            shiny::numericInput(inputId = "swft_EddyCo_y_lower",label = "Min Y", value = "")
                          ),
                          shiny::column(width = 3,
                            shiny::numericInput(inputId = "swft_EddyCo_y_upper",label = "Max Y", value = 500)
                          )
                        ),
                        shiny::column(width = 3,
                          shiny::selectInput(inputId = "swft_EddyCo_facet_wrap", label = "Facet by stream name?", choices = c("Yes", "No"), selected = "No")
                        )
                      )
                    )
                  ),
                  shiny::column(width = 3,
                    shiny::actionButton(inputId = "swft_ec_fast_actionButton", label = "  Start Plotting!", icon = icon(name = "play-circle"))
                  ),
                  shiny::column(width = 4)
                )
              ) # End Top Full Column
            )
          ), # End Top container Column
          shinydashboard::box(width = 12,
            shiny::column(width = 12,
              shiny::tabPanel("",width=12,
                shiny::fluidRow(
                  shiny::column(width =12,
                    # EC fast plot
                    shiny::plotOutput("swft_ec_fast_plot", height = "600px") %>% shinycssloaders::withSpinner(color="#012D74",type="8",color.background = "white"),
                    DT::dataTableOutput("swft_ec_fast_table") %>% shinycssloaders::withSpinner(color="#012D74",type="8",color.background = "white")
                  ) # End EC Fast shiny::column for Conditional Panels
                ) # End EC Fst shiny::fluidRow for Condtional panels
              ) # End blank tabPanel
            ) # End EC Fst Tab Panel for Condtional panels
          ) # End EC Fst box
        ) # End EC Fst
      ) # End Tab Items 
    ) # End Dashboard Body
  ) # End of Page
) # End of UI