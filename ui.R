library(shiny)
library(plyr)
library(vctrs)
library(glue)
library(tidyselect)
library(dplyr)
library(htmlwidgets)
library(plotly)
library(ggplot2)
library(DT)
library(tidyr)
library(data.table)
library(shinycssloaders)
library(shinydashboard)
library(viridis)
library(stringr)
library(scales)
library(aws.signature)
library(xml2)
library(aws.s3)
library(lubridate)
library(dashboardthemes)
library(ggdark)

swft.server.folder.path = "./"

# Essential Site Lookup Tables
swft.full.site.lookup <- data.table::fread(paste0(swft.server.folder.path, "data/lookup/swft.full.site.lookup.csv"))
swft.ais.site.lookup <-  data.table::fread(paste0(swft.server.folder.path, "data/lookup/swft.ais.site.lookup.csv"))
swft.tis.site.lookup <-  data.table::fread(paste0(swft.server.folder.path, "data/lookup/swft.tis.site.lookup.csv"))


# Define UI for application that draws a histogram
shiny::shinyUI(
  shinydashboard::dashboardPage(skin = "black",
    # Header
    shinydashboard::dashboardHeader(title = 'Swift',
                                    titleWidth = 200
                                    ),
    # Menu bar
    shinydashboard::dashboardSidebar(
      width = 150,
      shinydashboard::sidebarMenu(id = "menu",
        shinydashboard::menuItem("Home Page",        tabName = "swft_home_tab"                                                                  ),
        shinydashboard::menuItem("LC Services",      tabName = "swft_lcservices_tab", icon = shiny::icon("signal",         lib = "font-awesome")),
        shinydashboard::menuItem("LC Time Check",    tabName = "swft_timestamp_tab",  icon = shiny::icon("hourglass-half", lib = "font-awesome")),
        shinydashboard::menuItem("Gas Cylinders",    tabName = "swft_spangas_tab",    icon = shiny::icon("adjust",         lib = "font-awesome")),
        shinydashboard::menuItem("CVAL Plotting",    tabName = "swft_cvalfast_tab",   icon = shiny::icon("atom",           lib = "font-awesome")),
        shinydashboard::menuItem("Eddy-Co Plotting", tabName = "swft_ecfast_tab",     icon = shiny::icon("sun",            lib = "font-awesome")),
        shinydashboard::menuItem("QFQM Plotting",    tabName = "swft_qfqm_tab",       icon = shiny::icon("flask",          lib = "font-awesome")),
        shinydashboard::menuItem("", tabName = "hidden")
      )
    ),
    # Body
    shinydashboard::dashboardBody(

      dashboardthemes::shinyDashboardThemes(theme = "grey_dark"),
      tags$style(type='text/css', ".selectize-input { font-size: 20px; line-height: 20px; font-color: #FFFFFF; font-weight: bold;}
                                   .selectize-dropdown { font-size: 20px; line-height: 20px; font-color: #FFFFFF; font-weight: bold;}
                                   .input-sm { font-size: 20px; line-height: 20px; font-color: #FFFFFF; font-weight: bold; }"),

      tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
      shinydashboard::tabItems(
        # ----------- Swift Tab ---------
        shinydashboard::tabItem(tabName = "swft_home_tab",
          shinydashboard::box(width = 12,
              shiny::column(width = 7,
              shiny::h1("An Eddy-Covariance State of Health Dashboard"),
              shiny::tags$h2(
                shiny::helpText(a("Swift Update Log",href="./Swift Update Log.pdf",target="_blank"))
              ),
              shiny::h4("Users can use this application to plot data from all TIS sites to identify Eddy-Co issues, view trends, and verify calibrations/validations."),
              shiny::icon("signal", lib = "font-awesome"),
              shiny::tags$b("LC Sevices"),
              shiny::h4("Raw LC Service uptimes! This tab features an overview plot and site specific LC Service plots. You can use the LC Services tab to quickly identify sites with LC Service issues and investigate site specific LC Service outages."),
              shiny::icon("hourglass-half", lib = "font-awesome"),
              shiny::tags$b("LC Time Check"),
              shiny::h4("Some IS sensors can have timestamp drift due to a variety of causes. Technicians/Engineers can use tab to see if there are any IS sensors that have large timestamp differentials (the difference between actual time and the sensor's time)."),
              shiny::icon("adjust", lib = "font-awesome"),
              shiny::tags$b("Gas Cylinders"),
              shiny::h4("Check current cylinder pressures, delivery pressures, and average pressure loss overtime. Also features a table showing expected cylinder depletion rates based upon average pressure loss."),
              shiny::icon("atom", lib = "font-awesome"),
              shiny::tags$b("CVAL Plotting"),
              shiny::h4("This tab that uses `fst` files that renders all daily CVALs and visualizes all CVALs phase shifted by month!"),
              shiny::icon("sun", lib = "font-awesome"),
              shiny::tags$b("Eddy-Co Plotting"),
              shiny::h4("A tab that uses `fst` data files to render Eddy-Co data; from Co2 measurements, CSAT3 wind data, and measurement level flows."),
              shiny::tags$b("QFQM Plotting"),
              shiny::h4("This tab is for investigateing the Quality Flag and Quality Metric data from Eddy4R."),

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
                shiny::selectizeInput(inputId = 'swft_lcservices_site',label = 'Select SiteID', multiple = FALSE,
                  choices = as.vector(swft.full.site.lookup$SiteID),
                  selected = "HARV",
                  width='100%'
                ),
                shiny::dateRangeInput(inputId = "swft_lcservices_date_range", label = "Select Date Range for Plot [dev]",
                      start = Sys.Date()-14, end = Sys.Date()+2, max = Sys.Date()+2
                ),
                shiny::selectizeInput(inputId = "swft_lcservices_lc_number", label = "Select LC Number",  multiple = FALSE,
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
                      plotly::plotlyOutput("CnCUptimePlot") %>% shinycssloaders::withSpinner(color="white",type="8",color.background = "white"),
                    ),
                    shiny::conditionalPanel(condition =  "input.swft_lcservices_radio_overall == 2",
                      plotly::plotlyOutput("CnCUptimePlotAquatics") %>% shinycssloaders::withSpinner(color="white",type="8",color.background = "white"),
                    )
                ),
                shiny::conditionalPanel(condition =  "input.swft_lcservices_radio_lcnumber == 'rtu'",
                  shiny::conditionalPanel(condition =  "input.swft_lcservices_radio_overall == 1",
                    plotly::plotlyOutput("RTUUptimePlot") %>% shinycssloaders::withSpinner(color="white",type="8",color.background = "white")
                  ),
                  shiny::conditionalPanel(condition =  "input.swft_lcservices_radio_overall == 2",
                    plotly::plotlyOutput("RTUUptimePlotAquatics") %>% shinycssloaders::withSpinner(color="white",type="8",color.background = "white")
                  )
                ),
                shiny::conditionalPanel(condition =  "input.swft_lcservices_radio_lcnumber == 'hornetq'",
                  shiny::conditionalPanel(condition =  "input.swft_lcservices_radio_overall == 1",
                    plotly::plotlyOutput("HornetQUptimePlot") %>% shinycssloaders::withSpinner(color="white",type="8",color.background = "white")
                  ),
                  shiny::conditionalPanel(condition =  "input.swft_lcservices_radio_overall == 2",
                    plotly::plotlyOutput("HornetQUptimePlotAquatics") %>% shinycssloaders::withSpinner(color="white",type="8",color.background = "white")
                  )
                )
              )
            ),
            shinydashboard::tabBox(width = 12,
              shiny::tabPanel("Site Plot",width=12,
                shiny::fluidRow(width = "100%",
                  shiny::fluidRow(
                    shiny::conditionalPanel(condition =  "input.swft_lcservices_radio_lcnumber == 'cnc'",
                      plotly::plotlyOutput("CnCPlot", height = "600px") %>% shinycssloaders::withSpinner(color="white",type="8",color.background = "white")
                    ), # End Conditional Panel
                    shiny::conditionalPanel(condition =  "input.swft_lcservices_radio_lcnumber == 'rtu'",
                      plotly::plotlyOutput("RTUPlot", height = "600px") %>% shinycssloaders::withSpinner(color="white",type="8",color.background = "white")
                    ), # End Conditional Panel
                    shiny::conditionalPanel(condition =  "input.swft_lcservices_radio_lcnumber == 'hornetq'",
                      plotly::plotlyOutput("HornetQPlot", height = "600px") %>% shinycssloaders::withSpinner(color="white",type="8",color.background = "white")
                    ) # End Conditional Panel
                  )
                )
              ) # End tabPanel
            ) # End tabBox
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
                shiny::p("Difference between actual UTC and the sensor's timestamp must be greater than 10 seconds to appear in this reports.")
              )
            ), # End Column 7
            shiny::column(width = 4),
            shiny::column(width = 4,
              shiny::fluidRow(
                shinydashboard::valueBoxOutput("swft_timestamp_last_update_box", width = 12),
                shiny::br(),
                shiny::br()
              )
            ),
            shinydashboard::box(width = 12,
              shiny::fluidRow(width = "100%",
                shiny::fluidRow(
                  shiny::plotOutput("swft_timestamp_plot") %>% shinycssloaders::withSpinner(color="white",type="8",color.background = "white"),
                  DT::dataTableOutput("swft_timestamp_table") %>% shinycssloaders::withSpinner(color="white",type="8",color.background = "white")
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
                shiny::selectizeInput(inputId = 'swft_spangas_site',label = 'Select SiteID', multiple = FALSE,
                                   choices = swft.tis.site.lookup$SiteID,
                                   selected = sample(swft.tis.site.lookup$SiteID, 1),
                                   width='100%')
              ),
              shiny::column(width = 3,
                  shiny::dateRangeInput(inputId = "swft_spangas_date_range", label = "Select Date Range for Plot",
                                        start = Sys.Date() - 28, end = Sys.Date() + 1, max = Sys.Date() + 1)
              )
            ),
            # ), # End Column 7
            shinydashboard::tabBox(width = 12,
              shiny::tabPanel("Total Pressure",width=12,
                shiny::fluidRow(width = "100%",
                  plotly::plotlyOutput("swft_spangas_overall_plot", height = "600px") %>% shinycssloaders::withSpinner(color="white",type="8",color.background = "white")
                ) # End fluidRow
              ), # End tabPanel
              shiny::tabPanel("Delivery Pressure",width=12,
                shiny::p("Ideal delivery pressure is 11.6 PSI, but an acceptable range is between 9.5 and 13 PSI."),
                shiny::fluidRow(width = "100%",
                  plotly::plotlyOutput("swft_spangas_delivery_plot", height = "600px") %>% shinycssloaders::withSpinner(color="white",type="8",color.background = "white")
                ) # End fluidRow
              ), # End tabPanel
              shiny::tabPanel("Avg Pressure Loss",width=12,
                shiny::p("Ideal pressure loss in a fully functional Cval system is ~ 4-6 PSI."),
                shiny::fluidRow(width = "100%",
                  plotly::plotlyOutput("swft_spangas_loss_plot", height = "600px") %>% shinycssloaders::withSpinner(color="white",type="8",color.background = "white")
                ) # End fluidRow
              ) # End tabPanel
            ) # End tabBox
          ) # End Cval Fst box
        ), # End Cval Fst

        ############################################                            Cval Fast                         ############################################

        shinydashboard::tabItem(tabName = "swft_cvalfast_tab",
          shinydashboard::box(width = 12,
            shiny::column(width = 12,
              shiny::fluidRow(
                shiny::column(width = 4,
                  shiny::h1("Calibrations and Validations Plotting"),
                  shiny::br(),
                  shiny::p("Use the data here to verify Cvals are occuring as expected. Check that span gases are being delivered properly. Zero*, Low, Int, High"),
                  shiny::column(width = 5,
                    shiny::fluidRow(
                      shiny::selectizeInput(inputId = "swft_cval_site", multiple = FALSE,
                                            label = "Select Site",
                                            choices = swft.tis.site.lookup$SiteID,
                                            selected = sample(swft.tis.site.lookup$SiteID, 1)),
                      shiny::selectizeInput(inputId = "swft_cval_sensor", multiple = FALSE,
                                         label = "Select Cal/Val to Render",
                                         choices = c("G2131 Validations" = "G2131i","Li840 CVALs" = "Li840A", "Li7200 Validations" = "Li7200", "L2130-i Validations" = "L2130i"))
                    )
                  ),
                  shiny::column(width = 7,
                    shiny::uiOutput('swft_cval_react_unique_cvals'),
                    shiny::conditionalPanel(condition = "input.swft_cval_sensor == 'L2130i'",
                      shiny::radioButtons(inputId = "swft_cval_l2130_options", label = "Choose Stream", inline = TRUE, choices = c("H2O" = "L2130_H2O", "18O" = "L2130_18O_isotope", "2H" = "L2130_2H_isotope"))
                    )
                  )
                ),
                shiny::column(width = 8,
                  shiny::h2("Observatory CVAL Records"),
                  shiny::fluidRow(
                    shinydashboard::valueBoxOutput("swft_cval_g2131_records",  width = 3),
                    shinydashboard::valueBoxOutput("swft_cval_li840_records",  width = 3),
                    shinydashboard::valueBoxOutput("swft_cval_li7200_records", width = 3),
                    shinydashboard::valueBoxOutput("swft_cval_l2130_records",  width = 3),
                  ),
                  shiny::fluidRow(
                    shinydashboard::valueBoxOutput("swft_cval_site_records", width = 3),
                    shinydashboard::valueBoxOutput("swft_cval_site_sensor_records", width = 3),
                    shinydashboard::valueBoxOutput("swft_cval_total_records",  width = 3)

                  )
                )
              ) # End fluidRow
            ) # End Top Master column
          ), # End tabBox
          shinydashboard::box(width = 12,
            shiny::column(width = 12,
              shiny::tabPanel("",width=12,
                shiny::fluidRow(
                  shiny::column(width =12,
                    # Co2 Analyzers Data
                    shiny::conditionalPanel(condition = 'input.swft_cval_sensor == "Li840A"',
                      shiny::fluidRow(
                        shinydashboard::valueBoxOutput("swiftEcseLow", width = 4),shinydashboard::valueBoxOutput("swiftEcseInt", width = 4),shinydashboard::valueBoxOutput("swiftEcseHigh", width = 4)
                      )
                    ),
                    shiny::conditionalPanel(condition = 'input.swft_cval_sensor == "G2131i"',
                      shiny::fluidRow(
                        shinydashboard::valueBoxOutput("swiftEcseLow2", width = 4),shinydashboard::valueBoxOutput("swiftEcseInt2", width = 4),shinydashboard::valueBoxOutput("swiftEcseHigh2", width = 4)
                      )
                    ),
                    shiny::conditionalPanel(condition = 'input.swft_cval_sensor == "Li7200"',
                      shiny::fluidRow(
                        shinydashboard::valueBoxOutput("swiftEcteLow", width = 4),shinydashboard::valueBoxOutput("swiftEcteInt", width = 4),shinydashboard::valueBoxOutput("swiftEcteHigh", width = 4)
                      )
                    ),
                    # EC First Plot for all systems
                    shiny::fluidRow(
                      shiny::h2("Cval Plot"),
                      shiny::br(),
                      plotly::plotlyOutput("plot_co2_ecse", height = "600px") %>% shinycssloaders::withSpinner(color="white",type="8",color.background = "white"),
                      # ECTE Post-Validation Leak Check
                      shiny::conditionalPanel(condition = 'input.swft_cval_sensor == "Li7200"',
                        shinydashboard::box(width = 12,
                          shiny::fluidRow(
                            shiny::h2("ECTE Post-Validation Leak Check"),
                            shiny::p("This plot shows the leak check valve status and the flow through the sensor. Ideally this flow is 0 while the valve status is 1, if there is flow during the leak check; a leak is occuring."),
                            shiny::p("The leak check lasts 15 minutes performing three 5-minute checks."),
                            plotly::plotlyOutput("plotly_co2_ecte_leak_check", height = "400px") %>% shinycssloaders::withSpinner(color="white",type="8",color.background = "white")
                          )
                        )
                      )
                    ),
                    shiny::fluidRow(
                      shiny::br(),
                      shiny::h2("Cval Table"),
                      shiny::column(width = 2),
                      shiny::column(width = 8,
                        DT::dataTableOutput("table_co2_ecse", width = "100%") %>% shinycssloaders::withSpinner(color="white",type="8",color.background = "white")
                      ),
                      shiny::column(width = 2)
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
                      shiny::br()
                    )
                  ),
                  shiny::fluidRow(
                    shiny::column(width = 4,
                      shiny::p("Data is collected from an automated Presto pulled designed by IS Science and CI. Data is updated in the early morning (10:00:00 UTC)."),
                      shiny::a("Open the Eddy-Co Architecture Map",target="_blank",href="EC_ArchMap.pdf"),
                      shiny::br(),
                      shiny::br()
                    ),
                    shiny::column(width = 4,
                      shiny::p("Daily sensor files are then stored at an S3 bucket, and retrieved by a IS Science designed function. All data are 2-minute point data, meaning that what ever the values was at the 2 minute interval, is the value that is stored."),
                      shiny::br(),
                      shiny::br()
                    ),
                    shiny::column(width = 4,
                      shinydashboard::valueBoxOutput("swft_ec_fast_collect_data_time", width = 12),
                      shiny::br(),
                      shiny::br(),
                      shiny::br(),
                      shiny::br(),
                      shiny::br()
                    )
                  ),
                  shiny::column(width = 3,
                    shiny::fluidRow(
                      shiny::dateRangeInput(inputId = "swft_EddyCo_date_range", label = "Select a Date Range",
                                            min = "2017-01-01",
                                            max = Sys.Date(),
                                            start = Sys.Date()-7,
                                            end = Sys.Date())
                    )
                  ),
                  shiny::column(width = 2,
                    shiny::fluidRow(
                      shiny::selectizeInput(inputId = "swft_EddyCo_site", multiple = FALSE,
                                        label = "Select Site",
                                        choices = swft.tis.site.lookup$SiteID,
                                        selected = sample(swft.tis.site.lookup$SiteID, 1))
                    )
                  ),
                  shiny::column(width = 3,
                    shiny::fluidRow(
                      shiny::selectizeInput(inputId = "swft_EddyCo_data_type", multiple = FALSE,
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
                                         selected = sample(x = c("G2131", "L2130", "Li840", "Li7200", "CO2", "H2O", "CSAT3", "amrs", "HMP155", "ecse.mfm", "ecse.voltage", "ec.temps"),size = 1))
                    )
                  ),
                  shiny::column(width = 2,
                    shiny::fluidRow(
                      shiny::conditionalPanel(condition = "input.swft_EddyCo_data_type == 'G2131'",
                        shiny::selectizeInput(inputId = 'swft_EddyCo_sub_data_type_G2131', multiple = FALSE, label = 'Sub Data Type', choices = c("CO2","H2O", "Isotopes", "Sample Valves"))
                      ),
                      shiny::conditionalPanel(condition = "input.swft_EddyCo_data_type == 'Li840'",
                        shiny::selectizeInput(inputId = 'swft_EddyCo_sub_data_type_Li840', multiple = FALSE, label = 'Sub Data Type', choices = c("CO2", "H2O", "Sample Valves", "Flow Rate"))
                      ),
                      shiny::conditionalPanel(condition = "input.swft_EddyCo_data_type == 'L2130'",
                        shiny::selectizeInput(inputId = 'swft_EddyCo_sub_data_type_L2130', multiple = FALSE, label = 'Sub Data Type', choices = c("Isotope - 2H", "Isotope - 18O", "H2O", "Sample Valves"))
                      ),
                      shiny::conditionalPanel(condition = "input.swft_EddyCo_data_type == 'Li7200'",
                        shiny::selectizeInput(inputId = 'swft_EddyCo_sub_data_type_Li7200', multiple = FALSE, label = 'Sub Data Type', choices = c("CO2", "H2O", "Flow", "Signal Strength", "Cell Temp", "Pressure Differential", "Diagnostic" ))
                      ),
                      shiny::conditionalPanel(condition = "input.swft_EddyCo_data_type == 'HMP155'",
                        shiny::selectizeInput(inputId = 'swft_EddyCo_sub_data_type_HMP155', multiple = FALSE, label = 'Sub Data Type', choices = c("Relative Humidity", "Temperature", "Dew Point"))
                      )
                    )
                  ),
                  shiny::column(width = 2,
                    shiny::radioButtons(inputId = "swft_EddyCo_radioButton", label = "Advanced Options",choices = c("Enabled", "Disabled"), selected = "Disabled")
                  ),
                ),
                shiny::fluidRow(
                  shiny::column(width = 5,
                    shiny::column(width = 12,
                      shiny::conditionalPanel(condition = "input.swft_EddyCo_radioButton == 'Enabled' ",
                        shiny::column(width = 4,
                          shiny::selectizeInput(inputId = "swft_EddyCo_radio_y_custom", multiple = FALSE, label = "Customize Y-axis", choices = c("Yes", "No"), selected = "No")
                        ),
                        shiny::conditionalPanel(condition = "input.swft_EddyCo_radio_y_custom == 'Yes'",
                          shiny::column(width = 2,
                            shiny::numericInput(inputId = "swft_EddyCo_y_lower",label = "Min Y", value = 350)
                          ),
                          shiny::column(width = 2,
                            shiny::numericInput(inputId = "swft_EddyCo_y_upper",label = "Max Y", value = 500)
                          )
                        ),
                        shiny::column(width = 4,
                          shiny::selectizeInput(inputId = "swft_EddyCo_facet_wrap", multiple = FALSE, label = "Facet by stream name?", choices = c("Yes", "No"), selected = "No")
                        )
                      )
                    )
                  ),
                  shiny::column(width = 3,
                    shiny::actionButton(inputId = "swft_ec_fast_actionButton", label = "  Start Plotting!", icon = icon(name = "play-circle"))
                  )
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
                    shiny::plotOutput("swft_ec_fast_plot", height = "600px") %>% shinycssloaders::withSpinner(color="white",type="8",color.background = "white")
                  ) # End EC Fast shiny::column for Conditional Panels
                ), # End EC Fst shiny::fluidRow for Condtional panels
                shiny::fluidRow(
                  shiny::column(width = 2),
                  shiny::column(width = 8,
                    DT::dataTableOutput("swft_ec_fast_table") %>% shinycssloaders::withSpinner(color="white",type="8",color.background = "white")
                  ), 
                  shiny::column(width = 2)
                )
              ) # End blank tabPanel
            ) # End EC Fst Tab Panel for Condtional panels
          ) # End EC Fst box
        ), # End EC Fst
        shinydashboard::tabItem(tabName = "swft_qfqm_tab",
          shinydashboard::box(width = 12,
            shiny::column(width = 12,
              shiny::fluidRow(
                shiny::h1("QFQM Plotting"),
                shiny::h2("Please give the code a few moments to load in the QFQM data..."),
                shiny::selectizeInput(inputId = "swft_qfqm_site", multiple = FALSE,
                                      label = "Select Site",
                                      choices = swft.tis.site.lookup$SiteID,
                                      selected = sample(swft.tis.site.lookup$SiteID, 1)
                ),
                shiny::br(),
                shiny::conditionalPanel(condition = "output.qfqm_data_loaded == 'True'",
                  shiny::selectizeInput(inputId = "swft_qfqm_dp", multiple = FALSE,
                                    label = "Select Data Product",
                                    choices = c("CO2 Storage" = "co2Stor", "CO2 Turbulent" = "co2Turb", "Flux Heat Soil" = "fluxHeatSoil", "H2O Soil Vol" = "h2oSoilVol",
                                                "H2O Storage" = "h2oStor", "H2O Turbulent" = "h2oTurb", "Isotopic CO2" = "isoCo2", "Isotopic H2O" = "isoH2o",
                                                "Net Radiation" = "radiNet", "Sonic Wind" = "soni", "Air Temperature Level" =  "tempAirLvl", "Air Temperature Top Level" = "tempAirTop",
                                                "Soil Temperature" = "tempSoil")),
                  shiny::dateInput(inputId = "swft_qfqm_date", label = "Select Date", value = "2021-01-31", max = Sys.Date() - 8, min = "2021-01-01"),
                  shiny::selectizeInput(inputId = "swft_qfqm_focus_in", multiple = FALSE, label = "Focus in on a specific variables?", choices = c("Yes","No"), selected = "No"),
                  shiny::conditionalPanel(condition = "input.swft_qfqm_focus_in == 'Yes'",
                   shiny::uiOutput('swft_qfqm_vars')
                  )
                )
              )
            ), # End Column 7
            shiny::column(width = 1),
            shiny::column(width = 7,
              )
          ),
          shinydashboard::box(width = 12,
            shiny::fluidRow(width = "100%",
              shiny::fluidRow(
                plotly::plotlyOutput("swft_qfqm_plot") %>% shinycssloaders::withSpinner(color="white",type="8",color.background = "white")
              ) # End fluidRow
            ) # End fluidRow
          ) # End box
        ), # End QFQM Fst box
        shinydashboard::tabItem(tabName = "hidden",
          shinydashboard::box(width = 12,
            shiny::column(width = 12,
              shiny::fluidRow(
                shiny::h1("QFQM Plotting"),
                shiny::h2("Please give the code a few moments to load in the QFQM data..."),
                shiny::selectizeInput(inputId = "swft_qfqm_dev_site", multiple = FALSE,
                                      label = "Select Site",
                                      choices = swft.tis.site.lookup$SiteID,
                                      selected = sample(swft.tis.site.lookup$SiteID, 1)
                )
              )
            )
          )
        )
        
      ) # End Tab Items
      
    ) # End Dashboard Body
    
  ) # End of Page
  
) # End of UI