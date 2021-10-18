library(shiny)
library(dplyr)
library(htmlwidgets)
library(plotly)
library(ggplot2)
library(DT)
library(tidyr)
library(data.table)
library(fst)
library(shinycssloaders)
library(shinydashboard)
library(viridis)
library(stringr)
library(scales)
library(aws.signature)
library(aws.s3)
library(lubridate)
library(dashboardthemes)
library(ggdark)
library(shinyWidgets)

swft.server.folder.path = "./"

# Essential Site Lookup Tables
swft.full.site.lookup = data.table::fread(paste0(swft.server.folder.path, "data/lookup/swft.full.site.lookup.csv"))
swft.ais.site.lookup  = data.table::fread(paste0(swft.server.folder.path, "data/lookup/swft.ais.site.lookup.csv"))
swft.tis.site.lookup  = data.table::fread(paste0(swft.server.folder.path, "data/lookup/swft.tis.site.lookup.csv"))


When_was_the_update_log_update = base::file.info(paste0(swft.server.folder.path,"www/Swift_Update_Log.pdf"))$mtime

# Define UI for application that draws a histogram
shiny::shinyUI(
  shinydashboard::dashboardPage(skin = "black",
    # Header
    shinydashboard::dashboardHeader(title = 'Swift',
                                    titleWidth = 200
                                    ),
    # Menu bar
    shinydashboard::dashboardSidebar(
      width = 160,
      shinydashboard::sidebarMenu(id = "menu",
        shinydashboard::menuItem("Home Page",        tabName = "swft_home_tab"                                                                  ),
        shinydashboard::menuItem("LC Services",      tabName = "swft_lcservices_tab", icon = shiny::icon("signal",         lib = "font-awesome")),
        shinydashboard::menuItem("LC Time Check",    tabName = "swft_timestamp_tab",  icon = shiny::icon("hourglass-half", lib = "font-awesome")),
        shinydashboard::menuItem("Gas Cylinders",    tabName = "swft_spangas_tab",    icon = shiny::icon("adjust",         lib = "font-awesome")),
        shinydashboard::menuItem("CalVals",       tabName = "",       icon = shiny::icon("flask",          lib = "font-awesome"),
          startExpanded = FALSE,
          collapsible = 
            shinydashboard::menuSubItem(text = "", tabName = ""), # I have no idea why, but this first one never appears on the ui?
            shinydashboard::menuSubItem(text = "CalVal Plots",    tabName = "swft_cvalfast_tab", icon = icon('flask')),
            shinydashboard::menuSubItem(text = "Span Values",    tabName = "swft_historic_span_table_tab", icon = icon('table'))
        ),
        shinydashboard::menuItem("Eddy-Co Plotting", tabName = "swft_ecfast_tab",     icon = shiny::icon("sun",            lib = "font-awesome")),
        shinydashboard::menuItem("Eddy QFQM ",       tabName = "",       icon = shiny::icon("flask",          lib = "font-awesome"),
          startExpanded = FALSE,
          collapsible = 
            shinydashboard::menuSubItem(text = "", tabName = ""), # I have no idea why, but this first one never appears on the ui?
            shinydashboard::menuSubItem(text = "Micro View",    tabName = "swft_qfqm_tab_micro", icon = icon('microscope')),
            shinydashboard::menuSubItem(text = "Macro View",    tabName = "swft_qfqm_tab_macro", icon = icon('superpowers'))
        ),
        shinydashboard::menuItem("TIS OS Data",       tabName = "",       icon = shiny::icon("circle-notch",          lib = "font-awesome"),
          startExpanded = FALSE,
          collapsible = 
            shinydashboard::menuSubItem(text = "", tabName = ""), # I have no idea why, but this first one never appears on the ui?
            shinydashboard::menuSubItem(text = "Site Maintenance", tabName = "swft_maintenance_tab",     icon = icon('wrench',   lib = "font-awesome")),
            shinydashboard::menuSubItem(text = "Obs Maintenance",  tabName = "swft_obs_maintenance_tab", icon = icon("globe",    lib = "font-awesome")),
            shinydashboard::menuSubItem(text = "Dust Mass (dev)",        tabName = "swft_dust_mass_tab",       icon = icon('braille',  lib = "font-awesome")),
            shinydashboard::menuSubItem(text = "Wet Dep (dev)",          tabName = "swft_wet_dep_tab",         icon = icon('vial',     lib = "font-awesome"))
        ),
        shinydashboard::menuItem("", tabName = "no"),
        shinydashboard::menuItem("", tabName = "swft_hidden_tab")
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

        ############################################                            Home                         ############################################
        shinydashboard::tabItem(tabName = "swft_home_tab",
          
          # TODO: make this only appear if the update log was recently updated. Until then... uncomment :D                                
          shiny::modalDialog(title = paste0("Swift was updated!\nUpdated the Timestamp Checker!!"),size = "l", shiny::helpText(a("2021-10-18", href="./Swift_Update_Log.pdf", target="_blank")), easyClose = TRUE),

          shinydashboard::box(width = 12,
              shiny::column(width = 7,
              shiny::h1("An Eddy-Covariance State of Health Dashboard"),
              shiny::tags$h2(
                shiny::helpText(a("Swift Update Log",href="./Swift_Update_Log.pdf",target="_blank"))
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
              shiny::tags$b("CalVals"),
              shiny::h4("CalVal Plots - This tab that uses `fst` files that renders all daily CVALs and visualizes all CVALs phase shifted by month!"),
              shiny::h4("Span Values - This tab provides historic gas cylinder installation information."),
              shiny::icon("sun", lib = "font-awesome"),
              shiny::tags$b("Eddy-Co Plotting"),
              shiny::h4("A tab that uses `fst` data files to render Eddy-Co data; from Co2 measurements, CSAT3 wind data, and measurement level flows."),
              shiny::icon("flask", lib = "font-awesome"),
              shiny::tags$b("Eddy QFQM"),
              shiny::h4("Micro View - This tab is for investigating recent Quality Flag and Quality Metric data from eddy4R."),
              shiny::h4("Macro View - This tab provides a heat map of EC quality flags, useful for investigating systemic quality issues at any TIS site."),
              shiny::icon("wrench", lib = "font-awesome"),
              shiny::tags$b("TIS Maintenance"),
              shiny::h4("Long promised, finally delivered, a reactive look at TIS maintenance data!")

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
                      start = Sys.Date()-8, end = Sys.Date()+2, max = Sys.Date()+2
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
                shiny::column(width = 6,
                  shiny::radioButtons("swft_lcservices_radio_overall", "Select IS System",
                                    choices = list("TIS" = 1, "AIS" = 2),selected = 1, inline = TRUE)
                )
              ),
              shiny::fluidRow(
                shiny::conditionalPanel(condition =  "input.swft_lcservices_radio_lcnumber == 'cnc'",
                    shiny::conditionalPanel(condition =  "input.swft_lcservices_radio_overall == 1",
                      plotly::plotlyOutput("CnCUptimePlot", height = "250px") %>% shinycssloaders::withSpinner(color="white", type="6", color.background = "white")
                    ),
                    shiny::conditionalPanel(condition =  "input.swft_lcservices_radio_overall == 2",
                      plotly::plotlyOutput("CnCUptimePlotAquatics") %>% shinycssloaders::withSpinner(color="white", type="6", color.background = "white")
                    )
                ),
                shiny::conditionalPanel(condition =  "input.swft_lcservices_radio_lcnumber == 'rtu'",
                  shiny::conditionalPanel(condition =  "input.swft_lcservices_radio_overall == 1",
                    plotly::plotlyOutput("RTUUptimePlot") %>% shinycssloaders::withSpinner(color="white", type="6", color.background = "white")
                  ),
                  shiny::conditionalPanel(condition =  "input.swft_lcservices_radio_overall == 2",
                    plotly::plotlyOutput("RTUUptimePlotAquatics") %>% shinycssloaders::withSpinner(color="white", type="6", color.background = "white")
                  )
                ),
                shiny::conditionalPanel(condition =  "input.swft_lcservices_radio_lcnumber == 'hornetq'",
                  shiny::conditionalPanel(condition =  "input.swft_lcservices_radio_overall == 1",
                    plotly::plotlyOutput("HornetQUptimePlot") %>% shinycssloaders::withSpinner(color="white", type="6", color.background = "white")
                  ),
                  shiny::conditionalPanel(condition =  "input.swft_lcservices_radio_overall == 2",
                    plotly::plotlyOutput("HornetQUptimePlotAquatics") %>% shinycssloaders::withSpinner(color="white", type="6", color.background = "white")
                  )
                )
              )
            ),
            shinydashboard::tabBox(width = 12,
              shiny::tabPanel("Site Plot",width=12,
                shiny::fluidRow(width = "100%",
                  shiny::fluidRow(
                    shiny::conditionalPanel(condition =  "input.swft_lcservices_radio_lcnumber == 'cnc'",
                      plotly::plotlyOutput("CnCPlot", height = "600px") %>% shinycssloaders::withSpinner(color="white", type="6", color.background = "white")
                    ), # End Conditional Panel
                    shiny::conditionalPanel(condition =  "input.swft_lcservices_radio_lcnumber == 'rtu'",
                      plotly::plotlyOutput("RTUPlot", height = "600px") %>% shinycssloaders::withSpinner(color="white", type="6", color.background = "white")
                    ), # End Conditional Panel
                    shiny::conditionalPanel(condition =  "input.swft_lcservices_radio_lcnumber == 'hornetq'",
                      plotly::plotlyOutput("HornetQPlot", height = "600px") %>% shinycssloaders::withSpinner(color="white", type="6", color.background = "white")
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
            shiny::fluidRow(
              shiny::h1("Timestamp Checker")
            ),
            shiny::fluidRow(
              shiny::column(width = 4,
              shiny::fluidRow(
                shiny::p("This tool checks for any differences between the LC time and the smart sensor timestamps (G2131, L2130, Li7200). If there are large differences data could be flagged as the time periods will not line up with real time."),
                                shiny::dateRangeInput(inputId = "swft_timestamp_date_range", label = "Select Date Range", start = Sys.Date()-4, end = Sys.Date(), max = Sys.Date()+1, min = "2021-08-01")
              )
              ), # End Column 7
              shiny::column(width = 4,
                shiny::p("If there is a timestamp difference greater than 10 seconds please sumbit a ticket (associate with the problem ticket PRB0040670) and put in an ITASK for ENG to resolve the issue ."),
                shiny::h3(shiny::helpText(a("How to fix Picarro Timestamp Issues",href="./Picarro_Timestamp_Drift_Troubleshooting.pdf",target="_blank"))),
                shiny::uiOutput(outputId = "swft_timestamp_site_select")
              ),
              shiny::column(width = 4,
                shiny::fluidRow(
                  shinydashboard::valueBoxOutput("swft_timestamp_last_update_box", width = 12)
                )
              )
            ),
            
            shiny::fluidRow(
              shiny::column(width = 2,
                # shiny::dateRangeInput(inputId = "swft_timestamp_date_range", label = "Select Date Range", start = Sys.Date()-4, end = Sys.Date(), max = Sys.Date()+1, min = "2021-08-01")
              ),
            ),
            shinydashboard::box(width = 12,
              shiny::fluidRow(width = "100%",
                shiny::fluidRow(
                  # shiny::plotOutput("swft_timestamp_plot") %>% shinycssloaders::withSpinner(color="white", type="6", color.background = "white"),
                  plotly::plotlyOutput("swft_timestamp_plot") %>% shinycssloaders::withSpinner(color="white", type="6", color.background = "white"),
                  shiny::br(),
                  DT::dataTableOutput("swft_timestamp_table") %>% shinycssloaders::withSpinner(color="white", type="6", color.background = "white")
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
                  plotly::plotlyOutput("swft_spangas_overall_plot", height = "600px") %>% shinycssloaders::withSpinner(color="white", type="6", color.background = "white")
                ) # End fluidRow
              ), # End tabPanel
              shiny::tabPanel("Delivery Pressure",width=12,
                shiny::p("Ideal delivery pressure is 11.6 PSI, but an acceptable range is between 9.5 and 13 PSI."),
                shiny::fluidRow(width = "100%",
                  plotly::plotlyOutput("swft_spangas_delivery_plot", height = "600px") %>% shinycssloaders::withSpinner(color="white", type="6", color.background = "white")
                ) # End fluidRow
              ), # End tabPanel
              shiny::tabPanel("Avg Pressure Loss",width=12,
                shiny::p("Theoretical pressure loss in a fully functional Cval system is ~ 3-6 PSI. However, due to pressure fluctuations you likely will not see this pressure loss unless you increase the sample period for more than 45 days AND all sensors are validating."),
                shiny::fluidRow(width = "100%",
                  plotly::plotlyOutput("swft_spangas_loss_plot", height = "600px") %>% shinycssloaders::withSpinner(color="white", type="6", color.background = "white")
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
                    shinydashboard::valueBoxOutput("swft_cval_l2130_records",  width = 3)
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
                      plotly::plotlyOutput("plot_co2_ecse", height = "600px") %>% shinycssloaders::withSpinner(color="white", type="6", color.background = "white"),
                      # ECTE Post-Validation Leak Check
                      shiny::conditionalPanel(condition = 'input.swft_cval_sensor == "Li7200"',
                        shinydashboard::box(width = 12,
                          shiny::fluidRow(
                            shiny::h2("ECTE Post-Validation Leak Check"),
                            shiny::p("This plot shows the leak check valve status and the flow through the sensor. Ideally this flow is 0 while the valve status is 1, if there is flow during the leak check; a leak is occuring."),
                            shiny::p("The leak check lasts 15 minutes performing three 5-minute checks."),
                            plotly::plotlyOutput("plotly_co2_ecte_leak_check", height = "400px") %>% shinycssloaders::withSpinner(color="white", type="6", color.background = "white")
                          )
                        )
                      )
                    ),
                    shiny::fluidRow(
                      shiny::br(),
                      shiny::h2("Cval Table"),
                      shiny::column(width = 12,
                        DT::dataTableOutput("table_co2_ecse", width = "100%") %>% shinycssloaders::withSpinner(color="white", type="6", color.background = "white")
                      )
                    )
                  ) # End column
                ) # End Cval Fast shiny::column for Conditional Panels
              ) # End Cval Fst shiny::fluidRow for Condtional panels
            ) # End Cval Fst Tab Panel for Condtional panels
          ) # End Cval Fst box
        ), # End Cval Fst
        
        ###########################################                         Span Gas Table                        ############################################
        
        shinydashboard::tabItem(tabName = "swft_historic_span_table_tab",
          shinydashboard::box(width = 12,
            shiny::column(width = 12,
              shiny::tabPanel("", width = 12,
                shiny::fluidRow(
                  shiny::h1("Historic Span Gas Assay Values Table"),
                  shiny::p("This table can be filtered to look back in time at the historic span gas assay values."),
                  DT::dataTableOutput(outputId = "swft_historic_span_table") %>% shinycssloaders::withSpinner(color="white", type="6", color.background = "white")
                )
              )
            )
          )
        ),

        ############################################                            EC Fast                           ############################################

        shinydashboard::tabItem(tabName = "swft_ecfast_tab",
          shinydashboard::box(width = 12,
            shiny::column(width = 12,
              shiny::tabPanel("", width = 12,
                shiny::fluidRow(
                  shiny::fluidRow(
                    shiny::column(width = 12,
                      shiny::h1("Eddy Covariance Plots"),
                      shiny::tags$a(
                        shinydashboard::valueBoxOutput("swft_ec_fast_ml_missing", width = 2), title = "An algorithm calculated for each site if each ML flow was above 2.0 SLPM, if this was the case, it was considered valid."
                      ),
                      shiny::tags$a(
                        shinydashboard::valueBoxOutput("swft_ec_fast_li840_working", width = 2), title = "An algorithm calculated for each site if the sample MFC flow was within 0.8 and 1.2 SLPM and the CO2 values were between 200 and 1000."
                      ),
                      shiny::tags$a(
                        shinydashboard::valueBoxOutput("swft_ec_fast_g2131_working", width = 2), title = "An algorithm calculated for each site if the CO2 values were between 200 and 1,000."
                      ),
                      shiny::tags$a(
                        shinydashboard::valueBoxOutput("swft_ec_fast_l2130_working", width = 2), title = "An algorithm calculated for each site if the H2O values were between 0 and 50,000."
                      ),
                      shiny::tags$a(
                        shinydashboard::valueBoxOutput("swft_ec_fast_li7200_working", width = 2), title = "An algorithm calculated for each site if the CO2 values were between 0 and 1,000, the diagnostic was between 8188 and 8191, the MFC flow was between 10.0 and 14.0 SLPM, and the differential cell temperature was between 2.0C and 6.0C."
                      ),
                      shiny::tags$a(
                        shinydashboard::valueBoxOutput("swft_ec_fast_collect_data_time", width = 2), title = "Calculates how long it took to pull all the files and produce the plot."
                      ),
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
                  shiny::column(width = 1,
                    shiny::fluidRow(
                      shiny::selectizeInput(inputId = "swft_EddyCo_site", multiple = FALSE,
                                        label = "Select Site",
                                        choices = swft.tis.site.lookup$SiteID,
                                        selected = sample(swft.tis.site.lookup$SiteID, 1))
                    )
                  ),
                  shiny::column(width = 4,
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
                                                    "ECSE ML/Hut MFM Pressures"              = "ecse.mfm.pressures",
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
                      ),
                      shiny::conditionalPanel(condition = "input.swft_EddyCo_data_type == 'amrs'",
                        shiny::selectizeInput(inputId = 'swft_EddyCo_sub_data_type_amrs', multiple = FALSE, label = 'Sub Data Type', choices = c("Level", "Time-Series"))
                      )
                      
                    )
                  ),
                  shiny::column(width = 2,
                    shiny::radioButtons(inputId = "swft_EddyCo_radioButton", label = "Advanced Options",choices = c("Enabled", "Disabled"), selected = "Disabled")
                  )
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
                    shiny::plotOutput("swft_ec_fast_plot", height = "600px") %>% shinycssloaders::withSpinner(color="white", type="6", color.background = "white")
                  ) # End EC Fast shiny::column for Conditional Panels
                ), # End EC Fst shiny::fluidRow for Condtional panels
                shiny::br(),
                shiny::fluidRow(
                  shiny::column(width = 2),
                  shiny::column(width = 8,
                    shiny::p("Click the CSV button to download the data"),
                    DT::dataTableOutput("swft_ec_fast_table") %>% shinycssloaders::withSpinner(color="white", type="6", color.background = "white"),
                    shiny::p("Data is collected from an automated Presto pulled designed by IS Science and CI. Data is updated in the early morning (10:00:00 UTC)."),
                    shiny::p("Daily sensor files are then stored at an S3 bucket, and retrieved by a IS Science designed function. All data are 2-minute point data, meaning that what ever the values was at the 2 minute interval, is the value that is stored.")
                  ), 
                  shiny::column(width = 2)
                )
              ) # End blank tabPanel
            ) # End EC Fst Tab Panel for Condtional panels
          ) # End EC Fst box
        ), # End EC Fst
        
        ############################################                            Maintenance                           ############################################
        
        shinydashboard::tabItem(tabName = "swft_maintenance_tab",
          shinydashboard::box(width = 12,
            shiny::column(width = 2,
              shiny::selectInput(inputId = "swft_mntc_site_select", label = "SiteID", choices = swft.tis.site.lookup$SiteID, selected = sample(swft.tis.site.lookup$SiteID, 1)), 
              shiny::dateInput(inputId = "swft_mntc_date_select", label = "Filter to Dates after ", min = "2020-05-01", max = Sys.Date() + 1, value = Sys.Date() - 40), 
              shiny::selectInput(inputId = "swft_mntc_data_select", label = "Maintenance Group", choices = c("Tower", "Soil", "Eddy", "DFIR", "Comments"), selected = "Tower")
            ),
            shiny::column(width = 4,
                          
              shinydashboard::valueBoxOutput(outputId = "swft_mtnc_bout_freq_recent", width = 12),
              shinydashboard::valueBoxOutput(outputId = "swft_mtnc_bout_freq_all_time", width = 12)
                          
              
              
            ),
            shiny::column(width = 3,        
              
              shinydashboard::valueBoxOutput(outputId = "swft_mtnc_first_bout", width = 12),
              shinydashboard::valueBoxOutput(outputId = "swft_mtnc_last_bout", width = 12)
                          
            ), 
            shiny::column(width = 3,
              shinydashboard::valueBoxOutput(outputId = "swft_mtnc_total_bouts", width = 6)
            )
            
          ),
          shinydashboard::box(width = 12,
            shiny::column(width = 12,
                          
              # Tower Maintenance
                          
              shiny::conditionalPanel(condition = "input.swft_mntc_data_select == 'Tower'",
                shiny::h1("Tower Maintenance"),
                shiny::fluidRow(
                  shiny::column(width = 4,
                    shiny::h3("Pheno:"),
                    DT::dataTableOutput(outputId = "swft_mtnc_tower_pheno")
                  )
                ),
                shiny::fluidRow(
                  shiny::column(width = 8,
                    shiny::h3("AAT:"),
                    DT::dataTableOutput(outputId = "swft_mtnc_tower_aat")
                  )
                ),
                shiny::fluidRow(
                  shiny::column(width = 8,
                    shiny::h3("Wind:"),
                    DT::dataTableOutput(outputId = "swft_mtnc_tower_wind")
                  )
                ),
                shiny::fluidRow(
                  shiny::column(width = 8,
                    shiny::h3("IR Biotemp:"),
                    DT::dataTableOutput(outputId = "swft_mtnc_tower_ir")
                  )
                ),
                shiny::fluidRow(
                  shiny::column(width = 8,
                    shiny::h3("PAR:"),
                    DT::dataTableOutput(outputId = "swft_mtnc_tower_par")
                  )
                ),
                shiny::fluidRow(
                  shiny::column(width = 10,
                    shiny::h3("Tower Top Sensors:"),
                    DT::dataTableOutput(outputId = "swft_mtnc_tt_sensors")
                  )
                ),
                shiny::fluidRow(
                  shiny::column(width = 8,
                    shiny::h3("Tower Comments:"),
                    DT::dataTableOutput(outputId = "swft_mtnc_tower_comments")
                  )
                )
              ),
              
              # Soil Maintenance
              
              shiny::conditionalPanel(condition = "input.swft_mntc_data_select == 'Soil'",
                shiny::h1("Soil Plot Maintenace"),
                shiny::fluidRow(
                  shiny::column(width = 8,
                    shiny::h3("Co2:"),
                    DT::dataTableOutput(outputId = "swft_mtnc_sp_co2")
                  )
                ),
                shiny::fluidRow(
                  shiny::column(width = 8,
                    shiny::h3("Heat Flux:"),
                    DT::dataTableOutput(outputId = "swft_mtnc_sp_soil_heat_flux")
                  )
                ),
                shiny::fluidRow(
                  shiny::column(width = 8,
                    shiny::h3("Q-Line PAR:"),
                    DT::dataTableOutput(outputId = "swft_mtnc_sp_soil_qline")
                  )
                ),
                shiny::fluidRow(
                  shiny::column(width = 8,
                    shiny::h3("Temperature:"),
                    DT::dataTableOutput(outputId = "swft_mtnc_sp_soil_temperature")
                  )
                ),
                shiny::fluidRow(
                  shiny::column(width = 12,
                    shiny::h3("Throughfall:"),
                    DT::dataTableOutput(outputId = "swft_mtnc_sp_soil_thrufall")
                  )
                ),
                shiny::fluidRow(
                  shiny::column(width = 8,
                    shiny::h3("Water Content:"),
                    DT::dataTableOutput(outputId = "swft_mtnc_sp_soil_water_content")
                  )
                ),
                shiny::fluidRow(
                  shiny::column(width = 8,
                    shiny::h3("SP3 Sensors:"),
                    DT::dataTableOutput(outputId = "swft_mtnc_sp3_sensors")
                  )
                ),
                shiny::fluidRow(
                  shiny::column(width = 8,
                    shiny::h3("Soil Comments:"),
                    DT::dataTableOutput(outputId = "swft_mtnc_sp_comments")
                  )
                )
              ),
              
              # Eddy Maintenance
              
              shiny::conditionalPanel(condition = "input.swft_mntc_data_select == 'Eddy'",
                shiny::h1("Eddy Maintenance"),
                shiny::fluidRow(
                  shiny::column(width = 12,
                    shiny::h3("Gas Analyzers:"),
                    DT::dataTableOutput(outputId = "swft_mtnc_eddy_gas_analyzer")
                  )
                ),
                shiny::fluidRow(
                  shiny::column(width = 12,
                    shiny::h3("Tower-Top Sensors:"),
                    DT::dataTableOutput(outputId = "swft_mtnc_eddy_tt")
                  )
                ),
                shiny::fluidRow(
                  shiny::column(width = 12,
                    shiny::h3("EC Inlets:"),
                    DT::dataTableOutput(outputId = "swft_mtnc_eddy_inlet")
                  )
                ),
                shiny::fluidRow(
                  shiny::column(width = 12,
                    shiny::h3("EC Pumps:"),
                    DT::dataTableOutput(outputId = "swft_mtnc_eddy_pump")
                  )
                ),
                shiny::fluidRow(
                  shiny::column(width = 12,
                    shiny::h3("Span Gas Cylinders:"),
                    DT::dataTableOutput(outputId = "swft_mtnc_eddy_gas_cylinder")
                  )
                ),
                shiny::fluidRow(
                  shiny::column(width = 12,
                    shiny::h3("ECSE Pressure Transducers:"),
                    DT::dataTableOutput(outputId = "swft_mtnc_eddy_pducer")
                  )
                ),
                shiny::fluidRow(
                  shiny::column(width = 12,
                    shiny::h3("ECSE Fep Tubing:"),
                    DT::dataTableOutput(outputId = "swft_mtnc_eddy_fep_tube")
                  )
                ),
                shiny::fluidRow(
                  shiny::column(width = 12,
                    shiny::h3("Eddy Comments:"),
                    DT::dataTableOutput(outputId = "swft_mtnc_eddy_comments")
                  )
                )
              ),
              
              # DFIR Maintenance
              
              shiny::conditionalPanel(condition = "input.swft_mntc_data_select == 'DFIR'",
                shiny::h1("DFIR Maintenance"),
                shiny::fluidRow(
                  shiny::column(width = 12,
                    shiny::h3("DFIR:"),
                    DT::dataTableOutput(outputId = "swft_mtnc_dfir")
                  )
                )
              ),
              
              # All Comments
              
              shiny::conditionalPanel(condition = "input.swft_mntc_data_select == 'Comments'",
                shiny::h1("All Comments"),
                shiny::fluidRow(
                  shiny::column(width = 12,
                    shiny::h3("Comments:"),
                    DT::dataTableOutput(outputId = "swft_mtnc_comments")
                  )
                )
              )
            )
          )
        ),
        
        
        ###########################################                            Observatory M                           ############################################
        
        shinydashboard::tabItem(tabName = "swft_obs_maintenance_tab",
          shinydashboard::box(width = 12,
            shiny::column(width = 12,
              shiny::fluidRow(
                shiny::h2("Monthly Maintenance Records"),
                shiny::column(3,
                  shiny::p("Total fulcrum maintenance records collected for each site during the month you select. If there is not a bar for a site, then no records were collected.")
                ),
                shiny::column(width = 3,
                  shiny::uiOutput("swft_obsMaintenance_month_pick") 
                ),
                shiny::column(3),
                shiny::column(width = 3,
                  shinydashboard::valueBoxOutput(outputId = "swft_obsMaintenance_last_updated", width = 12),              
                )
              ),
              shiny::fluidRow(
                plotly::plotlyOutput(outputId = "swftObsMaintenance_plot")
              )
            ),
            shiny::column(width = 12,
              shiny::fluidRow(
                shiny::column(width = 6,
                  shiny::h2("Biweekly Bout Rate"),
                  shiny::p("Frequency of biweekly maintenance bouts performed. The expected number of records is at least 1 per two weeks, so if the rate is at or greater than one, the expected number of records has been achieved")
                ),
                shiny::column(width = 3,
                  shiny::br(),
                  shiny::selectInput(inputId = "swft_obsMaintenance_rate_range", label = "How far back?", choices = c("Last month" = 30, "Last 2 months" = 60, "Last 6 months" = 365/2, "Last year" = 365))
                )
              ),
              shiny::fluidRow(
                plotly::plotlyOutput(outputId = "swft_obsMaintenance_rate_plot")
              )
            ) 
          )
        ),
        
        shinydashboard::tabItem(tabName = "swft_wet_dep_tab",
          shinydashboard::box(width = 12,
            shiny::fluidRow(
              shiny::column(width = 3,
                shiny::dateRangeInput(inputId = "swft_wet_dep_date_select", label = "Date Range", min = "2020-02-26", max = Sys.Date()+1, start = Sys.Date()-(4*7), end = Sys.Date()+1)
              ),
              shiny::column(9)
            ),
            shiny::fluidRow(
              shiny::column(width = 3,
                plotly::plotlyOutput(outputId = "swft_wet_dep_plot_2")
              ),
              shiny::column(width = 9,
                plotly::plotlyOutput(outputId = "swft_wet_dep_plot_1")
              )
            ),
            shiny::br(),
            shiny::fluidRow(
              shiny::column(width = 6,
                plotly::plotlyOutput(outputId = "swft_wet_dep_plot_3")
              ),
              shiny::column(width = 6,
                plotly::plotlyOutput(outputId = "swft_wet_dep_plot_4")
              )
            )
          )
        ),
        
        ############################################                            Eddy QFQM                          ############################################
        
        shinydashboard::tabItem(tabName = "swft_qfqm_tab_micro",
          shinydashboard::box(width = 12,
            shiny::fluidRow(
              shiny::column(width = 2,
                shiny::selectInput(   inputId = "swft_qfqm_site_select", label = "SiteID", choices = swft.tis.site.lookup$SiteID, selected = sample(swft.tis.site.lookup$SiteID, 1))
              ),
              shiny::column(width = 2,
                shiny::dateRangeInput(inputId = "swft_qfqm_date_select", label = "Date Range", min = "2020-08-08", max = Sys.Date() + 1, start = Sys.Date()-21, end = Sys.Date()-4)
              ),
              shiny::column(width = 2,
                shiny::selectInput(inputId = "swft_qfqm_eddy4R_terms", label = "Terms", 
                           choices = c("amrs","co2Stor","co2Turb","fluxHeatSoil","h2oSoilVol","h2oStor","h2oTurb","isoCo2","isoH2o","radiNet","soni","tempAirLvl","tempAirTop","tempSoil")) 
              ),
              shiny::column(width = 2,
                shiny::uiOutput("swft_qfqm_eddy4R_vars")
              ),
              shiny::column(width = 2,
                shiny::radioButtons(inputId = "swft_qfqm_facet", label = "Facet?", choices = c("Yes", "No"), selected = "Yes")              
              )
            ),
            shiny::fluidRow(
              shiny::column(width = 5,
                shiny::actionButton(inputId = "swft_qfqm_actionButton", label = "Gather QFQM data"),
                shiny::helpText("Press button once per query.\nThis takes roughly 3 seconds per week of data gathered. If the variable box is greyed out, you know that the code is still processing your request.")
              ),
              shiny::column(width = 2),
              shiny::column(width = 5)
            )
          ),
          shinydashboard::box(width = 12,
            shiny::fluidRow(
              shiny::column(width = 12,
                plotly::plotlyOutput(outputId = "swft_qfqm_plot", height = "700px")  %>% shinycssloaders::withSpinner(color="white", type="6", color.background = "white")
              )
            )
          )
        ),
        
        # swft_qfqm_tab_macro
        
        shinydashboard::tabItem(tabName = "swft_qfqm_tab_macro",
          shinydashboard::box(width = 12,
            shiny::fluidRow(
              shiny::column(width = 2,
                shiny::selectInput(inputId = "swft_qfqm_macro_site_select", label = "SiteID", choices = swft.tis.site.lookup$SiteID, selected = sample(swft.tis.site.lookup$SiteID, 1))
              ),
              shiny::column(width = 2,
                shiny::uiOutput('swft_qfqm_macro_year_select')
              ),
              shiny::column(width = 2,
                shiny::selectInput(inputId = "swft_qfqm_macro_system_select", label = "EC System", choices = "ECTE")
              ),
              shiny::column(width = 2,
                shiny::selectInput(inputId = "swft_qfqm_macro_terms", label = "Terms", choices = c("rtioMoleDryCo2", "rtioMoleDryH2o", "tempAir", "veloZaxsErth"))
                # shiny::uiOutput("swft_qfqm_macro_terms") 
              )
            )
          ),
          shinydashboard::box(width = 12,
            shiny::fluidRow(
              shiny::column(width = 12,
                # shiny::plotOutput(outputId = "swft_qfqm_macro_plot", height = "720px")  %>% shinycssloaders::withSpinner(color="white", type="6", color.background = "white")
                plotly::plotlyOutput(outputId = "swft_qfqm_macro_plot", height = "840px")  %>% shinycssloaders::withSpinner(color="white", type="6", color.background = "white")
              )
            )
          )
        ),
        
        
        
        ############################################                            Hidden Metrics                          ############################################
        
        shinydashboard::tabItem(tabName = "swft_hidden_tab",
          shinydashboard::box(width = 12, 
            shiny::column(width =6,
              plotly::plotlyOutput(outputId = "swft_hidden_plot_connections")
            ),
            shiny::column(width = 6,
              plotly::plotlyOutput(outputId = "swft_hidden_plot_durations")
            )
          )
        )

        
      ) # End Tab Items
      
    ) # End Dashboard Body
    
  ) # End of Page
  
) # End of UI
