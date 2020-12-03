# Server.R

# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.

# Specified path to the server folder for Swift. This MUST point to the Swift folder ie /srv/shiny-server/swift/
swft.server.folder.path = "/srv/shiny-server/neon-swift/"

Sys.setenv("AWS_ACCESS_KEY_ID"     = "research-eddy-inquiry",
           "AWS_S3_ENDPOINT"       = "neonscience.org",
           "AWS_DEFAULT_REGION"    = "s3.data")

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  # base::source(file=paste0(swft.server.folder.path, 'server/swiftFunctions.R',                local=T,echo=T))
  # base::source(file=paste0(swft.server.folder.path, 'server/swiftIS3RReportGenerator.R',      local=T,echo=T))
  # base::source(file=paste0(swft.server.folder.path, 'server/swiftIS3R_TIS.R',                 local=T,echo=T))
  # base::source(file=paste0(swft.server.folder.path, 'server/swiftCVAL.R',                  local=T,echo=T))
  base::source(file=paste0(swft.server.folder.path, 'server/swiftCvalFst.R'),                  local=T,echo=T)
  base::source(file=paste0(swft.server.folder.path, 'server/swiftAqua.R'),                     local=T,echo=T)
  base::source(file=paste0(swft.server.folder.path, 'server/swftEddyCo.R'),                    local=T,echo=T)
  base::source(file=paste0(swft.server.folder.path, 'server/swiftLcServices.R'), local = T, echo = T)
  base::source(file=paste0(swft.server.folder.path, 'server/swiftTimestampCheck.R'), local = T, echo = T)
  base::source(file=paste0(swft.server.folder.path, 'server/swiftSpanGases.R'), local = T, echo = T)
  
  # base::source(file='/srv/shiny-server/swift/server/swiftCo2AnalyzerUptime.R')
  # This code will be run once per user
  # users_data <- data.frame(START = Sys.time())
  # 
  # # This code will be run after the client has disconnected
  # session$onSessionEnded(function() {
  #   users_data$END <- Sys.time()
  #   names(users_data) <- c("Start","End")
  #   # Write a file in your working directory
  #   write.table( users_data,  
  #                file = file.path("/srv/shiny-server/swift/users_data.csv"), 
  #                append = T, 
  #                sep=',', 
  #                row.names=F, 
  #                col.names=F )
  # })
}
