# Server.R

# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.

# Specified path to the server folder for Swift. This MUST point to the Swift folder ie /srv/shiny-server/swift/
# swft.server.folder.path = "C:/1_GitHub/neon-swift/"
#swft.server.folder.path = "/srv/shiny-server/neon-swift/"
swft.server.folder.path="./"

Sys.setenv("AWS_ACCESS_KEY_ID"     = "research-eddy-inquiry",
           "AWS_S3_ENDPOINT"       = "neonscience.org",
           "AWS_DEFAULT_REGION"    = "s3.data")

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  base::source(file='./server/swiftCvalFst.R',        local = TRUE)#, echo = TRUE)
  base::source(file='./server/swiftAqua.R',           local = TRUE)#, echo = TRUE)
  base::source(file='./server/swftEddyCo.R',          local = TRUE)#, echo = TRUE)
  base::source(file='./server/swiftLcServices.R',     local = TRUE)#, echo = TRUE)
  base::source(file='./server/swiftTimestampCheck.R', local = TRUE)#, echo = TRUE)
  base::source(file='./server/swiftSpanGases.R',      local = TRUE)#, echo = TRUE)
  base::source(file='./server/swiftQfQm.R',           local = TRUE)#, echo = TRUE)

  # base::source(file=paste0(swft.server.folder.path, 'server/swiftCvalFst.R'),        local = TRUE, echo = TRUE)
  # base::source(file=paste0(swft.server.folder.path, 'server/swiftAqua.R'),           local = TRUE, echo = TRUE)
  # base::source(file=paste0(swft.server.folder.path, 'server/swftEddyCo.R'),          local = TRUE, echo = TRUE)
  # base::source(file=paste0(swft.server.folder.path, 'server/swiftLcServices.R'),     local = TRUE, echo = TRUE)
  # base::source(file=paste0(swft.server.folder.path, 'server/swiftTimestampCheck.R'), local = TRUE, echo = TRUE)
  # base::source(file=paste0(swft.server.folder.path, 'server/swiftSpanGases.R'),      local = TRUE, echo = TRUE)
  # base::source(file=paste0(swft.server.folder.path, 'server/swiftQfQm.R'),           local = TRUE, echo = TRUE)
}#
