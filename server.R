# Specified path to the server folder for Swift
swft.server.folder.path="./"

Sys.setenv("AWS_ACCESS_KEY_ID"     = "research-eddy-inquiry",
           "AWS_S3_ENDPOINT"       = "neonscience.org",
           "AWS_DEFAULT_REGION"    = "s3.data")

# Defined server logic that loads each seperate tab from it's own server file. 
server <- function(input, output,session) {
  base::source(file='./server/swiftCvalFst.R',        local = TRUE)
  base::source(file='./server/swiftAqua.R',           local = TRUE)
  base::source(file='./server/swftEddyCo.R',          local = TRUE)
  base::source(file='./server/swiftLcServices.R',     local = TRUE)
  base::source(file='./server/swiftTimestampCheck.R', local = TRUE)
  base::source(file='./server/swiftSpanGases.R',      local = TRUE)
  base::source(file='./server/swiftQfQm.R',           local = TRUE)
}