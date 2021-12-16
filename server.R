# Specified path to the server folder for Swift
swft.server.folder.path="./"

Sys.setenv(
  "AWS_ACCESS_KEY_ID"     = "research-eddy-inquiry",
  "AWS_SECRET_ACCESS_KEY" = base::readRDS(paste0(swft.server.folder.path, "secret.key.RDS")),
  "AWS_S3_ENDPOINT"       = "neonscience.org",
  "AWS_DEFAULT_REGION"    = "s3.data"
)

# Defined server logic that loads each seperate tab from it's own server file. 
server <- function(input, output, session) {
  
  # Grab start time of session
  server_start_time = Sys.time()
  # When session is closed, collect data and save it 
  session$onSessionEnded(function(){
    # Grab end time fo session
    server_end_time = Sys.time()
    # Create new data.table
    users_data_in = data.table::data.table("Start" = server_start_time, "End" = server_end_time, "Total Time" = difftime(server_end_time, server_start_time, units = "secs"))
    # Read in master file
    all_users_data = aws.s3::s3readRDS(object = "neon-swift/user_log.RDS", bucket = "research-eddy-inquiry")
    # Combine the new data to the master
    users_data_out = data.table::rbindlist(l = list(all_users_data, users_data_in))
    # Save out the new file
    aws.s3::s3saveRDS(x = all_users_data, bucket = "research-eddy-inquiry", object = "neon-swift/user_log.RDS")
  })
  
  base::source(file='./server/swiftCvalFst.R',           local = TRUE)
  base::source(file='./server/swiftCvalTable.R',         local = TRUE)
  base::source(file='./server/swftEddyCo.R',             local = TRUE)
  base::source(file='./server/swiftLcServices.R',        local = TRUE)
  base::source(file='./server/swiftTimestampCheck.R',    local = TRUE)
  base::source(file='./server/swiftSpanGases.R',         local = TRUE)
  base::source(file="./server/swiftMaintenance.R",       local = TRUE)
  base::source(file="./server/swiftObsMaintenance.R",    local = TRUE)
  base::source(file='./server/swiftQfQm.R',              local = TRUE)
  base::source(file='./server/swiftQfQmMacro.R',         local = TRUE)
  base::source(file='./server/swiftHidden.R',            local = TRUE)
  base::source(file='./server/swiftWetDep.R',            local = TRUE)
}