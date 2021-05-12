# Specified path to the server folder for Swift
swft.server.folder.path="./"

Sys.setenv("AWS_ACCESS_KEY_ID"     = "research-eddy-inquiry",
           "AWS_S3_ENDPOINT"       = "neonscience.org",
           "AWS_DEFAULT_REGION"    = "s3.data")

if(file.exists("./data/user_log/user_log.RDS") == FALSE){
  empty_data = data.table::data.table()
  
  base::saveRDS(empty_data, file = "./data/user_log/user_log.RDS")
  
}

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
    all_users_data = base::readRDS(file = "./data/user_log/user_log.RDS")
    # Combine the new data to the master
    users_data_out = data.table::rbindlist(l = list(all_users_data, users_data_in))
    # Save out the new file
    base::saveRDS(object = users_data_out, file = "./data/user_log/user_log.RDS")
  })
  
  base::source(file='./server/swiftCvalFst.R',        local = TRUE)
  base::source(file='./server/swftEddyCo.R',          local = TRUE)
  base::source(file='./server/swiftLcServices.R',     local = TRUE)
  base::source(file='./server/swiftTimestampCheck.R', local = TRUE)
  base::source(file='./server/swiftSpanGases.R',      local = TRUE)
  base::source(file="./server/swiftMaintenance.R",    local = TRUE)
  base::source(file='./server/swiftQfQm.R',           local = TRUE)
  base::source(file='./server/swiftHidden.R',         local = TRUE)
}