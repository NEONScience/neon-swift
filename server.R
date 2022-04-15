# Specified path to the server folder for Swift
swft.server.folder.path="./"

# eddycopipe::neon_gcs_connect_to_bucket(creds_json = "/srv/shiny-server/swift/service-auth.json")
eddycopipe::neon_gcs_connect_to_bucket(creds_json = "~/neon-swift/service-auth.json")

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
    all_users_data = eddycopipe::neon_gcs_get_rds(object = "neon-swift/user_log.RDS", bucket = "neon-eddy-inquiry")
    # Combine the new data to the master
    users_data_out = data.table::rbindlist(l = list(all_users_data, users_data_in))
    # Save out the new file
    eddycopipe::wrap_neon_gcs_upload(x = users_data_out, bucket = "neon-eddy-inquiry", object = "neon-swift/user_log.RDS")
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
  base::source(file='./server/swiftPostGres.R',          local = TRUE)
}