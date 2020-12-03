### Pull Data from S3 research-eddy-inquiry


# What will we need

# We will need the list of metadata
#   Need to save this to S3 so that way it can be automatically downloaded and the user doesn't have to point to specific location

# We will need the type of data
#   User will need the available types

# We will need the SiteID
#   User will need the available Sites

# We will need the time period
#   User will need the available time period for such data

read.eddy.inquiry <- function(
  dataType = NULL, 
  siteID = NULL, 
  startDate = NULL, 
  endDate = NULL, 
  sensor = NULL, 
  secretKey = NULL,
  silent = FALSE
){
  
  # Set Seed, required to create the .Random.seed variable
  set.seed(42)
  
  # Dependencies
  require(aws.s3)
  require(dplyr)
  require(data.table)
  require(fst)
  
  # Set enviroment up for S3 connection
  base::Sys.setenv(
    "AWS_S3_ENDPOINT"       = "neonscience.org",
    "AWS_DEFAULT_REGION"    = "s3.data"
  )
  
  aws.s3::bucket_exists(bucket = "research-eddy-inquiry")
  
  # Test connection
  eddyInquiry.bucket.exist <- aws.s3::bucket_exists(bucket = "research-eddy-inquiry")[1]
  
  # If test fails let the user know something is up
  if(eddyInquiry.bucket.exist == TRUE){
    
    # Function variables
    allowed.datatypes <- c("2min", "CnC", "amrs", "cval", "spanGas")
    allowed.2min <- c("amrs","HMP155","CSAT3","G2131","Li7200","Li840","ecse.mfm","ecse.sample.mfc","ec.temps","ecse.voltage","Li840.valves","L2130", "Li7200.valves")
    allowed.cval <- c("L2130i", "G2131i", "Li7200", "Li840A")
    allowed.SiteID <- aws.s3::s3read_using(FUN = base::readRDS, object = "s3.lookup/s3.site.meta.RDS", bucket = "research-eddy-inquiry")
    
    if(dataType == "meta"){
      if(sensor == "cval"){
        base::return(aws.s3::s3read_using(FUN = base::readRDS, object = "s3.lookup/s3.cval.lookup.RDS", bucket = "research-eddy-inquiry"))
      }
      if(sensor == "covid"){
        base::return(aws.s3::s3read_using(FUN = base::readRDS, object = "lookup/site.covid.status.RDS", bucket = "research-eddy-inquiry"))
      }
      if(sensor == "spanGas"){
        base::return(aws.s3::s3read_using(FUN = fst::read.fst, object = "lookup/spanGasConc.fst", bucket = "research-eddy-inquiry"))
      }
    } else {
      
      # Check if any of the REQUIRED fields are not specified
      if(is.null(dataType) == FALSE & is.null(siteID) == FALSE & is.null(startDate) == FALSE & is.null(endDate) == FALSE){
        
        # Check if the startDate and endDate is formatted correctly, else send them an error message
        if(grepl("([12]\\d{3}-(0[1-9]|1[0-2])-(0[1-9]|[12]\\d|3[01]))", startDate) == TRUE & grepl("([12]\\d{3}-(0[1-9]|1[0-2])-(0[1-9]|[12]\\d|3[01]))", endDate) == TRUE){
          
          # Datatype = 2min
          if(dataType == "2min" & siteID %in% allowed.SiteID){
            # Check if sensor is an allowed 2min sensor
            if(sensor %in% allowed.2min){
              twomin.starttime <- Sys.time()
              if(silent == FALSE){ base::message(paste0("Pull configured correctly: ", siteID, " ", dataType," ", sensor, " data from ", startDate, " to ", endDate)) }
              s3.2min.meta <- aws.s3::s3read_using(FUN = base::readRDS, object = "s3.lookup/s3.2min.meta.RDS", bucket = "research-eddy-inquiry")
              
              list.s3.2min <- s3.2min.meta %>%
                dplyr::filter(SiteID == siteID) %>%
                dplyr::filter(Sensor == sensor) %>%
                dplyr::filter(Date >= startDate & Date <= endDate)
              
              size.pull <- round(sum(as.numeric(list.s3.2min$Size))/8000000, 2)
              if(silent == FALSE){ message(paste0("Downloading ", nrow(list.s3.2min), " files - ", size.pull, " MB")) }
              pull.join <- data.table::data.table()
              
              if(length(list.s3.2min$Key) > 0){    
                
                amrs.join.starttime <- Sys.time()
                
                if(silent == FALSE){ pb <- txtProgressBar(0, length(list.s3.2min$Key), style = 3) }
                # message(knitr::kable(list.s3.2min))
                for(file in 1:length(list.s3.2min$Key)){
                  
                  if(silent == FALSE){ setTxtProgressBar(pb, file) }
                  
                  pull.file <- aws.s3::s3read_using(FUN = fst::read.fst, bucket = "research-eddy-inquiry", object = list.s3.2min$Key[file])
                  
                  pull.join <- data.table::rbindlist(l = list(pull.join, pull.file))
                }
              } else {
                message(paste0("No data found for pull: ", siteID, " ", dataType," ", sensor, " data from ", startDate, " to ", endDate))
              }
              # Collect and present time stats
              twomin.endtime <- Sys.time()
              twomin.pull.time <- round(difftime(twomin.endtime, twomin.starttime, units = "secs"),2)
              if(silent == FALSE){ message("\nPull Lasted: ", twomin.pull.time, " seconds...") }
              # Return data
              return(pull.join)
            } else {
              message("ERROR: sensor selected is incorrect. Please select one from: ", paste0(allowed.2min, collapse = ", "))
            }
          } else {
            base::message(
              base::paste0(
                "The entered SiteID is not from the approved list for your dataType, \n SiteID = ", siteID
              )
            )
          } 
            
          # Datatype is CnC
          if(dataType == "CnC"){
            cnc.starttime <- Sys.time()
            if(silent == FALSE){ base::message(paste0("Pull configured correctly: ", dataType," ", siteID, " data from ", startDate, " to ", endDate)) }
            s3.cnc.meta <- aws.s3::s3read_using(FUN = base::readRDS, object = "s3.lookup/s3.cnc.meta.RDS", bucket = "research-eddy-inquiry") %>%
              dplyr::filter(Site == siteID)
            
            pull.file <- aws.s3::s3read_using(FUN = fst::read.fst, bucket = "research-eddy-inquiry", object = s3.cnc.meta$Key[1]) %>%
              dplyr::filter(date >= startDate & date <= endDate)
            
            # Collect and present time stats
            cnc.endtime <- Sys.time()
            cnc.pull.time <- round(difftime(cnc.endtime, cnc.starttime, units = "secs"),2)
            if(silent == FALSE){ message("\nPull Lasted: ", cnc.pull.time, " seconds...") }
            # Return data
            return(pull.file)
          } else {
            base::message(
              base::paste0(
                "The entered SiteID is not from the approved list for your dataType, \n SiteID = ", siteID
              )
            )
          }
            
          # Datatype is amrs
          if(dataType == "amrs"){
            amrs.starttime <- Sys.time()
            if(silent == FALSE){ base::message(paste0("Pull configured correctly: ", dataType," ", siteID, " data from ", startDate, " to ", endDate)) }
            
            s3.amrs.meta <- aws.s3::s3read_using(FUN = base::readRDS, object = "s3.lookup/s3.amrs.meta.RDS", bucket = "research-eddy-inquiry") 
            
            list.s3.amrs <- s3.amrs.meta %>%
              dplyr::filter(Site == siteID) %>%
              dplyr::filter(Date >= startDate & Date <= endDate)
            
            size.pull <- round(sum(as.numeric(list.s3.amrs$Size))/8000000, 6)
            
            if(silent == FALSE){ message(paste0("Downloading ", nrow(list.s3.amrs), " files - ", size.pull, " MB")) }
            
            pull.join <- data.table::data.table()
            
            if(length(list.s3.amrs$Key) > 0){    
              
              amrs.join.starttime <- Sys.time()
              if(silent == FALSE){ pb <- txtProgressBar(0, length(list.s3.amrs$Key), style = 3) }
              
              for(file in 1:length(list.s3.amrs$Key)){
                if(silent == FALSE){ setTxtProgressBar(pb, file) }
                
                pull.file <- aws.s3::s3read_using(FUN = fst::read.fst, bucket = "research-eddy-inquiry", object = list.s3.amrs$Key[file])
                
                
                pull.join <- data.table::rbindlist(l = list(pull.join, pull.file))
              }
              # Join time statistics
              amrs.join.endtime <- Sys.time()
              amrs.join.time <- round(difftime(amrs.join.endtime, amrs.join.starttime, units = "secs"),2)
              if(silent == FALSE){ message("\nJoin Lasted: ", amrs.join.time, " seconds...") }
            } else {
              message(paste0("No data found for pull: ", siteID, " ", dataType," ", sensor, " data from ", startDate, " to ", endDate))
            }
            # Collect and present time stats
            amrs.endtime <- Sys.time()
            amrs.pull.time <- round(difftime(amrs.endtime, amrs.starttime, units = "secs"),2)
            if(silent == FALSE){ message("\nPull Lasted: ", amrs.pull.time, " seconds...") }
            # Return data
            return(pull.join)
          } else {
            base::message(
              base::paste0(
                "The entered SiteID is not from the approved list for your dataType, \n SiteID = ", siteID
              )
            )
          }
            
          # Datatype is spanGas
          if(dataType == "spanGas"){
            spanGas.starttime <- Sys.time()
            if(silent == FALSE){ base::message(paste0("Pull configured correctly: ", dataType," ", siteID, " data from ", startDate, " to ", endDate)) }
            
            s3.spanGas.meta <- aws.s3::s3read_using(FUN = base::readRDS, object = "s3.lookup/s3.spanGas.meta.RDS", bucket = "research-eddy-inquiry") 
            
            list.s3.spanGas <- s3.spanGas.meta %>%
              dplyr::filter(Site == siteID) %>%
              dplyr::filter(Date >= startDate & Date <= endDate)
            
            size.pull <- round(sum(as.numeric(list.s3.spanGas$Size))/8000000, 6)
            
            if(silent == FALSE){ message(paste0("Downloading ", nrow(list.s3.spanGas), " files - ", size.pull, " MB")) }
            
            pull.join <- data.table::data.table()
            
            if(length(list.s3.spanGas$Key) > 0){    
              
              spanGas.join.starttime <- Sys.time()
              if(silent == FALSE){ pb <- txtProgressBar(0, length(list.s3.spanGas$Key), style = 3) }
              
              for(file in 1:length(list.s3.spanGas$Key)){
                if(silent == FALSE){ setTxtProgressBar(pb, file) }
                
                pull.file <- aws.s3::s3read_using(FUN = fst::read.fst, bucket = "research-eddy-inquiry", object = list.s3.spanGas$Key[file])
                
                pull.join <- data.table::rbindlist(l = list(pull.join, pull.file))
              } 
              # Join time statistics
              spanGas.join.endtime <- Sys.time()
              spanGas.join.time <- round(difftime(spanGas.join.endtime, spanGas.join.starttime, units = "secs"),2)
              if(silent == FALSE){  message("\nJoin Lasted: ", spanGas.join.time, " seconds...") }
              # Span Gas lookup table DPID - Cyl Name
              spanGas.name.lookup <- data.table::fread("~/eddyInquiry/data/lookup/spanGas.lookup.csv")
              # Clean / Name the data streams
              pull.join <- pull.join %>%
                dplyr::mutate(DPID = substr(meas_strm_name, 15, 2000)) %>%
                dplyr::select(-meas_strm_name) %>%
                dplyr::mutate(minVal   = round(minVal/6.895, 2)) %>%
                dplyr::mutate(meanVal  = round(meanVal/6.895, 2)) %>%
                dplyr::mutate(maxVal   = round(maxVal/6.895, 2)) %>%
                dplyr::mutate(stDevVal = round(stDevVal/6.895, 2)) %>%
                dplyr::left_join(spanGas.name.lookup, by = "DPID") %>%
                dplyr::mutate(Date = StartTime) %>%
                dplyr::select(SiteID, Date, strm_name, meanVal, minVal, maxVal, stDevVal) 
              
            } else {
              message(paste0("No data found for pull: ", siteID, " ", dataType," ", sensor, " data from ", startDate, " to ", endDate))
            }
            # Collect and present time stats
            spanGas.endtime <- Sys.time()
            spanGas.pull.time <- round(difftime(spanGas.endtime, spanGas.starttime, units = "secs"),2)
            if(silent == FALSE){ message("\nPull Lasted: ", spanGas.pull.time, " seconds...") } 
            # Return data
            return(pull.join)
          } else {
            base::message(
              base::paste0(
                "The entered SiteID is not from the approved list for your dataType, \n SiteID = ", siteID
              )
            )
          } 
            
          # Datatype is cval
          if(dataType == "cval"){
            if(is.null(sensor) == FALSE){
              if(sensor %in% allowed.cval){
                cval.starttime <- Sys.time()
                if(silent == FALSE){ base::message(paste0("Pull configured correctly: ", dataType," ", siteID, " data from ", startDate, " to ", endDate)) }
                
                s3.cval.meta <- aws.s3::s3read_using(FUN = base::readRDS, object = "s3.lookup/s3.cval.lookup.RDS", bucket = "research-eddy-inquiry") 
                
                list.s3.cval <- s3.cval.meta %>%
                  dplyr::filter(Site == siteID) %>%
                  dplyr::filter(Sensor == sensor) %>%
                  dplyr::filter(StartDateTime >= startDate & EndDateTime <= endDate)
                
                size.pull <- round(sum(as.numeric(list.s3.cval$Size))/8000000, 3)
                
                if(silent == FALSE){ message(paste0("Downloading ", nrow(list.s3.cval), " files - ", size.pull, " MB")) }
                
                pull.join <- data.table::data.table()
                rand.seed <- as.data.table(.Random.seed) %>%
                  dplyr::filter(as.integer(`.Random.seed`) > 0)
                if(length(list.s3.cval$Key) > 0){    
                  
                  cval.join.starttime <- Sys.time()
                  if(silent == FALSE){ pb <- txtProgressBar(0, length(list.s3.cval$Key), style = 3) }
                  
                  for(file in 1:length(list.s3.cval$Key)){
                    if(silent == FALSE){ setTxtProgressBar(pb, file) }
                    
                    pull.file <- aws.s3::s3read_using(FUN = fst::read.fst, bucket = "research-eddy-inquiry", object = list.s3.cval$Key[file])
                    
                    pull.file$uid <- paste0(as.Date(list.s3.cval$StartDateTime[file]), "_",
                                            base::substr(rand.seed$.Random.seed[[file]], start = 1, stop = 4))
                    
                    pull.join <- data.table::rbindlist(l = list(pull.join, pull.file))
                  }
                } else {
                  message(paste0("No data found for pull: ", siteID, " ", dataType," ", sensor, " data from ", startDate, " to ", endDate))
                }
                # Collect and present time stats
                cval.endtime <- Sys.time()
                cval.pull.time <- round(difftime(cval.endtime, cval.starttime, units = "secs"),2)
                if(silent == FALSE){ message("\nPull Lasted: ", cval.pull.time, " seconds...") }
                # Return data
                return(pull.join)
              }
              # Check cval sensor, if sensor was even selected
            } else {
              message(
                paste0(
                  "ERROR: Please select a cval sensor: ", sensor, 
                  "\n Allowed sensor's: ", paste0(allowed.cval, collapse = ", ")
                )
              )
            }
            # Check dataType, if dataType is not correct
          } else {
            base::message(
              base::paste0(
                "The entered SiteID is not from the approved list for your dataType, \n SiteID = ", siteID
              )
            )
          }
          }else {
              message(
                paste0(
                  "ERROR: Please enter an approved dataType: ", dataType,
                  "\n Allowed dataType's: ", paste0(allowed.datatypes, collapse = ", ")
                )
              )
            } 
        # Check dates, if date is not properly formated, provide this error.
        # } else {
        #   base::message(
        #     base::paste0(
        #       "One of the input dates is formatted incorrectly. (YYYY-MM-DD) \n",
        #       "startDate: \t", startDate,"\n",
        #       "endDate: \t",    endDate,  "\n"
        #     )) 
        # }
        # Check if any REQUIRED variables are not specified  
      } else {
        base::message(
          base::paste0(
            "ERROR: one of your input variable is NULL...",
            "\n dataType  =\t", dataType, 
            "\n SiteID    =\t", siteID, 
            "\n startDate =\t", startDate, 
            "\n endDate   =\t", endDate
          ))
      }
    }
  } else {
    base::message("Connection to research-eddy-inquiry aws bucket fail! Check your secret key is correct and that you have access to this bucket.")
  }
}

## NOT RUN
# data.grab <- read.eddy.inquiry(dataType = "2min", siteID = "KONZ",  sensor = "Li840.valves", startDate = "2020-10-25", endDate = "2020-11-23", silent = TRUE)

