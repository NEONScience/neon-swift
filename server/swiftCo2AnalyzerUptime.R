# Check how many gas Analyzer are working today

# Create blank data frame to join all the site data together
blankDataframe <- data.frame(matrix(ncol=7,nrow=0, dimnames=list(NULL, c("SiteID","FinalTime", "strm_name",
                                                                         "meanVal","maxVal","minVal","stDevVal"))))
names(blankDataframe)<- c("SiteID","FinalTime", "strm_name",
                          "meanVal","maxVal","minVal","stDevVal")
# Seasoning the blank data fram
blankDataframe <- sapply( blankDataframe, as.numeric )
blankDataframe$SiteID <- as.character(blankDataframe$SiteID)
# blankDataframe$CSAT3_diag <- as.integer(blankDataframe$CSAT3_diag)
blankDataframe$FinalTime <- lubridate::ymd_hms(blankDataframe$FinalTime)
# blankDataframe$CondensationPossible <- as.character(blankDataframe$CondensationPossible)
# blankDataframe$flagged <- as.character(blankDataframe$flagged)
# List of sites (all) to pull CSAT3/HMP155 data from
siteList <- list("BART","HARV","BLAN","SCBI","SERC",
                 "JERC","OSBS","DSNY","GUAN","LAJA",
                 "STEI","UNDE","TREE","KONA","KONZ","UKFS",
                 "ORNL","GRSM","MLBS","TALL","LENO","DELA",
                 "WOOD","NOGP","DCFS","CPER","STER","NIWO","RMNP",
                 "OAES","CLBJ","YELL","MOAB","ONAQ",
                 "JORN","SRER","WREF","ABBY","TEAK","SOAP",
                 "SJER","BONA","HEAL","DEJU","BARR","TOOL",
                 "PUUM")
# For loop to grab and join the sensor data, then rbind that data into the seasoned data frame we just prepped
i <- "CPER"
for(i in siteList){
  
  # Read in CSAT3 data
  co2Data <- fst::read.fst(paste0("/srv/shiny-server/swift/data/fst/stacked/",i,"/",i,"_co2Data_master.fst"))%>%
    tidytable::dt_filter(is.na(meanVal)==FALSE) %>%
    tidytable::dt_filter(FinalTime > Sys.Date()-2)
  
  # Join the data into this data set over and over again
  names(blankDataframe)<- c("SiteID","FinalTime", "strm_name",
                            "meanVal","maxVal","minVal","stDevVal")
  listFrames <- list(blankDataframe,co2Data)
  blankDataframe <- rbindlist(listFrames)
}

unique(blankDataframe$SiteID)


co2G2131 <- blankDataframe %>%
  filter(stringr::str_detect(string = strm_name, pattern = "G2131_CO2"))
g2131Up <- (length(unique(co2G2131$SiteID)) / 47) * 100

co2Li840 <- blankDataframe%>%
  filter(stringr::str_detect(string = strm_name, pattern = "Li840_CO2_fwMole"))
li840Up <- (length(unique(co2Li840$SiteID)) / 47) * 100

co2Li7200 <- blankDataframe%>%
  filter(stringr::str_detect(string = strm_name, pattern = "Li7200_CO2"))
li7200Up <- (length(unique(co2Li7200$SiteID)) / 47) * 100

output$G2131Uptime <- shinydashboard::renderValueBox({
  shinydashboard::valueBox(
    value = paste0(g2131Up, " %"),
    subtitle = paste0(""),
    width = 5,
    color = "orange"
  )
})
output$Li840Uptime <- shinydashboard::renderValueBox({
  shinydashboard::valueBox(
    value = paste0(li840Up, " %"),
    subtitle = paste0(""),
    width = 5,
    color = "orange"
  )
})
output$Li7200Uptime <- shinydashboard::renderValueBox({
  shinydashboard::valueBox(
    value = paste0(li7200Up, " %"),
    subtitle = paste0(""),
    width = 5,
    color = "orange"
  )
})