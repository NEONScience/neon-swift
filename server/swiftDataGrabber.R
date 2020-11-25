# DropBoxDataGrabber.R
# Kevin Styers
# Backend Code for data pull

# Script to grab data from DropBox


library(rdrop2)

# SOM Code
local_SOM_path <- "/srv/shiny-server/swift/data/"
local_SOM_image_path <- "/srv/shiny-server/swift/www/"
image_path <- "SwiftData/phenoImages"
local_SOM_token_path <- "~/GitHub/swift/SwiftShinyDash/data/token.rds"

# Kev Code
local_KS_Path <- "/srv/shiny-server/swift/data/"
fastPath <- "/srv/shiny-server/swift/data/fst/"
local_KS_token_Path <-  "/srv/shiny-server/swift/data/token.rds"

drop_auth(rdstoken = local_KS_token_Path) # token is in C:/GitHub/fieldscience_collab/TIS/swift/shinydash/SwiftShinyDash/data

# Downloads the Li-7200 Validation Data
# drop_download(path = "SwiftData/field7200.rds", 
#               local_path =  local_KS_Path, overwrite = TRUE)

# Downloads the Aquatics Pursley Data
drop_download(path = "SwiftData/swiftAqua.rds", 
              local_path =  local_KS_Path,overwrite = TRUE)

# Downloads the CO2 Pursley Data
# drop_download(path = "SwiftData/co2MASTERFull.rds", 
#               local_path =  local_KS_Path,overwrite = TRUE)

# Download the Master list of Presto CVAL dates
# drop_download(path = "SwiftData/co2NaN.rds", 
#               local_path =  local_KS_Path,overwrite = TRUE)

# Download the Master list of Presto CVAL dates
# drop_download(path = "SwiftData/co2NaN180.rds", 
#               local_path =  local_KS_Path,overwrite = TRUE)

# Downloads the Cylinder Assay Pursley Data
# drop_download(path = "SwiftData/cylAssayMASTER.rds", 
#               local_path =  local_KS_Path,overwrite = TRUE)

# Downloads the CnC Dan Allen Data
# drop_download(path = "SwiftData/cncData.rds", 
#               local_path =  local_KS_Path,overwrite = TRUE)

# Downloads the Gas Cylinder Pursley Data
# drop_download(path = "SwiftData/gasMASTER.rds", 
#               local_path =  local_KS_Path,overwrite = TRUE)

# Downloads the non-7200 gas sensor Cal/Val data from the past 8 days Data
# drop_download(path = "SwiftData/calValLast8Days.rds", 
#               local_path =  local_KS_Path,overwrite = TRUE)

# Download the Master list of Presto CVAL dates
# drop_download(path = "SwiftData/LastCalValDates.rds", 
#               local_path =  local_KS_Path,overwrite = TRUE)

# Download the Master list of L2131 Data
# drop_download(path = "SwiftData/l2131MASTER.rds", 
#               local_path =  local_KS_Path,overwrite = TRUE)

# Download the NEWWWW Master list of L2131 Data
# drop_download(path = "SwiftData/L2130NEWBatch.rds", 
#               local_path =  local_KS_Path,overwrite = TRUE)
# Download the EC_Plots RDS
# drop_download(path = "SwiftData/EC_Data.rds",
#               local_path = local_KS_Path, overwrite = TRUE)
# Download the EC_Plots Water RDS
# drop_download(path = "SwiftData/EC_WaterData.rds",
#               local_path = local_KS_Path, overwrite = TRUE)

# Download the EC_Plots Pressure Diff RDS
# drop_download(path = "SwiftData/ECDataInputPressureDiff.rds",
#               local_path = local_KS_Path, overwrite = TRUE)

# Download the co2 CVAL data
# drop_download(path = "SwiftData/co2CVAL.rds",
#               local_path = local_KS_Path, overwrite = TRUE)

# drop_download(path = "SwiftData/fieldNot7200.rds",
#               local_path = local_KS_Path, overwrite = TRUE)

# Li840 "flagged" data
# drop_download(path = "SwiftData/swiftEC840Data.rds",
#               local_path = local_KS_Path, overwrite = TRUE)

# Download Tip Bucket Data
# drop_download(path = "SwiftData/tipData.rds",
#               local_path = local_KS_Path, overwrite = TRUE)

# drop_download(path = "SwiftData/lidData.rds",
#               local_path = local_KS_Path, overwrite = TRUE)
# Download Time Stamp Check Data
drop_download(path = "SwiftData/swiftTimeCheck.rds",
              local_path = local_KS_Path, overwrite = TRUE)
# Download Time Stamp Check Data
# drop_download(path = "SwiftData/fst/smallLi7200LastValidationTime.fst",
#               local_path = fastPath, overwrite = TRUE)
# drop_download(path = "SwiftData/fst/smallG2131iLastValidationTime.fst",
#               local_path = fastPath, overwrite = TRUE)
# drop_download(path = "SwiftData/fst/smallLi840LastCalibrationTime.fst",
#               local_path = fastPath, overwrite = TRUE)
# drop_download(path = "SwiftData/fst/smallLi840LastValidationTime.fst",
#               local_path = fastPath, overwrite = TRUE)
# drop_download(path = "SwiftData/fst/co2Fast.fst",
#               local_path = fastPath, overwrite = TRUE)
# drop_download(path = "SwiftData/fst/h2oValFast.fst",
#               local_path = fastPath, overwrite = TRUE)

cvalSiteFileList <- readRDS("/srv/shiny-server/swift/data/lookup/cvalSiteFileList.rds")
length(cvalSiteFileList)
cvalSiteFileList[71]
for(i in cvalSiteFileList[8:251]){
  # upload each file in the list.
  drop_download(path = paste0("cval/", i ),
                local_path = "/srv/shiny-server/swift/data/fst/cval", overwrite = TRUE)
}
drop_download(path = "cval/spanGasConc.fst",
              local_path = "/srv/shiny-server/swift/data/fst/cval", overwrite = TRUE)
