

qfqm_reporting = function(site = NULL, start_date = NULL, end_date = NULL){
  if(is.null(site) == TRUE){
    stop("Specify site. Format = SITE")
  }
  if(is.null(start_date) == TRUE){
    stop("Specify start_date. Format = YYYY-MM-DD")
  }
  if(is.null(end_date) == TRUE){
    stop("Specify end_date. Format = YYYY-MM-DD")
  }
  
  # The earliest QFQM data is August 8th, 2020. So we should just automatically filter the start date to be greater than 2020-08-07
  if(start_date > "2020-08-07"){
    start_date = "2020-08-08"
  }
  
  # Libraries
  library(aws.s3)
  library(dplyr)
  library(tidyr)
  library(data.table)
  library(ggplot2)
  
  # Bucket we are connecting too
  sae.bucket = "neon-sae-files"
  
  # S3 Connection
  Sys.setenv(
    "AWS_ACCESS_KEY_ID"     = sae.bucket,
    "AWS_S3_ENDPOINT"       = "neonscience.org",
    "AWS_DEFAULT_REGION"    = "s3.data"
  )
  
  # Days to query for QFQM data
  days = seq.Date(from = as.Date(start_date, origin = "1970-01-01"), to = as.Date(end_date, origin = "1970-01-01"), by = "1 day")
  # Progress Bar 
  pb <- txtProgressBar(0, length(days), style = 3)
  # Empty data.table to join to
  qfqm_out = data.table::data.table()
  for(i in seq_along(days)){
    # Set Progress Bar
    setTxtProgressBar(pb, i)
    # Convert from index to date
    day = as.Date(days[[i]], origin = "1970-01-01")
    # See what data is available
    qfqm_avail = aws.s3::get_bucket_df(bucket = sae.bucket, prefix = paste0("ods/qfqmRpt/", day, "/", site, "/")) %>% 
      dplyr::filter(stringr::str_detect(string = Key, pattern = ".rds") == TRUE) 
    # If the single RDS file is in the folder, read it in
    if(nrow(qfqm_avail) == 1){
      # Grab the ith day's QFQM report data
      qfqm_all = aws.s3::s3readRDS(object = qfqm_avail$Key[1], bucket = sae.bucket) %>% 
        dplyr::mutate(date = as.Date(date, origin = "1970-01-01")) %>% 
        dplyr::select(site, date, dp, levlTowr, metric, var, value, qmQfFinl,qfFinlTotl, qmQfAlphFinl)
      # Join to the "empty" data.table
      qfqm_out = data.table::rbindlist(l = list(qfqm_out, qfqm_all))
    }
  }

  # Plot templates
  if((Sys.Date() > Sys.Date()) == FALSE){
    # co2Stor
    qfqm_co2Stor = qfqm_out %>%
      dplyr::filter(dp == "co2Stor") %>%
      dplyr::filter(stringr::str_detect(string = levlTowr, pattern = "000_") == TRUE) %>%
      dplyr::group_by(site, date, dp, levlTowr, var) %>%
      dplyr::summarise(.groups = "drop",
        qfFinlTotl = mean(qfFinlTotl, na.rm = TRUE)
      )

    # ggplot(qfqm_co2Stor, aes(x = date, y = qfFinlTotl, color = levlTowr, group = levlTowr)) +
    #   geom_point()+
    #   geom_line()+
    #   scale_y_continuous(limits = c(0,48), breaks = c(0,10,20,30,40,48)) +
    #   scale_x_date(date_labels = "%m\n%d", date_breaks = "4 day") +
    #   facet_wrap(~var) +
    #   labs(title = paste0(qfqm_co2Stor$site[1],"'s Summary QFQM Report for ", qfqm_co2Stor$dp[1]), x = "", color = "Tower Level")


    # co2Turb
    qfqm_co2Turb = qfqm_out %>%
      dplyr::filter(dp == "co2Turb") %>%
      dplyr::filter(stringr::str_detect(string = levlTowr, pattern = "000_") == TRUE) %>%
      dplyr::group_by(site, date, dp, levlTowr, var) %>%
      dplyr::summarise(.groups = "drop",
        qfFinlTotl = mean(qfFinlTotl, na.rm = TRUE)
      )

    # ggplot(qfqm_co2Turb, aes(x = date, y = qfFinlTotl, color = var, group = var)) +
    #   geom_point()+
    #   geom_line()+
    #   scale_y_continuous(limits = c(0,48), breaks = c(0,10,20,30,40,48)) +
    #   scale_x_date(date_labels = "%m\n%d", date_breaks = "4 day") +
    #   facet_wrap(~var) +
    #   labs(title = paste0(qfqm_co2Turb$site[1],"'s Summary QFQM Report for ", qfqm_co2Turb$dp[1]), x = "", color = "Tower Level")

    # h2oStor
    qfqm_h2oStor = qfqm_out %>%
      dplyr::filter(dp == "h2oStor") %>%
      dplyr::filter(stringr::str_detect(string = levlTowr, pattern = "000_") == TRUE) %>%
      dplyr::group_by(site, date, dp, levlTowr, var) %>%
      dplyr::summarise(.groups = "drop",
        qfFinlTotl = mean(qfFinlTotl, na.rm = TRUE)
      )

    # ggplot(qfqm_h2oStor, aes(x = date, y = qfFinlTotl, color = var, group = var)) +
    #   geom_point()+
    #   geom_line()+
    #   scale_y_continuous(limits = c(0,48), breaks = c(0,10,20,30,40,48)) +
    #   scale_x_date(date_labels = "%m\n%d", date_breaks = "4 day") +
    #   facet_wrap(~var) +
    #   labs(title = paste0(qfqm_h2oStor$site[1],"'s Summary QFQM Report for ", qfqm_h2oStor$dp[1]), x = "", color = "Tower Level")

    # h2oTurb
    qfqm_h2oTurb = qfqm_out %>%
      dplyr::filter(dp == "h2oTurb") %>%
      dplyr::filter(stringr::str_detect(string = levlTowr, pattern = "000_") == TRUE) %>%
      dplyr::group_by(site, date, dp, levlTowr, var) %>%
      dplyr::summarise(.groups = "drop",
        qfFinlTotl = mean(qfFinlTotl, na.rm = TRUE)
      )

    # ggplot(qfqm_h2oTurb, aes(x = date, y = qfFinlTotl, color = var, group = var)) +
    #   geom_point()+
    #   geom_line()+
    #   scale_y_continuous(limits = c(0,48), breaks = c(0,10,20,30,40,48)) +
    #   scale_x_date(date_labels = "%m\n%d", date_breaks = "4 day") +
    #   facet_wrap(~var) +
    #   labs(title = paste0(qfqm_h2oTurb$site[1],"'s Summary QFQM Report for ", qfqm_h2oTurb$dp[1]), x = "", color = "Tower Level")


    # isoCo2
    qfqm_isoCo2 = qfqm_out %>%
      dplyr::filter(dp == "isoCo2") %>%
      dplyr::filter(stringr::str_detect(string = levlTowr, pattern = "000_") == TRUE) %>%
      dplyr::group_by(site, date, dp, levlTowr, var) %>%
      dplyr::summarise(.groups = "drop",
        qfFinlTotl = mean(qfFinlTotl, na.rm = TRUE)
      )

    # ggplot(qfqm_isoCo2, aes(x = date, y = qfFinlTotl, color = var, group = var)) +
    #   geom_point()+
    #   geom_line()+
    #   scale_y_continuous(limits = c(0,48), breaks = c(0,10,20,30,40,48)) +
    #   scale_x_date(date_labels = "%m\n%d", date_breaks = "4 day") +
    #   facet_wrap(~levlTowr) +
    #   labs(title = paste0(qfqm_isoCo2$site[1],"'s Summary QFQM Report for ", qfqm_isoCo2$dp[1]), x = "", color = "Tower Level")

    # isoH2o
    qfqm_isoH2o = qfqm_out %>%
      dplyr::filter(dp == "isoH2o") %>%
      dplyr::filter(stringr::str_detect(string = levlTowr, pattern = "000_") == TRUE) %>%
      dplyr::group_by(site, date, dp, levlTowr, var) %>%
      dplyr::summarise(.groups = "drop",
        qfFinlTotl = mean(qfFinlTotl, na.rm = TRUE)
      )

    # ggplot(qfqm_isoH2o, aes(x = date, y = qfFinlTotl, color = var, group = var)) +
    #   geom_point()+
    #   geom_line()+
    #   scale_y_continuous(limits = c(0,48), breaks = c(0,10,20,30,40,48)) +
    #   scale_x_date(date_labels = "%m\n%d", date_breaks = "4 day") +
    #   facet_wrap(~levlTowr) +
    #   labs(title = paste0(qfqm_isoH2o$site[1],"'s Summary QFQM Report for ", qfqm_isoH2o$dp[1]), x = "", color = "Tower Level")

    # soni
    qfqm_soni = qfqm_out %>%
      dplyr::filter(dp == "soni") %>%
      dplyr::filter(stringr::str_detect(string = levlTowr, pattern = "000_") == TRUE) %>%
      dplyr::group_by(site, date, dp, levlTowr, var) %>%
      dplyr::summarise(.groups = "drop",
        qfFinlTotl = mean(qfFinlTotl, na.rm = TRUE)
      )

    # ggplot(qfqm_soni, aes(x = date, y = qfFinlTotl, color = var, group = var)) +
    #   geom_point()+
    #   geom_line()+
    #   scale_y_continuous(limits = c(0,48), breaks = c(0,10,20,30,40,48)) +
    #   scale_x_date(date_labels = "%m\n%d", date_breaks = "4 day") +
    #   facet_wrap(~levlTowr) +
    #   labs(title = paste0(qfqm_soni$site[1],"'s Summary QFQM Report for ", qfqm_soni$dp[1]), x = "", color = "Tower Level")

    # fluxHeatSoil
    qfqm_fluxHeatSoil = qfqm_out %>%
      dplyr::filter(dp == "fluxHeatSoil") %>%
      tidyr::separate(col = levlTowr, into = c("sp", "location"), sep = "_") %>%
      # dplyr::filter(stringr::str_detect(string = levlTowr, pattern = "000_") == TRUE) %>%
      dplyr::group_by(site, date, dp, sp, location, var) %>%
      dplyr::summarise(.groups = "drop",
        qfFinlTotl = mean(qfFinlTotl, na.rm = TRUE)
      )

    # ggplot(qfqm_fluxHeatSoil, aes(x = date, y = qfFinlTotl, color = var, group = var)) +
    #   geom_point()+
    #   geom_line()+
    #   scale_y_continuous(limits = c(0,48), breaks = c(0,10,20,30,40,48)) +
    #   scale_x_date(date_labels = "%m\n%d", date_breaks = "4 day") +
    #   facet_grid(sp~location) +
    #   labs(title = paste0(qfqm_fluxHeatSoil$site[1],"'s Summary QFQM Report for ", qfqm_fluxHeatSoil$dp[1]), x = "", color = "Tower Level")

    # h2oSoilVol
    qfqm_h2oSoilVol = qfqm_out %>%
      dplyr::filter(dp == "h2oSoilVol") %>%
      tidyr::separate(col = levlTowr, into = c("sp", "location"), sep = "_") %>%
      # dplyr::filter(stringr::str_detect(string = levlTowr, pattern = "000_") == TRUE) %>%
      dplyr::group_by(site, date, dp, sp, location, var) %>%
      dplyr::summarise(.groups = "drop",
        qfFinlTotl = mean(qfFinlTotl, na.rm = TRUE)
      )

    # ggplot(qfqm_h2oSoilVol, aes(x = date, y = qfFinlTotl, color = var, group = var)) +
    #   geom_point()+
    #   geom_line()+
    #   scale_y_continuous(limits = c(0,48), breaks = c(0,10,20,30,40,48)) +
    #   scale_x_date(date_labels = "%m\n%d", date_breaks = "4 day") +
    #   facet_grid(sp~location) +
    #   labs(title = paste0(qfqm_h2oSoilVol$site[1],"'s Summary QFQM Report for ", qfqm_h2oSoilVol$dp[1]), x = "", color = "Tower Level")

    # h2oSoilVol
    qfqm_h2oSoilVol = qfqm_out %>%
      dplyr::filter(dp == "h2oSoilVol") %>%
      tidyr::separate(col = levlTowr, into = c("sp", "location"), sep = "_") %>%
      # dplyr::filter(stringr::str_detect(string = levlTowr, pattern = "000_") == TRUE) %>%
      dplyr::group_by(site, date, dp, sp, location, var) %>%
      dplyr::summarise(.groups = "drop",
        qfFinlTotl = mean(qfFinlTotl, na.rm = TRUE)
      )

    # ggplot(qfqm_h2oSoilVol, aes(x = date, y = qfFinlTotl, color = var, group = var)) +
    #   geom_point()+
    #   geom_line()+
    #   scale_y_continuous(limits = c(0,48), breaks = c(0,10,20,30,40,48)) +
    #   scale_x_date(date_labels = "%m\n%d", date_breaks = "4 day") +
    #   facet_grid(sp~location) +
    #   labs(title = paste0(qfqm_h2oSoilVol$site[1],"'s Summary QFQM Report for ", qfqm_h2oSoilVol$dp[1]), x = "", color = "Tower Level")

    # tempAirLvl
    qfqm_tempAirLvl = qfqm_out %>%
      dplyr::filter(dp == "tempAirLvl") %>%
      dplyr::filter(stringr::str_detect(string = levlTowr, pattern = "000_") == TRUE) %>%
      dplyr::group_by(site, date, dp, levlTowr, var) %>%
      dplyr::summarise(.groups = "drop",
        qfFinlTotl = mean(qfFinlTotl, na.rm = TRUE)
      )

    # ggplot(qfqm_tempAirLvl, aes(x = date, y = qfFinlTotl, color = var, group = var)) +
    #   geom_point()+
    #   geom_line()+
    #   scale_y_continuous(limits = c(0,48), breaks = c(0,10,20,30,40,48)) +
    #   scale_x_date(date_labels = "%m\n%d", date_breaks = "4 day") +
    #   facet_wrap(~levlTowr) +
    #   labs(title = paste0(qfqm_tempAirLvl$site[1],"'s Summary QFQM Report for ", qfqm_tempAirLvl$dp[1]), x = "", color = "Tower Level")


    # tempSoil
    qfqm_tempSoil = qfqm_out %>%
      dplyr::filter(dp == "tempSoil") %>%
      tidyr::separate(col = levlTowr, into = c("sp", "location"), sep = "_") %>%
      # dplyr::filter(stringr::str_detect(string = levlTowr, pattern = "000_") == TRUE) %>%
      dplyr::group_by(site, date, dp, sp, location, var) %>%
      dplyr::summarise(.groups = "drop",
        qfFinlTotl = mean(qfFinlTotl, na.rm = TRUE)
      )

    # ggplot(qfqm_tempSoil, aes(x = date, y = qfFinlTotl, color = var, group = var)) +
    #   geom_point()+
    #   geom_line()+
    #   scale_y_continuous(limits = c(0,48), breaks = c(0,10,20,30,40,48)) +
    #   scale_x_date(date_labels = "%m\n%d", date_breaks = "4 day") +
    #   facet_grid(sp~location) +
    #   labs(title = paste0(qfqm_tempSoil$site[1],"'s Summary QFQM Report for ", qfqm_tempSoil$dp[1]), x = "", color = "Tower Level")

    # amrs
    qfqm_amrs = qfqm_out %>%
      dplyr::filter(dp == "amrs") %>%
      dplyr::filter(stringr::str_detect(string = levlTowr, pattern = "000_") == TRUE) %>%
      dplyr::group_by(site, date, dp, levlTowr, var) %>%
      dplyr::summarise(.groups = "drop",
        qfFinlTotl = mean(qfFinlTotl, na.rm = TRUE)
      )

    # ggplot(qfqm_amrs, aes(x = date, y = qfFinlTotl, color = var, group = var)) +
    #   geom_point()+
    #   geom_line()+
    #   scale_y_continuous(limits = c(0,48), breaks = c(0,10,20,30,40,48)) +
    #   scale_x_date(date_labels = "%m\n%d", date_breaks = "4 day") +
    #   facet_wrap(~levlTowr) +
    #   labs(title = paste0(qfqm_amrs$site[1],"'s Summary QFQM Report for ", qfqm_amrs$dp[1]), x = "", color = "Tower Level")

      # radiNet
    qfqm_radiNet = qfqm_out %>%
      dplyr::filter(dp == "radiNet") %>%
      dplyr::filter(stringr::str_detect(string = levlTowr, pattern = "000_") == TRUE) %>%
      dplyr::group_by(site, date, dp, levlTowr, var) %>%
      dplyr::summarise(.groups = "drop",
        qfFinlTotl = mean(qfFinlTotl, na.rm = TRUE)
      )

    # ggplot(qfqm_radiNet, aes(x = date, y = qfFinlTotl, color = var, group = var)) +
    #   geom_point()+
    #   geom_line()+
    #   scale_y_continuous(limits = c(0,48), breaks = c(0,10,20,30,40,48)) +
    #   scale_x_date(date_labels = "%m\n%d", date_breaks = "4 day") +
    #   facet_wrap(~levlTowr) +
    #   labs(title = paste0(qfqm_radiNet$site[1],"'s Summary QFQM Report for ", qfqm_radiNet$dp[1]), x = "", color = "Tower Level")

      # tempAirTop
    qfqm_tempAirTop = qfqm_out %>%
      dplyr::filter(dp == "tempAirTop") %>%
      dplyr::filter(stringr::str_detect(string = levlTowr, pattern = "000_") == TRUE) %>%
      dplyr::group_by(site, date, dp, levlTowr, var) %>%
      dplyr::summarise(.groups = "drop",
        qfFinlTotl = mean(qfFinlTotl, na.rm = TRUE)
      )

    # ggplot(qfqm_tempAirTop, aes(x = date, y = qfFinlTotl, color = var, group = var)) +
    #   geom_point()+
    #   geom_line()+
    #   scale_y_continuous(limits = c(0,48), breaks = c(0,10,20,30,40,48)) +
    #   scale_x_date(date_labels = "%m\n%d", date_breaks = "4 day") +
    #   facet_wrap(~levlTowr) +
    #   labs(title = paste0(qfqm_tempAirTop$site[1],"'s Summary QFQM Report for ", qfqm_tempAirTop$dp[1]), x = "", color = "Tower Level")

    rm(pb, qfqm_all, qfqm_avail)
    dfs <- Filter(function(x) is(x, "data.frame"), mget(ls()))
    
  # Return the final output file
  return(dfs)
  }
    
}
data = qfqm_reporting(site = "TREE", start_date = "2020-01-01", end_date = "2021-01-01")


test = data %>%
  dplyr::filter(dp == "co2Stor") %>%
  dplyr::filter(stringr::str_detect(string = levlTowr, pattern = "000_") == TRUE) %>%
  dplyr::group_by(site, date, dp, levlTowr, var) %>%
  dplyr::summarise(.groups = "drop",
    qfFinlTotl = mean(qfFinlTotl, na.rm = TRUE)
  )
