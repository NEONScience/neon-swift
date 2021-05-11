

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
  if(start_date < "2020-08-07"){
    start_date = "2020-08-08"
  }
  
  # Libraries
  library(aws.s3)
  library(dplyr)
  library(tidyr)
  library(data.table)
  library(ggplot2)
  
  # Special functions
  `%not%` = Negate(`%in%`)
  
  
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
  
  if(nrow(qfqm_out) > 0){
  
  # Plot templates
    if((Sys.Date() > Sys.Date()) == FALSE){
      
      # browser()
      
      dates_expected = seq.Date(from = min(qfqm_out$date, na.rm = TRUE), to = max(qfqm_out$date, na.rm = TRUE), by = "1 day")
      # co2Stor
      co2Stor = qfqm_out %>%
        dplyr::filter(dp == "co2Stor") %>%
        dplyr::filter(stringr::str_detect(string = levlTowr, pattern = "000_") == TRUE) %>%
        dplyr::group_by(site, date, dp, levlTowr, var) %>%
        dplyr::summarise(.groups = "drop",
          qfFinlTotl = mean(qfFinlTotl, na.rm = TRUE)
        ) %>% 
        tidyr::unite(col = "uid", c(site, dp, levlTowr, var), sep = "_", remove = FALSE) 
      
      # 3 potentials, no data, some data, all data
      # browser()
      # Some data or all data
      if(nrow(co2Stor) > 0){
        for(i in unique(co2Stor$uid)){
          data_testing = co2Stor %>% 
            dplyr::filter(uid == i)
          missing_dates = data.table::data.table(dates = dates_expected, found = dates_expected %not% data_testing$date) %>% 
            dplyr::filter(found == TRUE)
          if(nrow(missing_dates) > 0){
            # Values to add to new table
            site_var     = data_testing$site[1]
            dp_var       = data_testing$dp[1]
            levlTowr_var = data_testing$levlTowr[1]
            var_var      = data_testing$var[1]
            qf_var       = 0
            # Add 0's to missing data 
            for(j in missing_dates$dates){
              missing_data = data.table::data.table(
                uid = i, 
                site = site_var, 
                date = as.Date(j, origin = "1970-01-01"), 
                dp = dp_var,
                levlTowr = levlTowr_var,
                var = var_var,
                qfFinlTotl = qf_var)
              co2Stor = data.table::rbindlist(l = list(co2Stor, missing_data))
            }
          }
        }
      } 
      
      # ggplot(qfqm_co2Stor, aes(x = date, y = qfFinlTotl, color = levlTowr, group = levlTowr)) +
      #   geom_point()+
      #   geom_line()+
      #   scale_y_continuous(limits = c(0,48), breaks = c(0,10,20,30,40,48)) +
      #   scale_x_date(date_labels = "%m\n%d", date_breaks = "4 day") +
      #   facet_wrap(~var) +
      #   labs(title = paste0(qfqm_co2Stor$site[1],"'s Summary QFQM Report for ", qfqm_co2Stor$dp[1]), x = "", color = "Tower Level")
      
      
      # co2Turb
      co2Turb = qfqm_out %>%
        dplyr::filter(dp == "co2Turb") %>%
        dplyr::filter(stringr::str_detect(string = levlTowr, pattern = "000_") == TRUE) %>%
        dplyr::group_by(site, date, dp, levlTowr, var) %>%
        dplyr::summarise(.groups = "drop",
                         qfFinlTotl = mean(qfFinlTotl, na.rm = TRUE)
        ) %>% 
        tidyr::unite(col = "uid", c(site, dp, levlTowr, var), sep = "_", remove = FALSE) 
      
      if(nrow(co2Turb) > 0){
        for(i in unique(co2Turb$uid)){
          data_testing = co2Turb %>% 
            dplyr::filter(uid == i)
          missing_dates = data.table::data.table(dates = dates_expected, found = dates_expected %not% data_testing$date) %>% 
            dplyr::filter(found == TRUE)
          if(nrow(missing_dates) > 0){
            # Values to add to new table
            site_var     = data_testing$site[1]
            dp_var       = data_testing$dp[1]
            levlTowr_var = data_testing$levlTowr[1]
            var_var      = data_testing$var[1]
            qf_var       = 0
            # Add 0's to missing data 
            for(j in missing_dates$dates){
              missing_data = data.table::data.table(
                uid = i, 
                site = site_var, 
                date = as.Date(j, origin = "1970-01-01"), 
                dp = dp_var,
                levlTowr = levlTowr_var,
                var = var_var,
                qfFinlTotl = qf_var)
              co2Turb = data.table::rbindlist(l = list(co2Turb, missing_data))
            }
          }
        }
      } 
      
      # ggplot(qfqm_co2Turb, aes(x = date, y = qfFinlTotl, color = var, group = var)) +
      #   geom_point()+
      #   geom_line()+
      #   scale_y_continuous(limits = c(0,48), breaks = c(0,10,20,30,40,48)) +
      #   scale_x_date(date_labels = "%m\n%d", date_breaks = "4 day") +
      #   facet_wrap(~var) +
      #   labs(title = paste0(qfqm_co2Turb$site[1],"'s Summary QFQM Report for ", qfqm_co2Turb$dp[1]), x = "", color = "Tower Level")
      
      # h2oStor
      h2oStor = qfqm_out %>%
        dplyr::filter(dp == "h2oStor") %>%
        dplyr::filter(stringr::str_detect(string = levlTowr, pattern = "000_") == TRUE) %>%
        dplyr::group_by(site, date, dp, levlTowr, var) %>%
        dplyr::summarise(.groups = "drop",
                         qfFinlTotl = mean(qfFinlTotl, na.rm = TRUE)
        ) %>% 
        tidyr::unite(col = "uid", c(site, dp, levlTowr, var), sep = "_", remove = FALSE) 
      
      if(nrow(h2oStor) > 0){
        for(i in unique(h2oStor$uid)){
          data_testing = h2oStor %>% 
            dplyr::filter(uid == i)
          missing_dates = data.table::data.table(dates = dates_expected, found = dates_expected %not% data_testing$date) %>% 
            dplyr::filter(found == TRUE)
          if(nrow(missing_dates) > 0){
            # Values to add to new table
            site_var     = data_testing$site[1]
            dp_var       = data_testing$dp[1]
            levlTowr_var = data_testing$levlTowr[1]
            var_var      = data_testing$var[1]
            qf_var       = 0
            # Add 0's to missing data 
            for(j in missing_dates$dates){
              missing_data = data.table::data.table(
                uid = i, 
                site = site_var, 
                date = as.Date(j, origin = "1970-01-01"), 
                dp = dp_var,
                levlTowr = levlTowr_var,
                var = var_var,
                qfFinlTotl = qf_var)
              h2oStor = data.table::rbindlist(l = list(h2oStor, missing_data))
            }
          }
        }
      } 
      
      # ggplot(qfqm_h2oStor, aes(x = date, y = qfFinlTotl, color = var, group = var)) +
      #   geom_point()+
      #   geom_line()+
      #   scale_y_continuous(limits = c(0,48), breaks = c(0,10,20,30,40,48)) +
      #   scale_x_date(date_labels = "%m\n%d", date_breaks = "4 day") +
      #   facet_wrap(~var) +
      #   labs(title = paste0(qfqm_h2oStor$site[1],"'s Summary QFQM Report for ", qfqm_h2oStor$dp[1]), x = "", color = "Tower Level")
      
      # h2oTurb
      h2oTurb = qfqm_out %>%
        dplyr::filter(dp == "h2oTurb") %>%
        dplyr::filter(stringr::str_detect(string = levlTowr, pattern = "000_") == TRUE) %>%
        dplyr::group_by(site, date, dp, levlTowr, var) %>%
        dplyr::summarise(.groups = "drop",
                         qfFinlTotl = mean(qfFinlTotl, na.rm = TRUE)
        ) %>% 
        tidyr::unite(col = "uid", c(site, dp, levlTowr, var), sep = "_", remove = FALSE) 
      
      
      if(nrow(h2oTurb) > 0){
        for(i in unique(h2oTurb$uid)){
          data_testing = h2oTurb %>% 
            dplyr::filter(uid == i)
          missing_dates = data.table::data.table(dates = dates_expected, found = dates_expected %not% data_testing$date) %>% 
            dplyr::filter(found == TRUE)
          if(nrow(missing_dates) > 0){
            # Values to add to new table
            site_var     = data_testing$site[1]
            dp_var       = data_testing$dp[1]
            levlTowr_var = data_testing$levlTowr[1]
            var_var      = data_testing$var[1]
            qf_var       = 0
            # Add 0's to missing data 
            for(j in missing_dates$dates){
              missing_data = data.table::data.table(
                uid = i, 
                site = site_var, 
                date = as.Date(j, origin = "1970-01-01"), 
                dp = dp_var,
                levlTowr = levlTowr_var,
                var = var_var,
                qfFinlTotl = qf_var)
              h2oTurb = data.table::rbindlist(l = list(h2oTurb, missing_data))
            }
          }
        }
      } 
      
      # ggplot(qfqm_h2oTurb, aes(x = date, y = qfFinlTotl, color = var, group = var)) +
      #   geom_point()+
      #   geom_line()+
      #   scale_y_continuous(limits = c(0,48), breaks = c(0,10,20,30,40,48)) +
      #   scale_x_date(date_labels = "%m\n%d", date_breaks = "4 day") +
      #   facet_wrap(~var) +
      #   labs(title = paste0(qfqm_h2oTurb$site[1],"'s Summary QFQM Report for ", qfqm_h2oTurb$dp[1]), x = "", color = "Tower Level")
      
      
      # isoCo2
      isoCo2 = qfqm_out %>%
        dplyr::filter(dp == "isoCo2") %>%
        dplyr::filter(stringr::str_detect(string = levlTowr, pattern = "000_") == TRUE) %>%
        dplyr::group_by(site, date, dp, levlTowr, var) %>%
        dplyr::summarise(.groups = "drop",
                         qfFinlTotl = mean(qfFinlTotl, na.rm = TRUE)
        ) %>% 
        tidyr::unite(col = "uid", c(site, dp, levlTowr, var), sep = "_", remove = FALSE) 
      
      if(nrow(isoCo2) > 0){
        for(i in unique(isoCo2$uid)){
          data_testing = isoCo2 %>% 
            dplyr::filter(uid == i)
          missing_dates = data.table::data.table(dates = dates_expected, found = dates_expected %not% data_testing$date) %>% 
            dplyr::filter(found == TRUE)
          if(nrow(missing_dates) > 0){
            # Values to add to new table
            site_var     = data_testing$site[1]
            dp_var       = data_testing$dp[1]
            levlTowr_var = data_testing$levlTowr[1]
            var_var      = data_testing$var[1]
            qf_var       = 0
            # Add 0's to missing data 
            for(j in missing_dates$dates){
              missing_data = data.table::data.table(
                uid = i, 
                site = site_var, 
                date = as.Date(j, origin = "1970-01-01"), 
                dp = dp_var,
                levlTowr = levlTowr_var,
                var = var_var,
                qfFinlTotl = qf_var)
              isoCo2 = data.table::rbindlist(l = list(isoCo2, missing_data))
            }
          }
        }
      } 
      
      # ggplot(qfqm_isoCo2, aes(x = date, y = qfFinlTotl, color = var, group = var)) +
      #   geom_point()+
      #   geom_line()+
      #   scale_y_continuous(limits = c(0,48), breaks = c(0,10,20,30,40,48)) +
      #   scale_x_date(date_labels = "%m\n%d", date_breaks = "4 day") +
      #   facet_wrap(~levlTowr) +
      #   labs(title = paste0(qfqm_isoCo2$site[1],"'s Summary QFQM Report for ", qfqm_isoCo2$dp[1]), x = "", color = "Tower Level")
      
      # isoH2o
      isoH2o = qfqm_out %>%
        dplyr::filter(dp == "isoH2o") %>%
        dplyr::filter(stringr::str_detect(string = levlTowr, pattern = "000_") == TRUE) %>%
        dplyr::group_by(site, date, dp, levlTowr, var) %>%
        dplyr::summarise(.groups = "drop",
                         qfFinlTotl = mean(qfFinlTotl, na.rm = TRUE)
        ) %>% 
        tidyr::unite(col = "uid", c(site, dp, levlTowr, var), sep = "_", remove = FALSE) 
      
      if(nrow(isoH2o) > 0){
        for(i in unique(isoH2o$uid)){
          data_testing = isoH2o %>% 
            dplyr::filter(uid == i)
          missing_dates = data.table::data.table(dates = dates_expected, found = dates_expected %not% data_testing$date) %>% 
            dplyr::filter(found == TRUE)
          if(nrow(missing_dates) > 0){
            # Values to add to new table
            site_var     = data_testing$site[1]
            dp_var       = data_testing$dp[1]
            levlTowr_var = data_testing$levlTowr[1]
            var_var      = data_testing$var[1]
            qf_var       = 0
            # Add 0's to missing data 
            for(j in missing_dates$dates){
              missing_data = data.table::data.table(
                uid = i, 
                site = site_var, 
                date = as.Date(j, origin = "1970-01-01"), 
                dp = dp_var,
                levlTowr = levlTowr_var,
                var = var_var,
                qfFinlTotl = qf_var)
              isoH2o = data.table::rbindlist(l = list(isoH2o, missing_data))
            }
          }
        }
      } 
      
      # ggplot(qfqm_isoH2o, aes(x = date, y = qfFinlTotl, color = var, group = var)) +
      #   geom_point()+
      #   geom_line()+
      #   scale_y_continuous(limits = c(0,48), breaks = c(0,10,20,30,40,48)) +
      #   scale_x_date(date_labels = "%m\n%d", date_breaks = "4 day") +
      #   facet_wrap(~levlTowr) +
      #   labs(title = paste0(qfqm_isoH2o$site[1],"'s Summary QFQM Report for ", qfqm_isoH2o$dp[1]), x = "", color = "Tower Level")
      
      # soni
      soni = qfqm_out %>%
        dplyr::filter(dp == "soni") %>%
        dplyr::filter(stringr::str_detect(string = levlTowr, pattern = "000_") == TRUE) %>%
        dplyr::group_by(site, date, dp, levlTowr, var) %>%
        dplyr::summarise(.groups = "drop",
                         qfFinlTotl = mean(qfFinlTotl, na.rm = TRUE)
        ) %>% 
        tidyr::unite(col = "uid", c(site, dp, levlTowr, var), sep = "_", remove = FALSE) 
      
      if(nrow(soni) > 0){
        for(i in unique(soni$uid)){
          data_testing = soni %>% 
            dplyr::filter(uid == i)
          missing_dates = data.table::data.table(dates = dates_expected, found = dates_expected %not% data_testing$date) %>% 
            dplyr::filter(found == TRUE)
          if(nrow(missing_dates) > 0){
            # Values to add to new table
            site_var     = data_testing$site[1]
            dp_var       = data_testing$dp[1]
            levlTowr_var = data_testing$levlTowr[1]
            var_var      = data_testing$var[1]
            qf_var       = 0
            # Add 0's to missing data 
            for(j in missing_dates$dates){
              missing_data = data.table::data.table(
                uid = i, 
                site = site_var, 
                date = as.Date(j, origin = "1970-01-01"), 
                dp = dp_var,
                levlTowr = levlTowr_var,
                var = var_var,
                qfFinlTotl = qf_var)
              soni = data.table::rbindlist(l = list(soni, missing_data))
            }
          }
        }
      } 
      
      # ggplot(qfqm_soni, aes(x = date, y = qfFinlTotl, color = var, group = var)) +
      #   geom_point()+
      #   geom_line()+
      #   scale_y_continuous(limits = c(0,48), breaks = c(0,10,20,30,40,48)) +
      #   scale_x_date(date_labels = "%m\n%d", date_breaks = "4 day") +
      #   facet_wrap(~levlTowr) +
      #   labs(title = paste0(qfqm_soni$site[1],"'s Summary QFQM Report for ", qfqm_soni$dp[1]), x = "", color = "Tower Level")
      
      # fluxHeatSoil
      fluxHeatSoil = qfqm_out %>%
        dplyr::filter(dp == "fluxHeatSoil") %>%
        tidyr::separate(col = levlTowr, into = c("sp", "location"), sep = "_") %>%
        # dplyr::filter(stringr::str_detect(string = levlTowr, pattern = "000_") == TRUE) %>%
        dplyr::group_by(site, date, dp, sp, location, var) %>%
        dplyr::summarise(.groups = "drop",
                         qfFinlTotl = mean(qfFinlTotl, na.rm = TRUE)
        ) %>% 
        tidyr::unite(col = "uid", c(site, dp, sp, location, var), sep = "_", remove = FALSE) 
      
      if(nrow(fluxHeatSoil) > 0){
        for(i in unique(fluxHeatSoil$uid)){
          data_testing = fluxHeatSoil %>% 
            dplyr::filter(uid == i)
          missing_dates = data.table::data.table(dates = dates_expected, found = dates_expected %not% data_testing$date) %>% 
            dplyr::filter(found == TRUE)
          if(nrow(missing_dates) > 0){
            # Values to add to new table
            site_var     = data_testing$site[1]
            dp_var       = data_testing$dp[1]
            sp_var       = data_testing$sp[1]
            location_var = data_testing$location[1]
            var_var      = data_testing$var[1]
            qf_var       = 0
            # Add 0's to missing data 
            for(j in missing_dates$dates){
              missing_data = data.table::data.table(
                uid = i, 
                site = site_var, 
                date = as.Date(j, origin = "1970-01-01"), 
                dp = dp_var,
                sp = sp_var,
                location = location_var,
                var = var_var,
                qfFinlTotl = qf_var)
              fluxHeatSoil = data.table::rbindlist(l = list(fluxHeatSoil, missing_data))
            }
          }
        }
      } 
      
      
      # ggplot(qfqm_fluxHeatSoil, aes(x = date, y = qfFinlTotl, color = var, group = var)) +
      #   geom_point()+
      #   geom_line()+
      #   scale_y_continuous(limits = c(0,48), breaks = c(0,10,20,30,40,48)) +
      #   scale_x_date(date_labels = "%m\n%d", date_breaks = "4 day") +
      #   facet_grid(sp~location) +
      #   labs(title = paste0(qfqm_fluxHeatSoil$site[1],"'s Summary QFQM Report for ", qfqm_fluxHeatSoil$dp[1]), x = "", color = "Tower Level")
      
      # h2oSoilVol
      h2oSoilVol = qfqm_out %>%
        dplyr::filter(dp == "h2oSoilVol") %>%
        tidyr::separate(col = levlTowr, into = c("sp", "location"), sep = "_") %>%
        # dplyr::filter(stringr::str_detect(string = levlTowr, pattern = "000_") == TRUE) %>%
        dplyr::group_by(site, date, dp, sp, location, var) %>%
        dplyr::summarise(.groups = "drop",
                         qfFinlTotl = mean(qfFinlTotl, na.rm = TRUE)
        ) %>% 
        tidyr::unite(col = "uid", c(site, dp, sp, location, var), sep = "_", remove = FALSE) 
      
      if(nrow(h2oSoilVol) > 0){
        for(i in unique(h2oSoilVol$uid)){
          data_testing = h2oSoilVol %>% 
            dplyr::filter(uid == i)
          missing_dates = data.table::data.table(dates = dates_expected, found = dates_expected %not% data_testing$date) %>% 
            dplyr::filter(found == TRUE)
          if(nrow(missing_dates) > 0){
            # Values to add to new table
            site_var     = data_testing$site[1]
            dp_var       = data_testing$dp[1]
            sp_var       = data_testing$sp[1]
            location_var = data_testing$location[1]
            var_var      = data_testing$var[1]
            qf_var       = 0
            # Add 0's to missing data 
            for(j in missing_dates$dates){
              missing_data = data.table::data.table(
                uid = i, 
                site = site_var, 
                date = as.Date(j, origin = "1970-01-01"), 
                dp = dp_var,
                sp = sp_var,
                location = location_var,
                var = var_var,
                qfFinlTotl = qf_var)
              h2oSoilVol = data.table::rbindlist(l = list(h2oSoilVol, missing_data))
            }
          }
        }
      }     
      # ggplot(qfqm_h2oSoilVol, aes(x = date, y = qfFinlTotl, color = var, group = var)) +
      #   geom_point()+
      #   geom_line()+
      #   scale_y_continuous(limits = c(0,48), breaks = c(0,10,20,30,40,48)) +
      #   scale_x_date(date_labels = "%m\n%d", date_breaks = "4 day") +
      #   facet_grid(sp~location) +
      #   labs(title = paste0(qfqm_h2oSoilVol$site[1],"'s Summary QFQM Report for ", qfqm_h2oSoilVol$dp[1]), x = "", color = "Tower Level")
      
      # tempAirLvl
      tempAirLvl = qfqm_out %>%
        dplyr::filter(dp == "tempAirLvl") %>%
        dplyr::filter(stringr::str_detect(string = levlTowr, pattern = "000_") == TRUE) %>%
        dplyr::group_by(site, date, dp, levlTowr, var) %>%
        dplyr::summarise(.groups = "drop",
                         qfFinlTotl = mean(qfFinlTotl, na.rm = TRUE)
        ) %>% 
        tidyr::unite(col = "uid", c(site, dp, levlTowr, var), sep = "_", remove = FALSE) 
      
      if(nrow(tempAirLvl) > 0){
        for(i in unique(tempAirLvl$uid)){
          data_testing = tempAirLvl %>% 
            dplyr::filter(uid == i)
          missing_dates = data.table::data.table(dates = dates_expected, found = dates_expected %not% data_testing$date) %>% 
            dplyr::filter(found == TRUE)
          if(nrow(missing_dates) > 0){
           # Values to add to new table
            site_var     = data_testing$site[1]
            dp_var       = data_testing$dp[1]
            levlTowr_var = data_testing$levlTowr[1]
            var_var      = data_testing$var[1]
            qf_var       = 0
        # Add 0's to missing data 
            for(j in missing_dates$dates){  
              missing_data = data.table::data.table(
                uid = i, 
                site = site_var, 
                date = as.Date(j, origin = "1970-01-01"), 
                dp = dp_var,
                levlTowr = levlTowr_var,
                var = var_var,
                qfFinlTotl = qf_var)
                  tempAirLvl = data.table::rbindlist(l = list(tempAirLvl, missing_data))
            }
          }
        }
      } 
      # ggplot(qfqm_tempAirLvl, aes(x = date, y = qfFinlTotl, color = var, group = var)) +
      #   geom_point()+
      #   geom_line()+
      #   scale_y_continuous(limits = c(0,48), breaks = c(0,10,20,30,40,48)) +
      #   scale_x_date(date_labels = "%m\n%d", date_breaks = "4 day") +
      #   facet_wrap(~levlTowr) +
      #   labs(title = paste0(qfqm_tempAirLvl$site[1],"'s Summary QFQM Report for ", qfqm_tempAirLvl$dp[1]), x = "", color = "Tower Level")
      
      
      # tempSoil
      tempSoil = qfqm_out %>%
        dplyr::filter(dp == "tempSoil") %>%
        tidyr::separate(col = levlTowr, into = c("sp", "location"), sep = "_") %>%
        # dplyr::filter(stringr::str_detect(string = levlTowr, pattern = "000_") == TRUE) %>%
        dplyr::group_by(site, date, dp, sp, location, var) %>%
        dplyr::summarise(.groups = "drop",
                         qfFinlTotl = mean(qfFinlTotl, na.rm = TRUE)
        ) %>% 
        tidyr::unite(col = "uid", c(site, dp, sp, location, var), sep = "_", remove = FALSE) 
      
      if(nrow(tempSoil) > 0){
        for(i in unique(tempSoil$uid)){
          data_testing = tempSoil %>% 
            dplyr::filter(uid == i)
          missing_dates = data.table::data.table(dates = dates_expected, found = dates_expected %not% data_testing$date) %>% 
            dplyr::filter(found == TRUE)
          if(nrow(missing_dates) > 0){
            # Values to add to new table
            site_var     = data_testing$site[1]
            dp_var       = data_testing$dp[1]
            sp_var       = data_testing$sp[1]
            location_var = data_testing$location[1]
            var_var      = data_testing$var[1]
            qf_var       = 0
            # Add 0's to missing data 
            for(j in missing_dates$dates){
              missing_data = data.table::data.table(
                uid = i, 
                site = site_var, 
                date = as.Date(j, origin = "1970-01-01"), 
                dp = dp_var,
                sp = sp_var,
                location = location_var,
                var = var_var,
                qfFinlTotl = qf_var)
              tempSoil = data.table::rbindlist(l = list(tempSoil, missing_data))
            }
          }
        }
      } 
      
      # ggplot(qfqm_tempSoil, aes(x = date, y = qfFinlTotl, color = var, group = var)) +
      #   geom_point()+
      #   geom_line()+
      #   scale_y_continuous(limits = c(0,48), breaks = c(0,10,20,30,40,48)) +
      #   scale_x_date(date_labels = "%m\n%d", date_breaks = "4 day") +
      #   facet_grid(sp~location) +
      #   labs(title = paste0(qfqm_tempSoil$site[1],"'s Summary QFQM Report for ", qfqm_tempSoil$dp[1]), x = "", color = "Tower Level")
      
      # amrs
      amrs = qfqm_out %>%
        dplyr::filter(dp == "amrs") %>%
        dplyr::filter(stringr::str_detect(string = levlTowr, pattern = "000_") == TRUE) %>%
        dplyr::group_by(site, date, dp, levlTowr, var) %>%
        dplyr::summarise(.groups = "drop",
                         qfFinlTotl = mean(qfFinlTotl, na.rm = TRUE)
        ) %>% 
        tidyr::unite(col = "uid", c(site, dp, levlTowr, var), sep = "_", remove = FALSE) 
      
      if(nrow(amrs) > 0){
        for(i in unique(amrs$uid)){
          data_testing = amrs %>% 
            dplyr::filter(uid == i)
          missing_dates = data.table::data.table(dates = dates_expected, found = dates_expected %not% data_testing$date) %>% 
            dplyr::filter(found == TRUE)
          if(nrow(missing_dates) > 0){
            # Values to add to new table
            site_var     = data_testing$site[1]
            dp_var       = data_testing$dp[1]
            levlTowr_var = data_testing$levlTowr[1]
            var_var      = data_testing$var[1]
            qf_var       = 0
            # Add 0's to missing data 
            for(j in missing_dates$dates){
              missing_data = data.table::data.table(
                uid = i, 
                site = site_var, 
                date = as.Date(j, origin = "1970-01-01"), 
                dp = dp_var,
                levlTowr = levlTowr_var,
                var = var_var,
                qfFinlTotl = qf_var)
              amrs = data.table::rbindlist(l = list(amrs, missing_data))
            }
          }
        }
      }  
      
      # ggplot(qfqm_amrs, aes(x = date, y = qfFinlTotl, color = var, group = var)) +
      #   geom_point()+
      #   geom_line()+
      #   scale_y_continuous(limits = c(0,48), breaks = c(0,10,20,30,40,48)) +
      #   scale_x_date(date_labels = "%m\n%d", date_breaks = "4 day") +
      #   facet_wrap(~levlTowr) +
      #   labs(title = paste0(qfqm_amrs$site[1],"'s Summary QFQM Report for ", qfqm_amrs$dp[1]), x = "", color = "Tower Level")
      
      # radiNet
      radiNet = qfqm_out %>%
        dplyr::filter(dp == "radiNet") %>%
        dplyr::filter(stringr::str_detect(string = levlTowr, pattern = "000_") == TRUE) %>%
        dplyr::group_by(site, date, dp, levlTowr, var) %>%
        dplyr::summarise(.groups = "drop",
                         qfFinlTotl = mean(qfFinlTotl, na.rm = TRUE)
        ) %>% 
        tidyr::unite(col = "uid", c(site, dp, levlTowr, var), sep = "_", remove = FALSE) 
      
      if(nrow(radiNet) > 0){
        for(i in unique(radiNet$uid)){
          data_testing = radiNet %>% 
            dplyr::filter(uid == i)
          missing_dates = data.table::data.table(dates = dates_expected, found = dates_expected %not% data_testing$date) %>% 
            dplyr::filter(found == TRUE)
          if(nrow(missing_dates) > 0){
            # Values to add to new table
            site_var     = data_testing$site[1]
            dp_var       = data_testing$dp[1]
            levlTowr_var = data_testing$levlTowr[1]
            var_var      = data_testing$var[1]
            qf_var       = 0
            # Add 0's to missing data 
            for(j in missing_dates$dates){
              missing_data = data.table::data.table(
                uid = i, 
                site = site_var, 
                date = as.Date(j, origin = "1970-01-01"), 
                dp = dp_var,
                levlTowr = levlTowr_var,
                var = var_var,
                qfFinlTotl = qf_var)
              radiNet = data.table::rbindlist(l = list(radiNet, missing_data))
            }
          }
        }
      } 
      
      # ggplot(qfqm_radiNet, aes(x = date, y = qfFinlTotl, color = var, group = var)) +
      #   geom_point()+
      #   geom_line()+
      #   scale_y_continuous(limits = c(0,48), breaks = c(0,10,20,30,40,48)) +
      #   scale_x_date(date_labels = "%m\n%d", date_breaks = "4 day") +
      #   facet_wrap(~levlTowr) +
      #   labs(title = paste0(qfqm_radiNet$site[1],"'s Summary QFQM Report for ", qfqm_radiNet$dp[1]), x = "", color = "Tower Level")
      
      # tempAirTop
      tempAirTop = qfqm_out %>%
        dplyr::filter(dp == "tempAirTop") %>%
        dplyr::filter(stringr::str_detect(string = levlTowr, pattern = "000_") == TRUE) %>%
        dplyr::group_by(site, date, dp, levlTowr, var) %>%
        dplyr::summarise(.groups = "drop",
                         qfFinlTotl = mean(qfFinlTotl, na.rm = TRUE)
        ) %>% 
        tidyr::unite(col = "uid", c(site, dp, levlTowr, var), sep = "_", remove = FALSE) 
      
      if(nrow(tempAirTop) > 0){
        for(i in unique(tempAirTop$uid)){
          data_testing = tempAirTop %>% 
            dplyr::filter(uid == i)
          missing_dates = data.table::data.table(dates = dates_expected, found = dates_expected %not% data_testing$date) %>% 
            dplyr::filter(found == TRUE)
          if(nrow(missing_dates) > 0){
            # Values to add to new table
            site_var     = data_testing$site[1]
            dp_var       = data_testing$dp[1]
            levlTowr_var = data_testing$levlTowr[1]
            var_var      = data_testing$var[1]
            qf_var       = 0
            # Add 0's to missing data 
            for(j in missing_dates$dates){
              missing_data = data.table::data.table(
                uid = i, 
                site = site_var, 
                date = as.Date(j, origin = "1970-01-01"), 
                dp = dp_var,
                levlTowr = levlTowr_var,
                var = var_var,
                qfFinlTotl = qf_var)
              tempAirTop = data.table::rbindlist(l = list(tempAirTop, missing_data)) 
            }
          }
        }
      }
      
      # ggplot(qfqm_tempAirTop, aes(x = date, y = qfFinlTotl, color = var, group = var)) +
      #   geom_point()+
      #   geom_line()+
      #   scale_y_continuous(limits = c(0,48), breaks = c(0,10,20,30,40,48)) +
      #   scale_x_date(date_labels = "%m\n%d", date_breaks = "4 day") +
      #   facet_wrap(~levlTowr) +
      #   labs(title = paste0(qfqm_tempAirTop$site[1],"'s Summary QFQM Report for ", qfqm_tempAirTop$dp[1]), x = "", color = "Tower Level")
      
      rm(pb, qfqm_all, qfqm_avail, qfqm_out, data_testing, missing_data, missing_dates)
      dfs <- Filter(function(x) is(x, "data.frame"), mget(ls()))
      
    }
  } else {
    dfs = list()
  }
  # Return the final output file
  return(dfs)
}