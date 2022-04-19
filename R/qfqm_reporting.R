

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
  library(dplyr)
  library(tidyr)
  library(data.table)
  library(ggplot2)
  
  # Special functions
  `%not%` = Negate(`%in%`)
  
  # Bucket we are connecting too
  sae.bucket = "neon-sae-files"

  
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

    qfqm_avail = eddycopipe::neon_gcs_public_list_objects(bucket = sae.bucket, prefix = paste0("ods/qfqmRpt/", day, "/", site, "/"))
    
    if(nrow(qfqm_avail) > 0){
      qfqm_avail = qfqm_avail  %>%
        dplyr::filter(stringr::str_detect(string = Key, pattern = ".rds") == TRUE) 
    }
      
    # If the single RDS file is in the folder, read it in
    if(nrow(qfqm_avail) == 1){
      # Grab the ith day's QFQM report data
      temp_file_path = eddycopipe::neon_gcs_public_sae_files_download(object = qfqm_avail$Key[1], bucket = sae.bucket)
      
      qfqm_all = base::readRDS(temp_file_path) %>% 
        dplyr::mutate(date = as.Date(date, origin = "1970-01-01")) %>% 
        dplyr::select(site, date, dp, levlTowr, metric, var, value, qmQfFinl,qfFinlTotl, qmQfAlphFinl)
      # Join to the "empty" data.table
      qfqm_out = data.table::rbindlist(l = list(qfqm_out, qfqm_all))
    }
  }
  
  if(nrow(qfqm_out) > 0){
    
    # Data for gap filling
    unique_tower_levels = qfqm_out %>%  dplyr::filter(stringr::str_detect(string = levlTowr, pattern = "000_")) 
    highest_level = max(unique_tower_levels$levlTowr)
    unique_tower_levels = unique(unique_tower_levels$levlTowr)
    
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
      
      # Missing levels
      missing_levels = data.table::data.table(levels = unique_tower_levels, found = unique_tower_levels %not% tempAirLvl$levlTowr) %>% 
          dplyr::filter(found == TRUE)
      
      if(nrow(missing_levels) > 0){
        for(i in seq_along(missing_levels$levels)){
          tempAirLvl_add = data.table::data.table(
            "uid" = paste0(qfqm_out$site[1], "_tempAirLvl_", missing_levels$levels[i], "_temp"),
            "site" =  qfqm_out$site[1],
            "date" =  tempAirLvl$date[1],
            "dp" = "tempAirLvl",
            "levlTowr" = missing_levels$levels[i],
            "var" = "temp",
            qfFinlTotl = 0
          )
          
          tempAirLvl = data.table::rbindlist(l = list(tempAirLvl, tempAirLvl_add))
          
        }
        rm(missing_levels, tempAirLvl_add)
      }
      
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
    
    rm(pb, qfqm_all, qfqm_avail, qfqm_out, data_testing, missing_data, missing_dates, unique_tower_levels)
    dfs <- Filter(function(x) is(x, "data.frame"), mget(ls()))
      
  } else {
    dfs = list()
  }
  # Return the final output file
  return(dfs)
}