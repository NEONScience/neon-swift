check_pm = function(site){
  
  library(dplyr)
  library(aws.s3)
  library(fst)
  
  # S3 Connection
  Sys.unsetenv("AWS_SECRET_ACCESS_KEY")
  Sys.unsetenv("AWS_S3_ENDPOINT")
  Sys.unsetenv("AWS_DEFAULT_REGION")
  Sys.setenv(
    "AWS_ACCESS_KEY_ID"     = "research-eddy-inquiry",
    "AWS_S3_ENDPOINT"       = "neonscience.org",
    "AWS_DEFAULT_REGION"    = "s3.data"
  )
  
  
  
  
  fulcrum_data_raw = aws.s3::s3read_using(FUN = fst::read.fst,object = "maintenance_app/all_data.fst", bucket = "research-eddy-inquiry") %>% 
    # Remove all useless columns
    dplyr::select(-technician_1, -technician_1_link, -technician_2, -technician_2_link,
                  -guy_tower_at_site, -cleaned_measurement_level_pars, -cleaned_measurement_level_irbiotemps, -cleaned_tower_top_rad_sensors)
  
  # Remove more columns
  fulcrum_data = fulcrum_data_raw[ , 20:304]
  
  # Filter down to specified site
  site_data = fulcrum_data %>% 
    dplyr::filter(siteid == site)
  
  # Total number of bouts
  bouts_recorded = unique(site_data$date)
  
  ### Filter down to specific sensor
  # Tower Stuffs
  ml_pheno_data = site_data %>% 
    dplyr::select(siteid, date, phenocam_lower, phenocam_upper)
  names(ml_pheno_data) = c("Site", "PM Date", "Lower Cam", "Upper Cam")
  ml_par_data = site_data %>% 
    dplyr::select(siteid, date, ml1_par, ml2_par, ml3_par, ml4_par, ml5_par, ml6_par, ml7_par, tower_top_updown_par)
  names(ml_par_data) = c("Site", "PM Date", "ML1", "ML2", "ML3", "ML4", "ML5", "ML6", "ML7", "Tower Top")
  ml_aat_data = site_data %>% 
    dplyr::select(siteid, date, ml1_aat, ml2_aat, ml3_aat, ml4_aat, ml5_aat, ml6_aat, ml7_aat, tower_top_att)
  names(ml_aat_data) = c("Site", "PM Date", "ML1", "ML2", "ML3", "ML4", "ML5", "ML6", "ML7", "Tower Top")
  ml_wind_data = site_data %>% 
    dplyr::select(siteid, date, ml1_2d_wind, ml2_2d_wind, ml3_2d_wind, ml4_2d_wind, ml5_2d_wind, ml6_2d_wind, ml7_2d_wind, tower_top_3d_wind)
  names(ml_wind_data) = c("Site", "PM Date","ML1", "ML2", "ML3", "ML4", "ML5", "ML6", "ML7", "Tower Top")
  ml_ir_biotemp_data = site_data %>% 
    dplyr::select(siteid, date, ml1_ir_biotemp, ml2_ir_biotemp, ml3_ir_biotemp, ml4_ir_biotemp, ml5_ir_biotemp, ml6_ir_biotemp) 
  names(ml_ir_biotemp_data) = c("Site", "PM Date", "ML1", "ML2", "ML3", "ML4", "ML5", "ML6")
  tt_sensor_data = site_data %>% 
    dplyr::select(siteid, date, tower_top_hmp155, tower_top_nr01, tower_top_spn1, tower_top_cimel, tower_top_cmp22, tower_top_secondary_precip, tower_top_environmental_enclosure, tower_top_particulate_size, tower_top_particulate_mass)
  names(tt_sensor_data) = c("Site", "PM Date", "HMP155", "NR01", "SPN1", "CIMEL", "CMP22", "2nd Precip", "Environmental Enclosure", "DustTrak", "HiVol3000")
  tower_comments = site_data %>% 
    dplyr::select(siteid, date, ml1_comments, ml2_comments, ml3_comments, ml4_comments, ml5_comments, ml6_comments, ml7_comments, tower_top_comments)
  names(tower_comments) = c("Site", "PM Date", "ML1", "ML2", "ML3", "ML4", "ML5", "ML6", "ML7", "Tower Top")
  
  # Soil Stuff
  sp_co2_data = site_data %>% 
    dplyr::select(siteid, date, sp1_soil_co2, sp2_soil_co2, sp3_soil_co2, sp4_soil_co2, sp5_soil_co2)
  names(sp_co2_data) = c("Site", "PM Date", "SP1", "SP2", "SP3", "SP4", "SP5")
  sp_soil_water_content = site_data %>%
    dplyr::select(siteid, date, sp1_soil_water_content, sp2_soil_water_content, sp3_soil_water_content, sp4_soil_water_content, sp5_soil_water_content)
  names(sp_soil_water_content) = c("Site", "PM Date", "SP1", "SP2", "SP3", "SP4", "SP5")
  sp_soil_temperature = site_data %>% 
    dplyr::select(siteid, date, sp1_soil_temperature, sp2_soil_temperature, sp3_soil_temperature, sp4_soil_temperature, sp5_soil_temperature)
  names(sp_soil_temperature) = c("Site", "PM Date", "SP1", "SP2", "SP3", "SP4", "SP5")
  sp_soil_thrufall = site_data %>% 
    dplyr::select(siteid, date, sp1_throughfall_collector, sp2_throughfall_collector, sp3_throughfall_collector, sp4_throughfall_collector, sp5_throughfall_collector)
  names(sp_soil_thrufall) = c("Site", "PM Date", "SP1", "SP2", "SP3", "SP4", "SP5")
  sp_soil_qline = site_data %>% 
    dplyr::select(siteid, date, sp1_q_line_par, sp2_q_line_par, sp3_q_line_par, sp5_q_line_par)
  names(sp_soil_qline) = c("Site", "PM Date", "SP1", "SP2", "SP3", "SP5")
  sp_soil_heat_flux = site_data %>% 
    dplyr::select(siteid, date, sp1_soil_heat_flux, sp2_soil_heat_flux, sp3_soil_heat_flux, sp5_soil_heat_flux)
  names(sp_soil_heat_flux) = c("Site", "PM Date", "SP1", "SP2", "SP3", "SP5")
  sp3_sensors = site_data %>% 
    dplyr::select(siteid, date, sp3_hmp155, sp3_ir_biotemp, sp3_nr01)
  names(sp3_sensors) = c("Site", "PM Date", "HMP155", "IR Biotemp", "NR01")
  sp_comments = site_data %>% 
    dplyr::select(siteid, date, sp1_comments, sp2_comments, sp3_comments, sp4_comments, sp5_comments)
  names(sp_comments) = c("Site", "PM Date","SP1", "SP2", "SP3", "SP4", "SP5")
  
  # DFIR Stuff
  dfir_data = site_data %>% 
    dplyr::select(siteid, date, dfir_maintenance, dfir_maintenance_other, amount_of_antifreeze_added, amount_of_oil_added, units_of_amount_oil_added, alter_shield_replaced, number_of_shields_replaced)
  names(dfir_data) = c("Site", "PM Date", "Maintenance", "Other", "Volume of Antifreeze Added", "Volume of Oil Added", "Units of Oil Added", "Alter Shield Replaced?", "Number of Shields Replaced")
  
  # Eddy Co Stuff
  eddy_gas_analyzer_data = site_data %>% 
    dplyr::select(siteid, date, li_840, li_840_other, g2131_i, g2131_i_other, l2130_i, l2130_i_other, l2130_i_sample_volume, turb_li_7200)
  names(eddy_gas_analyzer_data) = c("Site", "PM Date", "Li840A", "Li840A-other", "G2131i", "G2131i-other", "L2130i", "L2130i Other", "L2130i Sample Volume", "Li7200(RS)")
  eddy_gas_cylinder_data = site_data %>% 
    dplyr::select(siteid, date, stor_co2_zero_air, stor_h2o_zero_air, stor_archive, stor_low, stor_int, stor_high, turb_co2_zero_air, turb_archive, turb_low, turb_int, turb_high) 
  names(eddy_gas_cylinder_data) = c("Site", "PM Date", "ECSE Co2 Zero", "ECSE H2o Zero", "ECSE Archive", "ECSE Low", "ECSE Int", "ECSE High", "ECTE Co2 Zero", "ECTE Archive", "ECTE Low", "ECTE Int", "ECTE High")
  eddy_pump_data = site_data %>% 
    dplyr::select(siteid, date, li840_vacuum_pump, ml1_vacuum_pump, ml2_vacuum_pump, ml3_vacuum_pump, ml4_vacuum_pump, ml5_vacuum_pump, ml6_vacuum_pump, ml7_vacuum_pump, ml8_vacuum_pump)
  names(eddy_pump_data) = c("Site", "PM Date", "Li840A Pump", "ML1", "ML2", "ML3", "ML4", "ML5", "ML6", "ML7", "ML8")
  eddy_inlet_data = site_data %>% 
    dplyr::select(siteid, date, ml1_storage_inlet, ml2_storage_inlet, ml3_storage_inlet, ml4_storage_inlet, ml5_storage_inlet, ml6_storage_inlet, ml7_storage_inlet, ml8_storage_inlet)
  names(eddy_inlet_data) = c("Site", "PM Date", "ML1", "ML2", "ML3", "ML4", "ML5", "ML6", "ML7", "ML8")
  eddy_fep_tube_data = site_data %>% 
    dplyr::select(siteid, date, ml1_fep_tubing, ml2_fep_tubing, ml3_fep_tubing, ml4_fep_tubing, ml5_fep_tubing, ml6_fep_tubing, ml7_fep_tubing, ml8_fep_tubing)
  names(eddy_fep_tube_data) = c("Site", "PM Date", "ML1", "ML2", "ML3", "ML4", "ML5", "ML6", "ML7", "ML8")
  eddy_pducer_data = site_data %>% 
    dplyr::select(siteid, date, ml1_pressure_transducer, ml2_pressure_transducer, ml3_pressure_transducer, ml4_pressure_transducer, ml5_pressure_transducer, ml6_pressure_transducer, ml7_pressure_transducer, ml8_pressure_transducer)
  names(eddy_pducer_data) = c("Site", "PM Date", "ML1", "ML2", "ML3", "ML4", "ML5", "ML6", "ML7", "ML8")
  eddy_tt_data = site_data %>% 
    dplyr::select(siteid, date, turb_inlet, turb_amrs, turb_amrs_other, turb_csat3, turb_csat3_other, turb_pump)
  names(eddy_tt_data) = c("Site", "PM Date", "ECTE Inlet", "AMRS", "AMRS-other", "CSAT3", "CSAT3-other", "ECTE Pump")
  eddy_comments = site_data %>% 
    dplyr::select(siteid, date, ml1_stor_comments, ml2_stor_comments, ml3_stor_comments, ml4_stor_comments, ml5_stor_comments, ml6_stor_comments, ml7_stor_comments, ml8_stor_comments, turb_comments)
  names(eddy_comments) = c("Site", "PM Date", "ML1", "ML2", "ML3", "ML4", "ML5", "ML6", "ML7", "ML8", "Tower Top")
  
  # Pm level comments
  pm_comments = site_data %>% 
    dplyr::select(siteid, date, pm_level_comments)
  names(pm_comments) = c("Site", "PM Date", "Comments")
  # All comment data 
  all_comments = site_data %>% 
    dplyr::select(siteid, date, pm_level_comments,
                  ml1_comments, ml2_comments, ml3_comments, ml4_comments, ml5_comments, ml6_comments, ml7_comments, tower_top_comments,
                  sp1_comments, sp2_comments, sp3_comments, sp4_comments, sp5_comments,
                  ml1_stor_comments, ml2_stor_comments, ml3_stor_comments, ml4_stor_comments, ml5_stor_comments, ml6_stor_comments, ml7_stor_comments, ml8_stor_comments, turb_comments
    )
  names(all_comments) = c("Site", "PM Date", "PM", "ML1", "ML2", "ML3", "ML4", "ML5", "ML6", "ML7", "Tower Top", "SP1", "SP2", "SP3", "SP4", "SP5", "ECSE 1", "ECSE 2", "ECSE 3", "ECSE 4", "ECSE 5", "ECSE 6", "ECSE 7", "ECSE 8", "ECTE")
  
  
  last_bout = max(site_data$date)
  
  first_bout = min(site_data$date)
  
  unique_bouts = length(unique(site_data$date))
  
  total_time_period = as.numeric( difftime(Sys.Date(), min(site_data$date, na.rm = TRUE), units = "days"))
  
  bout_freq_per_2weeks_all_time = round((unique_bouts / total_time_period) * 14,2)
  
  date_check = site_data %>%
    dplyr::filter(date > Sys.Date()-62)
  
  last_bout_recent = max(date_check$date)
  
  first_bout_recent = min(date_check$date)
  
  unique_bouts_recent = length(unique(date_check$date))
  
  total_time_period_recent = as.numeric( difftime(Sys.Date(), min(date_check$date, na.rm = TRUE), units = "days"))
  
  bout_freq_per_2weeks_recent = round((unique_bouts_recent / total_time_period_recent) * 14,2)
  
  meta_statistics = data.table::data.table("2 Week Bout Frequency" = bout_freq_per_2weeks_recent, "2 Week Bout Frequency - All Time" = bout_freq_per_2weeks_all_time, "Total Number of Days Since the First and Last Record" = total_time_period, "Total Bouts" = unique_bouts, "First Bout" = first_bout, "Last Bout" = last_bout)
  
  rm(fulcrum_data_raw, fulcrum_data)
  dfs <- Filter(function(x) is(x, "data.frame"), mget(ls()))
  return(dfs)
}
