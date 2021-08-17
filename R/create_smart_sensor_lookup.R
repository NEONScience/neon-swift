# list_of_files = list.files("C:/Users/kstyers/Downloads/site_maps_for_lookup", full.names = TRUE)
# 
# site_map_lookup = data.table::data.table()
# for(i in seq_along(list_of_files)){
#   message(list_of_files[i])
#   site_map_in = data.table::fread(list_of_files[i]) %>% 
#     dplyr::select(MAC, DESC) %>% 
#     dplyr::filter(DESC %in% c(
#       "Sensor L2130-i Analyzer for isotopic water vapor 90 to 240VAC 60HZ", 
#       "Sensor G2131-i Gas Analyzer for Isotopic CO2 240VAC",
#       "Assembly  ECTE Sensor Head and Analyzer",
#       "Assembly  ECTE Sensor and Analyzer  Extreme Cold",
#       "Assembly  ECTE LI7200RS Sensor Head and Analyzer")) %>% 
#     dplyr::mutate(MAC = base::tolower(MAC)) %>% 
#     dplyr::mutate(DESC = ifelse(test = DESC == "Sensor L2130-i Analyzer for isotopic water vapor 90 to 240VAC 60HZ", yes = "L2130i", no = DESC)) %>% 
#     dplyr::mutate(DESC = ifelse(test = DESC == "Sensor G2131-i Gas Analyzer for Isotopic CO2 240VAC", yes = "G2131i", no = DESC)) %>% 
#     dplyr::mutate(DESC = ifelse(test = DESC == "Assembly  ECTE Sensor Head and Analyzer", yes = "Li7200", no = DESC)) %>% 
#     dplyr::mutate(DESC = ifelse(test = DESC == "Assembly  ECTE Sensor and Analyzer  Extreme Cold", yes = "Li7200", no = DESC)) %>%
#     dplyr::mutate(DESC = ifelse(test = DESC == "Assembly  ECTE LI7200RS Sensor Head and Analyzer", yes = "Li7200", no = DESC)) 
# 
# 
#   site_map_lookup = data.table::rbindlist(l = list(site_map_lookup, site_map_in))
#   rm(site_map_in)
#   
# }
# 
# names(site_map_lookup) = c("MacAddress", "Sensor")
# 
# saveRDS(site_map_lookup, "./data/lookup/smart_sensor_lookup.RDS")
