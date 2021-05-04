# Source all the valid sensor stats for running analysis
# message("WHY!")
# swft_eddy_co_uptime = function(){
#   # S3 enviro
#   Sys.setenv("AWS_ACCESS_KEY_ID"     = "research-eddy-inquiry",
#              "AWS_S3_ENDPOINT"       = "neonscience.org",
#              "AWS_DEFAULT_REGION"    = "s3.data")
#   
#   # list of dates to pull
#   swft_eddy_stats_date_list = seq.Date(from = Sys.Date()-900, to = Sys.Date()-1, by = "1 day")
#   
#   # Blank datatable to join to 
#   swft_ml_missing_running = data.table::data.table()
#   swft_li840_missing_running = data.table::data.table()
#   swft_li7200_missing_running = data.table::data.table()
#   swft_g2131_missing_running = data.table::data.table()
#   swft_l2130_missing_running = data.table::data.table()
#   
#   for(date in swft_eddy_stats_date_list){
#     # Make date into date object
#     date = as.Date(date, origin = "1970-01-01", format = "%Y-%m-%d")
#     
#     # Daily File Naming
#     swft_ml_file_name = paste0("swft_daily_stats/ml.missing_", date,".RDS")
#     swft_li840_file_name = paste0("swft_daily_stats/li840_working_", date,".RDS")
#     swft_li7200_file_name = paste0("swft_daily_stats/li7200_working_", date,".RDS")
#     swft_g2131_file_name = paste0("swft_daily_stats/g2131_working_", date,".RDS")
#     swft_l2130_file_name = paste0("swft_daily_stats/l2130_working_", date,".RDS")
#   
#     # ML Data Pull and Join
#     message(swft_ml_file_name)
#     if(aws.s3::object_exists(object = swft_ml_file_name, bucket = "research-eddy-inquiry")){
#       data.ml.in = aws.s3::s3readRDS(object = swft_ml_file_name, bucket = "research-eddy-inquiry")
#       swft_ml_missing_running = data.table::rbindlist(l = list(swft_ml_missing_running, data.ml.in))
#       rm(data.ml.in)
#     }
#     
#     # Li840 Data Pull and Join
#     message(swft_li840_file_name)
#     if(aws.s3::object_exists(object = swft_li840_file_name, bucket = "research-eddy-inquiry")){
#       data.li840.in = aws.s3::s3readRDS(object = swft_li840_file_name, bucket = "research-eddy-inquiry")
#       swft_li840_missing_running = data.table::rbindlist(l = list(swft_li840_missing_running, data.li840.in))
#       rm(data.li840.in)
#     }
#     
#     # Li7200 Data Pull and Join
#     message(swft_li7200_file_name)
#     if(aws.s3::object_exists(object = swft_li7200_file_name, bucket = "research-eddy-inquiry")){
#       data.li7200.in = aws.s3::s3readRDS(object = swft_li7200_file_name, bucket = "research-eddy-inquiry")
#       swft_li7200_missing_running = data.table::rbindlist(l = list(swft_li7200_missing_running, data.li7200.in))
#       rm(data.li7200.in)
#     }
#     
#     # G2131 Data Pull and Join
#     message(swft_g2131_file_name)
#     if(aws.s3::object_exists(object = swft_g2131_file_name, bucket = "research-eddy-inquiry")){
#       data.g2131.in = aws.s3::s3readRDS(object = swft_g2131_file_name, bucket = "research-eddy-inquiry")
#       swft_g2131_missing_running = data.table::rbindlist(l = list(swft_g2131_missing_running, data.g2131.in))
#       rm(data.g2131.in)
#     }
#     
#     # L2130 Data Pull and Join
#     message(swft_l2130_file_name)
#     if(aws.s3::object_exists(object = swft_l2130_file_name, bucket = "research-eddy-inquiry")){
#       data.l2130.in = aws.s3::s3readRDS(object = swft_l2130_file_name, bucket = "research-eddy-inquiry")
#       swft_l2130_missing_running = data.table::rbindlist(l = list(swft_l2130_missing_running, data.l2130.in))
#       rm(data.l2130.in)
#     }
#   
#   }
# 
#   return(list(swft_g2131_missing_running, swft_l2130_missing_running, swft_li7200_missing_running, swft_li840_missing_running, swft_ml_missing_running))
# 
# }
# 
# data.out = swft_eddy_co_uptime()
# 
# library(ggplot2)
# 
# swft_stats_rolling_plot = ggplot()+
#   geom_point(data = data.out[[1]], aes(x = date, y = as.numeric(percent.valid), color = "G2131")) +
#   # geom_line(data = swft_g2131_missing_running, aes(x = date, y = as.numeric(percent.valid), color = "G2131")) +
#   geom_point(data = data.out[[2]], aes(x = date, y = as.numeric(percent.valid), color = "L2130")) +
#   # geom_line(data = swft_l2130_missing_running, aes(x = date, y = as.numeric(percent.valid), color = "L2130")) +
#   geom_point(data = data.out[[3]], aes(x = date, y = as.numeric(percent.valid), color = "Li7200")) +
#   # geom_line(data = swft_li7200_missing_running, aes(x = date, y = as.numeric(percent.valid), color = "Li7200")) +
#   geom_point(data = data.out[[4]], aes(x = date, y = as.numeric(percent.li840.valid), color = "Li840")) +
#   # geom_line(data = swft_li840_missing_running, aes(x = date, y = as.numeric(percent.li840.valid), color = "Li840")) +
#   geom_point(data = data.out[[5]], aes(x = date, y = 100-as.numeric(gsub(`flag%`, pattern = "%", replacement = "")), color = "ECSE ML")) +
#   # geom_line(data = swft_ml_missing_running, aes(x = date, y = 100-as.numeric(gsub(`flag%`, pattern = "%", replacement = "")), color = "ECSE ML")) +
#   scale_y_continuous(limits = c(1,100)) +
#   scale_x_date(breaks = "14 day", date_labels = "%Y\n%b-%d") +
#   labs(y = "Percent Uptime", x = "", color = "Legend")
# 
# plotly::ggplotly(swft_stats_rolling_plot)
