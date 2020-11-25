# swiftUserConnections.R

# Swift User Data Collection

# Read In Number of connections
if(file.exists("/srv/shiny-server/swift/users_data.csv") == TRUE){
  UserConnectionsData <- fread("/srv/shiny-server/swift/users_data.csv")
  
  # Format Dates
  UserConnectionsData$Start <- as.POSIXct(UserConnectionsData$Start, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  UserConnectionsData$End <- as.POSIXct(UserConnectionsData$End, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  # Calculate total time spent on app
  UserConnectionsData <- UserConnectionsData %>%
    mutate(timeSpent = End - Start)
  UserConnectionsData$Start <-format(UserConnectionsData$Start, tz="America/Denver",usetz=TRUE)
  UserConnectionsData$End <-format(UserConnectionsData$End, tz="America/Denver",usetz=TRUE)
  UserConnectionsData$Date <- as.Date(UserConnectionsData$Start, format = "%Y-%m-%d")
  UserConnectionsData$day <- weekdays(as.Date(UserConnectionsData$Date))
  
  
  
  ## Density plot
  userConDensityPlot <- ggplot(UserConnectionsData, aes(x=Date))+
    geom_density()+
    labs(y="Density of Connections", title = "Density plot of Connections")
  
  output$userConDensityPlot <- renderPlotly({
    userConDensityPlot
  })
  
  ## Numbers plot
  UserConnectionsDataNoNA <- na.omit(UserConnectionsData)
  UserConnectionsDataNoNA$day <- factor(UserConnectionsDataNoNA$day, levels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
  
  UserConnectionsDataNoNA <- UserConnectionsDataNoNA %>% 
    mutate(timeSpent = timeSpent/60) %>%
    filter(timeSpent < 45)
  
  weekdayColors <- c("red","green","green","green","green","green","red")
  userConNumberPlot <- ggplot(UserConnectionsDataNoNA, aes(x=factor(Date), y=timeSpent, fill = day))+
    geom_boxplot(outlier.shape = NA) +
    scale_fill_manual(values = weekdayColors)+
    theme(axis.text.x = element_text(angle = 320,color = "#012D74", size =10))+ # Change axis text to Battelle Blue
    # scale_y_continuous(limits = quantile(UserConnectionsData$timeSpent, c(0.1, 0.9),na.rm = TRUE))+
    labs(y="Time Spent (s)",x="Date",title = "Box Plot representation of time spent on Swift")
  
  output$userConNumberPlot <- renderPlotly({
    userConNumberPlot
  })
  
  ## Number of Connections per day
  sumConnections <- UserConnectionsData %>%
    group_by(Date) %>%
    count(Date) 
  
  sumConnections$day <- weekdays(as.Date(sumConnections$Date))
  sumConnections$day <- factor(sumConnections$day, levels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
  weekdayColors <- c("red","green","green","green","green","green","red")
  
  sumConnections <- sumConnections %>%
    filter(day != "NA")
  
  userConTotalPlot <- ggplot(data = sumConnections, aes(x=Date,y=n))+
    geom_point(aes(color = day), size=3)+
    scale_color_manual(values = weekdayColors)+
    geom_smooth(se = F)+
    geom_line()+
    labs(y="Number of Connections",title="Number of Connections to Swift")
  
  output$userConTotalPlot <- renderPlotly({
    userConTotalPlot
  })
  
  # Calculate total number of Connections
  TodaysConnections <- UserConnectionsData %>%
    filter(Date == Sys.Date())
  numConnections <- length(TodaysConnections$Date)
  
  # Calculate Average Time Spent
  AverageTime <- mean(TodaysConnections$timeSpent)/60
  
  # Value box for Average Time Spent
  output$swiftAverageTime <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = paste0("The Average User Spent ", round(AverageTime,1), " Minutes on Swift"),
      subtitle = "  ",
      width = 12,
      color = "orange"
    )
  })
  
  # Value box for Number of connections
  output$swiftNumConnections <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = paste0("There have been ", numConnections, " connections to Swift today."),
      subtitle = "  ",
      width = 12,
      color = "orange"
    )
  })
} else {
  print("No Data!")
}