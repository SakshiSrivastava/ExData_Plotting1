plot4 <- function(){ 
  ## Purpose of this function is -
  ## 1. Read Data from household_power_consumption.txt
  ## 2. Get Data specific to Date - 2007-02-01 & 2007-02-02
  ## 3. Generate 4 plots of (2,2)
  
  ## Read Data  
  source_data <- read.table("household_power_consumption.txt",stringsAsFactors = FALSE,header = TRUE,sep = ";")
  
  ##Add Date and Time column in the Source data, merged together as DateTime
  
  TimeDate <- strptime(paste(source_data$Date, source_data$Time, sep=" "), "%d/%m/%Y %H:%M:%S")
  source_data <- cbind(source_data, TimeDate)
  
  ## Convert class of all fields as numeric and datetime format specific
  
  source_data$Date <- as.Date(source_data$Date,format = "%d/%m/%Y")
  source_data$Time <- format(source_data$Time,format = "%H:%M:%S:")
  source_data$Global_active_power <- as.numeric(source_data$Global_active_power)
  source_data$Global_reactive_power <- as.numeric(source_data$Global_reactive_power)
  source_data$Voltage <- as.numeric(source_data$Voltage)
  source_data$Global_intensity <- as.numeric(source_data$Global_intensity)
  source_data$Sub_metering_1 <- as.numeric(source_data$Sub_metering_1)
  source_data$Sub_metering_2 <- as.numeric(source_data$Sub_metering_2)
  source_data$Sub_metering_3 <- as.numeric(source_data$Sub_metering_3)
  
  ##Get Data only for the specific Dates
  
  data_subset <- subset(source_data, Date == "2007-02-01" | Date == "2007-02-02")
  
  ## Plot all 4 Graphs based on Global Active Power, Voltage, Submetering 1,2,3 & Global Reactive Power Vs. Date and Time
  
  ## plot the 4 graphs
  png("plot4.png", width=480, height=480)
  par(mfrow=c(2,2))
  with(data_subset, plot(TimeDate, Global_active_power, type="l", xlab="", ylab="Global Active Power"))
  with(data_subset, plot(TimeDate, Voltage, xlab="datetime", ylab="Voltage", type = "l"))
  with(data_subset, plot(TimeDate, Sub_metering_1, xlab="", ylab="Energy sub metering", type="l"))
  lines(data_subset$TimeDate, data_subset$Sub_metering_2, col= "red",type="l")
  lines(data_subset$TimeDate, data_subset$Sub_metering_3, col= "blue",type="l")
  legend(c("topright"), c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty= 1, lwd=2, col = c("black", "red", "blue"))
  with(data_subset, plot(TimeDate, Global_reactive_power, xlab="datetime", ylab="Global_reactive_power", type="l"))
  dev.off()
  
}