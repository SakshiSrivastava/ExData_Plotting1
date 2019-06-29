plot1 <- function(){ 
## Purpose of this function is -
## 1. Read Data from household_power_consumption.txt
## 2. Get Data specific to Date - 2007-02-01 & 2007-02-02
## 3. Generate a Histogram based on Global Active Power

## Read Data  
source_data <- read.table("household_power_consumption.txt",stringsAsFactors = FALSE,header = TRUE,sep = ";")

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

## Plot Histogram and specify details like color, title etc.

png("plot1.png", width = 480, height = 480)
hist(data_subset$Global_active_power,col = "red",border = "black",main = "Global Active Power",xlab = "Global Active Power (Kilowatts)",ylab = "Frequency")
dev.off()
}