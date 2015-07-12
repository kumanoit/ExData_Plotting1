##########################################################################################
                  ############# READING DATA  ################
##########################################################################################

getData <- function() {
  ## for read.csv.sql
  library(sqldf)
  
  ## all data will be saved to this directory
  dataDir <- "./data"
  
  ## create this directory if it didn't exist
  if (!file.exists(dataDir)) {
    message("Creating directory...")
    dir.create(dataDir)
  }
  
  ## file link of data set
  fileLink <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
  
  zippedFile <- "./data/exdata-data-household_power_consumption.zip"
  
  ## if file is already present then delete this file for fresh analysis
  if(file.exists(zippedFile)) {
    message("Deleting file... ", zippedFile)
    file.remove(zippedFile)
  }
  
  fileName <- "./data/household_power_consumption.txt"
  ## if unzipped file is already present then delete that too
  if(file.exists(fileName)) {
    message("Deleting already present file... ", fileName)
    file.remove(fileName)
  }
  
  ## downloading file
  message("Downloading file...")
  download.file(fileLink, destfile = zippedFile, method = "curl")
  
  ## unzipping file
  unzip(zippedFile, exdir = dataDir)
  
  ## Reading data for two dates viz. 2007-02-01 and 2007-02-02
  data <- read.csv.sql(fileName, sep = ";",
                       sql = "select * from file where Date in ('1/2/2007', '2/2/2007')",
                       colClasses = rep("character", 9))  
  
  ## takes a list and returns true if it doesn't have any missing value otherwise false
  filter <- function(x) {
    return(all(x != "?"))
  }
  
  ## filering out rows that contain "?" OR missing values
  data <- data[apply(data, 1, filter),]
  
  ## converting format of date from character to date format
  data$Date <- as.Date(data$Date, "%d/%m/%Y")
  
  ## converting time variable to time format
  time <- paste(data$Date, data$Time, sep = " ")
  data$Time <- strptime(time, "%Y-%m-%d %H:%M:%S")
  
  ## converting other variables data type from character to numeric
  data[,3:9] <- sapply(data[,3:9], as.numeric)
  
  ## returning data
  return(data)
}


##########################################################################################
                        ############# PLOTTING  ################
##########################################################################################

plot4 <- function() {
  ## getting data
  data <- getData()
  
  ## deleting plot file if it already existed
  plotFileName <- "./data/plot4.png"
  if(file.exists(plotFileName)) {
    message("deleting previos plot")
    file.remove(plotFileName)
  }
  
  ## opening png device and setting name of file where plot would be saved
  png(file = plotFileName, width = 480, height = 480)

  ## setting environment for plot
  par(mfrow = c(2,2), mar = c(4,4,2,1), oma = c(0,0,2,0))

  ## plotting and saving to file
  ## plot (a)
  plot(data$Time, data$Global_active_power, type = "l", xlab = "", ylab = "Global Active Power")
  
  ## plot (b)
  plot(data$Time, data$Voltage, type = "l", xlab = "datetime", ylab = "Voltage")
  
  ## plot (c)
  plot(data$Time, data$Sub_metering_1, type = "l", col = "black", xlab = "", ylab = "Energy sub metering")
  lines(data$Time, data$Sub_metering_2, type = "l", col = "red")
  lines(data$Time, data$Sub_metering_3, type = "l", col = "blue")
  legend("topright", col = c("black", "red", "blue"), 
         legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty = 1, bty = "n")
  
  ## plot (d)
  plot(data$Time, data$Global_reactive_power, xlab = "datetime", ylab = "Global_reactive_power", type = "l")
  
  ## closing png device
  dev.off()
}