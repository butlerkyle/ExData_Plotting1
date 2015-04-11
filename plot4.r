plot4 <- function(filename, sep = ";"){
        ## read in the text file
        data <- read.table(filename, sep = sep, header = TRUE)
        
        ## convert date for subseting
        data$Date <- as.Date(data$Date, format = "%d/%m/%Y")
        
        ##subset to include only dates 2007-02-01 and 2007-02-02
        date1 <- as.Date('01/02/2007', '%d/%m/%Y')
        date2 <- as.Date('02/02/2007', '%d/%m/%Y')
        data <- data[data$Date >= date1 & data$Date <= date2,]
        
        #$ convert time and merge datetime variable
        data$DateTime <- as.POSIXct(paste(data$Date, data$Time), format="%Y-%m-%d %H:%M:%S")
        data$Global_active_power <- as.numeric(as.character(data$Global_active_power))
        
        ##convert submetering to numeric
        data$Sub_metering_1 <- as.numeric(as.character(data$Sub_metering_1))
        data$Sub_metering_2 <- as.numeric(as.character(data$Sub_metering_2))
        data$Sub_metering_3 <- as.numeric(as.character(data$Sub_metering_3))
        
        ## create the plots
        par(mfrow = c(2,2))
        
        plot(data$DateTime, data$Global_active_power, type = "l", 
             ylab = "Global active power (kilowatts)", xlab = "")
        
        plot(data$DateTime, data$Voltage, type = "l",
             xlab = "datetime", ylab = "Voltage")
        
        plot(data$DateTime, data$Sub_metering_1, type = "l", 
             ylab = "Energy sub metering", xlab = "")
        points(data$DateTime, data$Sub_metering_2, type = "l", col = "red")
        points(data$DateTime, data$Sub_metering_3, type = "l", col = "blue")
        legend('topright', names(data)[7:9], col = c("black", "red", "blue"),
               bty = "n", lty = 1, cex = 0.75)
        
        plot(data$DateTime, data$Global_active_power, type = "l", 
             xlab = "datetime", ylab = "Global_active_power")
        
        ##copy a png file to the working directory
        dev.copy(png, file = "plot4.png", width = 480, height = 480)
        dev.off()
        
}
        