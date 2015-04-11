plot2 <- function(filename, sep = ";"){
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
        
        ## create the plot
        plot(data$DateTime, data$Global_active_power, type = "l", 
             ylab = "Global active power (kilowatts)", xlab = "")
        
        ##copy a png file to the working directory
        dev.copy(png, file = "plot2.png", width = 480, height = 480)
        dev.off()
        
}