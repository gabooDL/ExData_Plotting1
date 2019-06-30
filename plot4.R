plot4 <- function() {
  
  # Function to read in power usage date and plot a 4 pane grid of power usage
  
  
  # Open the png plotting device
  
  png("plot4.png", width = 480, height = 480)
  
  
  # Read in the power consumption file and subset for the two date time period
  
  test0 <- read.csv2("household_power_consumption.txt", stringsAsFactors = FALSE)
  test1 <- test0[which(test0$Date == '1/2/2007' | test0$Date == '2/2/2007'), ]
  
  
  # Convert consumption values in cols 3:9 from character to numeric class
  
  for (i in 3:9){
    test1[,i] <- as.numeric(test1[,i])
  }
  
  # Date and Time columns are character strings - must be converted to date-time class.
  # Create a new column, Date_Time, from the Date and Time column contents, pasted into
  # a vector and changed to a date-time POSIXlt class with strptime(). Use this column
  # to build the plots.
  
  Date_Time <- c()
  Date_Time <- paste(test1$Date, test1$Time)
  Date_Time <- paste(test1[,1],test1[,2])
  
  # Convert to date-time objects with these formats
  # Date: format dd/mm/yyyy
  # Time: time in format hh:mm:ss 
  
  Date_Time <- strptime(Date_Time, "%e/%m/%Y %H:%M:%S")
  
  # Add the Date_Time POSIXlt value vector as the new Date_Time column  
  test1$Date_Time <- Date_Time
  
  # This section builds the plotting area layout and creates each plot
  
  # Creat the four pane layout and margins
  
  layout(mat = matrix(c(1, 2, 3, 4), 
                      nrow = 2, 
                      ncol = 2),
         heights = c(1, 1),    # Heights of the two rows
         widths = c(1, 1))     # Widths of the two columns
  
  par(mfcol = c(2,2), mar = c(5, 4, 1, 2), oma = c(0,0,3,0))
  
  ################## Plot 1 ###########################################################
  par(mai=c(1.1,0.7,0.1,0.3))
  plot(Date_Time, test1$Global_active_power, type = "l", xlab = "", ylab = "Global Active Power")
  
  ################## Plot 2 - with legend for the three sub_metering plot lines #######
  par(mai=c(0.9,0.7,0.3,0.3))
  plot(Date_Time, test1$Sub_metering_1, type = "n", xlab = "", ylab = "Energy sub metering")
  
  sub1 <- test1$Sub_metering_1
  sub2 <- test1$Sub_metering_2
  sub3 <- test1$Sub_metering_3
  
  points(Date_Time, sub1, type = "l")
  points(Date_Time, sub2, type = "l", xlab = "", ylab = "", col = "red")
  points(Date_Time, sub3, type = "l", xlab = "", ylab = "", col = "blue")
  
  legend("topright", seg.len = 1, text.width = strwidth("1,000,000,000,"), bty = "n", lty = 1, col = c("black", "blue","red"), legend = c("Sub_metering_1","Sub_metering_2", "Sub_metering_3"), cex = 0.9, y.intersp=0.9, x.intersp = 0.6)
  
  ################## Plot 3 ############################################
  par(mai=c(1.1,0.7,0.1,0.3))
  plot(Date_Time, test1$Voltage, type = "l", xlab = "datetime",  ylab = "Voltage")
  
  ################## Plot 4 ############################################
  par(mai=c(0.9,0.7,0.3,0.3))
  plot(Date_Time, test1$Global_reactive_power, type = "l", xlab = "datetime", ylab = "Global_reactive_power")
  
  # Close the png plotting device
  
  dev.off()
  
  message("Plotting complete")
  
}