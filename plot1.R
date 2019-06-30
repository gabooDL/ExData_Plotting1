plot1 <- function() {
  
  # Function to read in power usage date and plot a 4 pane grid of power usage
  
  
  # Open the png plotting device
  
  png("plot1.png", width = 480, height = 480)
  
  
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
  
  # This section builds the plotting area 
  
  #################### Histogram of Global Active Power in kilowatts  ####################
  
  hist(test1$Global_active_power, xlab = "Global Active Power (kilowatts)", ylab = "Frequency", main = "Global Active Power", col = "red")
  
  # Close the png plotting device
  
  dev.off()
  
  message("Plotting complete")
}