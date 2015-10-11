# The below funtion plots the household global minute-averaged active power (in kilowatt)
# against time period across 2 days 2007-02-01 and 2007-02-02

plot2 <- function()
{
  # Read the data
  table <- read.table("household_power_consumption.txt", sep = ";", header = TRUE)
  
  # Format the date column to convert to the native date format
  table$Date <- as.Date(as.vector(table$Date), format = "%d/%m/%Y")
  
  # Subset the data to get readings on 2 days - 2007-02-01 and 2007-02-02
  data <- table[table$Date == "2007-02-01" | table$Date == "2007-02-02", ]
  
  # Get the DateTime Stamp
  x <- paste(data$Date, as.vector(data$Time))
  x <- strptime(x, format = "%Y-%m-%d %H:%M:%S")
  
  # Initiate the graphical device
  png("plot2.png", width = 480, height = 480)
  
  # Set the row and column counts to display only 1 graps. Also set the margins
  par(mfrow = c(1,1), mar = c(4,4,2,2)) 

  # Get the data to be plotted - household global minute-averaged 
  #active power (in kilowatt) and timeperiod
  plot_data <- data.frame(x, as.numeric(as.vector(data$Global_active_power)))
  
  # Set the column names
  colnames(plot_data) <- c("Date", "Global_active_power")
  
  # Plot the household global minute-averaged active power (in kilowatt) againts time period
  plot(plot_data$Date, plot_data$Global_active_power, type = "l", ylab = "Global Active Power (kilowatts)", 
       xlab = " ")
  
  # Switch off the graphical device
  dev.off()
  
}