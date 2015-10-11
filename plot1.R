# The below funtion plots the histogram of household global minute-averaged active power (in kilowatt)
# collected from the UC Irvine Machine Learning Repository for 2 days - 2007-02-01 and 2007-02-02

plot1 <- function()
{

  # Read the data
  table <- read.table("household_power_consumption.txt", sep = ";", header = TRUE)
  
  # Format the date column to convert to the native date format
  table$Date <- as.Date(as.vector(table$Date), format = "%d/%m/%Y")
  
  # Subset the data to get readings on 2 days - 2007-02-01 and 2007-02-02
  data <- table[table$Date == "2007-02-01" | table$Date == "2007-02-02", c(1,3)]
  
  # Format the Global_active_power values to make them numeric
  data$Global_active_power <- as.numeric(as.vector(data$Global_active_power))
  
  # Remove all the NaNs
  data <- data[complete.cases(data), ]
  
  # Initiate the plotting device
  png("plot1.png", width = 480, height = 480)
  
  # Set the row and column counts to display only 1 graps. Also set the margins
  par(mfrow = c(1,1), mar = c(4,4,2,2))

  # Plot the histogram of Global_active_power
  x <- data$Global_active_power
  hist(x, xlab = "Global Active Power (kilowatts)", col = "red", main = "Global Active Power")
  
  # Switch off the plotting device
  dev.off()
  
}