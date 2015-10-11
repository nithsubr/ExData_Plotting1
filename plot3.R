# The below funtion plots the energy sub-metering Nos. 1,2,3 (in watt-hour of active energy)
# against time period across 2 days 2007-02-01 and 2007-02-02

plot3 <- function()
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
  
  # Get the data to be plotted - energy sub-metering Nos. 1,2,3
  # (in watt-hour of active energy) and timeperiod
  plot_data <- data.frame(x, data$Sub_metering_1, data$Sub_metering_2, data$Sub_metering_3)
  
  # Copy the energy sub-metering Nos. 1,2,3 readings in 3 separate data frames
  a <- plot_data[ ,c(1,2)]
  b <- plot_data[ ,c(1,3)]
  c <- plot_data[ ,c(1,4)]
  
  # Create a Tag to be assigned to above 3 data frames
  # sub-metering No. 1 will be tagged as "a"
  # sub-metering No. 2 will be tagged as "b"
  # sub-metering No. 3 will be tagged as "c"
  a1 <- data.frame(rep("a", length(a[ ,1])))
  b1 <- data.frame(rep("b", length(b[ ,1])))
  c1 <- data.frame(rep("c", length(c[ ,1])))
  
  # Combine the Readings data and Tags. Remove NAs
  a <- cbind(a1,a)
  colnames(a) <- c("Tag", "Date", "Measure")
  a$Measure <- as.vector(a$Measure)
  a <- a[complete.cases(a), ]
  
  # Combine the Readings data and Tags. Remove NAs
  b <- cbind(b1,b)
  colnames(b) <- c("Tag", "Date", "Measure")
  b$Measure <- as.vector(b$Measure)
  b <- b[complete.cases(b), ]
  
  # Combine the Readings data and Tags. Remove NAs
  c <- cbind(c1,c)
  colnames(c) <- c("Tag", "Date", "Measure")
  c$Measure <- as.vector(c$Measure)
  c <- c[complete.cases(c), ]
  
  # Append the above 3 data frames into one with structure - 
  # "Tag", "Date", "Measure"
  final <- rbind(a,b,c)
  
  # Initiate the Graphics Device
  png("plot3.png", width = 480, height = 480)
  
  # Set the row and column counts to display only 1 graps. Also set the margins
  par(mfrow = c(1,1), mar = c(4,4,2,2))

  # Plot the energy sub-metering Nos. 1,2,3 (in watt-hour of active energy)
  # Against time period
  with(final, plot(final$Date, final$Measure, type = "n", xlab = " ", ylab = "Energy Sub Metering"))
  with(final[final$Tag == "a",], points(final[final$Tag == "a", "Date"], final[final$Tag == "a", "Measure"], 
       type = "l", col = "grey"))
  with(final[final$Tag == "b",], points(final[final$Tag == "b", "Date"], final[final$Tag == "b", "Measure"], 
       type = "l", col = "red"))
  with(final[final$Tag == "c",], points(final[final$Tag == "c", "Date"], final[final$Tag == "c", "Measure"], 
       type = "l", col = "blue"))
  legend("topright", lwd = 2, cex = 1, lty = c(1,1,1), col = c("grey", "red", "blue"), 
         legend = c("Sub_Metering_1", "Sub_Metering_2", "Sub_Metering_3"))
  
  # Switch off the graphics device
  dev.off()
  
}