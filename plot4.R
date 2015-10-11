# The below funtion plots 4 separate graphs - 
# Graph#1 - household global minute-averaged active power (in kilowatt)
# Graph#2 - minute-averaged voltage (in volt)
# Graph#3 - energy sub-metering Nos. 1,2,3 (in watt-hour of active energy)
# Gpaph#4 - household global minute-averaged reactive power (in kilowatt)
# against time period across 2 days 2007-02-01 and 2007-02-02

plot4 <- function()
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

  # Initiate the Graphics Device
  png("plot4.png", width = 480, height = 480) 
  
  # Set the row and column counts to display 4 graps. Also set the margins
  par(mfrow = c(2,2), mar = c(4,4,2,2), oma = c(0, 0, 2, 0))

  # Begin of Block
  with(data, {

############## PLOT 1
  plot_data1 <- data.frame(x, as.numeric(as.vector(data$Global_active_power)))
  colnames(plot_data1) <- c("Date", "Global_active_power")
  
  plot(plot_data1$Date, plot_data1$Global_active_power, type = "l", ylab = "Global Active Power", 
       xlab = " ")
  

############## PLOT 2
  plot_data2 <- data.frame(x, as.numeric(as.vector(data$Voltage)))
  colnames(plot_data2) <- c("Date", "Voltage")
  
  plot(plot_data2$Date, plot_data2$Voltage, type = "l", ylab = "Voltage", 
       xlab = "datetime")
  

############## PLOT 3  
  plot_data3 <- data.frame(x, data$Sub_metering_1, data$Sub_metering_2, data$Sub_metering_3)
  
  a <- plot_data3[ ,c(1,2)]
  b <- plot_data3[ ,c(1,3)]
  c <- plot_data3[ ,c(1,4)]
  
  
  a1 <- data.frame(rep("a", length(a[ ,1])))
  b1 <- data.frame(rep("b", length(b[ ,1])))
  c1 <- data.frame(rep("c", length(c[ ,1])))
  
  a <- cbind(a1,a)
  colnames(a) <- c("Tag", "Date", "Measure")
  a$Measure <- as.vector(a$Measure)
  a <- a[complete.cases(a), ]
  
  b <- cbind(b1,b)
  colnames(b) <- c("Tag", "Date", "Measure")
  b$Measure <- as.vector(b$Measure)
  b <- b[complete.cases(b), ]
  
  c <- cbind(c1,c)
  colnames(c) <- c("Tag", "Date", "Measure")
  c$Measure <- as.vector(c$Measure)
  c <- c[complete.cases(c), ]
  
  final <- rbind(a,b,c)
  
  
  
  with(final, plot(final$Date, final$Measure, type = "n", xlab = " ", ylab = "Energy Sub Metering"))
  with(final[final$Tag == "a",], points(final[final$Tag == "a", "Date"], final[final$Tag == "a", "Measure"], 
                                        type = "l", col = "grey"))
  with(final[final$Tag == "b",], points(final[final$Tag == "b", "Date"], final[final$Tag == "b", "Measure"], 
                                        type = "l", col = "red"))
  with(final[final$Tag == "c",], points(final[final$Tag == "c", "Date"], final[final$Tag == "c", "Measure"], 
                                        type = "l", col = "blue"))
  legend("topright", lwd = 2, cex = 0.5, lty = c(1,1,1), col = c("grey", "red", "blue"), 
         legend = c("Sub_Metering_1", "Sub_Metering_2", "Sub_Metering_3"))
  
  
############## PLOT 4
  plot_data4 <- data.frame(x, as.numeric(as.vector(data$Global_reactive_power)))
  colnames(plot_data4) <- c("Date", "Global_reactive_power")
  
  plot(plot_data4$Date, plot_data4$Global_reactive_power, type = "l", ylab = "Global_reactive_power", 
       xlab = "datetime", ylim = c(0,0.5))
  

}) # End of Block

  # Switch Off the Plotting Device
  dev.off()
  
}