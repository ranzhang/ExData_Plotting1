##init df
data = data.frame(stringsAsFactors=FALSE)
##open conn for read
conn = file("household_power_consumption.txt", open = "rt")

##read each line until EOF and keep needed lines
while (length(x <- readLines(conn, n=1)) > 0){
  
  ##get the header line
  if (grepl("Date", x)){
    data = rbind(data, data.frame(x, stringsAsFactors=F))
  }
  
  ##get data within these ranges, note ^
  if (grepl("^1/2/2007", x) || grepl("^2/2/2007", x)){
    data = rbind(data, data.frame(x, stringsAsFactors=F))
  }
}

close(conn)

##split the string into cols
tmp = do.call(rbind.data.frame, strsplit(as.character(data[,1]), ";"))

##use the first row as col names
names(tmp) = as.character(unlist(tmp[1,]))

##remove the first dummy row
tmp = tmp[-1,]

#combine Date and Time to a new col then convert into POSIX 
tmp$DateTime = paste(tmp$Date, tmp$Time)
tmp$DateTime = strptime(tmp$DateTime, format = "%Y-%m-%d %H:%M:%S")

##more data type clean-up
tmp$Global_active_power = as.numeric(as.character(tmp$Global_active_power))
tmp$Global_reactive_power = as.numeric(as.character(tmp$Global_reactive_power))
tmp$Voltage = as.numeric(as.character(tmp$Voltage))
tmp$Global_intensity = as.numeric(as.character(tmp$Global_intensity))
tmp$Sub_metering_1 = as.numeric(as.character(tmp$Sub_metering_1))
tmp$Sub_metering_2 = as.numeric(as.character(tmp$Sub_metering_2))
tmp$Sub_metering_3 = as.numeric(as.character(tmp$Sub_metering_3))

##generate plots
with(tmp, plot(DateTime, Sub_metering_1, type = "l", xlab="", ylab="Energy sub metering"))
with(tmp, lines(DateTime, Sub_metering_2, col = "red"))
with(tmp, lines(DateTime, Sub_metering_3, col = "purple"))
legend("topright", lwd=1, lty=1, col = c("black", "red", "purple"), cex = 0.5, legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

##copy to file
dev.copy(png, file = "plot3.png")

dev.off()


