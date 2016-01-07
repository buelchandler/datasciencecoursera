dataFile <- "household_power_consumption.txt" ## assume file in working directory
power <- read.table(dataFile, header=TRUE, sep=";", stringsAsFactors=FALSE, dec=".")

## for this graph, we are only looking at 2007-02-01 and 2007-02-02
subPower <- power[power$Date %in% c("1/2/2007","2/2/2007") ,]
## clean up our data column we're interested in
globalActivePower <- as.numeric(subPower$Global_active_power)
## clean-up dates
datetime <- strptime(paste(subPower$Date, subPower$Time, sep=" "), "%d/%m/%Y %H:%M:%S") 

## prep the graphics device
png("plot2.png", width=480, height=480)

## plot it
plot(datetime,
     globalActivePower,
     type="l",
     xlab="",
     ylab="Global Active Power (kilowatts)"
  )

## end graphics device
dev.off()

