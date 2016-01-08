dataFile <- "household_power_consumption.txt" ## assume file in working directory
power <- read.table(dataFile, header=TRUE, sep=";", stringsAsFactors=FALSE, na.strings = "?")

## for this graph, we are only looking at 2007-02-01 and 2007-02-02
power$Date <- as.Date(power$Date, format="%d/%m/%Y")
subPower <- power[(power$Date=="2007-02-01") | (power$Date=="2007-02-02"),]
## clean up our data column we're interested in
subPower$Global_active_power <- as.numeric(as.character(subPower$Global_active_power))

## prep the graphics device
png("plot1.png", width=480, height=480)

## plot it
with(subPower, hist(Global_active_power, main="Global Active Power", col="red", xlab="Global Active Power (kilowatts)"))

## end graphics device
dev.off()

