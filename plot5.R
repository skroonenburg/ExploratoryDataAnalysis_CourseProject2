# Read the data from the files
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Open the PNG device
png(file = "plot5.png", bg = "white", width = 480, height = 480)

library(ggplot2)
library(plyr)

# Find all of the motor vehicle related classification codes
motorVehicleClassificationCodes <- SCC[grep("motor vehicle", SCC$SCC.Level.Three, ignore.case = TRUE), "SCC"]

# Find all of the measurements for baltimore city
baltimoreRecords <- NEI[NEI$fips == "24510",]

# Find all of the entries in baltimore with a motor vehicle classification code
# OR the entries that were collected "on-road"  (these are meaasurements of motor vehicle emissions)
relevantDataPoints <- baltimoreRecords[baltimoreRecords$SCC %in% motorVehicleClassificationCodes | baltimoreRecords$type == "ON-ROAD",]

# Sum the baltimore values within the motor vehicle classification code by year
summed <- ddply(relevantDataPoints,~year,summarise,total=sum(Emissions))

# Create the bar plot, showing the total summed emissions by year, from coal combustion sources
barplot(summed$total, names.arg=summed$year, main="PM2.5 Motor Vehicle Related Emissions Over Time", ylab="Total PM2.5 emissions (tons)", xlab="Year")

# Create a regression line to show trend
years <- unique(summed$year)
summed$index <- match(summed$year, years)
regressionModel <- lm(total ~ index, summed)
abline(regressionModel, lwd = 2)

# Close the PNG device
dev.off()