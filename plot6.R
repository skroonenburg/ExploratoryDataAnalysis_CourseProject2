# Read the data from the files
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Open the PNG device
png(file = "plot6.png", bg = "white", width = 480, height = 480)

library(ggplot2)
library(plyr)

# Find all of the motor vehicle related classification codes
motorVehicleClassificationCodes <- SCC[grep("motor vehicle", SCC$SCC.Level.Three, ignore.case = TRUE), "SCC"]

# Find all of the measurements for baltimore city or LA county
relevantLocationRecords <- NEI[(NEI$fips == "24510" | NEI$fips == "06037"),]

# Find all of the entries in baltimore with a motor vehicle classification code
# OR the entries that were collected "on-road"  (these are meaasurements of motor vehicle emissions)
relevantDataPoints <- relevantLocationRecords[relevantLocationRecords$SCC %in% motorVehicleClassificationCodes | relevantLocationRecords$type == "ON-ROAD",]

# Sum the baltimore values within the motor vehicle classification code by year
summed <- ddply(relevantDataPoints,~fips+year,summarise,total=sum(Emissions))

# Create the plot, with facets for each location
qplot(year, total, data = summed, facets = . ~ fips, geom = "bar", stat = "identity")+ xlab("Year") + ylab("Total PM2.5 emissions (tons)") + ggtitle("PM2.5 Motor Vehicle Emissions By Location Over Time") 

# Close the PNG device
dev.off()