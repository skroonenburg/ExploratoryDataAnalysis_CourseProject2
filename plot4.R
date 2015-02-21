# Read the data from the files
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Open the PNG device
png(file = "plot4.png", bg = "white", width = 480, height = 480)

library(ggplot2)
library(plyr)

# Find all of the coal related classification codes
coalRows = grep("coal", SCC$SCC.Level.Three,ignore.case = TRUE)

# Find all of the combustion boiler related classification codes
combustionRows <- which(SCC$SCC.Level.One == "External Combustion Boilers" )

# Find the intersection of the 2 sets, to find the coal combustion classification codes
coalCombustionSCCValues <- SCC[intersect(coalRows,combustionRows),"SCC"]

# Sum these values by year
summed <- ddply(NEI[NEI$SCC %in% coalCombustionSCCValues,],~year,summarise,total=sum(Emissions))

# Create the bar plot, showing the total summed emissions by year, from coal combustion sources
barplot(summed$total, names.arg=summed$year, main="PM2.5 Coal Combustion Related Emissions Over Time", ylab="Total PM2.5 emissions (tons)", xlab="Year")

# Create a regression line to show trend
years <- unique(summed$year)
summed$index <- match(summed$year, years)
regressionModel <- lm(total ~ index, summed)
abline(regressionModel, lwd = 2)

# Close the PNG device
dev.off()