# Read the data from the files
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Open the PNG device
png(file = "plot2.png", bg = "white", width = 480, height = 480)

library(plyr)
# Sum the total emissions, by year
baltimoreRecords <- NEI[NEI$fips == 24510,]
summed <- ddply(baltimoreRecords,~year,summarise,total=sum(Emissions))

# Create the ploit
barplot(summed$total, names.arg=summed$year, main="Baltimore City PM2.5 Emissions Over Time", ylab="Total PM2.5 emissions (tons)", xlab="Year")

# Create a regression line to show trend
years <- unique(summed$year)
summed$index <- match(summed$year, years)
regressionModel <- lm(total ~ index, summed)
abline(regressionModel, lwd = 2)

# Close the PNG device
dev.off()