# Read the data from the files
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Open the PNG device
png(file = "plot3.png", bg = "white", width = 480, height = 480)

library(ggplot2)
library(plyr)

# Filter the baltimore records
baltimoreRecords <- NEI[NEI$fips == 24510,]
summed <- ddply(baltimoreRecords,~type+year,summarise,total=sum(Emissions))

# Create the plot, with facets for each source type
qplot(year, total, data = summed, facets = . ~ type, geom = "bar", stat = "identity")+ xlab("Year") + ylab("Total PM2.5 emissions (tons)") + ggtitle("Baltimore PM2.5 Emissions By Source Type Over Time") 

# Close the PNG device
dev.off()