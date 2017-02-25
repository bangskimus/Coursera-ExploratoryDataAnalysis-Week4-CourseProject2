# Coursera - Exploratory Data Analyis - Course Project 2 - Plot #6

# Compare emissions from motor vehicle sources in Baltimore City with emissions from motor 
# vehicle sources in Los Angeles County, California (fips == "06037" Which city has seen greater 
# changes over time in motor vehicle emissions?

# Load the ggplot library
library(ggplot2)

# FUnction to download the data file
downloadData <- function ()
{
  # 1. Download the zipped data from https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
  url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  targetFileName <- "exdata-data-NEI_data.zip"
  
  ## Download only if the zip file hasn't been dowloaded yet.
  if (!file.exists((targetFileName))) {
    download.file(url, targetFileName)  
  }
  
  ## Unzip the files if they haven't been extracted
  unzipFolderName <- "exdata-data-NEI_data"
  unzip(targetFileName, exdir = unzipFolderName)
}

downloadData()

# Check if the data frames have been loaded into memory or not
if(!exists("NEI")) {
  NEI <- readRDS("./exdata-data-NEI_data/summarySCC_PM25.rds")
}
if(!exists("SCC")) {
  SCC <- readRDS("./exdata-data-NEI_data/Source_Classification_Code.rds")
}

# Merge NEI and SCC
if(!exists("mergedNEISCC")) {
  mergedNEISCC <- merge(NEI, SCC, by = "SCC")
}

# Filter only where type is ON-ROAD and fips is 24510 (Baltimore, MD).
filteredNEI <- NEI[(NEI$fips == "24510" | NEI$fips == "06037") & NEI$type == "ON-ROAD",]

# Aggregate by Year and Type
aggregatedTotalByYearAndFips <- aggregate(Emissions ~ year + fips, filteredNEI, sum)

# Rename the fips value to something readable
aggregatedTotalByYearAndFips$fips[aggregatedTotalByYearAndFips$fips == "24510"] <- "Baltimore, MD"
aggregatedTotalByYearAndFips$fips[aggregatedTotalByYearAndFips$fips == "06037"] <- "Los Angeles, CA"


png("plot6.png", width=840, height=480)
g <- ggplot(aggregatedTotalByYearAndFips, aes(factor(year), Emissions))
g <- g + facet_grid(. ~ fips)
g <- g + geom_bar(stat="identity", aes(fill = factor(year)), show.legend = FALSE) + scale_fill_brewer(palette = "Set3") +
  guides(fill = F) +
  xlab("year") +
  ylab(expression('Total PM'[2.5]*" Emissions")) +
  geom_text(aes(label=round(Emissions,0)), col = "BLACK", hjust= 0.5, vjust = -1, size = 2.5) +
  ggtitle('Total Emissions from Motor Vehicles (type = ON-ROAD) from 1999 to 2008\nBaltimore, MD and Los Angeles, CA') +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
print(g)

dev.off()