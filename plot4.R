# Coursera - Exploratory Data Analyis - Course Project 2 - Plot #4

# Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

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

if(!exists("mergedNEISCC")) {
  mergedNEISCC <- merge(NEI, SCC, by = "SCC")
}

# Filter only where the short name contains the word coal.
coalFilter = grepl("coal", mergedNEISCC$Short.Name, ignore.case =TRUE)
filteredNEISCC <- mergedNEISCC[coalFilter,]

# Aggregate by Year and Type
aggregatedTotalByYear <- aggregate(Emissions ~ year, filteredNEISCC, sum)

png("plot4.png", width=840, height=480)
g <- ggplot(aggregatedTotalByYear, aes(year, Emissions))
g <- g + geom_line() + geom_point(aes(size=2, col="RED"), show.legend = FALSE) +   
  xlab("year") +
  ylab(expression('Total PM'[2.5]*" Emissions")) +
  geom_text(aes(label=round(Emissions,digits=2)), hjust= 0.5, vjust = 1.5, size = 3) +
  ggtitle('Total Emissions from Coal in the United States from 1999 to 2008') +
  theme(plot.title = element_text(hjust = 0.5))
print(g)

dev.off()