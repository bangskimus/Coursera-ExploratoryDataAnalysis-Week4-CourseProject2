# Coursera - Exploratory Data Analyis - Course Project 2 - Plot #3

# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
# Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make 
# a plot answer this question.

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

# filter only for fips == "24510" (Baltimore, Maryland)
filteredNEI <- NEI[NEI$fips == "24510",]

# Aggregate by Year and Type
aggregatedTotalByYearAndType <- aggregate(Emissions ~ year + type, filteredNEI, sum)

png("plot3.png", width=840, height=480)
g <- ggplot(aggregatedTotalByYearAndType, aes(year, Emissions, color = type))
g <- g + geom_line() + 
  xlab("year") +
  ylab(expression('Total PM'[2.5]*" Emissions")) +
  ggtitle('Total Emissions in Baltimore City, MD from 1999 to 2008') +
  theme(plot.title = element_text(hjust = 0.5))
print(g)


dev.off()