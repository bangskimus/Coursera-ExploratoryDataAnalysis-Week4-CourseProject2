# Coursera - Exploratory Data Analyis - Course Project 2 - Plot #1

# Determine if the total emissions from PM2.5 decreased in the United States from 1999 to 2008.
# Using the base plotting system, make a plot showing the total PM2.5 emission from all sources 
# for each of the years 1999, 2002, 2005, and 2008.


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

# Just to be sure, filter only for years 1999, 2002, 2005 and 2008
filteredNEI <- NEI[NEI$year %in% c(1999, 2002, 2005, 2008),]

aggregatedTotalByYear <- aggregate(Emissions ~ year, filteredNEI, sum)

png('plot1.png', width=840, height=480)
barplot(height=aggregatedTotalByYear$Emissions, names.arg=aggregatedTotalByYear$year, 
        xlab="years", ylab=expression('total PM'[2.5]*' emission'),
        main=expression('Total PM'[2.5]*' Emissions in the United States per Year'))
dev.off()