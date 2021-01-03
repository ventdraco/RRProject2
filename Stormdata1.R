library(dplyr)
library(R.utils)
filename <- "Dataset.csv.bz2"
##checking if the file already exists, if it doesn't it will download it
if (!file.exists(filename)){
        fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
        download.file(fileURL, filename, method="curl")
} 

##Reading data
bunzip2(filename, "dataset.csv", remove = FALSE, skip = TRUE)
xdata <- read.csv("dataset.csv")

pdata <- select(xdata, BGN_DATE, EVTYPE, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP, FATALITIES, INJURIES)

