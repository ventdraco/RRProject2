library(dplyr)
library(tidyr)
library(R.utils)
library(lubridate)
library(ggplot2)
filename <- "Dataset.csv.bz2"
##checking if the file already exists, if it doesn't it will download it
if (!file.exists(filename)){
        fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Fpdata.csv.bz2"
        download.file(fileURL, filename, method="curl")
} 

##Reading data
xdata <- read.csv(bzfile(filename))

#Calculating data
dim(xdata)
str(xdata)

#Creating the table with variables we are going to use
pdata <- select(xdata, BGN_DATE, STATE, EVTYPE, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP, FATALITIES, INJURIES)

#Checking if there are any NA's in the data
sum(is.na(pdata))

# Format the BGN_DATE variable as a date
pdata$BGN_DATE <- as.Date(pdata$BGN_DATE, "%m/%d/%Y")
pdata$YEAR <- year(pdata$BGN_DATE)

# Latest years are the ones with more relevant information, 48 event since 1996
pdata <- filter(pdata, YEAR >= 1996)

# Only use events with either health impact or economic damage
pdata <- filter(pdata, PROPDMG > 0 | CROPDMG > 0 | FATALITIES > 0 | INJURIES > 0)


#Cleaning Data

table(pdata$PROPDMGEXP)
table(pdata$CROPDMGEXP)

pdata$PROPDMGEXP <- toupper(pdata$PROPDMGEXP)
pdata$CROPDMGEXP <- toupper(pdata$CROPDMGEXP)

pdata <- pdata %>% 
        mutate(CROPDMGFACTOR = case_when(CROPDMGEXP == "" ~ 10^0 * CROPDMG,
                                         CROPDMGEXP == "?" ~ 10^0 * CROPDMG,
                                         CROPDMGEXP == "0" ~ 10^0 * CROPDMG,
                                         CROPDMGEXP == "2" ~ 10^2 * CROPDMG,
                                         CROPDMGEXP == "K" ~ 10^3 * CROPDMG,
                                         CROPDMGEXP == "M" ~ 10^6 * CROPDMG,
                                         CROPDMGEXP == "B" ~ 10^9 * CROPDMG)) %>%
        mutate(PROPDMGFACTOR = case_when(PROPDMGEXP == "" ~ 10^0 * PROPDMG,
                                         PROPDMGEXP == "-" ~ 10^0 * PROPDMG,
                                         PROPDMGEXP == "?" ~ 10^0 * PROPDMG,
                                         PROPDMGEXP == "+" ~ 10^0 * PROPDMG,
                                         PROPDMGEXP == "0" ~ 10^0 * PROPDMG,
                                         PROPDMGEXP == "1" ~ 10^1 * PROPDMG,
                                         PROPDMGEXP == "2" ~ 10^2 * PROPDMG,
                                         PROPDMGEXP == "3" ~ 10^3 * PROPDMG,
                                         PROPDMGEXP == "4" ~ 10^4 * PROPDMG,
                                         PROPDMGEXP == "5" ~ 10^5 * PROPDMG,
                                         PROPDMGEXP == "6" ~ 10^6 * PROPDMG,
                                         PROPDMGEXP == "7" ~ 10^7 * PROPDMG,
                                         PROPDMGEXP == "8" ~ 10^8 * PROPDMG,
                                         PROPDMGEXP == "H" ~ 10^2 * PROPDMG,
                                         PROPDMGEXP == "K" ~ 10^3 * PROPDMG,
                                         PROPDMGEXP == "M" ~ 10^6 * PROPDMG,
                                         PROPDMGEXP == "B" ~ 10^9 * PROPDMG,)) %>%
        mutate(SUMDMG = PROPDMGFACTOR+CROPDMGFACTOR) %>%
        mutate(SUMFATINJ = FATALITIES + INJURIES)

#Check if there is any NA in the new 2 columns
sum(is.na(pdata))

#Summarizing All data to proceed with getting data
sumpdata <- pdata %>%
        group_by(EVTYPE) %>%
        summarize(SUMFATALITIES = sum(FATALITIES),
                  SUMINJURIES = sum(INJURIES),
                  TOTALFATINJ = sum(SUMFATINJ),
                  SUMPROPDMG = sum(PROPDMGFACTOR),
                  SUMCROPDMG = sum(CROPDMGFACTOR),
                  TOTALDMG = sum(SUMDMG))
head(sumpdata)

Impact <- arrange(sumpdata, desc(TOTALFATINJ))
ImpactData <- head(Impact,10)

Economic <- arrange(sumpdata, desc(TOTALDMG))
EconomicData <- head(Economic,10)

#Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?
ImpactData$EVTYPE <- with(ImpactData, reorder(EVTYPE, -TOTALFATINJ))
ImpactDataS <- ImpactData %>%
        gather(key = "Type", value = "TOTALIMPACT", c("SUMFATALITIES", "SUMINJURIES")) %>%
        select(EVTYPE, Type, TOTALIMPACT)
ImpactDataS$Type[ImpactDataS$Type %in% c("SUMFATALITIES")] <- "Fatalities"
ImpactDataS$Type[ImpactDataS$Type %in% c("SUMINJURIES")] <- "Injuries"

g1 <- ggplot(ImpactDataS, aes(x = EVTYPE, y = TOTALIMPACT, fill = Type)) +
        geom_bar(stat = "identity", position = "stack") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        xlab("Event Type") +
        ylab("Total Health Impact") +
        ggtitle("Events with Most Health Impact") +
        theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")
g1

#Across the United States, which types of events have the greatest economic consequences?
EconomicData$EVTYPE <- with(EconomicData, reorder(EVTYPE, -TOTALDMG))
EconomicDataS <- EconomicData %>%
        gather(key = "Type", value = "TOTALDAMAGE", c("SUMPROPDMG", "SUMCROPDMG")) %>%
        select(EVTYPE, Type, TOTALDAMAGE)
EconomicDataS$Type[EconomicDataS$Type %in% c("SUMPROPDMG")] <- "Property damage"
EconomicDataS$Type[EconomicDataS$Type %in% c("SUMCROPDMG")] <- "Crop damage"

g2 <- ggplot(EconomicDataS, aes(x = EVTYPE, y = TOTALDAMAGE, fill = Type)) +
        geom_bar(stat = "identity", position = "stack") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        xlab("Event Type") +
        ylab("Total Economic Impact") +
        ggtitle("Events with Most Economic Impact") +
        theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")
g2
