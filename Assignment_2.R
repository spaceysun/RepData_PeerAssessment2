setwd('c:/git/RepData_PeerAssessment2')

# Extracting the bz file and storing it in a data frame

library(plyr)
library(data.table)
library(ggplot2)

Thoughts on the Questions

# Question 1
# Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?

## Variables relating to population health include fatalities and injuries. Let's first check the relationship between types of events and fatalities, types of events and injuries, respectively. Then we may have to come up with a method to combine both scenarios.

## Subset the data to contain only EVTYPE and INJURIES


data <- read.csv('repdata-data-StormData.csv.bz2', stringsAsFactors = FALSE, header = TRUE , colClasses = c(rep('character', 22), 'numeric', 'numeric', rep('character', 13)), fill = TRUE, nrow = 10000)

## So much paaaaain. Hours of pain of dealing with importing the huge csv data into the system with endless error messages.

## Use 'fread' instead

data_event <- read.csv('repdata-data-StormData-2.csv', stringsAsFactors = FALSE, header = TRUE)
### This takes 37 sec.

data_temp <- fread('repdata-data-StormData-2.csv', stringsAsFactors = FALSE, header = TRUE, nrow = 1000)
colnames(data_temp)
### Defining the columns to read

data_event <- fread('repdata-data-StormData-2.csv', stringsAsFactors = FALSE, header = TRUE, select = c(8, 23, 24))
### This takes 1.3 sec, by utilizing the efficiency of fread and narrowing down on the columns I am interested in.


## Clean up the EVTYPE contents
for (i in 1:nrow(data_event)) {
    if (data_event[i,1] == 'ASTRONOMICAL LOW TIDE') {
        data_event$EVTYPE_1[i] = 'ASTRONOMICAL LOW TIDE'
    }
    else {
        data_event$EVTYPE_1[i] = 'BLANK'
    }
}
### This is painfully slow.

## Try another way to clean up the EVYPTE contents
data_event$INDEX = as.numeric(row.names(data_event))
event_names = c('ASTRONOMICAL LOW TIDE', 'AVALANCHE', 'BLIZZARD', 'COASTAL FLOOD', 'COLD', 'DEBRIS FLOW', 'DENSE FOG', 'DENSE SMOKE', 'DROUGHT', 'DUST DEVIL', 'DUST STORM', 'EXCESSIVE HEAT', 'EXTREME COLD', 'FLASH FLOOD', 'FLOOD', 'FREEZING FOG', 'FROST', 'FREEZE', 'FUNNEL CLOUD', 'HAIL', 'HEAT', 'HEAVY RAIN', 'HEAVY SNOW', 'HIGH SURF', 'HURRICANE', 'TYPHOON', 'ICE STORM', 'LAKESHORE FLOOD', 'LAKE-EFFECT SNOW', 'LIGHTNING', 'MARINE HAIL', 'MARINE HIGH WIND', 'MARINE STRONG WIND', 'MARINE THUNDERSTORM WIND', 'RIP CURRENT', 'SEICHE', 'SLEET', 'STORM TIDE', 'STRONG WIND', 'THUNDERSTORM WIND', 'TORNADO', 'TROPICAL DEPRESSION', 'TROPICAL STORM', 'TSUNAMI', 'VOLCANIC ASH', 'WATERSPOUT', 'WILDFIRE', 'WINTER STORM', 'WINTER WEATHER')



data_event_captured = data_event[(data_event$EVTYPE == event_names[1]),]

for (i in 2:length(event_names)) {
    data_event_captured = rbind(data_event_captured, data_event[(data_event$EVTYPE == event_names[i]),])
}

### Check missing EVTYPES
event_names_missed <- unique(data_event_captured$EVTYPE)
setdiff(event_names, event_names_missed)

### Check if there are cases of setdiff results in data_event
subset(data_event, EVTYPE == 'DEBRIS FLOW')
subset(data_event, EVTYPE == 'STORM TIDE')

### Obtaining missed records
data_event_missed_index = setdiff(data_event$INDEX, data_event_captured$INDEX)
data_event_missed <- data_event[(data_event_missed_index),]

### Calculate total injuries and fatalities of data_event_captured and data_event_missed
data_event_captured_tot <- ddply(data_event_captured, 'EVTYPE', summarize, INJURIES_TOT = sum(as.numeric(INJURIES)), FATALITIES_TOT = sum(as.numeric(FATALITIES)))
data_event_missed_tot <- ddply(data_event_missed, 'EVTYPE', summarize, INJURIES_TOT = sum(as.numeric(INJURIES)), FATALITIES_TOT = sum(as.numeric(FATALITIES)))

### Calculate median of fatalities/injuries of data_event_captured_tot
median(data_event_captured_tot$INJURIES_TOT)
median(data_event_captured_tot$FATALITIES_TOT)

### Identify major missed events
major_missed <- subset(data_event_missed_tot, (FATALITIES_TOT > 20 | INJURIES_TOT > 100))

### From major_missed, we can ditch 'useless' TYPES and then, combine the meaningful ones into data_event_2

event_names_2 = c('COLD/WIND CHILL', 'EXTREME COLD/WIND CHILL', 'EXTREME HEAT', 'FOG', 'GLAZE', 'HEAT WAVE', 'HEAVE SURF/HIGH SURF', 'HIGH WIND', 'HIGH WINDS', 'HURRICANE/TYPHOON', 'ICE', 'LANDSLIDE', 'RIP CURRENTS', 'THUNDERSTORM WINDS', 'TSTM WIND', 'URBAN/SML STREAM FLD', 'WILD FIRES', 'WILD/FOREST FIRE', 'WIND', 'WINTER WEATHER/MIX')

data_event_captured_2 = data_event[(data_event$EVTYPE == event_names_2[1]),]

for (i in 2:length(event_names_2)) {
    data_event_captured_2 = rbind(data_event_captured_2, data_event[(data_event$EVTYPE == event_names_2[i]),])
}

data_event_captured_2_tot <- ddply(data_event_captured_2, 'EVTYPE', summarize, INJURIES_TOT = sum(as.numeric(INJURIES)), FATALITIES_TOT = sum(as.numeric(FATALITIES)))

### Combine the two captured total datasets
data_event_captured_all_tot <- rbind(data_event_captured_tot, data_event_captured_2_tot) 

### hurricane/typhoon
data_event_captured_all_tot[56,2:3] = (data_event_captured_all_tot[24, 2:3]) + (data_event_captured_all_tot[42, 2:3]) + (data_event_captured_all_tot[56, 2:3])

### thunderstorm = tstm
data_event_captured_all_tot[60, 2:3] = (data_event_captured_all_tot[60, 2:3]) + (data_event_captured_all_tot[61, 2:3]) + (data_event_captured_all_tot[37, 2:3])

### Combining heat, excessive heat and extreme heat
data_event_captured_all_tot[20, 2:3] = (data_event_captured_all_tot[20, 2:3]) + (data_event_captured_all_tot[11, 2:3]) + (data_event_captured_all_tot[50, 2:3])

### delete lines 24, 42, 61 and 37
data_event_captured_all_tot <- data_event_captured_all_tot[-c(11, 24, 42, 37, 50, 61),]


### Get the 25% upper limit data of total injuries or fatalities
summary(data_event_captured_all_tot$INJURIES_TOT)
summary(data_event_captured_all_tot$FATALITIES_TOT)
data_event_top <- subset(data_event_captured_all_tot, INJURIES_TOT > 686 | FATALITIES_TOT > 125)

### Sort the data_event_top dataset
data_event_top_injuries <- data_event_top[order(data_event_top$INJURIES_TOT, decreasing = TRUE),][1:10,]
data_event_top_fatalities <- data_event_top[order(data_event_top$FATALITIES_TOT, decreasing = TRUE),][1:10,]

### Reindex
rownames(data_event_top_injuries) <- 1:nrow(data_event_top_injuries)
rownames(data_event_top_fatalities) <- 1:nrow(data_event_top_fatalities)

### Plots
positions_injuries <- rev(data_event_top_injuries$EVTYPE)
positions_fatalities <- rev(data_event_top_fatalities$EVTYPE)

plot1 <- ggplot(data_event_top_injuries, aes(x = EVTYPE, y = INJURIES_TOT, label = INJURIES_TOT)) + geom_bar(stat = 'identity') + scale_x_discrete(limits = positions_injuries, name = 'Event Type') + coord_flip() + scale_y_continuous(name = "Injuries Caused by Severe Weather Events in the USA (1950 - 2011)", limits = c(0, 100000)) + theme(axis.title.x = element_text(size=15, face = 'bold', vjust = 0), axis.title.y = element_text(size=15, face = 'bold'), axis.text.x = element_text(size=15, color = 'black'), axis.text.y = element_text(size=13, color='black')) + geom_text(hjust = -0.5, aes(size = INJURIES_TOT)) + scale_size(range = c(3,5)) + theme(legend.position = 'none')

plot2 <- ggplot(data_event_top_fatalities, aes(x = EVTYPE, y = FATALITIES_TOT, label = FATALITIES_TOT)) + geom_bar(stat = 'identity') + scale_x_discrete(limits = positions_fatalities, name = 'Event Type') + coord_flip() + scale_y_continuous(name = "Fatalities Caused by Severe Weather Events in the USA (1950 - 2011)", limits = c(0, 6000)) + theme(axis.title.x = element_text(size=15, face = 'bold', vjust = 0), axis.title.y = element_text(size=15, face = 'bold'), axis.text.x = element_text(size=15, color = 'black'), axis.text.y = element_text(size=13, color='black')) + geom_text(hjust = -0.5, aes(size = INJURIES_TOT)) + scale_size(range = c(3,5)) + theme(legend.position = 'none') 

event_plot <- grid.arrange(plot1, plot2)


# Question 2 Across the United States, which types of events have the greatest economic consequences?

## Identify the columns of data to be analyzed for this purpose
data_temp2 <- fread('repdata-data-StormData-2.csv', stringsAsFactors = FALSE, header = TRUE, nrow = 1000)
colnames(data_temp2)

## Apparently, property damage and crop damage are to be considered. We may also consider fatalities and injuries.

data_eco <- fread('repdata-data-StormData-2.csv', stringsAsFactors = FALSE, header = TRUE, select = c(8, 23:28))

## Clean up the EVTYPE contents as before

data_eco$INDEX = as.numeric(row.names(data_eco))
event_names = c('ASTRONOMICAL LOW TIDE', 'AVALANCHE', 'BLIZZARD', 'COASTAL FLOOD', 'COLD', 'DEBRIS FLOW', 'DENSE FOG', 'DENSE SMOKE', 'DROUGHT', 'DUST DEVIL', 'DUST STORM', 'EXCESSIVE HEAT', 'EXTREME COLD', 'FLASH FLOOD', 'FLOOD', 'FREEZING FOG', 'FROST', 'FREEZE', 'FUNNEL CLOUD', 'HAIL', 'HEAT', 'HEAVY RAIN', 'HEAVY SNOW', 'HIGH SURF', 'HURRICANE', 'TYPHOON', 'ICE STORM', 'LAKESHORE FLOOD', 'LAKE-EFFECT SNOW', 'LIGHTNING', 'MARINE HAIL', 'MARINE HIGH WIND', 'MARINE STRONG WIND', 'MARINE THUNDERSTORM WIND', 'RIP CURRENT', 'SEICHE', 'SLEET', 'STORM TIDE', 'STRONG WIND', 'THUNDERSTORM WIND', 'TORNADO', 'TROPICAL DEPRESSION', 'TROPICAL STORM', 'TSUNAMI', 'VOLCANIC ASH', 'WATERSPOUT', 'WILDFIRE', 'WINTER STORM', 'WINTER WEATHER')



data_eco_captured = data_eco[(data_eco$EVTYPE == event_names[1]),]

for (i in 2:length(event_names)) {
    data_eco_captured = rbind(data_eco_captured, data_eco[(data_eco$EVTYPE == event_names[i]),])
}

### Check missing EVTYPES
event_names_missed <- unique(data_eco_captured$EVTYPE)
setdiff(event_names, event_names_missed)

### Check if there are cases of setdiff results in data_event
subset(data_eco, EVTYPE == 'DEBRIS FLOW')
subset(data_eco, EVTYPE == 'STORM TIDE')

### Obtaining missed records
data_eco_missed_index = setdiff(data_eco$INDEX, data_eco_captured$INDEX)
data_eco_missed <- data_eco[(data_eco_missed_index),]



### Picking out all data with viable property/crop damage data
data_eco_captured_calc <- subset(data_eco_captured, (PROPDMGEXP == '' | PROPDMGEXP == 'B' | PROPDMGEXP == 'K' | PROPDMGEXP == 'M' | PROPDMGEXP == 'm') & (CROPDMGEXP == '' | CROPDMGEXP == 'B' | CROPDMGEXP == 'K' | CROPDMGEXP == 'k' | CROPDMGEXP == 'M'))

data_eco_missed_calc <- subset(data_eco_missed, (PROPDMGEXP == '' | PROPDMGEXP == 'B' | PROPDMGEXP == 'K' | PROPDMGEXP == 'M' | PROPDMGEXP == 'm') & (CROPDMGEXP == '' | CROPDMGEXP == 'B' | CROPDMGEXP == 'K' | CROPDMGEXP == 'k' | CROPDMGEXP == 'M'))

### Calculate individual injuries, fatalities, and damages of data_event_captured and data_event_missed
### According to 'http://www.msha.gov/s&hinfo/costgenerator/costgenerator.htm', we set USD 910k for one death and USD 28k for one injury. 

data_eco_captured_calc$PROPDMGTOT = 0
data_eco_captured_calc$CROPDMGTOT = 0
data_eco_captured_calc$DMGTOT = 0
data_eco_captured_calc$PROPDMGEXP2 = 0
data_eco_captured_calc$CROPDMGEXP2 = 0

data_eco_captured_calc[data_eco_captured_calc$PROPDMGEXP == 'B',]$PROPDMGEXP2 <- 1000000000
data_eco_captured_calc[data_eco_captured_calc$PROPDMGEXP == 'M',]$PROPDMGEXP2 <- 1000000
data_eco_captured_calc[data_eco_captured_calc$PROPDMGEXP == 'm',]$PROPDMGEXP2 <- 1000000
data_eco_captured_calc[data_eco_captured_calc$PROPDMGEXP == 'k',]$PROPDMGEXP2 <- 1000

data_eco_captured_calc[data_eco_captured_calc$CROPDMGEXP == 'B',]$CROPDMGEXP2 <- 1000000000
data_eco_captured_calc[data_eco_captured_calc$CROPDMGEXP == 'M',]$CROPDMGEXP2 <- 1000000
data_eco_captured_calc[data_eco_captured_calc$CROPDMGEXP == 'm',]$CROPDMGEXP2 <- 1000000
data_eco_captured_calc[data_eco_captured_calc$CROPDMGEXP == 'k',]$CROPDMGEXP2 <- 1000

data_eco_captured_calc$DMGTOT = as.numeric(data_eco_captured_calc$FATALITIES) * 910000 + as.numeric(data_eco_captured_calc$INJURIES) * 28000 + as.numeric(data_eco_captured_calc$PROPDMG) * data_eco_captured_calc$PROPDMGEXP2 + as.numeric(data_eco_captured_calc$CROPDMG) * data_eco_captured_calc$CROPDMGEXP2 



data_eco_missed_calc$PROPDMGTOT = 0
data_eco_missed_calc$CROPDMGTOT = 0
data_eco_missed_calc$DMGTOT = 0
data_eco_missed_calc$PROPDMGEXP2 = 0
data_eco_missed_calc$CROPDMGEXP2 = 0

data_eco_missed_calc[data_eco_missed_calc$PROPDMGEXP == 'B',]$PROPDMGEXP2 <- 1000000000
data_eco_missed_calc[data_eco_missed_calc$PROPDMGEXP == 'M',]$PROPDMGEXP2 <- 1000000
data_eco_missed_calc[data_eco_missed_calc$PROPDMGEXP == 'm',]$PROPDMGEXP2 <- 1000000
data_eco_missed_calc[data_eco_missed_calc$PROPDMGEXP == 'k',]$PROPDMGEXP2 <- 1000

data_eco_missed_calc[data_eco_missed_calc$CROPDMGEXP == 'B',]$CROPDMGEXP2 <- 1000000000
data_eco_missed_calc[data_eco_missed_calc$CROPDMGEXP == 'M',]$CROPDMGEXP2 <- 1000000
data_eco_missed_calc[data_eco_missed_calc$CROPDMGEXP == 'm',]$CROPDMGEXP2 <- 1000000
data_eco_missed_calc[data_eco_missed_calc$CROPDMGEXP == 'k',]$CROPDMGEXP2 <- 1000

data_eco_missed_calc$DMGTOT = as.numeric(data_eco_missed_calc$FATALITIES) * 910000 + as.numeric(data_eco_missed_calc$INJURIES) * 28000 + as.numeric(data_eco_missed_calc$PROPDMG) * data_eco_missed_calc$PROPDMGEXP2 + as.numeric(data_eco_missed_calc$CROPDMG) * data_eco_missed_calc$CROPDMGEXP2 



data_eco_captured_tot <- ddply(data_eco_captured_calc, 'EVTYPE', summarize, TOTAL = sum(DMGTOT))

data_eco_missed_tot <- ddply(data_eco_missed_calc, 'EVTYPE', summarize, TOTAL = sum(DMGTOT))



### Find top 20 expensive disasters
data_eco_top_captured <- data_eco_captured_tot[order(data_eco_captured_tot$TOTAL, decreasing = TRUE),][1:20,]
data_eco_top_missed <- data_eco_missed_tot[order(data_eco_missed_tot$TOTAL, decreasing = TRUE),][1:20,]
rownames(data_eco_top_captured) = c(1:nrow(data_eco_top_captured))
rownames(data_eco_top_missed) = c(1:nrow(data_eco_top_missed))

### hurricane/typhoon
data_eco_top_captured[6,2] = data_eco_top_captured[6,2] + data_eco_top_captured[19,2] + data_eco_top_missed[1,2]
data_eco_top_captured[6,1] = 'HURRICANE/TYPHOON'
data_eco_top_missed <- data_eco_top_missed[-1,]
data_eco_top_captured <- data_eco_top_captured[-19,]
rownames(data_eco_top_missed) = c(1:nrow(data_eco_top_missed))
rownames(data_eco_top_captured) = c(1:nrow(data_eco_top_captured))

### thunderstorm = tstm
data_eco_top_captured[11,2] = data_eco_top_captured[11,2] + data_eco_top_missed[10,2] + data_eco_top_missed[5,2]
data_eco_top_missed <- data_eco_top_missed[-c(5,10),]
rownames(data_eco_top_missed) = c(1:nrow(data_eco_top_missed))
rownames(data_eco_top_captured) = c(1:nrow(data_eco_top_captured))

### combine two top datasets and order
data_eco_top <- rbind(data_eco_top_missed, data_eco_top_captured)
data_eco_top <- data_eco_top[order(data_eco_top$TOTAL, decreasing = TRUE),][1:20,]
rownames(data_eco_top) = c(1:nrow(data_eco_top))

### find top10 the data_eco_top contents
data_eco_top <- data_eco_top[1:10,]

### get million data
data_eco_top$TOTALB = data_eco_top$TOTAL / 1000000000
data_eco_top$TOTALB = round(data_eco_top$TOTALB, digits = 1)

## Plots
positions_eco <- rev(data_eco_top$EVTYPE)

ggplot(data_eco_top, aes(x = EVTYPE, y = TOTALB, label = TOTALB)) + geom_bar(stat = 'identity') + scale_x_discrete(limits = positions_eco, name = 'Event Type') + coord_flip() + scale_y_continuous(name = "Damages Caused by Severe Weather Events in the USA (1950 - 2011) ($ Billion) ", limits = c(0, 200)) + theme(axis.title.x = element_text(size=15, face = 'bold', vjust = 0), axis.title.y = element_text(size=15, face = 'bold'), axis.text.x = element_text(size=15, color = 'black'), axis.text.y = element_text(size=13, color='black')) + geom_text(hjust = -0.2, aes(size = TOTALB)) + scale_size(range = c(3,5)) + theme(legend.position = 'none')
