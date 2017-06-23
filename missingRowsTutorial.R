###############################################################
#### How to insert rows for missing reports in ESM studies ####
###############################################################

# Problem: I want to look at within-person predictors of missingness, but the
# ESM surveys were completed on Qualtrics, which only generated rows when reports were completed
# So, we have no record of which reports were missed.

# Data: Subset of meta-data of ESM participants from the Personality and Interpersonal Roles Study
# esm.IDnum.w1 = participant ID
# esm.fileDate.w1 = YYYYMMDD
# esm.PRO03.w1 = hour block variable. 1 = 11am-12pm, 2 = 2-3pm, 3 = 5-6pm, 4 = 8-9pm
# esm.PRO02.w1 = basically the same as esm.PRO03.w1, with extra procedural details
  # but 6 = the participant's first survey (practice survey in the lab) 
  # and 5 = the participant's last completed survey (not important for this tutorial)

# Complete data = participants have reports 1, 2, 3, and 4 for all days they were in the study.
# We want to add rows for the missed reports.

#### Load packages & data ####

library(plyr); library(data.table); library(chron)

setwd('yourWorkingDirectory')
esm <- read.csv('missingRowsData.csv',stringsAsFactors=FALSE)

#### Get list of missing rows for each unique combination of ID and file date ####

# Concatenate ID & Date
esm$ID.fileDate <- paste0(esm$esm.IDnum.w1,".",esm$esm.fileDate.w1)  

# Unique combinations of IDs and file dates
ID.fileDate <- unique(esm$ID.fileDate)

missingStats <- data.frame(ID.fileDate)

# indicate which reports were completed (1, 2, 3, 4) on each day for each participant

for(i in ID.fileDate){
  for(j in c(1,2,3,4)){
    
    missingStats[which(missingStats$ID.fileDate==i),paste0('complete',j)] <- nrow(esm[which(esm$ID.fileDate==i &
                                                                                              esm$esm.PRO03.w1==j),])
    
  }
}

# take a look at this dataframe: 1 = complete, 0 = missed

head(missingStats)

# convert missingStats to long format

missingStatsLong <- reshape(missingStats, varying=c(2:5), direction="long", 
                            idvar="ID.fileDate", sep="", timevar="esm.PRO03.w1")

missingStatsLong <- setorder(missingStatsLong,ID.fileDate,esm.PRO03.w1)

# only keep missing rows

missingStatsLong <- missingStatsLong[which(missingStatsLong$complete==0),]

# separate out ID & date variables

missingStatsLong$esm.IDnum.w1 <- gsub( "\\..*$", "", missingStatsLong$ID.fileDate )
missingStatsLong$esm.fileDate.w1 <- sub('.*\\.', '', missingStatsLong$ID.fileDate)

# create esm.PRO02.w1 variable

missingStatsLong$esm.PRO02.w1 <- missingStatsLong$esm.PRO03.w1

# reorder new rows

missingStatsLong <- missingStatsLong[,c("esm.IDnum.w1", "esm.fileDate.w1", "esm.PRO03.w1", "esm.PRO02.w1", "complete")]

# add completeness variable to main esm dataframe

esm$complete <- 1

# reorder main dataframe

esm <- esm[,c("esm.IDnum.w1", "esm.fileDate.w1", "esm.PRO03.w1","esm.PRO02.w1",  "complete")]

# combine the two dataframes and reorder temporally

esm <- rbind(esm,missingStatsLong)
esm <- setorder(esm,esm.IDnum.w1,esm.fileDate.w1,esm.PRO03.w1)

#### Exclude "missing" reports that were before the participant's *actual* first report ####
# The code above inserted "missing" reports for all hour blocks, but participants may have only
# started the study at hour block 2, 3 or 4
# So, if they started the study at hour block 3, they did not actually miss hour blocks 1 & 2
# because they had not yet started teh study at that point

# Get first days for each participant

firstDay.rows <- esm[which(esm$esm.PRO02.w1==6),]

firstDay.rows$ID.fileDate <- paste0(firstDay.rows$esm.IDnum.w1,".",firstDay.rows$esm.fileDate.w1)
esm$ID.fileDate <- paste0(esm$esm.IDnum.w1,".",esm$esm.fileDate.w1)

firstDay <- esm[which(esm$ID.fileDate %in% firstDay.rows$ID.fileDate),]

# renumber rows

row.names(firstDay) <- 1:nrow(firstDay)

# get row numbers for first report

firstDay$rowNumber <- as.numeric(row.names(firstDay))

firstRows <- firstDay[which(firstDay$esm.PRO02.w1==6),c('esm.IDnum.w1','rowNumber')]
names(firstRows)[2] <- 'firstRow'

# merge

firstDay <- merge(firstDay,firstRows)

# subtract first row number from current row number

firstDay$rowDif <- firstDay$rowNumber - firstDay$firstRow

firstDay$valid <- 1

# for each participant ID, remove the rows for which the rowdif is negative
  # because if rowdif is negative, the "missing" row is actually before the participant's first report

firstDay[which(firstDay$rowDif < 0),'valid'] <- 0

# save vector of invalid rows

firstDay$ID.fileDate.hour <- paste0(firstDay$esm.IDnum.w1,'.',firstDay$esm.fileDate.w1,'.',
                                    firstDay$esm.PRO03.w1)

invalid <- firstDay[which(firstDay$valid==0),'ID.fileDate.hour']

# remove invalid rows from main ESM dataframe

esm$ID.fileDate.hour <- paste0(esm$esm.IDnum.w1,'.',esm$esm.fileDate.w1,'.',
                               esm$esm.PRO03.w1)

esm <- esm[-which(esm$ID.fileDate.hour %in% invalid),]

# Remove training surveys

esm <- esm[which(esm$esm.PRO02.w1!=6),]

# Renumber rows

row.names(esm) <- 1:nrow(esm)

esm <- esm[,c('esm.IDnum.w1','esm.fileDate.w1','esm.PRO03.w1','complete','ID.fileDate')]

#### Final step: Add missed reports from completely missing days ####
# the above code depended on there being at least one report on a given day
# but participants may have missed entire days
# I want to also insert the four missed reports for each of these missed days

# get first and last day (i.e., last day with any reports)

esm.IDnum.w1 <- unique(esm$esm.IDnum.w1)
allDays <- data.frame(esm.IDnum.w1)

for(i in esm.IDnum.w1){
  allDays[which(allDays$esm.IDnum.w1==i),'firstDay'] <- unique(esm[which(esm$esm.IDnum.w1==i),'esm.fileDate.w1'])[1]
  allDays[which(allDays$esm.IDnum.w1==i),'lastDay'] <- tail(esm[which(esm$esm.IDnum.w1==i),'esm.fileDate.w1'],n=1)
}

# reformat dates

allDays$yyFirst <- substr(allDays$firstDay,1,4)
allDays$mmFirst <- substr(allDays$firstDay,5,6)
allDays$ddFirst <- substr(allDays$firstDay,7,8)

allDays$yyLast <- substr(allDays$lastDay,1,4)
allDays$mmLast <- substr(allDays$lastDay,5,6)
allDays$ddLast <- substr(allDays$lastDay,7,8)

allDays$firstDate <- paste0(allDays$yyFirst,'/',allDays$mmFirst,'/',allDays$ddFirst)
allDays$lastDate <- paste0(allDays$yyLast,'/',allDays$mmLast,'/',allDays$ddLast)

allDays$firstDate <- chron(dates=allDays$firstDate,
                           format=c('y/m/d'))

allDays$lastDate <- chron(dates=allDays$lastDate,
                          format=c('y/m/d'))

# use seq() to generate a sequence of dates from the first day to the last day
# store them in dayList

dayList <- list()

for(i in esm.IDnum.w1){
  dayList[[i]] <- seq(allDays[which(allDays$esm.IDnum.w1==i),'firstDate'],allDays[which(allDays$esm.IDnum.w1==i),'lastDate'], by = "days")
}

# now convert these sequences to dataframes

bindList <- list()

for(i in esm.IDnum.w1){
  bindList[[i]] <- ldply(dayList[i], data.frame)
}

allDaysLong <- ldply(bindList, data.frame)

names(allDaysLong)[1] <- 'esm.IDnum.w1'
names(allDaysLong) <- c('esm.IDnum.w1','esm.fileDate.w1')

# reformat dates

allDaysLong$esm.fileDate.w1 <- paste0('20',substr(allDaysLong$esm.fileDate.w1,1,2),
                                      substr(allDaysLong$esm.fileDate.w1,4,5),
                                      substr(allDaysLong$esm.fileDate.w1,7,8))

# we now have a list of all days from the first report to the last report
# for each participant. limitation: need to look at the sign-in sheet if you 
# also want to insert rows for missed reports after the last completed report
# i.e., drop-off

# which combinations of ID.fileDate aren't in the main ESM dataset?

allDaysLong$ID.fileDate <- paste0(allDaysLong$esm.IDnum.w1,'.',allDaysLong$esm.fileDate.w1)

existingDays <- unique(esm$ID.fileDate)  # days where at least 1 survey was submitted
allDays <- unique(allDaysLong$ID.fileDate)  # days in between the first & last day (inclusive)

# which elements of existingDays are not in allDays?

missingDays <- setdiff(allDays,existingDays)

# for each of these missing days, create a dataframe with 4 rows for each missing hour block

missingHours <- data.frame(missingDays)
missingHours <- rbind(missingHours,missingHours,missingHours,missingHours)  # repeat 4 times (once for each hour block)

names(missingHours)[1] <- 'ID.fileDate'

missingHours <- setorder(missingHours,ID.fileDate)

# add hour block variable

missingHours$esm.PRO03.w1 <- sequence(rle(as.character(missingHours$ID.fileDate))$lengths)

# separate out ID & date variables

missingHours$esm.IDnum.w1 <- gsub( "\\..*$", "", missingHours$ID.fileDate )
missingHours$esm.fileDate.w1 <- sub('.*\\.', '', missingHours$ID.fileDate)

# add completeness variable

missingHours$complete <- 0

# merge with main dataset

missingHours <- missingHours[,c('esm.IDnum.w1','esm.fileDate.w1','esm.PRO03.w1','complete')]
esm <- esm[,c('esm.IDnum.w1','esm.fileDate.w1','esm.PRO03.w1','complete')]

esm <- rbind(esm,missingHours)

# reorder

esm <- setorder(esm,esm.IDnum.w1,esm.fileDate.w1,esm.PRO03.w1)

# voila! complete = 1 means a report was submitted, complete = 0 means a report was missed

View(esm)

# email me if anything was unclear/you have ideas for improving the code!
# jesun@ucdavis.edu
