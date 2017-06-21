library(plyr)
library(data.table)
library(chron)

setwd('/Users/Jessie/Dropbox/Collaborations/Missing Data in ESM/Inserting Missing Rows')

#### ESM setup ####

esm <- read.csv('esm_w1_RENAMED.csv',stringsAsFactors=FALSE)

str(unique(esm$esm.IDnum.w1))

esm <- esm[,c('esm.IDnum.w1','esm.PRO03.w1','esm.startDateTime.w1',
              'esm.PRO02.w1')]

# esm.NQ07.w1 = happy, esm.NQ11.w1 = posemo, esm.NQ12.w1 = negemo

names(esm)[3] <- 'esm.fileDate.w1'

# Reformat dates

esm$mm <- gsub( "/.*$", "", esm$esm.fileDate.w1)

for(i in as.character(1:9)){
  esm[which(esm$mm==i),'mm'] <- paste0('0',i)
}

esm$dd <- gsub("(^.+/)(\\d+)(/.+$)", "\\2", esm$esm.fileDate.w1)
#You would "read" that regex pattern as splitting any matched string into three parts:
#1) anything up to and including the first forward slash, 2) any digits(= "\d") in a sequence before the next slash and
#3) from the next slash to the end. 
#And then only returning the second part....

for(i in as.character(1:9)){
  esm[which(esm$dd==i),'dd'] <- paste0('0',i)
}

# This function extracts the last n characters from a string (needs to be a character vector)
# from http://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

x <- as.character(esm$esm.fileDate.w1)
esm$yy <- substrRight(x,2)

esm[which(esm$yy=='12'),'yy'] <- '2012'
esm[which(esm$yy=='13'),'yy'] <- '2013'

esm$esm.fileDate.w1 <- as.numeric(paste0(esm$yy,esm$mm,esm$dd))

# Concatenate ID, Date, & File ID for matching with within-person dataset

esm$ID.fileDate.hour <- paste0(esm$esm.IDnum.w1,".",esm$esm.fileDate.w1,".",
                                esm$esm.PRO03.w1)  # create a variable concatenating participant ID, file date, & file number

# Concatenate ID & Date for matching with between-person dataset

esm$ID.fileDate <- paste0(esm$esm.IDnum.w1,".",esm$esm.fileDate.w1)  

names(esm)

#### Fill in missing rows ####

esmCopy <- esm

esm <- esm[,c('esm.IDnum.w1','esm.PRO02.w1','esm.PRO03.w1','esm.fileDate.w1','ID.fileDate')]

ID.fileDate <- unique(esm$ID.fileDate)

missingStats <- data.frame(ID.fileDate)

# this for loop takes about 10 seconds to run...suggestions for avoiding for loops welcome!

for(i in ID.fileDate){
  for(j in c(1,2,3,4)){
    
    missingStats[which(missingStats$ID.fileDate==i),paste0('complete',j)] <- nrow(esm[which(esm$ID.fileDate==i &
                                                                 esm$esm.PRO03.w1==j),])
  
  }
}

# convert missingStats to long format

missingStatsLong <- reshape(missingStats, varying=c(2:5), direction="long", 
                            idvar="ID.fileDate", sep="", timevar="esm.PRO03.w1")

missingStatsLong <- setorder(missingStatsLong,ID.fileDate,esm.PRO03.w1)

# remove rows where ESM reports were submitted 

missingStatsLong <- missingStatsLong[which(missingStatsLong$complete==0),]

# separate out ID & date variables

missingStatsLong$esm.IDnum.w1 <- gsub( "\\..*$", "", missingStatsLong$ID.fileDate )
missingStatsLong$esm.fileDate.w1 <- sub('.*\\.', '', missingStatsLong$ID.fileDate)

# add esm.PRO02.w1 variable

missingStatsLong$esm.PRO02.w1 <- missingStatsLong$esm.PRO03.w1

# reorder new rows

missingStatsLong <- missingStatsLong[,c("esm.IDnum.w1", "esm.fileDate.w1", "esm.PRO02.w1", "esm.PRO03.w1", "complete")]

# add completeness variable to main esm dataframe

esm$complete <- 1

# reorder main dataframe

esm <- esm[,c("esm.IDnum.w1", "esm.fileDate.w1", "esm.PRO02.w1", "esm.PRO03.w1", "complete")]

# combine the two dataframes and reorder temporally

esm <- rbind(esm,missingStatsLong)
esm <- setorder(esm,esm.IDnum.w1,esm.fileDate.w1,esm.PRO03.w1)

# grab first days

id.rows <- esm[which(esm$esm.PRO02.w1==6),]

id.rows$ID.fileDate <- paste0(id.rows$esm.IDnum.w1,".",id.rows$esm.fileDate.w1)
esm$ID.fileDate <- paste0(esm$esm.IDnum.w1,".",esm$esm.fileDate.w1)

firstDay <- esm[which(esm$ID.fileDate %in% id.rows$ID.fileDate),]

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

firstDay[which(firstDay$rowDif < 0),'valid'] <- 0

# save vector of invalid rows

firstDay$ID.fileDate.hour <- paste0(firstDay$esm.IDnum.w1,'.',firstDay$esm.fileDate.w1,'.',
                                    firstDay$esm.PRO03.w1)

invalid <- firstDay[which(firstDay$valid==0),'ID.fileDate.hour']

# remove invalid rows from main ESM dataframe

esm$ID.fileDate.hour <- paste0(esm$esm.IDnum.w1,'.',esm$esm.fileDate.w1,'.',
                               esm$esm.PRO03.w1)

esm <- esm[-which(esm$ID.fileDate.hour %in% invalid),]

# Remove test surveys

esm <- esm[which(esm$esm.PRO02.w1!=6),]

# Renumber rows

row.names(esm) <- 1:nrow(esm)

esm <- esm[,c(1,2,4,5)]

# may need to combine with info from sign-in sheet/EAR recordings to assess drop-off

# More importantly: it doesn't deal with cases where participants don't answer any surveys in a given day!!

seq(20121009, 20121022)

#### add code to address missing days ####

# get first and last day (i.e., last day with any reports)

esm.IDnum.w1 <- unique(esm$esm.IDnum.w1)
allDays <- data.frame(esm.IDnum.w1)

for(i in esm.IDnum.w1){
  allDays[which(allDays$esm.IDnum.w1==i),'firstDay'] <- unique(esm[which(esm$esm.IDnum.w1==i),'esm.fileDate.w1'])[1]
  allDays[which(allDays$esm.IDnum.w1==i),'lastDay'] <- tail(esm[which(esm$esm.IDnum.w1==i),'esm.fileDate.w1'],n=1)
}

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

dayList <- list()

for(i in esm.IDnum.w1){
  dayList[[i]] <- seq(allDays[which(allDays$esm.IDnum.w1==i),'firstDate'],allDays[which(allDays$esm.IDnum.w1==i),'lastDate'], by = "days")
}

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

# which combinations of ID.fileDate aren't in the main ESM dataset?

allDaysLong$ID.fileDate <- paste0(allDaysLong$esm.IDnum.w1,'.',allDaysLong$esm.fileDate.w1)

existingDays <- unique(esm$ID.fileDate)
allDays <- unique(allDaysLong$ID.fileDate)

missingDays <- setdiff(allDays,existingDays)

# for each of these missing days, create a dataframe with 4 rows for each missing hour block

missingHours <- data.frame(missingDays)
missingHours <- rbind(missingHours,missingHours,missingHours,missingHours)

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

esm <- setorder(esm,esm.IDnum.w1,esm.fileDate.w1,esm.PRO03.w1)
