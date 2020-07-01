library(zoo)
library(dplyr)
library(data.table)
library(ggplot2)
library(plyr)
library(lubridate)
library(xts)
library(anytime)
library(reshape2)
library(scales)
library(chron)
library(forecast)

metadata <- read.csv("metadata.csv")
dwarf <- read.csv("7_dwarfs_train.csv")
alien <- read.csv("alien_saucers.csv")
dinosaur <- read.csv("dinosaur.csv")
everest <- read.csv("expedition_everest.csv")
flight <- read.csv("flight_of_passage.csv")
kilimanjaro <- read.csv("kilimanjaro_safaris.csv")
navi <- read.csv("navi_river.csv")
pirates <- read.csv("pirates_of_caribbean.csv")
rocknroll <- read.csv("rock_n_rollercoaster.csv")
slinky <- read.csv("slinky_dog.csv")
soarin <- read.csv("soarin.csv")
spaceship <- read.csv("spaceship_earth.csv")
splash <- read.csv("splash_mountain.csv")
toystory <- read.csv("toy_story_mania.csv")

# convert time to POSIXct
dwarf$datetime <- as.POSIXct(dwarf$datetime)
alien$datetime <- as.POSIXct (alien$datetime)
dinosaur$datetime <- as.POSIXct(dinosaur$datetime)
everest$datetime <- as.POSIXct(everest$datetime)
flight$datetime <- as.POSIXct(flight$datetime)
kilimanjaro$datetime <- as.POSIXct(kilimanjaro$datetime)
navi$datetime <- as.POSIXct(navi$datetime)
pirates$datetime <- as.POSIXct(pirates$datetime)
rocknroll$datetime <- as.POSIXct(rocknroll$datetime)
slinky$datetime <- as.POSIXct(slinky$datetime)
soarin$datetime <- as.POSIXct(soarin$datetime)
spaceship$datetime <- as.POSIXct(spaceship$datetime)
splash$datetime <- as.POSIXct(splash$datetime)
toystory$datetime <- as.POSIXct(toystory$datetime)

# create new data frame to manipulate
dwarf1 <- dwarf
alien1 <- alien
dinosaur1 <- dinosaur
everest1 <- everest
flight1 <- flight
kilimanjaro1 <- kilimanjaro
navi1 <- navi
pirates1 <- pirates
rocknroll1 <- rocknroll
slinky1 <- slinky
soarin1 <- soarin
spaceship1 <- spaceship
splash1 <- splash
toystory1 <- toystory

# round time to the nearest hour
dwarf1$datetime = round(as.POSIXct(dwarf1$datetime, format="%H:%M:%S"), units="hours")
alien1$datetime = round(as.POSIXct(alien1$datetime, format="%H:%M:%S"), units="hours")
dinosaur1$datetime = round(as.POSIXct(dinosaur1$datetime, format="%H:%M:%S"), units="hours")
everest1$datetime = round(as.POSIXct(everest1$datetime, format="%H:%M:%S"), units="hours")
flight1$datetime = round(as.POSIXct(flight1$datetime, format="%H:%M:%S"), units="hours")
kilimanjaro1$datetime = round(as.POSIXct(kilimanjaro1$datetime, format="%H:%M:%S"), units="hours")
navi1$datetime = round(as.POSIXct(navi1$datetime, format="%H:%M:%S"), units="hours")
pirates1$datetime = round(as.POSIXct(pirates1$datetime, format="%H:%M:%S"), units="hours")
rocknroll1$datetime = round(as.POSIXct(rocknroll1$datetime, format="%H:%M:%S"), units="hours")
slinky1$datetime = round(as.POSIXct(slinky1$datetime, format="%H:%M:%S"), units="hours")
soarin1$datetime = round(as.POSIXct(soarin1$datetime, format="%H:%M:%S"), units="hours")
spaceship1$datetime = round(as.POSIXct(spaceship1$datetime, format="%H:%M:%S"), units="hours")
splash1$datetime = round(as.POSIXct(splash1$datetime, format="%H:%M:%S"), units="hours")
toystory1$datetime = round(as.POSIXct(toystory1$datetime, format="%H:%M:%S"), units="hours")

# remove -999 values (attraction closed)
dwarf1 <- subset.data.frame(dwarf1,dwarf1$SPOSTMIN!=-999)
alien1 <- subset.data.frame(alien1,alien1$SPOSTMIN!=-999)
dinosaur1 <- subset.data.frame(dinosaur1,dinosaur1$SPOSTMIN!=-999)
everest1 <- subset.data.frame(everest1,everest1$SPOSTMIN!=-999)
flight1 <- subset.data.frame(flight1,flight1$SPOSTMIN!=-999)
kilimanjaro1 <- subset.data.frame(kilimanjaro1,kilimanjaro1$SPOSTMIN!=-999)
navi1 <- subset.data.frame(navi1,navi1$SPOSTMIN!=-999)
pirates1 <- subset.data.frame(pirates1,pirates1$SPOSTMIN!=-999)
rocknroll1 <- subset.data.frame(rocknroll1,rocknroll1$SPOSTMIN!=-999)
slinky1 <- subset.data.frame(slinky1,slinky1$SPOSTMIN!=-999)
soarin1 <- subset.data.frame(soarin1,soarin1$SPOSTMIN!=-999)
spaceship1 <- subset.data.frame(spaceship1,spaceship1$SPOSTMIN!=-999)
splash1 <- subset.data.frame(splash1,splash1$SPOSTMIN!=-999)
toystory1 <- subset.data.frame(toystory1,toystory1$SPOSTMIN!=-999)

# create column with date and hour
date1a = function (timeindex){
  return(strftime(timeindex, format = "%Y-%m-%d %H:%M"))
}
dwarf1$datetime = sapply(dwarf1$datetime, date1a)
dwarf1$datetime <- as.POSIXct(dwarf1$datetime)
alien1$datetime = sapply(alien1$datetime, date1a)
alien1$datetime <- as.POSIXct(alien1$datetime)
dinosaur1$datetime = sapply(dinosaur1$datetime, date1a)
dinosaur1$datetime <- as.POSIXct(dinosaur1$datetime)
everest1$datetime = sapply(everest1$datetime, date1a)
everest1$datetime <- as.POSIXct(everest1$datetime)
flight1$datetime = sapply(flight1$datetime, date1a)
flight1$datetime <- as.POSIXct(flight1$datetime)
kilimanjaro1$datetime = sapply(kilimanjaro1$datetime, date1a)
kilimanjaro1$datetime <- as.POSIXct(kilimanjaro1$datetime)
navi1$datetime = sapply(navi1$datetime, date1a)
navi1$datetime <- as.POSIXct(navi1$datetime)
pirates1$datetime = sapply(pirates1$datetime, date1a)
pirates1$datetime <- as.POSIXct(pirates1$datetime)
rocknroll1$datetime = sapply(rocknroll1$datetime, date1a)
rocknroll1$datetime <- as.POSIXct(rocknroll1$datetime)
slinky1$datetime = sapply(slinky1$datetime, date1a)
slinky1$datetime <- as.POSIXct(slinky1$datetime)
soarin1$datetime = sapply(soarin1$datetime, date1a)
soarin1$datetime <- as.POSIXct(soarin1$datetime)
spaceship1$datetime = sapply(spaceship1$datetime, date1a)
spaceship1$datetime <- as.POSIXct(spaceship1$datetime)
splash1$datetime = sapply(splash1$datetime, date1a)
splash1$datetime <- as.POSIXct(splash1$datetime)
toystory1$datetime = sapply(toystory1$datetime, date1a)
toystory1$datetime <- as.POSIXct(toystory1$datetime)

# calculate hourly SPOSTMIN mean
dwarfhourly = aggregate(dwarf1$SPOSTMIN, by=list(hour=dwarf1$datetime), FUN=mean)
alienhourly = aggregate(alien1$SPOSTMIN, by=list(hour=alien1$datetime), FUN=mean)
dinosaurhourly = aggregate(dinosaur1$SPOSTMIN, by=list(hour=dinosaur1$datetime), FUN=mean)
everesthourly = aggregate(everest1$SPOSTMIN, by=list(hour=everest1$datetime), FUN=mean)
flighthourly = aggregate(flight1$SPOSTMIN, by=list(hour=flight1$datetime), FUN=mean)
kilimanjarohourly = aggregate(kilimanjaro1$SPOSTMIN, by=list(hour=kilimanjaro1$datetime), FUN=mean)
navihourly = aggregate(navi1$SPOSTMIN, by=list(hour=navi1$datetime), FUN=mean)
pirateshourly = aggregate(pirates1$SPOSTMIN, by=list(hour=pirates1$datetime), FUN=mean)
rocknrollhourly = aggregate(rocknroll1$SPOSTMIN, by=list(hour=rocknroll1$datetime), FUN=mean)
slinkyhourly = aggregate(slinky1$SPOSTMIN, by=list(hour=slinky1$datetime), FUN=mean)
soarinhourly = aggregate(soarin1$SPOSTMIN, by=list(hour=soarin1$datetime), FUN=mean)
spaceshiphourly = aggregate(spaceship1$SPOSTMIN, by=list(hour=spaceship1$datetime), FUN=mean)
splashhourly = aggregate(splash1$SPOSTMIN, by=list(hour=splash1$datetime), FUN=mean)
toystoryhourly = aggregate(toystory1$SPOSTMIN, by=list(hour=toystory1$datetime), FUN=mean)

# rename columns
colnames(alienhourly) = c('datetime', 'Alien')
colnames(dinosaurhourly) = c('datetime', 'Dinosaur')
colnames(dwarfhourly) = c('datetime', 'Dwarf')
colnames(everesthourly) = c('datetime', 'Everest')
colnames(flighthourly) = c('datetime', 'Flight')
colnames(kilimanjarohourly) = c('datetime', 'Kilimanjaro')
colnames(navihourly) = c('datetime', 'Navi')
colnames(pirateshourly) = c('datetime', 'Pirates')
colnames(rocknrollhourly) = c('datetime', 'RockNRoller')
colnames(slinkyhourly) = c('datetime', 'Slinky')
colnames(soarinhourly) = c('datetime', 'Soarin')
colnames(spaceshiphourly) = c('datetime', 'Spaceship')
colnames(splashhourly) = c('datetime', 'Splash')
colnames(toystoryhourly) = c('datetime', 'ToyStory')

# merge hourly data sets
attractionhourly <- merge(alienhourly, dinosaurhourly, by = "datetime", all = TRUE)
attractionhourly <- merge(attractionhourly, dwarfhourly, by = "datetime", all = TRUE)
attractionhourly <- merge(attractionhourly, everesthourly, by = "datetime", all = TRUE)
attractionhourly <- merge(attractionhourly, flighthourly, by = "datetime", all = TRUE)
attractionhourly <- merge(attractionhourly, kilimanjarohourly, by = "datetime", all = TRUE)
attractionhourly <- merge(attractionhourly, navihourly, by = "datetime", all = TRUE)
attractionhourly <- merge(attractionhourly, pirateshourly, by = "datetime", all = TRUE)
attractionhourly <- merge(attractionhourly, rocknrollhourly, by = "datetime", all = TRUE)
attractionhourly <- merge(attractionhourly, slinkyhourly, by = "datetime", all = TRUE)
attractionhourly <- merge(attractionhourly, soarinhourly, by = "datetime", all = TRUE)
attractionhourly <- merge(attractionhourly, spaceshiphourly, by = "datetime", all = TRUE)
attractionhourly <- merge(attractionhourly, splashhourly, by = "datetime", all = TRUE)
attractionhourly <- merge(attractionhourly, toystoryhourly, by = "datetime", all = TRUE)

# data frame for daily data
aliendaily <- alienhourly
dinosaurdaily <- dinosaurhourly
dwarfdaily <- dwarfhourly
everestdaily <- everesthourly
flightdaily <- flighthourly
kilimanjarodaily <- kilimanjarohourly
navidaily <- navihourly
piratesdaily <- pirateshourly
rocknrolldaily <-rocknrollhourly
slinkydaily <- slinkyhourly
soarindaily <- soarinhourly
spaceshipdaily <- spaceshiphourly
splashdaily <- splashhourly
toystorydaily <- toystoryhourly

# calculate daily average waiting time
date1b = function (timeindex){
  return(strftime(timeindex,format = "%Y-%m-%d"))
}
aliendaily$date1b = sapply(aliendaily$datetime, date1b)
aliendaily = ddply(aliendaily, .(date1b), summarize, mean=mean(Alien))
dinosaurdaily$date1b = sapply(dinosaurdaily$datetime, date1b)
dinosaurdaily = ddply(dinosaurdaily, .(date1b), summarize, mean=mean(Dinosaur))
dwarfdaily$date1b = sapply(dwarfdaily$datetime, date1b)
dwarfdaily = ddply(dwarfdaily, .(date1b), summarize, mean=mean(Dwarf))
everestdaily$date1b = sapply(everestdaily$datetime, date1b)
everestdaily = ddply(everestdaily, .(date1b), summarize, mean=mean(Everest))
flightdaily$date1b = sapply(flightdaily$datetime, date1b)
flightdaily = ddply(flightdaily, .(date1b), summarize, mean=mean(Flight))
kilimanjarodaily$date1b = sapply(kilimanjarodaily$datetime, date1b)
kilimanjarodaily = ddply(kilimanjarodaily, .(date1b), summarize, mean=mean(Kilimanjaro))
navidaily$date1b = sapply(navidaily$datetime, date1b)
navidaily = ddply(navidaily, .(date1b), summarize, mean=mean(Navi))
piratesdaily$date1b = sapply(piratesdaily$datetime, date1b)
piratesdaily = ddply(piratesdaily, .(date1b), summarize, mean=mean(Pirates))
rocknrolldaily$date1b = sapply(rocknrolldaily$datetime, date1b)
rocknrolldaily = ddply(rocknrolldaily, .(date1b), summarize, mean=mean(RockNRoller))
slinkydaily$date1b = sapply(slinkydaily$datetime, date1b)
slinkydaily = ddply(slinkydaily, .(date1b), summarize, mean=mean(Slinky))
soarindaily$date1b = sapply(soarindaily$datetime, date1b)
soarindaily = ddply(soarindaily, .(date1b), summarize, mean=mean(Soarin))
spaceshipdaily$date1b = sapply(spaceshipdaily$datetime, date1b)
spaceshipdaily = ddply(spaceshipdaily, .(date1b), summarize, mean=mean(Spaceship))
splashdaily$date1b = sapply(splashdaily$datetime, date1b)
splashdaily = ddply(splashdaily, .(date1b), summarize, mean=mean(Splash))
toystorydaily$date1b = sapply(toystorydaily$datetime, date1b)
toystorydaily = ddply(toystorydaily, .(date1b), summarize, mean=mean(ToyStory))

# rename columns
colnames(aliendaily) = c('date', 'Alien')
colnames(dinosaurdaily) = c('date', 'Dinosaur')
colnames(dwarfdaily) = c('date', 'Dwarf')
colnames(everestdaily) = c('date', 'Everest')
colnames(flightdaily) = c('date', 'Flight')
colnames(kilimanjarodaily) = c('date', 'Kilimanjaro')
colnames(navidaily) = c('date', 'Navi')
colnames(piratesdaily) = c('date', 'Pirates')
colnames(rocknrolldaily) = c('date', 'RockNRoller')
colnames(slinkydaily) = c('date', 'Slinky')
colnames(soarindaily) = c('date', 'Soarin')
colnames(spaceshipdaily) = c('date', 'Spaceship')
colnames(splashdaily) = c('date', 'Splash')
colnames(toystorydaily) = c('date', 'ToyStory')

# convert date to POSIXct
aliendaily$date = as.POSIXct(anytime(aliendaily$date))
dinosaurdaily$date = as.POSIXct(anytime(dinosaurdaily$date))
dwarfdaily$date = as.POSIXct(anytime(dwarfdaily$date))
everestdaily$date = as.POSIXct(anytime(everestdaily$date))
flightdaily$date = as.POSIXct(anytime(flightdaily$date))
kilimanjarodaily$date = as.POSIXct(anytime(kilimanjarodaily$date))
navidaily$date = as.POSIXct(anytime(navidaily$date))
piratesdaily$date = as.POSIXct(anytime(piratesdaily$date))
rocknrolldaily$date = as.POSIXct(anytime(rocknrolldaily$date))
slinkydaily$date = as.POSIXct(anytime(slinkydaily$date))
soarindaily$date = as.POSIXct(anytime(soarindaily$date))
spaceshipdaily$date = as.POSIXct(anytime(spaceshipdaily$date))
splashdaily$date = as.POSIXct(anytime(splashdaily$date))
toystorydaily$date = as.POSIXct(anytime(toystorydaily$date))

# merge daily data sets
attractiondaily <- merge(aliendaily, dinosaurdaily, by = "date", all = TRUE)
attractiondaily <- merge(attractiondaily, dwarfdaily, by = "date", all = TRUE)
attractiondaily <- merge(attractiondaily, everestdaily, by = "date", all = TRUE)
attractiondaily <- merge(attractiondaily, flightdaily, by = "date", all = TRUE)
attractiondaily <- merge(attractiondaily, kilimanjarodaily, by = "date", all = TRUE)
attractiondaily <- merge(attractiondaily, navidaily, by = "date", all = TRUE)
attractiondaily <- merge(attractiondaily, piratesdaily, by = "date", all = TRUE)
attractiondaily <- merge(attractiondaily, rocknrolldaily, by = "date", all = TRUE)
attractiondaily <- merge(attractiondaily, slinkydaily, by = "date", all = TRUE)
attractiondaily <- merge(attractiondaily, soarindaily, by = "date", all = TRUE)
attractiondaily <- merge(attractiondaily, spaceshipdaily, by = "date", all = TRUE)
attractiondaily <- merge(attractiondaily, splashdaily, by = "date", all = TRUE)
attractiondaily <- merge(attractiondaily, toystorydaily, by = "date", all = TRUE)

# data frame for monthly data
alienmonthly <- alienhourly
dinosaurmonthly <- dinosaurhourly
dwarfmonthly <- dwarfhourly
everestmonthly <- everesthourly
flightmonthly <- flighthourly
kilimanjaromonthly <- kilimanjarohourly
navimonthly <- navihourly
piratesmonthly <- pirateshourly
rocknrollmonthly <- rocknrollhourly
slinkymonthly <- slinkyhourly
soarinmonthly <- soarinhourly
spaceshipmonthly <- spaceshiphourly
splashmonthly <- splashhourly
toystorymonthly <- toystoryhourly

# calculate monthly avg wait time
date1c = function (timeindex){
  return(strftime(timeindex,format = "%Y-%m"))
}
alienmonthly$datetime = sapply(alienmonthly$datetime, date1c)
alienmonthly = ddply(alienmonthly, .(datetime), summarize, mean=mean(Alien))
dinosaurmonthly$datetime = sapply(dinosaurmonthly$datetime, date1c)
dinosaurmonthly = ddply(dinosaurmonthly, .(datetime), summarize, mean=mean(Dinosaur))
dwarfmonthly$datetime = sapply(dwarfmonthly$datetime, date1c)
dwarfmonthly = ddply(dwarfmonthly, .(datetime), summarize, mean=mean(Dwarf))
everestmonthly$datetime = sapply(everestmonthly$datetime, date1c)
everestmonthly = ddply(everestmonthly, .(datetime), summarize, mean=mean(Everest))
flightmonthly$datetime = sapply(flightmonthly$datetime, date1c)
flightmonthly = ddply(flightmonthly, .(datetime), summarize, mean=mean(Flight))
kilimanjaromonthly$datetime = sapply(kilimanjaromonthly$datetime, date1c)
kilimanjaromonthly = ddply(kilimanjaromonthly, .(datetime), summarize, mean=mean(Kilimanjaro))
navimonthly$datetime = sapply(navimonthly$datetime, date1c)
navimonthly = ddply(navimonthly, .(datetime), summarize, mean=mean(Navi))
piratesmonthly$datetime = sapply(piratesmonthly$datetime, date1c)
piratesmonthly = ddply(piratesmonthly, .(datetime), summarize, mean=mean(Pirates))
rocknrollmonthly$datetime = sapply(rocknrollmonthly$datetime, date1c)
rocknrollmonthly = ddply(rocknrollmonthly, .(datetime), summarize, mean=mean(RockNRoller))
slinkymonthly$datetime = sapply(slinkymonthly$datetime, date1c)
slinkymonthly = ddply(slinkymonthly, .(datetime), summarize, mean=mean(Slinky))
soarinmonthly$datetime = sapply(soarinmonthly$datetime, date1c)
soarinmonthly = ddply(soarinmonthly, .(datetime), summarize, mean=mean(Soarin))
spaceshipmonthly$datetime = sapply(spaceshipmonthly$datetime, date1c)
spaceshipmonthly = ddply(spaceshipmonthly, .(datetime), summarize, mean=mean(Spaceship))
splashmonthly$datetime = sapply(splashmonthly$datetime, date1c)
splashmonthly = ddply(splashmonthly, .(datetime), summarize, mean=mean(Splash))
toystorymonthly$datetime = sapply(toystorymonthly$datetime, date1c)
toystorymonthly = ddply(toystorymonthly, .(datetime), summarize, mean=mean(ToyStory))

# rename columns
colnames(alienmonthly) = c('date', 'Alien')
colnames(dinosaurmonthly) = c('date', 'Dinosaur')
colnames(dwarfmonthly) = c('date', 'Dwarf')
colnames(everestmonthly) = c('date', 'Everest')
colnames(flightmonthly) = c('date', 'Flight')
colnames(kilimanjaromonthly) = c('date', 'Kilimanjaro')
colnames(navimonthly) = c('date', 'Navi')
colnames(piratesmonthly) = c('date', 'Pirates')
colnames(rocknrollmonthly) = c('date', 'RockNRoller')
colnames(slinkymonthly) = c('date', 'Slinky')
colnames(soarinmonthly) = c('date', 'Soarin')
colnames(spaceshipmonthly) = c('date', 'Spaceship')
colnames(splashmonthly) = c('date', 'Splash')
colnames(toystorymonthly) = c('date', 'ToyStory')

# convert date to POSIXct
alienmonthly$date = as.POSIXct(anytime(alienmonthly$date))
dinosaurmonthly$date = as.POSIXct(anytime(dinosaurmonthly$date))
dwarfmonthly$date = as.POSIXct(anytime(dwarfmonthly$date))
everestmonthly$date = as.POSIXct(anytime(everestmonthly$date))
flightmonthly$date = as.POSIXct(anytime(flightmonthly$date))
kilimanjaromonthly$date = as.POSIXct(anytime(kilimanjaromonthly$date))
navimonthly$date = as.POSIXct(anytime(navimonthly$date))
piratesmonthly$date = as.POSIXct(anytime(piratesmonthly$date))
rocknrollmonthly$date = as.POSIXct(anytime(rocknrollmonthly$date))
slinkymonthly$date = as.POSIXct(anytime(slinkymonthly$date))
soarinmonthly$date = as.POSIXct(anytime(soarinmonthly$date))
spaceshipmonthly$date = as.POSIXct(anytime(spaceshipmonthly$date))
splashmonthly$date = as.POSIXct(anytime(splashmonthly$date))
toystorymonthly$date = as.POSIXct(anytime(toystorymonthly$date))

# merge monthly data sets
attractionmonthly <- merge(alienmonthly, dinosaurmonthly, by = "date", all = TRUE)
attractionmonthly <- merge(attractionmonthly, dwarfmonthly, by = "date", all = TRUE)
attractionmonthly <- merge(attractionmonthly, everestmonthly, by = "date", all = TRUE)
attractionmonthly <- merge(attractionmonthly, flightmonthly, by = "date", all = TRUE)
attractionmonthly <- merge(attractionmonthly, kilimanjaromonthly, by = "date", all = TRUE)
attractionmonthly <- merge(attractionmonthly, navimonthly, by = "date", all = TRUE)
attractionmonthly <- merge(attractionmonthly, piratesmonthly, by = "date", all = TRUE)
attractionmonthly <- merge(attractionmonthly, rocknrollmonthly, by = "date", all = TRUE)
attractionmonthly <- merge(attractionmonthly, slinkymonthly, by = "date", all = TRUE)
attractionmonthly <- merge(attractionmonthly, soarinmonthly, by = "date", all = TRUE)
attractionmonthly <- merge(attractionmonthly, spaceshipmonthly, by = "date", all = TRUE)
attractionmonthly <- merge(attractionmonthly, splashmonthly, by = "date", all = TRUE)
attractionmonthly <- merge(attractionmonthly, toystorymonthly, by = "date", all = TRUE)
attractionmonthly <- attractionmonthly[-c(83),]

# hourly analysis
attractionhourly1 <- attractionhourly

date1d = function (timeindex){
  return(strftime(timeindex, format = "%H:%M"))
}
attractionhourly1$time = sapply(attractionhourly1$datetime, date1d)
attractionhourly1$meanwaittime <- rowMeans(attractionhourly1[,2:15], na.rm=TRUE)
attractionhourly1 <- attractionhourly1[, -c(1,2:15)]
attractionhourly1<-as.data.table(attractionhourly1)
attractionhourly1<-attractionhourly1[,list(meanwaittime=mean(meanwaittime)),by=time]

melthour <- melt(attractionhourly1,id="time")
colnames(melthour) = c('time', 'variable', 'waittime')
ggplot(melthour,aes(x=time,y=waittime,colour=variable,group=variable)) + geom_line()

# daily analysis
attractiondaily1 <- attractiondaily
attractiondaily1$meanwaittime <- rowMeans(attractiondaily1[,2:15], na.rm=TRUE)
attractiondaily1 <- attractiondaily1[-c(2494), -c(2:15)]

meltday <- melt(attractiondaily1,id="date")
colnames(meltday) = c('date', 'variable', 'waittime')
ggplot(meltday,aes(x=date,y=waittime,colour=variable,group=variable)) + geom_line()

# monthly analysis
attractionmonthly1 <- attractionmonthly
attractionmonthly1$meanwaittime <- rowMeans(attractionmonthly1[,2:15], na.rm=TRUE)
attractionmonthly1 <- attractionmonthly1[-c(2:15)]

meltmonth <- melt(attractionmonthly1,id="date")
colnames(meltmonth) = c('month', 'variable', 'waittime')
ggplot(meltmonth,aes(x=month,y=waittime,colour=variable,group=variable)) + geom_line()

# plot all attraction data
meltmonthly <- melt(attractionmonthly,id="date")
ggplot(meltmonthly,aes(x=date,y=value,colour=variable,group=variable)) + geom_line()+
scale_color_manual(values=c('red', 'plum', 'gold', 'deep pink', 'blue', 'dodgerblue', 'hotpink',
                            'black', 'grey', 'darkorchid', 'darkorange', 'cyan', 'darkgreen', 'green'))

meltdaily <- melt(attractiondaily,id="date")
ggplot(meltdaily,aes(x=date,y=value,colour=variable,group=variable)) + geom_line()

melthourly <- melt(attractionhourly,id="datetime")
ggplot(melthourly,aes(x=datetime,y=value,colour=variable,group=variable)) +
   geom_line()

# dataset for analysis
metadata$DATE = as.POSIXct(anytime(metadata$DATE))
colnames(metadata)[1]<-"date"
dailydata <- merge(attractiondaily, metadata, by = "date", all = TRUE)
dailydata1 <- merge(attractiondaily1, metadata, by = "date", all = TRUE)

# week of the year analysis
attractionweekly <- dailydata[,c(1:15,19)]
attractionweekly$meanwaittime <- rowMeans(attractionweekly[,2:15], na.rm=TRUE)
attractionweekly <- attractionweekly[-c(2494), -c(1:15)]
attractionweekly<-as.data.table(attractionweekly)
attractionweekly<-attractionweekly[,list(meanwaittime=mean(meanwaittime)),by=WEEKOFYEAR]

meltweekly <- melt(attractionweekly,id="WEEKOFYEAR")
colnames(meltweekly) = c('week', 'variable', 'waittime')
ggplot(meltweekly,aes(x=week,y=waittime,colour=variable,group=variable)) + geom_line()

# day of the week analysis
attractionday <- dailydata[,c(1:15,17)]
attractionday$meanwaittime <- rowMeans(attractionday[,2:15], na.rm=TRUE)
attractionday <- attractionday[-c(2494),-c(1:15)]
attractionday<-as.data.table(attractionday)
attractionday<-attractionday[,list(meanwaittime=mean(meanwaittime)),by=DAYOFWEEK]

meltday <- melt(attractionday,id="DAYOFWEEK")
colnames(meltday) = c('day', 'variable', 'waittime')
ggplot(meltday,aes(x=day,y=waittime,colour=variable,group=variable)) + geom_line()

# linear regression

# percentage of schools in session in the US
fit01 = lm(meanwaittime~inSession_Enrollment, data=dailydata1)
summary(fit01)

# seasons/holidays
fit02 = lm(meanwaittime~SEASON, data=dailydata1)
summary(fit02)

# temperature
fit03 = lm(meanwaittime~WDWMAXTEMP+WDWMINTEMP+WDWMEANTEMP, data=dailydata1)
summary(fit03)

# STL decomposition forecast
myts <- ts(attractionmonthly1, start=c(2012, 1), end=c(2018, 10), frequency=12) 
myts <- myts[,-(1)]
autoplot(myts)

myts %>% mstl() %>%
  autoplot() + xlab("month")

myts %>%  stlf() %>%
  autoplot() + xlab("month")

# dynamic harmonic regression
plots<-list()
for (i in seq(6)) {
  fit <- auto.arima(myts, xreg = fourier(myts, K = i),
                    seasonal = FALSE, lambda = 0)
  plots[[i]] <- autoplot(forecast(fit,
                                  xreg=fourier(myts, K=i, h=24))) +
    xlab(paste("K=",i,"   AICC=",round(fit[["aicc"]],2))) +
    ylab("")
}
gridExtra::grid.arrange(
  plots[[1]],plots[[2]],plots[[3]],
  plots[[4]],plots[[5]],plots[[6]], nrow=3)
