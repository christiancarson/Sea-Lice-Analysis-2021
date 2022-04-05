


#Title: Sea Lice Monitoring Prevalance Plots 2021
#Author : Critty (Christian) Carson
#Last updated : October 04, 2021
#Description : Makes mean prevalence from sea Lice Monitoring Program data
#at Cedar Coast Field Station. Plots are made for all available locations
#Based on sea lice monitoring data of Cedar Coast Field Station

# This code has been made so that it only has to be edited slightly to produce 
#plots/tables on different data.Follow the steps by searching #X# for the sections
#you need to edit for the code to run and produce results
#Can search with ctrl F for main headings or their subheadings:

#X#To use this code, make a .csv file of your new datafile,
#and ensure it matches the format of the original data file.
#EX: If your datafile


#X##SET UP##### 
#X#first make sure R and R studio are up to date
#install.packages("installr")
#library(installr)

#update r studio
#From within RStudio, go to Help > Check for Updates to install newer version of
#RStudio (if available, optional).

#x#************** change to your own directory

#--------------make project folders and folder paths----------------------------
#set your wd here, MAKE SURE ITS SET TO YOUR PROJECT DATA BASE IN SESSION DROPDOWN MENU ABOVE

### END OF SET UP ###
wd <- "/Users/user/Documents/GitHub/Sea-Lice-Analysis-2021" 
setwd(wd)
getwd()
folders <- c("Code", "Data", "OutputFigures", "OutputData")

# function to create folders below
for(i in 1:length(folders)){
  if(file.exists(folders[i]) == FALSE)
    dir.create(folders[i])
}

# we also need to store the paths to these new folders

code.output.path <- paste(wd, "/", folders[1], sep = "")
data.input.path <- paste(wd, "/", folders[2], sep = "")
figures.path <- paste(wd, "/", folders[3], sep = "")
data.output.path <- paste(wd, "/", folders[4], sep = "")

# our raw data is stored in different folders, lets make the paths
#changed the path - ML
sealicedata.path <- paste(getwd(), "/", "Data", sep = "")

# now we can access and save stuff to these folders!

#---------------------Below, we upload and clean the  data----------

#x# time to upload the datas into folder
sealicedata <- read.csv(paste(sealicedata.path, "/", "CCFS_SeaLice_Monitoring_2021.csv",
                              sep = ""), stringsAsFactors = FALSE)


#unhashtag to install packages below 
#install.packages(c("boot", "MASS","plyr","dplyr", "ggplot2", "tibble", "car", "reshape2",
#                  "epitools", "readxl", "tidyverse","arsenal")))
library(boot)
library(MASS)
library(plyr)
library(dplyr)
library(ggplot2)
library(tibble)
library(car)
library(reshape2)
library(epitools)
library(readxl)
library(tidyverse)
library(readr)
library(arsenal)

#pacman::p_load(tidyverse,nlme,emmans,here)
#pacman::p_load(ggthemes, ggplot2)

source("https://raw.githubusercontent.com/koundy/ggplot_theme_Publication/master/ggplot_theme_Publication-2.R")


warnings()

#***************************CLEANING
#x#cleaning time
#adjusting datas***********************
sealicedata$year<-as.numeric(sealicedata$year)
#adjusting dates to date format
sealicedata$date <- as.Date(with(sealicedata, 
                                 paste(year, month, day, sep="-")), "%Y-%m-%d")
#height -> as.numeric
sealicedata$height<-as.numeric(sealicedata$height)
#making it all a df
sealicedata<-data.frame(sealicedata)
#changing na's to 0's
sealicedata[ , 12:26][is.na(sealicedata[ , 12:26] ) ] <- 0 
#col 1 to "fish_id"
names(sealicedata)[1]<-paste("fish_id")
#na's to 0 in sum_all_lice
sealicedata$sum_all_lice[is.na(sealicedata$sum_all_lice)]<-0

sealicedata <- sealicedata %>% rowwise() %>%
  dplyr::mutate(sum_all_lice = sum(c_across(Lep_cope:unid_adult)))

salmcounts <- sealicedata %>%
  select( Lep_cope, chalA, chalB, Lep_PAmale, Lep_PAfemale, Lep_male,
          Lep_nongravid, Lep_gravid, Caligus_cope, Caligus_mot, Caligus_gravid, unid_cope,
          chal_unid,unid_PA, unid_adult)

# RM defining categories of lice.

#motile lice sub
motlice<-sealicedata[,c("Caligus_mot", "Caligus_gravid", "Lep_gravid", "Lep_nongravid", "Lep_male", "Lep_PAfemale", "Lep_PAmale", "unid_PA", "unid_adult")]
#cope lice sub
copes<-sealicedata[,c("Lep_cope", "Caligus_cope", "unid_cope")]
#chalimus lice sub
chals<-sealicedata[,c("chalA", "chalB", "chal_unid")]
#attached lice sub
attlice<-sealicedata[,c("Lep_cope","chalA","chalB","Caligus_cope","unid_cope","chal_unid")]


#X# List character variables and go through one by one
list(unique(colnames(sealicedata)))
#make sure year is consistent and case sensitive
list(unique(sealicedata$year))
#make sure names of species are correct
list(unique(sealicedata$species))
#fixing species names
sealicedata$species[sealicedata$species == "chum "]<- "chum"
sealicedata$species[sealicedata$species == "coho "]<- "coho"
sealicedata$species[sealicedata$species == "chinook "]<- "chinook"




#adjusting sample locations
#list locations
list(unique(sealicedata$location))

#adjusting names based on list output
#x#fix errors in names, do this by inputing the incorrect/ current name into the brackets, 
#and then the corrected version into the assigned end <-

#Bedwell Sound North Fix
sealicedata$location[sealicedata$location == "Bedwell Estuary "]<- "Bedwell Sound North"
sealicedata$location[sealicedata$location == "Bedwell estuary"]<- "Bedwell Sound North"
#Currently Bedwell Estuary is mapped as multiple locations, several close to the estuary and
#several near Bedwell Sound Middle: NEED TO FIX
#sealicedata$location[sealicedata$location == "Bedwell Estuary"]<- "Bedwell Sound North"
sealicedata$location[sealicedata$location ==  "Bedwell River"]<-"Bedwell Sound North"
sealicedata$location[sealicedata$location == "Bedwell Estuary 4"]<- "Bedwell Sound North"
sealicedata$location[sealicedata$location == "Bedwell estuary 4"]<- "Bedwell Sound North"

#Bedwell Sound Middle Fix
sealicedata$location[sealicedata$location == "Bedwell 2 "]<- "Bedwell Sound Middle"
sealicedata$location[sealicedata$location == "Bedwell estuary 3"]<- "Bedwell Sound Middle"
sealicedata$location[sealicedata$location == "Bedwell Estuary 3"]<- "Bedwell Sound Middle"
sealicedata$location[sealicedata$location == "Bedwell estuary 2"]<- "Bedwell Sound Middle"
sealicedata$location[sealicedata$location == "Bedwell Estuary 2"]<- "Bedwell Sound Middle"
sealicedata$location[sealicedata$location ==  "Bedwell 2"]<-"Bedwell Sound Middle"
sealicedata$location[sealicedata$location ==  "Sniffles"]<-"Bedwell Sound Middle"
sealicedata$location[sealicedata$location ==  "Sniffles 2"]<-"Bedwell Sound Middle"


#Bedwell Sound South Fix
sealicedata$location[sealicedata$location ==  "Bedwell 3"]<- "Bedwell Sound South"

#Cypre Fix
sealicedata$location[sealicedata$location == "Cypre "]<- "Cypre River"

#North Meares Fix
sealicedata$location[sealicedata$location == "Meares North"]<- "North Meares"

#White Pine Fix
sealicedata$location[sealicedata$location == "White Pine "]<- "White Pine"

#Ritchie Bay Fix
sealicedata$location[sealicedata$location == "Ritchie Bay "]<- "Ritchie Bay"

#Tranquil Fix
sealicedata$location[sealicedata$location == "Tranquil estuary"]<- "Tranquil Estuary"

list(unique(sealicedata$location))


#X#change year to current



#subset to adjust for year

sealice.current<-sealicedata

length(unique(sealice.current$location))

#adjust for our focus species
sealice.current <- sealice.current<-data.frame(subset(sealice.current,
                                                      species == "chum"))

salmcounts <- sealice.current %>%
  select( Lep_cope, chalA, chalB, Lep_PAmale, Lep_PAfemale, Lep_male,
          Lep_nongravid, Lep_gravid, Caligus_cope, Caligus_mot, Caligus_gravid, unid_cope,
          chal_unid,unid_PA, unid_adult)

# RM defining categories of lice.

#motile lice sub
motlice<-sealice.current[,c("Caligus_mot", "Caligus_gravid", "Lep_gravid", "Lep_nongravid", "Lep_male", "Lep_PAfemale", "Lep_PAmale", "unid_PA", "unid_adult")]
#cope lice sub
copes<-sealice.current[,c("Lep_cope", "Caligus_cope", "unid_cope")]
#chalimus lice sub
chals<-sealice.current[,c("chalA", "chalB", "chal_unid")]
#attached lice sub
attlice<-sealice.current[,c("Lep_cope","chalA","chalB","Caligus_cope","unid_cope","chal_unid")]

#total lice sub
sealice.current <- sealice.current %>% rowwise() %>%
  dplyr::mutate(Sum_all_lice = sum(c_across(colnames(salmcounts)))) 

#Below gives columns of summed motiles, attached, copepodids, chalimus, and total counts. Useful for prevalence and abundance plots.
sealice.current$motsum<-rowSums(motlice, na.rm = TRUE)
sealice.current$copsum<-rowSums(copes, na.rm = TRUE)
sealice.current$chalsum<-rowSums(chals, na.rm = TRUE)
sealice.current$attachedsum<-rowSums(attlice, na.rm = TRUE)
sealice.current$sum_all_lice<-rowSums(salmcounts, na.rm = T)

# ensuring the column class is numeric
sealice.current$motsum<- as.numeric(sealice.current$motsum)
sealice.current$copsum<- as.numeric(sealice.current$copsum)
sealice.current$chalsum<-as.numeric(sealice.current$chalsum)
sealice.current$attachedsum<-as.numeric(sealice.current$attachedsum)
sealice.current$sum_all_lice<-as.numeric(sealice.current$sum_all_lice)



### END OF SET UP ###
### ## ABUNDANCE ESTIMATES ## #####

#Setting the cap for bootstrapping
n.boot.b<-1000

##ASSIGNING WEEKLY INTERVALS TO ALL THE dates in the abundance.base data set 
#weekly intervals.
#set up vectors to hold data

#X#beginning abundance plots, start bye creating a list function. 
#x#Start of Abundance Plot #1 for first Location in list
#Then determine how long the list is and replicate the following code that many times, only changing the x in base.list[x,] below
#you should replicate this whole code for every location sampled, unless there are not enough samples, rmeber each site needs a min of 30
base.list <- data.frame(unique(sealice.current$location))
list(base.list)
#starting with the first river in the list, 1

abundance.base<- sealice.current
#######CHUMS#########

# 1
####################################################################################################
## Taking out the dates where less than 30 fish were caught
## Creating a weekly interval column
####################################################################################################
a.b.prod <- data.frame(abundance.base[1,])
a.b.prod <- a.b.prod[-1,]


for(j in 1:nrow(base.list)){

  abundance.base<-data.frame(subset(sealice.current, location == base.list[j,]))

  # Removing any dates with less than 30 fish
  
  
  ## Making julian dates
  abundance.base$date <- as.Date(with(abundance.base, paste(year, month, day, sep="-")), "%Y-%m-%d")
  abundance.base$date  <- julian(abundance.base$date) 
  datelist <-(abundance.base$date)
  ## Making a new column for weeklyinterval dates
  abundance.base$weeklyintvl<-rep(0, each = length(abundance.base$date))
  
  ## Finding the weekly intervals
  value <- abundance.base$weeklyintvl
  # all dates next to an empty column
  dat <- data.frame(datelist, value)
  # vector of all the unique dates
  dates = unique(dat$date)
  # vector for the number of rows for each date
  count <- c()
  
  for(i in 1:length(dates)){
    # length of the subset associated with each date
    n <- length(which(dat$datelist == dates[i]))
    # put the count into the empty vector
    count[i] <- n
  }
  # each unique date has a count of it's rows (assuming each row is a fish)
  output1 <- data.frame(dates, count)
  
  # Here you just turn dates into julian dates for ease later on
  #cbind to as.date version
  abundance.base$date <- as.Date(with(abundance.base, paste(year, month, day, sep="-")), "%Y-%m-%d")
  datelist <-(abundance.base$date)
  abundance.base$weeklyintvl<-rep(0, each = length(abundance.base$date))
  value <- abundance.base$weeklyintvl
  dat <- data.frame(datelist, value)
  dates = unique(dat$date)
  count <- c()
  for(i in 1:length(dates)){
    n <- length(which(dat$datelist == dates[i]))
    count[i] <- n
  }
  output2 <- data.frame(dates, count)
  
  #cbind outputs
  output.date <- cbind(output1,output2)
  output.date = subset(output.date, select = -c(count))
  
  #subset to remove dates with less than 30 fish
  remove.dates <- subset(output.date$dates.1, count < 30)
  #remove.dates <- as.Date(subset(output.date$dates, count <30)
  remove.dates <- data.frame(remove.dates)
  #remove dates from main data
  abundance.base$date  <- as.Date(abundance.base$date) 
  require(lubridate)
  class(remove.dates[,])
  class(abundance.base$date)
  list(remove.dates)
  
  #x# from the list above, input the dates into the filter function below by their column number
  if(nrow(remove.dates)>0){
    for(i in 1:nrow(remove.dates)){
      abundance.base <- abundance.base %>%
        filter(date != remove.dates[i,])#%>%
      #filter(date != as.Date(remove.dates[2,])) 
      #  filter(date != as.Date(remove.dates[,])) # unsure if this second line will cause problems
    }
  }
  
  #try <- abundance.base[(abundance.base$date != remove.dates[,]),]
  remove.dates
  unique(abundance.base$date)
  a.b.prod <- rbind(a.b.prod,abundance.base)
}
abundance.base <- a.b.prod # this is redundant but was kept to work with the rest of the code which calls on abundance.base


chum <- abundance.base

#Chinook#######
sealice.current<-sealicedata

length(unique(sealice.current$location))

#adjust for our focus species
sealice.current <- sealice.current<-data.frame(subset(sealice.current, species == "chinook"))

salmcounts <- sealice.current %>%
  select( Lep_cope, chalA, chalB, Lep_PAmale, Lep_PAfemale, Lep_male,
          Lep_nongravid, Lep_gravid, Caligus_cope, Caligus_mot, Caligus_gravid, unid_cope,
          chal_unid,unid_PA, unid_adult)

# RM defining categories of lice.

#motile lice sub
motlice<-sealice.current[,c("Caligus_mot", "Caligus_gravid", "Lep_gravid", "Lep_nongravid", "Lep_male", "Lep_PAfemale", "Lep_PAmale", "unid_PA", "unid_adult")]
#cope lice sub
copes<-sealice.current[,c("Lep_cope", "Caligus_cope", "unid_cope")]
#chalimus lice sub
chals<-sealice.current[,c("chalA", "chalB", "chal_unid")]
#attached lice sub
attlice<-sealice.current[,c("Lep_cope","chalA","chalB","Caligus_cope","unid_cope","chal_unid")]

#total lice sub
sealice.current <- sealice.current %>% rowwise() %>%
  dplyr::mutate(Sum_all_lice = sum(c_across(colnames(salmcounts)))) 

#Below gives columns of summed motiles, attached, copepodids, chalimus, and total counts. Useful for prevalence and abundance plots.
sealice.current$motsum<-rowSums(motlice, na.rm = TRUE)
sealice.current$copsum<-rowSums(copes, na.rm = TRUE)
sealice.current$chalsum<-rowSums(chals, na.rm = TRUE)
sealice.current$attachedsum<-rowSums(attlice, na.rm = TRUE)
sealice.current$sum_all_lice<-rowSums(salmcounts, na.rm = T)

# ensuring the column class is numeric
sealice.current$motsum<- as.numeric(sealice.current$motsum)
sealice.current$copsum<- as.numeric(sealice.current$copsum)
sealice.current$chalsum<-as.numeric(sealice.current$chalsum)
sealice.current$attachedsum<-as.numeric(sealice.current$attachedsum)
sealice.current$sum_all_lice<-as.numeric(sealice.current$sum_all_lice)



### END OF SET UP ###
### ## ABUNDANCE ESTIMATES ## #####

#Setting the cap for bootstrapping
n.boot.b<-1000

##ASSIGNING WEEKLY INTERVALS TO ALL THE dates in the abundance.base data set 
#weekly intervals.
#set up vectors to hold data

#X#beginning abundance plots, start bye creating a list function. 
#x#Start of Abundance Plot #1 for first Location in list
#Then determine how long the list is and replicate the following code that many times, only changing the x in base.list[x,] below
#you should replicate this whole code for every location sampled, unless there are not enough samples, rmeber each site needs a min of 30
base.list <- data.frame(unique(sealice.current$location))
list(base.list)

  # 1
  ####################################################################################################
  ## Taking out the dates where less than 30 fish were caught
  ## Creating a weekly interval column
  ####################################################################################################
  a.b.prod <- data.frame(abundance.base[1,])
  a.b.prod <- a.b.prod[-1,]
  
  
  for(j in 1:nrow(base.list)){

    abundance.base<-data.frame(subset(sealice.current, location == base.list[j,]))

    # Removing any dates with less than 30 fish
    
    
    ## Making julian dates
    abundance.base$date <- as.Date(with(abundance.base, paste(year, month, day, sep="-")), "%Y-%m-%d")
    abundance.base$date  <- julian(abundance.base$date) 
    datelist <-(abundance.base$date)
    ## Making a new column for weeklyinterval dates
    abundance.base$weeklyintvl<-rep(0, each = length(abundance.base$date))
    
    ## Finding the weekly intervals
    value <- abundance.base$weeklyintvl
    # all dates next to an empty column
    dat <- data.frame(datelist, value)
    # vector of all the unique dates
    dates = unique(dat$date)
    # vector for the number of rows for each date
    count <- c()
    
    for(i in 1:length(dates)){
      # length of the subset associated with each date
      n <- length(which(dat$datelist == dates[i]))
      # put the count into the empty vector
      count[i] <- n
    }
    # each unique date has a count of it's rows (assuming each row is a fish)
    output1 <- data.frame(dates, count)
    
    # Here you just turn dates into julian dates for ease later on
    #cbind to as.date version
    abundance.base$date <- as.Date(with(abundance.base, paste(year, month, day, sep="-")), "%Y-%m-%d")
    datelist <-(abundance.base$date)
    abundance.base$weeklyintvl<-rep(0, each = length(abundance.base$date))
    value <- abundance.base$weeklyintvl
    dat <- data.frame(datelist, value)
    dates = unique(dat$date)
    count <- c()
    for(i in 1:length(dates)){
      n <- length(which(dat$datelist == dates[i]))
      count[i] <- n
    }
    output2 <- data.frame(dates, count)
    
    #cbind outputs
    output.date <- cbind(output1,output2)
    output.date = subset(output.date, select = -c(count))
    
    #subset to remove dates with less than 30 fish
    remove.dates <- subset(output.date$dates.1, count < 30)
    #remove.dates <- as.Date(subset(output.date$dates, count <30)
    remove.dates <- data.frame(remove.dates)
    #remove dates from main data
    abundance.base$date  <- as.Date(abundance.base$date) 
    require(lubridate)
    class(remove.dates[,])
    class(abundance.base$date)
    list(remove.dates)
    
    #x# from the list above, input the dates into the filter function below by their column number
    if(nrow(remove.dates)>0){
      for(i in 1:nrow(remove.dates)){
        abundance.base <- abundance.base %>%
          filter(date != remove.dates[i,])#%>%
        #filter(date != as.Date(remove.dates[2,])) 
        #  filter(date != as.Date(remove.dates[,])) # unsure if this second line will cause problems
      }
    }
    
    #try <- abundance.base[(abundance.base$date != remove.dates[,]),]
    remove.dates
    unique(abundance.base$date)
    a.b.prod <- rbind(a.b.prod,abundance.base)
  }
  abundance.base <- a.b.prod # this is redundant but was kept to work with the rest of the code which calls on abundance.base

chinook <- abundance.base

########Coho#########

sealice.current<-sealicedata

length(unique(sealice.current$location))

#adjust for our focus species
sealice.current <- sealice.current<-data.frame(subset(sealice.current, species == "coho"))

salmcounts <- sealice.current %>%
  select( Lep_cope, chalA, chalB, Lep_PAmale, Lep_PAfemale, Lep_male,
          Lep_nongravid, Lep_gravid, Caligus_cope, Caligus_mot, Caligus_gravid, unid_cope,
          chal_unid,unid_PA, unid_adult)

# RM defining categories of lice.

#motile lice sub
motlice<-sealice.current[,c("Caligus_mot", "Caligus_gravid", "Lep_gravid", "Lep_nongravid", "Lep_male", "Lep_PAfemale", "Lep_PAmale", "unid_PA", "unid_adult")]
#cope lice sub
copes<-sealice.current[,c("Lep_cope", "Caligus_cope", "unid_cope")]
#chalimus lice sub
chals<-sealice.current[,c("chalA", "chalB", "chal_unid")]
#attached lice sub
attlice<-sealice.current[,c("Lep_cope","chalA","chalB","Caligus_cope","unid_cope","chal_unid")]

#total lice sub
sealice.current <- sealice.current %>% rowwise() %>%
  dplyr::mutate(Sum_all_lice = sum(c_across(colnames(salmcounts)))) 

#Below gives columns of summed motiles, attached, copepodids, chalimus, and total counts. Useful for prevalence and abundance plots.
sealice.current$motsum<-rowSums(motlice, na.rm = TRUE)
sealice.current$copsum<-rowSums(copes, na.rm = TRUE)
sealice.current$chalsum<-rowSums(chals, na.rm = TRUE)
sealice.current$attachedsum<-rowSums(attlice, na.rm = TRUE)
sealice.current$sum_all_lice<-rowSums(salmcounts, na.rm = T)

# ensuring the column class is numeric
sealice.current$motsum<- as.numeric(sealice.current$motsum)
sealice.current$copsum<- as.numeric(sealice.current$copsum)
sealice.current$chalsum<-as.numeric(sealice.current$chalsum)
sealice.current$attachedsum<-as.numeric(sealice.current$attachedsum)
sealice.current$sum_all_lice<-as.numeric(sealice.current$sum_all_lice)



### END OF SET UP ###
### ## ABUNDANCE ESTIMATES ## #####

#Setting the cap for bootstrapping
n.boot.b<-1000

##ASSIGNING WEEKLY INTERVALS TO ALL THE dates in the abundance.base data set 
#weekly intervals.
#set up vectors to hold data

#X#beginning abundance plots, start bye creating a list function. 
#x#Start of Abundance Plot #1 for first Location in list
#Then determine how long the list is and replicate the following code that many times, only changing the x in base.list[x,] below
#you should replicate this whole code for every location sampled, unless there are not enough samples, rmeber each site needs a min of 30
base.list <- data.frame(unique(sealice.current$location))
list(base.list)
abundance.base <- sealice.current

  # 1
  ####################################################################################################
  ## Taking out the dates where less than 30 fish were caught
  ## Creating a weekly interval column
  ####################################################################################################
  a.b.prod <- data.frame(abundance.base[1,])
  a.b.prod <- a.b.prod[-1,]
  
  
  for(j in 1:nrow(base.list)){
    
    abundance.base<-data.frame(subset(sealice.current, location == base.list[j,]))

    # Removing any dates with less than 30 fish
    
    
    ## Making julian dates
    abundance.base$date <- as.Date(with(abundance.base, paste(year, month, day, sep="-")), "%Y-%m-%d")
    abundance.base$date  <- julian(abundance.base$date) 
    datelist <-(abundance.base$date)
    ## Making a new column for weeklyinterval dates
    abundance.base$weeklyintvl<-rep(0, each = length(abundance.base$date))
    
    ## Finding the weekly intervals
    value <- abundance.base$weeklyintvl
    # all dates next to an empty column
    dat <- data.frame(datelist, value)
    # vector of all the unique dates
    dates = unique(dat$date)
    # vector for the number of rows for each date
    count <- c()
    
    for(i in 1:length(dates)){
      # length of the subset associated with each date
      n <- length(which(dat$datelist == dates[i]))
      # put the count into the empty vector
      count[i] <- n
    }
    # each unique date has a count of it's rows (assuming each row is a fish)
    output1 <- data.frame(dates, count)
    
    # Here you just turn dates into julian dates for ease later on
    #cbind to as.date version
    abundance.base$date <- as.Date(with(abundance.base, paste(year, month, day, sep="-")), "%Y-%m-%d")
    datelist <-(abundance.base$date)
    abundance.base$weeklyintvl<-rep(0, each = length(abundance.base$date))
    value <- abundance.base$weeklyintvl
    dat <- data.frame(datelist, value)
    dates = unique(dat$date)
    count <- c()
    for(i in 1:length(dates)){
      n <- length(which(dat$datelist == dates[i]))
      count[i] <- n
    }
    output2 <- data.frame(dates, count)
    
    #cbind outputs
    output.date <- cbind(output1,output2)
    output.date = subset(output.date, select = -c(count))
    
    #subset to remove dates with less than 30 fish
    remove.dates <- subset(output.date$dates.1, count < 30)
    #remove.dates <- as.Date(subset(output.date$dates, count <30)
    remove.dates <- data.frame(remove.dates)
    #remove dates from main data
    abundance.base$date  <- as.Date(abundance.base$date) 
    require(lubridate)
    class(remove.dates[,])
    class(abundance.base$date)
    list(remove.dates)
    
    #x# from the list above, input the dates into the filter function below by their column number
    if(nrow(remove.dates)>0){
      for(i in 1:nrow(remove.dates)){
        abundance.base <- abundance.base %>%
          filter(date != remove.dates[i,])#%>%
        #filter(date != as.Date(remove.dates[2,])) 
        #  filter(date != as.Date(remove.dates[,])) # unsure if this second line will cause problems
      }
    }
    
    #try <- abundance.base[(abundance.base$date != remove.dates[,]),]
    remove.dates
    unique(abundance.base$date)
    a.b.prod <- rbind(a.b.prod,abundance.base)
  }
  abundance.base <- a.b.prod # this is redundant but was kept to work with the rest of the code which calls on abundance.base


coho <- abundance.base

unique(chum$species)

####cbind all the species subs together

abundance.base <- rbind(chum,chinook,coho)

colnames(abundance.base)
check <- abundance.base %>% group_by(date,location,species) %>% 
  summarize(Captures = sum(notNA(fish_id)))


#####YEAR SWITCH!!!1!!@@@@######
yr <- "2021"
#subset to adjust for year

abundance.base <- subset(abundance.base, year == yr)

## A table of weekly averages for different lice stages at different sites

#################################################################################################

#a. Finding the weekly intervals
#################################################################################################

# RM convert to julian
juliandates<-julian(abundance.base$date)

# RM find first day of sampling
firstday<-min(juliandates)

# RM The no. of weeks, rounded
no.weeks<-ceiling((max(juliandates)-min(juliandates))/7)

# RM A dataframe for the weekly interval data.
JDweeklyintervals<-rep(0, times = no.weeks)

# RM The weekly intervals are at multiples of 7 starting from the first day. 
for (i in 1:no.weeks) {
  if(i == 1){JDweeklyintervals[i]<-firstday+(6)} # This is to make sure that the first week only has 7 days, not 8.
  else{JDweeklyintervals[i]<-(firstday+(7*i)-1)} # This is to make sure that the following weeks are 7 days and the weekly interval date is the last day of the interval.
}

#Below converts julian to normal date. This is a useful bit of code to recycle... 
weeklyintervals<-as.Date(JDweeklyintervals, origin=as.Date("1970-01-01"))

#weekly intervals are given above to use for making weekly means. Now you can calculate means within those dates.

#b. Making a column in the main data set that assigns weekly intervals to each row.
#################################################################################################

#may need to make the abundance.base into julian date
abundance.base$j.date<-julian(abundance.base$date)

JDweeklyintervalsloops<-c(0, JDweeklyintervals) #RM The 0 at the sart captures all the dates prior to the first weekly interval date (ie. the first 7 days of sampling)
abundance.base$weeklyintvl<-rep(0, each = length(abundance.base$date))
#using subsets to add data of appropriate date to the vectors

for (i in 1:(length(JDweeklyintervalsloops)-1)) {
  # RM Weekly interval subset
  loopintvl<-subset(abundance.base, abundance.base$j.date > JDweeklyintervalsloops[i] & abundance.base$j.date <= JDweeklyintervalsloops[i+1])
  # RM The index of the dates in this interval subset.
  positionsforaddingtoabundance.base<-which(abundance.base$j.date > JDweeklyintervalsloops[i] & abundance.base$j.date <= JDweeklyintervalsloops[i+1])
  # RM Assigning the latest date of a weekly interval to the weekly interval column of the main dataset 
  abundance.base$weeklyintvl[positionsforaddingtoabundance.base]<-JDweeklyintervalsloops[i+1]
  
  
}

#setting up counts
#x# make sure the coloumns selected below 12-26 correspond to actual
salmcounts<-subset(abundance.base[,c(11:25)])
#motile lice sub
motlice<-abundance.base[,c("Caligus_mot", "Caligus_gravid", "Lep_gravid", "Lep_nongravid", "Lep_male", "Lep_PAfemale", "Lep_PAmale", "unid_PA", "unid_adult")]
#cope lice sub
copes<-abundance.base[,c("Lep_cope", "Caligus_cope", "unid_cope")]
#chalimus lice sub
chals<-abundance.base[,c("chalA", "chalB", "chal_unid")]
#attached lice sub
attlice<-abundance.base[,c("Lep_cope","chalA","chalB","Caligus_cope","unid_cope","chal_unid")]
#total lice sub
abundance.base <- abundance.base %>% rowwise() %>%
  dplyr::mutate(Sum_all_lice = sum(c_across(Lep_cope:unid_adult)))


#Below gives columns of summed motiles, attached, copepodids, chalimus, and total counts. Useful for prevalence and abundance plots.
abundance.base$motsum<-rowSums(motlice, na.rm = TRUE)
abundance.base$copsum<-rowSums(copes, na.rm = TRUE)
abundance.base$chalsum<-rowSums(chals, na.rm = TRUE)
abundance.base$attachedsum<-rowSums(attlice, na.rm = TRUE)
abundance.base$sum_all_lice<-rowSums(salmcounts, na.rm = T)
abundance.base$motsum<- as.numeric(abundance.base$motsum)
abundance.base$copsum<- as.numeric(abundance.base$copsum)
abundance.base$attachedsum<-as.numeric(abundance.base$attachedsum)
abundance.base$sum_all_lice<-as.numeric(abundance.base$sum_all_lice)


#Below gives columns of summed motiles, attached, copepodids, chalimus, and total counts. Useful for prevalence and abundance plots.
abundance.base$motsum<-rowSums(motlice, na.rm = TRUE)
abundance.base$copsum<-rowSums(copes, na.rm = TRUE)
abundance.base$chalsum<-rowSums(chals, na.rm = TRUE)
abundance.base$attachedsum<-rowSums(attlice, na.rm = TRUE)
abundance.base$Sum_all_lice<-rowSums(salmcounts, na.rm = T)

#Last line in this chunk assembles the stages-tables to give the SUM of all lice stages by location

Motlicetab<-aggregate(motsum~location, data = abundance.base, sum)
Attlicetab<-aggregate(attachedsum~location, data = abundance.base, sum)
Coplicetab<-aggregate(copsum~location, data = abundance.base, sum)
Challicetab<-aggregate(chalsum~location, data = abundance.base, sum)
alltab<-aggregate(Sum_all_lice~location, data = abundance.base, sum)
# This is the final table for plots of sums! :)))
licetable<-data.frame(Motlicetab, Coplicetab[2], Challicetab[2], alltab[2], Attlicetab[2])

# This is a table for means by site and date

Motlicetab.mean.site.date<-aggregate(motsum~location + date, data = abundance.base, mean)
Attlicetab.mean.site.date<-aggregate(attachedsum~location + date, data = abundance.base, mean)
Coplicetab.mean.site.date<-aggregate(copsum~location+ date, data = abundance.base, mean)
Challicetab.mean.site.date<-aggregate(chalsum~location+ date, data = abundance.base, mean)
alltab.mean.site.date<-aggregate(Sum_all_lice~location+ date, data = abundance.base, mean)
# This is the final table for plots of means! :)))
licetable.mean.site.date<-data.frame(Motlicetab.mean.site.date, Coplicetab.mean.site.date[3], Challicetab.mean.site.date[3], alltab.mean.site.date[3], Attlicetab.mean.site.date[3])


#view to make sense

#make sure to check that sums make sense

#Last line in this chunk assembles the stages-tables to give the MEAN of all lice stages by location
mMotlicetab<-aggregate(motsum~location, data = abundance.base,mean)
mAttlicetab<-aggregate(attachedsum~location, data = abundance.base, mean)
mCoplicetab<-aggregate(copsum~location, data = abundance.base, mean)
mChallicetab<-aggregate(chalsum~location, data = abundance.base, mean)
malltab<-aggregate(Sum_all_lice~location, data = abundance.base, mean)
meanlicetable<-data.frame(mMotlicetab, mAttlicetab[2], mCoplicetab[2], mChallicetab[2], malltab[2])
meanlicetable<-meanlicetable[order(meanlicetable$location),]
secols<-data.frame(motsum = numeric(0), attsum = numeric(0), copesum = numeric(0), chalsum = numeric(0), allsum = numeric(0))
for (i in 1:length(meanlicetable$location)) {
  
  semeans.site.temp<-subset(abundance.base, location == meanlicetable$location[i])
  secols[i,1]<-sd(semeans.site.temp$motsum)/sqrt(length(semeans.site.temp$motsum))
  secols[i,2]<-sd(semeans.site.temp$attachedsum)/sqrt(length(semeans.site.temp$motsum))
  secols[i,3]<-sd(semeans.site.temp$copsum)/sqrt(length(semeans.site.temp$motsum))
  secols[i,4]<-sd(semeans.site.temp$chalsum)/sqrt(length(semeans.site.temp$motsum))
  secols[i,5]<-sd(semeans.site.temp$Sum_all_lice)/sqrt(length(semeans.site.temp$motsum))
}

names(secols)<-paste(c("SE.motile", "SE.attached", "SE.cops", "SE.chals", "SE.all"))
#line of code below shows error " Error in data.frame(..., check.names = FALSE) : 
#arguments imply differing number of rows: 5, 0
meanlicetablewithtotalse<-(cbind(meanlicetable, secols))
# This is the final table for plots of means! :)))
#liceofmeanlicetable<-data.frame(cbind(meanlicetable$motsum, meanlicetable$chalsum, meanlicetable$copsum))
allmeanlice <- data.frame(cbind(meanlicetable$motsum, meanlicetable$chalsum, meanlicetable$copsum, deparse.level = 1))
#names(allmeanlice[names(allmeanlice)== "X1"]) <- "motsum"
#names(allmeanlice[names(allmeanlice)== "X2"]) <- "chalsum"
#names(allmeanlice[names(allmeanlice)== "X3"]) <- "copsum"
#check order of names from grouped sites in best 2020, copy that orientation below
unique(abundance.base$location)
colnames(allmeanlice)<- c("mot.sum", "chalsum", "copsum")
rownames(allmeanlice) <- c( "Cypre River","North Meares","Bedwell Sound North","Ritchie Bay","Bedwell Sound Middle", "Bedwell Sound South","Moyeha","Cancer","White Pine")

#$% issue with Total showing up....
#making a shareable table of the lice means
getwd()

#save above table to csv with specified path = wd/data.output.path
#automated version

write.csv(meanlicetablewithtotalse,file.path(data.output.path,paste(i,"meanlicetable.bysite.csv")))

write.csv(licetable,file.path(data.output.path,paste(i,"totalsumslicetable.csv")))

####optional subsets for site groupings################
#prev.bedwell2019<-data.frame(subset(siteprevalence, location == "Bedwell Estuary North" | location == "Bedwell Estuary Middle" | location == "Bedwell Estuary South"))
#prev.Macks2019<- data.frame(subset(siteprevalence, location == "Cypre River" | location == "Ritchie Bay" | location == "Buckle Bay"))
#prev.Misc2019<- data.frame(subset(siteprevalence, location == "Tranquil estuary"| location == "Keltsmaht"| location == "Moyeha"| location == "Elbow Bank" | location == "TRM"|location == "Tsapee Narrows"))

#########this is prevalence at each site which can be shown in the barchart

#current sample site list
#"Bedwell Estuary", "Bedwell Estuary 2", "Bedwell Estuary 3", "Bedwell Estuary 4", 
#"Bedwell River", "Buckle Bay", "Cypre River", "Elbow Bank", "Keltsmaht", "Moyeha", "Ritchie Bay", 
#"Sniffles", "Sniffles 2", "Tranquil Estuary", "TRM", "Tsapee Narrows"

##

#making the attached and motile lice into one column. 
#This is for the ggplot which is pretty meh. We didn't use it, but it could be used.
motile_lice<-licetable$motsum
attached_lice<-licetable$attachedsum

Lice_Sum<-c(rbind(motile_lice, attached_lice))
##****************************

#Need to add any new sites here in "_", like Tsapee Narrows and TRM
unique(abundance.base$location)
Sample_Site<- c(rep(c("Cypre River","North Meares","Bedwell Sound North","Ritchie Bay","Bedwell Sound Middle","Bedwell Sound South","Moyeha","Cancer","White Pine"), each = 2))

#need to put in the total number of sample sites here
ns<-(length(Sample_Site)/2)
Lice_Stages<- c(rep(c("Motile", "Attached"), times = ns))
Data<-data.frame(Sample_Site, Lice_Stages, Lice_Sum)
## END OF CALCULATIONS ##


## PLOTTING ##
dev.off()
#setting up plot descriptions
colours<-rainbow(n)
linetype<-c(1:n)   
plotchar<-seq(18,18+n,1)

##PREVALENCE WITH STAGES plots for each site

#This gives you the prevalence for different stages over time. 

#Need this vector for the legend in the for loops.
prevalence.stage.legend<-c("Total","Motile", "Chalimus", "Copepodid")
groups.locations<-data.frame(Sample_Site)


prevsiteday <- data.frame(date = numeric(0),
                          site = character(0),
                          totalprev = numeric(0),
                          motprev = numeric(0),
                          chalprev = numeric(0),
                          copeprev = numeric(0))


listofsites <- unique(abundance.base$location)
### 
#Just doing a for loops for each location.
library(viridis)
nloop<-length(listofsites)
#RM : trying to make the legend appear outside the plot so it doesn't get overlapped by data

for (i in 1:nloop) {
  par(mfrow = c(2,1), mar = c(4,4,1,1), oma=c(2,2,2,2))
  site3<-subset(abundance.base, location == listofsites[i]) 
  #this gives you an individual site to work with.
  #optional subset for chum. Subsetting for chinook and coho might be ok, but probably very low numbers.
  
  #site.s3<-subset(site3, species == "chum")
  #for (j in 1:datecount) {
  
  site3$countcol <- rep(1,nrow(site3))
  #this gives you a column of ones
  nc3<-length(site3$countcol)
  #this is the count of fish at the sites
  site3$infected<-rep(0,nc3)
  site3$infected = site3$infected + (site3$Sum_all_lice > 0)
  #This gives you a column of 1 or 0 where 1 means they are infected and 0 means they are clean
  
  ##Trying to make prevalence of the different stages##
  #Make a column for the cope, chal and motile stages
  site3$copinf<-rep(0,nc3)
  site3$copinf = site3$copinf + (site3$Lep_cope >0 | site3$unid_cope >0| site3$Caligus_cope >0)
  site3$copinf[is.na(site3$copinf)]<-0
  site3$chalinf<-rep(0,nc3)
  site3$chalinf = site3$chalinf + (site3$chalA >0 | site3$chalB >0| site3$chal_unid >0)
  site3$chalinf[is.na(site3$chalinf)]<-0
  site3$motinf<-rep(0,nc3)
  site3$motinf = site3$motinf + (site3$Lep_PAmale >0 | site3$Lep_PAfemale >0| site3$Lep_male >0 |site3$Lep_nongravid >0| site3$Lep_gravid >0|site3$Caligus_mot >0|site3$Caligus_gravid >0|site3$unid_PA >0 |site3$unid_adult >0)
  site3$motinf[is.na(site3$motinf)]<-0
  #The above seems weird because the total prevalence is not additive of all the different stages.So you can have as many total infected as the max number for a given stage.
  
  
  
  # now just need to aggregate by week
  siteagg3<-aggregate(x = site3[c("infected", "countcol", "copinf", "chalinf", "motinf")], FUN = sum, by = list(Group.date = site3$date))
  #shows you how many were infected for each date that the specific site was sampled
  siteforsiteagg<-rep(paste(listofsites[i]), length(siteagg3$Group.date))
  siteagg3$site<-siteforsiteagg
  #aggregates for the other stages so that we may have a prevalence line per stage
  #siteaggcop<-aggregate(x = site3[c("copinf", "countcol")]), FUN = sum, by = list(Group.date = site3$date))
  #siteaggchal<-aggregate(x = site3[c("chalinf", "countcol")]), FUN = sum, by = list(Group.date = site3$date))
  #siteaggmot<-aggregate(x = site3[c("motinf", "countcol")]), FUN = sum, by = list(Group.date = site3$date))
  
  names(siteagg3)[3]<-paste("total.fish")
  #changing header names
  names(siteagg3)[2]<-paste("total.infected.fish")
  
  #calculating prevalence for sites
  siteagg3$copprev<-siteagg3$copinf/siteagg3$total.fish
  siteagg3$chalprev<-siteagg3$chalinf/siteagg3$total.fish
  siteagg3$motprev<-siteagg3$motinf/siteagg3$total.fish
  siteagg3$totalprevalence<-siteagg3$total.infected.fish/siteagg3$total.fish
  
  #*************************
  #can change ranges to match the subset
  names(siteagg3)[8]<-paste("copepodid.prevalence")
  names(siteagg3)[9]<-paste("chalimus.prevalence")
  names(siteagg3)[10]<-paste("motile.prevalence")
  names(siteagg3)[11]<-paste("total.prevalence")
  
  loopssubset1<-subset(abundance.base, location == listofsites[i])
  loop1xrange.dp<-range(loopssubset1$date) 
  #xrange.dp<-range(abundance.base$date)
  forprevyrange<-seq(0.00, signif(max(siteagg3$total.prevalence, na.omit = TRUE ), digits = 2), 0.01)
  loops1yrange.dp<-range(forprevyrange)
  coloursloop<-c("#39558CFF","#238A8DFF","#B8DE29FF","#3CBC75FF")
  
  plot(siteagg3$total.prevalence~siteagg3$Group.date, xlim = loop1xrange.dp, ylim = loops1yrange.dp, type="n", xlab = "Date", ylab = "Prevalence (infected fish/total fish)",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5) 
  
  # RM : if a site was only sampled once, the plot looks pretty silly. It shows 2000 - 2040 with data for one x value. 
  #     We probably don't need to change these plots because seeing this data over time is uneccesary. They can just be discarded and the info can be seen in the barplot
  
  title(main = paste(listofsites[i]), j)
  
  
  lines(siteagg3$Group.date, siteagg3$total.prevalence, lty=linetype[1], pch=plotchar[1], lwd = 2, type ="o", col = "#39558CFF", cex =1.5)
  lines(siteagg3$Group.date, siteagg3$motile.prevalence, lty=linetype[2], pch=plotchar[2], lwd = 2, type ="o", "#238A8DFF", cex =1.5 )
  lines(siteagg3$Group.date, siteagg3$chalimus.prevalence, lty=linetype[3], pch=plotchar[3], lwd = 2, type ="o", col = "#B8DE29FF", cex =1.5 )
  lines(siteagg3$Group.date, siteagg3$copepodid.prevalence, lty=linetype[4], pch=plotchar[4], lwd = 2, type ="o", col = "#3CBC75FF", cex =1.5 )
  axis(side = 1, at = 1:9,
       labels = c("Mar 01", "Mar 15", "Apr 01", "Apr 15", "May 01", "May 15", "Jun 01", "June 15", "Jul 01"),
       cex.axis = .5)
  
  prevsiteday<-rbind(prevsiteday, siteagg3)
  library(vtable)
  st(siteagg3, group = "Group.date",group.long = TRUE, vars = "total.prevalence", title = listofsites[i])
  setwd(figures.path)
  ggsave(filename=paste(j, listofsites[i], "_Prevalence_by_sample_date", ".png", sep = "_"))
  }
colnames(siteagg3)

#legend("bottomright", legend = c("chalimus","copepodid", "motile","total"),
       col = c("#B8DE29FF", "#3CBC75FF", "#238A8DFF", "#39558CFF"), cex = 1.5,box.lwd = "o",
       lwd = 1, title = "Louse Life Stage", lty = c(linetype[3],linetype[4],linetype[2],linetype[1]), pch = c(plotchar[3],plotchar[4],plotchar[2],plotchar[1]), ncol=4, xpd=NA)


twoone <- subset(prevsiteday, site == "Cypre River"| site == "North Meares" | site == "Ritchie Bay") 

colnames(twoone)

totals2021<-data.frame(mean.prev = numeric(0), sd.prev = numeric(0), se.prev = numeric(0))

totals2021[1,1:3]<-c(mean(twoone$total.prevalence), sd(twoone$total.prevalence), 
                     sd(twoone$total.prevalence)/sqrt(length(twoone$Group.date)))

