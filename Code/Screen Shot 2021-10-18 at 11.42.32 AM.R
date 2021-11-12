#Title: Sea Lice Monitoring Outputs 2022
#Author : Critty (Christian) Carson
#Last updated : August 6th, 2021
#Description : Makes mean prevalence, abundance, TS and forklength plots for 
#Sea Lice Monitoring Program 
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
wd <- "/Users/user/Desktop/Sea-Lice-Analysis-2021"
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
sealicedata.path <- paste(wd, "/", "Data", sep = "")

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

pacman::p_load(tidyverse,nlme,emmans,here)
pacman::p_load(ggthemes, ggplot2)

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


#X# List character variables and go through one bye one
list(unique(colnames(sealicedata)))
#make sure year is consistent and case sensitive
list(unique(sealicedata$year))
#make sure names of species are correct
list(unique(sealicedata$species))
#fixing species names
sealicedata$species[sealicedata$species == "chum "]<- "chum"
sealicedata$species[sealicedata$species == "coho "]<- "coho"
#adjusting sample locations
#list locations
list(unique(sealicedata$location))

#adjusting names based on list output
#x#fix errors in names, do this by inputing the incorrect/ current name into the brackets, 
#and then the corrected version into the assigned end <-
sealicedata$location[sealicedata$location == "Bedwell Estuary "]<- "Bedwell Estuary"
sealicedata$location[sealicedata$location == "Meares North"]<- "North Meares"
sealicedata$location[sealicedata$location == "White Pine "]<- "White Pine"
sealicedata$location[sealicedata$location == "Ritchie Bay "]<- "Ritchie Bay"
sealicedata$location[sealicedata$location == "Cypre "]<- "Cypre River"
sealicedata$location[sealicedata$location == "Bedwell 2 "]<- "Bedwell 2"
sealicedata$location[sealicedata$location == "Bedwell Sound"]<- "Bedwell 3"
sealicedata$location[sealicedata$location == "Bedwell estuary"]<- "Bedwell Estuary"
sealicedata$location[sealicedata$location == "Tranquil estuary"]<- "Tranquil Estuary"
sealicedata$location[sealicedata$location == "Bedwell estuary 3"]<- "Bedwell 3"
sealicedata$location[sealicedata$location == "Bedwell estuary 4"]<- "Bedwell 4"
sealicedata$location[sealicedata$location == "Bedwell estuary 2"]<- "Bedwell 2"
sealicedata$location[sealicedata$location == "Bedwell Estuary"]<- "Bedwell Sound North"
sealicedata$location[sealicedata$location ==  "Bedwell 2"]<-"Bedwell Sound Middle"
sealicedata$location[sealicedata$location ==  "Bedwell 3"]<- "Bedwell Sound South"


#X#change year to current

yr <- "2021"
#subset to adjust for year

sealice.current <- subset(sealicedata, year == yr)

length(unique(sealice.current$location))

#adjust for our focus species
sealice.current <- sealice.current<-data.frame(subset(sealice.current,
                                                      species == "coho"|species == "chum"|species == "chinook"|
                                                        species == "sockeye"|species == "pink"))


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
abundance.base<-subset(sealice.current, species == "chum" | species == "coho" | species == "chinook"| species == "sockeye" |species == "salmon")


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
#################################################################################################

# 2
#################################################################################################

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
  
  #c. Finding weekly counts
  #################################################################################################
  
  #setting up counts
  
  #Select columns corresponding to all the lice count columns
  #x# if column names change in the database, the lines below must change.
  salmcounts <- abundance.base %>%
    select( Lep_cope, chalA, chalB, Lep_PAmale, Lep_PAfemale, Lep_male,
            Lep_nongravid, Lep_gravid, Caligus_cope, Caligus_mot, Caligus_gravid, unid_cope,
            chal_unid,unid_PA, unid_adult)
  
  # RM defining categories of lice.
  
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
    dplyr::mutate(Sum_all_lice = sum(c_across(colnames(salmcounts)))) 
  
  #Below gives columns of summed motiles, attached, copepodids, chalimus, and total counts. Useful for prevalence and abundance plots.
  abundance.base$motsum<-rowSums(motlice, na.rm = TRUE)
  abundance.base$copsum<-rowSums(copes, na.rm = TRUE)
  abundance.base$chalsum<-rowSums(chals, na.rm = TRUE)
  abundance.base$attachedsum<-rowSums(attlice, na.rm = TRUE)
  abundance.base$sum_all_lice<-rowSums(salmcounts, na.rm = T)
  
  # ensuring the column class is numeric
  abundance.base$motsum<- as.numeric(abundance.base$motsum)
  abundance.base$copsum<- as.numeric(abundance.base$copsum)
  abundance.base$chalsum<-as.numeric(abundance.base$chalsum)
  abundance.base$attachedsum<-as.numeric(abundance.base$attachedsum)
  abundance.base$sum_all_lice<-as.numeric(abundance.base$sum_all_lice)
  
  
  #d. Finding the weekly means for each lice category
  #################################################################################################
  
  #making for loop for each, starting with motiles
  
  # making a dataframe with dates, lice, and all sites included
  weeksitelice<-data.frame(abundance.base$date, abundance.base$j.date, abundance.base$weeklyintvl ,abundance.base$location,  
                           abundance.base$copsum, abundance.base$chalsum, abundance.base$motsum, abundance.base$sum_all_lice)
  names(weeksitelice)<-paste(c("date", "j.date", "weeklyintvl", "location", "copsum", "chalsum", "motsum", "sum_all_lice"))
  
  # subsetting the dataframe for each site
  
  #d. i. making a loop to focus on one site at a time for weekly mean abundance for each louse stage 
  #################################################################################################
 
   for (ll in 1:nrow(base.list)){
setwd(wd)
  # the site to focus on.
  focusweeksitelice<-subset(weeksitelice, location == print(base.list[ll,]))

  # RM : check
  #unique(focusweeksitelice$location)
  rowcounts.fin <- data.frame(weeklyintvl = numeric(0),
                          Stage = numeric(0),
                          site = numeric(0),
                          mean = numeric(0),
                          lci = numeric(0),
                          uci = numeric(0))
  rowcounts.fin$site <- factor(rowcounts.fin$site, levels= base.list)  
  
  lice.cat <- c("copepodid", "chalimus","motile","total")

  names(focusweeksitelice)[5] <- "copepodid"
  names(focusweeksitelice)[6] <- "chaligus"
  names(focusweeksitelice)[7] <- "motile"
  names(focusweeksitelice)[8] <- "total"
  
  
  
  library(reshape)
  focusweeksitelice <- melt(focusweeksitelice, id=c("date","j.date","weeklyintvl","location"))
  
  for(lc in 1:length(lice.cat)){
  rowcounts <- data.frame(weeklyintvl = numeric(0),
                          Stage = numeric(0),
                          site = numeric(0),
                          mean = numeric(0),
                          lci = numeric(0),
                          uci = numeric(0))
  
  rowcounts$site <- factor(rowcounts$site)  
  
  lci<- NULL
  uci <- NULL
  site.boot<-NULL
  #put site.boot here
  bigsiteboot<-rep(0, times = length(JDweeklyintervals))
  weeklyintervalscol<- JDweeklyintervals
  meanbootcol<-NULL
  


    loopsfocusdata<-focusweeksitelice[focusweeksitelice$variable == lice.cat[lc],]
    
    
    for (j in 1:length(JDweeklyintervals)) {
      mean.boot<-NULL
      #for each interval 1000 iterations are done
      #Subset main data by 1 weeklyintvl to get samples of counts per fish for all dates of one interval.
      temp<-subset(loopsfocusdata, weeklyintvl == JDweeklyintervals[j])
      
      #create the vessel for the 1000 samples which will renew for every weekly interval
      temp.sample <- NULL
      
      
      for(k in 1:n.boot.b) {  
        # for each of the 1000 iterations of boostrapping
        
        # Object to store resampled data for this iteration of bootstrapping
        temp.sample <- c(temp.sample, sample(x=temp$value, size=length(temp$value), replace=T)) 
        mean.boot <- c(mean.boot, mean(temp.sample))
      }   
      
      #I have 1000 means of 1000 samples to get the 25 and 976 percentiles from. And the mean of the means.
      mean.boot <- sort(mean.boot)
      #getting lci and uci and then out of the loop
      lcisamp<-mean.boot[25]
      ucisamp<-mean.boot[975]
      lci<-c(lci, lcisamp)
      uci<-c(uci, ucisamp)
      #Getting mean.boot output out of loop
      meanbootcol<- c(meanbootcol, mean(mean.boot))
    }

  
  JDweeklyintervalsfin<-JDweeklyintervals
  ntable<-length(JDweeklyintervals)
  for (i in 1:ntable) {
    rowcounts[(i),1] <- c(JDweeklyintervalsfin[i])
    rowcounts[(i),4:6] <- c(meanbootcol[i], lci[i], uci[i])
  }
  
  rowcounts$site<-rep(base.list[ll,], each = length(JDweeklyintervals))
  rowcounts$Stage<-rep(lice.cat[lc], each = length(JDweeklyintervals))
  
  rowcounts.fin <- rbind(rowcounts.fin, rowcounts)
  }
 
  weeklyliceloctable<-rowcounts.fin
  View(weeklyliceloctable)
  
  #into a table that has the site name and the date and the means for all lice. 
  
  weeklyliceloctable$weeklyintvl<-as.Date(weeklyliceloctable$weeklyintvl, origin = as.Date("1970-01-01"))
  weeklyliceloctable$weeklyintvl<-format(weeklyliceloctable$weeklyintvl, format = "%b %d %y")

  #d. ii. making a final table and plot of weekly mean abundances of each louse stage, for each site
  #################################################################################################
  
  
  Ritchieweeklies <- na.omit(weeklyliceloctable)
  
  #ritchie #ggplot attempt 
  Ritchieweeklies <- subset(Ritchieweeklies,select= -c(site))
  
  colnames(Ritchieweeklies)[which(names(Ritchieweeklies) == "assignstage")] <- "Stage"
  
  colnames(Ritchieweeklies)[which(names(Ritchieweeklies) == "weeklyintvl")] <- "Week"
  
  colnames(Ritchieweeklies)[which(names(Ritchieweeklies) == "site")] <- "Site"
  
  
    library(reshape2)
  Ritchieweeklies$Week <- factor(Ritchieweeklies$Week)#, list(unique(Ritchieweeklies$Week)))
  
  setwd(data.output.path)
  Ritchieweeklies$Stage <- factor(Ritchieweeklies$Stage, c("total","motile","chalimus","copepodid"))
  write.csv(Ritchieweeklies,file.path(data.output.path, paste(base.list[ll,], "WeeklyAbundance", yr, ".csv", sep = "_")))
  
   Ritchieweeklies <- read.csv(paste(base.list[ll,], "WeeklyAbundance", yr, ".csv", sep = "_"))
   ggplot(Ritchieweeklies, aes(Week, mean, fill= Stage)) + 
    geom_bar(stat = 'identity', position = 'dodge')+ labs(x = "Week", y = "Mean Abundance") + 
    theme_Publication()+
    scale_fill_brewer(palette="Greys")+
    geom_errorbar(data = Ritchieweeklies,aes(ymin=lci, ymax=uci), position=position_dodge(.9), width=0.1)+
    theme(axis.text=element_text(size=14),
          axis.title.x=element_blank(),
          axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
          axis.text.y=element_text(color="black"))+
    ggtitle(paste("Weekly Mean Abundance -", base.list[ll,], sep = " "))  +
    theme(plot.title = element_text(hjust = 0.5))  
  #png(filename=paste(base.list[9,], "Weekly_Mean_Abundance", ".png", sep = "_"))
  #dev.off()
   setwd(figures.path)
     ggsave(filename=paste(base.list[ll,], "Weekly_Mean_Abundance", ".png", sep = "_"))
  #ggsave("output.pdf")
    }
  
  
  check <- subset(abundance.base,abundance.base$location == base.list[ll,])
  check$weeklyintvl<-as.Date(check$weeklyintvl, origin = as.Date("1970-01-01"))
  check$weeklyintvl<-format(check$weeklyintvl, format = "%b %d %y")
  unique(check$weeklyintvl)
  
  summary(check$sum_all_lice)
  summary(check$motsum)
  summary(check$copsum)
  summary(check$chalsum)
  summary(check$attachedsum)

  
  