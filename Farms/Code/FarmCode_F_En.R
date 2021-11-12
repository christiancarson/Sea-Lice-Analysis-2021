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
wd
getwd()
wd <- getwd()  # working directory
setwd(wd)
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
post2019 <- read.csv(paste(sealicedata.path, "/", "2019oncounts.csv",
                              sep = ""), stringsAsFactors = FALSE)
pre2019 <- read.csv(paste(sealicedata.path, "/", "2011-2019counts.csv",
                                sep = ""), stringsAsFactors = FALSE)


unique(colnames(post2019))
unique(colnames(pre2019))


sealicedata <- rbind(post2019,pre2019)
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

warnings()
unique(sealicedata$Number.of.Pens.Sampled)
sealicedata$Finfish.Aquaculture.Reporting.Zone[sealicedata$Finfish.Aquaculture.Reporting.Zone == "2.3"]<- "Clayoquot Sound"


clayoquot <- subset(sealicedata, Finfish.Aquaculture.Reporting.Zone == "Clayoquot Sound")





# 2
#################################################################################################

## A table of weekly averages for different lice stages at different sites



#c. Finding weekly counts
#################################################################################################

#setting up counts

#Select columns corresponding to all the lice count columns
#x# if column names change in the database, the lines below must change.

unique(colnames(clayoquot))
abundance.base <- clayoquot %>%
  select(Year, Month, Licence.Holder, Site.Common.Name,Latitude,Longitude,Finfish.Aquaculture.Reporting.Zone,Number.of.Pens.Sampled,Average.L..salmonis.motiles.per.fish,
  Average.L..salmonis.females.per.fish,Average.chalimus.per.fish,Average.caligus.per.fish,Incident.Date)



# 2
#################################################################################################

## A table of weekly averages for different lice stages at different sites

#################################################################################################

#a. Finding the weekly intervals
#################################################################################################

# RM convert to julian

library(dplyr)

abundance.base <- abundance.base %>% 
  filter(!is.na(Incident.Date))

unique(abundance.base$Incident.Date)
abundance.base = filter(abundance.base, Incident.Date != "n/a")

abundance.base$Incident.Date <- as.Date(abundance.base$Incident.Date, "%Y-%m-%d")

juliandates<-julian(abundance.base$Incident.Date)

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
abundance.base$j.date<-julian(abundance.base$Incident.Date)

JDweeklyintervalsloops<-c(0, JDweeklyintervals) #RM The 0 at the sart captures all the dates prior to the first weekly interval date (ie. the first 7 days of sampling)
abundance.base$weeklyintvl<-rep(0, each = length(abundance.base$Incident.Date))
#using subsets to add data of appropriate date to the vectors

for (i in 1:(length(JDweeklyintervalsloops)-1)) {
  # RM Weekly interval subset
  loopintvl<-subset(abundance.base, abundance.base$j.date > JDweeklyintervalsloops[i] & abundance.base$j.date <= JDweeklyintervalsloops[i+1])
  # RM The index of the dates in this interval subset.
  positionsforaddingtoabundance.base<-which(abundance.base$j.date > JDweeklyintervalsloops[i] & abundance.base$j.date <= JDweeklyintervalsloops[i+1])
  # RM Assigning the latest date of a weekly interval to the weekly interval column of the main dataset 
  abundance.base$weeklyintvl[positionsforaddingtoabundance.base]<-JDweeklyintervalsloops[i+1]
  
  
}
unique(colnames(abundance.base))
write.csv(abundance.base,file.path(data.output.path,"CermaqData2019-2021.csv"))


View(abundance.base)
abundance.base<-subset(abundance.base, Year == "2021")
unique(abundance.base$Site.Common.Name)

# making a dataframe with dates, lice, and all sites included
weeksitelice<-data.frame(abundance.base$Incident.Date, abundance.base$j.date, abundance.base$weeklyintvl ,abundance.base$Site.Common.Name,  
                         abundance.base$Average.caligus.per.fish, abundance.base$Average.chalimus.per.fish, abundance.base$Average.L..salmonis.motiles.per.fish, abundance.base$Average.L..salmonis.females.per.fish)
names(weeksitelice)<-paste(c("date", "j.date", "weeklyintvl", "location", "caligussum", "chalsum", "motsum", "sum_all_lice"))
#names(weeksitelice)<-paste(c("Incident.Date", "j.date", "weeklyintvl", "location", "copsum", "chalsum", "motsum", "sum_all_lice"))
# subsetting the dataframe for each site




base.list <- data.frame(unique(weeksitelice$location))

unique(weeksitelice$location)
view(weeksitelice)

#d. i. making a loop to 
Ritchieweeklies<-subset(weeksitelice, location == "Saranac Island")
Ritchieweeklies <- na.omit(Ritchieweeklies)

Ritchieweeklies$weeklyintvl<-as.Date(Ritchieweeklies$weeklyintvl, origin = as.Date("1970-01-01"))
Ritchieweeklies$weeklyintvl<-format(Ritchieweeklies$weeklyintvl, format = "%b %d %y")
View(Ritchieweeklies)


#Ritchieweeklies <- na.omit(Ritchieweeklies)

colnames(Ritchieweeklies)

#ritchie #ggplot attempt 
unique(colnames(Ritchieweeklies))
Ritchieweeklies <- subset(Ritchieweeklies,select= -c(date, j.date, location))

colnames(Ritchieweeklies)[which(names(Ritchieweeklies) == "weeklyintvl")] <- "Week"

colnames(Ritchieweeklies)[which(names(Ritchieweeklies) == "caligussum")] <- "Mean Caligus Per Fish"

colnames(Ritchieweeklies)[which(names(Ritchieweeklies) == "chalsum")] <- "Mean Chalimus Per Fish"

colnames(Ritchieweeklies)[which(names(Ritchieweeklies) == "motsum")] <- "Mean L.s. Motiles Per Fish"

colnames(Ritchieweeklies)[which(names(Ritchieweeklies) == "sum_all_lice")] <- "Mean L.s. Females Per Fish"

write.csv(Ritchieweeklies,file.path(data.output.path,"SarnacIsland2019-2021.csv"))


library(ggplot2)
library(tidyr)

Ritchieweeklies %>%
  gather("Type", "Value",-Week) %>%
  ggplot(aes(Species, Value, fill = Type)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_bw()


#reshape
unique(Ritchieweeklies$Week)
library(reshape2)
Ritchieweeklies$Week <- factor(Ritchieweeklies$Week, c("May 07 19”,”Jun 04 19”,”Jun 11 19”,”Jun 18 19”,”Jun 25 19”,”Jul 02 19",
                                                       "Jul 16 19”,”Aug 20 19”,”Sep 03 19”,”Sep 10 19”,”Sep 17 19”,”Oct 01 19",
                                                       "Oct 15 19”,”Oct 29 19”,”Nov 12 19”,”Nov 26 19”,”Dec 10 19”,”Dec 24 19",
                                                       "Jan 07 20”,”Jan 21 20”,”Feb 18 20”,”Mar 03 20”,”Mar 10 20”,”Mar 17 20",
                                                       "Mar 24 20”,”Mar 31 20”,”Apr 14 20”,”Apr 21 20”,”Apr 28 20”,”May 05 20",
                                                       "May 19 20”,”Jun 02 20”,”Jun 16 20”,”Jul 14 20”,”Jul 21 20”,”Jul 28 20",
                                                       "Aug 04 20”,”Aug 11 20”,”Aug 25 20”,”Sep 01 20”,”Sep 08 20”,”Mar 16 21",
                                                       "Mar 23 21”,”Mar 30 21”,”Apr 06 21”,”Apr 13 21”,”Apr 20 21”,”Apr 27 21",
                                                       "May 04 21”,”May 18 21”,”May 25 21”,”Jun 01 21”,”Jun 08 21”,”Jun 15 21",
                                                       "Jun 22 21”,”Jun 29 21”,”Jul 06 21”,”Jul 27 21"))


view(Ritchieweeklies)
ggplot(Ritchieweeklies, aes(Week, mean, fill= Stage)) + 
  geom_bar(stat = 'identity', position = 'dodge')+ labs(x = "Week", y = "Mean Abundance") + 
  theme_classic()+
  scale_fill_brewer(palette="Greys")+
  geom_errorbar(data = Ritchieweeklies,aes(ymin=lci, ymax=uci), position=position_dodge(.9), width=0.1)+
  theme(axis.text=element_text(size=14),
        axis.title.x=element_blank(),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Weekly Mean Abundance - Bedwell Sound Middle")  +
  theme(plot.title = element_text(hjust = 0.5))
errors()


write.csv(Ritchieweeklies,file.path(data.output.path,"BedwellSoundMiddle_WeeklyAbundance_2022.csv"))

check <- subset(abundance.base,abundance.base$location == "Bedwell Sound Middle")
check$weeklyintvl<-as.Date(check$weeklyintvl, origin = as.Date("1970-01-01"))
check$weeklyintvl<-format(check$weeklyintvl, format = "%b %d %y")
unique(check$weeklyintvl)
