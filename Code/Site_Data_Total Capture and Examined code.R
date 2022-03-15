#Title: Sea Lice Monitoring Outputs 2022
#Authors : Critty (Christian) Carson and Rowen Monks
#Last updated : August 27th, 2021
#Description : Makes TS plots for 
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

#TS PLOTS

#For TS plots, just change the spots that are marked (copy and paste )


sealicedata <- read.csv("Data/ClayoquotSeaLice_Site_Data.csv", header=TRUE, stringsAsFactors=FALSE)

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

#X#change year to current

#subset to adjust for year

sealicedata <- subset(sealicedata)



#adjusting names based on list output
#x#fix errors in names, do this by inputing the incorrect/ current name into the brackets,  
#and then the corrected version into the assigned end <-
unique(sealicedata$location)
sealicedata$location[sealicedata$location == "Cancer (Herbert)"]<- "Cancer"
sealicedata$location[sealicedata$location == "Bedwell Estuary "]<- "Bedwell Estuary"
sealicedata$location[sealicedata$location == "Meares North"]<- "North Meares"
sealicedata$location[sealicedata$location == "White Pine "]<- "White Pine"
sealicedata$location[sealicedata$location == "Ritchie Bay "]<- "Ritchie Bay"
sealicedata$location[sealicedata$locatizaon == "Cypre "]<- "Cypre River"
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

unique(sealicedata$location)

tsaltemp <- sealicedata

tsaltemp$date <- as.Date(with(tsaltemp, paste(year, month, day, sep="-")), "%Y-%m-%d")


tsalsites<-unique(tsaltemp$location)
tsaltemp$salt_surf<-as.numeric(as.character(tsaltemp$salt_surf))
tsaltemp$salt_1m<-as.numeric(as.character(tsaltemp$salt_1m))
tsaltemp$temp_surf<-as.numeric(as.character(tsaltemp$temp_surf))
tsaltemp$temp_1m<-as.numeric(as.character(tsaltemp$temp_1m))
tsaltemp$date<-as.Date(tsaltemp$date, origin ="%Y-%m-%d")
year <- yr

#Makes a dataframe of the averages of temperature and salinity. No julian dates
#without weekly interval. There are 2 averages (0m + 1m). 

for (d in 1:length(tsalsites)) {
  
  temptsal<-subset(tsaltemp, location == tsalsites[d])
  
  datetsal <- as.Date(unique(temptsal$date), origin ="%Y-%m-%d")
  tsd.site<- data.frame(meansurfsalt = numeric(0),
                        meansalt1 = numeric(0),
                        meantempsurf = numeric(0),
                        meantemp1 = numeric(0))
  
  
  #x# it looks like the objects being pulled from in this forloop are not returning any actual values, just NA's and 0's. I think it has to do with the plotsal df's, 
  #these issues seem to propogate into the plots below when tempdates is used
  # RM : I think the issue is that the length is based on the subset of data for one site, but is applied to all sites. I would recommend redoing this whole section as a forloop.
  for (j in 1:length(datetsal)) {
    #subsetting by the first date
    tempdates<-subset(temptsal, date == datetsal[j])  
    for (k in 1:4) {
      #taking the mean of the salinity/temp values for the one date
      tsd.site[j,k]<-mean(tempdates[,(6+k)])
    }
  }
  site.ts <- rep(tsalsites[d], each = length(tsd.site$meantemp1))
  tsd.site1 <- cbind(site.ts, tsd.site)
  if(d == 1) {tsdframe <- as.data.frame(tsd.site1)
  } else if(d > 1 ){tsdframe <- as.data.frame(rbind(tsdframe, tsd.site1))}
}
#Subset by the date which will have multiple sites. If you want to understand the oceanography of
#each site, so if you averaging over all the individual sites, you do get information about the region as a whole over time.

tsaltemp$salmon_captured[is.na(tsaltemp$salmon_captured)]<-0
tsaltemp$salmon_examined[is.na(tsaltemp$salmon_examined)]<-0
tsaltemp$salmon_captured <- as.numeric(tsaltemp$salmon_captured)
tsaltemp$salmon_examined <- as.numeric(tsaltemp$salmon_examined)
tsaltemp$salmon_captured <-  abs(tsaltemp$salmon_captured)
tsaltemp$salmon_examined <-  abs(tsaltemp$salmon_examined)



st(tsaltemp, vars = "salmon_examined")

is.na(tsaltemp$salmon_captured)

sum(tsaltemp$salmon_captured)
sum(tsaltemp$salmon_examined)

st(tsaltemp, group = "year",group.long = TRUE, vars = "salmon_captured", 
   summ = c('notNA(x)',
            'mean(x)',
            'median(x)',
            'propNA(x)', 'sum(x)'))
library(tidyverse)
library(ggrepel)
pacman::p_load(tidyverse,nlme,emmans,here)
pacman::p_load(ggthemes, ggplot2)

source("https://raw.githubusercontent.com/koundy/ggplot_theme_Publication/master/ggplot_theme_Publication-2.R")




# grouped and summarised data
tsaltemp1 <- tsaltemp %>% 
  group_by(year) %>% 
  summarise(salmon_captured = sum(salmon_captured), salmon_examined = sum(salmon_examined))
view(tsaltemp1)

tsaltemp2 <- tidyr::pivot_longer(tsaltemp1, cols=c('salmon_captured', 'salmon_examined'), names_to='variable', 
                           values_to="value")
print(tsaltemp2)

ggplot(tsaltemp2, aes(x=year, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')

# edited plot

capexamplot<- ggplot(tsaltemp2, aes(x=factor(year),y=value, fill=variable))+
  geom_col(position=position_dodge()) +labs(x="Year",y="Total Sum",
                                            title="Juvenile Salmon Captured or Examined from 2018 - 2021" + 
  theme_Publication()+ scale_fill_viridis_d(option = "D",begin = 0.3, end = 0.9))

ggsave(filename=paste(capexamplot, "Captured&ExaminedLice_Yearly", ".png", sep = "_"))

#species by year and exam######
#cleaning
tsaltemp$salmon_captured[is.na(tsaltemp$salmon_captured)]<-0
tsaltemp$salmon_captured <- as.numeric(tsaltemp$salmon_captured)
tsaltemp$salmon_captured <-  abs(tsaltemp$salmon_captured)
#pink
tsaltemp$pink_examined[is.na(tsaltemp$pink_examined)]<-0
tsaltemp$pink_examined <- as.numeric(tsaltemp$pink_examined)
tsaltemp$pink_examined <-  abs(tsaltemp$pink_examined)
#coho
tsaltemp$coho_examined[is.na(tsaltemp$coho_examined)]<-0
tsaltemp$coho_examined <- as.numeric(tsaltemp$coho_examined)
tsaltemp$coho_examined <-  abs(tsaltemp$coho_examined)
#chinook
tsaltemp$chinook_examined[is.na(tsaltemp$chinook_examined)]<-0
tsaltemp$chinook_examined <- as.numeric(tsaltemp$chinook_examined)
tsaltemp$chinook_examined <-  abs(tsaltemp$chinook_examined)
#chum
tsaltemp$chum_examined[is.na(tsaltemp$chum_examined)]<-0
tsaltemp$chum_examined <- as.numeric(tsaltemp$chum_examined)
tsaltemp$chum_examined <-  abs(tsaltemp$chum_examined)
#sockeye
tsaltemp$sockeye_examined[is.na(tsaltemp$sockeye_examined)]<-0
tsaltemp$sockeye_examined <- as.numeric(tsaltemp$sockeye_examined)
tsaltemp$sockeye_examined <-  abs(tsaltemp$sockeye_examined)

#2021 check and breakdown
tsaltemp2021 <- subset(tsaltemp, tsaltemp$year == "2021")

colnames(tsaltemp2021)
tsaltemp2021 <- tsaltemp2021 %>% 
  group_by(year) %>% 
  summarise(salmon_examined = sum(salmon_examined), pink_examined = sum(pink_examined), 
            coho_examined = sum(coho_examined), chinook_examined = sum(chinook_examined), chum_examined = sum(chum_examined), sockeye_examined = sum(sockeye_examined))
view(tsaltemp2021)

library(vtable)

colnames(tsaltemp2021)
st(tsaltemp2021, group = "location",group.long = TRUE, vars = "salmon_captured", 
   summ = c('max(x)',
            'mean(x)',
            'median(x)',
            'propNA(x)', 'sum(x)'))

colnames(tsaltemp)
speciesset <- tsaltemp %>% 
  group_by(year) %>% 
  summarise(salmon_examined = sum(salmon_examined), pink_examined = sum(pink_examined), 
            coho_examined = sum(coho_examined), chinook_examined = sum(chinook_examined), chum_examined = sum(chum_examined), sockeye_examined = sum(sockeye_examined))
view(speciesset)

speciesset2 <- tidyr::pivot_longer(speciesset, cols=c('pink_examined','coho_examined', 'chinook_examined','chum_examined','sockeye_examined'), names_to='variable', 
                                 values_to="value")
print(speciesset2)

# edited plot

ggplot(speciesset2, aes(x=factor(year),y=value, fill=variable))+
  geom_col(position=position_stack()) +labs(x="Year",y="Total Sum",fill = "Species",
          title="Juvenile Salmon Examined from 2018 - 2021")+
            theme_Publication()+  scale_fill_viridis_d(direction = 1, option = "D")


ggsave(filename=paste(capexamplot, "Captured&ExaminedLice_Yearly", ".png", sep = "_"))

