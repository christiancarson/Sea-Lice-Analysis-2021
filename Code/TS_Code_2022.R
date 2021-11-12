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

yr <- "2021"
#subset to adjust for year

sealicedata <- subset(sealicedata, year == yr)



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


# RM : This should be a for loop.
wd <-  "/Users/user/Desktop/Sea-Lice-Analysis-2021/OutputFigures/TS plots" 
  view(tsaltemp)
for(i in 1 : length(tsalsites)){
  
  temptsal<-subset(tsaltemp, location == tsalsites[i])
  datetsal <- as.Date(unique(temptsal$date), origin ="%Y-%m-%d")
  temp.d <- subset(tsdframe, site.ts == tsalsites[i])
  ritchieplottsal<-data.frame(datetsal, temp.d)
  ritchieplottsal$datetsal<-as.Date(format(ritchieplottsal$datetsal, format = "%Y/%m/%d"))
  #RM : note that the line below could omit more rows than you want if the row is Only missing surface salinity
  ritchieplottsal <- ritchieplottsal[!is.na(ritchieplottsal$meansurfsalt),]
  ritchieplottsal1 <- as.data.frame(ritchieplottsal) 
  ritchieplottsal1 <- ritchieplottsal1[, colSums(is.na(ritchieplottsal1)) < nrow(ritchieplottsal1)]
  
  #na.omit(ritchieplottsal1)
  #complete.cases(ritchieplottsal1)
  #ritchieplottsal1$meansurfsalt <- NULL
  #ritchieplottsal1<-as.data.frame(na.omit(ritchieplottsal), stringsAsFactors=FALSE)  
  
  xrangets<-as.Date(format(range(ritchieplottsal1$datetsal), format = "%Y-%m-%d"))
  xts<-as.Date(format(ritchieplottsal1$datetsal, format = "%Y-%m-%d"))
  
  yrangets2<-c(10,30)
  
  par(mar = c(5,5,5,5), xpd = F, new = F)
  
  plot(ritchieplottsal1$meansalt1~xts,  type = "n", 
       xlim = xrangets, ylim = yrangets2, ylab = "Salinity (psu)",
       cex.lab = 1.5, cex.axis = 1.5, xlab = "", main = paste("", tsalsites[i], sep = " "), cex.main = 2)
  
  lines(ritchieplottsal1$meansalt1~xts,
        type = "b", lwd = 2, lty = 1, col = "darkblue") 
  lines(ritchieplottsal1$meansurfsalt~xts, 
        type = "b", lwd = 2, lty = 2, col = "dodgerblue") 
  

  par(new = TRUE)
  
  yrangets3<-c(5,20)
  
  plot(ritchieplottsal1$meantemp1~xts,  type = "n", axes = FALSE,
       xlim = xrangets, ylim = yrangets3, ylab = "",
       cex.lab = 1.5, cex.axis = 1.5, xlab = "", main = "", cex.main = 2)
  
  lines(ritchieplottsal1$meantemp1~xts,
        type = "b", lwd = 2, lty = 1, col = "gray44") 
  lines(ritchieplottsal1$meantempsurf~xts, 
        type = "b", lwd = 2, lty = 2, col = "darkgray") 
  axis(4, ylim = yrangets3, cex.lab=1.5,cex.axis=1.5)
  mtext(side = 4, "Temperature (C)", line = 2.5, cex = 1.5)
  
  
  write.csv(ritchieplottsal1, paste("meanTS.",tsalsites[i], sep ="_"))
  
  png(paste(tsalsites[i],year,"TS.png", sep='_'),5,5)
  dev.off()
  
}



#RM : you shouldn't need the code between these hashtags

  
  #Ritchie check
  unique(tsaltemp$Site.ID)
  ritch <- subset(tsaltemp, tsaltemp$location == "Ritchie Bay")
View(ritch)
ritch <- cbind(ritch$date,ritch$salt_surf,ritch$salt_1m,ritch$temp_surf,ritch$temp_1m)
colnames(ritch)[which(names(ritch) == "DD")] <- "Date"
colnames(ritch)[which(names(ritch) == "V2")] <- "Surface SL"
colnames(ritch)[which(names(ritch) == "V3")] <- "-1m SL"
colnames(ritch)[which(names(ritch) == "V4")] <- "Surface Temp"
colnames(ritch)[which(names(ritch) == "V5")] <- "-1m SL"

colnames(temptsal)

view(tsaltemp)
st(tsaltemp, vars = "salt_surf")
st(abundance.base, group = "year",group.long = TRUE, vars = "sum_all_lice", 
   summ = c('notNA(x)',
            'mean(x)',
            'median(x)',
            'propNA(x)', 'sum(x)'))

  