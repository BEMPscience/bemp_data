# This script exist to run QAQC on the litterfall data by year. This will help to check
# to see if all data is accounted for and what if any are outlying points. 

library(ggplot2)
library(dplyr)
library(reshape2)
library(ggmap)
library(maps)
library(mapdata)

setwd("~/Desktop/BEMP/bemp_to_r/litterfall/")

litterfall <- read.csv("./data/raw/litterfall_BEMP_to_2017.txt",sep="\t",
                       na.strings='.')

###

head(litterfall)

### Provides information on how R is reading in your data.
str(litterfall)

### Summarizes all the data in the data frame.
summary(litterfall)

### Check to see if all the sites and months are in the data set. 
sort(unique(litterfall$site))

sort(unique(litterfall$month))

###

litterfall$Date <- as.Date( paste( litterfall$Month , litterfall$Day ,litterfall$Year, sep = "." )
                             , format = "%m.%d.%Y" )

litterfall$native <- (litterfall$cw+litterfall$will+litterfall$seep+
                        litterfall$nmol+litterfall$indbu)

litterfall$exotic <- (litterfall$sc + litterfall$ro + litterfall$elm)

litterfall$total_npp <- (litterfall$native + litterfall$exotic)

litterfall$n_e_diff <- (litterfall$native - litterfall$exotic)

### Visual data checks

overview <- ggplot(data = litterfall, aes(month, total_npp)) +
  geom_point()+
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.background = element_blank()
    ,axis.text.x  = element_text(angle=90, vjust=0.5, size=8)
  )
overview

# Check for outliers based on the plot 
subset(litterfall, total_npp > 50)
check_total_npp <-subset(litterfall, total_npp > 50)
check_total_npp$ID

### View the plot by site. 
overview + facet_wrap(~site)

# This plots the native and exotic litterfall data. 

natives_exotics <- ggplot(data = litterfall, aes(exotic, native)) +
  geom_point()+
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.background = element_blank()
    ,axis.text.x  = element_text(angle=90, vjust=0.5, size=8)
  )
natives_exotics

### Here you can check for outliers.
subset(litterfall, exotic > 30)

subset(litterfall, native > 60)

### Check the total litterfall by month, tub, and site. When you export as a pdf
# it needs to be at least 24 x 24 inches. 
month_total <- ggplot(data = litterfall, aes(month, total_npp)) +
  geom_point()+
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.background = element_blank()
    ,axis.text.x  = element_text(angle=90, vjust=0.5, size=8)
  )
month_total  + facet_grid(site ~ tub)

###
