library(ggplot2)
library(ggthemes)
library(tidyverse)
library(dplyr)
library(viridis)

# This sets a global theme for all my plots. 
theme_set(theme_bw() +
            theme(
              plot.background = element_blank()
              ,panel.grid.major = element_blank()
              ,panel.grid.minor = element_blank()
              ,panel.background = element_blank()
              ,panel.border = element_blank()
              ,axis.text.x  = element_text(angle=90, vjust=0.5, size=8)
              ,axis.ticks = element_blank()
            ))

# Points at my working directory
setwd("~/Documents/BEMP/bemp_to_r/precip/")

# Reads in the raw data.
precip <- read.csv("./data/raw/bemp_precip_data.txt", sep="\t",
                  na.strings='.')

# Quick check to make sure everything looks ok. 
head(precip)
str(precip)
summary(precip)

# Lat/long for each site
sites <- read.csv("./data/raw/site_location.txt",sep="\t",
                  na.strings='.')
as_tibble(sites)


# Merge commands are tricking. If you merge the incorrect way you will duplicate all your data!
# Always check your tibble/dataframe size. If it blows up or is to small then you merged incorrectly. 
precip_site <- merge(precip, sites, by="site")
as_tibble(precip_site)

# Writes out the data to a csv.
write.table(precip_site, "bemp_precipitation_monthly_data_1997_to_2017.csv", sep=",",
            row.names = FALSE, quote = FALSE)

# Writes out the data to the R binary format. 
saveRDS(precip_site,"FOR_R_bemp_precipitation_monthly_data_1997_2017.rds")

# Series of plots to check the data.

# Annual summary
annual_precip <- group_by(precip_site, site, year, month)
head(annual_precip)
summary(annual_precip)

# Select a single site so it's easier to see. 
annual_precip <- filter(annual_precip, site == 14)
annual_precip

summary(annual_sum_precip)
str(annual_sum_precip)
head(annual_sum_precip)

# Shows both the open precip points with diameter scaled by the canopy precip. 

ts <- ggplot(data = annual_precip,
             aes(month, ppt_open)) +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))+
  geom_point()+
  geom_line() +
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.background = element_blank()
    ,axis.text.x  = element_text(angle=90, vjust=0.5, size=8)
  )

ts + facet_wrap(~year)

###
ggplot(data = annual_precip,
       aes(month, year)) +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))+
  scale_y_continuous(breaks=c(unique(annual_precip$year)))+
  geom_point(aes(size=ppt_open, color =ppt_open, alpha=ppt_open))+
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.background = element_blank()
    ,axis.text.x  = element_text(angle=90, vjust=0.5, size=8)
  )





