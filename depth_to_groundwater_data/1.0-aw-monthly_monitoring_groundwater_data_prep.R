library(ggplot2)
library(ggthemes)
library(tidyverse)
library(dplyr)
library(viridis)

# Points at my working folder.
setwd("~/Documents/BEMP/bemp_to_r/ground_water_r/")

# This sets a global theme for all my plots. 
theme_set(theme_bw() +
            theme(
              plot.background = element_blank()
              ,panel.grid.major = element_blank()
              ,panel.grid.minor = element_blank()
              ,panel.background = element_blank()
              ,axis.text.x  = element_text(angle=90, vjust=0.5, size=8)
            ))

# Read in the raw monthly well data.
well_data <- read.csv("./gw_qaqc/gw_data_to_2019", sep = "\t", na.strings = c('.','-999'),
                      quote="")

# Sanity check. Was the data read in  correctly. 
str(well_data)
head(well_data)
summary(well_data)

# BEMP doesn't know how to write their dates correctly so we have to wrangle.
well_data$Date <- as.Date( paste( well_data$month, well_data$day,
                                  well_data$year,
                                       sep = "." ), format = "%m.%d.%Y" )

# This subtracts the well casing heights to get actual depth to groundwater.
depth_to_gw <- well_data %>% mutate(north_depth = northwell - NorthCaseHeight,
                                    east_depth = eastwell - EastCaseHeight,
                                    center_depth = centerwell - CenterCaseHeight,
                                    south_depth = southwell - SouthCaseHeight,
                                    west_depth = westwell - WestCaseHeight)
summary(depth_to_gw)

depth_to_gw <- depth_to_gw %>%
  mutate(mean_depth_gw=rowMeans(.[ , c("north_depth", "east_depth","center_depth",
                                       "south_depth", "west_depth")], na.rm=TRUE))
summary(depth_to_gw)

# Lat/long for each site
sites <- read.csv("./site_location.txt",sep="\t",
                  na.strings='.')
as_tibble(sites)


# Merge commands are tricking. If you merge the incorrect way you will duplicate all your data!
# Always check your tibble/dataframe size. If it blows up or is to small then you merged incorrectly. 
depth_to_gw_site_info <- merge(depth_to_gw, sites, by="site")
as.tibble(depth_to_gw_site_info)

# Two sites are removed due to the proprietary nature of the data. 
twenty_years_depth_to_gw <- depth_to_gw_site_info %>% filter(year < 2018 & site !=5 &
                                                               site !=9)
summary(twenty_years_depth_to_gw)
unique(twenty_years_depth_to_gw$site)

# This writes out the data that is pushed up to github. 
write.table(twenty_years_depth_to_gw, "bemp_depth_to_groundwater_1997_to_2017.csv", sep=",",
            row.names = FALSE, quote = FALSE)

twenty_years_depth_to_gw %>% 
  ggplot(aes(x=as.factor(year), y= mean_depth_gw)) + geom_boxplot()

# Visual QAQC we just want the last five or so years of data.
# Here is a filter that makes that happen. 
site_5_years <- twenty_years_depth_to_gw %>% filter(year > 2013)
site_5_years

# Selecting just the data we want to look at. 
plot_data <- site_5_years %>% select(Date, site, north_depth, center_depth, east_depth, west_depth,
                        south_depth, mean_depth_gw) %>% 
  gather(well, depth, north_depth:south_depth)
as_tibble(plot_data)

# This plots the monthly data for the past five years. 
ggplot(data=plot_data, aes(x=Date, y=depth)) + geom_line(aes(color=well)) + geom_point() +
  facet_wrap(~site, ncol = 4)

# This is a histogram of all the data. 
ggplot(data = plot_data, aes(x = depth)) +
  geom_histogram(bins = 20, fill = "blue") +
  labs(x = "Depth to groundwater (cm)") +
  ggtitle("Histogram of Depth to Groundwater - 5 years")

# Boxplot of the depth to groundwater by site.
ggplot(data = plot_data) +
  geom_boxplot(aes(x = as.factor(site), y=-depth)) +
  labs(x = "BEMP site") +
  ggtitle("Boxplot of Depth to Groundwater - 5 years")

# Observation is not an outlier TURE or FALSE based off of the z-score.
isnt_out_z <- function(x, thres = 3, na.rm = TRUE) {
  abs(x - mean(x, na.rm = na.rm)) <= thres * sd(x, na.rm = na.rm)
}

# Counts the number of outliers.
summary(isnt_out_z(plot_data$depth))



