library(tidyverse)
library(ggplot2)
library(viridis)
library(lubridate)


# First we set some ggplot2 options so the plot aren't eye horrors. This will save space
# in the code.
theme_set(  theme_bw() +
              theme(
                plot.background = element_blank()
                ,panel.grid.major = element_blank()
                ,panel.grid.minor = element_blank()
                ,panel.background = element_blank()
                ,axis.text.x  = element_text(angle=90, vjust=0.5, size=8)
              ))


setwd("~/Documents/BEMP/bemp_to_r/temp_loggers/")

# Reads in the csv file dumped out from Excel. 
temp_data <- read_delim("./data/raw/TempAllData2018_2019.csv", delim=",",
                       na=c('NA','.','#VALUE!'))
# Are the data types id correctly. Note that date is not. 
str(temp_data)

# Record the number of unique sites
unique(temp_data$site.name)

summary(temp_data)
# Record the number of NA's for each temperature category.

head(temp_data)

# Changes the date to something readable by R. 
temp_data$proper_date <- mdy(temp_data$date)
temp_data

### QAQC

# Number of data points per site.
temp_data %>% group_by(site.name) %>% tally()

# Counts up the number of NA's per site. Record these numbers for each temp column.
temp_data %>% 
  group_by(site.name) %>% 
  summarise(sumNA = sum(is.na(open_sub_C)))

# Plot the raw data to see if anything stands out. The temperature across sites
# should track fairly closely with some differences from north to south. If any
# site stands out as wildly different, flag it and check the original data entry.

# Canopy rain gauge above ground temperature.
temp_data %>%
  ggplot(aes(x=proper_date, y=canopy_C)) +  
  geom_line(aes(color=site.name)) + scale_colour_viridis(discrete = TRUE) +
  #geom_point() +
  labs(x = "Date",
       y = expression('Raw Canopy ⁰C'))

# This should look like a hairly catepillar. Are the winter times cooler than the
# summer time? 

# Canopy rain gauge subsurface tempature.
temp_data %>%
  ggplot(aes(x=proper_date, y=canopy_sub_C)) +  
  geom_line(aes(color=site.name)) + scale_colour_viridis(discrete = TRUE) +
  #geom_point() +
  labs(x = "Date",
       y = expression('Raw Canopy Subsurface ⁰C'))

# Open rain gauge subsurface tempature. 
temp_data %>%
  ggplot(aes(x=proper_date, y=open_sub_C)) +  
  geom_line(aes(color=site.name)) + scale_colour_viridis(discrete = TRUE) +
  #geom_point() +
  labs(x = "Date",
       y = expression('Raw Open Subsurface ⁰C'))

# Function 
# Observation is not an outlier TURE or FALSE based off of the z-score.
isnt_out_z <- function(x, thres = 3, na.rm = TRUE) {
  abs(x - mean(x, na.rm = na.rm)) <= thres * sd(x, na.rm = na.rm)
}
# Count up the number of FALSE statements. This shows you how many points are
# more then three times the SD based on the z-score transformation. 

summary(isnt_out_z(temp_data$canopy_C))
summary(isnt_out_z(temp_data$canopy_sub_C))
summary(isnt_out_z(temp_data$open_sub_C))

# 
# Canopy rain gauge above ground temperature faceted by site. 
temp_data %>%
  ggplot(aes(x=proper_date, y=canopy_C)) +  
  geom_line(aes(color=site.name)) + scale_colour_viridis(discrete = TRUE) +
  #geom_point() +
  labs(x = "Date",
       y = expression('Raw Canopy ⁰C'))

# Look at the mean, max, and min by day 
temp_daily_canopy_summary <- temp_data %>% group_by(proper_date, site.name) %>% 
  select(date, site.name, canopy_C) %>% 
  summarise(mean_daily_temp = mean(canopy_C, na.rm = FALSE),
            max_daily_temp = max(canopy_C, na.rm = FALSE),
            min_daily_temp = min(canopy_C, na.rm = FALSE))

temp_daily_canopy_summary

temp_daily_canopy_summary %>%
  ggplot(aes(x=proper_date, y=mean_daily_temp)) +  
  geom_line(aes(color=site.name)) + scale_colour_viridis(discrete = TRUE) +
  #geom_point() +
  labs(x = "Date",
       y = expression('Mean Daily Canopy ⁰C'))

temp_daily_canopy_summary %>%
  ggplot(aes(x=proper_date, y=max_daily_temp)) +  
  geom_line(aes(color=site.name)) + scale_colour_viridis(discrete = TRUE) +
  #geom_point() +
  labs(x = "Date",
       y = expression('Max Daily Canopy ⁰C'))

temp_daily_canopy_summary %>%
  ggplot(aes(x=proper_date, y=min_daily_temp)) +  
  geom_line(aes(color=site.name)) + scale_colour_viridis(discrete = TRUE) +
  #geom_point() +
  labs(x = "Date",
       y = expression('Min Daily Canopy ⁰C'))

###
temp_daily_canopy_sub_summary <- temp_data %>% group_by(proper_date, site.name) %>% 
  select(proper_date, site.name, canopy_sub_C) %>% 
  summarise(mean_daily_temp_sub = mean(canopy_sub_C, na.rm = FALSE),
            max_daily_temp_sub = max(canopy_sub_C, na.rm = FALSE),
            min_daily_temp_sub = min(canopy_sub_C, na.rm = FALSE))

temp_daily_canopy_sub_summary %>%
  ggplot(aes(x=proper_date, y=mean_daily_temp_sub)) +  
  geom_line(aes(color=site.name)) + scale_colour_viridis(discrete = TRUE) +
  #geom_point() +
  labs(x = "Date",
       y = expression('Mean Daily Subsurface Canopy ⁰C'))

temp_daily_canopy_sub_summary %>%
  ggplot(aes(x=proper_date, y=max_daily_temp_sub)) +  
  geom_line(aes(color=site.name)) + scale_colour_viridis(discrete = TRUE) +
  #geom_point() +
  labs(x = "Date",
       y = expression('Max Daily Subsurface Canopy ⁰C'))

temp_daily_canopy_sub_summary %>%
  ggplot(aes(x=proper_date, y=min_daily_temp_sub)) +  
  geom_line(aes(color=site.name)) + scale_colour_viridis(discrete = TRUE) +
  #geom_point() +
  labs(x = "Date",
       y = expression('Min Daily Subsurface Canopy ⁰C'))

###
temp_daily_open_sub_summary <- temp_data %>% group_by(proper_date, site.name) %>% 
select(proper_date, site.name, open_sub_C) %>% 
  summarise(mean_daily_temp_open_sub = mean(open_sub_C, na.rm = FALSE),
            max_daily_temp_open_sub = max(open_sub_C, na.rm = FALSE),
            min_daily_temp_open_sub = min(open_sub_C, na.rm = FALSE))

temp_daily_open_sub_summary %>%
  ggplot(aes(x=proper_date, y=mean_daily_temp_open_sub)) +  
  geom_line(aes(color=site.name)) + scale_colour_viridis(discrete = TRUE) +
  #geom_point() +
  labs(x = "Date",
       y = expression('Mean Daily Subsurface Open ⁰C'))

temp_daily_canopy_sub_summary %>%
  ggplot(aes(x=proper_date, y=max_daily_temp_sub)) +  
  geom_line(aes(color=site.name)) + scale_colour_viridis(discrete = TRUE) +
  #geom_point() +
  labs(x = "Date",
       y = expression('Max Daily Subsurface Canopy ⁰C'))

temp_daily_canopy_sub_summary %>%
  ggplot(aes(x=proper_date, y=min_daily_temp_sub)) +  
  geom_line(aes(color=site.name)) + scale_colour_viridis(discrete = TRUE) +
  #geom_point() +
  labs(x = "Date",
       y = expression('Min Daily Subsurface Canopy ⁰C'))
