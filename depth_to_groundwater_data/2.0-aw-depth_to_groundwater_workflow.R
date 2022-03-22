library(ggplot2)
library(ggthemes)
library(tidyverse)
library(viridis)

# Working directory should be set using Session -> Set Working Directory. Not hard coded. 
# Better practices suggest your file structure look like this:
# .
# └── Project name/
#   ├── data/
#   │   ├── external
#   │   ├── interim
#   │   ├── processed
#   │   └── raw
#   ├── docs
#   ├── models
#   └── reports/
#       ├── images
#       └── graphs

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
depth_to_gw_processed <- read_csv("./data/processed/bemp_depth_to_groundwater_riverflow_to_current.csv",
                                  na = c('.','-999','NA'))
depth_to_gw_processed

# Boxplot of monthly data by year to look at the changes in the mean and variance.
depth_to_gw_processed %>% ggplot(., aes(x=as.factor(Year), y=-mean_depth_gw)) + 
  geom_boxplot()+
  xlab("Year") + ylab("Mean depth to groundwater (cm)") +
  geom_hline(aes(yintercept= -300), colour= 'blue')

# Boxplot of monthly data by year to look at the changes in the mean and variance.
depth_to_gw_processed %>% filter(`Site number` > 0 & `Site number` <= 26 ) %>% 
  filter(Year != 2022) %>% 
  ggplot(., aes(x=as.factor(Year), y=-mean_depth_gw)) + 
  geom_boxplot()+
  xlab("Year") + ylab("Mean depth to groundwater (cm)") +
  geom_hline(aes(yintercept= -300), colour= 'blue')+
  ggtitle("Mean depth to groundwater for sites with 10+ years of data")

#
depth_to_gw_processed %>% filter(`Site number` > 0 & `Site number` <= 26 ) %>% 
  filter(Year != 2022 & Reaches != "Cochiti Reach") %>% 
  ggplot(., aes(x=as.factor(Reaches), y=-mean_depth_gw)) + 
  geom_boxplot()+
  xlab("Year") + ylab("Mean depth to groundwater (cm)") +
  geom_hline(aes(yintercept= -300, linetype = "Mature cottonwood root threshold"), colour= 'blue')+
  ggtitle("Mean depth to groundwater for sites with 10+ years of data")

#
ten_years_of_data <- depth_to_gw_processed %>% filter(`Site number` > 0 & `Site number` <= 26 ) %>% 
  filter(Year != 2022)
ten_years_of_data

#
boxplot_outliers <- subset(ten_years_of_data, ten_years_of_data$mean_depth_gw %in% 
         boxplot(ten_years_of_data$mean_depth_gw ~ ten_years_of_data$Year)$out)
boxplot_outliers

outliers_for_plotting <- boxplot_outliers %>% group_by(Year) %>% 
  summarise("Outlier count" = n())
outliers_for_plotting

outliers_for_plotting %>% ggplot(., aes(x=Year, y=`Outlier count`)) + geom_point() +
  xlab("Year") + ylab("Number of outliers from the boxplot")


# Sometimes we want to know which months and for how long a site was flooded. Looking for well readings
# that are zero or negative numbers. 

flooded_wells <- depth_to_gw_processed %>% select(Year, Month, Day, `Site number`, `Site name`, north_depth, east_depth,
                                 west_depth, center_depth, south_depth) %>% 
  filter(north_depth <= 0| east_depth <= 0| west_depth <= 0 | center_depth <= 0 |
                                   south_depth <= 0) %>% 
  filter(Year > 2015 & Year < 2022)

flooded_wells

write.table(flooded_wells, "./data/processed/flooded_wells_2016_2021.csv", sep=",", 
            quote = TRUE, na = ".")


# Annual sum data wrangling
annual_groundwater <- depth_to_gw_processed %>% select(Year, Month, Day, `Site number`, `Site name`, mean_depth_gw,
                                                       Latitude, Longitude, Reaches) %>% 
  group_by(Year, `Site number`, `Site name`, Latitude, Longitude) %>% 
  summarise(annual_mean_depth_to_groundwater = mean(mean_depth_gw, na.rm = TRUE))

annual_groundwater

# Write out the mean annual depth to groundwater
write_csv(annual_groundwater, "./data/processed/mean_annual_depth_to_groundwater.csv", na = ".")

# Just plotting the annual mean depth to groundwater over time.


