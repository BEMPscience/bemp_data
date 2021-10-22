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
well_data <- read_csv("./data/raw/allmmdata_depth_to_groundwater.csv", na = c('.','-999'))
well_data

# Was the data read in  correctly. 
summary(well_data)

# Wranlge dates
well_data$date <- as.Date( paste( well_data$month, well_data$day,
                                  well_data$year,
                                       sep = "." ), format = "%m.%d.%Y" )
well_data

# This removes columns. Mostly to clean up the data for export. 
well_data_cleaned <-  well_data %>% select(-c(CaseHeightNotes, ...31, ...32, ...33, ...34, ...35, ...36,
                                              `comments - litterfall related`,
                                              `Data qualifier, USGS`, `Outreach: Student`,`Outreach: Adults (no staff)`,
                                              `comments - water related`, `QA/QC and plotting`
                                              ))
well_data_cleaned

# This subtracts the well casing heights to get actual depth to groundwater.
depth_to_gw <- well_data_cleaned %>% mutate(north_depth = northwell - NorthCaseHeight,
                                    east_depth = eastwell - EastCaseHeight,
                                    center_depth = centerwell - CenterCaseHeight,
                                    south_depth = southwell - SouthCaseHeight,
                                    west_depth = westwell - WestCaseHeight)
depth_to_gw

# Does a mean across the rows and adds a column for mean depth to groundwater.
depth_to_gw <- depth_to_gw %>%
  mutate(mean_depth_gw=rowMeans(.[ , c("north_depth", "east_depth","center_depth",
                                       "south_depth", "west_depth")], na.rm=TRUE))
depth_to_gw

# Lat/long for each site
sites <- read_csv("./data/raw/site_location.csv", na = '.')
sites


# Merge commands are tricky. If you merge the incorrect way you will duplicate all your data!
# Always check your tibble/dataframe size. If it blows up or is to small, then you merged incorrectly. 
depth_to_gw_site_info <- inner_join(depth_to_gw, sites, by="site")
depth_to_gw_site_info


# Three sites are removed due to the proprietary nature of the data. 
depth_to_gw_clean <- depth_to_gw_site_info %>% filter(site !=5 &
                                                               site !=9 & site != 32)
depth_to_gw_clean

# 
unique(depth_to_gw_clean$site)

### Need to update the dataset with the current USGS stream flow gauges. 

# Pre-2018 data have the riverflow already attached.
pre_2018 <- depth_to_gw_clean %>% 
  filter(year < 2018)
pre_2018

# From 2018 on the river flow is not updated
post_2017 <- depth_to_gw_clean %>% 
  filter(year > 2017) %>% 
  select(-`Discharge (cfs), USGS`)

post_2017
summary(post_2017)

#
usgs_river_flow <-  read_csv("./data/external/usgs_gauges_near_bemp_sites.csv", na = ".")
usgs_river_flow

usgs_flow_retitled <- usgs_river_flow %>% 
  rename('Discharge (cfs), USGS' = X_00060_00003) %>% 
  mutate(`current usgs gauge` = as.double(`current usgs gauge`))
usgs_flow_retitled

depth_to_gw_river_flow_2018 <-inner_join(post_2017, usgs_flow_retitled, by=c('date', 'current usgs gauge'))
depth_to_gw_river_flow_2018

depth_to_gw_riverflow <- bind_rows(pre_2018, depth_to_gw_river_flow_2018)
                               
depth_to_gw_riverflow

# Did the merge work correctly?
summary(depth_to_gw_riverflow$year)
summary(depth_to_gw_riverflow$mean_depth_gw)

depth_to_gw_riverflow %>% ggplot(., aes(x=`Discharge (cfs), USGS`, y=-mean_depth_gw)) + geom_point()
# Should be a hot mess of non-linear dots.

depth_to_gw_riverflow

# This writes out the data that is pushed up to github. 
write.table(depth_to_gw_riverflow, "./data/processed/bemp_depth_to_groundwater_riverflow_to_current.csv", sep=",",
            row.names = FALSE, quote = TRUE)

# Subsetting to plot different sites
depth_to_gw_riverflow %>% filter(site == 25) %>% 
  ggplot(aes(x=date, y= -mean_depth_gw)) + geom_point(aes(color=site_name)) + geom_line() +
  scale_color_viridis(discrete = TRUE, option = "D") +
  #ylim(0, 8000)+
  xlab("Date (monthly)") + ylab("Depth to groundwater")