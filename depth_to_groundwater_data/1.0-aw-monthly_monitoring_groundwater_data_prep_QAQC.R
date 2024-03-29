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
              ,panel.border = element_blank()
              ,axis.text.x  = element_text(angle=90, vjust=0.5, size=8)
              ,axis.ticks = element_blank()
            ))

# Read in the raw monthly well data.
well_data <- read_csv("./data/raw/allmmdata 03-29-2023.csv",
                      na = c('.','-999','-999.0'))
well_data

# Was the data read in  correctly. 
summary(well_data)

# Wrangle dates
well_data$Date <- as.Date( paste( well_data$Month, well_data$Day,
                                  well_data$Year,
                                       sep = "." ), format = "%m.%d.%Y" )
well_data

# Check all the column names
colnames(well_data)

# This removes columns. Mostly to clean up the data for export. And trims off the latest year.
# Typically we report on the previous year. 
well_data_cleaned <-  well_data %>% filter(Year < 2023) %>% select(-c(`Case Height Notes`,
                                              `comments - litterfall related`,
                                              `Data qualifier, USGS`, `Outreach: Student`,`Outreach: Adults (no staff)`,
                                              `comments - water related`, `QA/QC and plotting`
                                              ))
well_data_cleaned

# This subtracts the well casing heights to get actual depth to groundwater.
depth_to_gw <- well_data_cleaned %>% mutate("north_groundwater_depth_cm" = `North well` - `North Case Height`,
                                            "east_groundwater_depth_cm" = `East well` - `East Case Height`,
                                            "center_groundwater_depth_cm" = `Center well` - `Center Case Height`,
                                            "south_groundwater_depth_cm" = `South well` - `South Case Height`,
                                            "west_groundwater_depth_cm" = `West well` - `West Case Height`)
depth_to_gw

# Does a mean across the rows and adds a column for mean depth to groundwater.
depth_to_gw <- depth_to_gw %>%
  mutate("mean_depth_to_groundwater_cm"=rowMeans(.[ , c("north_groundwater_depth_cm",
                                       "east_groundwater_depth_cm",
                                       "center_groundwater_depth_cm",
                                       "south_groundwater_depth_cm",
                                       "west_groundwater_depth_cm")], na.rm=TRUE))
depth_to_gw
colnames(depth_to_gw)

# Lat/long for each site
sites <- read_csv("./data/raw/BEMP_site_locations.csv", na = '.')
sites


# Merge commands are tricky. If you merge the incorrect way you will duplicate all your data!
# Always check your tibble/dataframe size. If it blows up or is to small, then you merged incorrectly. 
depth_to_gw_site_info <- inner_join(depth_to_gw, sites, by="Site number")
depth_to_gw_site_info

colnames(depth_to_gw_site_info)

# Three sites are removed due to the proprietary nature of the data. 
depth_to_gw_clean <- depth_to_gw_site_info %>% filter(`Site number` !=5 &
                                                        `Site number` !=9 & `Site number` != 32)
depth_to_gw_clean

# Check to make sure all site numbers are there. 
unique(depth_to_gw_clean$`Site number`)
colnames(depth_to_gw_clean)
unique(depth_to_gw_clean_tnames$site_name)

depth_to_gw_clean_tnames <- depth_to_gw_clean %>% select_all(~gsub("\\s+|\\.", "_", .)) %>% 
  select_all(tolower) 
depth_to_gw_clean_tnames

write.table(depth_to_gw_clean, "./data/processed/bemp_monthly_depth_to_groundwater_current.csv", sep=",",
            row.names = FALSE, quote = TRUE, na=".")


### Need to update the data set with the current USGS stream flow gauges. There is a
# USGS stream gauge R script that needs to be run first before all this. 

# Pre-2018 data have the river flow already attached.
pre_2018 <- depth_to_gw_clean %>% 
  filter(Year < 2018)
pre_2018

# From 2018 on the river flow is not updated
post_2017 <- depth_to_gw_clean %>% 
  filter(Year > 2017) %>% 
  select(-`Discharge (cfs), USGS`)

post_2017
colnames(post_2017)

#
usgs_river_flow <-  read_csv("data/external/usgs_gauges_near_bemp_sites.csv", na = ".")
usgs_river_flow
tail(usgs_river_flow)

usgs_flow_retitled <- usgs_river_flow %>% 
  rename('Discharge (cfs), USGS' = X_00060_00003) %>% 
  mutate(`Current USGS gauge` = as.double(`current usgs gauge`))
usgs_flow_retitled

depth_to_gw_river_flow_2018 <-inner_join(post_2017, usgs_flow_retitled, by=c('Date', 'Current USGS gauge'))
depth_to_gw_river_flow_2018

depth_to_gw_riverflow <- bind_rows(pre_2018, depth_to_gw_river_flow_2018)
                               
depth_to_gw_riverflow

# Did the merge work correctly?
summary(depth_to_gw_riverflow$Year)
summary(depth_to_gw_riverflow$`Mean depth to groundwater (cm)`)

depth_to_gw_riverflow %>% select(`Discharge (cfs), USGS`, Year) %>% tail(n = 20)

depth_to_gw_riverflow %>% ggplot(., aes(x=`Discharge (cfs), USGS`, 
                                        y=-`Mean depth to groundwater (cm)`)) +
  geom_point()
# Should be a hot mess of non-linear dots.

# This writes out the data that is pushed up to github. 
write.table(depth_to_gw_riverflow, "./data/processed/bemp_monthly_depth_to_groundwater_riverflow_to_current.csv", sep=",",
            row.names = FALSE, quote = TRUE, na = ".")


unique(depth_to_gw_clean$Year)
# Look at all sites.
depth_to_gw_clean %>% filter(`Site name`!= "Santo Domingo" & `Site name` != "River Realignment") %>%
  ggplot(., aes(x=Date, y=-(`Mean depth to groundwater (cm)`))) + geom_line() +
  facet_wrap(~reorder(`Site name`, -Latitude), ncol = 4) +
  xlab("Year") + ylab("Monthyl mean depth to groundwater (cm)")

# Look at one site and all wells.
depth_to_gw_clean %>% filter(`Site number` == 1) %>% 
  filter(Year > 2018) %>% 
  select(date, "North groundwater depth (cm)":"West groundwater depth (cm)") %>% 
  pivot_longer(!date, names_to = "wells", values_to = "depth") %>% 
  ggplot(aes(x=date, y= -depth)) + geom_line(aes(color=wells))+
  scale_color_viridis(discrete = TRUE, option = "D") +
  #ggtitle("Hispanic Cultural Center - BEMP Site 8")+
  xlab("Date (monthly)") + ylab("Depth to groundwater")

# Visual QA/QC - you really need to look at each site with all wells to check for outliers. 
# Outlier detection will fail given the high variability of our river. 

depth_to_gw_clean %>% filter(`Site number` == 12) %>% 
  filter(Year > 2020 ) %>% 
  select(date, Year, Month, "North groundwater depth (cm)":"West groundwater depth (cm)") %>% 
  pivot_longer(!date:Month, names_to = "wells", values_to = "depth") %>% 
  ggplot(., aes(x=date, y= -depth)) + geom_line(aes(color=wells)) +
  geom_point() + geom_text(aes(label=paste("(",date,",",wells,",",Month,")"))) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  xlab("Date (monthly)") + ylab("Depth to groundwater")

# We also need to compare each well at a given site relative to center to check if any 
# relationships flip. 




