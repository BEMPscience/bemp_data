library(ggplot2)
library(ggthemes)
library(tidyverse)
library(viridis)
library(zoo)

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
well_data <- read_csv("data/raw/1.0 allmmdata.csv",
                      na = c('.','-999','-999.0','NA',' '))
well_data
colnames(well_data)

# Was the data read in  correctly. 
summary(well_data)
summary(well_data$Year)

# Wrangle dates
well_data$`Date ym` <- as.yearmon(paste(well_data$Year, well_data$Month), "%Y %m")
well_data$`Date ym`

well_data$Date <- as.Date( paste(well_data$Month, well_data$Day,
                                       well_data$Year,
                                    sep = "." ), format = "%m.%d.%Y" )

# Check all the column names
colnames(well_data)

# This removes columns. Mostly to clean up the data for export. And trims off the latest year.
# Typically we report on the previous year. 
well_data_cleaned <-  well_data %>% select(-c(`Case height notes`,
                                              `comments litterfall related`,
                                              `Data qualifier USGS`, `Outreach K12 Students`,`Outreach Adults UNM Interns not staff`,
                                              `comments water related`, `QA QC and plotting`,
                                              `checked against datasheet`,`gauge type`,
                                              `Precipitation canopy mm`,`Precipitation open mm`,
                                              `Temperature ppt canopy`,`Temperature ppt open`
                                              ))
well_data_cleaned
colnames(well_data_cleaned)

# This subtracts the well casing heights to get actual depth to groundwater.
# Flips the depth to groundwater so it's negative
depth_to_gw <- well_data_cleaned %>% mutate("North groundwater depth cm" = -1 * (`North well cm`) + `North Case Height cm`,
                                            "East groundwater depth cm" = -1 * (`East well cm`) + `East Case Height cm`,
                                            "Center groundwater depth cm" = -1 * (`Center well cm`) + `Center Case Height cm`,
                                            "South groundwater depth cm" = -1 * (`South well cm`) + `South Case Height cm`,
                                            "West groundwater depth cm" = -1 * (`West well cm`) + `West Case Height cm`)
colnames(depth_to_gw)
summary(depth_to_gw$`Center groundwater depth cm`)

# Does a mean across the rows and adds a column for mean depth to groundwater.

depth_to_gw <- depth_to_gw %>%
  mutate("Mean depth to groundwater cm"= rowMeans(.[ , c("North groundwater depth cm",
                                       "East groundwater depth cm",
                                       "Center groundwater depth cm",
                                       "South groundwater depth cm",
                                       "West groundwater depth cm")], na.rm=TRUE))
depth_to_gw
colnames(depth_to_gw)

summary(depth_to_gw$`Mean depth to groundwater cm`)

# Lat/long for each site
sites <- read_csv("./data/raw/BEMP_site_locations.csv", na = '.')
sites
unique(sites$`Site name`)

# Merge commands are tricky. If you merge the incorrect way you will duplicate all your data!
# Always check your tibble/dataframe size. If it blows up or is to small, then you merged incorrectly. 
depth_to_gw_site_info <- left_join(depth_to_gw, sites, by="Site number")
depth_to_gw_site_info
colnames(depth_to_gw_site_info)

# Three sites are removed due to the proprietary nature of the data. 
depth_to_gw_clean <- depth_to_gw_site_info %>% filter(`Site number` !=5 &
                                                        `Site number` !=9 & `Site number` != 32 &
                                                        `Site number` != 24 &
                                                        Year < 2026)
depth_to_gw_clean
unique(depth_to_gw_clean$Year)

# Check to make sure all site numbers are there. 
unique(depth_to_gw_clean$`Site name`)
unique(depth_to_gw_clean$Year)
colnames(depth_to_gw_clean)

#depth_to_gw_clean_tnames <- depth_to_gw_clean %>% select_all(~gsub("\\s+|\\.", "_", .)) %>% 
#  select_all(tolower) 
#depth_to_gw_clean_tnames

write.table(depth_to_gw_clean, "./data/processed/bemp_monthly_depth_to_groundwater_2025-08-21.csv", sep=",",
            row.names = FALSE, quote = TRUE, na=".")

unique(depth_to_gw_clean$Year)

depth_to_gw_clean

print(depth_to_gw_clean, n=1, width = Inf)
summary(depth_to_gw_clean$`Mean depth to groundwater cm`)

# Here you will need to run a quick report.
# Open groundwater-qaqc-report.qmd and run that, email to the science director and
# program director. 

# Look at one site
depth_to_gw_clean %>%
  filter(`Site name`=="Alameda") %>% 
  ggplot(., aes(x=`Date ym`, y=`Mean depth to groundwater cm`)) + 
  geom_line(alpha=0.5) +
  # Highlight the current year of data in blue
  # geom_line(data=subset(depth_to_gw_clean, Year == 2023), colour="blue")+
  xlab("Year") + ylab("Mean monthly mean depth to groundwater (cm)")

# Look at all sites.
depth_to_gw_clean %>% filter( `Site name` != "River Realignment") %>% 
  ggplot(., aes(x=`Date ym`, y=`Mean depth to groundwater cm`)) + 
  geom_line(alpha=0.5) +
  facet_wrap(~reorder(`Site name`, -Latitude), ncol = 4) +
  # Highlight the current year of data in blue
  geom_line(data=subset(depth_to_gw_clean, Year == 2025), colour="blue")+
  xlab("Year") + ylab("Mean monthly mean depth to groundwater (cm)")

# Look at one site and all wells.
colnames(depth_to_gw_clean)

# 
depth_to_gw_clean %>% filter(`Site number` == 1) %>% 
  select(`Date`, "North groundwater depth cm":"West groundwater depth cm") %>% 
  pivot_longer(!`Date`, names_to = "wells", values_to = "depth") %>% 
  ggplot(aes(x=`Date`, y=depth)) + geom_line(aes(color=wells, linewidth=0.1))+
  geom_point(size=1) +
  #scale_color_viridis(discrete = TRUE, option = "D") +
  #scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))+
  xlab("Date") + ylab("Depth to groundwater cm") +
  theme(axis.text.x = element_text(size = 16)) +
  theme(axis.text.y = element_text(size = 16)) +
  theme(axis.title = element_text(size = 16)) +
  scale_colour_colorblind()

depth_to_gw_clean %>% filter(`Site number` == 1) %>% 
  filter(Year == 2025) %>% 
  select(Month, "North groundwater depth cm":"West groundwater depth cm") %>% 
  pivot_longer(!Month, names_to = "wells", values_to = "depth") %>% 
  ggplot(aes(x=Month, y=depth)) + geom_line(aes(color=wells, linewidth=0.5))+
  geom_point(size=2) +
  #scale_color_viridis(discrete = TRUE, option = "D") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))+
  xlab("Month") + ylab("Depth to groundwater cm") +
  theme(axis.text.x = element_text(size = 16)) +
  theme(axis.text.y = element_text(size = 16)) +
  theme(axis.title = element_text(size = 16)) +
  scale_colour_colorblind()


# Visual QA/QC - you really need to look at each site with all wells to check for outliers. 
# Outlier detection will fail given the high variability of our river. 

depth_to_gw_clean %>% filter(`Site number` == 2)%>% 
  filter(Year > 2023) %>% 
  select(`Date ym`, Year, Month, "North groundwater depth cm":"West groundwater depth cm") %>% 
  pivot_longer(!`Date ym`:Month, names_to = "wells", values_to = "depth") %>% 
  ggplot(., aes(x=`Date ym`, y= depth)) + geom_line(aes(color=wells, size=0.1)) +
  geom_point() + geom_text(aes(label=paste("(",`Date ym`,",",wells,",",Month,")"))) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  xlab("Date (monthly)") + ylab("Depth to groundwater")

# We also need to compare each well at a given site relative to center to check if any 
# relationships flip. 

### Need to update the data set with the current USGS stream flow gauges. There is a
# USGS stream gauge R script that needs to be run first before all this. 

# Pre-2018 data have the river flow already attached.
pre_2018 <- depth_to_gw_clean %>% 
  filter(Year < 2018)
pre_2018

# From 2018 on the river flow is not updated
post_2017 <- depth_to_gw_clean %>% 
  filter(Year > 2017) %>% 
  select(-`Discharge cfs USGS`)

post_2017
colnames(post_2017)

#
usgs_river_flow <-  read_csv("./data/external/usgs_gauges_near_bemp_sites.csv",
                             na = ".")
usgs_river_flow
colnames(usgs_river_flow)
tail(usgs_river_flow)

usgs_flow_retitled <- usgs_river_flow %>% 
  rename('Discharge cfs USGS' = X_00060_00003) %>% 
  mutate(`Current usgs gauge` = as.double(`current usgs gauge`))
usgs_flow_retitled
colnames(usgs_flow_retitled)

depth_to_gw_river_flow_2018 <-inner_join(post_2017, usgs_flow_retitled, by=c('Date', 'Current usgs gauge'))
depth_to_gw_river_flow_2018
colnames(depth_to_gw_river_flow_2018)

depth_to_gw_riverflow <- bind_rows(pre_2018, depth_to_gw_river_flow_2018)
colnames(depth_to_gw_riverflow)                               
depth_to_gw_riverflow$`Discharge cfs USGS`

# Did the merge work correctly?
summary(depth_to_gw_riverflow$Year)
summary(depth_to_gw_riverflow$`Mean depth to groundwater cm`)
colnames(depth_to_gw_riverflow)

depth_to_gw_riverflow %>% select(`Discharge cfs USGS`, Year) %>% tail(n = 20)

depth_to_gw_riverflow %>% ggplot(., aes(x=`Discharge cfs USGS`, 
                                        y=`Mean depth to groundwater cm`)) +
  geom_point()
# Should be a hot mess of non-linear dots.

# This writes out the data that is pushed up to github. 
write.table(depth_to_gw_riverflow, "./data/processed/bemp_monthly_depth_to_groundwater_riverflow_to_current.csv", sep=",",
            row.names = FALSE, quote = TRUE, na = ".")

# Look at two sites response to riverflow
depth_to_gw_riverflow %>% filter(`Site name`=="Reynolds Forest" |
                                   `Site name` == "Reynolds Cleared") %>% 
  ggplot(., aes(x=`Discharge cfs USGS`, 
                                        y=`Mean depth to groundwater cm`)) +
  geom_point(aes(color=`Site name`))

# 
depth_to_gw_riverflow %>% filter(`Site name`=="Alameda" |
                                   `Site name` == "Rio Grande Nature Center" |
                                   `Site name` == "Reynolds Forest" |
                                   `Site name` == "Los Lunas" |
                                   `Site name` == "Bosque del Apache" |
                                   `Site name` == "Montano") %>% 
  ggplot(., aes(x=`Discharge cfs USGS`, 
                y=`Mean depth to groundwater cm`)) +
  geom_point(aes(color=`Site name`))

