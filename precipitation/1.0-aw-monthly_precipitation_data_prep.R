library(ggplot2)
library(ggthemes)
library(tidyverse)
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

# Reads in the raw data.
mm_data <- read_csv("./data/raw/well_depth_to_groundwater_monthly_monitoring_downloaded 2_07_2022.csv",
                  na ='.')

# Quick check to make sure everything looks ok. 
mm_data

# Drop columns from the monthly monitoring data we don't need
precip <- mm_data %>% 
  select(Year, Month, Day, `Site number`, `Precipitation canopy`, `Precipitation open`)

precip

# Lat/long for each site
sites <- read_csv("./data/raw/BEMP_site_locations.csv", na ='.')
sites



# Merge commands are tricking. If you merge the incorrect way you will duplicate all your data!
# Always check your tibble/dataframe size. If it blows up or is to small then you merged incorrectly. 
precip_site <- inner_join(precip, sites, by="Site number")
precip_site


# 
# BEMP doesn't know how to write their dates correctly so we have to wrangle.
precip_site$date <- as.Date( paste( precip_site$Month, precip_site$Day,
                                    precip_site$Year,
                                  sep = "." ), format = "%m.%d.%Y" )
precip_site <- as_tibble(precip_site)

precip_site

# Writes out the data to a csv.
write.table(precip_site, "./data/processed/bemp_precipitation_monthly_data.csv", sep=",",
            row.names = FALSE, quote = FALSE, na = ".")


#
precip_site

precip_site %>% filter(Year == 2021) %>% ggplot(., aes(x=as.factor(Reaches), y=`Precipitation open`)) + 
  geom_boxplot() + xlab("Year") + ylab("Precipitation (cm)") + 
  ggtitle("Monthly open precipitation")

precip_site %>% filter(Year < 2022) %>% ggplot(., aes(x=as.factor(Year), y=`Precipitation open`)) + 
  geom_boxplot() + xlab("Year") + ylab("Precipitation (cm)") + 
  ggtitle("Monthly open precipitation")

boxplot_outliers <- subset(precip_site, precip_site$`Precipitation open` %in% 
                             boxplot(precip_site$`Precipitation open` ~ precip_site$Year)$out)
boxplot_outliers

outliers_for_plotting <- boxplot_outliers %>% group_by(Year) %>% filter(Year < 2022) %>% 
  summarise("Outlier count" = n())
outliers_for_plotting

outliers_for_plotting %>% ggplot(., aes(x=Year, y=`Outlier count`)) + geom_point() +
  xlab("Year") + ylab("Number of outliers from the precipitation open boxplot")



#
precip_long <- precip_site %>% select(-c("current usgs gauge","nearest_usgs_gauge")) %>% 
  pivot_longer(cols = c("ppt canopy","ppt open"),
    names_to = "rain gauge location",
    values_to = "millimeters precipitation")

precip_long



#
precip_site %>% filter(`site number` == 33) %>% 
  filter(year > 2016) %>% 
  select(date, year, month, `ppt canopy`) %>% 
  pivot_longer(!date:month, names_to = "precip gauge", values_to = "mm") %>% 
  ggplot(., aes(x=date, y= mm)) + geom_line() +
  geom_point() + geom_text(aes(label=paste("(",date,",",month,")"))) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  xlab("Date (monthly)") + ylab("Precipatation (mm)")

#
precip_site %>%
  filter(year > 2016) %>% 
  select(date, year, month, site_lat, `site name`, `ppt canopy`) %>% 
  pivot_longer(!date:`site name`, names_to = "precip gauge", values_to = "mm") %>% 
  ggplot(., aes(x=date, y= mm)) + geom_line() +
  geom_point() + xlab("Date (monthly)") + ylab("Precipatation (mm)") +
  facet_wrap(~reorder(`site name`, -site_lat, ncol = 4))

# Precip over time with the latest year in color. 
precip_site %>%
  filter(`site name` != "Santo Domingo" | `site name` != "Ohkay Owingeh") %>% 
  select(date, year, month, site_lat, `site name`,`ppt canopy`) %>% 
  pivot_longer(!date:`site name`, names_to = "Gauge", values_to = "mm") %>% 
  ggplot(., aes(x=date, y= mm)) + geom_line(aes(color = (year > 2020))) +
  xlab("Date (monthly)") + ylab("Monthly precipitation (mm)") +
  facet_wrap(~reorder(`site name`, -site_lat),ncol = 6)

# Uses the precip_long as a visual QA/QC. Just the past five years of data are needed.
precip_long %>% 
  filter(`site name` != "Santo Domingo" | `site name` != "Ohkay Owingeh") %>% 
  filter( year > 2017) %>% 
  select(date, year, month, site_lat, `site name`, `rain gauge location`, `millimeters precipitation`) %>% 
  ggplot(., aes(x=date, y= `millimeters precipitation`)) + geom_line(aes(color =`rain gauge location`)) +
  xlab("Date (monthly)") + ylab("Monthly precipitation (mm)") +
  facet_wrap(~reorder(`site name`, -site_lat),ncol = 6)


