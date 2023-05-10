# This loads in the packages we need. 
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


# This reads in a csv. 
litterfall_raw <- read_csv("./data/raw/monthly monitoring litterfall downloaded 2023-03-15.csv",
                           na =c('NA','.','#VALUE!',' ') )

litterfall_raw
colnames(litterfall_raw)

# Check for completed current year of reporting.
litterfall_raw %>% filter(Year == 2022)
  

### Data transformations
litterfall_raw$Date <- as.Date( paste( litterfall_raw$Month,
                                                               litterfall_raw$Day,
                                                               litterfall_raw$Year,
                                       sep = "." ), format = "%m.%d.%Y" )

litterfall_raw

# Transform data to g/m^2
tub_area_function <- function(x) {round((x/0.13),2)} # liiterfall tubs are 0.13 square meters.

litterfall <- mutate_at(litterfall_raw, vars(cw:wood),
                        list(tub_area_function))
litterfall


# Lat/long for each site
sites <- read_csv("./data/raw/BEMP_site_locations.csv", na ='.')
sites

# Merge commands are tricking. If you merge the incorrect way you will duplicate all your data!
# Always check your tibble/dataframe size. If it blows up or is to small then you merged incorrectly. 
litterfall_site_info <- inner_join(litterfall, sites, by="Site number")

litterfall_site_info

# Check for negative values here
summary(litterfall_site_info$`totalwt-indwts`)

# Check to see how many rows have a total weight - individual weights > 0.3
litterfall_site_info %>% filter(`totalwt-indwts` > 0.3)

# Make sure to include on the years that are due, if this is for a report. 
litterfall_site_info <- litterfall_site_info %>% select(Year < 2022)

#
write.table(litterfall_site_info, "./data/processed/bemp_monthly_leaf_litterfall_by_tub_area.csv", sep=",", 
            quote = TRUE, na = ".")

# This does all the hard works of taking the mean of the data from the group_by variables.
monthly_litterfall_means <- litterfall_site_info %>% group_by(Date, `Site number`, `Site name`,
                                                              Latitude, Longitude, Reaches,
                                                Month, Year) %>% 
  summarise("monthly_cottonwood_leaf_litterfall_g_m2" = round(mean(cw, na.rm = TRUE),2),
            "monthly_willow_leaf_litterfall_g_m2" = round(mean(will, na.rm = TRUE),2),
            "monthly_russian_olive_leaf_litterfall_g_m2" = round(mean(ro, na.rm = TRUE),2),
            "monthly_saltcedar_leaf_litterfall_g_m2" = round(mean(sc, na.rm = TRUE),2),
            "monthly_elm_leaf_litterfall_g_m2" = round(mean(elm, na.rm = TRUE),2),
            "monthly_seep_willow_leaf_litterfall_g_m2" = round(mean(seep, na.rm = TRUE),2),
            "monthly_new_mexico_olive_leaf_litterfall_g_m2" = round(mean(nmol, na.rm = TRUE),2),
            "monthly_indigo_bush_leaf_litterfall_g_m2" = round(mean(indbu, na.rm = TRUE),2),
            "monthly_woodfall_g_m2" = round(mean(wood, na.rm = TRUE),2),
            "monthly_cottonwood_reproductive_litterfall_g_m2" = round(mean(repcw , na.rm = TRUE),2),
            "monthly_willow_reproductive_litterfall_g_m2" = round(mean(repwil, na.rm = TRUE),2),
            "monthly_russian_olive_reproductive_litterfall_g_m2" = round(mean(repruol, na.rm = TRUE),2),
            "monthly_elm_reproductive_litterfall_g_m2" = round(mean(repelm, na.rm = TRUE),2),
            "monthly_saltcedar_reproductive_litterfall_g_m2" = round(mean(repsc, na.rm = TRUE),2),
            "monthly_total_reproductive_litterfall_g_m2" = round(mean(totalrepro, na.rm = TRUE),2)
            )

monthly_litterfall_means
colnames(monthly_litterfall_means)

# Only up to certain years are reported out for the annual report. Check which years are
# due and remove all others, they will likely be incomplete. 
monthly_litterfall_means %>% filter(Year == 2022)

# Code for filtering our just one site and year.  
monthly_litterfall_means %>% filter(Year == 1997 & `Site number` == 3)

# Look at each site but you will need to change which litterfall col you want. 
monthly_litterfall_means %>% filter(`Site number` == 1 & Year==2021) %>% 
  select(Date, Year, Month, monthly_cottonwood_leaf_litterfall_g_m2) %>% 
  ggplot(., aes(x=Date, y= monthly_cottonwood_leaf_litterfall_g_m2)) + geom_line() +
  geom_point() + # geom_text(aes(label=paste("(",Date,",",Month,")"))) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  xlab("Date (monthly)") + ylab("Litterfall g/m^2")


# Writes out the mean leaf litterfall data. See above code for how the data was 
# transformed. 
write.table(monthly_litterfall_means, "./data/processed/bemp_mean_monthly_leaf_litterfall_by_area.csv", sep=",",
            row.names = FALSE, quote = TRUE, na = ".")



# All sites
monthly_litterfall_means %>%
  select(Date, Year, Month, monthly_cottonwood_leaf_litterfall_g_m2) %>% 
  ggplot(., aes(x=Date, y= monthly_cottonwood_leaf_litterfall_g_m2)) + geom_line() +
  #geom_text(aes(label=paste("(",Date,",",Month,")"))) +
  facet_wrap(~reorder(`Site name`, -Latitude), ncol = 4, scales="free_y") +
  xlab("Date (monthly)") + ylab("Litterfall g/m^2")

# Add our predictors like precipitation and depth to groundwater
groundwater <- read_csv("./data/processed/bemp_monthly_depth_to_groundwater_current.csv",
                        na =c('.','#VALUE!','NA','-999'))

groundwater
colnames(groundwater)

groundwater_clean <- groundwater %>% select(Year, Month, Latitude, Longitude, `Site number`,
                                            `Site name`, `Mean depth to groundwater (cm)`)

unique(groundwater_clean$Year)

precip <- read_csv("./data/processed/bemp_precipitation_monthly_data.csv",
                   na =c('.','#VALUE!','NA','-999'))
precip

precip_clean <- precip %>% select(Year, Month, Latitude, Longitude, `Site number`,
                                            `Site name`, `Precipitation canopy (mm)`,
                                  `Precipitation open (mm)`)
colnames(precip_clean)

gw_precip <- inner_join(groundwater_clean, precip_clean, by=c("Year","Month","Site number","Site name",
                                                  "Longitude","Latitude"))

gw_precip
colnames(gw_precip)


colnames(monthly_litterfall_means)

monthly_litterfall_means_predictors <- left_join(monthly_litterfall_means, gw_precip,
                                                 by=c("Year","Month","Site number","Site name",
                                                       "Longitude","Latitude"))
monthly_litterfall_means_predictors

write.table(monthly_litterfall_means_predictors,
            "./data/processed/bemp_mean_monthly_leaf_litterfall_by_area_w_predictors.csv", sep=",",
            row.names = FALSE, quote = TRUE, na = ".")
