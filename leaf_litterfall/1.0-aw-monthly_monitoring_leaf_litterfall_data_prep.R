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

# This reads in a csv that is tab sep. 
litterfall_raw <- read_csv("./data/raw/all_litterfall_QA_QCed_to_2020.csv",
                           na =c('NA','.','#VALUE!',' ') )

litterfall_raw

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

#
write.table(litterfall_site_info, "./data/processed/bemp_raw_leaf_litterfall_1997_2020.csv", sep=",", 
            quote = TRUE, na = ".")

#



# This does all the hard works of taking the mean of the data from the group_by variables.
monthly_litterfall_means <- litterfall_site_info %>% group_by(Date, `Site number`, `Site name`,
                                                              Latitude, Longitude, Reaches,
                                                Month, Year) %>% 
  summarise(cw = round(mean(cw, na.rm = TRUE),2),
            will = round(mean(will, na.rm = TRUE),2),
            ro = round(mean(ro, na.rm = TRUE),2),
            sc = round(mean(sc, na.rm = TRUE),2),
            elm = round(mean(elm, na.rm = TRUE),2),
            seep = round(mean(seep, na.rm = TRUE),2),
            nmol = round(mean(nmol, na.rm = TRUE),2),
            indbu = round(mean(indbu, na.rm = TRUE),2),
            woody = round(mean(wood, na.rm = TRUE),2),
            repcw = round(mean(repcw , na.rm = TRUE),2),
            repwil = round(mean(repwil, na.rm = TRUE),2),
            repruol = round(mean(repruol, na.rm = TRUE),2),
            repelm = round(mean(repelm, na.rm = TRUE),2),
            repsc = round(mean(repsc, na.rm = TRUE),2),
            totalrepro = round(mean(totalrepro, na.rm = TRUE),2)
            )

monthly_litterfall_means


monthly_litterfall_means %>% filter(Year == 1997 & `Site number` == 3)

# Writes out the mean leaf litterfall data. See above code for how the data was 
# transformed. 
write.table(monthly_litterfall_means, "./data/processed/bemp_mean_monthly_leaf_litterfall_by_area_1997_2020.csv", sep=",",
            row.names = FALSE, quote = TRUE, na = ".")


# Look at each site but you will need to change which litterfall col you want. 
monthly_litterfall_means %>% filter(`Site number` == 4) %>% 
  select(Date, Year, Month, cw) %>% 
  ggplot(., aes(x=Date, y= cw)) + geom_line() +
  geom_point() + geom_text(aes(label=paste("(",Date,",",Month,")"))) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  xlab("Date (monthly)") + ylab("Litterfall g/m^2")

# All sites
monthly_litterfall_means %>%
  select(Date, Year, Month, cw) %>% 
  ggplot(., aes(x=Date, y= cw)) + geom_line() +
  geom_point() +
  #geom_text(aes(label=paste("(",Date,",",Month,")"))) +
  facet_wrap(~reorder(`Site name`, -Latitude), ncol = 4, scales="free_y") +
  xlab("Date (monthly)") + ylab("Litterfall g/m^2")

