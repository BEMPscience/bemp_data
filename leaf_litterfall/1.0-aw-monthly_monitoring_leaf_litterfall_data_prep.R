# "The p-value is the probability of seeing data as extreme or more extreme than the result, under the 
# assumption that the result was produced by a specific random number generator (called the null hypothesis).
# -Lakeland

# "When a hypothesis test doesn’t reject, that’s more interesting: it tells us that we know so little about
# the data that we can’t reject the hypothesis that the data where produced by a specific random number 
# generator." -Gelman

# This loads in the packages we need. 
library(ggplot2)
library(ggthemes)
library(ggsci)
library(tidyverse)
library(gridExtra)
library(GGally)
library(viridis)

# Points at my working folder.
setwd("~/Documents/BEMP/bemp_to_r/litterfall/")

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
litterfall_raw <- read.csv("./data/raw/bemp_litterfall_to_2018_partial_QAQC.txt", sep="\t",
                           na.strings=c('NA','.','#VALUE!') )

# Summary of the data
head(litterfall_raw)
str(litterfall_raw)
summary(litterfall_raw)
colnames(litterfall_raw)

# Rename some of the cols so the are not annoying to call. 
litterfall <- as_tibble(litterfall_raw %>% rename(cw = cw...new,
                                                  cw_prev_year =cw...L.yr,
                                                  year = Year,
                                                  month = Month,
                                                  day = Day,
                                                  site = Site)) 
# Did it work?
litterfall

# Data transformations
litterfall$date <- as.Date( paste( litterfall$month, litterfall$day,
                                       litterfall$year,
                                       sep = "." ), format = "%m.%d.%Y" )
# Transform data to g/m^2
tub_area_function <- function(x) {(x/0.13)} # liiterfall tubs are 0.13 square meters.
litterfall <- mutate_at(litterfall, vars(cw:totalwt.indwts),
                        list(tub_area_function))
head(litterfall)
summary(litterfall)

# Total native litterfall transformation
litterfall$native <- (litterfall$cw + litterfall$will + litterfall$seep + 
                        litterfall$nmol + litterfall$indbu)

# Total exotic litterfall transformation
litterfall$exotic <- (litterfall$sc + litterfall$ro + litterfall$elm)

# Total both native and exotic. This is ~ above ground npp.
litterfall$total_productivity <- (litterfall$native + litterfall$exotic)

summary(litterfall)

# Lat/long for each site
sites <- read.csv("./data/raw/site_location.txt",sep="\t",
                  na.strings='.')
as_tibble(sites)


# Merge commands are tricking. If you merge the incorrect way you will duplicate all your data!
# Always check your tibble/dataframe size. If it blows up or is to small then you merged incorrectly. 
litterfall_site_info <- merge(litterfall, sites, by="site")
as_tibble(litterfall_site_info)


write.table(litterfall_site_info, "bemp_raw_leaf_litterfall_1997_2017.csv", sep=",", row.names = FALSE,
            quote = FALSE)

as_tibble(litterfall_site_info)

# This does all the hard works of taking the mean of the data from the group_by variables.
monthly_litterfall_means <- litterfall_site_info %>% group_by(date, site, site_name, site_lat, site_long, 
                                                month, year) %>% 
  summarise(mean_productivity = mean(total_productivity, na.rm = TRUE), 
            cw = mean(cw, na.rm = TRUE),
            will = mean(will, na.rm = TRUE),
            ro = mean(ro, na.rm = TRUE),
            sc = mean(sc, na.rm = TRUE),
            elm = mean(elm, ra.rm = TRUE),
            seep = mean(seep, na.rm = TRUE),
            nmol = mean(nmol, na.rm = TRUE),
            thcr = mean(thcr, na.rm = TRUE),
            indbu = mean(indbu, na.rm = TRUE),
            woody = mean(wood, na.rm = TRUE),
            total_weight = mean(totalwt, na.rm = TRUE),
            natives = mean(native, na.rm = TRUE),
            exotics = mean(exotic, na.rm = TRUE))

monthly_litterfall_means

summary(monthly_litterfall_means)

# Writes out the mean leaf litterfall data. See above code for how the data was 
# transformed. 
write.table(monthly_litterfall_means, "bemp_mean_leaf_litterfall_1997_2017.csv", sep=",",
            row.names = FALSE, quote = FALSE)
