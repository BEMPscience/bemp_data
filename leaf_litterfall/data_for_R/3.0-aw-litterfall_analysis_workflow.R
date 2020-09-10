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
                ,axis.text.x  = element_text(angle=90, vjust=0.5, size=8)
              ))

# This reads in a csv that is tab sep. 
litterfall_raw <- read.csv("./data/raw/bemp_litterfall_to_2018_partial_QAQC.txt", sep="\t",
                        na.strings=c('NA','.','#VALUE!') )
head(litterfall_raw)
str(litterfall_raw)
summary(litterfall_raw)

# Data transformations
litterfall_raw$date <- as.Date( paste( litterfall_raw$month, litterfall_raw$day, litterfall_raw$year,
                                       sep = "." ), format = "%m.%d.%Y" )
# Transform data to g/m^2
tub_area_function <- function(x) {(x/0.13)}
litterfall_raw <- mutate_at(litterfall_raw, vars(cw...new:totalwt.indwts),
                   list(tub_area_function))
head(litterfall_raw)
summary(litterfall_raw)

# Total native litterfall
litterfall_raw$native <- (litterfall_raw$cw...new + litterfall_raw$will + litterfall_raw$seep + 
                            litterfall_raw$nmol + litterfall_raw$indbu)

# Total exotic litterfall
litterfall_raw$exotic <- (litterfall_raw$sc + litterfall_raw$ro + litterfall_raw$elm)

# Total both native and exotic. This is ~ npp.
litterfall_raw$total_npp <- (litterfall_raw$native + litterfall_raw$exotic)

summary(litterfall_raw)

# This does all the hard works of summing up the data by the above group_by variables.
monthly_npp_mean <- litterfall_raw %>% group_by(date, site, month, year) %>% 
  summarise(total_productivity = mean(total_npp, na.rm = TRUE), 
                             cw = mean(cw...new, na.rm = TRUE),
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

monthly_npp_mean
summary(monthly_npp_mean)
#View(monthly_npp_mean)

# ID10T Error checking. Sometimes called QAQC. 
monthly_npp_mean %>% filter(cw > 300)

monthly_npp_mean %>% filter(year == 1999)

# Plot to check data.
ggplot(data = monthly_npp_mean, aes(x=date,y=cw))+geom_point()+geom_line()+
  facet_wrap(~site, ncol=4)

# Merge data sets.
# Now we need to merge a bunch of additional data to provide context for the litterfall. 
# Each new data is read in and then viewed as a tibble to make sure everything looks right.

# Lat/long for each site
sites <- read.csv("./data/raw/site_location.txt",sep="\t",
                  na.strings='.')
as_tibble(sites)


# Merge commands are tricking. If you merge the incorrect way you will duplicate all your data!
# Always check your tibble/dataframe size. If it blows up or is to small then you merged incorrectly. 
lf_sites <- merge(monthly_npp_mean, sites, by="site")
as.tibble(lf_sites)

# Depth to groundwater by month, year, and BEMP site. 
# We start with the well readings and need to subtract off the casing heights for each.

well_data <- read.csv("data/raw/bemp_well_data_to_2019", sep = "\t", na.strings = c('.','-999'),
                      quote="")
str(well_data)
head(well_data)
summary(well_data)

depth_to_gw <- well_data %>% mutate(north_depth = northwell - NorthCaseHeight,
                                    east_depth = eastwell - EastCaseHeight,
                                    center_depth = centerwell - CenterCaseHeight,
                                    south_depth = southwell - SouthCaseHeight,
                                    west_depth = westwell - WestCaseHeight,
                                    river_flow_cfs = river_cfs)

depth_to_gw <- filter(depth_to_gw, year < 2018)
summary(depth_to_gw)

# The data is running wide so we need to take the row means and drop NAs to get the mean depth
# to groundwater for each site.
mean_depth_to_gw <- depth_to_gw %>%
  mutate(mean_depth_gw=rowMeans(.[ , c("north_depth", "east_depth","center_depth",
                                       "south_depth", "west_depth")], na.rm=TRUE))
summary(mean_depth_to_gw)

# Here we are dropping a bunch of columns we don't need.
mean_depth_gw_select <- mean_depth_to_gw %>% 
  select(year, month, day, site, north_depth, east_depth, center_depth, south_depth, west_depth,
         mean_depth_gw, river_flow_cfs)
as_tibble(mean_depth_gw_select)
summary(mean_depth_gw_select)

write.table(mean_depth_to_gw, "actual_gw_depth_by_well_site_year.csv", row.names=FALSE)

redacted <- read.csv("./data/raw/santa_ana_well_summary_near_bemp_site.csv", sep = "\t", na.strings=".")
as_tibble(redacted)
redacted <- select(redacted, site, year, month, mean_depth_gw)

mean_depth_gw_select <- bind_rows(mean_depth_gw_select, redacted)
head(mean_depth_gw_select)

summary(filter(mean_depth_gw_select, site == 5))

# Merges the depth to groundwater into the litterfall data. 
lf_gw <- merge(lf_sites, mean_depth_gw_select, by=c("site","month","year"))
as.tibble(lf_gw)

# Rainfall by month, year, and BEMP site. 
rain <- read.csv("./data/raw/bemp_precip_data.txt",sep="\t",
                 na.strings='.')
as.tibble(rain)

litterfall <- inner_join(lf_gw, rain, by=c("site","month","year"))
as.tibble(litterfall)

# This contains the history of fires, floods, and magement practices for each site.
history <- read.csv("data/raw/site_history.csv", sep="\t", na.strings = c(",","."))
as_tibble(history)

monthly_litterfall <- merge(litterfall , history, by=c("site","year"))
summary(monthly_litterfall)

# Monthly data check
ggplot(data=monthly_litterfall, aes(x=year, y=mean_depth_gw))+geom_point(aes(size=))

# Albuqueurque South Valley NOAA temperature data
temp <- read.csv("./data/interim/noaa_temperature_data_three_sites", sep = "\t", 
                 na.strings = "M")
head(temp)
str(temp)

abq_valley_temp <- temp %>% filter(location == "abq_valley")

abq_temp_wide <- abq_valley_temp %>% spread(key = temp_ranges, value = temp)
str(abq_temp_wide)

monthly_litterfall_temp <- merge(monthly_litterfall, abq_temp_wide, by="year")
head(monthly_litterfall_temp)

monthly_litterfall_temp$precipitation <- rowMeans(monthly_litterfall_temp[c('ppt_open',
                                                                     'ppt_canopy')],
                                           na.rm=TRUE)

monthly_litterfall_temp$precipitation

# Writes out all the transformations to the raw data into a tab seperated file. 
write.table(monthly_litterfall_temp, "./data/interim/BEMP_monthly_litterfall_for_modeling.txt",
            sep = "\t", row.names = FALSE, quote=FALSE)

# MONTHLY DATA PLOTS
# This shows the monthly litterfall over time by site. 
# Orders it so the sites are north to south. 
summary(monthly_litterfall_temp)

p1 <- ggplot(data=monthly_litterfall_temp, aes(x=reorder(site, -lat), y=cw)) +
  geom_boxplot()
p1

one_site <- monthly_litterfall_temp %>% filter(site == 1)
head(one_site)

p2 <- ggplot(data=one_site, aes(x=Date, y=mean_depth_gw)) +
  geom_point() + geom_line()
p2

p3 <- ggplot(data=monthly_litterfall_temp, aes(x=reorder(site, -lat),
                                               y=precipitation)) +
  geom_boxplot()
p3

p4 <- ggplot(data=monthly_litterfall_temp, aes(x=mean_depth_gw, y=river_flow_cfs ))
+ geom_point()
p4

#
ggplot(data=monthly_litterfall_temp) + geom_point(aes(x=mean_depth_gw, y=npp))
ggplot(data=monthly_litterfall_temp) + geom_point(aes(x=max_temp, y=npp))
ggplot(data=monthly_litterfall_temp) + geom_boxplot(aes(x=as.factor(cleared_impact), y=npp))


ggplot(data=monthly_litterfall_temp) + geom_boxplot(aes(x=as.factor(flood_impact), y=npp))
ggplot(data=monthly_litterfall_temp) + geom_boxplot(aes(x=as.factor(fire_impact), y=npp))
ggplot(data=monthly_litterfall_temp) + geom_boxplot(aes(x=as.factor(cleared_impact), y=npp))
ggplot(data=monthly_litterfall_temp) + geom_boxplot(aes(x=as.factor(flood_status), y=npp))


ggplot(data=monthly_litterfall_temp, aes(x=cw)) + geom_histogram() + 
  facet_wrap(~year, ncol=4)

# ANNUAL DATA WRANGLING. Mostly for looking at the data on the yearly scale. 
head(monthly_litterfall)
summary(monthly_litterfall_temp)


annual_litterfall_data <- summarise(monthly_litterfall_temp, npp = sum((total_weight-woody), na.rm = TRUE), 
                            cw = sum(cw, na.rm = TRUE),
                            will = sum(will, na.rm = TRUE),
                            ro = sum(ro, na.rm = TRUE),
                            sc = sum(sc, na.rm = TRUE),
                            elm = sum(elm, na.rm = TRUE),
                            seep = sum(seep, na.rm = TRUE),
                            nmol = sum(nmol, na.rm = TRUE),
                            thcr = sum(thcr, na.rm = TRUE),
                            indbu = sum(indbu, na.rm = TRUE),
                            woody = sum(woody, na.rm = TRUE),
                            total_weight = sum(total_weight, na.rm = TRUE),
                            natives = sum(natives, na.rm = TRUE),
                            exotics = sum(exotics, na.rm = TRUE),
                            annual_precipitation = mean(precipitation, na.rm = TRUE),
                            annual_tmax = mean(max_temp, na.rm = TRUE),
                            annual_tmean = mean(mean_temp, na.rm = TRUE),
                            annual_tmin = mean(min_temp, na.rm = TRUE),
                            depth_gw = mean(mean_depth_gw, na.rm = TRUE),
                            sd_depth_gw = sd(mean_depth_gw, na.rm = TRUE))

summary(annual_litterfall_data)

# Spring flooding impact on fall litterfall.
# 
# The primary litterfall we want is from August to Decemeber. With the spring flood pulse from
# March to May.

# What follows is a bunch of data wrangling to make this happen. 
leafdrop_npp <- filter(monthly_litterfall_temp, month > 8 & month <= 12 )
summary(leafdrop_npp)

leafdrop_npp_yearly <- group_by(leafdrop_npp, site, year, lat, lon)

yearly_npp_sum <- summarise(leafdrop_npp_yearly, npp = sum((total_weight-woody), na.rm = TRUE), 
                            cw = sum(cw, na.rm = TRUE),
                            will = sum(will, na.rm = TRUE),
                            ro = sum(ro, na.rm = TRUE),
                            sc = sum(sc, na.rm = TRUE),
                            elm = sum(elm, na.rm = TRUE),
                            seep = sum(seep, na.rm = TRUE),
                            nmol = sum(nmol, na.rm = TRUE),
                            thcr = sum(thcr, na.rm = TRUE),
                            indbu = sum(indbu, na.rm = TRUE),
                            woody = sum(woody, na.rm = TRUE),
                            total_weight = sum(total_weight, na.rm = TRUE),
                            natives = sum(natives, na.rm = TRUE),
                            exotics = sum(exotics, na.rm = TRUE),
                            annual_precipitation = sum(precipitation, na.rm = TRUE),
                            annual_tmax = mean(max_temp, na.rm = TRUE),
                            annual_tmean = mean(mean_temp, na.rm = TRUE),
                            annual_tmin = mean(min_temp, na.rm = TRUE))

yearly_npp_sum
summary(yearly_npp_sum)
#View(yearly_npp_sum)

# This takes only the spring runoff depth to groundwater data. 
spring_pulse <- filter(mean_depth_gw_select, month < 6  & month > 2)
#View(spring_pulse)

spring_pulse_by_year <- group_by(spring_pulse, site, year)

yearly_spring_pulse <- summarise(spring_pulse_by_year, mean_annual_depth_gw = mean(mean_depth_gw, na.rm = TRUE), 
                                std_depth_gw = sd(mean_depth_gw, na.rm = TRUE))

yearly_spring_pulse
summary(yearly_spring_pulse)

# Merge the fall litterfall and spring runoff depth to groundwater. 
flood_test <- merge(yearly_npp_sum, yearly_spring_pulse, by=c("site","year"))

history$flood_age = paste(history$flood_status, history$cottonwood, sep="-")

full_flood_test <- merge(flood_test, history, by=c("site","year"))
as.tibble(full_flood_test)

full_flood_test %>% filter(site == 5 & year >2010)

# Removing sites with no data or less than three data points. 
flood_model <- filter(full_flood_test, site != 24 & site != 26 & site != 28 & 
                        site != 29 & site != 30 &
                        site != 31 & site != 32 & site != 5 & site != 27)
summary(flood_model)
str(flood_model)

flood_model[flood_model == -999] <- NA

ggplot(data=flood_model, aes(x=cw)) + geom_density(alpha=0.6)


ggplot(data=flood_model, aes(x=year, y=-mean_annual_depth_gw)) + geom_point() +
  facet_wrap(~reorder(site, -lat), ncol=4)

ggplot(data=flood_model, aes(x=-mean_annual_depth_gw, y=cw)) + geom_point() +
  geom_smooth(method = "lm")

ggplot(data=flood_model, aes(x=cw)) + geom_histogram()

ggplot(data=flood_model, aes(x=cw)) + geom_dotplot(binwidth = 20) + 
  scale_y_continuous(NULL, breaks = NULL)+
  facet_wrap(~year, ncol=4)

ggplot(data=flood_model, aes(y=will, x=flood_status)) + geom_boxplot()


# Look at the litterfall by site, year, and ordered by mean.
litterfall_stats <- flood_model %>%
  group_by(site) %>%
  summarise(cw_mean = mean(cw),
            cw_sd = sd(cw),
            n = length(site)) %>%
  mutate(cw_se = cw_sd / sqrt(n))
litterfall_stats

ggplot() +
  geom_boxplot(data = flood_model,
               mapping = aes(y = cw,
                             x = fct_reorder(as.factor(site), cw, mean)),
               colour = "gray") +
  geom_point(data = flood_model,
             mapping = aes(y = cw,
                           x = fct_reorder(as.factor(site), cw, mean)),
             colour = "gray")+
  geom_point(data = litterfall_stats,
             mapping = aes(x = fct_reorder(as.factor(site), cw_mean),
                           y = cw_mean),
             colour = "black")+
  coord_flip() +
  labs(y = "Cottonwood litterfall", x = "BEMP Sites")

write_rds(full_model_dbh, "./data/interim/litterfall_annual_data_for_modeling.RDS")

# Check for collinearity in the data being used for the model. 
pair_plots <- flood_model %>% 
  select(cw, annual_precipitation, annual_tmax, annual_tmean, annual_tmin, mean_annual_depth_gw)

ggpairs(pair_plots, aes(alpha = 0.4))

#


# Minard plot for a single site. 
ala <- filter(flood_model, site == 16)

ala <- ala %>% 
  mutate_if(is.numeric, round, digits=2)
ala

groundwater_to_2018 <- ggplot(ala, aes(x = year, y = -mean_depth_gw, size = mean_depth_gw)) +
  geom_path(color="blue",lineend = "round") +
  ylim(-250, -100) +
  scale_size(range = c(0.8, 10)) +
  guides(color = FALSE, size = FALSE)#+
  #theme_void()
groundwater_to_2018

ggsave(groundwater_to_2018, filename = "groundwater_path_to_2018.pdf", device = cairo_pdf,
       width = 11, height = 8.5, units = "in")

litterfall.nice <- ala %>%
  mutate(nice.label = paste0(cw, ", ", year))

litterfall.to.2018 <- ggplot(data = litterfall.nice, aes(x = year, y = cw)) +
  geom_line() +
  geom_label(aes(label = nice.label),
             family = "Ubuntu", size = 2.5) + 
  labs(x = NULL, y = "Annual litterfall (g/m^2)") +
  scale_x_continuous(limits = ggplot_build(groundwater_to_2018)$layout$panel_ranges[[1]]$x.range) +
  scale_y_continuous(position = "right") +
  coord_cartesian(ylim = c(0, 200)) +  # Add some space above/below
  theme_bw(base_family = "Open Sans Condensed Light") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(), axis.ticks = element_blank(),
        panel.border = element_blank())

litterfall.to.2018

ggsave(litterfall.to.2018, filename = "litterfall_path_to_2018_site16.pdf", device = cairo_pdf,
       width = 11, height = 8.5, units = "in")

both.plots <- gtable_rbind(ggplotGrob(groundwater_to_2018),
                        ggplotGrob(litterfall.to.2018))

grid::grid.newpage()
grid::grid.draw(both.plots)

ggsave(grid::grid.draw(both.plots), filename = "paths_to_2018_site16.pdf", device = cairo_pdf,
       width = 11, height = 8.5, units = "in")

panels <- both.plots$layout$t[grep("panel", both.plots$layout$name)]
both.plots$heights[panels] <- unit(c(3, 1), "null")

grid::grid.newpage()
grid::grid.draw(both.plots)

ggsave(grid::grid.draw(both.plots), filename = "paths_to_2018_scaled_site16.pdf", device = cairo_pdf,
       width = 11, height = 8.5, units = "in")
