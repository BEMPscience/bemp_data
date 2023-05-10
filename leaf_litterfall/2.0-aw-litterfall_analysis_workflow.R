# This loads in the packages we need. 
library(ggplot2)
library(ggtext)
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


# This reads in a csv as a tibble. This is already corrected for area of the tub. 
litterfall_monthly <- read_csv("./data/processed/bemp_mean_monthly_leaf_litterfall_by_area.csv",
                           na =c('NA','.','#VALUE!',' ') )

litterfall_monthly
colnames(litterfall_monthly)


litterfall_monthly_repro_recalc <- litterfall_monthly %>% 
  mutate(totalrepro2 = case_when(is.na(repcw) & Year < 2007 ~ Month,
                                 !is.na(repcw) & Year > 2006 ~ repcw+repwil+repruol+repelm+repsc))

litterfall_monthly_repro_recalc

litterfall_monthly_repro_recalc$totalrepro2

# Monthly litterfall plots
litterfall_monthly  %>% filter(Year==2022) %>% 
  ggplot(., aes(x=Date, y=cw)) + geom_line() + # Here we are selecting what we want to look at. 
  #geom_text(aes(label=paste("(",Date,",",Month,")"))) +
  facet_wrap(~reorder(`Site name`, -Latitude), ncol = 4, scales="free_y") +
  xlab("Year") + ylab("Monthly cottonwood litterfall g/"~m^2)
ggsave("./reports/cottonwood_monthly_area.pdf", width = 36, height = 36)

litterfall_monthly  %>% 
  ggplot(., aes(x=Date, y=repcw)) + geom_line() + # Here we are selecting what we want to look at. 
  geom_point() +
  #geom_text(aes(label=paste("(",Date,",",Month,")"))) +
  facet_wrap(~reorder(`Site name`, -Latitude), ncol = 4, scales="free_y") +
  xlab("Year") + ylab("Monthly cottonwood reproductive litterfall g/m^2")
ggsave("./reports/cottonwood_repro_monthly_area.pdf", width = 36, height = 36)

litterfall_monthly  %>% 
  ggplot(., aes(x=Date, y=repcw)) + geom_line() + # Here we are selecting what we want to look at. 
  geom_point() +
  #geom_text(aes(label=paste("(",Date,",",Month,")"))) +
  facet_wrap(~reorder(`Site name`, -Latitude), ncol = 4, scales="free_y") +
  xlab("Year") + ylab("Monthly cottonwood reproductive litterfall g/m^2")
ggsave("./reports/cottonwood_repro_monthly_area.pdf", width = 36, height = 36)

litterfall_monthly  %>% 
  ggplot(., aes(x=Date, y=ro)) + geom_line() + # Here we are selecting what we want to look at. 
  geom_point() +
  #geom_text(aes(label=paste("(",Date,",",Month,")"))) +
  facet_wrap(~reorder(`Site name`, -Latitude), ncol = 4, scales="free_y") +
  xlab("Year") + ylab("Monthly russian olive litterfall g/m^2")
ggsave("./reports/russian_olive_monthly_area.pdf", width = 36, height = 36)

litterfall_monthly  %>% 
  ggplot(., aes(x=Date, y=sc)) + geom_line() + # Here we are selecting what we want to look at. 
  geom_point() +
  #geom_text(aes(label=paste("(",Date,",",Month,")"))) +
  facet_wrap(~reorder(`Site name`, -Latitude), ncol = 4, scales="free_y") +
  xlab("Year") + ylab("Monthly saltcedar reproductive litterfall g/m^2")
ggsave("./reports/saltcedar_monthly_area.pdf", width = 36, height = 36)

# ANNUAL DATA WRANGLING. Mostly for looking at the data on the yearly scale. 

annual_litterfall_data <- litterfall_monthly  %>% 
  group_by(`Site number`, `Site name`, Latitude, Longitude, Reaches, Year) %>% 
  summarise("Annual cw gm per m2" = sum(`Monthly cottonwood leaf litterfall, g/m2`, na.rm = TRUE),
            "Annual will gm per m2" = sum(`Monthly willow leaf litterfall, g/m2`, na.rm = TRUE),
            "Annual ro gm per m2" = sum(`Monthly russian olive leaf litterfall, g/m2`, na.rm = TRUE),
            "Annual sc gm per m2" = sum(`Monthly saltcedar leaf litterfall, g/m2`, na.rm = TRUE),
            "Annual elm gm per m2" = sum(`Monthly elm leaf litterfall, g/m2`, na.rm = TRUE),
            "Annual seep gm per m2" = sum(`Monthly seep willow leaf litterfall, g/m2`, na.rm = TRUE),
            "Annual nmol gm per m2" = sum(`Monthly new mexico olive leaf litterfall, g/m2`, na.rm = TRUE),
            "Annual indbu gm per m2" = sum(`Monthly indigo bush leaf litterfall, g/m2`, na.rm = TRUE),
            "Annual woody gm per m2" = sum(`Monthly wood fall, g/m2`, na.rm = TRUE),
            "Annual repcw gm per m2" = sum(`Monthly cottonwood reproductive parts, g/m2` , na.rm = TRUE),
            "Annual repwil gm per m2" = sum(`Monthly willow reproductive parts, g/m2`, na.rm = TRUE),
            "Annual repruol gm per m2" = sum(`Monthly russian olive reproductive parts, g/m2`, na.rm = TRUE),
            "Annual repelm gm per m2" = sum(`Monthly elm reproductive parts, g/m2`, na.rm = TRUE),
            "Annual repsc gm per m2" = sum(`Monthly saltcedar reproductive parts, g/m2`, na.rm = TRUE),
            "Annual totalrepro2 gm per m2" = sum(`Monthly total reproductive parts, g/m2`, na.rm = TRUE)
  )
annual_litterfall_data           
            
# Writes out the annual litterfall
write.table(annual_litterfall_data, "./data/processed/bemp_annual_leaf_litterfall_by_area.csv", sep=",",
            row.names = FALSE, quote = TRUE, na = ".")

# Box plot to show mean and variance over time for sites with 10+ years of data
annual_litterfall_data  %>% filter(Year != "NA") %>% #filter(`Site number` > 0 & `Site number` <= 26 ) %>% 
  ggplot(., aes(x=as.factor(Year), y=`Annual cw gm per m2`)) + geom_boxplot() +
  xlab("Year") + ylab("Annual cottonwood litterfall (g/m^2)") 

annual_litterfall_data  %>% filter(Year != "NA") %>% #filter(`Site number` > 0 & `Site number` <= 26 ) %>% 
  ggplot(., aes(x=as.factor(Year), y=`Annual will`)) + geom_boxplot() +
  xlab("Year") + ylab("Annual willow litterfall (g/m^2)")

annual_litterfall_data  %>% filter(Year != "NA") %>% #filter(`Site number` > 0 & `Site number` <= 26 ) %>% 
  ggplot(., aes(x=as.factor(Year), y=`Annual woody`)) + geom_boxplot() +
  xlab("Year") + ylab("Annual wood fall (g/m^2)")

annual_litterfall_data  %>% filter(Year != "NA") %>% #filter(`Site number` > 0 & `Site number` <= 26 ) %>% 
  ggplot(., aes(x=as.factor(Year), y=`Annual totalrepro2`)) + geom_boxplot() +
  xlab("Year") + ylab("Annual total reproductive parts (g/m^2)")

annual_litterfall_data  %>% filter(Year != "NA") %>% #filter(`Site number` > 0 & `Site number` <= 26 ) %>% 
  ggplot(., aes(x=as.factor(Year), y=`Annual sc`)) + geom_boxplot() +
  xlab("Year") + ylab("Annual total saltcedar litterfall (g/m^2)")

annual_litterfall_data  %>% filter(Year != "NA") %>% #filter(`Site number` > 0 & `Site number` <= 26 ) %>% 
  ggplot(., aes(x=as.factor(Year), y=`Annual elm`)) + geom_boxplot() +
  xlab("Year") + ylab("Annual total elm litterfall (g/m^2)")

annual_litterfall_data  %>% filter(Year != "NA") %>% #filter(`Site number` > 0 & `Site number` <= 26 ) %>% 
  ggplot(., aes(x=as.factor(Year), y=`Annual ro`)) + geom_boxplot() +
  xlab("Year") + ylab("Annual total russian olive litterfall (g/m^2)")


# Plots the annual cottonwood litterfall over time
annual_litterfall_data  %>% 
  ggplot(., aes(x=Year, y=`Annual cw gm per m2`)) + geom_line() + # Here we are selecting what we want to look at. 
  geom_point() +
  #geom_text(aes(label=paste("(",Date,",",Month,")"))) +
  facet_wrap(~reorder(`Site name`, -Latitude), ncol = 4, scales="free_y") +
  xlab("Year") + ylab("Annual cottonwood litterfall g/"~m^2)


annual_litterfall_data  %>% filter(`Site name` == "Belen") %>% 
  ggplot(., aes(x=Year, y=`Annual cw`)) + geom_line() + # Here we are selecting what we want to look at. 
  geom_point() +
  #geom_text(aes(label=paste("(",Date,",",Month,")"))) +
  facet_wrap(~reorder(`Site name`, -Latitude), ncol = 4, scales="free_y") +
  xlab("Year") + ylab("Annual cottonwood litterfall g/"~m^2)
ggsave("./reports/cottonwood_annual_litterfall_area_belen.pdf", width = 6, height = 5)

# Plots the annual cottonwood repro litterfall over time
annual_litterfall_data  %>% 
  ggplot(., aes(x=Year, y=`Annual repcw`)) + geom_line() + # Here we are selecting what we want to look at. 
  geom_point() +
  #geom_text(aes(label=paste("(",Date,",",Month,")"))) +
  facet_wrap(~reorder(`Site name`, -Latitude), ncol = 4, scales="free_y") +
  xlab("Year") + ylab("Annual cottonwood reproductive litterfall g/m^2")
ggsave("./reports/cottonwood_repro_annual_litterfall_area.pdf", width = 8.5, height = 11)

# Plots the annual russian olive litterfall over time
annual_litterfall_data  %>% 
  ggplot(., aes(x=Year, y=`Annual ro`)) + geom_line() + # Here we are selecting what we want to look at. 
  geom_point() +
  #geom_text(aes(label=paste("(",Date,",",Month,")"))) +
  facet_wrap(~reorder(`Site name`, -Latitude), ncol = 4, scales="free_y") +
  xlab("Year") + ylab("Annual russian olive litterfall g/m^2")
ggsave("./reports/russian_olive_annual_litterfall_area.pdf", width = 8.5, height = 11)

# Plots the annual russian olive litterfall over time
annual_litterfall_data  %>% 
  ggplot(., aes(x=Year, y=`Annual sc`)) + geom_line() + # Here we are selecting what we want to look at. 
  geom_point() +
  #geom_text(aes(label=paste("(",Date,",",Month,")"))) +
  facet_wrap(~reorder(`Site name`, -Latitude), ncol = 4, scales="free_y") +
  xlab("Year") + ylab("Annual saltcedar litterfall g/m^2")
ggsave("./reports/saltcedar_annual_litterfall_area.pdf", width = 8.5, height = 11)

# Plots the annual cottonwood litterfall over time
annual_litterfall_data  %>% 
  ggplot(., aes(x=Year, y=`Annual cw`)) + geom_line() + # Here we are selecting what we want to look at. 
  geom_point() +
  #geom_text(aes(label=paste("(",Date,",",Month,")"))) +
  facet_wrap(~reorder(`Site name`, -Latitude), ncol = 4, scales="free_y") +
  xlab("Year") + ylab("Annual litterfall g/m^2")

# To just look at annual productivity you will need the monthly data and just the fall months.
litterfall_annual_productivity <- litterfall_monthly %>%
  filter(Month > 4 & Month <= 9 ) %>%
  group_by(`Site number`, `Site name`, Latitude, Longitude, Reaches, Year) %>% 
  summarise("Annual autumn cw gm per m2" = sum(cw, na.rm = TRUE),
            "Annual autumn will gm per m2" = sum(will, na.rm = TRUE),
            "Annual autumn ro gm per m2" = sum(ro, na.rm = TRUE),
            "Annual autumn sc gm per m2" = sum(sc, na.rm = TRUE),
            "Annual autumn elm gm per m2" = sum(elm, na.rm = TRUE),
            "Annual autumn seep gm per m2" = sum(seep, na.rm = TRUE),
            "Annual autumn nmol gm per m2" = sum(nmol, na.rm = TRUE),
            "Annual autumn indbu gm per m2" = sum(indbu, na.rm = TRUE))

litterfall_annual_productivity 

write.table(litterfall_annual_productivity ,
            "./data/processed/bemp_annual_autumn_litterfall_by_area.csv", sep=",",
            row.names = FALSE, quote = TRUE, na = ".")  

# Productivity, using autumn leaf fall as a proxy, is driven by the shallow riparian aquifer.
# Here we need to merge the autumn leaf fall with the annual depth to groundwater.

groundwater_annual <- read_csv("/Users/anmwinter/Documents/BEMP/bemp_to_r/depth_to_ground_water/data/processed/mean_annual_depth_to_groundwater.csv",
                               na =c('NA','.','#VALUE!',' ') )
groundwater_annual

# Visual check to make sure the inner_join worked.
litter_prod_gw <- inner_join(litterfall_annual_productivity, 
                             groundwater_annual, by=c("Year","Site number"))
litter_prod_gw

write_csv(litter_prod_gw, "./data/processed/mean_annual_autumn_litterfall_w_groundwater.csv",
          na = ".")

litter_prod_gw %>% ggplot(aes(x=-annual_mean_depth_to_groundwater,
                              y=`Annual autumn cw`)) + geom_point() +
  scale_x_reverse() +
  labs(x="Mean depth to groundwater (cm)",
       y="Annual autumn cottonwood litterfall (g/m^(2))")+
  theme(axis.title.y = element_markdown())

litter_prod_gw %>% ggplot(aes(x=annual_sd_depth_to_groundwater,
                              y=`Annual autumn cw`)) + geom_point() +
  labs(x="Standard deviation of annual depth to groundwater (cm)",
       y="Annual autumn cottonwood litterfall (g/m^(2))")+
  theme(axis.title.y = element_markdown())

litter_prod_gw %>% ggplot(aes(x=-annual_mean_depth_to_groundwater,
                              y=`Annual autumn will`)) + geom_point() +
  scale_x_reverse() +
  labs(x="Mean depth to groundwater (cm)",
       y="Annual autumn willow litterfall (g/m^(2))")+
  theme(axis.title.y = element_markdown())

litter_prod_gw %>% ggplot(aes(x=-annual_mean_depth_to_groundwater,
                              y=`Annual autumn nmol`)) + geom_point() +
  scale_x_reverse() +
  labs(x="Mean depth to groundwater (cm)",
       y="Annual autumn New Mexico olive litterfall (g/m^(2))")+
  theme(axis.title.y = element_markdown())

litter_prod_gw %>% ggplot(aes(x=-annual_mean_depth_to_groundwater,
                              y=`Annual autumn sc`)) + geom_point() +
  scale_x_reverse() +
  labs(x="Mean depth to groundwater (cm)",
       y="Annual autumn saltcedar litterfall (g/m^(2))")+
  theme(axis.title.y = element_markdown())

litter_prod_gw %>% ggplot(aes(x=-annual_mean_depth_to_groundwater,
                              y=`Annual autumn ro`)) + geom_point() +
  scale_x_reverse() +
  labs(x="Mean depth to groundwater (cm)",
       y="Annual autumn russian olive litterfall (g/m^(2))")+
  theme(axis.title.y = element_markdown())

litter_prod_gw %>% ggplot(aes(x=-annual_mean_depth_to_groundwater,
                              y=`Annual autumn elm`)) + geom_point() +
  scale_x_reverse() +
  labs(x="Mean depth to groundwater (cm)",
       y="Annual autumn elm litterfall (g/m^(2))")+
  theme(axis.title.y = element_markdown())

write_csv(litter_prod_gw, "./data/processed/annual_aut_litterfall_groundwater.csv",
          na = ".")

# It's likely that the spring flows are most important, followed by the rate of change of the shallow
# aquifer, and time the shallow riparian aquifer is below some critical threshold that varies 
# depending in the plant species. 

groundwater_monthly <- read_csv("/home/awinter/Documents/BEMP/bemp_to_r/depth_to_ground_water/data/processed/bemp_depth_to_groundwater_riverflow_to_current.csv",
                                na =c('NA','.','#VALUE!',' ') )

groundwater_monthly

# Trim out all months but spring flow months
groundwater_spring <- groundwater_monthly %>% 
  filter(Month > 4 & Month <= 9 )

# Sometimes we will need to see which sites flooded. This will not sure up in the mean annual.
# So we pull out any site where at least one well has a raw depth of 0 or - .

flooded_wells <- read_csv("/Users/anmwinter/Documents/BEMP/",
                               na =c('NA','.','#VALUE!',' ') )