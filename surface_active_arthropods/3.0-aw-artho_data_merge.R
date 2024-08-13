library(ggplot2)
library(tidyverse)

# Working directory should be set using Session -> Set Working Directory. Not hard coded. 
# Better practices suggest your file structure look like this:
# .
# └── Project name/
#   ├── data/
#   │   ├── external
#   │   ├── fake
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

artho_annual <- read_csv("./data/processed/annual_mean_counts_with_site_data.csv",
                na=c('NA','.','#VALUE!','#N/A'))

# Double check to make sure it's read in correctly
artho_annual
colnames(artho_annual)

# How many total buggies?
artho_annual %>% 
  summarise(totals = sum(`Annual mean counts`, na.rm=TRUE))

# By year?
artho_annual %>% group_by(Year) %>% 
  summarise("Total mean counts" = sum(`Annual mean counts`, na.rm=TRUE)) %>% 
  print(n=Inf)

# Merge the lat long data for each site so we can plot them in order from N-S
# Lat/long for each site
gw_data <- read_csv("./data/interim/mean_annual_depth_to_groundwater.csv",
                    na=".")
gw_data
colnames(gw_data)

# Merge commands are tricking. If you merge the incorrect way you will duplicate all your data!
# Always check your tibble/dataframe size. If it blows up or is to small then you merged incorrectly. 
artho_gw <- left_join(artho_annual, gw_data, by=c("Year","Site number", "Site name",
                                                  "Latitude", "Longitude"))
artho_gw

# Merge the annual veg data. Note that this data is long and we will
# need to transform it wide. 
veg <- read_csv("./data/interim/veg_survey_annual_sum_w_metadata.csv",
                    na=".")

annual_veg <- veg %>% group_by(Year, `Site number`, `Life form`) %>% 
  summarise("Total annual cover cm" = sum(`Total cover cm`))
annual_veg  

annual_veg$can_under <- with(annual_veg, ifelse(`Life form` %in% c("T", "S"),"Canopy cm","Understory cm"))
annual_veg 

annual_veg_wide <- annual_veg %>% 
  group_by(Year,`Site number`, can_under) %>% 
  summarise("Canopy understory annual sum cm"= sum(`Total annual cover cm`)) %>% 
  pivot_wider(names_from = can_under, 
              values_from = `Canopy understory annual sum cm`) %>% 
  mutate(`Total annual cover cm`= `Canopy cm` + `Understory cm`)

annual_veg_wide 

# 
artho_gw

#
artho_gw_veg <- left_join(artho_gw, annual_veg_wide, by=c("Year","Site number"))
artho_gw_veg

artho_gw_veg %>% filter(Reaches == "Angostura Reach") %>% 
  write_csv(., "./data/interim/annual_arthro_gw_veg_20240208.csv",
          na=".")

colnames(artho_gw_veg)
unique(artho_gw_veg$Reaches)

artho_gw_veg %>% 
  ggplot(., aes(x=`Annual mean depth to groundwater cm`,
                y=`Annual mean counts`)) + geom_point()
         
artho_gw_veg %>% 
  ggplot(., aes(x=`Annual cv depth to groundwater cm`,
                y=`Annual mean counts`)) + geom_point()        

artho_gw_veg %>% 
  ggplot(., aes(x=`Canopy cm`,
                y=`Annual mean counts`)) + geom_point()  

artho_gw_veg %>% 
  ggplot(., aes(x=`Understory cm`,
                y=`Annual mean counts`)) + geom_point()

artho_gw_veg %>% 
  ggplot(., aes(x=`Understory cm`,
                y=`Annual mean counts`)) + geom_point()
