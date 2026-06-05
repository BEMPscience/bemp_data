library(ggplot2)
library(ggthemes)
library(tidyverse)

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

# Reads in the annual sum in the long format.
veg_raw <- read_csv("data/raw/summer 2023 short term veg.csv", 
                          na =c('.','#VALUE!','NA','-999'))
veg_raw
colnames(veg_raw)
unique(veg_raw)



#### QAQC

# Search for duplicate rows
nrow(veg_raw)
nrow(distinct(veg_raw))

# Returns total number of duplicate rows
(nrow(veg_raw) - nrow(distinct(veg_raw)))
# Record this number in the QAQC document for the technical report.

# Sum of cover per transect and percent by species per transect
transect_sum <- veg_raw %>% ungroup() %>% group_by(Site, Transect) %>% 
  summarise(cover_total = sum(Difference, na.rm = TRUE))
transect_sum

write.table(transect_sum, "./data/processed/veg_survey_summer2023_transect_sum", sep=",",
            row.names = FALSE, quote = FALSE, na = ".")

transect_sum_species <- veg_raw %>% ungroup() %>% group_by(Site, Transect, `Species code`) %>% 
  summarise(cover_total = sum(Difference, na.rm = TRUE))
transect_sum_species

write.table(transect_sum_species, "./data/processed/veg_survey_summer2023_species_sum.csv", sep=",",
            row.names = FALSE, quote = FALSE, na = ".")

# Plotting the transects
veg_mid_points <- veg_raw %>% mutate(midpoint = (Start+End)/2) 
veg_mid_points
colnames(veg_mid_points)
veg_mid_points$midpoint

veg_mid_points %>% ggplot(., aes(x=midpoint,y=Transect, 
                                 size = Difference, color=`Species code`)) +
                            geom_point()
