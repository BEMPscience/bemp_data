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

#
raw_veg <- read_csv("./data/raw/veg survey bank attached bars.csv",
                    na = c('.','-999','NA'))
raw_veg

#### QAQC

# Search for duplicate rows
nrow(raw_veg)
nrow(distinct(raw_veg))

# Returns total number of duplicate rows
(nrow(raw_veg) - nrow(distinct(raw_veg)))
# Record this number in the QAQC document for the technical report.

# Count up the number of negative values in the Difference column.
veg_w_neg_cover <- raw_veg %>% filter(sum < 0)
veg_w_neg_cover

# Check for negative or greater than 30 Start values
raw_veg %>% filter(Start < 0 | Start > 30)

# Check for End values greater than 30 meters.
raw_veg %>% filter(End > 30)

veg_mids <- raw_veg %>% mutate("Midpoint for plotting" = ((Start+End)/2))
veg_mids$`Midpoint for plotting`

# Add some ordering by age for plotting
level_order<- c('MinBar1','MinBar3','LosIsle3','HarIsle1','HarIsle3','LosIsle1')


veg_mids %>% ggplot(., aes(y=factor(Site,level=level_order),
                           x=`Midpoint for plotting`, size=sum, color=Species)) +
  geom_point() + ylab("Sites arragned from top (oldest) to bottom by age estimates")

# Here we get the sums by plant by island/bar. 
survey_sum_long <- raw_veg %>% group_by(Site, Species) %>% 
  summarise("Total cover in mm" = sum(sum, na.rm = TRUE))
survey_sum_long

survey_sum_long %>% ggplot(., aes(x=`Total cover in mm`,y=factor(Site, level=level_order))) +
  geom_bar(stat="identity",aes(fill = Species), position="fill") +
  scale_fill_viridis_d()+
  xlab("Percent total cover") + ylab("Sites arragned from top (oldest) to bottom by age estimates")

write.table(survey_sum_long, "./data/processed/islands_bars_veg_survey_sums_long.csv", sep=",",
            row.names = FALSE, quote = FALSE, na = ".")

# Wide format
survey_sum_wide <- survey_sum_long %>% 
  pivot_wider(names_from = Species, values_from = `Total cover in mm`, names_repair = "unique") %>% 
  replace(is.na(.), 0)
survey_sum_wide

write.table(survey_sum_wide, "./data/processed/islands_bars_veg_survey_sums_wide.csv", sep=",",
            row.names = FALSE, quote = FALSE, na = ".")


# Order heat map
level_order<- c('MinBar1','MinBar3','LosIsle3','HarIsle1','HarIsle3','LosIsle1')

survey_sum_long %>% 
  pivot_wider(names_from = Species, values_from = `Total cover in mm`, names_repair = "unique") %>% 
  pivot_longer(!Site,names_to = "Species", values_to = "mm cover") %>% 
  ggplot(., aes(x=factor(Site, level=level_order), Species, fill= `mm cover`, na.value="white")) + 
  geom_tile() + xlab("Bank attached bar ordered by age estimates (youngest to oldest)")


# Read in age estimates and then merge into the veg. 
age_est <- read_csv("data/raw/age estimates bank attached bars.csv")
age_est <- age_est %>% select(-Notes)
age_est

veg_w_age <- left_join(survey_sum_long, age_est, by=("Site"))
veg_w_age

veg_w_age %>% ggplot(., aes(x=factor(Site, level=level_order), y=`Total cover in mm`, fill=Species)) +
  geom_bar(stat="identity") + xlab("Bank attached bar ordered by age estimates (youngest to oldest)")

veg_w_age %>% ggplot(., aes(x=as.factor(`Estimated age of establishment`), y=`Total cover in mm`)) +
  geom_boxplot() + ylab("Total cover in mm by species") + 
  xlab("Bank attached bars ordered by age estimates (youngest to oldest)")


