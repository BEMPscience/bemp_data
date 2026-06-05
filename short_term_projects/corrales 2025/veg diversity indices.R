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
              ,strip.background = element_rect()
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

veg_clean
unique(veg_clean$Species)

shannon_by_line <- veg_clean %>%
  group_by(Site, Transect, Species) %>%
  summarise(cover = sum(difference, na.rm = TRUE), .groups = "drop_last") %>%
  filter(cover > 0) %>%                           # drop zero-cover species
  mutate(p = cover / sum(cover)) %>%              # proportions within each line
  summarise(
    H = -sum(p * log(p)),                         # Shannon (nat. log)
    S = n(),                                      # richness
    J = H / log(S),                               # Pielou evenness
    .groups = "drop"
  )
shannon_by_line

write_csv(shannon_by_line, "./data/processed/shannon_diversity_by_line.csv")

shannon_by_line %>% 
  ggplot(., aes(x=Site, y=H)) + geom_boxplot()

shannon_by_line %>% 
  ggplot(., aes(x=Site, y=J)) + geom_boxplot()
