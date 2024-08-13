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
              ,axis.text.x  = element_text(angle=90, vjust=0.5, size=8)
            ))

# This reads in a csv that is tab sep.
artho_raw <- read_csv("data/raw/qaqc_pitfalls_1997-2021-20240219.csv",
                      na=c('NA','.','#VALUE!','!NAN'))
artho_raw
colnames(artho_raw)

# Check the trap numbers. Should only be 1 - 20. If something else shows up correct it in the
# csv file. 
unique(artho_raw$Trap)

# Are all the years present?
unique(artho_raw$Year)

# Check all taxa numbers and correct in the main sheet if something other than a int
# is present. You can use the Data -> Filters in LibreOffice or Excel so clean up these values.
unique(artho_raw$class)
unique(artho_raw$order)
unique(artho_raw$family)
unique(artho_raw$genspp)

# Pitfall traps are on lines in a BEMP site so we add the line number on for reference.
artho_line_numbers <- artho_raw %>% 
  mutate(line_number = ifelse(Trap < 5, 1, 
                              ifelse(Trap == 5 | Trap < 9, 2,
                                     ifelse(Trap == 9 |Trap < 13, 3,
                                            ifelse(Trap == 13 | Trap <17, 4,
                                                   ifelse(Trap == 17 | Trap < 21, 5, "NA"))))))

artho_line_numbers

# Dumps out the data to a csv
write.table(artho_line_numbers, "data/processed/bemp_surface_active_arthropod_1997_2020_w_line_numbers.csv",
            sep=",", row.names = FALSE, quote = FALSE, na = ".")

# How many total buggies?
artho_line_numbers %>% 
  summarise(totals = sum(Quantity, na.rm=TRUE))

# By year?
artho_line_numbers %>% group_by(Year) %>% 
  summarise(totals = sum(Quantity, na.rm=TRUE)) %>% 
  print(n=Inf)

artho_line_numbers %>% group_by(Year, `Site number`) %>% 
  summarise(totals = sum(Quantity, na.rm=TRUE)) %>% 
  print(n=Inf)


# We will need to report out averages from the line. Usually there are traps missing or
# incorrectly set. The averages will undercount the true data. 

artho_line_numbers <- ungroup(artho_line_numbers)
artho_line_numbers

# Annual mean count by site and year. This is what we usually dump out.  
artho_annual_mean_lines <- artho_line_numbers %>% group_by(`Site number`, Year, Name, lettercode, numbercode, family,
                                                            class, order, genspp, line_number) %>%
  summarise("Mean counts of the line" = round(mean(Quantity, na.rm=TRUE)))

artho_annual_mean_lines

write.table(artho_line_numbers, "data/interim/bemp_surface_active_arthropod_mean_line_counts.csv",
            sep=",", row.names = FALSE, quote = FALSE, na = ".")

summary(artho_annual_mean_lines)

# Ungrouping to make things a bit easier. 
artho_annual_mean_lines <- artho_annual_mean_lines %>% ungroup()

# This is the mean annual count by species.
artho_annual_mean_counts_species <- artho_annual_mean_lines %>% 
  group_by(`Site number`, Year, Name, lettercode, numbercode, family, class, order, genspp) %>%
  summarise("Annual mean counts" = sum(`Mean counts of the line`, na.rm=TRUE))

artho_annual_mean_counts_species

write_csv(artho_annual_mean_counts_species,
          "./data/processed/surface_active_arthropods_annual_mean_counts_by_species.csv",
          na = '.')

# This is the mean annual count by site and year. Think of this as total
# abundance.
artho_annual_mean_counts <- artho_annual_mean_lines %>% 
  group_by(`Site number`, Year) %>%
  summarise("Annual mean counts" = sum(`Mean counts of the line`, na.rm=TRUE),
            "Species Richness" = n_distinct(lettercode)) %>%
  
  print(n=Inf)

artho_annual_mean_counts

# Writes out the mean abundance by site, species, and year. 
write_csv(artho_annual_mean_counts , "./data/processed/surface_active_arthropods_annual_mean_counts.csv",
          na = '.')

# Merge the BEMP site data with the mean abundance data
# Lat/long for each site
sites <- read_csv("./data/raw/BEMP_site_locations.csv", na = '.')
sites

# Merge commands are tricky. If you merge the incorrect way you will duplicate all your data!
# Always check your tibble/dataframe size. If it blows up or is to small, then you merged incorrectly. 
annual_mean_count_sites <- inner_join(artho_annual_mean_counts, sites,
                                      by="Site number")
annual_mean_count_sites 
colnames(annual_mean_count_sites)

# Writes out the tab;e
write.table(annual_mean_count_sites ,
            "./data/processed/annual_mean_counts_with_site_data.csv", 
            sep=",", row.names = FALSE, quote = TRUE)
unique(annual_mean_count_sites$`Site name`)

#
annual_mean_count_species_sites <- inner_join(artho_annual_mean_counts_species, sites,
                                      by="Site number")
annual_mean_count_species_sites 
colnames(annual_mean_count_species_sites)

# Writes out the table
write.table(annual_mean_count_species_sites ,
            "./data/processed/annual_mean_species_counts_with_site_data.csv", 
            sep=",", row.names = FALSE, quote = TRUE)

# Seasonal counts
# Annual mean count by site and year. This is what we usually dump out.  
artho_seasonal_mean_lines <- artho_line_numbers %>% 
  mutate("Season" = case_when(`Month` == "11" ~ "Winter",
                              `Month` == "12" ~ "Winter",
                              `Month` == "1" ~ "Winter",
                              `Month` == "2" ~ "Winter",
                              `Month` == "3" ~ "Spring",
                              `Month` == "4" ~ "Spring",
                              `Month` == "5" ~ "Spring",
                              `Month` == "6" ~ "Monsoon",
                              `Month` == "7" ~ "Monsoon",
                              `Month` == "8" ~ "Monsoon",
                              `Month` == "9" ~ "Fall",
                              `Month` == "10" ~ "Fall")) %>% 
  group_by(`Site number`, Year, Season, Name, lettercode, numbercode, family,
                                                           class, order, genspp, line_number) %>%
  summarise("Seasonal mean counts of the line" = round(mean(Quantity, na.rm=TRUE)))

artho_seasonal_mean_lines

write.table(artho_seasonal_mean_lines, "data/interim/surface_active_arthropod_mean_seasonal_line_counts.csv",
            sep=",", row.names = FALSE, quote = FALSE, na = ".")

artho_seasonal_mean_counts <- artho_seasonal_mean_lines %>% 
  group_by(`Site number`, Year, Season) %>%
  summarise("Seasonal mean counts surface active arthropods" = sum(`Seasonal mean counts of the line`,
                                         na.rm=TRUE)) 
artho_seasonal_mean_counts

write_csv(artho_seasonal_mean_counts, "./data/interim/surface_active_arthropod_mean_seasonal_counts.csv",
          na = ".")

annual_mean_seasonal_sites <- inner_join(artho_seasonal_mean_counts, sites,
                                      by="Site number")
annual_mean_seasonal_sites 
colnames(annual_mean_seasonal_sites)

write_csv(annual_mean_seasonal_sites, "./data/interim/surface_active_arthropod_mean_seasonal_counts_sites.csv",
          na = ".")
