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

#

raw_veg <- read_csv("data/raw/Veg/Veg_Corrales_SWFL_RAV.csv",
                    na = c('.','-999','NA'))
raw_veg

#### QAQC
# Identify species NA rows
problematic_rows <- raw_veg %>%
  filter(`Species code` == "" | is.na(`Species code`))

# Count how many rows are removed
num_removed <- nrow(problematic_rows)
print(paste("Number of rows removed:", num_removed))

# Save problematic rows to a CSV file for inspection
write.csv(problematic_rows, "data/processed/problematic_species_NA_rows.csv", row.names = FALSE)

# Remove rows where the species is NA
raw_veg  <- raw_veg %>%
  filter(`Species code` != "" & !is.na(`Species code`))  # Remove empty or NA species names

# Search for duplicate rows
nrow(raw_veg)
nrow(distinct(raw_veg))

# Returns total number of duplicate rows
(nrow(raw_veg) - nrow(distinct(raw_veg)))


# Remove exact duplicate rows
veg_derep <- raw_veg[!duplicated(raw_veg),]
veg_derep
nrow(veg_derep)

# Count up the number of negative values in the Difference column.
diff_less_than_zero <- veg_derep %>% filter(difference < 0)
diff_less_than_zero
write_csv(diff_less_than_zero, "./data/processed/veg_survey_diff_less_than_zero.csv",
          na=".")
# These will be to be corrected in the main sheet on drive or removed in downstream processing


# Veg species codes get updated on a regular basis. Before writing out the cleaned up
# data replace outdated veg codes.

# 1) Read lookup (2 columns: old -> new)
lk <- read_csv("data/raw/Veg/veg code fixes.csv", show_col_types = FALSE) %>%
  transmute(
    old = str_to_upper(str_squish(`Outdated species code`)),
    new = str_to_upper(str_squish(`Species code`))
  )

# 2) Normalize your data’s key and join
veg_clean <- veg_derep %>%
  mutate(code = str_to_upper(str_squish(`Species code`))) %>%
  left_join(lk, by = c("code" = "old")) %>%
  mutate(
    Species = coalesce(new, code)   # prefer NEW; fallback to original code if no match
  ) %>%
  select(-new, -code)               # keep `Species` only

any(veg_clean$Species == "FEAR3")   # should be FALSE if FEAR3 has a row in lookup
any(veg_clean$Species == "SCAR7")   # should be TRUE if you mapped FEAR3 -> SCAR7




# Write out the rows that have negative cover and double check with the original data
write_csv(veg_clean, "./data/processed/veg_survey_cleaned_data.csv",
            na = ".")

# Check for negative or greater than 30 Start values
neg_or_30plus_veg_survey <- veg_clean %>% filter(Start < 0 | Start > 30)
neg_or_30plus_veg_survey

# Check for End values greater than 30 meters.
veg_clean %>% filter(End > 30)

# Here we get the annual sums for the veg in a long format
veg_sum <- veg_clean %>% ungroup() %>% group_by(Site, Species) %>% 
  summarise("Total cover cm" = sum(difference, na.rm = TRUE))
veg_sum

write_csv(veg_sum, "./data/processed/veg_survey_sums_long.csv",
          na = ".")

# Wide format
sum_wide <- veg_sum %>% 
  pivot_wider(names_from = Species, values_from = "Total cover cm", names_repair = "unique") %>% 
  replace(is.na(.), 0)
sum_wide

write_csv(annual_sum_wide, "./data/processed/veg_survey_annual_sums_wide.csv",
            na = ".")

### Attaching metadata for downstream analysis.

# This adds in the common names and other plant traits based on the species code.
plant_metadata <- read_csv("./data/raw/Veg/veg species list metadata and replacements - complete.csv",
                     na = c('.','-999','NA'))

nrow(distinct(plant_metadata))
(nrow(plant_metadata) - nrow(distinct(plant_metadata)))

plant_metadata
colnames(plant_metadata)
unique(plant_metadata$`Life form`)
unique(plant_metadata$`Native to U.S.`)
unique(plant_metadata$Species)

plant_metadata %>% 
  group_by(Species) %>% 
  filter(n()>1)

# Merge
veg_sum

veg <- left_join(veg_sum, plant_metadata, by="Species")
veg
tail(veg)
colnames(veg)


# Writes out the annual sum with all metadata
write_csv(veg, "./data/processed/veg_survey_annual_sum_w_metadata.csv",
            na = ".")

# Write out unique species by site
species_by_site <- veg %>%
  group_by(Species, `Common name`) %>%
  summarise(sites = paste(unique(Site), collapse = ", "),
            present_at_n_sites = n_distinct(Site)) %>% 
  arrange(desc(present_at_n_sites))

species_by_site

write_csv(species_by_site, "./data/processed/veg_species_by_site.csv",
          quote = c("needed"))

#
species_by_site_per_year <- veg %>% group_by(Site) %>% 
  summarise("Unique species" = n_distinct(Species))
species_by_site_per_year

write_csv(species_by_site_per_year, "./data/processed/veg_species_count_by_site_by_year.csv")



# Some QAQC might require looking at each line of veg over time to find errors.
veg_clean

veg_clean_info <- left_join(veg_clean, plant_metadata, by="Species")
veg_clean_info

veg_clean_info %>% 
  ggplot(., aes(x=((End+Start)/2), y=as.character(Transect), size = difference,
                color=`Life form`)) + 
  geom_point() +
  facet_wrap(~Site, ncol=1) +
  ggtitle("Vegetation survey lines")

veg_clean_info %>% 
  ggplot(., aes(x=((End+Start)/2), y=as.character(Transect), size = difference,
                color=`Species code`)) + 
  geom_point() +
  facet_wrap(~Site, ncol=1) +
  ggtitle("Vegetation survey lines")



### Head over to 2.0 which is the workflow
