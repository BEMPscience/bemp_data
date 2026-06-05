library(ggplot2)
library(tidyverse)
library(ggwordcloud)

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
artho_raw <- read_csv("data/raw/Arthropods/SWFL_RAV_pitfalls_2025.csv",
                      na=c('NA','.','#VALUE!','!NAN'))
artho_raw
colnames(artho_raw)

# Check the trap numbers. Should only be 1 - 20. If something else shows up correct it in the
# csv file. 
unique(artho_raw$trap)

# Are all the years present?
unique(artho_raw$year)

# Check all taxa numbers and correct in the main sheet if something other than a int
# is present. You can use the Data -> Filters in LibreOffice or Excel so clean up these values.
unique(artho_raw$class)
unique(artho_raw$order)
unique(artho_raw$family)
unique(artho_raw$genspp)

# Pitfall traps are on lines in a BEMP site so we add the line number on for reference.
artho_line_numbers <- artho_raw %>% 
  mutate(line_number = ifelse(trap < 5, 1, 
                              ifelse(trap == 5 | trap < 9, 2,
                                     ifelse(trap == 9 |trap < 13, 3,
                                            ifelse(trap == 13 | trap <17, 4,
                                                   ifelse(trap == 17 | trap < 21, 5, "NA"))))))

artho_line_numbers

# Dumps out the data to a csv
write.table(artho_line_numbers, "data/processed/corrales_surface_active_arthropod_2025_w_line_numbers.csv",
            sep=",", row.names = FALSE, quote = FALSE, na = ".")

# How many total buggies?
artho_line_numbers %>% 
  summarise(totals = sum(quantity, na.rm=TRUE))

artho_line_numbers %>% group_by(site) %>% 
  summarise(totals = sum(quantity, na.rm=TRUE)) %>% 
  print(n=Inf)

# We will need to report out averages from the line. Usually there are traps missing or
# incorrectly set. The averages will undercount the true data. 

artho_line_numbers <- ungroup(artho_line_numbers)
artho_line_numbers

# Annual mean count by site and year. This is what we usually dump out.  
artho_annual_mean_lines <- artho_line_numbers %>% group_by(site, name, lettercode, numbercode, family,
                                                            class, order, genspp, line_number) %>%
  summarise("Mean counts of the line" = round(mean(quantity, na.rm=TRUE)))

artho_annual_mean_lines

write.table(artho_line_numbers, "data/interim/corrales_surface_active_arthropod_mean_line_counts.csv",
            sep=",", row.names = FALSE, quote = FALSE, na = ".")

summary(artho_annual_mean_lines)

# Ungrouping to make things a bit easier. 
artho_annual_mean_lines <- artho_annual_mean_lines %>% ungroup()

# This is the mean annual count by species.
artho_annual_mean_counts_species <- artho_annual_mean_lines %>% 
  group_by(site, name, lettercode, numbercode, family, class, order, genspp) %>%
  summarise("Annual mean counts" = sum(`Mean counts of the line`, na.rm=TRUE))

artho_annual_mean_counts_species

write_csv(artho_annual_mean_counts_species,
          "./data/processed/corrales_surface_active_arthropods_annual_mean_counts_by_species.csv",
          na = '.')

artho_annual_mean_counts_species

# Visual differences between sites?
# Make the column easier to use
artho_annual_mean_counts_species <- artho_annual_mean_counts_species %>% rename(annual_mean_counts = `Annual mean counts`)

# Pick a label to show on the cloud (prefer 'name', then 'genspp', then 'family')
artho_annual_mean_counts_species <- artho_annual_mean_counts_species %>%
  mutate(label = dplyr::coalesce(name))

# (Optional) keep top N per site to avoid clutter
topN <- 20
artho_annual_mean_counts_species_top <- artho_annual_mean_counts_species %>%
  group_by(site, label) %>%
  summarise(mean_count = sum(annual_mean_counts, na.rm = TRUE), .groups = "drop") %>%
  group_by(site) %>%
  slice_max(order_by = mean_count, n = topN, with_ties = FALSE) %>%
  ungroup()

set.seed(123)  # reproducible placement
ggplot(artho_annual_mean_counts_species_top, aes(label = label, size = mean_count)) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 40, guide = "legend") +
  facet_wrap(~ site) +
  labs(
    title = "Surface-active arthropods: mean counts by site",
    size  = "Mean count"
  ) +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave("./reports/plots/corrales_arthropods_by_site.png", device = ragg::agg_png,
       width = 10, height = 6, dpi = 600)

# Difference cloud
df <- artho_annual_mean_counts_species %>% 
  mutate(label = dplyr::coalesce(name)) %>%
  group_by(site, label) %>%
  summarise(mean_count = sum(annual_mean_counts, na.rm = TRUE), .groups = "drop")

# Pivot to wide: one column per site
wide <- df %>%
  pivot_wider(names_from = site, values_from = mean_count, values_fill = 0)

# Identify site columns (assumes exactly two)
site_cols <- setdiff(names(wide), "label")

# Compute differences (Site A – Site B)
diff_tbl <- wide %>%
  mutate(
    diff   = .data[[site_cols[1]]] - .data[[site_cols[2]]],
    winner = case_when(
      diff  > 0 ~ site_cols[1],
      diff  < 0 ~ site_cols[2],
      TRUE      ~ "Tie"
    ),
    size = abs(diff)
  ) %>%
  filter(size > 0)   # drop ties

#
keepN <- 30
diff_top <- diff_tbl %>%
  slice_max(order_by = size, n = keepN, with_ties = FALSE)

set.seed(123)
ggplot(diff_top, aes(label = label, size = size, color = winner)) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 30, guide = "legend") +
  labs(
    title    = "Difference cloud: which site has higher mean counts?",
    subtitle = paste(site_cols[1], "vs", site_cols[2]),
    size     = "Δ mean count",
    color    = "Higher at"
  ) +
  theme_void()
# ggsave("reports/graphs/arthropods_wordcloud_difference.png", width = 10, height = 6, dpi = 300)