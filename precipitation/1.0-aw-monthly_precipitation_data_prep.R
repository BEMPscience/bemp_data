# Monthly Precipitation Data Preparation & QAQC
# BEMP Program

# Script purpose:
#   1. Load and clean raw monthly monitoring precipitation data
#   2. Run a structured QAQC workflow that flags potential data issues
#   3. Export a QAQC report and a cleaned, processed data file
#   4. Generate diagnostic plots for visual review

# File structure expected (set working directory to project root):
# .
# └── Project name/
#   ├── data/
#   │   ├── raw/          <- source data, never modified
#   │   ├── interim/      <- intermediate cleaned files
#   │   └── processed/    <- final outputs for analysis
#   └── reports/
#       └── qaqc/         <- QAQC reports and flagging logs
#
# Working directory: Session -> Set Working Directory -> To Project Directory

# PACKAGES 

library(tidyverse)  # dplyr, ggplot2, readr, tidyr, etc.
library(viridis)    # colorblind-friendly palettes


# GLOBAL PLOT THEME

theme_set(
  theme_bw() +
    theme(
      plot.background  = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border     = element_blank(),
      axis.text.x      = element_text(angle = 90, vjust = 0.5, size = 8),
      axis.ticks       = element_blank()
    )
)

# LOAD DATA 
mm_data <- read_csv(
  "./data/raw/1.0 allmmdata.csv",
  na = c(".", "NA"),
  # Remove show_col_types = FALSE if you want to verify column types on load.
  show_col_types = FALSE
)

# Quick structural check before proceeding.
glimpse(mm_data)

# SELECT COLUMNS
#
# Column names are kept exactly as they appear in the raw data. This ensures
# the processed CSV is readable as a standalone public file and flows
# directly into downstream scripts (2.0, etc.) without translation.
#
# Site 9 is excluded from publication unless permission is granted (2026-05).

precip <- mm_data %>%
  select(
    Year,
    Month,
    Day,
    `Site number`,
    `Precipitation open mm`,
    `Precipitation canopy mm`
  ) %>%
  filter(`Site number` != 9) %>%
  # Force precip columns to numeric in case any stray characters are present.
  # If you see coercion warnings here, those rows need manual review.
  mutate(
    `Precipitation open mm`   = as.numeric(`Precipitation open mm`),
    `Precipitation canopy mm` = as.numeric(`Precipitation canopy mm`)
  )

glimpse(precip)
message("Sites in dataset: ", paste(sort(unique(precip$`Site number`)), collapse = ", "))

# BUILD DATE COLUMN

# Combines separate Year/Month/Day columns into a proper Date object.
# Using mutate() keeps everything in the pipeline rather than assigning
# with $ (which is harder to read and requires re-coercing to tibble).

precip <- precip %>%
  mutate(
    Date = as.Date(
      paste(Year, Month, Day, sep = "-"),
      format = "%Y-%m-%d"
    )
  )

# Flag any rows where the date failed to parse.
bad_dates <- precip %>% filter(is.na(Date))

if (nrow(bad_dates) > 0) {
  message("WARNING: ", nrow(bad_dates), " rows have unparseable dates. Review bad_dates.")
  print(bad_dates)
} else {
  message("Date check passed: all dates parsed successfully.")
}

# MERGE SITE LOCATIONS

# inner_join() keeps only rows present in BOTH tables.
# If the row count changes unexpectedly, check that all site numbers in
# precip exist in the sites lookup file.

sites <- read_csv(
  "./data/raw/BEMP_site_locations.csv",
  na = ".",
  show_col_types = FALSE
)

n_before    <- nrow(precip)
precip_site <- inner_join(precip, sites, by = "Site number")
n_after     <- nrow(precip_site)

if (n_before != n_after) {
  message(
    "NOTE: Row count changed after join: ", n_before, " -> ", n_after,
    ". Sites in precip not found in lookup: ",
    paste(setdiff(precip$`Site number`, sites$`Site number`), collapse = ", ")
  )
} else {
  message("Join check passed: row count unchanged (", n_after, " rows).")
}

# CALCULATE MEAN PRECIPITATION

# rowMeans() with na.rm = TRUE handles months where only ONE gauge has data:
# it returns the available value rather than NA.
# The simpler (open + canopy) / 2 returns NA if either gauge is missing.

# mean_is_single_gauge is an internal QAQC flag only -- it is NOT written
# to the public processed CSV.

precip_site <- precip_site %>%
  mutate(
    `Monthly mean precipitation mm` = rowMeans(
      cbind(`Precipitation open mm`, `Precipitation canopy mm`),
      na.rm = TRUE
    ),
    mean_is_single_gauge = is.na(`Precipitation open mm`) |
      is.na(`Precipitation canopy mm`)
  )

# QAQC CHECKS

# Each check adds an internal flag column (TRUE = potential issue).
# These flags are NOT written to the public processed CSV -- only to the
# QAQC report. No data is deleted here; flagged rows go to human review.


# --- 7a. MISSING DATA ---------------------------------------------------------
#
# is.na() catches both NA and the numeric NaN type, so nothing slips through.

na_summary <- precip_site %>%
  summarise(
    n_rows        = n(),
    na_open       = sum(is.na(`Precipitation open mm`)),
    na_canopy     = sum(is.na(`Precipitation canopy mm`)),
    na_both       = sum(is.na(`Precipitation open mm`) &
                          is.na(`Precipitation canopy mm`)),
    pct_na_open   = round(100 * na_open   / n_rows, 1),
    pct_na_canopy = round(100 * na_canopy / n_rows, 1)
  )

message("\n--- Missing Data Summary ---")
print(na_summary)

# NA breakdown by site -- useful for spotting persistently malfunctioning gauges.
na_by_site <- precip_site %>%
  group_by(`Site number`) %>%
  summarise(
    n             = n(),
    na_open       = sum(is.na(`Precipitation open mm`)),
    na_canopy     = sum(is.na(`Precipitation canopy mm`)),
    pct_na_open   = round(100 * na_open   / n, 1),
    pct_na_canopy = round(100 * na_canopy / n, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_na_open))

message("\n--- Missing Data by Site (sorted by % missing open gauge) ---")
print(na_by_site, n = Inf)

# RANGE / PHYSICAL PLAUSIBILITY

# Negative precipitation is physically impossible.
# The upper bound (200 mm) is a soft threshold for NM conditions -- intense
# monsoon months can approach this, so flagged rows still need human review.
# Adjust UPPER_BOUND_MM if your climate context changes.

UPPER_BOUND_MM <- 200

precip_site <- precip_site %>%
  mutate(
    flag_negative   = (`Precipitation open mm`   < 0 |
                         `Precipitation canopy mm` < 0) &
      !is.na(`Precipitation open mm`) &
      !is.na(`Precipitation canopy mm`),
    flag_high_value = (`Precipitation open mm`   > UPPER_BOUND_MM |
                         `Precipitation canopy mm` > UPPER_BOUND_MM) &
      !is.na(`Precipitation open mm`) &
      !is.na(`Precipitation canopy mm`)
  )

message("\n--- Range Checks ---")
message("Negative values:              ", sum(precip_site$flag_negative,   na.rm = TRUE))
message("Values above ", UPPER_BOUND_MM, " mm:  ", sum(precip_site$flag_high_value, na.rm = TRUE))

# GAUGE DISCREPANCY

# Large open-vs-canopy differences may indicate equipment issues or a
# transcription error. Two thresholds: moderate (worth noting) and severe
# (review these first).

GAUGE_DIFF_MODERATE_MM <- 20
GAUGE_DIFF_SEVERE_MM   <- 50

precip_site <- precip_site %>%
  mutate(
    gauge_diff             = abs(`Precipitation open mm` - `Precipitation canopy mm`),
    flag_gauge_discrepancy = gauge_diff > GAUGE_DIFF_MODERATE_MM & !is.na(gauge_diff),
    flag_gauge_severe      = gauge_diff > GAUGE_DIFF_SEVERE_MM   & !is.na(gauge_diff)
  )

message("\n--- Gauge Discrepancy ---")
message("Open vs. canopy > ", GAUGE_DIFF_MODERATE_MM, " mm: ",
        sum(precip_site$flag_gauge_discrepancy, na.rm = TRUE), " rows")
message("Open vs. canopy > ", GAUGE_DIFF_SEVERE_MM, " mm: ",
        sum(precip_site$flag_gauge_severe, na.rm = TRUE), " rows (review these first)")

# DUPLICATE ROWS

# Each site should have one measurement per date. Duplicates indicate a data
# entry error and must be resolved before analysis.
# Both copies of a duplicate pair are flagged (fromLast = TRUE catches the first).

precip_site <- precip_site %>%
  mutate(
    flag_duplicate =
      duplicated(select(precip_site, Year, Month, Day, `Site number`),
                 fromLast = FALSE) |
      duplicated(select(precip_site, Year, Month, Day, `Site number`),
                 fromLast = TRUE)
  )

n_dupes <- sum(precip_site$flag_duplicate, na.rm = TRUE)
message("\n--- Duplicate Check ---")
message("Rows in duplicate site/date pairs: ", n_dupes)

if (n_dupes > 0) {
  print(
    precip_site %>%
      filter(flag_duplicate) %>%
      select(`Site number`, Date, Year, Month, Day,
             `Precipitation open mm`, `Precipitation canopy mm`)
  )
}


# STATISTICAL OUTLIERS

# Flags values beyond 3 * IQR above the site-level median. Computed per site
# so a legitimately wet site is not flagged against a drier one.
# These are SOFT flags -- many will be real monsoon events. Use them as a
# starting point for visual review with plot_site_precip() below.

precip_site <- precip_site %>%
  group_by(`Site number`) %>%
  mutate(
    flag_iqr_open   = `Precipitation open mm` >
      (median(`Precipitation open mm`,    na.rm = TRUE) +
         3 * IQR(`Precipitation open mm`,   na.rm = TRUE)) &
      !is.na(`Precipitation open mm`),
    flag_iqr_canopy = `Precipitation canopy mm` >
      (median(`Precipitation canopy mm`,  na.rm = TRUE) +
         3 * IQR(`Precipitation canopy mm`, na.rm = TRUE)) &
      !is.na(`Precipitation canopy mm`)
  ) %>%
  ungroup()

message("\n--- Statistical Outlier Flags (3 x IQR above site median) ---")
message("Open gauge:   ", sum(precip_site$flag_iqr_open,   na.rm = TRUE), " flagged rows")
message("Canopy gauge: ", sum(precip_site$flag_iqr_canopy, na.rm = TRUE), " flagged rows")

# COMPILE & EXPORT QAQC REPORT

# All flagged rows in one file for manual review.
# Includes mean_is_single_gauge here since it is relevant context for review.

flag_cols <- c(
  "flag_negative", "flag_high_value",
  "flag_gauge_discrepancy", "flag_gauge_severe",
  "flag_duplicate", "flag_iqr_open", "flag_iqr_canopy"
)

qaqc_report <- precip_site %>%
  filter(if_any(all_of(flag_cols), ~ .x == TRUE)) %>%
  select(
    `Site number`, Date, Year, Month, Day,
    `Precipitation open mm`, `Precipitation canopy mm`,
    `Monthly mean precipitation mm`,
    gauge_diff, mean_is_single_gauge,
    all_of(flag_cols)
  ) %>%
  arrange(`Site number`, Date)

message("\n--- QAQC Summary ---")
message(
  "Total flagged rows: ", nrow(qaqc_report),
  " (", round(100 * nrow(qaqc_report) / nrow(precip_site), 1), "% of data)"
)

write_csv(
  qaqc_report,
  "./reports/qaqc/precipitation_qaqc_flags.csv",
  na = "."
)
message("QAQC report written to: ./reports/qaqc/precipitation_qaqc_flags.csv")


# EXPORT PROCESSED DATA

# Internal QAQC columns (flag_*, gauge_diff, mean_is_single_gauge) are dropped
# before writing. The public CSV contains only plain-language measurement columns.
# Downstream scripts (2.0, etc.) read this file directly.

precip_site %>%
  select(
    -mean_is_single_gauge,
    -gauge_diff,
    -starts_with("flag_")
  ) %>%
  write_csv(
    "./data/processed/precipitation_monthly_data.csv",
    na = "."
  )

message("Processed data written to: ./data/processed/precipitation_monthly_data.csv")

# ANNUAL BOXPLOTS

precip_site %>%
  filter(Year < 2023) %>%
  ggplot(aes(x = as.factor(Year), y = `Precipitation open mm`)) +
  geom_boxplot(outlier.colour = "firebrick", outlier.size = 1.5) +
  xlab("Year") +
  ylab("Precipitation (mm)") +
  ggtitle("Monthly open precipitation by year")


# MISSING DATA HEATMAP

# Dark cells = many missing months; white = complete record.
# Good for spotting gauge outages at specific sites or across an entire year.

precip_site %>%
  group_by(`Site number`, Year) %>%
  summarise(
    pct_na = 100 * mean(is.na(`Precipitation open mm`)),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = as.factor(Year), y = as.factor(`Site number`), fill = pct_na)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma", direction = -1, name = "% missing") +
  xlab("Year") +
  ylab("Site number") +
  ggtitle("Missing data (open gauge) — % of months per site-year")


# GAUGE DISCREPANCY OVER TIME

precip_site %>%
  filter(!is.na(gauge_diff)) %>%
  ggplot(aes(x = Date, y = gauge_diff, colour = flag_gauge_severe)) +
  geom_point(alpha = 0.4, size = 1) +
  scale_colour_manual(
    values = c("FALSE" = "grey60", "TRUE" = "firebrick"),
    name   = "Severe discrepancy\n(> 50 mm)"
  ) +
  xlab("Date") +
  ylab("| Open - Canopy | (mm)") +
  ggtitle("Gauge discrepancy over time")

# PER-SITE TIME SERIES

# Use plot_site_precip() to visually review any site. Pass a site number and
# an optional start year. Useful for checking flagged rows in context.

plot_site_precip <- function(data, site_id, start_year = 2010) {
  
  site_data <- data %>%
    filter(`Site number` == site_id, Year >= start_year) %>%
    select(Date, Year, Month,
           `Precipitation open mm`, `Precipitation canopy mm`) %>%
    pivot_longer(
      cols      = c(`Precipitation open mm`, `Precipitation canopy mm`),
      names_to  = "Gauge",
      values_to = "Precipitation (mm)"
    ) %>%
    mutate(
      Gauge = recode(Gauge,
                     `Precipitation open mm`   = "Open",
                     `Precipitation canopy mm` = "Canopy"
      )
    )
  
  if (nrow(site_data) == 0) {
    message("No data for site ", site_id, " from ", start_year, " onward.")
    return(invisible(NULL))
  }
  
  ggplot(site_data, aes(x = Date, y = `Precipitation (mm)`, colour = Gauge)) +
    geom_line(na.rm = TRUE) +
    geom_point(na.rm = TRUE, size = 1.5) +
    scale_colour_viridis_d(option = "D", name = "Gauge") +
    xlab("Date") +
    ylab("Precipitation (mm)") +
    ggtitle(
      paste0("Site ", site_id, " — Monthly precipitation (", start_year, " onward)")
    )
}

# Examples -- edit site_id and start_year as needed:
plot_site_precip(precip_site, site_id = 34, start_year = 2016)
plot_site_precip(precip_site, site_id = 33, start_year = 2020)

# To batch-save plots for all sites, uncomment the block below:
#
# dir.create("./reports/qaqc/site_plots", recursive = TRUE, showWarnings = FALSE)
#
# walk(unique(precip_site$`Site number`), function(s) {
#   p <- plot_site_precip(precip_site, site_id = s, start_year = 2010)
#   if (!is.null(p)) {
#     ggsave(
#       filename = paste0("./reports/qaqc/site_plots/site_", s, "_precip.png"),
#       plot = p, width = 10, height = 4, dpi = 150
#     )
#   }
# })

# END OF SCRIPT