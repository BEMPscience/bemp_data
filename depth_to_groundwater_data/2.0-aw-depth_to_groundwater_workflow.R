library(ggplot2)
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
          
          
# Read in monthly well data without river flow from the USGS gauge. 
depth_to_gw_processed <- read_csv("./data/processed/bemp_monthly_depth_to_groundwater_2025-08-21.csv",
                                  na = c('.','-999','NA'))
depth_to_gw_processed
colnames(depth_to_gw_processed)
summary(depth_to_gw_processed$Year)
summary(depth_to_gw_processed$`Mean depth to groundwater cm`)

# Boxplot of monthly data by year to look at the changes in the mean and variance.
# CAUTION: The first three years are highly skewed due to a low number of samples.
# They do not represent the actual trend in the shallow riparian aquifer. 
depth_to_gw_processed %>% filter(Year > 1999 & Year < 2024) %>% 
  ggplot(., aes(x=as.factor(Year), y=`Mean depth to groundwater cm`)) + 
  geom_boxplot()+
  xlab("Year") + ylab("Mean depth to groundwater cm") +
  geom_hline(aes(yintercept= -300), colour= 'blue') +
  theme(axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=13))

# Boxplot of monthly data by year to look at the changes in the mean and variance.
depth_to_gw_processed %>% filter(`Site number` > 0 & `Site number` <= 26 ) %>% 
  filter(Year != 2024) %>% 
  ggplot(., aes(x=as.factor(Year), y=`Mean depth to groundwater cm`)) + 
  geom_boxplot()+
  xlab("Year") + ylab("Mean depth to groundwater cm") +
  geom_hline(aes(yintercept= -300), colour= 'blue')+
  ggtitle("Mean depth to groundwater for sites with 10+ years of data")

# By reach.
depth_to_gw_processed %>% filter(`Site number` > 0 & `Site number` <= 26 ) %>% 
  filter(Year != 2023 & Reaches != "Cochti Reach") %>% 
  ggplot(., aes(x=as.factor(Reaches), y=`Mean depth to groundwater cm`)) + 
  geom_boxplot()+
  xlab("Year") + ylab("Mean depth to groundwater cm") +
  geom_hline(aes(yintercept= -300), colour= 'blue')+
  ggtitle("Monthly mean depth to groundwater for sites with 10+ years of data")

# Looking at sites with more than ten years of data.
ten_years_of_data <- depth_to_gw_processed %>% filter(`Site number` > 0 & `Site number` <= 26 ) %>% 
  filter(Year != 2023)
ten_years_of_data

colnames(ten_years_of_data)

ten_years_of_data %>% filter(`Site number` == 2| `Site number` == 1 |
                               `Site number` == 6 | `Site number` == 17) %>% 
  ggplot(., aes(y=-`Mean depth to groundwater cm`, x=Date, color=`Site name`)) + geom_line() +
  ylim(-400, 50) + xlab("Year") + ylab("Mean depth to groundwater cm") +
  geom_hline(aes(yintercept= -300), colour= 'blue')


ten_years_of_data %>% filter(`Site number` == 2| `Site number` == 1 |
                               `Site number` == 6 | `Site number` == 17) %>% 
  mutate(crop = fct_reorder2(`Site name`, Date, `Mean depth to groundwater cm`)) %>%
  ggplot(aes(Date, -`Mean depth to groundwater cm`, color = `Site name`)) +
  geom_line() +
  labs(x = NULL, y = NULL, color = NULL) + 
  ylim(-400, 50) + xlab("Year") + ylab("Mean depth to groundwater cm") +
  geom_hline(aes(yintercept= -300), colour= 'blue')

#
boxplot_outliers <- subset(ten_years_of_data, ten_years_of_data$`Mean depth to groundwater cm` %in% 
         boxplot(ten_years_of_data$`Mean depth to groundwater cm` ~ ten_years_of_data$Year)$out)
boxplot_outliers

outliers_for_plotting <- boxplot_outliers %>% group_by(Year) %>% 
  summarise("Outlier count" = n(),
            "Outliers less than zero" = sum(-`Mean depth to groundwater cm` < 0),
            "Outliers greater than or equal to zero" = sum(-`Mean depth to groundwater cm` >= 0))
outliers_for_plotting

outliers_for_plotting %>% ggplot(., aes(x=Year, y=`Outlier count`)) + geom_point() +
  xlab("Year") + ylab("Number of outliers from the boxplot")

# Plot negative and positive values for outliers.
outliers_neg_pos <- outliers_for_plotting %>% select(Year, `Outliers less than zero`,
                                                     `Outliers greater than or equal to zero`) %>% 
  pivot_longer(!Year, names_to = "Count type", values_to = "count")
outliers_neg_pos

outliers_neg_pos %>% ggplot(., aes(x=Year, y=count, color=`Count type` )) + geom_point() +
  xlab("Year") + ylab("Number of outliers from the boxplot")

# Sometimes we want to know which months and for how long a site was flooded. Looking for well readings
# that are zero or negative numbers. 

flooded_wells <- depth_to_gw_processed %>% select(Year, Month, `Site number`, `Site name`,
                                                  `North groundwater depth cm`,
                                                  `East groundwater depth cm`,
                                                  `West groundwater depth cm`, 
                                                  `Center groundwater depth cm`,
                                                  `South groundwater depth cm`) %>% 
  filter(`North groundwater depth cm` >= 0| `East groundwater depth cm` >= 0|
           `West groundwater depth cm` >= 0 | `Center groundwater depth cm` >= 0 |
           `South groundwater depth cm` >= 0)

colnames(flooded_wells)
summary(flooded_wells)

write.table(flooded_wells, "./data/processed/flooded_wells_to_2023.csv", sep=",", 
            quote = TRUE, na = ".",row.names = FALSE)

### Annual sum data wrangling
colnames(depth_to_gw_processed)

annual_groundwater <- depth_to_gw_processed %>% select(Year, Month, `Site number`, `Site name`, 
                                                       `Mean depth to groundwater cm`,
                                                       Latitude, Longitude) %>% 
  group_by(Year, `Site number`, `Site name`, Latitude, Longitude) %>% 
  summarise("Annual mean depth to groundwater cm" = mean(`Mean depth to groundwater cm`,
                                                           na.rm = TRUE))


annual_groundwater
annual_groundwater <- annual_groundwater %>% group_by(`Site number`) %>%
  mutate("Lag of mean annual depth to groundwater" = lag(`Annual mean depth to groundwater cm`,
                       order_by= Year))

colnames(annual_groundwater)
unique(annual_groundwater$`Site name`)
annual_groundwater$`Annual cv depth to groundwater cm`
annual_groundwater$`Annual mean depth to groundwater cm`
annual_groundwater$`Lag of mean annual depth to groundwater`
summary(annual_groundwater$Year)

# Write out the mean annual depth to groundwater
write_csv(annual_groundwater, "./data/processed/mean_annual_depth_to_groundwater.csv",
          na = ".")


annual_groundwater %>% ggplot(.,aes(x=Year, y=`Annual cv depth to groundwater cm`)) +
  geom_point()

# Looks at the annual mean for some set of sites
annual_groundwater %>% filter(`Site name`=="Alameda" |
                                `Site name` == "Rio Grande Nature Center" |
                                `Site name` == "Reynolds Forest" |
                                `Site name` == "Los Lunas" |
                                `Site name` == "Bosque del Apache" |
                                `Site name` == "Montano") %>% 
  ggplot(.,aes(x=Year, y=`Annual mean depth to groundwater cm`)) +
  geom_point() + geom_line() +
  facet_wrap(~reorder(`Site name`, -Latitude), ncol = 1, scales="free_y")

#
annual_groundwater %>% filter(`Site name`=="Alameda" |
                                `Site name` == "Rio Grande Nature Center" |
                                `Site name` == "Reynolds Forest" |
                                `Site name` == "Los Lunas" |
                                `Site name` == "Bosque del Apache" |
                                `Site name` == "Montano") %>% 
  ggplot(.,aes(x=Year, y=`Annual cv depth to groundwater cm`)) +
  geom_point() + geom_line() +
  facet_wrap(~reorder(`Site name`, -Latitude), ncol = 1, scales="free_y")

# Looks at the annual mean for a single site with a regression line
annual_groundwater %>% filter(`Site name` == "Alameda") %>% 
  ggplot(.,aes(x=Year, y=`Annual mean depth to groundwater cm`)) +
  geom_point() + geom_line() + stat_smooth(method = "stan", 
                                           formula = y ~ x, 
                                           geom = "smooth", se = TRUE)

annual_groundwater %>% filter(`Site name` == "Alameda") %>% 
  ggplot(.,aes(x=Year, y=`Annual cv depth to groundwater cm`)) +
  geom_point() + geom_line() + stat_smooth(method = "loess",
                                           geom = "smooth", se = FALSE)

annual_groundwater %>% ggplot(.,aes(x=Year, y=`Annual cv depth to groundwater cm`)) +
  geom_point()

# Just plotting the annual mean depth to groundwater over time.

### SPARKLINE PLOT

# This calculates the quarts.  
sparkline_data <- annual_groundwater %>% 
  mutate("Annual mean depth to groundwater cm" = -`Annual mean depth to groundwater cm`,
         quart1 = quantile(-`Annual mean depth to groundwater cm`, 0.25, na.rm = TRUE),
         quart3 = (quantile(-`Annual mean depth to groundwater cm`, 0.75, na.rm = TRUE)))
sparkline_data

# Round to 2 sig figs
sparkline_data$`Annual mean depth to groundwater cm` <- round(sparkline_data$`Annual mean depth to groundwater cm`,
                                                         digits = 1)  

# This sets up the mix, min, end and start values for the sparkline plot

mins <- group_by(sparkline_data, `Site number`) %>% 
  slice(which.min(`Annual mean depth to groundwater cm`))
maxs <- group_by(sparkline_data, `Site number`) %>% 
  slice(which.max(`Annual mean depth to groundwater cm`))
ends <- group_by(sparkline_data, `Site number`) %>% 
  filter(Year == max(Year))
starts <- group_by(sparkline_data, `Site number`) %>% 
  filter(Year == min(Year))

mins
maxs
ends
starts

starts %>% filter(`Site number` == 3)

# The actually sparkline code.
sparkline_data %>% ggplot(., aes(x=Year, y=-`Annual mean depth to groundwater cm`)) + 
  facet_grid(reorder(`Site number`, -Latitude) ~ ., scales = "free_y") + 
  geom_ribbon(aes(ymin = quart1, max = quart3, fill="#839496")) +
  scale_fill_manual(values=c('#eeeeee')) +
  geom_line(col="#839496") +
  theme(axis.title=element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),
        strip.text = element_blank())+
  geom_point(data = starts, col = 'red') +
  geom_point(data = ends, col = 'red') +
  geom_text(data = ends, aes(label = -`Annual mean depth to groundwater cm`), hjust = -0.2) +
  geom_text(data = starts, aes(label = -`Annual mean depth to groundwater cm`), hjust = 1.2) +
  geom_text(data = ends, aes(label = `Site number`), hjust = 0, nudge_x = 5) +
  theme_tufte(base_size = 15)+
  theme(axis.title=element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),
        strip.text = element_blank(),
        legend.position = "none")
sparkline_data

### 
# Comparing depth to groundwater to Rio Grande river flow
###
gw_flow <- read_csv("./data/processed/bemp_monthly_depth_to_groundwater_riverflow_to_current.csv",
                    na=".")
gw_flow
colnames(gw_flow)
summary(gw_flow$`Discharge cfs USGS`)
unique(gw_flow$Year)

gw_flow %>% filter(`Site name`=="Alameda" | `Site name` == "Rio Grande Nature Center" |
                     `Site name`=="Los Lunas"| `Site name`=="Lemitar" |
                     `Site name`=="Sevilleta") %>% 
  ggplot(., aes(x=`Discharge cfs USGS`, y=`Mean depth to groundwater cm`)) +
  geom_point(aes(color=`Site name`)) #+ geom_smooth(aes(group=`Site name`),method=lm)

# Annual depth to groundwater with riverflow. 
annual_gw_riverflow <- gw_flow %>% select(Year, Month, `Site number`, `Site name`,
                                          `Mean depth to groundwater cm`, `Discharge cfs USGS`) %>% 
  group_by(`Site name`, `Site number`, Year) %>% 
  summarise("Mean annual depth to groundwater cm" = mean(`Mean depth to groundwater cm`, na.rm=TRUE),
            "Mean annual riverflow cfs" = mean(`Discharge cfs USGS`, na.rm=TRUE))
annual_gw_riverflow

write_csv(annual_gw_riverflow, "data/interim/mean_annual_gw_riverflow_bemp.csv")

### y(t) vs y1(t)
#

y_t_y_1_t_data <- annual_groundwater %>%
  filter(Year < 2023) %>% 
  group_by(`Site name`) %>% 
  arrange(Year, .by_group = TRUE) %>%
  mutate("Percent change" = (`Annual mean depth to groundwater cm`/lag(`Annual mean depth to groundwater cm`) - 1) * 100,
         "Absolute change" = abs(`Annual mean depth to groundwater cm`/lag(`Annual mean depth to groundwater cm`) - 1) )

y_t_y_1_t_data$`Percent change`
y_t_y_1_t_data$`Absolute change`

write_csv(y_t_y_1_t_data, "./data/processed/mean_annual_depth_to_groundwater_w_rate_or_change.csv",
          na = ".")

y_t_y_1_t_data %>% filter(`Site name`== "Alameda") %>% ggplot(., aes(x=`Percent change`, 
                                 y=`Annual mean depth to groundwater cm`,
                                 label=Year,
                                 color=Year))+
  geom_point(aes(size=abs(`Percent change`)), alpha=0.4) + geom_text(hjust=0, vjust=0) +
  geom_path(size=1, arrow = arrow(length = unit(7, "points"))) +
  xlab("Percent change") + theme(legend.position="none") +
  ggtitle("Alameda")

y_t_y_1_t_data %>% filter(`Site name`== "Rio Grande Nature Center") %>% ggplot(., aes(x=`Percent change`, 
                                                                     y=`Annual mean depth to groundwater cm`,
                                                                     label=Year,
                                                                     color=Year))+
  geom_point(aes(size=abs(`Percent change`)), alpha=0.4) + geom_text(hjust=0, vjust=0) +
  geom_path(size=1, arrow = arrow(length = unit(7, "points"))) +
  xlab("Percent change") + theme(legend.position="none")+
  ggtitle("Rio Grande Nature Center")

y_t_y_1_t_data %>% filter(`Site name`== "Los Lunas") %>% ggplot(., aes(x=`Percent change`, 
                                                                     y=`Annual mean depth to groundwater cm`,
                                                                     label=Year,
                                                                     color=Year))+
  geom_point(aes(size=abs(`Percent change`)), alpha=0.4) + geom_text(hjust=0, vjust=0) +
  geom_path(size=1, arrow = arrow(length = unit(7, "points"))) +
  xlab("Percent change") + theme(legend.position="none") +
  ggtitle("Los Lunas")

y_t_y_1_t_data %>% filter(`Site name`== "Belen") %>% ggplot(., aes(x=`Percent change`, 
                                                                       y=`Annual mean depth to groundwater cm`,
                                                                       label=Year,
                                                                       color=Year))+
  geom_point(aes(size=abs(`Percent change`)), alpha=0.4) + geom_text(hjust=0, vjust=0) +
  geom_path(size=1, arrow = arrow(length = unit(7, "points"))) +
  xlab("Percent change") + theme(legend.position="none") +
  ggtitle("Belen")

### Split out the data into the mean and sd of depth to groundwater for
# the spring flood pulse.

spring_flood_pulse_groundwater <- depth_to_gw_processed %>% 
  select(Year, Month, `Site number`, `Site name`, `Mean depth to groundwater cm`,
         Latitude, Longitude) %>% 
  filter(Month > 3 & Month < 7) %>% 
  group_by(Year, `Site number`, `Site name`, Latitude, Longitude) %>% 
  summarise("Mean spring flood pulse depth to groundwater cm" = mean(-`Mean depth to groundwater cm`,
                                                         na.rm = TRUE),
            "Standard deviation spring flood pulse depth to groundwater cm" = sd(-`Mean depth to groundwater cm`,
                                                     na.rm = TRUE))

spring_flood_pulse_groundwater
spring_flood_pulse_groundwater$`Mean spring flood pulse depth to groundwater cm`

write_csv(spring_flood_pulse_groundwater, "./data/processed/bemp_annual_spring_flood_pulse.csv",
          na=".")

#
# Date prep and wranlging
df_prepped <- depth_to_gw_clean %>% filter(Year < 2025) %>%                        
  mutate(
    month  = Month,                                  # keep explicit names local
    year   = Year,
    site_number   = `Site number`,
    # Day vs Night (adjust?)
    reach = case_when(
      `Site number` %in% c(12,1,2,10) ~ "Northern Albuquerque",
      `Site number` %in% c(13, 20, 29) ~ "Southern Albuquerque",
      `Site number` %in% c(27, 3, 4, 15) ~ "Isleta Reach",
      `Site number` %in% c(7, 14, 33) ~ "San Acacia Reach",
    ),
    # Season labels
    season = case_when(
      month %in% 4:6 ~ "Spring runoff",
      month %in% 7:10 ~ "Summer/Irrigation",
      month %in% c(11, 12, 1, 2, 3) ~ "Winter",
      TRUE ~ NA_character_
    ),
    # Season-year. Summer uses calendar Year; Winter spans years!
    season_year = case_when(
      season == "Spring runoff" ~ year,
      season == "Summer/Irrigation" ~ year,
      season == "Winter" & month %in% c(11, 12) ~ year + 1,  # We do this trick for the precip as well
      season == "Winter" & month %in% c(1, 2, 3) ~ year,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(season), !is.na(season_year), !is.na(reach))
df_prepped

# Yearly seasonal means
seasonal_means <- df_prepped %>%
  group_by(season, season_year, reach, `Site name`) %>%
  summarise(mean_depth = mean(`Mean depth to groundwater cm`, na.rm = TRUE), .groups = "drop")
seasonal_means

# uncomment if you want per-station trends
# seasonal_means <- df_prepped %>%
#   group_by(StationID, season, day_night, season_year) %>%
#   summarise(mean_temp = mean(Temp_C, na.rm = TRUE), .groups = "drop")

# slopes & p-values per facet for annotation, there are other ways to add p and slope. 
facet_stats <- seasonal_means %>%
  group_by(season, reach) %>%
  reframe({
    fit <- lm(mean_depth ~ season_year, data = cur_data())
    coef_tbl <- tidy(fit)
    slope_row <- filter(coef_tbl, term == "season_year")
    rng <- summarise(cur_data(),
                     xmin = min(season_year, na.rm = TRUE),
                     xmax = max(season_year, na.rm = TRUE),
                     ymin = min(mean_depth, na.rm = TRUE),
                     ymax = max(mean_depth, na.rm = TRUE))
    # place label at lower-right with a small pad
    x_pos <- rng$xmax
    y_pad <- 0.05 * (rng$ymax - rng$ymin)
    y_pos <- rng$ymin + y_pad
    
    tibble(
      x = x_pos,
      y = y_pos,
      label = sprintf("slope = %.3f °C/yr\np = %.3g", slope_row$estimate, slope_row$p.value)
    )
  })

# 2×2 facets (season × day/night), easier to read I think. 

seasonal_means <- seasonal_means %>%
  mutate(
    reach = factor(
      reach,
      levels = c(
        "Northern Albuquerque",
        "Southern Albuquerque",
        "Isleta Reach",
        "San Acacia Reach"
      )
    ),
    season = factor(
      season,
      levels = c("Spring runoff", "Summer/Irrigation", "Winter")
    )
  )
reach_lvls  <- c("Northern Albuquerque", "Southern Albuquerque",
                 "Isleta Reach", "San Acacia Reach")
season_lvls <- c("Spring runoff", "Summer/Irrigation", "Winter")

# Clean + set levels in BOTH datasets used in the plot
seasonal_means <- seasonal_means %>%
  mutate(
    reach  = str_trim(reach),
    season = str_trim(season),
    reach  = factor(reach,  levels = reach_lvls),
    season = factor(season, levels = season_lvls)
  )

facet_stats <- facet_stats %>%        # <- if you have geom_text annotations
  mutate(
    reach  = str_trim(reach),
    season = str_trim(season),
    reach  = factor(reach,  levels = reach_lvls),
    season = factor(season, levels = season_lvls)
  )

# (Optional) sanity checks
print(levels(seasonal_means$reach))
print(levels(seasonal_means$season))
print(unique(seasonal_means$reach))   # look for typos/extra spaces

seasonal_means %>%
  ggplot(., aes(x = season_year, y = mean_depth)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  facet_grid(season ~ reach, drop = FALSE) +
  # geom_text(
  #   data = facet_stats,
  #   aes(x = x, y = y, label = label),
  #   inherit.aes = FALSE,
  #   hjust = 1, vjust = 0, size = 3
  # ) +
  labs(
    x = "Year",
    y = "Seasonal mean depth to groundwater (cm)",
    title = "Seasonal Groundwater Depth Trends by Reach",
    subtitle = "Spring runoff = Apr–Jun; Summer/Irrigation = Jul–Oct; Winter = Nov–Mar"
  ) +
  theme_classic()

