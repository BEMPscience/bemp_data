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
depth_to_gw_processed <- read_csv("./data/processed/bemp_monthly_depth_to_groundwater_current.csv",
                                  na = c('.','-999','NA'))
depth_to_gw_processed

colnames(depth_to_gw_processed)
unique(depth_to_gw_processed$Reaches)
unique(depth_to_gw_processed$Year)

# Boxplot of monthly data by year to look at the changes in the mean and variance.
# CAUTION: The first four year are highly skewed due to a low number of samples.
# They do not represent the actual trend in the shallow riparian aquifer. 
depth_to_gw_processed %>% ggplot(., aes(x=as.factor(Year), y=-`Mean depth to groundwater (cm)`)) + 
  geom_boxplot()+
  xlab("Year") + ylab("Mean depth to groundwater (cm)") +
  geom_hline(aes(yintercept= -300), colour= 'blue')+
  theme(axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=13))


# Boxplot of monthly data by year to look at the changes in the mean and variance.
depth_to_gw_processed %>% filter(`Site number` > 0 & `Site number` <= 26 ) %>% 
  filter(Year != 2023) %>% 
  ggplot(., aes(x=as.factor(Year), y=-`Mean depth to groundwater (cm)`)) + 
  geom_boxplot()+
  xlab("Year") + ylab("Mean depth to groundwater (cm)") +
  geom_hline(aes(yintercept= -300), colour= 'blue')+
  ggtitle("Mean depth to groundwater for sites with 10+ years of data")

# By reach.
depth_to_gw_processed %>% filter(`Site number` > 0 & `Site number` <= 26 ) %>% 
  filter(Year != 2023 & Reaches != "Cochti Reach") %>% 
  ggplot(., aes(x=as.factor(Reaches), y=-`Mean depth to groundwater (cm)`)) + 
  geom_boxplot()+
  xlab("Year") + ylab("Mean depth to groundwater (cm)") +
  geom_hline(aes(yintercept= -300), colour= 'blue')+
  ggtitle("Mean depth to groundwater for sites with 10+ years of data")

# Looking at sites with more than ten years of data.
ten_years_of_data <- depth_to_gw_processed %>% filter(`Site number` > 0 & `Site number` <= 26 ) %>% 
  filter(Year != 2023)
ten_years_of_data

colnames(ten_years_of_data)

ten_years_of_data %>% filter(`Site number` == 2| `Site number` == 1 |
                               `Site number` == 6 | `Site number` == 17) %>% 
  ggplot(., aes(y=-`Mean depth to groundwater (cm)`, x=Date, color=`Site name`)) + geom_line() +
  ylim(-400, 50) + xlab("Year") + ylab("Mean depth to groundwater (cm)") +
  geom_hline(aes(yintercept= -300), colour= 'blue')


ten_years_of_data %>% filter(`Site number` == 2| `Site number` == 1 |
                               `Site number` == 6 | `Site number` == 17) %>% 
  mutate(crop = fct_reorder2(`Site name`, Date, `Mean depth to groundwater (cm)`)) %>%
  ggplot(aes(Date, -`Mean depth to groundwater (cm)`, color = `Site name`)) +
  geom_line() +
  labs(x = NULL, y = NULL, color = NULL) + 
  ylim(-400, 50) + xlab("Year") + ylab("Mean depth to groundwater (cm)") +
  geom_hline(aes(yintercept= -300), colour= 'blue')

#
boxplot_outliers <- subset(ten_years_of_data, ten_years_of_data$`Mean depth to groundwater (cm)` %in% 
         boxplot(ten_years_of_data$`Mean depth to groundwater (cm)` ~ ten_years_of_data$Year)$out)
boxplot_outliers

outliers_for_plotting <- boxplot_outliers %>% group_by(Year) %>% 
  summarise("Outlier count" = n(),
            "Outliers less than zero" = sum(-`Mean depth to groundwater (cm)` < 0),
            "Outliers greater than or equal to zero" = sum(-`Mean depth to groundwater (cm)` >= 0))
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
                                                  `North groundwater depth (cm)`,
                                                  `East groundwater depth (cm)`,
                                                  `West groundwater depth (cm)`, 
                                                  `Center groundwater depth (cm)`,
                                                  `South groundwater depth (cm)`) %>% 
  filter(`North groundwater depth (cm)` <= 0| `East groundwater depth (cm)` <= 0|
           `West groundwater depth (cm)` <= 0 | `Center groundwater depth (cm)` <= 0 |
           `South groundwater depth (cm)` <= 0)

flooded_wells

write.table(flooded_wells, "./data/processed/flooded_wells_to_2022.csv", sep=",", 
            quote = TRUE, na = ".",row.names = FALSE)

# Annual sum data wrangling
annual_groundwater <- depth_to_gw_processed %>% select(Year, Month, Day, `Site number`, `Site name`, 
                                                       `Mean depth to groundwater (cm)`,
                                                       Latitude, Longitude, Reaches) %>% 
  group_by(Year, `Site number`, `Site name`, Latitude, Longitude) %>% 
  summarise("Annual mean depth to groundwater (cm)" = mean(-`Mean depth to groundwater (cm)`,
                                                           na.rm = TRUE),
            "Annual sd depth to groundwater (cm)" = sd(-`Mean depth to groundwater (cm)`,
                                                       na.rm = TRUE))

annual_groundwater

# Write out the mean annual depth to groundwater
write_csv(annual_groundwater, "./data/processed/mean_annual_depth_to_groundwater.csv",
          na = ".")

# Just plotting the annual mean depth to groundwater over time.

### SPARKLINE PLOT

# This calculates the quarts.  
sparkline_data <- annual_groundwater %>% 
  mutate("Annual mean depth to groundwater (cm)" = -`Annual mean depth to groundwater (cm)`,
         quart1 = quantile(-`Annual mean depth to groundwater (cm)`, 0.25, na.rm = TRUE),
         quart3 = (quantile(-`Annual mean depth to groundwater (cm)`, 0.75, na.rm = TRUE)))
sparkline_data

# Round to 2 sig figs
sparkline_data$`Annual mean depth to groundwater (cm)` <- round(sparkline_data$`Annual mean depth to groundwater (cm)`,
                                                         digits = 1)  

# This sets up the mix, min, end and start values for the sparkline plot

mins <- group_by(sparkline_data, `Site number`) %>% 
  slice(which.min(`Annual mean depth to groundwater (cm)`))
maxs <- group_by(sparkline_data, `Site number`) %>% 
  slice(which.max(`Annual mean depth to groundwater (cm)`))
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
sparkline_data %>% ggplot(., aes(x=Year, y=-`Annual mean depth to groundwater (cm)`)) + 
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
  geom_text(data = ends, aes(label = -`Annual mean depth to groundwater (cm)`), hjust = 0) +
  geom_text(data = starts, aes(label = -`Annual mean depth to groundwater (cm)`), hjust = 0) +
  geom_text(data = ends, aes(label = `Site number`), hjust = 0, nudge_x = 5) +
  theme_tufte(base_size = 15)+
  theme(axis.title=element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),
        strip.text = element_blank(),
        legend.position = "none")

### 
# Comparing depth to groundwater to Rio Grande river flow
###
gw_flow <- read_csv("./data/processed/bemp_depth_to_groundwater_riverflow_to_current.csv")
gw_flow
colnames(gw_flow)

gw_flow %>% filter(`Site name`=="Alameda" | `Site name` == "Rio Grande Nature Center" |
                     `Site name`=="Los Lunas"| `Site name`=="Lemitar" |
                     `Site name`=="Sevilleta") %>% 
  ggplot(., aes(x=`Discharge (cfs), USGS`, y=-`Mean depth to groundwater (cm)`)) +
  geom_point(aes(color=`Site name`)) #+ geom_smooth(aes(group=`Site name`),method=lm)

### y(t) vs y1(t)
#

y_t_y_1_t_data <- annual_groundwater %>%
  filter(Year < 2023) %>% 
  group_by(`Site name`) %>% 
  arrange(Year, .by_group = TRUE) %>%
  mutate("Percent change" = (`Annual mean depth to groundwater (cm)`/lag(`Annual mean depth to groundwater (cm)`) - 1) * 100,
         "Absolute change" = abs(`Annual mean depth to groundwater (cm)`/lag(`Annual mean depth to groundwater (cm)`) - 1) )

y_t_y_1_t_data$`Percent change`
y_t_y_1_t_data$`Absolute change`

write_csv(y_t_y_1_t_data, "./data/processed/mean_annual_depth_to_groundwater_w_rate_or_change.csv",
          na = ".")

y_t_y_1_t_data %>% filter(`Site name`== "Alameda") %>% ggplot(., aes(x=`Percent change`, 
                                 y=`Annual mean depth to groundwater (cm)`,
                                 label=Year,
                                 color=Year))+
  geom_point(aes(size=abs(`Percent change`)), alpha=0.4) + geom_text(hjust=0, vjust=0) +
  geom_path(size=1, arrow = arrow(length = unit(7, "points"))) +
  xlab("Percent change") + theme(legend.position="none") +
  ggtitle("Alameda")

y_t_y_1_t_data %>% filter(`Site name`== "Rio Grande Nature Center") %>% ggplot(., aes(x=`Percent change`, 
                                                                     y=`Annual mean depth to groundwater (cm)`,
                                                                     label=Year,
                                                                     color=Year))+
  geom_point(aes(size=abs(`Percent change`)), alpha=0.4) + geom_text(hjust=0, vjust=0) +
  geom_path(size=1, arrow = arrow(length = unit(7, "points"))) +
  xlab("Percent change") + theme(legend.position="none")+
  ggtitle("Rio Grande Nature Center")

y_t_y_1_t_data %>% filter(`Site name`== "Los Lunas") %>% ggplot(., aes(x=`Percent change`, 
                                                                     y=`Annual mean depth to groundwater (cm)`,
                                                                     label=Year,
                                                                     color=Year))+
  geom_point(aes(size=abs(`Percent change`)), alpha=0.4) + geom_text(hjust=0, vjust=0) +
  geom_path(size=1, arrow = arrow(length = unit(7, "points"))) +
  xlab("Percent change") + theme(legend.position="none") +
  ggtitle("Los Lunas")

y_t_y_1_t_data %>% filter(`Site name`== "Belen") %>% ggplot(., aes(x=`Percent change`, 
                                                                       y=`Annual mean depth to groundwater (cm)`,
                                                                       label=Year,
                                                                       color=Year))+
  geom_point(aes(size=abs(`Percent change`)), alpha=0.4) + geom_text(hjust=0, vjust=0) +
  geom_path(size=1, arrow = arrow(length = unit(7, "points"))) +
  xlab("Percent change") + theme(legend.position="none") +
  ggtitle("Belen")

