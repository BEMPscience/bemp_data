
# Annual
annual_groundwater

y_t_y_1_t_data <- annual_groundwater %>%
  filter(Year < 2023) %>% 
  group_by(`Site name`) %>% 
  arrange(Year, .by_group = TRUE) %>%
  mutate("Percent change" = (`Annual mean depth to groundwater (cm)`/lag(`Annual mean depth to groundwater (cm)`) - 1) * 100,
         "Absolute change" = abs(`Annual mean depth to groundwater (cm)`/lag(`Annual mean depth to groundwater (cm)`) - 1) )

y_t_y_1_t_data$`Percent change`
y_t_y_1_t_data$`Absolute change`

y_t_y_1_t_data %>% filter(`Site name`== "Alameda") %>% 
  ggplot(., aes(x=`Percent change`, 
                y=`Annual mean depth to groundwater (cm)`,
                label=Year,
                color=Year)) +
  geom_point(aes(size=abs(`Percent change`)), alpha=0.4) + geom_text(hjust=0, vjust=0) +
  geom_path(size=1, arrow = arrow(length = unit(7, "points"))) +
  xlab("Percent change") + theme(legend.position="none") +
  ggtitle("Alameda") + ylab("Annual mean monthly depth to groundwater (cm)")

# Monthly
y_t_y_1_t_data <- depth_to_gw_processed %>%
  filter(Year < 2023) %>% 
  group_by(`Site name`) %>% 
  arrange(date, .by_group = TRUE) %>%
  mutate("Percent change" = (`Mean depth to groundwater (cm)`/lag(`Mean depth to groundwater (cm)`) - 1) * 100,
         "Absolute change" = abs(`Mean depth to groundwater (cm)`/lag(`Mean depth to groundwater (cm)`) - 1) )

y_t_y_1_t_data$`Percent change`
y_t_y_1_t_data$`Absolute change`

y_t_y_1_t_data %>% filter(`Site name`== "Alameda") %>% ggplot(., aes(x=`Percent change`, 
                                                                     y=`Mean depth to groundwater (cm)`,
                                                                     label=Month,
                                                                     color=Month))+
  geom_point(aes(size=abs(`Percent change`)), alpha=0.4) + geom_text(hjust=0, vjust=0) +
  geom_path(size=1, arrow = arrow(length = unit(7, "points"))) +
  xlab("Percent change") + theme(legend.position="none") +
  ggtitle("Alameda") + ylab("Mean monthly depth to groundwater (cm)")
