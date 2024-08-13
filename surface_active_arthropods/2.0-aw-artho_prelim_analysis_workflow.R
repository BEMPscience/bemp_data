library(ggplot2)
library(tidyverse)
library(vegan)

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
              ,panel.border = element_blank()
              ,axis.text.x  = element_text(angle=90, vjust=0.5, size=8)
              ,axis.ticks = element_blank()
            ))

artho_means <- read_csv("data/processed/surface_active_arthropods_annual_mean_counts_by_species.csv",
                na=c('NA','.','#VALUE!','#N/A'))
# Double check to make sure it's read in correctly
artho_means
colnames(artho_means)
unique(artho_means$Year)
unique(artho_means$class)

# Removes any vertbrate 
artho_means <- artho_means %>% filter(class != 9)

# How many total buggies?
artho_means %>% 
  summarise(totals = sum(`Annual mean counts`, na.rm=TRUE))

# By year?
artho_means %>% group_by(Year) %>% 
  summarise("Total mean counts" = sum(`Annual mean counts`, na.rm=TRUE)) %>% 
  print(n=Inf)

# Merge the lat long data for each site so we can plot them in order from N-S
# Lat/long for each site
sites <- read_csv("./data/raw/BEMP_site_locations.csv")
sites

# Merge commands are tricking. If you merge the incorrect way you will duplicate all your data!
# Always check your tibble/dataframe size. If it blows up or is to small then you merged incorrectly. 
artho_annual_sites <- left_join(artho_means, annual_gw, by=c("Site number","Year",
                                                             "Latitude",))
artho_annual_sites

# Ants
artho_annual_sites %>% filter(class == "1" & order == "11" & family == "13" & `Site number` != 9) %>%
  group_by(`Site name`, Year, Latitude) %>% 
  summarise(mean_abundance = sum(`Annual mean counts`)) %>% 
  ggplot(aes(x=Year, y=mean_abundance)) + geom_point() + geom_line() +
  facet_wrap(~reorder(`Site name`, -Latitude), ncol = 4, scales="free_y") +
  xlab("Year") + ylab("Mean Counts (Ants)")

# Pills and sows
artho_annual_sites %>% filter(class == "6" & order == "2" & family == "2" & `Site number` != 9) %>%
  group_by(`Site name`, Year, Latitude) %>% 
  summarise(mean_abundance = sum(`Annual mean counts`)) %>% 
  ggplot(aes(x=Year, y=mean_abundance)) + geom_point() + geom_line() +
  facet_wrap(~reorder(`Site name`, -Latitude), ncol = 4, scales="free_y") +
  xlab("Year") + ylab("Mean Counts (Pill and sow bugs)")

# Carabs and tenebs
artho_annual_sites %>% filter(class == "1" & order == "2" & family == "55" | family == "46"  &
                                       `Site number` != 9) %>%
  group_by(`Site name`, Year, Latitude, family) %>% 
  summarise(mean_abundance = sum(`Annual mean counts`)) %>% 
  ggplot(aes(x=Year, y=mean_abundance, color=as.factor(family))) + geom_point() + geom_line() +
  facet_wrap(~reorder(`Site name`, -Latitude), ncol = 4, scales="free_y") +
  xlab("Year") + ylab("Mean Counts (Carabs-55 and Tenebs-46)")

# Spiders
artho_annual_sites %>% filter(class == "5" & order == "2" & `Site number` != 9) %>%
  group_by(`Site name`, Year, Latitude) %>% 
  summarise(mean_abundance = sum(`Annual mean counts`)) %>% 
  ggplot(aes(x=Year, y=mean_abundance)) + geom_point() + geom_line() +
  facet_wrap(~reorder(`Site name`, -Latitude), ncol = 4, scales="free_y") +
  xlab("Year") + ylab("Mean Counts (Spiders)")

# Next we want to understand what is happening at the community level. To do this we need
# to convert the data into a OTU table. Species across the table, sites down the column,
# and counts for the data. 
artho_annual_sites
colnames(artho_annual_sites)
artho_annual_sites$numbercode

otu_table_prep<- artho_annual_sites %>% filter(Year > 2014) %>% 
  group_by(`Site number`, `Site name`, Year, numbercode, Latitude, Longitude, Reaches) %>%
  summarise("Annual mean counts by species" = sum(`Annual mean counts`, na.rm=FALSE))
otu_table_prep

otu_table <- otu_table_prep %>% select(`Site number`,`Site name`, Year, numbercode,
                                                Latitude, Longitude, Reaches,
                                           `Annual mean counts by species`) %>%
  pivot_wider(names_from = numbercode, values_from = `Annual mean counts by species`) %>% 
  select(-c("0")) %>% 
  replace(is.na(.), 0)
  

otu_table
colnames(otu_table)

#
write.table(otu_table, "data/interim/bemp_surface_active_arthropod_otu_multisite_year.csv",
            sep=",", row.names = FALSE, quote = FALSE, na = ".")

#

# Here we make the community composition data frame and double check it. 
com_table <- otu_table[,7:ncol(otu_table)]
com_table

#com_table_clean <- com_table[, colSums(com_table) != 1]
#com_table_clean <- com_table[rowSums(com_table_clean[])>0,]
#com_table_clean

# Convert the abundance data frame into a matrix for distance measures. 
com_table_matrix <- as.matrix(com_table)
class(com_table_matrix)

# Dumps out the matrix for troubleshooting
write.table(com_table_matrix, "data/interim/bemp_surface_active_arthropod_otu_matrix.csv",
            sep=",", row.names = FALSE, quote = FALSE, na = ".")
# 
nmds_com = metaMDS(com_table_matrix, distance = "bray")
nmds_com

plot(nmds_com)

#
# Takes the NMDS scores and adds on the environmental parameters.
site.scrs <- as.data.frame(scores(nmds_com, display = "sites")) #save NMDS results into dataframe
site.scrs <- cbind(site.scrs, "Site number" = otu_table$`Site number`) #add grouping variable "Management" to dataframe
site.scrs <- cbind(site.scrs, Year = otu_table$Year) #add grouping variable of cluster grouping to dataframe
site.scrs <- cbind(site.scrs, Reaches = otu_table$Reaches)
colnames(site.scrs)
site.scrs

#
site.scrs %>% ggplot(., aes(x=NMDS1, y=NMDS2))+ 
  geom_point(aes(colour = factor(Reaches))) + 
  coord_fixed()+
  theme(legend.position = "right", legend.text = element_text(size = 12),
        legend.title = element_text(size = 12), axis.text = element_text(size = 10))

site.scrs %>% ggplot(., aes(x=NMDS1, y=NMDS2, label=`Site number`)) + 
  geom_point(aes(colour = factor(`Site number`))) + 
  coord_fixed() +  geom_text(hjust=0, vjust=0) +
  theme(legend.position = "right", legend.text = element_text(size = 12),
        legend.title = element_text(size = 12), axis.text = element_text(size = 10))

site.scrs %>% filter(Year == 2020) %>% ggplot(., aes(x=NMDS1, y=NMDS2, label=`Site number`)) + 
  geom_point(aes(colour = factor(`Site number`))) + 
  coord_fixed() +  geom_text(hjust=0, vjust=0) +
  theme(legend.position = "right", legend.text = element_text(size = 12),
        legend.title = element_text(size = 12), axis.text = element_text(size = 10))
 
site.scrs %>% filter(Year == 2015) %>% ggplot(., aes(x=NMDS1, y=NMDS2, label=`Site number`)) + 
  geom_point(aes(colour = factor(`Site number`))) + 
  coord_fixed() +  geom_text(hjust=0, vjust=0) +
  theme(legend.position = "right", legend.text = element_text(size = 12),
        legend.title = element_text(size = 12), axis.text = element_text(size = 10))

site.scrs %>% filter(Year == 2000) %>% ggplot(., aes(x=NMDS1, y=NMDS2, label=`Site number`)) + 
  geom_point(aes(colour = factor(`Site number`))) + 
  coord_fixed() +  geom_text(hjust=0, vjust=0) +
  theme(legend.position = "right", legend.text = element_text(size = 12),
        legend.title = element_text(size = 12), axis.text = element_text(size = 10))

###
# Looking only at beetles

artho_annual_sites
colnames(artho_annual_sites)
artho_annual_sites$numbercode

otu_table_prep_beetle<- artho_annual_sites %>% filter(Year > 1999) %>% 
  filter(class == 1 & order == 02 | family == 55 | family == 46) %>% 
  group_by(`Site number`, `Site name`, Year, numbercode, Latitude, Longitude, Reaches) %>%
  summarise("Annual mean counts by species" = sum(`Annual mean counts`, na.rm=FALSE))
otu_table_prep_beetle
unique(otu_table_prep_beetle$numbercode)


#artho_annual_sites %>% filter(Year > 1999) %>% select(numbercode, class, family, order) %>% 
#  filter(numbercode == 5010000) %>% print(n=500)

otu_table_beetles <- otu_table_prep_beetle %>% select(`Site number`,`Site name`, Year, numbercode,
                                       Latitude, Longitude, Reaches,
                                       `Annual mean counts by species`) %>%
  pivot_wider(names_from = numbercode, values_from = `Annual mean counts by species`) %>% 
  replace(is.na(.), 0)


otu_table_beetles
colnames(otu_table_beetles)

#
#write.table(otu_table_beetles, "data/interim/bemp_surface_beetles_otu_multisite_year.csv",
#            sep=",", row.names = FALSE, quote = FALSE, na = ".")

#

# Here we make the community composition data frame and double check it. 
com_table_beetles = otu_table_beetles[,7:ncol(otu_table_beetles)]
com_table_beetles 
colnames(com_table_beetles)

#com_table_clean <- com_table[, colSums(com_table) != 1]
#com_table_clean <- com_table[rowSums(com_table_clean[])>0,]
#com_table_clean

# Convert the abundance data frame into a matrix for distance measures. 
com_table_matrix_beetles  <- as.matrix(com_table_beetles)
class(com_table_matrix_beetles)
colnames(com_table_matrix_beetles)

# Dumps out the matrix for troubleshooting
#write.table(com_table_matrix_beetles, "data/interim/bemp_surface_active_arthropod_otu_matrix.csv",
#            sep=",", row.names = FALSE, quote = FALSE, na = ".")
# 
nmds_com = metaMDS(com_table_matrix_beetles, distance = "bray", try=30)
nmds_com

plot(nmds_com)

#
# Takes the NMDS scores and adds on the environmental parameters.
site.scrs <- as.data.frame(scores(nmds_com, display = "sites")) #save NMDS results into dataframe
site.scrs <- cbind(site.scrs, "Site number" = otu_table_beetles$`Site number`) #add grouping variable "Management" to dataframe
site.scrs <- cbind(site.scrs, Year = otu_table_beetles$Year) #add grouping variable of cluster grouping to dataframe
site.scrs <- cbind(site.scrs, Reaches = otu_table_beetles$Reaches)
colnames(site.scrs)

#
site.scrs %>% ggplot(., aes(x=NMDS1, y=NMDS2))+ 
  geom_point(aes(colour = factor(Reaches))) + 
  coord_fixed()+
  theme(legend.position = "right", legend.text = element_text(size = 12),
        legend.title = element_text(size = 12), axis.text = element_text(size = 10))

site.scrs %>% ggplot(., aes(x=NMDS1, y=NMDS2))+ 
  geom_point(aes(colour = factor(Year))) + 
  coord_fixed()+
  theme(legend.position = "right", legend.text = element_text(size = 12),
        legend.title = element_text(size = 12), axis.text = element_text(size = 10))

plot2020 <- site.scrs %>% filter(Year == 2020) %>% ggplot(., aes(x=NMDS1, y=NMDS2)) + 
  geom_point(aes(colour = factor(`Site number`))) + 
  coord_fixed() +
  theme(legend.position = "right", legend.text = element_text(size = 12),
        legend.title = element_text(size = 12), axis.text = element_text(size = 10)) +
  xlim(-1,1) + ylim(-1.5,1)
plot2020

site.scrs %>% filter(Year == 2019) %>% ggplot(., aes(x=NMDS1, y=NMDS2, label=`Site number`)) + 
  geom_point(aes(colour = factor(`Site number`))) + 
  coord_fixed() +  geom_text(hjust=0, vjust=0) +
  theme(legend.position = "right", legend.text = element_text(size = 12),
        legend.title = element_text(size = 12), axis.text = element_text(size = 10))

veg.spp.fit <- envfit(nmds_com, com_table_matrix_beetles, permutations = 999)
veg.spp.fit

spp.scrs <- as.data.frame(scores(veg.spp.fit, display = "vectors")) #save species intrinsic values into dataframe
spp.scrs <- cbind(spp.scrs, Species = rownames(spp.scrs))
spp.scrs <- cbind(spp.scrs, pval = veg.spp.fit$vectors$pvals)
spp.scrs

sig.spp.scrs <- subset(spp.scrs, pval<=0.001)
sig.spp.scrs

plot2020 + geom_segment(data = sig.spp.scrs, aes(x = 0, xend=NMDS1, y=0, yend=NMDS2),
             arrow = arrow(length = unit(0.25, "cm")), colour = "grey10", lwd=0.3) + #add vector arrows of significant species
  ggrepel::geom_text_repel(data = sig.spp.scrs, aes(x=NMDS1, y=NMDS2, label = Species),
                           cex = 3, direction = "both", segment.size = 0.25,
                           max.overlaps = 15)+ #add labels for species, use ggrepel::geom_text_repel so that labels do not overlap
  labs(title = "Ordination with species vectors")
