library(ggplot2)
library(dplyr)
library(tidyverse)

# Points at my working folder.
setwd("~/Documents/BEMP/bemp_to_r/arthropod/")

# This reads in a csv that is tab sep.
artho_raw <- read.csv("artho_1997_2016.txt",sep="\t",
                      na.strings=c('NA','.','#VALUE!'))
head(artho_raw)
summary(artho_raw)

# Converts the trap number to an int. 
artho_raw$trap <- as.integer(artho_raw$trap)

# Cleaning up the columns. 
artho <- select(artho_raw, site, year, month, quantity, lettercode, name, family, class, order, genspp, trap)
head(artho)
str(artho)

# Check the trap numbers
unique(artho$trap)

# Pitfall traps are on lines in a BEMP site so we add the line number on for reference.
artho_line_numbers <- artho %>% 
  mutate(line_number = ifelse(trap < 5, 1, 
                              ifelse(trap == 5 | trap < 9, 2,
                                     ifelse(trap == 9 |trap < 13, 3,
                                            ifelse(trap == 13 | trap <17, 4,
                                                   ifelse(trap == 17 | trap < 21, 5, "NA"))))))

artho_line_numbers

# Dumps out the data to a csv
write.table(artho_line_numbers, "bemp_surface_active_arthropod_1997_2016.csv", sep=",",
            row.names = FALSE, quote = FALSE)
