library(tidyverse)
library(ggplot2)
library(readxl)
options(warn = -1)
library(dplyr, warn.conflicts = F)
library(RColorBrewer)
library(corrplot)
library(ggthemes)
library(fmsb)
library(magrittr, warn.conflicts = F)
library(gridExtra, warn.conflicts = F)

#read in Spotify dataset 

music <- read_xlsx("raw-data/top2018.xlsx")

music <- music[,-1]

music$artist_name <- with(music, paste(artists, " - ", name))

cat("Rows: ", nrow(music))

#music analysis

music %>% 
  group_by(artists) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq)) %>% 
  slice(1:25) %>% 
  ggplot(., aes(reorder(artists + freq), freq)) + 
  geom_bar(stat = "identity", fill = "deepskyblue3", col = "grey20") +
  coord_flip() +
  labs(x = "", 
       y = "Number of Songs in Top 100", 
       title = "25 Most Popular Artists")
