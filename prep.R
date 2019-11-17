library(tidyverse)
library(ggplot2)
library(readxl)


#read in Spotify dataset 

dir.create("raw-data")

music <- read_xlsx("raw-data/top2018.xlsx", skip = 3)
