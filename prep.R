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

#rap songs vs. non rap

rap <- music[c(1:7, 12:13, 16, 19:20, 22, 29, 31, 33, 39, 41, 43, 51, 54, 56, 59, 62:63, 74:77, 80, 82:84, 88, 92, 95),]

non_rap <- music[c(8:11, 14:15, 17:18, 21, 23, 24:28, 30, 32, 34:38, 40, 42, 44:50, 52:53, 55, 57:58, 60:61, 64:73, 78:79, 81, 85:87, 89:91, 93:94, 96:100),]

rap$type <- "Rap"

non_rap$type <- "Non Rap"

music <- rbind(rap, non_rap)

#25 most popular artists

top25 <- music %>% 
  group_by(artists) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq)) %>% 
  slice(1:25) %>% 
  ggplot(., aes(reorder(artists, +freq), freq)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Artist",
       y = "Number of Songs in Top 100",
       title = "25 Most Popular Artists")

print(top25)

#dancebility

dance <- music %>% 
  arrange(desc(danceability)) %>% 
  slice(1:10) %>% 
  ggplot(., aes(reorder(artist_name, +danceability), danceability)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Song",
       y = "Value", 
       title = "Top 10 Most Danceable Songs")

print(dance)

#energy

energy <- music %>% 
  arrange(desc(energy)) %>% 
  slice(1:10) %>% 
  ggplot(aes(reorder(artist_name, +energy), energy)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Song",
       y = "Value",
       title = "Top 10 Most Energenic Songs")

print(energy)  

#loudness

loud <- music %>% 
  arrange(desc(loudness)) %>% 
  slice(1:10) %>% 
  ggplot(aes(reorder(artist_name, +loudness), loudness)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Song",
       y = "Value",
       title = "Top 10 Loudest Songs")

print(loud)


#speechiness 

speech <- music %>% 
  arrange(desc(speechiness)) %>% 
  slice(1:10) %>% 
  ggplot(aes(reorder(artist_name, +speechiness), speechiness)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Song",
       y = "Value",
       title = "Top 10 Most Speech Filled Songs")

print(speech)

#acousticness

acoustic <- music %>% 
  arrange(desc(acousticness)) %>% 
  slice(1:10) %>% 
  ggplot(aes(reorder(artist_name, +acousticness), acousticness)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Song",
       y = "Value",
       title = "Top 10 Most Acoustic Songs")

print(acoustic)

#liveness

live <- music %>% 
  arrange(desc(liveness)) %>% 
  slice(1:10) %>% 
  ggplot(aes(reorder(artist_name, +liveness), liveness)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Song",
       y = "Value",
       title = "Top 10 Songs with Audience Presence")

print(live)

#instrumentalness

instrumental <- music %>% 
  arrange(desc(instrumentalness)) %>% 
  slice(1:5) %>% 
  ggplot(aes(reorder(artist_name, +instrumentalness), instrumentalness)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Song",
       y = "Value",
       title = "Top 5 Most Instrumental Songs")

print(instrumental)


#valence

pos <- music %>% 
  arrange(desc(valence)) %>% 
  slice(1:10) %>% 
  ggplot(aes(reorder(artist_name, +valence), valence)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Song",
       y = "Value",
       title = "Top 10 Most Positive Songs")

print(pos)

neg <- music %>% 
  arrange(valence) %>% 
  slice(1:10) %>% 
  ggplot(aes(reorder(artist_name, +valence), valence)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Song",
       y = "Value",
       title = "Top 10 Most Negative Songs")

print(neg)

#tempo

up <- music %>% 
  arrange(desc(tempo)) %>% 
  slice(1:10) %>% 
  ggplot(aes(reorder(artist_name, +tempo), tempo)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Song",
       y = "Beats per Minute (BPM)",
       title = "Top 10 Most Upbeat Songs")

print(up)

down <- music %>% 
  arrange(tempo) %>% 
  slice(1:10) %>% 
  ggplot(aes(reorder(artist_name, +tempo), tempo)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Song",
       y = "Beats per Minute (BPM)",
       title = "Top 10 Least Upbeat Songs")

print(down)

#song length

long <- music %>% 
  arrange(desc(duration_ms)) %>% 
  slice(1:10) %>% 
  ggplot(aes(reorder(artist_name, +duration_ms), duration_ms)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Song",
       y = "Length (minutes)",
       title = "Top 10 Longest Songs") +
  scale_y_time(labels = function(l) strftime(l, '%M:%S'))

print(long)

short <- music %>% 
  arrange(duration_ms) %>% 
  slice(1:10) %>% 
  ggplot(aes(reorder(artist_name, +duration_ms), duration_ms)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Song",
       y = "Length (minutes)",
       title = "Top 10 Shortest Songs") +
  scale_y_time(labels = function(l) strftime(l, '%M:%S'))

print(short)

#compare rap v nonrap

pie <- as.data.frame(table(music$type))

pie %>% 
  ggplot(aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank())

#boxplots of each category

#danceability

chart1 <- music %>% 
  ggplot(aes(type, danceability, fill = type)) +
  geom_boxplot() +
  labs(x = "",
       y = "Danceability") +
  theme(legend.position = "none")

print(chart1)

#energy

chart2 <- music %>% 
  ggplot(aes(type, energy, fill = type)) +
  geom_boxplot() +
  labs(x = "",
       y = "Energy") +
  theme(legend.position = "none")

print(chart2)

#loud

chart3 <- ggplot(music, aes(type, loudness, fill = type)) +
  geom_boxplot(outlier.size = 1,
               outlier.shape = 20) +
  theme_economist()

print(chart3)

#speech

chart4 <- music %>% 
  ggplot(aes(type, speechiness, fill = type)) +
  geom_boxplot() +
  labs(x = "",
       y = "Speechiness") +
  theme(legend.position = "none")

print(chart4)
