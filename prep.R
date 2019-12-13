# Here is where I read in the raw data, cleaned it up, and played around with
# some graphs I was interested in showing.

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
library(genius)
library(tidyverse)
library(wordcloud)
library(tm)
library(NLP)
library(SnowballC)

#read in Spotify dataset

music <- read_xlsx("raw-data/top2018.xlsx")

music <- music[, -1]

music$artist_name <- with(music, paste(artists, " - ", name))

cat("Rows: ", nrow(music))

#rap songs vs. non rap

rap <-
  music[c(
    1:7,
    12:13,
    16,
    19:20,
    22,
    29,
    31,
    33,
    39,
    41,
    43,
    51,
    54,
    56,
    59,
    62:63,
    74:77,
    80,
    82:84,
    88,
    92,
    95
  ), ]

non_rap <-
  music[c(
    8:11,
    14:15,
    17:18,
    21,
    23,
    24:28,
    30,
    32,
    34:38,
    40,
    42,
    44:50,
    52:53,
    55,
    57:58,
    60:61,
    64:73,
    78:79,
    81,
    85:87,
    89:91,
    93:94,
    96:100
  ), ]

rap$type <- "Rap"

non_rap$type <- "Non Rap"

music <- rbind(rap, non_rap)

write_rds(music, path = "milestone8/music.rds")

# Song lyric import from .Rmd 

gods_plan <- genius_lyrics("drake", "god's plan")

sad <- genius_lyrics("xxxtentacion", "sad!")

rockstar <- genius_lyrics("post malone", "rockstar")

psycho <- genius_lyrics("post malone", "psycho")

in_my_feelings <- genius_lyrics("drake", "in my feelings")

better_now <- genius_lyrics("post malone", "better now")

i_like_it <- genius_lyrics("cardi b", "i like it")

lucid_dreams <- genius_lyrics("juice wrld", "lucid dreams")

nice_for_what <- genius_lyrics("drake", "nice for what")

all_the_stars <- genius_lyrics("kendrick lamar", "all the stars")

moonlight <- genius_lyrics("xxxtentacion", "moonlight")

look_alive <- genius_lyrics("blocboy jb", "look alive")

te_bote <- genius_lyrics("nio garcia, casper magico & bad bunny", "te bote (remix)")

jocelyn_flores <- genius_lyrics("xxxtentacion", "jocelyn flores")

taste <- genius_lyrics("tyga", "taste")

i_fall_apart <- genius_lyrics("post malone", "i fall apart")

changes <- genius_lyrics("xxxtentacion", "changes")

river <- genius_lyrics("eminem", "river")

sicko_mode <- genius_lyrics("travis scott", "sicko mode")

ric_flair_drip <- genius_lyrics("offset & metro boomin", "ric flair drip")

freaky_friday <- genius_lyrics("lil dicky", "freaky friday")

fefe <- genius_lyrics("6ix9ine", "fefe")

xo_tour_Llif3 <- genius_lyrics("lil uzi vert", "xo tour llif3")

nonstop <- genius_lyrics("drake", "nonstop")

fuck_love <- genius_lyrics("xxxtentacion", "fuck love")

walk_it_talk_it <- genius_lyrics("migos", "walk it talk it")

him_n_i <- genius_lyrics("g-eazy", "him & i")

candy_paint <- genius_lyrics("post malone", "candy paint")

congratulations <- genius_lyrics("post malone", "congratulations")

plug_walk <- genius_lyrics("rich the kid", "plug walk")

stir_fry <- genius_lyrics("migos", "stir fry")

humble <- genius_lyrics("kendrick lamar", "humble.")

vaina_loca <- genius_lyrics("ozuna", "vaina loca")

siguelo_bailando <- genius_lyrics("ozuna", "siguelo bailando")

yes_indeed <- genius_lyrics("lil baby & drake", "yes indeed")

everybody_dies_in_their_nightmares <- genius_lyrics("xxxtentacion", "everybody dies in their nightmares")

one_kiss <- genius_lyrics("calvin harris", "one kiss")

idgaf <- genius_lyrics("dua lipa", "idgaf")

friends <- genius_lyrics("marshmello", "friends")

havana <- genius_lyrics("camila cabello", "havana")

girls_like_you <- genius_lyrics("maroon 5", "girls like you")

the_middle <- genius_lyrics("zedd", "the middle")

no_tears_left_to_cry <- genius_lyrics("ariana grande", "no tears left to cry")

x <- genius_lyrics("nicky jam", "x")

these_days <- genius_lyrics("rudimental", "these days")

mine <- genius_lyrics("bazzi", "mine")

youngblood <- genius_lyrics("5 seconds of summer", "youngblood")

new_rules <- genius_lyrics("dua lipa", "new rules")

shape_of_you <- genius_lyrics("ed sheeran", "shape of you")

love_lies <- genius_lyrics("khalid", "love lies")

meant_to_be <- genius_lyrics("bebe rexha", "meant to be")

perfect <- genius_lyrics("ed sheeran", "perfect")

solo <- genius_lyrics("clean bandit", "solo")

nevermind <- genius_lyrics("dennis lloyd", "nevermind")

echame_la_culpa <- genius_lyrics("luis fonsi & demi lovato", "echame la culpa")

eastside <- genius_lyrics("benny blanco", "eastside")

never_be_the_same <- genius_lyrics("camila cabello", "never be the same")

wolves <- genius_lyrics("selena gomez", "wolves")

in_my_mind <- genius_lyrics("dynoro & gigi d'agostino", "in my mind")

dura <- genius_lyrics("daddy yankee", "dura")

thunder <- genius_lyrics("imagine dragons", "thunder")

me_niego <- genius_lyrics("reik", "me niego")

jackie_chan <- genius_lyrics("tiesto", "jackie chan")

finesse <- genius_lyrics("bruno mars", "finesse")

back_to_you <- genius_lyrics("selena gomez", "back to you")

let_you_down <- genius_lyrics("nf", "let you down")

call_out_my_name <- genius_lyrics("the weeknd", "call out my name")

happier <- genius_lyrics("marshmello", "happier")

too_good_at_goodbyes <- genius_lyrics("sam smith", "too good at goodbyes")

believer <- genius_lyrics("imagine dragons", "believer")

rise <- genius_lyrics("jonas blue", "rise")

body <- genius_lyrics("loud luxury", "body")

sin_pijama <- genius_lyrics("becky g", "sin pijama")

two_thousand_two <- genius_lyrics("anne-marie", "2002")

in_my_blood <- genius_lyrics("shawn mendes", "in my blood")

silence <- genius_lyrics("marshmello", "silence")

god_is_a_woman <- genius_lyrics("ariana grande", "god is a woman")

dejala_que_vuelva <- genius_lyrics("piso 21", "dejala que vuelva")

flames <- genius_lyrics("david guetta", "flames")

what_lovers_do <- genius_lyrics("maroon 5", "what lovers do")

taki_taki <- genius_lyrics("dj snake", "taki taki")

let_me_go <- genius_lyrics("hailee steinfeld", "let me go")

feel_it_still <- genius_lyrics("portugal the man", "feel it still")

pray_for_me <- genius_lyrics("the weeknd & kendrick lamar", "pray for me")

one_two_three <- genius_lyrics("sofia reyes", "1, 2, 3")

criminal <- genius_lyrics("natti natasha", "criminal")

lovely <- genius_lyrics("billie eilish", "lovely")

perfect_duet <- genius_lyrics("ed sheeran", "perfect duet")

corazon <- genius_lyrics("maluma", "corazon")

young_dumb_n_broke <- genius_lyrics("khalid", "young dumb & broke")

downtown <- genius_lyrics("anitta", "downtown")

bella <- genius_lyrics("wolfine", "bella")

promises <- genius_lyrics("calvin harris", "promises")

i_like_me_better <- genius_lyrics("lauv", "i like me better")

this_is_me <- genius_lyrics("keala settle", "this is me")

rewrite_the_stars <- genius_lyrics("zac efron", "rewrite the stars")

i_miss_you <- genius_lyrics("clean bandit", "i miss you")

no_brainer <- genius_lyrics("dj khaled", "no brainer")

dusk_till_dawn <- genius_lyrics("zayn", "dusk till dawn")

be_alright <- genius_lyrics("dean lewis", "be alright")

lyrics <- bind_rows(gods_plan, sad, rockstar, psycho, in_my_feelings, better_now, i_like_it, lucid_dreams, nice_for_what, all_the_stars, moonlight, look_alive, te_bote, jocelyn_flores, taste, i_fall_apart, changes, river, sicko_mode, ric_flair_drip, freaky_friday, fefe, xo_tour_Llif3, nonstop, fuck_love, walk_it_talk_it, him_n_i, candy_paint, congratulations, plug_walk, stir_fry, humble, vaina_loca, siguelo_bailando, yes_indeed, everybody_dies_in_their_nightmares, one_kiss, idgaf, friends, havana, girls_like_you, the_middle, no_tears_left_to_cry, x, these_days, mine, youngblood, new_rules, shape_of_you, love_lies, meant_to_be, perfect, solo, nevermind, echame_la_culpa, eastside, never_be_the_same, wolves, in_my_mind, dura, thunder, me_niego, jackie_chan, finesse, back_to_you, let_you_down, call_out_my_name, happier, too_good_at_goodbyes, believer, rise, body, sin_pijama, two_thousand_two, in_my_blood, silence, god_is_a_woman, dejala_que_vuelva, flames, what_lovers_do, taki_taki, let_me_go, feel_it_still, pray_for_me, one_two_three, criminal, lovely, perfect_duet, corazon, young_dumb_n_broke, downtown, bella, promises, i_like_me_better, this_is_me, rewrite_the_stars, i_miss_you, no_brainer, dusk_till_dawn, be_alright) %>% 
  separate_rows(lyric, sep = " ") %>% 
  separate_rows(lyric, sep = "[!-&(-.]") %>% 
  select(lyric)

cloud <- Corpus(VectorSource(lyrics))
cloud2 <- tm_map(cloud, removePunctuation)
cloud2 <- tm_map(cloud2, removeWords, stopwords("english"))

a <- TermDocumentMatrix(cloud2)
b <- as.matrix(a)
c <- sort(rowSums(b), decreasing = TRUE)
wordz <- data.frame(word = names(c), freq = c)

write_rds(wordz, path = "milestone8/wordz.rds")

set.seed(1234)
wordcloud(words = wordz$word, freq = wordz$freq, min.freq = 1, max.words = 100, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "PRGn"))

#25 most popular artists

top25 <- music %>%
  group_by(artists) %>%
  summarise(freq = n()) %>%
  arrange(desc(freq)) %>%
  slice(1:25) %>%
  ggplot(., aes(reorder(artists,+freq), freq)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = "Greens") +
  coord_flip() +
  labs(x = "Artist",
       y = "Number of Songs in Top 100",
       title = "25 Most Popular Artists")

print(top25)

#dancebility

dance <- music %>%
  arrange(desc(danceability)) %>%
  slice(1:10) %>%
  ggplot(., aes(reorder(artist_name,+danceability), danceability)) +
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
  ggplot(aes(reorder(artist_name,+energy), energy)) +
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
  ggplot(aes(reorder(artist_name,+loudness), loudness)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Song",
       y = "Value",
       title = "Top 10 Loudest Songs")

print(loud)

soft <- music %>% 
  arrange(loudness) %>% 
  slice(1:10) %>% 
  ggplot(aes(reorder(artist_name, +loudness), loudness)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Song",
       y = "Value",
       title = "Top 10 Softest Songs")

print(soft)

loudness <- grid.arrange(loud, soft)

#speechiness

speech <- music %>%
  arrange(desc(speechiness)) %>%
  slice(1:10) %>%
  ggplot(aes(reorder(artist_name,+speechiness), speechiness)) +
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
  ggplot(aes(reorder(artist_name,+acousticness), acousticness)) +
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
  ggplot(aes(reorder(artist_name,+liveness), liveness)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Song",
       y = "Value",
       title = "Top 10 Songs with Audience Presence")

print(live)

#instrumentalness

instrumental <- music %>%
  arrange(desc(instrumentalness)) %>%
  slice(1:10) %>%
  ggplot(aes(reorder(artist_name,+instrumentalness), instrumentalness)) +
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
  ggplot(aes(reorder(artist_name,+valence), valence)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Song",
       y = "Value",
       title = "Top 10 Most Positive Songs")

print(pos)

neg <- music %>%
  arrange(valence) %>%
  slice(1:10) %>%
  ggplot(aes(reorder(artist_name,+valence), valence)) +
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
  ggplot(aes(reorder(artist_name,+tempo), tempo)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Song",
       y = "Beats per Minute (BPM)",
       title = "Top 10 Most Upbeat Songs")

print(up)

down <- music %>%
  arrange(tempo) %>%
  slice(1:10) %>%
  ggplot(aes(reorder(artist_name,+tempo), tempo)) +
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
  ggplot(aes(reorder(artist_name,+duration_ms), duration_ms)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Song",
       y = "Length (minutes)",
       title = "Top 10 Longest Songs") +
  scale_y_time(
    labels = function(l)
      strftime(l, '%M:%S')
  )

print(long)

short <- music %>%
  arrange(duration_ms) %>%
  slice(1:10) %>%
  ggplot(aes(reorder(artist_name,+duration_ms), duration_ms)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Song",
       y = "Length (minutes)",
       title = "Top 10 Shortest Songs") +
  scale_y_time(
    labels = function(l)
      strftime(l, '%M:%S')
  )

print(short)

#compare rap v nonrap

pie <- as.data.frame(table(music$type))

write_rds(pie, path = "milestone8/pie.rds")

pie %>%
  ggplot(aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "Greens") +
  theme(
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid = element_blank(),
    
)

#boxplots of each category

#danceability

chart1 <- music %>%
  ggplot(aes(type, danceability, fill = type)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Greens") +
  labs(x = "",
       y = "Danceability") +
  theme(legend.position = "none")

print(chart1)

#energy

chart2 <- music %>%
  ggplot(aes(type, energy, fill = type)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Greens") +
  labs(x = "",
       y = "Energy") +
  theme(legend.position = "none",
        plot.background =  element_rect(fill = "black")) +
  theme_dark()

print(chart2)

#loud

chart3 <- music %>%
  ggplot(aes(type, loudness, fill = type)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Greens") +
  labs(x = "",
       y = "Loudness") +
  theme(legend.position = "none")

print(chart3)

#speech

chart4 <- music %>%
  ggplot(aes(type, speechiness, fill = type)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Greens") +
  labs(x = "",
       y = "Speechiness") +
  theme(legend.position = "none")

print(chart4)

#acousticness

chart5 <- music %>%
  ggplot(aes(type, acousticness, fill = type)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Greens") +
  labs(x = "",
       y = "Acousticness") +
  theme(legend.position = "none")

print(chart5)

#liveness

chart6 <- music %>%
  ggplot(aes(type, liveness, fill = type)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Greens") +
  labs(x = "",
       y = "Liveness") +
  theme(legend.position = "none")

print(chart6)

#valence

chart7 <- music %>%
  ggplot(aes(type, valence, fill = type)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Greens") +
  labs(x = "",
       y = "Valence") +
  theme(legend.position = "none")

print(chart7)

#tempo

chart8 <- music %>%
  ggplot(aes(type, tempo, fill = type)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Greens") +
  labs(x = "",
       y = "Tempo") +
  theme(legend.position = "none")

print(chart8)

#group them together

grid.arrange(chart1,
             chart2,
             chart3,
             chart4,
             chart5,
             chart6,
             chart7,
             chart8,
             ncol = 2)

#length rap v non

music %>%
  ggplot(aes((duration_ms / 1000) / 60, fill = type)) +
  geom_density(alpha = 0.5) +
  scale_fill_brewer(palette = "Greens") +
  labs(x = "Length (minutes)",
       y = "") +
  scale_x_continuous(limits = c(0, 8), breaks = seq(0, 8, 1)) +
  guides(fill = guide_legend(title = "Type of Song"))

#correlation between variables

corrplot(
  cor(music[c(3, 4, 6, 8, 9, 10, 11, 12, 13, 14)]),
  method = "color",
  type = "upper",
  col = brewer.pal(n = 10, name = "Greens"),
  tl.col = "black",
  tl.srt = 90,
  addCoef.col = "gray8",
  diag = T,
  number.cex = 0.65,
  order = "alphabet"
) 


#regression
music %>% 
  ggplot(aes(loudness, energy))+
  geom_point(colour = "black", shape = 21, size = 3, aes(fill = factor(type)))+ 
  scale_fill_brewer(palette = "Greens")+
  geom_smooth(method = lm)+
  annotate("text", x = -9.3, y = 0.85, label = "italic(r) == 0.73", parse = T, size = 6, col = "gray20")+
  labs(x = "Loudness", y = "Energy")+
  theme_economist()+
  theme(axis.text.x = element_text(size=10), axis.text.y = element_text(size=10), legend.position = "right")+
  guides(fill = guide_legend(title = "Type of song")) +
  theme_dark()

