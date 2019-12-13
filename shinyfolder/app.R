library(corrplot)
library(tidyverse)
library(ggplot2)
library(shiny)
library(shinythemes)
library(plotly)
library(scales)
library(RColorBrewer)
library(ggthemes)
library(wordcloud)

# Read in data from rds created in prep.R

music <- read_rds("music.rds")

pie <- read_rds("pie.rds")

wordz <- read_rds("wordz.rds")

# Needed to create lists for different calls of reactive graphs. These are
# options people will be able to choose from when wanting to view a graph.

audio_choices <- c(
  "Danceability" = "danceability",
  "Energy" = "energy",
  "Loudness" = "loudness",
  "Speechiness" = "speechiness",
  "Acousticness" = "acousticness",
  "Liveness" = "liveness",
  "Valence" = "valence",
  "Tempo" = "tempo"
)

chart_features <- c(
  "Danceable" = "danceability",
  "Energetic" = "energy",
  "Loud" = "loudness",
  "Speechy" = "speechiness",
  "Acoustic" = "acousticness",
  "Live" = "liveness",
  "Positive" = "valence",
  "Up-Beat" = "tempo",
  "Long" = "duration_ms"
)

ui <- fluidPage(
  # added a theme to create a dark background.
  
  theme = shinytheme("slate"),
  
  # navbarPage allows me to create different tabs for people to select and
  # organize my data better.
  
  navbarPage(
    "Spotify - Top Tracks 2018 Data",
    
    # I first organized the data to present the chart overall. Within each
    # subpanel, I break down differnet variables of the chart.
    
    tabPanel("Song Characteristcs",
             tabsetPanel(
               tabPanel(
                 "Top 25 Artists of 2018",
                 h3("How did Artist Compete on the Spotify Top Tracks Chart?"),
                 sidebarLayout(sidebarPanel(
                   h6(
                     "XXXTENTACION and Post Malone both had six songs on the Top 100 Chart."
                   ),
                   h6(
                     "Post Malone had release a new album 'Beerbongs and Bentleys' in 2018. XXXTENTACION released two ablums in 2018, 'Skins' and '?'. XXXTENTACION was also murdered in 2018. These factors may account for a spike in their music."
                   )
                 ),
                 mainPanel(# Ideally, I would like to make the background black, the bars
                   # green, and writing white to pop off of the screen. Working on
                   # making this happen for the final version.
                   
                   plotOutput("top25")))
               ),
               tabPanel("Audio Features",
                        sidebarLayout(
                          sidebarPanel(
                            # I would like to add the Spotify defnitions of each term to match
                            # when the graph pops up but I'm not sure how to do that or if
                            # making a definition page alone would be better.
                            
                            # selectInput allows me to create options for viewers to select.
                            
                            selectInput(
                              inputId = "most_least",
                              label = "What audio feature would you like to see?",
                              choices = c("Most", "Least")
                            ),
                            selectInput(
                              inputId = "chart_features",
                              label = "",
                              choices = chart_features
                            ),
                            h6("Tempo (up-beat) is measure in beats per minute (BPM)."),
                            h6(
                              "Duration (long) is normally measured in minutes and seconds, but in this case it's measure in milliseconds."
                            )
                          ),
                          mainPanel(plotOutput("chart_plot"))
                        ))
             )),
    tabPanel(#This is where I compare rap music to other genres featured on the chart.
      
      "Rap Music",
      tabsetPanel(
        tabPanel(
          "Overall Distribution",
          h2("How often did rap appear on the Top Tracks Chart?"),
          mainPanel(plotOutput("piechart")),
          h3(
            "The rap genre appeard thirty-six times on the Top 100 chart. This is likely due to Hip Hop's ",
            a("heavy influence on 21st century music", href = "https://www.britannica.com/art/hip-hop/Hip-hop-in-the-21st-century"),
            " ."
          )
        ),
        tabPanel(
          "Audio Features",
          h3("How did rap audio features compare to other genres?"),
          sidebarLayout(sidebarPanel(
            selectInput(
              inputId = "audio_variables",
              label = "What do you want to compare?",
              choices = audio_choices
            )
          ),
          mainPanel(plotOutput("rap_plot")))
        ),
        tabPanel(
          "Song Duration",
          h3("How long are rap songs compared to other genres?"),
          mainPanel(plotOutput("length"),
                    h6("It was interesting to see that non-rap genre songs have a greater density of song length than rap. Rap songs tend to have a wider distribution of length."))
        )
      )),
    tabPanel(# This is where viewers can explore the correlation between each variable.
      # I find it interesting to see how they work together.
      
      "Correlation",
      tabsetPanel(
        tabPanel(
          "Audio Features Chart",
          h3("How do audio variables correlate to one another?"),
          mainPanel(
            plotOutput("corr"),
            h3(
              "Correlation is a statistical technique used to show how strongly variables are related to one another. Correlation is measured on a scale of 0 to 1. The closer the correlation is to one, the strong the relation between variables. The closer to zero, the weaker the relation."
            )
          )
        ),
        tabPanel(
          "Feature by Feature",
          h3("How do individual features correlate to one another?"),
          sidebarLayout(
            sidebarPanel(
              # Here is where I made my project a little more interactive and
              # allowed viewers to choose elements to view.
              
              selectInput(
                inputId = "x_cor",
                label = "What do you want as your x variable?",
                choices = audio_choices
              ),
              selectInput(
                inputId = "y_cor",
                label = "What do you want as your y variable?",
                choices = audio_choices
              )
            ),
            mainPanel(
              plotlyOutput("cor_plot"),
              h3(
                "The blue line represents the regression trend between each variable across the entire chart. Each individual dot specifies a singular song that is color coordinated based on genre, as defined in the legend."
              )
            )
          )
        )
      )),
    
    # This is where the wordcloud is introduced.
    
    tabPanel("Common Words",
             mainPanel(
               h3("What words were commonly used by artists in the Top Tracks chart?"),
               plotOutput("lyriccloud"),
               h3(
                 "This visual includes 100 of the most common words used in song lyrics. There are a significant number of references to profanity and derogatory language. I chose to include these because I felt that the representation of the data would be extremely inaccurate with that kind of manipulation."
               )
             )),
    
    # I debated where I wanted to include this information but I found that it
    # would be most useful in its own section because of how much detail was
    # included.
    
    tabPanel(
      "Audio Feature Definitions",
      mainPanel(
        h5(
          strong("Acousticess:"),
          "A confidence measure from 0.0 to 1.0 of whether the track is acoustic. 1.0 represents high confidence the track is acoustic."
        ),
        h5(
          strong("Dancebility:"),
          "Danceability describes how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability, beat strength, and overall regularity. A value of 0.0 is least danceable and 1.0 is most danceable."
        ),
        h5(strong("Duration:"), "The duration of the track in milliseconds."),
        h5(
          strong("Energy:"),
          "Energy is a measure from 0.0 to 1.0 and represents a perceptual measure of intensity and activity. Typically, energetic tracks feel fast, loud, and noisy. For example, death metal has high energy, while a Bach prelude scores low on the scale. Perceptual features contributing to this attribute include dynamic range, perceived loudness, timbre, onset rate, and general entropy."
        ),
        h5(
          strong("Instrumentalness:"),
          "Predicts whether a track contains no vocals. “Ooh” and “aah” sounds are treated as instrumental in this context. Rap or spoken word tracks are clearly “vocal”. The closer the instrumentalness value is to 1.0, the greater likelihood the track contains no vocal content. Values above 0.5 are intended to represent instrumental tracks, but confidence is higher as the value approaches 1.0."
        ),
        h5(
          strong("Liveliness:"),
          "Detects the presence of an audience in the recording. Higher liveness values represent an increased probability that the track was performed live. A value above 0.8 provides strong likelihood that the track is live."
        ),
        h5(
          strong("Loudness:"),
          "The overall loudness of a track in decibels (dB). Loudness values are averaged across the entire track and are useful for comparing relative loudness of tracks. Loudness is the quality of a sound that is the primary psychological correlate of physical strength (amplitude). Values typical range between -60 and 0 db."
        ),
        h5(
          strong("Speechiness:"),
          "Speechiness detects the presence of spoken words in a track. The more exclusively speech-like the recording (e.g. talk show, audio book, poetry), the closer to 1.0 the attribute value. Values above 0.66 describe tracks that are probably made entirely of spoken words. Values between 0.33 and 0.66 describe tracks that may contain both music and speech, either in sections or layered, including such cases as rap music. Values below 0.33 most likely represent music and other non-speech-like tracks."
        ),
        h5(
          strong("Tempo:"),
          "The overall estimated tempo of a track in beats per minute (BPM). In musical terminology, tempo is the speed or pace of a given piece and derives directly from the average beat duration."
        ),
        h5(
          strong("Valence:"),
          "A measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track. Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), while tracks with low valence sound more negative (e.g. sad, depressed, angry)."
        ),
        h6(
          "Definitions are courtesy of ",
          a("Spotify for Developers", href = "https://developer.spotify.com/documentation/web-api/reference/tracks/get-several-audio-features/"),
          "."
        )
      )
    ),
    
    # Finally, this where viewers can find information about myself and other
    # details about the project.
    
    tabPanel("About",
             mainPanel(
               h2("The Data"),
               h5(
                 "These visualizations on data from ",
                 a("Spotify - Top Tracks of 2018", href = "https://open.spotify.com/playlist/37i9dQZF1DX1HUbZS4LEyL"),
                 "."
               ),
               h5(
                 "The cleaned dataset was found on ",
                 a("Kaggle.com", href = "https://www.kaggle.com/"),
                 "."
               ),
               h5(
                 "Lyrics were scrapped and cleaned by me from data found on ",
                 a("Genius.com", href = "https://genius.com/"),
                 "."
               ),
               h2("About Me: Chelsea Marlborough"),
               h5(
                 "I am a Harvard undergraduate studying government and data science. I am a member of the Harvard Women's Lacrosse Team and Marketing Chair for Women of Harvard Athletics."
               ),
               h5(
                 "Contact me at chelseamarlborough@college.harvard.edu or connect with me on ",
                 a("LinkedIn", href = "https://www.linkedin.com/in/chelseamarlborough/"),
                 "."
               ),
               h2("Source Code"),
               h5(
                 "The source code for this Shiny App can be found on my ",
                 a("GitHub", href = "https://github.com/chelseamarlborough/spotify-top-100-tracks-2018"),
                 "."
               )
               
             ))
  )
)

# Define server logic required to draw the graphs

server <- function(input, output, session) {
  # This graph is used for comparing rap to other genres.
  
  output$rap_plot <- renderPlot({
    ggplot(music, aes(type, get(input$audio_variables), fill = type)) +
      geom_boxplot() +
      scale_fill_brewer(palette = "Greens") +
      labs(x = "",
           y = names(audio_choices[which(audio_choices == input$audio_variables)])) +
      theme(legend.position = "none") +
      theme_dark()
  })
  
  # This if statement were needed in order to change th graph when different
  # variables were selected.
  
  chart_plot_data <- reactive({
    req(input$most_least)
    if (input$most_least == "Most") {
      chart_plot_data <- music %>%
        arrange(desc(get(input$chart_features))) %>%
        slice(1:5)
    } else {
      chart_plot_data <- music %>%
        arrange(get(input$chart_features)) %>%
        slice(1:5)
    }
    return(chart_plot_data)
  })
  
  
  # This graph is to represent different audio values for the whole Top 100 Chart.
  
  output$chart_plot <- renderPlot({
    ggplot(chart_plot_data(), aes(reorder(
      artist_name,+get(input$chart_features)
    ), get(input$chart_features))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(x = "Artist - Song",
           y = "Value")
  })
  
  # This graph represents the top 25 artist of the chart.
  
  output$top25 <- renderPlot({
    music %>%
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
           title = "25 Most Popular Artists") +
      theme_dark()
  })
  
  # This graph compares the duration of rap songs to other genres.
  
  output$length <- renderPlot({
    music %>%
      ggplot(aes((duration_ms / 1000) / 60, fill = type)) +
      geom_density(alpha = 0.5) +
      scale_fill_brewer(palette = "Greens") +
      labs(x = "Length (minutes)",
           y = "") +
      scale_x_continuous(limits = c(0, 8), breaks = seq(0, 8, 1)) +
      guides(fill = guide_legend(title = "Type of Song")) +
      theme_dark()
  })
  
  # This graph represents the correlation between all of the variables in one
  # image.
  
  output$corr <- renderPlot({
    corrplot(
      cor(music[c(3, 4, 6, 8, 9, 10, 11, 12, 13, 14)]),
      method = "color",
      type = "upper",
      col = brewer.pal(n = 9, name = "Greens"),
      tl.col = "black",
      tl.srt = 90,
      addCoef.col = "gray8",
      diag = T,
      number.cex = 0.65,
      order = "alphabet"
    )
  })
  
  # This graph is a pie chart showing how often rap showed up in the chart
  # compared to other genres.
  
  output$piechart <- renderPlot({
    pie %>%
      ggplot(aes(x = "", y = Freq, fill = Var1)) +
      geom_bar(stat = "identity") +
      coord_polar("y", start = 0) +
      scale_fill_brewer(palette = "Greens") +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_blank()
      )
  })
  
  # This graph is a template of how I want my regressions to work.
  
  output$cor_plot <- renderPlotly({
    data_to_plot <- music %>%
      mutate(correlation = cor(get(input$x_cor), get(input$y_cor)))
    
    # The below commands allowed me to display information when hovering over
    # different dots, or in this case songs.
    
    labels <- paste0(
      "r = ",
      round(data_to_plot$correlation, 3),
      "<br> Song: ",
      data_to_plot$name,
      "<br> Artist: ",
      data_to_plot$artists
    ) %>%
      lapply(htmltools::HTML)
    
    
    plot <- music %>%
      ggplot(aes(get(input$x_cor), get(input$y_cor), text = labels)) +
      geom_point(
        colour = "black",
        shape = 21,
        size = 3,
        aes(fill = factor(type))
      ) +
      scale_fill_brewer(palette = "Greens") +
      geom_smooth(method = lm) +
      labs(x = names(audio_choices[which(audio_choices == input$x_cor)]),
           y = names(audio_choices[which(audio_choices == input$y_cor)])) +
      theme_economist() +
      theme(
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "right"
      ) +
      guides(fill = guide_legend(title = "Type of song")) +
      theme_dark()
    
    ggplotly(plot, tooltip = "text")
  })
  
  # This my wordcloud of popular lyrics.
  
  output$lyriccloud <- renderPlot({
    set.seed(1234)
    
    wordcloud(
      words = wordz$word,
      freq = wordz$freq,
      min.freq = 1,
      max.words = 100,
      random.order = FALSE,
      rot.per = 0.35,
      colors = brewer.pal(8, "PRGn")
    )
  })
}

# Run the application

shinyApp(ui = ui, server = server)
