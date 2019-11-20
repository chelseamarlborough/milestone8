


library(ggplot2)
library(shiny)
library(shinythemes)
library(plotly)
library(scales)


music <- read_rds("music.rds")

pie <- read_rds("pie.rds")

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

chart_features <- c("Danceable" = "danceability",
                    "Energetic" = "energy",
                    "Loud" = "loudness",
                    "Speechy" = "speechiness",
                    "Acoustic"= "acousticness",
                    "Live" = "liveness",
                    "Positive" = "valence",
                    "Up-Beat" = "tempo",
                    "Long" = "duration_ms")

ui <- fluidPage(
  theme = shinytheme("slate"),

  br(),

  navbarPage(
    "Spotify Top 100 2018 Data",
    tabPanel(
      "Song Characteristcs",
      tabsetPanel(
        tabPanel(
          "Top 25 Artists of 2018",
          h3("How did Artist Perform on the Spotify Top 100 Chart?"),
          br(),
          sidebarLayout(
            sidebarPanel(
              h6(
                "XXXTENTACION and Post Malone both had six songs on the Top 100 Chart."
              ),
              h6(
                "Post Malone had release a new album 'Beerbongs and Bentleys' in 2018. XXXTENTACION released two ablums in 2018, 'Skins' and '?'. XXXTENTACION was also murdered in 2018. These factors may account for a spike in their music."
              )
            ),
            mainPanel(
              plotOutput("top25"),
              br()
            ),
          ),
          br()
        ),
        tabPanel(
          "Audio Features",
          br(),
          sidebarLayout(
              sidebarPanel(
                  h6(
                      "Acousticness is measured on a confidence interval of 0.0 to 1.0. The closer the confidence interval is to 1.0, the more acoustic a song is."
                  ),
                  selectInput(
                      inputId = "most_least",
                      label = "idk",
                      choices = c("Most", "Least")
                  ),
                  selectInput(
                      inputId = "chart_features",
                      label = "What chart do you want to create?",
                      choices = chart_features
                  )
          ), 
          mainPanel(
              plotOutput("chart_plot"),
              br()
          )
          )
        )
      )
    ),
    tabPanel(
      "Rap Music",
      tabsetPanel(
        tabPanel(
          "Overall Distribution",
          h3("How often did the Rap Genre Appear on the Top 100 Chart?"),
          sidebarLayout(
            sidebarPanel(
              selectInput(
                inputId = "audio_variables",
                label = "What do you want to compare?",
                choices = audio_choices
              )
            ),
            mainPanel(
              plotOutput(
                "rap_plot"
              )
            )
          )
        )
      )
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$rap_plot <- renderPlot({
    ggplot(music, aes(type, get(input$audio_variables), fill = type)) +
      geom_boxplot() +
      scale_fill_brewer(palette = "Greens") +
      labs(
        x = "",
        y = names(audio_choices[which(audio_choices == input$audio_variables)])
      ) +
      theme(legend.position = "none")
  })
  
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

chart_units <- reactive({
    if (input$chart_features == "tempo") {
        chart_units <- "BPM"
    }
})
  
  output$chart_plot <- renderPlot({
      ggplot(chart_plot_data(), aes(reorder(artist_name, +get(input$chart_features)), get(input$chart_features))) +
          geom_bar(stat = "identity") +
          coord_flip() +
          labs(x = "Artist - Song",
               y = "Value") + 
          scale_y_continuous(labels = unit_format(unit = chart_units()))
  })

  output$top25 <- renderPlot({
    music %>%
      group_by(artists) %>%
      summarise(freq = n()) %>%
      arrange(desc(freq)) %>%
      slice(1:25) %>%
      ggplot(., aes(reorder(artists, +freq), freq)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = "Greens") +
      coord_flip() +
      labs(
        x = "Artist",
        y = "Number of Songs in Top 100",
        title = "25 Most Popular Artists"
      )
  })

  output$dance <- renderPlot({
    music %>%
      arrange(desc(danceability)) %>%
      slice(1:10) %>%
      ggplot(., aes(reorder(artist_name, +danceability), danceability)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(
        x = "Song",
        y = "Value",
        title = "Top 10 Most Danceable Songs"
      )
  })

  output$energy <- renderPlot({
    music %>%
      arrange(desc(energy)) %>%
      slice(1:10) %>%
      ggplot(aes(reorder(artist_name, +energy), energy)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(
        x = "Song",
        y = "Value",
        title = "Top 10 Most Energenic Songs"
      )
  })

  output$loud <- renderPlot({
    music %>%
      arrange(desc(loudness)) %>%
      slice(1:10) %>%
      ggplot(aes(reorder(artist_name, +loudness), loudness)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(
        x = "Song",
        y = "Value",
        title = "Top 10 Loudest Songs"
      )
  })

  output$soft <- renderPlot({
    music %>%
      arrange(loudness) %>%
      slice(1:10) %>%
      ggplot(aes(reorder(artist_name, +loudness), loudness)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(
        x = "Song",
        y = "Value",
        title = "Top 10 Softest Songs"
      )
  })

  output$speech <- renderPlot({
    music %>%
      arrange(desc(speechiness)) %>%
      slice(1:10) %>%
      ggplot(aes(reorder(artist_name, +speechiness), speechiness)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(
        x = "Song",
        y = "Value",
        title = "Top 10 Most Speech Filled Songs"
      )
  })

  output$acoustic <- renderPlot({
    music %>%
      arrange(desc(acousticness)) %>%
      slice(1:10) %>%
      ggplot(aes(reorder(artist_name, +acousticness), acousticness)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(
        x = "Song",
        y = "Value",
        title = "Top 10 Most Acoustic Songs"
      )
  })

  output$live <- renderPlot({
    music %>%
      arrange(desc(liveness)) %>%
      slice(1:10) %>%
      ggplot(aes(reorder(artist_name, +liveness), liveness)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(
        x = "Song",
        y = "Value",
        title = "Top 10 Songs with Audience Presence"
      )
  })

  output$instrumental <- renderPlot({
    music %>%
      arrange(desc(instrumentalness)) %>%
      slice(1:5) %>%
      ggplot(aes(reorder(artist_name, +instrumentalness), instrumentalness)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(
        x = "Song",
        y = "Value",
        title = "Top 5 Most Instrumental Songs"
      )
  })

  output$pos <- renderPlot({
    music %>%
      arrange(desc(valence)) %>%
      slice(1:10) %>%
      ggplot(aes(reorder(artist_name, +valence), valence)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(
        x = "Song",
        y = "Value",
        title = "Top 10 Most Positive Songs"
      )
  })

  output$neg <- renderPlot({
    music %>%
      arrange(valence) %>%
      slice(1:10) %>%
      ggplot(aes(reorder(artist_name, +valence), valence)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(
        x = "Song",
        y = "Value",
        title = "Top 10 Most Negative Songs"
      )
  })

  output$up <- renderPlot({
    music %>%
      arrange(desc(tempo)) %>%
      slice(1:10) %>%
      ggplot(aes(reorder(artist_name, +tempo), tempo)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(
        x = "Song",
        y = "Beats per Minute (BPM)",
        title = "Top 10 Most Upbeat Songs"
      )
  })

  output$down <- renderPlot({
    music %>%
      arrange(tempo) %>%
      slice(1:10) %>%
      ggplot(aes(reorder(artist_name, +tempo), tempo)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(
        x = "Song",
        y = "Beats per Minute (BPM)",
        title = "Top 10 Least Upbeat Songs"
      )
  })

  output$long <- renderPlot({
    music %>%
      arrange(desc(duration_ms)) %>%
      slice(1:10) %>%
      ggplot(aes(reorder(artist_name, +duration_ms), duration_ms)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(
        x = "Song",
        y = "Length (minutes)",
        title = "Top 10 Longest Songs"
      ) +
      scale_y_time(
        labels = function(l)
          strftime(l, "%M:%S")
      )
  })

  output$short <- renderPlot({
    music %>%
      arrange(duration_ms) %>%
      slice(1:10) %>%
      ggplot(aes(reorder(artist_name, +duration_ms), duration_ms)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(
        x = "Song",
        y = "Length (minutes)",
        title = "Top 10 Shortest Songs"
      ) +
      scale_y_time(
        labels = function(l)
          strftime(l, "%M:%S")
      )
  })

  output$chart1 <- renderPlot({
    music %>%
      ggplot(aes(type, danceability, fill = type)) +
      geom_boxplot() +
      scale_fill_brewer(palette = "Greens") +
      labs(
        x = "",
        y = "Danceability"
      ) +
      theme(legend.position = "none")
  })

  output$chart2 <- renderPlot({
    music %>%
      ggplot(aes(type, energy, fill = type)) +
      geom_boxplot() +
      scale_fill_brewer(palette = "Greens") +
      labs(
        x = "",
        y = "Energy"
      ) +
      theme(legend.position = "none")
  })

  output$chart3 <- renderPlot({
    music %>%
      ggplot(aes(type, loudness, fill = type)) +
      geom_boxplot() +
      scale_fill_brewer(palette = "Greens") +
      labs(
        x = "",
        y = "Loudness"
      ) +
      theme(legend.position = "none")
  })

  output$chart4 <- renderPlot({
    music %>%
      ggplot(aes(type, speechiness, fill = type)) +
      geom_boxplot() +
      scale_fill_brewer(palette = "Greens") +
      labs(
        x = "",
        y = "Speechiness"
      ) +
      theme(legend.position = "none")
  })

  output$chart5 <- renderPlot({
    music %>%
      ggplot(aes(type, acousticness, fill = type)) +
      geom_boxplot() +
      scale_fill_brewer(palette = "Greens") +
      labs(
        x = "",
        y = "Acousticness"
      ) +
      theme(legend.position = "none")
  })

  output$chart6 <- renderPlot({
    music %>%
      ggplot(aes(type, liveness, fill = type)) +
      geom_boxplot() +
      scale_fill_brewer(palette = "Greens") +
      labs(
        x = "",
        y = "Liveliness"
      ) +
      theme(legend.position = "none")
  })

  output$chart7 <- renderPlot({
    music %>%
      ggplot(aes(type, valence, fill = type)) +
      geom_boxplot() +
      scale_fill_brewer(palette = "Greens") +
      labs(
        x = "",
        y = "Valence"
      ) +
      theme(legend.position = "none")
  })

  output$chart8 <- renderPlot({
    music %>%
      ggplot(aes(type, tempo, fill = type)) +
      geom_boxplot() +
      scale_fill_brewer(palette = "Greens") +
      labs(
        x = "",
        y = "Tempo"
      ) +
      theme(legend.position = "none")
  })

  output$length <- renderPlot({
    music %>%
      ggplot(aes((duration_ms / 1000) / 60, fill = type)) +
      geom_density(alpha = 0.5) +
      scale_fill_brewer(palette = "Greens") +
      labs(
        x = "Length (minutes)",
        y = ""
      ) +
      scale_x_continuous(limits = c(0, 8), breaks = seq(0, 8, 1)) +
      guides(fill = guide_legend(title = "Type of Song"))
  })

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
        panel.grid = element_blank()
      )
  })

  output$loudenergy <- renderPlot({
    music %>%
      ggplot(aes(loudness, energy)) +
      geom_point(colour = "black", shape = 21, size = 3, aes(fill = factor(type))) +
      scale_fill_brewer(palette = "Greens") +
      geom_smooth(method = lm) +
      annotate("text", x = -9.3, y = 0.85, label = "italic(r) == 0.73", parse = T, size = 6, col = "gray20") +
      labs(x = "Loudness", y = "Energy") +
      theme_economist() +
      theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), legend.position = "right") +
      guides(fill = guide_legend(title = "Type of song"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)