# For the final version, I would like to make some aesthetic changes that I
# mention below. I will also be adding in more regressions, and comments in the
# side pannel to clarify things for the viewer. Also, in my .Rmd I am working
# with scraping genuis lyrics in order to create a word cloud of commonly used
# words amongst songs to incorporate a second data set. I also need to add my
# about page.

library(corrplot)
library(tidyverse)
library(ggplot2)
library(shiny)
library(shinythemes)
library(plotly)
library(scales)
library(RColorBrewer)
library(ggthemes)

# Read in data from rds created in prep.R

music <- read_rds("music.rds")

pie <- read_rds("pie.rds")

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
    "Spotify Top 100 2018 Data",
    
    # I first organized the data to present the chart overall. Within each
    # subpanel, I break down differnet variables of the chart.
    
    tabPanel(
      "Song Characteristcs",
      tabsetPanel(
        tabPanel(
          "Top 25 Artists of 2018",
          h3("How did Artist Perform on the Spotify Top 100 Chart?"),
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
              
              # Ideally, I would like to make the background black, the bars
              # green, and writing white to pop off of the screen. Working on
              # making this happen for the final version.
              
              plotOutput("top25")
            )
          )
        ),
        tabPanel(
          "Audio Features",
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
              )
            ),
            mainPanel(
              
              # I was able to get BPM for tempo, but I am trying to figure out
              # how to convert the seconds to minutes and seconds for duration.
              
              plotOutput("chart_plot")
            )
          )
        )
      )
    ),
    tabPanel(
      
      #This is where I compare rap music to other genres featured on the chart.
      
      "Rap Music",
      tabsetPanel(
        tabPanel(
          "Overall Distribution",
          h3("How often did rap appear in the Top 100 Chart?"),
          mainPanel(
            
            #Again, I would like to make the background black here and writing
            #white, along with the rest of the project.
            
            plotOutput("piechart")
          )
        ),
        tabPanel(
          "Audio Features",
          h3("How did rap audio features compare to other genres?"),
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
        ),
        tabPanel(
          "Song Duration",
          h3("How long are rap songs compared to other genres?"),
          mainPanel(
            plotOutput(
              "length"
            )
          )
        )
      )
    ),
    tabPanel(
      
      # This is where viewers can explore the correlation between each variable.
      # I find it interesting to see how they work together.
      
      "Correlation",
      tabsetPanel(
        tabPanel(
          "Audio Features Chart",
          h3("How do audio variables correlate to one another?"),
          mainPanel(
            plotOutput("corr")
          )
        ),
        tabPanel(
          "Feature by Feature",
          h3("How do individual features correlate to one another?"),
          sidebarLayout(
            sidebarPanel(
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
              plotlyOutput(
                "cor_plot"
              )
            )
          )
        )
      )
    )
  )
)

# Define server logic required to draw the graphs

server <- function(input, output, session) {
  
  # This graph is used for comparing rap to other genres.
  
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

  # This if statement were needed in order to change th grahp when different
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

  # This if statement is to change the x-values to 'BPM' when tempo is selected.
  
  chart_units <- reactive({
    if (input$chart_features == "tempo") {
      chart_units <- "BPM"
    }
  })

  # This graph is to represent different audio values for the whole Top 100 Chart.
  
  output$chart_plot <- renderPlot({
    ggplot(chart_plot_data(), aes(reorder(artist_name, +get(input$chart_features)), get(input$chart_features))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(
        x = "Artist - Song",
        y = "Value"
      ) +
      
      # Adding this allows the units to change with the tempo call. 
      
      scale_y_continuous(labels = unit_format(unit = chart_units()))
  })

  # This graph represents the top 25 artist of the chart. 
  
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

  # This graph compares the duration of rap songs to other genres.
  
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
    
    labels <- paste0("r = ", round(data_to_plot$correlation, 3)) %>%
      lapply(htmltools::HTML)
    
    plot <- music %>%
      ggplot(aes(get(input$x_cor), get(input$y_cor), text = labels)) +
      geom_point(colour = "black", shape = 21, size = 3, aes(fill = factor(type))) +
      scale_fill_brewer(palette = "Greens") +
      geom_smooth(method = lm) +
      labs(x = names(audio_choices[which(audio_choices == input$x_cor)]), 
           y = names(audio_choices[which(audio_choices == input$y_cor)])) +
      theme_economist() +
      theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), legend.position = "right") +
      guides(fill = guide_legend(title = "Type of song"))
    
    ggplotly(plot, tooltip = "text")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
