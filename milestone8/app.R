#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

music <- read_rds("music.rds")

pie <- read_rds("pie.rds")

ui <- fluidPage(
    navbarPage("Spotify Top 100 2018 Data",
               tabPanel("Rap v. Non Rap",
                        plotOutput("chart1"),
                        plotOutput("chart2"),
                        plotOutput("chart3"),
                        plotOutput("chart4"),
                        plotOutput("chart5"),
                        plotOutput("chart6"),
                        plotOutput("chart7"),
                        plotOutput("chart8")
                        ), 
        tabPanel("All graphs",
           plotOutput("top25"), 
           plotOutput("dance"),
           plotOutput("energy"), 
           plotOutput("loud"),
           plotOutput("soft"),
           plotOutput("speech"),
           plotOutput("acoustic"),
           plotOutput("live"),
           plotOutput("instrumental"),
           plotOutput("pos"),
           plotOutput("neg"),
           plotOutput("up"),
           plotOutput("down"), 
           plotOutput("long"),
           plotOutput("short"),
           plotOutput("chart1"),
           plotOutput("chart2"),
           plotOutput("chart3"),
           plotOutput("chart4"),
           plotOutput("chart5"),
           plotOutput("chart6"),
           plotOutput("chart7"),
           plotOutput("chart8"),
           plotOutput("length"),
           plotOutput("corr"),
           plotOutput("piechart"),
           plotOutput("loudenergy")
           
        )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

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
                 title = "25 Most Popular Artists")
    })
    
    output$dance <- renderPlot({
        music %>%
            arrange(desc(danceability)) %>%
            slice(1:10) %>%
            ggplot(., aes(reorder(artist_name,+danceability), danceability)) +
            geom_bar(stat = "identity") +
            coord_flip() +
            labs(x = "Song",
                 y = "Value",
                 title = "Top 10 Most Danceable Songs")
    })
    
    output$energy <- renderPlot({
        music %>%
            arrange(desc(energy)) %>%
            slice(1:10) %>%
            ggplot(aes(reorder(artist_name,+energy), energy)) +
            geom_bar(stat = "identity") +
            coord_flip() +
            labs(x = "Song",
                 y = "Value",
                 title = "Top 10 Most Energenic Songs")
    })
    
    output$loud <- renderPlot({
        music %>%
            arrange(desc(loudness)) %>%
            slice(1:10) %>%
            ggplot(aes(reorder(artist_name,+loudness), loudness)) +
            geom_bar(stat = "identity") +
            coord_flip() +
            labs(x = "Song",
                 y = "Value",
                 title = "Top 10 Loudest Songs")
    })
    
    output$soft <- renderPlot({
        music %>% 
            arrange(loudness) %>% 
            slice(1:10) %>% 
            ggplot(aes(reorder(artist_name, +loudness), loudness)) +
            geom_bar(stat = "identity") +
            coord_flip() +
            labs(x = "Song",
                 y = "Value",
                 title = "Top 10 Softest Songs")
    })
    
    output$speech <- renderPlot({
        music %>%
            arrange(desc(speechiness)) %>%
            slice(1:10) %>%
            ggplot(aes(reorder(artist_name,+speechiness), speechiness)) +
            geom_bar(stat = "identity") +
            coord_flip() +
            labs(x = "Song",
                 y = "Value",
                 title = "Top 10 Most Speech Filled Songs")
    })
    
    output$acoustic <- renderPlot({
        music %>%
            arrange(desc(acousticness)) %>%
            slice(1:10) %>%
            ggplot(aes(reorder(artist_name,+acousticness), acousticness)) +
            geom_bar(stat = "identity") +
            coord_flip() +
            labs(x = "Song",
                 y = "Value",
                 title = "Top 10 Most Acoustic Songs")
    })
    
    output$live <- renderPlot({
        music %>%
            arrange(desc(liveness)) %>%
            slice(1:10) %>%
            ggplot(aes(reorder(artist_name,+liveness), liveness)) +
            geom_bar(stat = "identity") +
            coord_flip() +
            labs(x = "Song",
                 y = "Value",
                 title = "Top 10 Songs with Audience Presence")
    })
    
    output$instrumental <- renderPlot({
        music %>%
            arrange(desc(instrumentalness)) %>%
            slice(1:5) %>%
            ggplot(aes(reorder(artist_name,+instrumentalness), instrumentalness)) +
            geom_bar(stat = "identity") +
            coord_flip() +
            labs(x = "Song",
                 y = "Value",
                 title = "Top 5 Most Instrumental Songs")
    })
    
    output$pos <- renderPlot({
        music %>%
            arrange(desc(valence)) %>%
            slice(1:10) %>%
            ggplot(aes(reorder(artist_name,+valence), valence)) +
            geom_bar(stat = "identity") +
            coord_flip() +
            labs(x = "Song",
                 y = "Value",
                 title = "Top 10 Most Positive Songs")
    })
    
    output$neg <- renderPlot({
        music %>%
            arrange(valence) %>%
            slice(1:10) %>%
            ggplot(aes(reorder(artist_name,+valence), valence)) +
            geom_bar(stat = "identity") +
            coord_flip() +
            labs(x = "Song",
                 y = "Value",
                 title = "Top 10 Most Negative Songs")
    })
    
    output$up <- renderPlot({
        music %>%
            arrange(desc(tempo)) %>%
            slice(1:10) %>%
            ggplot(aes(reorder(artist_name,+tempo), tempo)) +
            geom_bar(stat = "identity") +
            coord_flip() +
            labs(x = "Song",
                 y = "Beats per Minute (BPM)",
                 title = "Top 10 Most Upbeat Songs")
    })
    
    output$down <- renderPlot({
        music %>%
            arrange(tempo) %>%
            slice(1:10) %>%
            ggplot(aes(reorder(artist_name,+tempo), tempo)) +
            geom_bar(stat = "identity") +
            coord_flip() +
            labs(x = "Song",
                 y = "Beats per Minute (BPM)",
                 title = "Top 10 Least Upbeat Songs")
    })
    
    output$long <- renderPlot({
        music %>%
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
    })
    
    output$short <- renderPlot({
        music %>%
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
    })
    
    output$chart1 <- renderPlot({
        music %>%
            ggplot(aes(type, danceability, fill = type)) +
            geom_boxplot() +
            scale_fill_brewer(palette = "Greens") +
            labs(x = "",
                 y = "Danceability") +
            theme(legend.position = "none")
    })
    
    output$chart2 <- renderPlot({
        music %>%
            ggplot(aes(type, energy, fill = type)) +
            geom_boxplot() +
            scale_fill_brewer(palette = "Greens") +
            labs(x = "",
                 y = "Energy") +
            theme(legend.position = "none")
    })
    
    output$chart3 <- renderPlot({
        music %>%
            ggplot(aes(type, loudness, fill = type)) +
            geom_boxplot() +
            scale_fill_brewer(palette = "Greens") +
            labs(x = "",
                 y = "Loudness") +
            theme(legend.position = "none")
    })
    
    output$chart4 <- renderPlot({
        music %>%
            ggplot(aes(type, speechiness, fill = type)) +
            geom_boxplot() +
            scale_fill_brewer(palette = "Greens") +
            labs(x = "",
                 y = "Speechiness") +
            theme(legend.position = "none")
    })
    
    output$chart5 <- renderPlot({
        music %>%
            ggplot(aes(type, acousticness, fill = type)) +
            geom_boxplot() +
            scale_fill_brewer(palette = "Greens") +
            labs(x = "",
                 y = "Acousticness") +
            theme(legend.position = "none")
    })
    
    output$chart6 <- renderPlot({
        music %>%
            ggplot(aes(type, liveness, fill = type)) +
            geom_boxplot() +
            scale_fill_brewer(palette = "Greens") +
            labs(x = "",
                 y = "Liveliness") +
            theme(legend.position = "none")
    })
    
    output$chart7 <- renderPlot({
        music %>%
            ggplot(aes(type, valence, fill = type)) +
            geom_boxplot() +
            scale_fill_brewer(palette = "Greens") +
            labs(x = "",
                 y = "Valence") +
            theme(legend.position = "none")
    })
    
    output$chart8 <- renderPlot({
        music %>%
            ggplot(aes(type, tempo, fill = type)) +
            geom_boxplot() +
            scale_fill_brewer(palette = "Greens") +
            labs(x = "",
                 y = "Tempo") +
            theme(legend.position = "none")
    })
    
    output$length <- renderPlot({
        music %>%
            ggplot(aes((duration_ms / 1000) / 60, fill = type)) +
            geom_density(alpha = 0.5) +
            scale_fill_brewer(palette = "Greens") +
            labs(x = "Length (minutes)",
                 y = "") +
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
            ggplot(aes(loudness, energy))+
            geom_point(colour = "black", shape = 21, size = 3, aes(fill = factor(type)))+ 
            scale_fill_brewer(palette = "Greens")+
            geom_smooth(method = lm)+
            annotate("text", x = -9.3, y = 0.85, label = "italic(r) == 0.73", parse = T, size = 6, col = "gray20")+
            labs(x = "Loudness", y = "Energy")+
            theme_economist()+
            theme(axis.text.x = element_text(size=10), axis.text.y = element_text(size=10), legend.position = "right")+
            guides(fill = guide_legend(title = "Type of song"))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
