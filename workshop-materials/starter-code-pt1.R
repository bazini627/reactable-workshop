# IMPORT LIBRARIES
# To install packages you can either use the UI (Packages ->Install)
# Or you can install with install.packages("packagename")
# Note, one library is not on CRAN - remotes::install_github("timelyportfolio/dataui")

# To build our tables
library(reactablefmtr)
library(reactable)
# To add in HTML
library(htmltools)
library(htmlwidgets)
# For added data manipulation
library(tidyverse)
library(glue)
# Save as png 
library(webshot2)

#IMPORT HELPER FUNCTIONS - used for additional HTML/CSS styling
source("https://raw.githubusercontent.com/tashapiro/reactable-workshop/main/workshop-materials/helper_functions.R")

#IMPORT DATA
artist_data<- read.csv("https://raw.githubusercontent.com/tashapiro/reactable-workshop/main/workshop-materials/data/spotify-artists.csv")

#WRANGLE DATA
#arrange artists by followers in descending order
#create rank with row_number
#arrange the columns in this order using select: rank, name, genres, followers, populatiy, image_url
#get the first 500 records
spotify_artists <- artist_data %>% 
  arrange(desc(followers)) %>% 
  mutate(rank = row_number()) %>% 
  select(rank, name, genres, followers, popularity, image_url) %>% 
  head(500)

#STORE AESTHETICS IN VARIABLES - helps us stay consistent, easier to remember / edit
heatmap_pal<-rev(c("#64d591","#afe88d","#D6E888","#fce782","#fdf2c0","#fdfdfd"))
bg_color <- '#0B0B0A'
border_color<-'#2E2E2E'

#Build Spotify Table
#use reactable, reactablefmtr, and htmltools to customize table
spotify_table <- reactable(spotify_artists,
          searchable = TRUE,
          theme = reactableTheme(
            backgroundColor = bg_color,
            borderColor = border_color,
            style = list(
              color = "white"),
            # Search Styling
            searchInputStyle = list(
              backgroundColor = bg_color,
              borderColor = border_color
            )
            ),
          defaultColDef = colDef(vAlign = "center"),
          columns = list(
            rank = colDef(name = "Rank", align = "center"),
            image_url = colDef(show = FALSE),
            name = colDef(name = "Artist", width = 300, 
                          cell = function(value, index) {
                            image = spotify_artists$image_url[index]
                            image_label(image_url = image, 
                                        label = value, 
                                        height = 30,
                                        circle = TRUE,
                                        border_color = "white",
                                        border_width = 2
                                        )
            }),
            followers = colDef(name = "Followers", cell = data_bars(spotify_artists,
                                                                    fill_color = "#1DB954",
                                                                    text_position = "above",
                                                                    background = NULL,
                                                                    text_color = "white",
                                                                    round_edges = TRUE,
                                                                    bar_height = 10,
                                                                    number_fmt = scales::label_number(scale_cut = scales::cut_short_scale()))
            ),
            genres = colDef(name = "Genres", align = "left", width = 200,
                            
                            cell = function(value) {
              genre_tags(value)
            }),
            popularity = colDef(name = "Popularity", cell = icon_sets(spotify_artists,
                                                            icons = c("star"),
                                                            colors = heatmap_pal),
                                                     footer = "Calulated By Spotify",
                                                     footerStyle = list(fontSize = 11))
            ))%>% 
  reactablefmtr::google_font(font_family = "Heebo")
  

#Add Header & Caption with htmlwidgets + htmltools
spotify_finished <- spotify_table%>%
  htmlwidgets::prependContent(
    tags$div(style = 'background-color:#0B0B0A;color:white;padding-left:30px;padding-top:5px;',
             tags$h1("Popular Artists on Spotify",style = "margin-bottom:0px;"),
             tags$span("Ranking based on follower counts on Spotify. Data as of April 2023.",
                       style = 'color:#DDDDDD')
    )
  )%>%
  htmlwidgets::appendContent(
    tags$div("Source: Spotify API {spotifyR}", style = 'background-color:#0B0B0A;color:#DDDDDD;padding-left:30px;padding-bottom:20px;font-size:12px;',
    )
  )


#preview table output


#export as HTML widget with htmlwidget
htmlwidgets::saveWidget(spotify_finished, file = "spotify.html", selfcontained = TRUE)


#export as png with webshot2
webshot2::webshot("spotify.html", file = "spotify.png")

