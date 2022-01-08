library(shiny)
library(plotly)
library(DT)

shinyUI(fluidPage(
  tags$head(includeCSS("www/style.css")),
  h1("El Niño de Medellín", style="color:black;"),
  # dashboardHeader(),
  # dashboardSidebar(),
  # dashboardBody()
  br(),
  
  tabsetPanel(
    tabPanel("Home", 
    verticalLayout(h1("An Analysis of J Balvin’s Music and Position in the Artist Network", 
                      align = "center"),
                   h3("Lu Maggioncalda", 
                      align = "center"),
                   h4("January 2022", 
                      align = "center"),
                   img(src = "J-Balvin-billboard.jpg", style="display: block; margin-left: auto; margin-right: auto;", width = "50%")
                   # Stats?
                   # Intro to project
                   )
    ),
    
    #TRACKS_____________________________________________________________________________________________________
    
    tabPanel("Tracks", 
             verticalLayout(h1("Tracks", 
                               align = "left"),
                            p("The tracks below were retrieved by using the Spotify API to fetch all of J Balvin’s released albums and singles.
                              Any track containing the words “remix,” “mix,” “edit,” or “version” in the title were marked as “remix”.
                              Singles that were later released on albums were removed so only the album version remains.
                              Any tracks that were released on multiple albums only appear once in the data, on the last album it was released on."),
                            dataTableOutput("all_tracks_DT"),
                            h2("Number of Released Tracks"),
                            textOutput("totalnumsongs"),
                            textOutput("totalnumsongsnoremix"),
                            sidebarLayout(
                              sidebarPanel(checkboxGroupInput("type_choice_numtracks", "Type of release",
                                                              choices = c("Single" = "single",
                                                                          "Album" = "album",
                                                                          "Remix/Alternate Version" = "remix"), 
                                                              selected = c("single","album","remix")), width = 2),
                              mainPanel(
                                textOutput("numreleasedtracks_text"),
                                plotlyOutput("numreleasedtracks"),
                                width = 10
                              )
                            ),
                            #p("J Balvin has released 232 unique songs (this number includes remixes and different versions of the same song)."),
                            #graph,
                            #notes,
                            h2("Keys of Released tracks"),
                            sidebarLayout(
                              sidebarPanel(checkboxGroupInput("type_choice_key", "Type of release",
                                                              choices = c("Single" = "single",
                                                                          "Album" = "album",
                                                                          "Remix/Alternate Version" = "remix"), 
                                                              selected = c("single","album","remix")), width = 2),
                              mainPanel(
                                plotlyOutput("keyreleasedtracks"), width = 10
                              )
                            ),
                            h2("Spotify Attributes of Tracks"),
                            sidebarLayout(
                              sidebarPanel(selectInput("x_axis_choice", "X-axis Attribute", choices =
                                                       list("Valence" = "valence",
                                                            "Energy" = "energy", "Danceability" = "danceability",
                                                            "Speechiness" = "speechiness"), 
                                                              selected = "valence"),
                                           selectInput("y_axis_choice", "Y-axis Attribute", choices =
                                                         list("Valence" = "valence",
                                                              "Energy" = "energy", "Danceability" = "danceability",
                                                              "Speechiness" = "speechiness"), 
                                                       selected = "energy"),
                                           selectInput("group_by_choice", "Color by", choices =
                                                         list("Year" = "album_release_year", 
                                                              "Type of release" = "album_type",
                                                              "Collab vs. Solo" = "is_collab",
                                                              "Album" = "album_name"), 
                                                       selected = "energy"),
                                           checkboxInput("coordgraph_check", label = "Show as separate plots", value = FALSE),
                                           h5("Description of x:"),
                                           uiOutput("x_description"),
                                           br(),
                                           h5("Description of y:"),
                                           uiOutput("y_description"),
                                           width = 3),
                              mainPanel(plotlyOutput("coordgraph", height = 800), width = 9)
                            )
             )
    ),
    
    #ALBUMS_____________________________________________________________________________________________________
    
    tabPanel("Albums", 
             verticalLayout(h1("Albums", 
                               align = "left"),
                            p("The tracks below were retrieved by using the Spotify API to fetch all of J Balvin’s released albums. Some songs appear on multiple albums."),
                            dataTableOutput("albums_DT"),
                            wellPanel(radioButtons("radio_album", label = h5("Albums to include in analysis:"),
                                                   choices = list("Original Albums" = "only_og", "Deluxe Albums" = "only_all"), 
                                                   selected = "only_og")),
                            #textOutput("album_num_songs_text"),
                            h2("Number of tracks per album"),
                            plotlyOutput("numtrackseachalbum"),
                            h2("Album Length"),
                            plotlyOutput("lengtheachalbum"),
                            h2("Track Length, Valence, Energy, Danceability, and Speechiness on Each Album"),
                            sidebarLayout(
                              sidebarPanel(selectInput("attribute_choice", label = h5("Attribute"), 
                                                       choices = list("Track Length" = "track_length", "Valence" = "valence",
                                                                      "Energy" = "energy", "Danceability" = "danceability",
                                                                      "Speechiness" = "speechiness"), 
                                                       selected = "track_length"),
                                           h5("Description"),
                                           uiOutput("attribute_description"),
                                           width = 2),
                              mainPanel(
                                h4(textOutput("whichattributetext"), align = "center"),
                                plotlyOutput("attributeplot", height = 700),
                                textOutput("testtext"),
                                width = 10
                              )
                            ),
                            h2("Key"),
                            HTML("<p>The key the track is in. Integers map to pitches using standard Pitch Class notation. E.g. 0 = C, 1 = C♯/D♭, 2 = D, and so on (<a href='https://developer.spotify.com/documentation/web-api/reference/#/operations/get-audio-features'>Spotify API documentation</a>).</p>"),
                            plotlyOutput("keyalbum"),
                            h2("Mode"),
                            HTML("<p>Mode indicates the modality (major or minor) of a track, the type of scale from which its melodic content is derived (<a href='https://developer.spotify.com/documentation/web-api/reference/#/operations/get-audio-features'>Spotify API documentation</a>).</p>"),
                            plotlyOutput("modealbum")
                            #- J Balvin Mix Tape, Energía, and Vibras all have songs in major/minor pretty equally
                            #- La Familia, OASIS, and Colores all have more songs in minor
                            #- It looks like there is a trend toward more songs in minor.
             )
    ),
    
    #COLLABORATIONS_________________________________________________________________________________________________
    
    tabPanel("Collaborations",
             h1("Collaborations", 
                align = "left"),
             p("J Balvin's collaborations were obtained by fetching all of his songs
             from the Spotify API and examining those for which there are additional artists listed.
             If J Balvin collaborated with two separate artists on the same track (such as “LOCATION”), 
             that counts as two collaborations (one collaboration with Anuel AA, one collaboration with KAROL G)."),
             dataTableOutput("collab_DT"),
             h2("Collaborative vs. Solo Releases"),
             sidebarLayout(
               sidebarPanel(selectInput("num_percent_choice", label = h5("Display"), 
                                        choices = list("Count" = "count", "Percentage" = "percentage"), 
                                        selected = "count"),
                            checkboxInput("num_percent_remix_check", label = "Include remixes", value = FALSE),
                            width = 2),
               mainPanel(
                 plotlyOutput("collabs_vs_solos"),
                 width = 10
               )
             ),
             h2("Collaboration on Singles"),
             fluidRow(column(5, plotlyOutput("singlespiechart")),
                      column(7, plotlyOutput("singlesbarchart"))),
             h2("Total Number of Collaborators"),
             wellPanel(checkboxGroupInput("type_choice_collaborators", "Type of release",
                                               choices = c("Single" = "single",
                                                           "Album" = "album",
                                                           "Remix/Alternate Version" = "remix"), 
                                               selected = c("single","album","remix"))),
             h2("J Balvin's Collaborators"),
             plotlyOutput("collaborator_plot", height = 1550),
             h2("Collaborations per Year"),
             sidebarLayout(
               sidebarPanel(checkboxGroupInput("type_choice_collaborations", "Type of release",
                                               choices = c("Single" = "single",
                                                           "Album" = "album",
                                                           "Remix/Alternate Version" = "remix"), 
                                               selected = c("single","album","remix")),
                            width = 2),
               mainPanel(
                 plotlyOutput("collaborations_plot"),
                 width = 10
               )
             ),
             h2("Same-genre vs. Cross-genre Collaborations by Year"),
             sidebarLayout(
               sidebarPanel(checkboxGroupInput("type_choice_same_diff_col", "Type of release",
                                               choices = c("Single" = "single",
                                                           "Album" = "album",
                                                           "Remix/Alternate Version" = "remix"), 
                                               selected = c("single","album","remix")),
                            selectInput("num_percent_choice_genre", label = h5("Display"), 
                                        choices = list("Count" = "count", "Percentage" = "percentage"), 
                                        selected = "count"),
                            width = 2),
               mainPanel(
                 plotlyOutput("same_diff_collabs_plot"),
                 width = 10
               )
             ),
             verticalLayout(
               h2("Genres of Collaborators"),
               wellPanel(checkboxInput("genre_collaborator_remix_check", label = "Include remixes", value = TRUE)),
               plotlyOutput("genres_of_collabors_plot", height = 1500),
               h2("Genres of Collaborators by Year of Collaboration"),
               wellPanel(checkboxGroupInput("type_choice_genre_col_yr", "Type of release",
                                            choices = c("Single" = "single",
                                                        "Album" = "album",
                                                        "Remix/Alternate Version" = "remix"), 
                                            selected = c("single","album","remix"))),
               plotlyOutput("genres_of_collabors_by_year_plot"),
               dataTableOutput("genre_yr_DT")
             )
             ),
   
    #NETWORK_____________________________________________________________________________________________________ 
    
    tabPanel("Network Analysis",
             verticalLayout(h1("J Balvin in the Artist Network", 
                               align = "left"),
                            p("type intro here"),
                            img(src = "/plain-network/artist-network_2022-01-01_.jpg", style="display: block; margin-left: auto; margin-right: auto;", width = "60%"),
                            textOutput("clusterText") %>% tagAppendAttributes(class = 'p'),
                            textOutput("clusterText2") %>% tagAppendAttributes(class = 'p'),
                            br(),
                            img(src = "/color-network/artist-network-color_2022-01-01_.jpg", style="display: block; margin-left: auto; margin-right: auto;", width = "60%"),
                            p("The graph above is identical to the previous one,
                              except the colors represent genre, and larger nodes
                              represent artists who have participated in a higher number of
                              unique collaborations (higher degree).
                              As we can see from the groupings and the relative sizes of the nodes,
                              it looks like EDM/techno/house artists and hip hop/rap artists tend to collaborate the most."),
                            # REGGAETONERO GRAPH
                            p("The information collected for each artist (vertex) in the network is below.
                              Collaborations collected for analysis include singles, albums, and remixes for all Billboard artists.
                              Tracks released on multiple albums are only counted once in total_num_songs."),
                            dataTableOutput("artist_vertices_DT"),
                            h2("Popularity"),
                            HTML("<p>The popularity of the artist. The value will be between 0 and 100, with 100 being the most popular.
                              The artist’s popularity is calculated from the popularity of all the artist’s tracks (<a href = https://developer.spotify.com/documentation/web-api/reference/#/operations/get-an-artist>Spotify API documentation</a>).</p>"),
                            tabsetPanel(
                              tabPanel("All Artists in the Network", 
                                       textOutput("popularity_text_all") %>% tagAppendAttributes(class = 'net-text'),
                                       br(),
                                       dataTableOutput("popularity_DT")),
                              tabPanel(
                                "Artists Within Genre",
                                sidebarLayout(
                                  sidebarPanel(
                                    uiOutput("pop_genre_checkboxes"),
                                    radioButtons(
                                      "pop_and_or",
                                      label = h5("and/or"),
                                      choices = list("And" = "and", "Or" = "or"),
                                      selected = "or"
                                    ), 
                                    width = 2
                                  ),
                                  mainPanel(
                                    textOutput("popularity_text_genre") %>% tagAppendAttributes(class = "net-text"),
                                    dataTableOutput("pop_genre_DT"), width = 10)
                                )
                              )
                            ), 
                            h2("Degree Centrality"),
                            p(class = "caption", "Collaborator"),
                            p("Degree represents number of artists that the artist has released at least one collaboration with."),
                            tabsetPanel(
                              tabPanel("All Billboard Artists in the Network",
                                       textOutput("degree_text_all") %>% tagAppendAttributes(class = 'net-text'),
                                       br(),
                                       dataTableOutput("degree_DT"),
                                       plotlyOutput("degree_plot")),
                              tabPanel( "Billboard Artists Within Genre",
                                sidebarLayout(
                                  sidebarPanel(
                                    uiOutput("deg_genre_checkboxes"),
                                    radioButtons(
                                      "deg_and_or",
                                      label = h5("and/or"),
                                      choices = list("And" = "and", "Or" = "or"),
                                      selected = "or"
                                    ), 
                                    width = 2
                                  ),
                                  mainPanel(
                                    textOutput("degree_text_genre") %>% tagAppendAttributes(class = "net-text"),
                                    dataTableOutput("deg_genre_DT"),
                                    plotlyOutput("degree_plot_genre"),
                                    width = 10
                                    )
                                )
                              )
                            ),
                            h2("Eigenvector Centrality"),
                            p(class = "caption", "Collaborator of Collaborators, Networker"),
                            HTML("<p>Eigenvector centrality is a measure of the influence of a node in a network. 
                            Relative scores are assigned to all nodes in the network based on the concept that connections to high-scoring nodes contribute more to the score
                            of the node in question than equal connections to low-scoring nodes. A high eigenvector score means that a node is connected to many nodes who 
                            themselves have high scores (<a href = https://en.wikipedia.org/wiki/Eigenvector_centrality>Wikipedia</a>). Artists with a high centrality scores are those who collaborate with many people who *also* collaborate with many people. These are collaborators of collaborators, or artists who are connected to influential people.</p>"),
                            tabsetPanel(
                              tabPanel("All Billboard Artists in the Network",
                                       textOutput("eigenvector_text_all") %>% tagAppendAttributes(class = 'net-text'),
                                       br(),
                                       dataTableOutput("eigenvector_DT"),
                                       plotlyOutput("eigenvector_plot")),
                              tabPanel(
                                "Billboard Artists Within Genre",
                                sidebarLayout(
                                  sidebarPanel(
                                    uiOutput("eig_genre_checkboxes"),
                                    radioButtons(
                                      "eig_and_or",
                                      label = h5("and/or"),
                                      choices = list("And" = "and", "Or" = "or"),
                                      selected = "or"
                                    ), 
                                    width = 2
                                  ),
                                  mainPanel(
                                    textOutput("eigenvector_text_genre") %>% tagAppendAttributes(class = "net-text"),
                                    dataTableOutput("eig_genre_DT"),
                                    plotlyOutput("eigenvector_plot_genre"),
                                    width = 10)
                                )
                              )
                            ),
                            h2("Betweenness Centrality"),
                            p(class = "caption", "Connector, Power Broker"),
                            HTML("<p>Betweenness centrality measures a person's role in allowing information to pass from one part of the network to another. An artist with a high betweenness score is connected to multiple different clusters of collaborating artists and has a highly influential position in the network.</p>"),
                            tabsetPanel(
                              tabPanel("All Billboard Artists in the Network", 
                                       textOutput("betweenness_text_all") %>% tagAppendAttributes(class = 'net-text'),
                                       br(),
                                       dataTableOutput("betweenness_DT"),
                                       plotlyOutput("betweenness_plot")),
                              tabPanel(
                                "Billboard Artists Within Genre",
                                sidebarLayout(
                                  sidebarPanel(
                                    uiOutput("bet_genre_checkboxes"),
                                    radioButtons(
                                      "bet_and_or",
                                      label = h5("and/or"),
                                      choices = list("And" = "and", "Or" = "or"),
                                      selected = "or"
                                    ), 
                                    width = 2
                                  ),
                                  mainPanel(
                                    textOutput("betweenness_text_genre") %>% tagAppendAttributes(class = "net-text"),
                                    dataTableOutput("bet_genre_DT"),
                                    plotlyOutput("betweenness_plot_genre"),
                                    width = 10)
                                )
                              )
                            ),
                            h2("Followers"),
                            p(class = "caption", "Fan Favorite"),
                            p("The total number of Spotify followers the artist has."),
                            tabsetPanel(
                              tabPanel("All Artists in the Network", 
                                       textOutput("followers_text_all") %>% tagAppendAttributes(class = 'net-text'),
                                       br(),
                                       dataTableOutput("followers_DT"),
                                       plotlyOutput("followers_plot")),
                              tabPanel(
                                "Artists Within Genre",
                                sidebarLayout(
                                  sidebarPanel(
                                    uiOutput("fol_genre_checkboxes"),
                                    radioButtons(
                                      "fol_and_or",
                                      label = h5("and/or"),
                                      choices = list("And" = "and", "Or" = "or"),
                                      selected = "or"
                                    ), 
                                    width = 2
                                  ),
                                  mainPanel(
                                    textOutput("followers_text_genre") %>% tagAppendAttributes(class = "net-text"),
                                    dataTableOutput("fol_genre_DT"),
                                    plotlyOutput("followers_plot_genre"),
                                    width = 10
                                    )
                                )
                              )
                            ),
                            h2("Number of Songs"),
                            p(class = "caption", "Prolific Creator"),
                            p("The total number of unique songs an artist has released on Spotify. The number excludes tracks with the same name, but includes all remixes and versions of songs."),
                            tabsetPanel(
                              tabPanel("All Artists in the Network" ,
                                       textOutput("num_songs_text_all") %>% tagAppendAttributes(class = 'net-text'),
                                       br(),
                                       dataTableOutput("num_songs_DT"),
                                       plotlyOutput("num_songs_plot")),
                              tabPanel(
                                "Artists Within Genre",
                                sidebarLayout(
                                  sidebarPanel(
                                    uiOutput("num_genre_checkboxes"),
                                    radioButtons(
                                      "num_and_or",
                                      label = h5("and/or"),
                                      choices = list("And" = "and", "Or" = "or"),
                                      selected = "or"
                                    ), 
                                    width = 2
                                  ),
                                  mainPanel(
                                    textOutput("num_songs_text_genre") %>% tagAppendAttributes(class = "net-text"),
                                    dataTableOutput("num_genre_DT"),
                                    plotlyOutput("num_songs_plot_genre"),
                                    width = 10)
                                )
                              )
                            )
                            ))
    )
)
)
