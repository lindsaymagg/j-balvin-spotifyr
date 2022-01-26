library(shiny)
library(plotly)
library(DT)
# ESKETIT

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
                              Any tracks that were released on multiple albums only appear once in the data, on the first album it was released on."),
                            dataTableOutput("all_tracks_DT"),
                            h2("Number of Released Tracks"),
                            textOutput("totalnumsongs"),
                            textOutput("totalnumsongsnoremix"),
                            sidebarLayout(
                              sidebarPanel(checkboxGroupInput("type_choice_numtracks", "Type of release",
                                                              choices = c("Single" = "single",
                                                                          "Album" = "album",
                                                                          "Remix/Alternate Version" = "remix"), 
                                                              selected = c("single","album","remix")),
                                           checkboxInput("preserve_albums_check", label = "Keep album tracks together", value = FALSE),
                                           width = 2),
                              mainPanel(
                                textOutput("numreleasedtracks_text"),
                                plotlyOutput("numreleasedtracks"),
                                HTML("<ul class = 'p-mini'>
<li>The year with most total releases was 2019. J Balvin released 55 songs. Over half of the releases were remixes (of Que Calor, RITMO, Human Lost, and Contra La Pared).</li>
<li>In 2021, the majority of J Balvin’s releases (23 out of 34) were on his album, JOSE.</li>
<li>Excluding remixes, the year with most releases was 2021, with 29 releases.</li>
<li>Starting in 2017, J Balvin started releasing an increasing number of singles. The year with the highest number of releases was 2019, with 17 singles released.</li>
<li>Since 2018, J Balvin has released an album every year.</li>
<li>In 2018 and 2019, J Balvin released many remixes: 22 in 2018 (most of which are Bum Bum Tam Tam and Mi Gente remixes) and 30 in 2019.</li>
</ol>"),
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
                                                              selected = c("single","album")), width = 2),
                              mainPanel(
                                plotlyOutput("keyreleasedtracks"),
                                HTML("<ul class = 'p-mini'>
<li>J Balvin has released tracks in every key.</li>
<li>The top keys he produces songs in are C# major, B minor, and F minor.</li>
<li>When we include remixes in analysis, the order just changes a bit because some popular songs get remixed and the keys of those original songs that are remixed are then multiplied. For example, B minor becomes the top key because Mi Gente was in B minor, and many remixes were made of Mi Gente.</li>
</ol>"),
                                width = 10
                              )
                            ),
                            h2("Spotify Attributes of Tracks"),
                            sidebarLayout(
                              sidebarPanel(selectInput("x_axis_choice", "X-axis Attribute", choices =
                                                       list("Positivity" = "valence",
                                                            "Energy" = "energy", "Danceability" = "danceability"
                                                            #,
                                                            #"Speechiness" = "speechiness"
                                                            ), 
                                                              selected = "valence"),
                                           selectInput("y_axis_choice", "Y-axis Attribute", choices =
                                                         list("Positivity" = "valence",
                                                              "Energy" = "energy", "Danceability" = "danceability"
                                                              #,
                                                              #"Speechiness" = "speechiness"
                                                              ), 
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
                              mainPanel(plotlyOutput("coordgraph", height = 800),
                                        HTML("<ul class = 'p-mini'>
<li>J Balvin’s “home quadrant” is very positive and energetic. Most of his songs (approximately 75%) lie in this quadrant. The data reinforce the idea that he is all about spreading good vibes.</li>
<li>From 2011-2015, 91% of his songs were in this quadrant. His original sound is uniformly happy and high energy. In 2016, 2017, and 2018, he started to release songs with greater variety. Especially in 2019, J Balvin released many songs outside of his typical quadrant, especially in the less-positive high-energy quadrant. In 2020 and 2021, he stayed true to his original style (happy and energetic) but also released some songs outside his home quadrant.</li>
<li>Singles and remixes are almost always high energy, whereas he releases some lower-energy songs on his albums. Remixes expand his range to include songs that have lower positivity.</li>
<li>Collaborations greatly expand the range of his sound and allow him to experiment outside of his home quadrant.</li>
<li>His music is consistently very danceable. The only exception to this rule is “En Mí (Interlude)”.</li><br>
<li>Overall, the graph shows that J Balvin started off with a distinct style — high energy and high positivity. Over the years through remixes and collaborations, he has expanded the range of his sound and introduced his audience to new sounds while staying true to his original style.</li>
</ol>"),
                                        width = 9)
                            )
             )
    ),
    
    #ALBUMS_____________________________________________________________________________________________________
    
    tabPanel("Albums", 
             verticalLayout(h1("Albums", 
                               align = "left"),
                            p("The tracks below were retrieved by using the Spotify API to fetch all of J Balvin’s released albums. Some songs appear on multiple albums."),
                            dataTableOutput("albums_DT"),
                            #textOutput("album_num_songs_text"),
                            h2("Number of tracks per album"),
                            sidebarLayout(sidebarPanel(radioButtons("radio_album_num", label = h5("Albums to include in analysis:"),
                                                                    choices = list("Original Albums" = "only_og", "Deluxe Albums" = "only_all"), 
                                                                    selected = "only_og"), width = 2), 
                                          mainPanel(
                                            plotlyOutput("numtrackseachalbum"),
                                            HTML("<ul class = 'p-mini'>
<li>Albums looked like they were trending toward including fewer tracks over time, but JOSE breaks that pattern.</li>
<li>OASIS had the fewest tracks (8), which makes sense because it was a collaborative album with Bad Bunny.</li>
<li>JOSE is the longest album, with 24 tracks.</li>
</ol>"), width= 10
                                            )
                                          ),
                            h2("Album Length"),
                            sidebarLayout(sidebarPanel(radioButtons("radio_album_dur", label = h5("Albums to include in analysis:"),
                                                                    choices = list("Original Albums" = "only_og", "Deluxe Albums" = "only_all"), 
                                                                    selected = "only_og"), width = 2), 
                                          mainPanel(
                                            plotlyOutput("lengtheachalbum"),
                                            HTML("<ul class = 'p-mini'>
<li>Albums have generally become shorter as time has passed, although JOSE completely breaks that pattern.</li>
</ol>"), width= 10
                                          )
                            ),
                            h2("Track Length, Positivity, Energy, and Danceability on Each Album"),
                            sidebarLayout(
                              sidebarPanel(
                                radioButtons("radio_album_att", label = h5("Albums to include in analysis:"),
                                             choices = list("Original Albums" = "only_og", "Deluxe Albums" = "only_all"), 
                                             selected = "only_og"),
                                selectInput("attribute_choice", label = h5("Attribute"), 
                                                       choices = list("Track Length" = "track_length", "Positivity" = "valence",
                                                                      "Energy" = "energy", "Danceability" = "danceability"
                                                                      #,
                                                                      #"Speechiness" = "speechiness"
                                                                      ), 
                                                       selected = "track_length"),
                                           h5("Description"),
                                           uiOutput("attribute_description"),
                                           width = 2),
                              mainPanel(
                                h4(textOutput("whichattributetext") %>% tagAppendAttributes(class = "above-plot"), align = "center"),
                                plotlyOutput("attributeplot", height = 700),
                                HTML("<ul class = 'p-mini'>
<li><strong>Track Length:</strong> OASIS has the longest tracks, which makes sense because in each song he and Bad Bunny have a verse. Aside from OASIS, track length looks pretty constant across time. Colores tracks are a bit shorter on average. The range in track length has increased over time, with JOSE having the most range in track length.</li>
<li><strong>Positivity:</strong> His first three albums look very similar; almost all tracks had high positivity between .6 and .8. In Vibras, OASIS, and Colores, more tracks fall outside that range, showing that his range of sound and mood has expanded over the years. JOSE has a slightly lower positivity than the other albums , potentially reflecting that his music has become more authentic over time. Not happy 100% of the time, there are ups and downs.</li>
<li><strong>Energy:</strong> All of his albums have been very energetic. Vibras was the most chill/vibey album (in agreement with the album title). JOSE is quite symmetric, more than Vibras and Energía. It's more balanced and broader range.</li>
<li><strong>Danceability:</strong> His albums are consistently very danceable. Average danceability has increased somewhat over time and the range has too.</li>
<br>
<li><strong>Over time, for certain features (track length, positivity, energy, and danceability), J Balvin’s albums are increasing in variety and range. This suggests that his albums are getting more complex as he continues to explore different styles and sounds.</strong></li>
</ol>"),
                                textOutput("testtext"),
                                width = 10
                              )
                            ),
                            h2("Key"),
                            HTML("<p>The key the track is in.</p>"),
                            sidebarLayout(sidebarPanel(radioButtons("radio_album_key", label = h5("Albums to include in analysis:"),
                                                                    choices = list("Original Albums" = "only_og", "Deluxe Albums" = "only_all"), 
                                                                    selected = "only_og"), width = 2),
                                          mainPanel(plotlyOutput("keyalbum"),
                                                    HTML("<ul class = 'p-mini'><li>Every album has songs in a wide range of keys.</li></ol>"),
                                                    width = 10
                                                    )
                                          ),
                            h2("Major/Minor Scale (Mode)"),
                            HTML("<p>Mode indicates the modality (major or minor) of a track, the type of scale from which its melodic content is derived (<a href='https://developer.spotify.com/documentation/web-api/reference/#/operations/get-audio-features'>Spotify API documentation</a>).</p>"),
                            sidebarLayout(
                              sidebarPanel(
                                radioButtons("radio_album_mode", label = h5("Albums to include in analysis:"),
                                             choices = list("Original Albums" = "only_og", "Deluxe Albums" = "only_all"), 
                                             selected = "only_og"),
                                width = 2), 
                              mainPanel(
                                plotlyOutput("modealbum"), 
                                HTML("<ul class = 'p-mini'>
<li>J Balvin Mix Tape, Energía, and Vibras all have songs in major/minor pretty equally</li>
<li>La Familia, OASIS, and Colores all have more songs in minor.</li>
<li>Every album has at least half of its songs in minor.</li>
</ol>"),
                                width = 10))
                            
                            
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
             p("A track is considered collaborative if it features at least one additional artist."),
             textOutput("collab_vs_solo1"),
             textOutput("collab_vs_solo2"),
             sidebarLayout(
               sidebarPanel(selectInput("num_percent_choice", label = h5("Display"), 
                                        choices = list("Count (Stacked)" = "count_stacked", "Count (Grouped)" = "count_grouped", "Percentage" = "percentage"), 
                                        selected = "count_stacked"),
                            checkboxInput("num_percent_remix_check", label = "Include remixes", value = FALSE),
                            width = 2),
               mainPanel(
                 plotlyOutput("collabs_vs_solos"),
                 HTML("<ul class = 'p-mini'>
<li>In 2019, J Balvin only released one solo song, La Rebelión. 96% of his songs that year were with at least one other artist.</li>
<li>The percentage of J Balvin’s released songs that are collaborations has greatly increased since the start of his career.</li>
<li>Starting in 2017, every year over half of his released songs are collaborative.</li>
</ol>"),
                 width = 10
               )
             ),
             h2("Collaboration on Singles"),
             verticalLayout(
               fluidRow(column(5, plotlyOutput("singlespiechart")),column(7, plotlyOutput("singlesbarchart"))),
               HTML("<ul class = 'p-mini'>
<li>Most of J Balvin's singles (92%) feature another artist.</li>
</ol>")),
             h2("J Balvin's Collaborators"),
             textOutput("totalnumcollaborators"),
             textOutput("totalnumcollaboratorsnoremix"),
             wellPanel(checkboxGroupInput("type_choice_collaborators", "Type of release",
                                          choices = c("Single" = "single",
                                                      "Album" = "album",
                                                      "Remix/Alternate Version" = "remix"), 
                                          selected = c("single","album","remix"))),
             verticalLayout(
               plotlyOutput("collaborator_plot", height = 1550),
               HTML("<ul class = 'p-mini'>
<li>When we include remixes, Willy William is JB’s top collaborator, followed by Bad Bunny, Nicky Jam, MC Fioti, and Major Lazer.</li>
<li>When we exclude remixes, Bad Bunny is J Balvin's biggest collaborator by far. J Balvin collaborated on more singles with Bad Bunny than anyone else, and on top of that, they have a whole album together. After Bad Bunny, his top collaborators are Sky, Jhay Cortez, Ozuna, Mr. Eazi, Arcangel, Anuel AA, and Anitta.</li>
<li>For most of the artists J Balvin has collaborated with, he only made one song with them.</li>
<li>Mr. Eazi is the only non-Spanish-speaking artist J Balvin has collaborated with more than once on non-remixed tracks.</li>
<li>The artists he has featured most on his albums are Bad Bunny, Zion y Lennox, Yandel, Sky, and Mr. Eazi.</li>
</ol>")),
             h2("Collaborators per Year"),
             sidebarLayout(
               sidebarPanel(checkboxGroupInput("type_choice_collaborators_yr", "Type of release",
                                               choices = c("Single" = "single",
                                                           "Album" = "album",
                                                           "Remix/Alternate Version" = "remix"), 
                                               selected = c("single","album","remix")),
                            width = 2),
               mainPanel(
                 plotlyOutput("collaborators_yr_plot"),
                 HTML("<ul class = 'p-mini'>
<li>The number of artists J Balvin collaborated with increased from 2016 to 2019 from 8 to 68 (8.5x increase). Excluding remixes, the number of collaborators per year increased from 7 to 35 (5x increase).</li>
<li>Excluding remixes, in 2021, J Balvin more than doubled the number of artists he collaborated with in 2020.</li>
<li>J Balvin has collaborated with more artists on his album this year than any other year. On JOSE, he collaborated with 15 artists. This number is even larger if we were to include Poblado - Remix as an album release rather than a remix, this number would increase to 20 artists. Other albums only had a maximum of 7 collaborators (Vibras and Energía.)</li>
</ol>"),
                 width = 10
               )
             ),
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
                 HTML("<ul class = 'p-mini'>
<li>Most collaborations come from remixes (64%).</li>
<li>After remixes, most collaborations come from singles (24%).</li>
<li>12.8% of his collaborations come from albums.</li>
<li>From 2016 to 2019, his number of collaborations increased from 7 to 42 (6x) if we exclude remixes, and 8 to 124 (15.5x) if we include remixes.</li>
<li>2019 was the year in which he had most total collaborations (124), remix collaborations (82), and single collaborations (32). The year with the most album collaborations was 2021 (15).</li>
                      </ol>"),
                 width = 10
               )
             ),
             h2("Same-genre vs. Cross-genre Collaborations by Year"),
             p("Collaborations with artists for which genre information was not available are not included in the following analysis."),
             sidebarLayout(
               sidebarPanel(checkboxGroupInput("type_choice_same_diff_col", "Type of release",
                                               choices = c("Single" = "single",
                                                           "Album" = "album",
                                                           "Remix/Alternate Version" = "remix"), 
                                               selected = c("single","album","remix")),
                            selectInput("num_percent_choice_genre", label = h5("Display"), 
                                        choices = list("Count (Stacked)" = "count_stacked", "Count (Grouped)" = "count_grouped", "Percentage" = "percentage"), 
                                        selected = "count_stacked"),
                            width = 2),
               mainPanel(
                 plotlyOutput("same_diff_collabs_plot"),
                 width = 10
               )
             ),
             verticalLayout(
               h2("Genres of Collaborators"),
               textOutput("totalnumgenres"),
               textOutput("totalnumgenresnoremix"),
               wellPanel(checkboxInput("genre_collaborator_remix_check", label = "Include remixes", value = TRUE)),
               plotlyOutput("genres_of_collabors_plot", height = 1500),
               HTML("<ul class = 'p-mini'>
<li>Excluding remixes, the top genres of J Balvin’s collaborators are latin, trap latino, reggaeton, latin hip hop, pop, reggaeton flow, pop rap, and dance rap.</li>
<li>The most common genres of collaborators on his albums are trap latino, latin, reggaeton, and latin hip hop.</li>
<li>Including remixes, the top genres of J Balvin’s collaborators are mostly the same, but we see a much larger presence of electronic dance genres such as EDM, electrohouse, and tropical house.</li>
                      </ol>"),
               h2("Genres of Collaborators by Year of Collaboration"),
               sidebarLayout(sidebarPanel(checkboxGroupInput("type_choice_genre_col_yr", "Type of release",
                                                             choices = c("Single" = "single",
                                                                         "Album" = "album",
                                                                         "Remix/Alternate Version" = "remix"), 
                                                             selected = c("single","album","remix")), width = 2),
                             mainPanel(plotlyOutput("genres_of_collabors_by_year_plot"), width = 10)),
               plotlyOutput("num_genres_of_collabors_by_year_plot"),
               HTML("<br><br><p>notes here</p>")
             )
             ),
   
    #NETWORK_____________________________________________________________________________________________________ 
    
    tabPanel("Network Analysis",
             verticalLayout(h1("J Balvin in the Artist Network", 
                               align = "left"),
                            p("This page includes analysis of a collaboration network of the most popular artists on Spotify. The network was constructed as follows:"),
                            HTML("<ol class = 'p'>
<li>Webscrape the Billboard Top 100 Artists Weekly to obtain a list of every artist who has made it onto the artist charts in the past decade.</li>
<li>Use the Spotify API to retrieve information about each Billboard artist (such as Spotify popularity and number of Spotify followers.)</li>
<li>For each Billboard artist, retrieve all songs they have released as singles or on albums using the Spotify API. For every song released with additional artists listed, record a collaboration between the artists.</li>
<li>Construct a collaboration network in which a vertex represents an artist and an edge between two vertices signifies that those two artists collaborated on at least one song.</li>
</ol>"),
                            textOutput("clusterText") %>% tagAppendAttributes(class = 'p'),
                            img(src = "/plain-network/artist-network_2022-01-01_.jpg", style="display: block; margin-left: auto; margin-right: auto;", width = "60%"),
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
                            HTML("<p>The information collected for each artist (vertex) in the network is below.
                              Collaborations collected for analysis include singles, albums, and remixes for all Billboard artists.
                              Tracks released on multiple albums are only counted once in <code>total_num_songs</code>.</p>"),
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
                            p(class = "caption", "Collaborator of Collaborators • Networker"),
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
                            p(class = "caption", "Connector • Power Broker"),
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
                            p(class = "caption", "Creator"),
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
