#___________________________________________________________________________________________________________
#PACKAGES___________________________________________________________________________________________________
#___________________________________________________________________________________________________________

library(shiny)
library(tidyverse)
library(stringr)
library(tm)
library(lubridate)
library(stringr)
library(dplyr)
library(purrr)
library(plyr)
library(spotifyr)
library(genius)
library(igraph)
library(ggraph)
library(ggplot2)
library(sjPlot)
library(tidyr)
library(rapport)
library(sjmisc)
library(plotly)
library(ggbeeswarm)
library(ggridges)
library(RColorBrewer)
library(DT)
library(Rfast)
library(toOrdinal)
library(forcats)

#___________________________________________________________________________________________________________
#HELPER FUNCTIONS___________________________________________________________________________________________
#___________________________________________________________________________________________________________

# This function gets rid of duplicate tracks by either preserving "albums" (keeping albums together and deleting duplicate tracks released as singles) or by choosing the first time the song was released "date"
tracks_helper <- function(jb_tracks, preserve_what){
  
  # Get popularities
  tracks_popularity <- jb_tracks %>% arrange(track_name, -track_popularity) %>% distinct(tolower(track_name), .keep_all = TRUE)
  
  # Get rid of duplicates by preserving either release date or albums
  tracks_ordered <- {
    if (preserve_what == "album") {
      # Keep albums together
      jb_tracks %>% arrange(track_name, album_type, album_name, album_release_date) %>% distinct(tolower(track_name), .keep_all = TRUE)
    } else {
      # Prioritize first song released
      jb_tracks %>% arrange(desc(track_name), as.Date(album_release_date)) %>% distinct(tolower(track_name), .keep_all = TRUE)
    }
  }
  
  # Merge together
  tracks_full <- left_join(tracks_ordered, tracks_popularity, by = "tolower(track_name)")
  
  # Drop repetitive columns
  tracks_full <- tracks_full[,!(names(tracks_full) %in% c("tolower(track_name)", "album_type.y", "album_release_date.y", "album_release_year.y", "danceability.y", "energy.y", "key.y", "speechiness.y", "valence.y", "track_id.y", "artists.y", "artist_ids.y", "duration_ms.y", "track_name.y", "album_name.y", "mode_name.y", "key_mode.y", "track_number.y", "tempo.y", "track_popularity.x"))]
  
  # Manually drop additional duplicates: 
  # Mi Gente - F4ST, Velza & Loudness Remix, Mi Gente Feat. Beyoncé, Que Calor - Saweetie Remix, Que Calor (with J Balvin & El Alfa), RITMO (Bad Boys For Life) - Rosabel Dub Remix, Roses (Imanbek Remix) [Latino Gang], Roses - Imanbek Remix [Latino Gang], Ven y Hazlo Tú, X (Remix), Bum Bum Tam Tam - Jonas Blue Extended Mix, Hey Ma (feat. Camila Cabello), Hey Ma (with J Balvin & Pitbull feat. Camila Cabello), UN DIA (Feat. Tainy), Ven y Hazlo Tú, Que Calor, Hey Ma, Hey Ma, La Venganza, La Venganza
  duplicates <- c("3Oo7GUPSn3Nvdi4XBBhHyv", "5tSSdDqt0UvWXbxqRd9hTk", "2bUeGVkbuz2uJNBmHkNVhY", "1G2CAJeP7rCwOZjlSJ0Zw3", "6kBhyF31wa4Dt0Pt733vth", "03fb0nlJGQBTybCb71ZW2R", "2zJ2jLVDrl1tfMZkR89j4k", "46F5O39iDmdRuwRAS7KR6B", "5cZaDnYFu5FjuMLucBH7vO", "23cixn3e6AcLz2G3ujfMEJ", "05OzEk1HeSYwVE2byVkKbU", "7ggff9uRIDrDelWpwlUhSs", "5lsttfyShVGIdbbBsy83xt", "1sviDBcAqVOxMXq4xITVEs", "3mVEHig1Fn5MAqZLBeZOuu", "44w99ojqUCx9s1NXBpx7VW", "6Q0YxsP9Rne0Tn2xQCRE5W", "5YRpDfCCBCNAKDGIlBODbU", "0Pe4DpJDmZtOybHgrZ1hyM")
  
  if (preserve_what == "album"){
    duplicates <- duplicates %>% append("0EhpEsp4L0oRGM0vmeaN5e")
  } else {
    duplicates <- duplicates %>% append("66x45Yh9x397PWpxMX0ZDT")
  }
  
  
  tracks <- tracks_full %>% filter(!track_id.x %in% duplicates) %>% arrange(-album_release_year.x)
  
  # Clean column names
  names(tracks) = gsub(pattern = "\\.x", replacement = "", x = names(tracks))
  names(tracks) = gsub(pattern = "\\.y", replacement = "", x = names(tracks))
  
  # There are two unique songs called La Venganza. We need to re-insert both.
  la_venganza_familia = subset(jb_tracks, track_id == "5YRpDfCCBCNAKDGIlBODbU")
  la_venganza_jose = subset(jb_tracks, track_id == "0Pe4DpJDmZtOybHgrZ1hyM")
  tracks <- rbind(tracks,la_venganza_familia) %>% rbind(la_venganza_jose)
  tracks <- tracks %>% arrange(desc(album_release_date), desc(track_number))
  
  col_order <- c("track_name", "album_name",
                 "album_release_year", "album_type", "track_popularity", "duration_ms", "tempo", "valence", "energy", "danceability", "speechiness", "mode_name", "key_mode", "key", "artist_ids", "artists", "track_id", "album_release_date")
  tracks <- tracks[, col_order]
  
  # Add Jowell y Randy as artists of Sin Compromiso w Jowell y Randy
  sincomp_index <- which(tracks$track_id == "0gHSFpCQZmZ1LwPHQpzn6T")
  tracks[sincomp_index,]$artist_ids <- "4IMAo2UQchVFyPH24PAjUs;1vyhD5VmyZ7KMfW5gqLgo5"
  tracks[sincomp_index,]$artists <- "Jowell & Randy;J Balvin"
  
  return(tracks)
}

# Gets J Balvin's collaborations.
get_collaborators_artist <- function(artist_id, artist_name, artist_tracks){
  
  collab_year_df <- artist_tracks[,(names(artist_tracks) %in% c("artists", "artist_ids", "album_release_year", "track_id"))]
  
  list_of_collabs <- apply(collab_year_df, 1, exploder_jb)
  collaborations <- ldply(list_of_collabs, data.frame)
  
  filtered <-filter(collaborations, id != artist_id)
  
  artist_collabs <- data.frame(artist1 = artist_name, id1 = artist_id, artist2 = collaborations$name, id2 = collaborations$id, year = collaborations$year, track_id = collaborations$track_id) %>% filter(id2 != artist_id)
  return(artist_collabs)
}

exploder_jb <- function(row){
  year <- row[1]
  track_id <- row[4]
  artist_ids <- row[2]
  artist_names <- row[3]
  
  if (str_contains(artist_ids, ";", )){
    artist_ids <- unlist(strsplit(artist_ids, ";"))
    artist_names <- unlist(strsplit(artist_names, ";"))
  }
  unpacked_df <- data.frame(year = year, track_id = track_id, id = artist_ids, name = artist_names)
  return(unpacked_df)
}

# Return a "Collaboration" if the song has more than one artist and "Solo" if there is only one artist.
is_collab <- function(artists){
  if (grepl(';', artists)){
    return("Collaboration")
  }
  else {
    return("Solo")
  }
}

exploder_genre <- function(row){
  genre_string <- row[4]
  year <- row[5]
  type <- row[6]
  collaborator <- row[3]
  track_name <- row[7]
  if (str_contains(genre_string, ";", )){
    list_of_genres <- unlist(strsplit(genre_string, ";"))
    unpacked_df <- data.frame(genre = list_of_genres, year = year, album_type = type, collaborator = collaborator, track_name = track_name)
  }
  else {
    unpacked_df <- data.frame(genre = genre_string, year = year, album_type = type, collaborator = collaborator, track_name = track_name)
  }
  return(unpacked_df)
}

contains_certain_genre <- function(row, specific_genres){
  genre_string <- unlist(row)[5]
  #print(genre_string)
  if (genre_string == 0 || is.na(genre_string)) {
    return(FALSE)
  }
  for (genre in specific_genres){
    if (str_contains(genre_string, genre)){
      return(TRUE)
    }
  }
  return(FALSE)
}

or_genre_selector <- function(artist_vertices, selected_genres){
  within_genres <- NULL
  if (length(selected_genres >= 1)){
    first_genre <- selected_genres[1]
    within_genres <- artist_vertices %>% filter(get(first_genre) == TRUE)
  }
  i <- 2
  while (i < length(selected_genres)+1) {
    next_genre <- artist_vertices %>% filter(get(selected_genres[i]) == TRUE)
    within_genres <- rbind(within_genres, next_genre)
    i = i + 1
  }
  within_genres <- unique(within_genres)
  return(within_genres)
}

network_genre_selector <- function(artist_vertices, selected_genres, and_or_choice) {
    if (is.null(selected_genres)) {
      within_genres <- NULL
    } else {
      within_genres <- artist_vertices
      # Some genres are selected
      if (and_or_choice == "and") {
        # AND
        i <- 1
        while (i < length(selected_genres) + 1) {
          within_genres <- within_genres %>% filter(get(selected_genres[i]) == TRUE)
          i = i + 1
        }
      }
      
      else {
        # OR
        within_genres <- or_genre_selector(artist_vertices, selected_genres)
      }
    }
  return(within_genres)
}

# Determine whether a collaboration occurred between two artists of at least one overlapping genre (TRUE) or two artists of different genres (FALSE).
genre_overlap <- function(row){
  genrex <- row[[2]]
  genrey <- row[[4]]
  if (str_contains(genrex, ";")){
    genrex <- unlist(strsplit(genrex, ";"))
  }
  if (str_contains(genrey, ";")){
    genrey <- unlist(strsplit(genrey, ";"))
  }
  overlapping <- intersect(genrex,genrey)
  if (length(overlapping) != 0){
    return("share a genre")
  }
  else {
    return("different genres")
  }
}

# Helper function to change format of dataframe
flatten_collab_info <- function(df){
  genres <- unique(df$genre)
  collaborator <- unique(df$collaborator)
  album_types <- (paste(sort(unique(df$album_type)), collapse=" & "))
  track_names <- (paste(sort(unique(df$track_name)), collapse="\n"))
  new_df <- data.frame(genre = genres, collaborator = collaborator, album_types = album_types, tracks = track_names)
  return(new_df)
}

# Function that returns is_genre for every genre of artist
get_genres <- function(genre_string){
  if (str_contains(genre_string, ";", )){
    genres <- unlist(strsplit(genre_string, ";")) %>% sort()
  }
  else {
    genres <- c(genre_string)
  }
  return(genres)
}

# Helper function to prepare dataframe for flattening (genres by year)
flatten_artists <- function(single_genre_df){
  split_df_yr <- split(single_genre_df, f = single_genre_df$year)
  new_df <- map_df(split_df_yr, flatten_artists_2)
  return(new_df)
}

# Helper function to change format of dataframe, flatten all artists for a given genre into one row  (genres by year)
flatten_artists_2 <- function(df){
  genre <- unique(df$genre)
  year <- unique(df$year)
  unique_artists <- sort(unique(df$collaborator))
  artists <- (paste(unique_artists, collapse="\n"))
  num_artists <- length(unique_artists)
  new_df <- data.frame(genre = genre, year = year, num_artists = num_artists, artists = artists)
  return(new_df)
}

#___________________________________________________________________________________________________________
#DATA PROCESSING____________________________________________________________________________________________
#___________________________________________________________________________________________________________

target_artist_id = "1vyhD5VmyZ7KMfW5gqLgo5"
target_artist_name = "J Balvin"
remix_color = "#00BA38"
single_color = "#619CFF"
album_color = "#F8766D"

t <- list(
  family = "Helvetica",
  size = 14)

# TRACKS
# Open CSV containing information about all of J Balvin's released tracks (albums and singles) retrieved from Spotify API.
setwd("/Users/lindsaymaggioncalda/Documents/J Balvin Project/j-balvin-spotifyr/Web App/jb-tracks-all")
jb_tracks_all_file <- list.files("/Users/lindsaymaggioncalda/Documents/J Balvin Project/j-balvin-spotifyr/Web App/jb-tracks-all") %>% tail(n = 1)
jb_tracks_all <- read.csv(jb_tracks_all_file)
jb_tracks_all <- jb_tracks_all[,!(names(jb_tracks_all) %in% c("X"))]
setwd("/Users/lindsaymaggioncalda/Documents/J Balvin Project/j-balvin-spotifyr/Web App")

# Label remixes and alternate versions of the same song as 'remixes' rather than singles or album tracks.
# Mi Gente with Beyonce (0GzmMQizDeA2NVMUaZksv0, 5tSSdDqt0UvWXbxqRd9hTk, 1DoK3CdipMjplLk5LXUdcp, 0SjzIvde8QyAGeXwOgy9rs) and Sin Compromiso (feat. Jowell Y Randy) (0gHSFpCQZmZ1LwPHQpzn6T) should also be labeled as a remix.
label_as_remix <- c("0GzmMQizDeA2NVMUaZksv0", "0gHSFpCQZmZ1LwPHQpzn6T", "5tSSdDqt0UvWXbxqRd9hTk", "1DoK3CdipMjplLk5LXUdcp", "0SjzIvde8QyAGeXwOgy9rs")
is_remix <- grepl('remix', tolower(jb_tracks_all$track_name)) | grepl('dub', tolower(jb_tracks_all$track_name)) | grepl('edit', tolower(jb_tracks_all$track_name)) | (grepl('mix', tolower(jb_tracks_all$track_name)) & !grepl('mixtape volume 1', tolower(jb_tracks_all$track_name))) | grepl('version', tolower(jb_tracks_all$track_name)) | grepl('acappella', tolower(jb_tracks_all$track_name)) | grepl('spanglish', tolower(jb_tracks_all$track_name)) | grepl('instrumental', tolower(jb_tracks_all$track_name)) | (jb_tracks_all$track_id %in% label_as_remix)
is_single <- grepl('single', jb_tracks_all$album_type)
is_album <- grepl('album', jb_tracks_all$album_type)
jb_tracks_all$album_type <- case_when(is_remix ~ 'remix',
                                      is_single ~ 'single',
                                      is_album ~ 'album')

# Delete songs that do not exist on Spotify (Tranquila listed as a song on Energía..., Mi Gente - Mariciano Remix, Bum Bum Tam Tam Jonas Blue Extended Mix) 
nonexistent <- c("2tm6ZO4t9MoDVvnZyY1hDt", "4ItohW9QW9ImmP8aGmt9sh", "3JD39Mvo447EhbkgaZ1Vbz")
jb_tracks_all <- jb_tracks_all %>% filter(track_id %in% nonexistent == FALSE)

# Get rid of duplicate tracks (some songs are simply listed more than once on Spotify). How you do this depends on whether you want to keep the first-released version of the song or the album-version.
jb_tracks_preserve_albums <- tracks_helper(jb_tracks_all, "album")
jb_tracks_preserve_release_date <- tracks_helper(jb_tracks_all, "release_date")

jb_tracks <- jb_tracks_preserve_albums

all_tracks_DT <- datatable(
  jb_tracks, caption = paste("All tracks released on Spotify by J Balvin (excluding duplicate songs released on separate albums)"), extensions = 'Buttons', options = list(
    dom = 'Bfrtip',
    pageLength = 5,
    scrollX = TRUE,
    buttons = c('csv')
  )
)


# ALBUMS
album_names <- (jb_tracks_all %>% filter(album_type == "album"))$album_name %>% unique()
og_versions <- c("La Familia", "Energía")
deluxe_versions <- c("La Familia B Sides", "Energía Lado B")
albums_only <- jb_tracks_all %>% filter(album_name %in% album_names) %>% arrange(track_name, -track_popularity) %>% distinct(track_name, album_name, .keep_all = TRUE) %>% arrange(desc(album_release_date), desc(track_number))
col_order_albums <- c("track_name", "track_number", "album_name",
               "album_release_year", "album_type", "track_popularity", "artists", "duration_ms", "tempo", "valence", "energy", "danceability", "speechiness", "mode_name", "key_mode", "key", "artist_ids", "track_id")
albums_only <- albums_only[, col_order_albums]
albums_only_og <- filter(albums_only, (album_name %in% deluxe_versions) == FALSE)
albums_only_all <- filter(albums_only, (album_name %in% og_versions) == FALSE)

# COLLABORATIONS

jb_collabs <- get_collaborators_artist(target_artist_id, target_artist_name, jb_tracks)
jb_collabs <- jb_collabs %>% arrange(desc(year))

# Merge with track information dataframe to get information about the songs.
jb_collabs_full <- left_join(jb_collabs, jb_tracks_all, by = "track_id")

col_order <- c("artist1", "artist2", "year", "track_name", "album_type", "album_name", "duration_ms", "valence", "energy", "danceability", "speechiness", "mode_name", "key_mode", "key", "artists", "artist_ids", "track_id", "id1", "id2", "album_release_year")
jb_collabs_full <- jb_collabs_full[, col_order]

# table holding information about all of J Balvin's collaborations.
collab_DT <- datatable(
  jb_collabs_full, caption = "All tracks released on Spotify by J Balvin that featured other artists", extensions = 'Buttons', options = list(
    dom = 'Bfrtip', scrollX = TRUE, pageLength = 5,
    buttons = c('csv')
  )
)

# NETWORK

# Artist vertices
setwd("/Users/lindsaymaggioncalda/Documents/J Balvin Project/j-balvin-spotifyr/Web App/artist-vertices")
artist_vertices_file <- list.files("/Users/lindsaymaggioncalda/Documents/J Balvin Project/j-balvin-spotifyr/Web App/artist-vertices") %>% tail(n = 1)
artist_vertices <- read.csv(artist_vertices_file)
artist_vertices <- artist_vertices[,!(names(artist_vertices) %in% c("X"))]
artist_vertices_date <- str_split(artist_vertices_file, "_")[[1]][2]
setwd("/Users/lindsaymaggioncalda/Documents/J Balvin Project/j-balvin-spotifyr/Web App")

# Add columns with genres.
index <- which(artist_vertices$name == target_artist_name)
jb_genres <- artist_vertices[index,]$genres
jb_genres <-get_genres(jb_genres)

i <- 1
while (i < length(jb_genres)+1) {
  genre <-jb_genres[i]
  bool_vec <- apply(artist_vertices, 1, contains_certain_genre, c(genre))
  artist_vertices[, genre] <- bool_vec
  i <- i+1 
}

# Pics of network

artist_vertices_DT <- datatable(
  artist_vertices, caption = "All artists in the Artist Network (Billboard Top 100 artists and their collaborators)", extensions = 'Buttons', options = list(
    dom = 'Bfrtip',
    scrollX = TRUE,
    pageLength = 5,
    buttons = c('csv')
  )
)

# Create network
setwd("/Users/lindsaymaggioncalda/Documents/J Balvin Project/j-balvin-spotifyr/Web App/collaboration-info")
collaboration_info_file <- paste("collaboration-info", artist_vertices_date, ".csv", sep = "_")
collaboration_info <- read_delim(collaboration_info_file, delim=',', escape_double=FALSE, escape_backslash=TRUE)
collaborations <- filter(collaboration_info, id2 != "")
setwd("/Users/lindsaymaggioncalda/Documents/J Balvin Project/j-balvin-spotifyr/Web App")

edge_ids <- data.frame(source = collaborations$id1, target = collaborations$id2) %>% unique()
artist_network <- graph_from_data_frame(d=unique(edge_ids), vertices = unique(artist_vertices), directed = FALSE)

# Components
connected_components <- components(artist_network)
num_1 <- connected_components$csize[connected_components$csize == 1]
num_1_to_4 <- connected_components$csize[connected_components$csize %in% c(1,2,3,4)]

networkClusterText1 <- paste(NROW(artist_vertices %>% filter(type == "billboard")),
                            " artists have made it onto the Billboard Artist Top 100 in the past decade. After finding all of their collaborations using the Spotify API and adding the collaborators to the graph, the network contains ",
                            NROW(artist_vertices), " vertices and ", NROW(edge_ids),
                            " edges.", sep = "")

networkClusterText2 <- paste("There are ", length(connected_components$csize),
                             " distinct components of the graph. The largest component contains ",
                             format(max(connected_components$csize), scientific=F),
                             " artists, or ", round(max(connected_components$csize)/NROW(artist_vertices)*100,2),
                             "% of the artists, while the second largest component has only ",
                             Rfast::nth(connected_components$csize, 2, descending = T),
                             " artists. ", length(num_1), " components (",
                             round(length(num_1)/length(connected_components$csize)*100, 2),
                             "% of the components) comprise only 1 artist. ", length(num_1_to_4),
                             " components (", round(length(num_1_to_4)/length(connected_components$csize)*100,2),
                             "% of the components) connect up to 4 artists.", sep = "")

#___________________________________________________________________________________________________________
#SERVER_____________________________________________________________________________________________________
#___________________________________________________________________________________________________________

shinyServer(function(input, output, session) {
  
  #TRACKS_____________________________________________________________________________________________________
  
  # The table holding information about all of J Balvin's released tracks on Spotify.
  output$all_tracks_DT <- DT::renderDataTable(all_tracks_DT)
  
  #________________# NUMBER OF RELEASED TRACKS
  
  output$totalnumsongs <- renderText(paste("J Balvin has released ", NROW(jb_tracks), " unique songs (all songs)."))
  output$totalnumsongsnoremix <- renderText(paste("J Balvin has released ", NROW(jb_tracks %>% filter(album_type != "remix")), " unique original songs (only single/album releases)."))
  
  types_chosen_numtracks <- reactive(input$type_choice_numtracks)
  types_chosen_key <- reactive(input$type_choice_key)
  should_preserve_albums <- reactive(input$preserve_albums_check)

  output$numreleasedtracks <- renderPlotly({
    
    colors <- {
      if(all(types_chosen_numtracks() == c("remix"))){
        c(remix_color)
      }
      else if(all(types_chosen_numtracks() == c("album"))){
        c(album_color)
      }
      else if(all(types_chosen_numtracks() == c("single"))){
        c(single_color)
      }
      else if(all(types_chosen_numtracks() == c("album", "remix"))){
        c(remix_color, album_color)
      }
      else if(all(types_chosen_numtracks() == c("single", "album"))){
        c(single_color, album_color)
      }
      else if(all(types_chosen_numtracks() == c("single", "remix"))){
        c(remix_color, single_color)
      }
      else if(all(types_chosen_numtracks() == c("single", "album", "remix"))){
        c(remix_color, single_color, album_color)
      }
      else {
        c(single_color, album_color, remix_color)
      }
    }
    
    tracks_preserved <- {
      if (should_preserve_albums() == FALSE){
        jb_tracks_preserve_release_date
      } else {
        jb_tracks
      }
    }
    
    # Group by album year and album type to find how many tracks were released per year and what kind of release it was (album, single, or remix).
    num_tracks <- reactive(tracks_preserved %>% filter(album_type %in% types_chosen_numtracks()) %>%
      group_by(album_release_year, album_type, track_name) %>%
      dplyr::summarize(num_tracks = n()))

    # Plot the number of tracks released per year.
    num_released_tracks_gg <- ggplot(num_tracks(), aes_string(fill = "factor(album_type, c('remix','single','album'))", x="album_release_year", weight="num_tracks", track_name="track_name", num_tracks = "num_tracks"))+
      geom_bar(color = 'white')+
      ggtitle("Number of Tracks Released by Year")+
      theme_minimal()+
      theme(plot.title = element_text(hjust = 0.5))+
      scale_y_continuous(breaks=seq(0,NROW(tracks_preserved),5))+
      scale_x_continuous(breaks=seq(min(tracks_preserved$album_release_year), max(tracks_preserved$album_release_year), 1))+
      scale_fill_manual(values=colors, name = "Type\nof Release")
    num_released_tracks_plotly <- ggplotly(num_released_tracks_gg, source = "select", 
                                           tooltip = c("track_name")) %>% layout(legend = list(itemdoubleclick = FALSE))

    num_released_tracks_plotly %>% layout(font=t)
    })
  
  #________________# KEY OF RELEASED TRACKS
  
  output$keyreleasedtracks <- renderPlotly({
    
    colors <- {
      if(all(types_chosen_key() == c("remix"))){
        c(remix_color)
      }
      else if(all(types_chosen_key() == c("album"))){
        c(album_color)
      }
      else if(all(types_chosen_key() == c("single"))){
        c(single_color)
      }
      else if(all(types_chosen_key() == c("album", "remix"))){
        c(remix_color, album_color)
      }
      else if(all(types_chosen_key() == c("single", "album"))){
        c(single_color, album_color)
      }
      else if(all(types_chosen_key() == c("single", "remix"))){
        c(remix_color, single_color)
      }
      else if(all(types_chosen_key() == c("single", "album", "remix"))){
        c(remix_color, single_color, album_color)
      }
      else {
        c(single_color, album_color, remix_color)
      }
    }
    
    # Group by key to find how many tracks there are in each key and what kind of release it was (album, single, or remix).
    key <- reactive(jb_tracks %>% filter(album_type %in% types_chosen_key()) %>%
                    group_by(key_mode, album_type,track_name) %>%
                    dplyr::summarize(num_tracks = n(),track_name) %>% arrange(num_tracks))
    
    # Plot the number of tracks released per key.
    key_of_tracks_gg <- ggplot(key(), aes_string(fill = "factor(album_type, c('remix','single','album'))", x="reorder(key_mode, num_tracks, sum)", y="num_tracks", track_name = "track_name"))+
      geom_bar(stat='identity', color = "white", width = .5)+
      labs(title=" ",
           x ="Key", y = "Number of songs")+
      theme_minimal()+
      scale_y_continuous(breaks=seq(0,NROW(jb_tracks),5))+
      scale_fill_manual(values=colors, name = "Type\nof Release")+
      coord_flip()
    key_of_tracks_plotly <- ggplotly(key_of_tracks_gg, tooltip = c("track_name")) %>% layout(font=t)
  })
  
  #______# Coord graph
  
  jb_tracks_collab <- jb_tracks
  is_collab_vec <- unlist(lapply(jb_tracks_collab$artists, is_collab))
  jb_tracks_collab$is_collab <- is_collab_vec
  
  coord_graph_tracks <- reactive({
    switch(input$group_by_choice,
           album_name = albums_only_og,
           album_release_year = jb_tracks,
           album_type = jb_tracks,
           is_collab = jb_tracks_collab
    )
  })
  
  output$x_description <- renderUI({
    switch(input$x_axis_choice,
           track_length = HTML("<p class = 'no-indent'>The duration of a track in minutes.</p>"),
           valence = HTML("<p class = 'no-indent'>Positivity (which the Spotify API calls valence) is a measure from 0.0 to 1.0 that describes the musical positiveness conveyed by a track. Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), while tracks with low valence sound more negative (e.g. sad, depressed, angry) (<a href='https://developer.spotify.com/documentation/web-api/reference/#category-tracks'>Spotify API documentation</a>)</p>"),
           energy = HTML("<p class = 'no-indent'>Energy is a measure from 0.0 to 1.0 and represents a perceptual measure of intensity and activity. Typically, energetic tracks feel fast, loud, and noisy. For example, death metal has high energy, while a Bach prelude scores low on the scale. Perceptual features contributing to this attribute include dynamic range, perceived loudness, timbre, onset rate, and general entropy (<a href='https://developer.spotify.com/documentation/web-api/reference/#category-tracks'>Spotify API documentation</a>)</p>"),
           danceability = HTML("<p class = 'no-indent'>Danceability describes how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability, beat strength, and overall regularity. A value of 0.0 is least danceable and 1.0 is most danceable (<a href='https://developer.spotify.com/documentation/web-api/reference/#category-tracks'>Spotify API documentation</a>)</p>"),
           speechiness = HTML("<p class = 'no-indent'>Speechiness detects the presence of spoken words in a track. The more exclusively speech-like the recording (e.g. talk show, audio book, poetry), the closer to 1.0 the attribute value. Values above 0.66 describe tracks that are probably made entirely of spoken words. Values between 0.33 and 0.66 describe tracks that may contain both music and speech, either in sections or layered, including such cases as rap music. Values below 0.33 most likely represent music and other non-speech-like tracks (<a href='https://developer.spotify.com/documentation/web-api/reference/#category-tracks'>Spotify API documentation</a>)</p>")
    )
  })
  
  output$y_description <- renderUI({
    switch(input$y_axis_choice,
           track_length = HTML("<p class = 'no-indent'>The duration of a track in minutes.</p>"),
           valence = HTML("<p class = 'no-indent'>Positivity (which the Spotify API calls valence) is a measure from 0.0 to 1.0 that describes the musical positiveness conveyed by a track. Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), while tracks with low valence sound more negative (e.g. sad, depressed, angry) (<a href='https://developer.spotify.com/documentation/web-api/reference/#category-tracks'>Spotify API documentation</a>)</p>"),
           energy = HTML("<p class = 'no-indent'>Energy is a measure from 0.0 to 1.0 and represents a perceptual measure of intensity and activity. Typically, energetic tracks feel fast, loud, and noisy. For example, death metal has high energy, while a Bach prelude scores low on the scale. Perceptual features contributing to this attribute include dynamic range, perceived loudness, timbre, onset rate, and general entropy (<a href='https://developer.spotify.com/documentation/web-api/reference/#category-tracks'>Spotify API documentation</a>)</p>"),
           danceability = HTML("<p class = 'no-indent'>Danceability describes how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability, beat strength, and overall regularity. A value of 0.0 is least danceable and 1.0 is most danceable (<a href='https://developer.spotify.com/documentation/web-api/reference/#category-tracks'>Spotify API documentation</a>)</p>"),
           speechiness = HTML("<p class = 'no-indent'>Speechiness detects the presence of spoken words in a track. The more exclusively speech-like the recording (e.g. talk show, audio book, poetry), the closer to 1.0 the attribute value. Values above 0.66 describe tracks that are probably made entirely of spoken words. Values between 0.33 and 0.66 describe tracks that may contain both music and speech, either in sections or layered, including such cases as rap music. Values below 0.33 most likely represent music and other non-speech-like tracks (<a href='https://developer.spotify.com/documentation/web-api/reference/#category-tracks'>Spotify API documentation</a>)</p>")
    )
  })
  
  output$coordgraph <- renderPlotly({
    
  group_by_choice <- input$group_by_choice
  x_choice <- input$x_axis_choice
  y_choice <- input$y_axis_choice
  
  if (input$coordgraph_check){
    #Show subplots
    
    #Num categories
    num_cat <- length(unique(coord_graph_tracks()[,(names(coord_graph_tracks()) == group_by_choice)]))
    num_row <- sqrt(round(sqrt(num_cat)) ** 2)
    
    
    # Basic scatter plot
    x <- ggplot(coord_graph_tracks(), aes_string(x="get(x_choice)", y="get(y_choice)", 
                                     color="fct_reorder(as.factor(get(group_by_choice)), album_release_year, .desc = FALSE)",
                                     track_name = "track_name"))+ 
    geom_point(alpha = .5)+
      theme_minimal()+
      #scale_y_continuous(breaks=seq(0, 1, .25))+
      labs(title = "",
           y = y_choice,
           x = x_choice,
           col = " ")+
      scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
      facet_wrap(~get(group_by_choice), nrow=num_row)+
      geom_hline(yintercept=.5, size = .125)+
      geom_vline(xintercept=.5, size = .125)
     
    
    ggplotly(x, tooltip = c("track_name")) %>% layout(
      xaxis = list(range = c(0, 1), rangemode = "tozero"),
      yaxis = list(range = c(0, 1), rangemode = "tozero"))
    
    
  }
  else {
    # Show single plot
    
    single_coord_plot <- ggplot(coord_graph_tracks(), aes_string(x="get(x_choice)", y="get(y_choice)",
                                          color="fct_reorder(as.factor(get(group_by_choice)), album_release_year, .desc = FALSE)",
                                                            track_name = "track_name", group = "get(group_by_choice)"))+ 
      geom_point(aes(size = track_popularity), alpha = .5)+
      theme_minimal()+
      #scale_y_continuous(breaks=seq(0, 1, .25))+
      labs(title = "",
           y = y_choice,
           x = x_choice,
           col = " ")+
      scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
      geom_hline(yintercept=.5, size = .25)+
      geom_vline(xintercept=.5, size = .25)
    
    
    ggplotly(single_coord_plot, tooltip = c("track_name", "track_popularity")) %>% layout(
      xaxis = list(range = c(0, 1), rangemode = "tozero"),
      yaxis = list(range = c(0, 1), rangemode = "tozero")) %>% layout(font=t)
    
  }
  
  
  })
  
  #ALBUMS_____________________________________________________________________________________________________
  
  output$albums_DT <- DT::renderDataTable(DT::datatable(
    albums_only, caption = paste("All tracks released on albums"), extensions = 'Buttons', options = list(
      dom = 'Bfrtip',
      pageLength = 5,
      scrollX = TRUE,
      buttons = c('csv')
    )
  ))
  
  
  # FIX THIS: add total number of unique songs across all albums.
  #output$album_num_songs_text <- renderText(paste(artist_name, " released a total of ", NROW(albums_only_og), " songs on his original albums: J Balvin Mix Tape, La Familia, Energía, Vibras, OASIS, Colores, and JOSE.", sep = ""))
  
  #________________# NUM TRACKS PER ALBUM
  
  which_albums <- reactive({
    switch(input$radio_album_num,
           only_og = albums_only_og,
           only_all = albums_only_all
    )
  })
  
  output$numtrackseachalbum <- renderPlotly({
    # Group by album to find the total number of songs of each album.
    album_num_songs <- which_albums() %>%
      group_by(album_name, album_release_year, track_name, track_number) %>%
      dplyr::summarize(count = n()) %>% arrange(desc(track_number))
    
    # Plot albums by total album length.
    album_num_tracks_gg <- ggplot(album_num_songs, aes_string(x="album_name", y="count", track_name = "track_name"))+
      geom_bar(stat='identity', fill = "turquoise", color = "white", width = .8)+
      aes(x = fct_reorder(album_name, album_release_year, .desc = TRUE))+
      theme_minimal()+
      scale_y_continuous(breaks=seq(0,NROW(jb_tracks),5))+
      labs(title = "",
           y = "Total Number of Tracks",
           x = "Album")+
      coord_flip()
    
    album_num_tracks_plotly <- ggplotly(album_num_tracks_gg, tooltip = c("track_name"))
    album_num_tracks_plotly %>% layout(font=t)
  })
  
  #________________# DURATION OF ALBUM
  
  which_albums_dur <- reactive({
    switch(input$radio_album_dur,
           only_og = albums_only_og,
           only_all = albums_only_all
    )
  })
  
  output$lengtheachalbum <- renderPlotly({
    # Group by album to find the total duration of each album.
    song_durations <- which_albums_dur() %>%
      group_by(album_name, album_release_year, track_name) %>%
      dplyr::summarize(total_duration = sum(duration_ms)/60000) %>% arrange(album_release_year)
    album_durations <- which_albums_dur() %>%
      group_by(album_name, album_release_year) %>%
      dplyr::summarize(total_duration = sum(duration_ms)/60000) %>% arrange(album_release_year)
    
    durations <- merge(song_durations, album_durations, by.x=c("album_name", "album_release_year"), by.y=c("album_name", "album_release_year"))
    names(durations)[names(durations) == 'total_duration.y'] <- 'album_duration'
    names(durations)[names(durations) == 'total_duration.x'] <- 'track_duration_min'
    durations$track_duration_min <- round(durations$track_duration_min,2)
    
    # Plot albums by total album length.
    length_albums_gg <- ggplot(durations, aes_string(x="album_name", y="track_duration_min", track_name = "track_name", group = "-track_duration_min"))+
      geom_bar(stat='identity', fill = "orchid", color = "white", width = .8)+
      aes(x = fct_reorder(album_name, album_release_year, .desc = TRUE))+
      scale_y_continuous(breaks=seq(0,300,10))+
      coord_flip()+
      theme_minimal()+
      labs(title = "",
           y = "Total Duration (min)",
           x = "Album")
    length_albums_plotly <- ggplotly(length_albums_gg, tooltip = c("track_name", "track_duration_min"))
    length_albums_plotly %>% layout(font=t)
  })
  
  #________________# TRACK LENGTH ON ALBUM
  
  which_albums_att <- reactive({
    switch(input$radio_album_att,
           only_og = albums_only_og,
           only_all = albums_only_all
    )
  })
  
  which_attribute_text <- reactive({
    switch(input$attribute_choice,
           track_length = "Track Length (min)",
           valence = "Positivity",
           energy = "Energy",
           danceability = "Danceability",
           speechiness = "Speechiness"
    )
  })
  
  output$attribute_description <- renderUI({
    switch(input$attribute_choice,
           track_length = HTML("<p class = 'no-indent'>The duration of a track in minutes.</p>"),
           valence = HTML("<p class = 'no-indent'>Positivity (which the Spotify API calls valence) is a measure from 0.0 to 1.0 that describes the musical positiveness conveyed by a track. Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), while tracks with low valence sound more negative (e.g. sad, depressed, angry) (<a href='https://developer.spotify.com/documentation/web-api/reference/#category-tracks'>Spotify API documentation</a>)</p>"),
           energy = HTML("<p class = 'no-indent'>Energy is a measure from 0.0 to 1.0 and represents a perceptual measure of intensity and activity. Typically, energetic tracks feel fast, loud, and noisy. For example, death metal has high energy, while a Bach prelude scores low on the scale. Perceptual features contributing to this attribute include dynamic range, perceived loudness, timbre, onset rate, and general entropy (<a href='https://developer.spotify.com/documentation/web-api/reference/#category-tracks'>Spotify API documentation</a>)</p>"),
           danceability = HTML("<p class = 'no-indent'>Danceability describes how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability, beat strength, and overall regularity. A value of 0.0 is least danceable and 1.0 is most danceable (<a href='https://developer.spotify.com/documentation/web-api/reference/#category-tracks'>Spotify API documentation</a>)</p>"),
           speechiness = HTML("<p class = 'no-indent'>Speechiness detects the presence of spoken words in a track. The more exclusively speech-like the recording (e.g. talk show, audio book, poetry), the closer to 1.0 the attribute value. Values above 0.66 describe tracks that are probably made entirely of spoken words. Values between 0.33 and 0.66 describe tracks that may contain both music and speech, either in sections or layered, including such cases as rap music. Values below 0.33 most likely represent music and other non-speech-like tracks (<a href='https://developer.spotify.com/documentation/web-api/reference/#category-tracks'>Spotify API documentation</a>)</p>")
    )
  })
                                                                                                                                                      
  
                                                                                                                                                      
                                                                                                                                                      
  output$whichattributetext <- renderText(which_attribute_text())
  
  which_attribute <- reactive(input$attribute_choice)
  
  output$attributeplot <- renderPlotly({
    
    albums_in_order <- unique((which_albums_att()[order(which_albums_att()$album_release_year),])$album_name)
    xform <- list(categoryorder = "array",
                  categoryarray = albums_in_order)
    
    albums_track_dur <- mutate(which_albums_att(), duration_min = round(duration_ms/60000,2))
    
    # Track Length
    # Plot each album's songs and their durations, and show the average length per album, excluding Mixtape Volume 1.
    track_length <- albums_track_dur %>% filter(track_name != "Mixtape Volume 1") %>%
      plot_ly(
        x = ~album_name,
        y = ~duration_min,
        split = ~album_name,
        type = 'violin',
        box = list(
          visible = T
        ),
        meanline = list(
          visible = T
        )
      ) 
    track_length <- track_length %>%
      layout(
        xaxis = xform,
        yaxis = list(
          title = "Track Length (min)",
          zeroline = F
        )
      )
    track_length <- track_length %>% add_markers(x = ~album_name, 
                                                 y = ~duration_min, color = I("black"), 
                                                 text = ~paste('track_name: ', track_name)) %>% layout(showlegend = FALSE)
    
    # Valence
    valence <- which_albums_att() %>%
      plot_ly(
        x = ~album_name,
        y = ~valence,
        split = ~album_name,
        type = 'violin',
        box = list(
          visible = T
        ),
        meanline = list(
          visible = T
        )
      ) 
    
    valence <- valence %>%
      layout(
        xaxis = xform,
        yaxis = list(
          title = "Positivity",
          zeroline = F
        )
      )
    
    valence <- valence %>% add_markers(x = ~album_name, y = ~valence, color = I("black"), text = ~paste('track_name: ', track_name)) %>% layout(showlegend = FALSE)
    
    # Energy
    energy <- which_albums_att() %>%
      plot_ly(
        x = ~album_name,
        y = ~energy,
        split = ~album_name,
        type = 'violin',
        box = list(
          visible = T
        ),
        meanline = list(
          visible = T
        )
      ) 
    
    energy <- energy %>%
      layout(
        xaxis = xform,
        yaxis = list(
          title = "Energy",
          zeroline = F
        )
      )
    
    energy <- energy %>% add_markers(x = ~album_name, y = ~energy, color = I("black"), text = ~paste('track_name: ', track_name)) %>% layout(showlegend = FALSE)
    
    # Danceability
    danceability <- which_albums_att() %>%
      plot_ly(
        x = ~album_name,
        y = ~danceability,
        split = ~album_name,
        type = 'violin',
        box = list(
          visible = T
        ),
        meanline = list(
          visible = T
        )
      ) 
    
    danceability <- danceability %>%
      layout(
        xaxis = xform,
        yaxis = list(
          title = "Danceability",
          zeroline = F
        )
      )
    
    danceability <- danceability %>% add_markers(x = ~album_name, y = ~danceability, color = I("black"), text = ~paste('track_name: ', track_name)) %>% layout(showlegend = FALSE)
    
    # Speechiness
    speechiness <- which_albums_att() %>%
      plot_ly(
        x = ~album_name,
        y = ~speechiness,
        split = ~album_name,
        type = 'violin',
        box = list(
          visible = T
        ),
        meanline = list(
          visible = T
        )
      ) 
    
    speechiness <- speechiness %>%
      layout(
        xaxis = xform,
        yaxis = list(
          title = "Speechiness",
          zeroline = F
        )
      )
    
    speechiness <- speechiness %>% add_markers(x = ~album_name, y = ~speechiness, color = I("black"), text = ~paste('track_name: ', track_name)) %>% layout(showlegend = FALSE)
    
    plotToReturn <- switch(which_attribute(),
                           track_length = track_length,
                           valence = valence,
                           energy = energy,
                           danceability = danceability,
                           speechiness = speechiness
    )
    plotToReturn %>% layout(font=t)
  })
  
  #_______________# KEY
  
  which_albums_key <- reactive({
    switch(input$radio_album_key,
           only_og = albums_only_og,
           only_all = albums_only_all
    )
  })
  
  output$keyalbum <- renderPlotly({
    # Plot key distribution for each album.
    x <- which_albums_key() %>%
      ggplot(aes_string(y = "key_mode", x = "album_name", track_name="track_name")) + 
      ggbeeswarm::geom_beeswarm(groupOnX = TRUE, aes(color = album_name), size = 1) + 
      guides(color = FALSE) +
      theme_minimal()+
      aes(x = fct_reorder(album_name, album_release_year, .desc = FALSE))+
      labs(title = "", 
           subtitle = "Dots represents songs.",
           y = "Key",
           x = "Album")
    ggplotly(x, source = "select", tooltip = c("track_name")) %>% layout(showlegend = FALSE) %>% layout(font=t)
  })
  
  #______________# MODE
  
  which_albums_mode <- reactive({
    switch(input$radio_album_mode,
           only_og = albums_only_og,
           only_all = albums_only_all
    )
  })
  
  output$modealbum <- renderPlotly({
    # Group by mode and album to determine how tracks on each album are in major and minor key.
    modee <- which_albums_mode() %>%
      group_by(mode_name, album_name, album_release_year) %>%
      dplyr::summarize(percentage = n()) %>% arrange(percentage)
    
    # Plot proportions of major/minor key tracks for each album.
    mode_key <- ggplot(modee, aes(fill = mode_name, x=album_name, y=percentage))+
      geom_bar(position="fill", stat='identity')+
      scale_fill_manual(values = c("plum1","royalblue1"), name = "Mode")+
      theme_minimal()+
      theme(plot.title = element_text(hjust = 0.5))+
      scale_y_continuous(breaks=seq(0,1,.1))+
      aes(x = fct_reorder(album_name, album_release_year, .desc = FALSE))+
      labs(title = 'Percentage of Songs on Each Album that are in Major/Minor', 
           y = "Percentage",
           x = "Album")
    ggplotly(mode_key, tooltip = c("percentage")) %>% layout(font=t)
  })
  
  
  #COLLABORATIONS_____________________________________________________________________________________________________
  
  output$collab_DT <- DT::renderDataTable(collab_DT)
  
  #______________# Collabs vs solos
  
  jb_tracks_collab <- jb_tracks
  is_collab_vec <- unlist(lapply(jb_tracks_collab$artists, is_collab))
  jb_tracks_collab$is_collab <- is_collab_vec
  
  num_percent_choice <- reactive(input$num_percent_choice)
  num_percent_include_remix <- reactive(input$num_percent_remix_check)
  
  collaborative_songs <- jb_tracks_collab %>% filter(is_collab == "Collaboration")
  
  output$collab_vs_solo1 <- renderText(paste("All tracks: ", target_artist_name, " has released ", NROW(collaborative_songs)," collaborative tracks — ", round((NROW(collaborative_songs)/NROW(jb_tracks_collab))*100, 2), "% of his releases.", sep=""))
  output$collab_vs_solo2 <- renderText(paste("Singles/Albums only: ", target_artist_name, " has released ", NROW(collaborative_songs %>% filter(album_type != "remix"))," collaborative tracks — ", round((NROW(collaborative_songs %>% filter(album_type != "remix"))/NROW(jb_tracks_collab %>% filter(album_type != "remix")))*100,2), "% of his releases.", sep = ""))
  
  
  output$collabs_vs_solos <- renderPlotly({
    
    tracks_to_look_at <- jb_tracks_collab
    
    if (num_percent_include_remix() == FALSE){
      # Don't include remixes
      tracks_to_look_at <- tracks_to_look_at %>% filter(album_type != "remix")
    }
    
    jb_is_collab <- tracks_to_look_at %>% group_by(album_release_year, is_collab, track_name) %>% dplyr::summarize(count = n()) %>% arrange(-count)
    num_stacked <- ggplot(jb_is_collab, aes_string(fill="is_collab", x="album_release_year", y="count", track_name = "track_name"))+
      geom_bar(stat='identity', color = "white")+
      labs(title = "Number of Collab vs. Solo Songs Per Year (Stacked)")+
      scale_fill_manual(values=c("mediumaquamarine", "palevioletred"), name="Type", breaks=c("Collaboration", "Solo"))+
      theme_minimal()+
      theme(plot.title = element_text(hjust = 0.5))+
      #theme(axis.text.x = element_text(angle = 90, size = 10))+
      scale_x_continuous(breaks=seq(min(tracks_to_look_at$album_release_year), max(tracks_to_look_at$album_release_year), 1))+
      scale_y_continuous(breaks=seq(0,NROW(jb_tracks_collab),5))
    num_stacked <- ggplotly(num_stacked, tooltip = c("track_name"))
    
    jb_is_collab2 <- tracks_to_look_at %>% group_by(album_release_year, is_collab) %>% dplyr::summarize(count = n()) %>% arrange(-count)
    
    num_grouped <- ggplot(jb_is_collab2, aes_string(fill="is_collab", x="album_release_year", y="count"))+
      geom_bar(width = .3,position = "dodge",stat='identity', color = "white")+
      labs(title = "Number of Collab vs. Solo Songs Per Year (Grouped)")+
      scale_fill_manual(values=c("mediumaquamarine", "palevioletred"), name="Type", breaks=c("Collaboration", "Solo"))+
      theme_minimal()+
      theme(plot.title = element_text(hjust = 0.5))+
      scale_x_continuous(breaks=seq(min(tracks_to_look_at$album_release_year), max(tracks_to_look_at$album_release_year), 1))+
      scale_y_continuous(breaks=seq(0,NROW(jb_tracks_collab),5))
    num_grouped <- ggplotly(num_grouped, tooltip = c("count"))
    
    jb_is_collab <- tracks_to_look_at %>% group_by(album_release_year, is_collab) %>% dplyr::summarize(percentage = n()) %>% arrange(-percentage)
    per <- ggplot(jb_is_collab, aes_string(fill="is_collab", x="album_release_year", y="percentage"))+
       geom_bar(position = "fill", stat='identity')+
       labs(title = "Percentage of Songs that are Collaborations")+
       scale_fill_manual(values=c("mediumaquamarine", "palevioletred"), name="Type", breaks=c("Collaboration", "Solo"))+
       theme_minimal()+
       theme(plot.title = element_text(hjust = 0.5))+
       scale_x_continuous(breaks=seq(min(tracks_to_look_at$album_release_year), max(tracks_to_look_at$album_release_year), 1))+
       scale_y_continuous(breaks=seq(0,1,.1))
    per <- ggplotly(per, tooltip = c("percentage"))
    
    if (num_percent_choice() == "count_stacked") {
      num_stacked %>% layout(font=t)
    }
    else if (num_percent_choice() == "count_grouped") {
      num_grouped %>% layout(font=t)
    }
    else {
      per %>% layout(font=t)
    }
    
  })
  
  #_____________# Percentage of Singles that are Collabs
  
  jb_tracks_collab <- jb_tracks
  is_collab_vec <- unlist(lapply(jb_tracks_collab$artists, is_collab))
  jb_tracks_collab$is_collab <- is_collab_vec
  
  jb_is_collab_single <- jb_tracks_collab %>% filter(album_type == "single") %>% group_by(is_collab) %>% dplyr::summarize(count = n()) %>% arrange(-count)
  num_singles <- sum(jb_is_collab_single$count)
  jb_is_collab_single$percentage <- jb_is_collab_single$count/num_singles
  
  # Pie Chart
  fig <- plot_ly(jb_is_collab_single, labels = c("Collaboration", "Solo"), values = ~count, type = 'pie', marker = list(colors = c("mediumaquamarine", "palevioletred")))
  fig <- fig %>% layout(title = "Percentage of Singles that are Collaborations",
                        showlegend = FALSE,
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>% layout(font=list(
                          family = "Helvetica",
                          size = 12))
  
  output$singlespiechart <- renderPlotly(fig)
  
  # Bar graph
  
  jb_is_collab_single <- jb_tracks_collab %>% filter(album_type == "single") %>% group_by(is_collab, track_name) %>% dplyr::summarize(count = n()) %>% arrange(-count)
  singlesbar <- ggplot(jb_is_collab_single, aes_string(fill="is_collab", x="is_collab", y="count", track_name = "track_name"))+
    geom_bar(stat='identity', color = "white")+
    labs(title = "Number of Collaborative vs. Solo Singles", x = "")+
    scale_fill_manual(values=c("mediumaquamarine", "palevioletred"), name="Type", breaks=c("Collaboration", "Solo"))+
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5))+
    scale_y_continuous(breaks=seq(0,NROW(jb_tracks_collab),5))
  singlesbar <- ggplotly(singlesbar, tooltip = c("track_name")) %>% layout(font=t)
  
  output$singlesbarchart <- renderPlotly(singlesbar)
  
  #____________# Number of Collaborators

  types_chosen_collaborators <- reactive(input$type_choice_collaborators)
  output$testtext2 <- renderText(types_chosen_collaborators())
  
  output$totalnumcollaborators <- renderText(paste(target_artist_name, " has collaborated with ", length(unique(jb_collabs_full$artist2)), " total artists (all releases)."))
  output$totalnumcollaboratorsnoremix <- renderText(paste(target_artist_name, " has collaborated with ", length(unique((jb_collabs_full %>% filter(album_type != "remix"))$artist2)), " artists if we exclude remixes."))
  
  output$collaborator_plot <- renderPlotly({
    
    colors <- {
      if(all(types_chosen_collaborators() == c("remix"))){
        c(remix_color)
      }
      else if(all(types_chosen_collaborators() == c("album"))){
        c(album_color)
      }
      else if(all(types_chosen_collaborators() == c("single"))){
        c(single_color)
      }
      else if(all(types_chosen_collaborators() == c("album", "remix"))){
        c(remix_color, album_color)
      }
      else if(all(types_chosen_collaborators() == c("single", "album"))){
        c(single_color, album_color)
      }
      else if(all(types_chosen_collaborators() == c("single", "remix"))){
        c(remix_color, single_color)
      }
      else if(all(types_chosen_collaborators() == c("single", "album", "remix"))){
        c(remix_color, single_color, album_color)
      }
      else {
        c(single_color, album_color, remix_color)
      }
    }
  
  # Group by the collaborating artist to find how many times J Balvin has collaborated with individual artists and on what kind of release (album, single, or remix).
  num_collabs_person <- jb_collabs_full %>% filter(album_type %in% types_chosen_collaborators()) %>%
    group_by(collaborator = artist2, album_type) %>%
    dplyr::summarize(num_collabs_type = n())
  num_collabs_person_track <- jb_collabs_full %>% filter(album_type %in% types_chosen_collaborators()) %>%
    group_by(collaborator = artist2, album_type, track_name) %>%
    dplyr::summarize(num_collabs_track = n())
  
  num_collabs_person_track<- merge(num_collabs_person_track, num_collabs_person, by.x=c("collaborator", "album_type"), by.y=c("collaborator", "album_type"))
  names(num_collabs_person_track)[names(num_collabs_person_track) == 'num_collabs_type'] <- 'num_collabs'
  
  collaborators <- ggplot(num_collabs_person_track, aes_string(fill="factor(album_type, c('remix','single','album'))", x="reorder(collaborator, num_collabs_track, sum)", weight="num_collabs_track", track_name = "track_name"))+
    geom_bar(width = .7, color = "white")+
    labs(title = "Number of Tracks per Collaborator",
         y = "Number of Tracks with Collaborator",
         x = "Collaborator")+
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5))+
    scale_y_continuous(breaks=seq(0, (num_collabs_person$collaborator %>% unique() %>% length()),5))+
    coord_flip()+
    scale_fill_manual(values=colors, name = "Type\nof Release")
  collaborators <- ggplotly(collaborators, tooltip = c("track_name"))
  collaborators %>% layout(font=t)
  })
  
  #_________# Collaborators by Year
  
  types_chosen_collaborators_yr <- reactive(input$type_choice_collaborators_yr)
  
  output$collaborators_yr_plot <- renderPlotly({
    
    if (is.null(types_chosen_collaborators_yr())){
      return(NULL)
    }
    
    tracks_to_use <- jb_collabs_full %>% filter(album_type %in% types_chosen_collaborators_yr())
    split_df_artist <- split(tracks_to_use, f = tracks_to_use$artist2)
    collaborators_yr <- map_df(split_df_artist, flatten_songs)
    collaborators_yr$weight <- 1
    
    # Helper function to prepare dataframe for flattening
    flatten_songs <- function(single_artist_df){
      split_df_yr <- split(single_artist_df, f = single_artist_df$year)
      new_df <- map_df(split_df_yr, flatten_songs_2)
      return(new_df)
    }
    
    # Helper function to change format of dataframe, flatten all tracks for a given year and genre into one row
    flatten_songs_2 <- function(df){
      artist <- unique(df$artist2)
      year <- unique(df$year)
      unique_tracks <- sort(unique(df$track_name))
      tracks <- (paste(unique_tracks, collapse="\n"))
      num_tracks <- length(unique_tracks)
      new_df <- data.frame(artist = artist, tracks_together = tracks, num_tracks = num_tracks, year = year)
      return(new_df)
    }
    
    b <- ggplot(collaborators_yr, aes_string(fill="artist", x="year", weight="weight", tracks = "tracks"))+
      geom_bar(position = "stack", width = .7, color = "white")+
      labs(title = "",
           y = "Number of Collaborators that Year",
           x = "Year of Collaboration")+
      scale_y_continuous(breaks=seq(0, NROW(collaborators_yr), 5))+
      theme_minimal()
    ggplotly(b, tooltip = c("artist", "tracks")) %>% layout(showlegend = FALSE) %>% layout(font=t)
  })
  
  #_________# Number of Collaborations
  
  types_chosen_collaborations <- reactive(input$type_choice_collaborations)
  
  output$collaborations_plot <- renderPlotly({
    
    colors <- {
      if(all(types_chosen_collaborations() == c("remix"))){
        c(remix_color)
      }
      else if(all(types_chosen_collaborations() == c("album"))){
        c(album_color)
      }
      else if(all(types_chosen_collaborations() == c("single"))){
        c(single_color)
      }
      else if(all(types_chosen_collaborations() == c("album", "remix"))){
        c(remix_color, album_color)
      }
      else if(all(types_chosen_collaborations() == c("single", "album"))){
        c(single_color, album_color)
      }
      else if(all(types_chosen_collaborations() == c("single", "remix"))){
        c(remix_color, single_color)
      }
      else if(all(types_chosen_collaborations() == c("single", "album", "remix"))){
        c(remix_color, single_color, album_color)
      }
      else {
        c(single_color, album_color, remix_color)
      }
    }
  
  num_collabs_year <- jb_collabs_full %>% filter(album_type %in% types_chosen_collaborations()) %>% 
    group_by(year, album_type, track_name, artist2) %>%
    dplyr::summarize(num_collabs= n()) %>% arrange(-num_collabs)
  num_collabs_year_grouped <- jb_collabs_full %>% 
    group_by(year, album_type) %>%
    dplyr::summarize(num_collabs= n()) %>% arrange(-num_collabs)
  
  num_collabs_yr<- merge(num_collabs_year, num_collabs_year_grouped, by.x=c("year", "album_type"), by.y=c("year", "album_type"))
  names(num_collabs_yr)[names(num_collabs_yr) == 'num_collabs.y'] <- 'num_collabs_of_type'
  num_collabs_yr$collaborator <- num_collabs_yr$artist2
  
  num_collabs <- ggplot(num_collabs_yr, aes_string(fill="factor(album_type, c('remix','single','album'))", x="year", weight="num_collabs.x", track_name = "track_name", collaborator = "collaborator", num_collabs_of_type = "num_collabs_of_type"))+
    geom_bar(width = .7, color = "white")+
    labs(title = " ",
         y = "Number of Collaborations",
         x = "Year")+
    theme_minimal()+
    scale_y_continuous(breaks=seq(0, NROW(num_collabs_yr), 10))+
    scale_fill_manual(values=colors, name = "Type\nof Release")
  ggplotly(num_collabs, tooltip = c("track_name", "collaborator")) %>% layout(font=t)
  
  })
  
  #___________# Same vs. diff-genre collabs
  
  
  types_chosen_same_diff <- reactive(input$type_choice_same_diff_col)
  num_percent_choice_genre <- reactive(input$num_percent_choice_genre)
  
  # Merge the dataframe holding info about J Balvin's collaborations with the dataframe artist_vertices, which holds all artist info. See later in the document (under the networks section) to see how this dataframe of artist info was created.
  collaborations_genre_yr <- left_join(jb_collabs_full, artist_vertices, by = c("id1" = "id")) 
  collaborations_genre_yr <- left_join(collaborations_genre_yr, artist_vertices, by = c("id2" = "id"))
  
  # Drop all collaborations where we don't have genre data for the second artist
  collaborations_genre_yr <- subset(collaborations_genre_yr, select = c(name.x, genres.x, name.y, genres.y, year, album_type, track_name, track_id)) %>% filter(genres.y != 0)
  
  # Create new column that tells whether 1 or more genres are overlapping between J Balvin and the collaborator
  collaborations_genre_yr$same_genre <- apply(collaborations_genre_yr, 1, genre_overlap)
  
  # Get dataframe with all genres of collaborators
  collab_genre <- apply(collaborations_genre_yr, 1, exploder_genre)
  all_genres <- ldply(collab_genre, data.frame)
  
  num_collabs_genre <- all_genres %>%
    group_by(genre, album_type, collaborator, track_name)
  
  
  output$same_diff_collabs_plot <- renderPlotly({
  
  # Group by year and whether or not JB and the collaborator shared a genre to see how many same-genre and cross-genre JB does per year
  num_collabs_year_genre <- collaborations_genre_yr %>% filter(album_type %in% types_chosen_same_diff()) %>%
    group_by(year, same_genre, track_name, name.y, genres.y) %>%
    dplyr::summarize(num_collabs = n()) %>% arrange(-num_collabs)
  
  names(num_collabs_year_genre)[names(num_collabs_year_genre) == 'name.y'] <- 'collaborator'
  names(num_collabs_year_genre)[names(num_collabs_year_genre) == 'genres.y'] <- 'genres'
  
  # Plot how many same-genre and cross-genre collaborations JB does per year (Num Stacked)
  num <- ggplot(num_collabs_year_genre, aes_string(fill="same_genre", x="year", weight="num_collabs", track_name = "track_name", collaborator = "collaborator", genres = "genres"))+
    geom_bar(color = "white")+
    scale_fill_manual(values = c("plum1","royalblue1"), name = "Genre of\ncollaborator")+
    labs(title = "Number of Same-/Cross-Genre Collabs (Stacked)")+
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5))+
    scale_y_continuous(breaks=seq(0, NROW(num_collabs_year_genre), 10))
  num <- ggplotly(num, tooltip = c("track_name", "collaborator", "genres")) 
  
  # Num grouped
  
  num_collabs_year_genre_grouped <- collaborations_genre_yr %>% filter(album_type %in% types_chosen_same_diff()) %>%
    group_by(year, same_genre) %>%
    dplyr::summarize(num_collabs = n()) %>% arrange(-num_collabs)
  names(num_collabs_year_genre_grouped)[names(num_collabs_year_genre_grouped) == 'name.y'] <- 'collaborator'
  names(num_collabs_year_genre_grouped)[names(num_collabs_year_genre_grouped) == 'genres.y'] <- 'genres'
  
  num_grouped <- ggplot(num_collabs_year_genre_grouped, aes_string(fill="same_genre", x="year", weight="num_collabs"))+
    geom_bar(color = "white", position = "dodge")+
    scale_fill_manual(values = c("plum1","royalblue1"), name = "Genre of\ncollaborator")+
    labs(title = "Number of Same-/Cross-Genre Collabs (Grouped)")+
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5))+
    scale_y_continuous(breaks=seq(0, NROW(num_collabs_year_genre), 10))
  num_grouped <- ggplotly(num_grouped, tooltip = c("count"))
  
  # Percentage
  
  perc <- ggplot(num_collabs_year_genre, aes_string(fill="same_genre", x="year", weight="num_collabs"))+
    geom_bar(position = "fill")+
    labs(title = "Percentage of Same-/Cross-Genre Collabs")+
    scale_fill_manual(values = c("plum1", "royalblue1"), name = "Genre of\ncollaborator")+
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5))+
    scale_y_continuous(breaks=seq(0, 1, .1))
  perc <- ggplotly(perc, tooltip = c("count"))
  
  if (num_percent_choice_genre() == "count_stacked") {
    num %>% layout(font=t)
  }
  else if (num_percent_choice_genre() == "count_grouped") {
    num_grouped %>% layout(font=t)
  }
   else {
    perc %>% layout(font=t)
  }
  
  })
  
  #__________# Genres of Collaborators
  
  genre_collaborator_include_remix <- reactive(input$genre_collaborator_remix_check)
  
  
  output$totalnumgenres <- renderText(paste("J Balvin has collaborated with artists of ", length(unique(num_collabs_genre$genre)), " different genres (all releases).", sep=""))
  output$totalnumgenresnoremix <- renderText(paste("J Balvin has collaborated with artists of ", length(unique((num_collabs_genre %>% filter(album_type != "remix"))$genre)), " different genres (excluding remixes).", sep = ""))
  

  output$genres_of_collabors_plot <- renderPlotly({
    
    if (genre_collaborator_include_remix() == FALSE){
      # Don't include remixes
      num_collabs_genre_no_remix <- num_collabs_genre %>% filter(album_type != "remix") %>% arrange(collaborator)
      split_df <- split(num_collabs_genre_no_remix, f = num_collabs_genre_no_remix$collaborator)
      artist_genres <- map_df(split_df, flatten_collab_info) %>% 
        group_by(genre, collaborator, album_types, tracks) %>%
        dplyr::summarize(artist_val = n())
      
      b <- ggplot(artist_genres, aes_string(fill="factor(album_types, c('single','album & single','album'))", x="reorder(genre, artist_val, sum)", y="artist_val", collaborator = "collaborator", tracks = "tracks"))+
        geom_bar(stat = "identity", width = .6, color = "white")+
        labs(title = "",
             y = "Number of Collaborators Who Have This Genre Tag",
             x = "Genre tag of collaborator")+
        theme_minimal()+
        scale_y_continuous(breaks=seq(0, (NROW(unique(artist_genres$genre))),5))+
        coord_flip()+
        scale_fill_manual(values=c("#619CFF", "#C77CFF", "#F8766D"), name = "Type\nof Release(s)")
      ggplotly(b, tooltip = c("collaborator", "tracks")) %>% layout(font=t)
    }
    else {
  
  split_df <- split(num_collabs_genre, f = num_collabs_genre$collaborator)
  artist_genres <- map_df(split_df, flatten_collab_info) %>% 
    group_by(genre, collaborator, album_types, tracks) %>%
    dplyr::summarize(artist_val = n())
  
  b <- ggplot(artist_genres, aes_string(fill = "genre", x="reorder(genre, artist_val, sum)", weight="artist_val", collaborator = "collaborator", album_types = "album_types", tracks = "tracks"))+
    geom_bar(width=0.6, color = "white")+
    labs(title = "",
         y = "Number of Collaborators Who Have This Genre Tag",
         x = "Genre of collaborator")+
    theme_minimal()+
    scale_y_continuous(breaks=seq(0, (NROW(unique(artist_genres$genre))),5))+
    coord_flip()
  ggplotly(b, tooltip = c("collaborator", "album_types", "tracks")) %>% layout(showlegend = FALSE) %>% layout(font=t)
    }
  
  })
  
  #________# Number of Genres Collaborated with by Year
  
  types_chosen_genre_col_yr <- reactive(input$type_choice_genre_col_yr)
  
  output$genres_of_collabors_by_year_plot <- renderPlotly({
    
    if (is.null(types_chosen_genre_col_yr())){
      return(NULL)
    }
  
  all_genres_yrs <- all_genres %>% filter(album_type %in% types_chosen_genre_col_yr())
  drop_cols <- c("album_type","track_name")
  all_genres_yrs <- unique(all_genres_yrs[,!(names(all_genres_yrs) %in% drop_cols)])
  
  split_df_genre <- split(all_genres_yrs, f = all_genres_yrs$genre)
  genre_yr_artists <- map_df(split_df_genre, flatten_artists)
  genre_yr_artists$weight <- 1
  
  b <- ggplot(genre_yr_artists, aes_string(fill="reorder(genre, num_artists, sum)", x="year", y="weight", genre = "genre", artists = "artists", num_artists = "num_artists"))+
    geom_bar(position = "stack", stat = "identity"
      ,color="white"
      )+
    labs(title = "",
         y = "Number of Genres Collaborated With that Year",
         x = "Year")+
    theme_minimal()+
    scale_y_continuous(breaks=seq(0, (num_collabs_person$collaborator %>% unique() %>% length()), 5))
  ggplotly(b, tooltip = c("genre", "artists", "num_artists")) %>% layout(showlegend = FALSE) %>% layout(font=t)
  
  })
  
  # Genres of Collaborators by Year
  
  output$num_genres_of_collabors_by_year_plot <- renderPlotly({
    
    if (is.null(types_chosen_genre_col_yr())){
      return(NULL)
    }
    
    all_genres_yrs <- all_genres %>% filter(album_type %in% types_chosen_genre_col_yr())
    drop_cols <- c("album_type","track_name")
    all_genres_yrs <- unique(all_genres_yrs[,!(names(all_genres_yrs) %in% drop_cols)])
    
    split_df_genre <- split(all_genres_yrs, f = all_genres_yrs$genre)
    genre_yr_artists <- map_df(split_df_genre, flatten_artists)
    
    b <- ggplot(genre_yr_artists, aes_string(fill="reorder(genre, num_artists, sum)", x="year", y="num_artists", genre = "genre", artists = "artists", num_artists = "num_artists"))+
      geom_bar(position = "dodge", stat = "identity"
               #,color="white"
      )+
      labs(title = "",
           y = "Number of Collaborators With Genre Tag",
           x = "Year")+
      theme_minimal()+
      scale_y_continuous(breaks=seq(0, (num_collabs_person$collaborator %>% unique() %>% length()), 5))
    ggplotly(b, tooltip = c("genre", "artists")) %>% layout(font=t)
    #%>% layout(showlegend = FALSE) 
    
  })
  
  #NETWORK_____________________________________________________________________________________________________
  
  output$clusterText <- renderText(networkClusterText1)
  output$clusterText2 <- renderText(networkClusterText2)
  
  highlight_color = "skyblue"

  output$artist_vertices_DT <- DT::renderDataTable(artist_vertices_DT)
  
  index <- which(artist_vertices$name == target_artist_name)
  jb_genres <- artist_vertices[index,]$genres
  jb_genres <-get_genres(jb_genres)
  
  artist_vertices_color <- artist_vertices
  chart_colors <-c("#C0C0C0", "#0096FF") # gray, blue
  
  # Need to create column that is TRUE if any of jb's genres and false if not.
  # Create a vector of color FIX THIS!!!
  reg_color <- chart_colors[as.numeric(as.factor(any(jb_genres %in% artist_vertices_color$genres)))]
  
  #Color all dots gray or light blue
  artist_vertices_color$color <- reg_color
  
  #Color J Balvin red
  index <- which(artist_vertices_color$name == target_artist_name)
  artist_vertices_color[index,]$color = "#FF0000"

  # Popularity

  #_______Everyone
  
  pop <- artist_vertices %>% arrange(-popularity)
  pop_index <- which(pop$name == target_artist_name)
  pop2 <- pop[,(names(pop) %in% c("name","popularity", "genres"))]
  
  output$popularity_text_all <- renderText(paste(target_artist_name,
  " has a popularity score of ", pop[pop_index,]$popularity,
  ", making him the ", toOrdinal(pop_index),
  " most popular artist in the network of ", NROW(pop)," artists.", sep = ""))

  output$popularity_DT <- DT::renderDataTable({
    datatable(
      pop2, caption = "Billboard artists and collaborators ordered by popularity", 
      options = list(
        dom = 'frtip',
        pageLength = pop_index,
        scrollX = TRUE,
        buttons = c()
      )
    ) %>% formatStyle(
      "name",
      target = 'row',
      backgroundColor = styleEqual(target_artist_name, highlight_color)
    )
  })
  
  
  #______Within genre
  
  output$pop_genre_checkboxes <- renderUI({
    checkboxGroupInput("pop_genre_checkboxes", "Genres", choices = jb_genres, selected = jb_genres)
  })
  
  selected_genres <- reactive(input$pop_genre_checkboxes)
  and_or_choice <- reactive(input$pop_and_or)
  
  pop_within_genres <- reactive(network_genre_selector(artist_vertices, selected_genres(), and_or_choice()))
  
  
  output$pop_genre_DT <- renderDataTable({
    
    if (is.null(selected_genres())){
      return(NULL)
    }
    
    within_genres <- pop_within_genres()[,(names(pop_within_genres()) %in% c("name","popularity", "genres"))] %>% arrange(-popularity)
    rowindex <- which(within_genres$name == target_artist_name)
    datatable(
      within_genres,
      caption = "Billboard artists and collaborators of selected genres ordered by popularity",
      options = list(
        dom = 'frtip',
        pageLength = 10,
        scrollX = TRUE,
        buttons = c()
      )
    ) %>% formatStyle(
      "name",
      target = 'row',
      backgroundColor = styleEqual(target_artist_name, highlight_color)
    )
  })
  
  output$popularity_text_genre <- renderText({
    if (is.null(selected_genres())){
      return("")
    }
    
    within_genres <- pop_within_genres() %>% arrange(-popularity)
    rank <- which(within_genres$name == target_artist_name)
    
    ranking_string <- ifelse(rank == 1, "", toOrdinal(rank))
    
    return(paste(target_artist_name,
                 " has the ", ranking_string,
                 " highest popularity score of the ",
                 NROW(within_genres),
                 " artists of the selected genres.",
                 sep = ""))
  })

  
  
  # Degree Centrality
  
  #___Everyone
  
  deg <- artist_vertices_color %>% filter(type == "billboard") %>% arrange(-degree)
  deg_index <- which(deg$name == target_artist_name)
  deg2 <- deg[,(names(deg) %in% c("name","popularity", "degree", "genres"))]
  col_order <- c("name", "degree", "genres", "popularity")
  deg2 <- deg2[, col_order]
  
  output$degree_text_all <- renderText(paste(target_artist_name,
                                             " (", toOrdinal(deg_index),
                                             ") has a degree centrality of ",
                                             deg[deg_index,]$degree,
                                             ", which means he has collaborated with more unique artists than ",
                                             round((NROW(artist_df)-deg_index)/NROW(artist_df)*100,2),
                                             "% of all of the ",
                                             NROW(deg),
                                             " artists who have been on Billboard in the past decade.",
                                             sep=""))
  
  
  output$degree_DT <- DT::renderDataTable({
    datatable(
      deg2, caption = "Billboard artists ordered by degree centrality", 
      options = list(
        dom = 'frtip',
        pageLength = deg_index,
        scrollX = TRUE,
        buttons = c()
      )
    ) %>% formatStyle(
      "name",
      target = 'row',
      backgroundColor = styleEqual(target_artist_name, highlight_color)
    )
  })
  
  output$degree_plot <- renderPlotly({
    fig <- plot_ly(deg, x = ~degree, y = ~popularity, text = deg$name)
    fig <- fig %>% add_markers(
      marker = list(color = deg$color)
      )
    fig %>% layout(font=t)
  })
  
  #___Within genre
  
  output$deg_genre_checkboxes <- renderUI({
    checkboxGroupInput("deg_genre_checkboxes", "Genres", choices = jb_genres, selected = jb_genres)
  })
  
  deg_selected_genres <- reactive(input$deg_genre_checkboxes)
  deg_and_or_choice <- reactive(input$deg_and_or)
  
  deg_within_genres <- reactive(network_genre_selector(artist_vertices_color %>% filter(type == "billboard"),
                                                       deg_selected_genres(),
                                                       deg_and_or_choice()))
  
  
  output$deg_genre_DT <- renderDataTable({
    
    if (is.null(deg_selected_genres())){
      return(NULL)
    }
    
    within_genres <- deg_within_genres()[,(names(deg_within_genres()) %in% c("name","popularity", "degree", "genres"))] %>% arrange(-degree)
    col_order <- c("name", "degree", "genres", "popularity")
    within_genres <- within_genres[, col_order]
    
    deg_rowindex <- which(within_genres$name == target_artist_name)
    datatable(
      within_genres,
      caption = "Billboard artists of selected genres ordered by degree centrality",
      options = list(
        dom = 'frtip',
        pageLength = 10,
        scrollX = TRUE,
        buttons = c()
      )
    ) %>% formatStyle(
      "name",
      target = 'row',
      backgroundColor = styleEqual(target_artist_name, highlight_color)
    )
  })
  
  output$degree_plot_genre <- renderPlotly({
    
    if (is.null(deg_selected_genres())){
      return(NULL)
    }

     fig <- plot_ly(deg_within_genres(), x = ~degree, y = ~popularity, text = deg_within_genres()$name)
     fig <- fig %>% add_markers(
       marker = list(color = deg_within_genres()$color)
     )
     fig %>% layout(font=t)
   })
  
  output$degree_text_genre <- renderText({
    if (is.null(deg_selected_genres())){
      return("")
    }
    
    within_genres <- deg_within_genres() %>% arrange(-degree)
    deg_rank <- which(within_genres$name == target_artist_name)
    
    ranking_string <- ifelse(deg_rank == 1, "", toOrdinal(deg_rank))
    
    return(paste(target_artist_name,
                 " has the ", ranking_string,
                 " highest degree centrality of the ",
                 NROW(within_genres),
                 " Billboard artists of the selected genres.",
                 sep = ""))
    })
  
  
  
  # Eigenvector Centrality
  
  #___Everyone
  
  eig <- artist_vertices_color %>% filter(type == "billboard") %>% arrange(-eigenvector)
  eig_index <- which(eig$name == target_artist_name)
  eig2 <- eig[,(names(eig) %in% c("name","popularity", "eigenvector", "genres"))]
  col_order <- c("name", "eigenvector", "genres", "popularity")
  eig2 <- eig2[, col_order]
  
  
  output$eigenvector_text_all <- renderText(paste(target_artist_name,
                                                  " (", toOrdinal(eig_index),
                                                  ") has an eigenvector value of ",
                                                  format(round(eig[eig_index,]$eigenvector,2), scientific=F),
                                                  ", which is greater than ",
                                                  round((NROW(artist_df)-eig_index)/NROW(artist_df)*100,2),
                                                  "% of the ",
                                                  NROW(eig),
                                                  " artists who have been on Billboard in the past decade.
                                                  This means that he is an influential collaborator who collaborates
                                                  with other influential collaborators.", sep=""))
  
  output$eigenvector_DT <- DT::renderDataTable({
    datatable(
      eig2, caption = "Billboard artists ordered by eigenvector centrality", 
      options = list(
        dom = 'frtip',
        pageLength = eig_index,
        scrollX = TRUE,
        buttons = c()
      )
    ) %>% formatStyle(
      "name",
      target = 'row',
      backgroundColor = styleEqual(target_artist_name, highlight_color)
    )
  })
  
  output$eigenvector_plot <- renderPlotly({
    fig <- plot_ly(eig, x = ~eigenvector, y = ~popularity, text = eig$name)
    fig <- fig %>% add_markers(
      marker = list(color = eig$color)
    )
    fig %>% layout(font=t)
  })
  
  #___Within genre
  
  output$eig_genre_checkboxes <- renderUI({
    checkboxGroupInput("eig_genre_checkboxes", "Genres", choices = jb_genres, selected = jb_genres)
  })
  
  eig_selected_genres <- reactive(input$eig_genre_checkboxes)
  eig_and_or_choice <- reactive(input$eig_and_or)
  
  eig_within_genres <- reactive(network_genre_selector(artist_vertices_color %>% filter(type == "billboard"), eig_selected_genres(), eig_and_or_choice()))
  
  output$eig_genre_DT <- renderDataTable({
    
    if (is.null(eig_selected_genres())){
      return(NULL)
    }
    
    within_genres <- eig_within_genres()[,(names(eig_within_genres()) %in% c("name","popularity", "eigenvector", "genres"))] %>% arrange(-eigenvector)
    col_order <- c("name", "eigenvector", "genres", "popularity")
    within_genres <- within_genres[, col_order]
    eig_rowindex <- which(within_genres$name == target_artist_name)
    datatable(
      within_genres,
      caption = "Billboard artists of selected genres ordered by eigenvector centrality",
      options = list(
        dom = 'frtip',
        pageLength = 10,
        scrollX = TRUE,
        buttons = c()
      )
    ) %>% formatStyle(
      "name",
      target = 'row',
      backgroundColor = styleEqual(target_artist_name, highlight_color)
    )
  })
  
  output$eigenvector_plot_genre <- renderPlotly({
    
    if (is.null(eig_selected_genres())){
      return(NULL)
    }
    
    fig <- plot_ly(eig_within_genres(), x = ~eigenvector, y = ~popularity, text = eig_within_genres()$name)
    fig <- fig %>% add_markers(
      marker = list(color = eig_within_genres()$color)
    )
    fig %>% layout(font=t)
  })
  
  output$eigenvector_text_genre <- renderText({
    if (is.null(eig_selected_genres())){
      return("")
    }
    
    within_genres <- eig_within_genres() %>% arrange(-eigenvector)
    rank <- which(within_genres$name == target_artist_name)
    
    ranking_string <- ifelse(rank == 1, "", toOrdinal(rank))
    
    return(paste(target_artist_name,
                 " has the ", ranking_string,
                 " highest eigenvector centrality of the ",
                 NROW(within_genres),
                 " Billboard artists in the selected genres.",
                 sep = ""))
  })
  
  # Betweenness Centrality
  
  #___Everyone
  
  bet <- artist_vertices_color %>% filter(type == "billboard") %>% arrange(-betweenness)
  bet_index <- which(bet$name == target_artist_name)
  bet2 <- bet[,(names(bet) %in% c("name","popularity", "betweenness", "genres"))]
  col_order <- c("name", "betweenness", "genres", "popularity")
  bet2 <- bet2[, col_order]
  
  output$betweenness_text_all <- renderText(paste(target_artist_name,
                                             " (", toOrdinal(bet_index),
                                             ") has a betweenness centrality of ",
                                             round(bet[bet_index,]$betweenness),
                                             ", a higher value than ",
                                             round((NROW(artist_df)-bet_index)/NROW(artist_df)*100,2),
                                             "% of the ",
                                             NROW(bet),
                                             " artists who have been on Billboard in the past decade.",
                                             sep=""))
  
  output$betweenness_DT <- DT::renderDataTable({
    datatable(
      bet2, caption = "Billboard artists ordered by betweenness centrality", 
      options = list(
        dom = 'frtip',
        pageLength = bet_index,
        scrollX = TRUE,
        buttons = c()
      )
    ) %>% formatStyle(
      "name",
      target = 'row',
      backgroundColor = styleEqual(target_artist_name, highlight_color)
    )
  })
  
  output$betweenness_plot <- renderPlotly({
    fig <- plot_ly(bet, x = ~betweenness, y = ~popularity, text = bet$name)
    fig <- fig %>% add_markers(
      marker = list(color = bet$color)
    )
    fig %>% layout(font=t)
  })
  
  #___Within genre
  
  output$bet_genre_checkboxes <- renderUI({
    checkboxGroupInput("bet_genre_checkboxes", "Genres", choices = jb_genres, selected = jb_genres)
  })
  
  bet_selected_genres <- reactive(input$bet_genre_checkboxes)
  bet_and_or_choice <- reactive(input$bet_and_or)
  
  bet_within_genres <- reactive(network_genre_selector(artist_vertices_color %>% filter(type == "billboard"), bet_selected_genres(), bet_and_or_choice()))
  
  output$bet_genre_DT <- renderDataTable({
    
    if (is.null(bet_selected_genres())){
      return(NULL)
    }
    
    within_genres <- bet_within_genres()[,(names(bet_within_genres()) %in% c("name","popularity", "betweenness", "genres"))] %>% arrange(-betweenness)
    col_order <- c("name", "betweenness", "genres", "popularity")
    within_genres <- within_genres[, col_order]
    bet_rowindex <- which(within_genres$name == target_artist_name)
    datatable(
      within_genres,
      caption = "Billboard artists of selected genres ordered by betweenness centrality",
      options = list(
        dom = 'frtip',
        pageLength = 10,
        scrollX = TRUE,
        buttons = c()
      )
    ) %>% formatStyle(
      "name",
      target = 'row',
      backgroundColor = styleEqual(target_artist_name, highlight_color)
    )
  })
  
  output$betweenness_plot_genre <- renderPlotly({
    
    if (is.null(bet_selected_genres())){
      return(NULL)
    }
    
    fig <- plot_ly(bet_within_genres(), x = ~betweenness, y = ~popularity, text = bet_within_genres()$name)
    fig <- fig %>% add_markers(
      marker = list(color = bet_within_genres()$color)
    )
    fig %>% layout(font=t)
  })
  
  output$betweenness_text_genre <- renderText({
    if (is.null(bet_selected_genres())){
      return("")
    }
    
    within_genres <- bet_within_genres() %>% arrange(-betweenness)
    rank <- which(within_genres$name == target_artist_name)
    
    ranking_string <- ifelse(rank == 1, "", toOrdinal(rank))
    
    return(paste(target_artist_name,
                 " has the ", ranking_string,
                 " highest betweenness centrality of the ",
                 NROW(within_genres),
                 " Billboard artists in the selected genres.",
                 sep = ""))
  })
  
  # Genre Homophily
  
  # Followers
  
  #_____Everyone
  
  fol <- artist_vertices_color %>% arrange(-followers.total)
  folindex <- which(fol$name == target_artist_name)
  fol2 <- fol[,(names(fol) %in% c("name","popularity", "followers.total", "genres"))]
  col_order <- c("name", "followers.total", "genres", "popularity")
  fol2 <- fol2[, col_order]
  
  output$followers_text_all <- renderText(paste(target_artist_name,
                                                " (", toOrdinal(folindex), ") has ",
                                                format(fol[folindex,]$followers.total, scientific=F),
                                                " followers on Spotify — more followers than ",
                                                round((NROW(artist_df)-folindex)/NROW(artist_df)*100,2),
                                                "% of the ",
                                                NROW(fol),
                                                " Billboard artists.", sep=""))
  
  output$followers_DT <- DT::renderDataTable({
    datatable(
      fol2, caption = "Billboard artists and collaborators ordered by number of Spotify followers", 
      options = list(
        dom = 'frtip',
        pageLength = folindex,
        scrollX = TRUE,
        buttons = c()
      )
    ) %>% formatStyle(
      "name",
      target = 'row',
      backgroundColor = styleEqual(target_artist_name, highlight_color)
    )
  })
  
  output$followers_plot <- renderPlotly({
    fig <- plot_ly(fol, x = ~followers.total, y = ~popularity, text = fol$name)
    fig <- fig %>% add_markers(
      marker = list(color = fol$color)
    )
    fig %>% layout(font=t)
  })
  
  #_____Within genre
  
  output$fol_genre_checkboxes <- renderUI({
    checkboxGroupInput("fol_genre_checkboxes", "Genres", choices = jb_genres, selected = jb_genres)
  })
  
  fol_selected_genres <- reactive(input$fol_genre_checkboxes)
  fol_and_or_choice <- reactive(input$fol_and_or)
  
  fol_within_genres <- reactive(network_genre_selector(artist_vertices_color, fol_selected_genres(), fol_and_or_choice()))
  
  output$fol_genre_DT <- renderDataTable({
    
    if (is.null(fol_selected_genres())){
      return(NULL)
    }
    
    within_genres <- fol_within_genres()[,(names(fol_within_genres()) %in% c("name","popularity", "followers.total", "genres"))] %>% arrange(-followers.total)
    col_order <- c("name", "followers.total", "genres", "popularity")
    within_genres <- within_genres[, col_order]
    rowindex <- which(within_genres$name == target_artist_name)
    datatable(
      within_genres,
      caption = "Billboard artists and collaborators of selected genres ordered by number of Spotify followers",
      options = list(
        dom = 'frtip',
        pageLength = 10,
        scrollX = TRUE,
        buttons = c()
      )
    ) %>% formatStyle(
      "name",
      target = 'row',
      backgroundColor = styleEqual(target_artist_name, highlight_color)
    )
  })
  
  output$followers_plot_genre <- renderPlotly({
    
    if (is.null(fol_selected_genres())){
      return(NULL)
    }
    
    fig <- plot_ly(fol_within_genres(), x = ~followers.total, y = ~popularity, text = fol_within_genres()$name)
    fig <- fig %>% add_markers(
      marker = list(color = fol_within_genres()$color)
    )
    fig %>% layout(font=t)
  })
  
  output$followers_text_genre <- renderText({
    if (is.null(fol_selected_genres())){
      return("")
    }
    
    within_genres <- fol_within_genres() %>% arrange(-followers.total)
    rank <- which(within_genres$name == target_artist_name)
    
    ranking_string <- ifelse(rank == 1, "", toOrdinal(rank))
    
    return(paste(target_artist_name,
                 " has the ", ranking_string,
                 " highest number of Spotify followers of the ",
                 NROW(within_genres),
                 " artists in the selected genres.",
                 sep = ""))
  })
  
  # Number of Songs
  
  #Everyone
  
  num <- artist_vertices_color %>% filter(type == "billboard") %>% arrange(-total_num_songs)
  numindex <- which(num$name == target_artist_name)
  num2 <- num[,(names(num) %in% c("name","popularity", "total_num_songs", "genres"))][1:500,]
  col_order <- c("name", "total_num_songs", "genres", "popularity")
  num2 <- num2[, col_order]
  
  output$num_songs_text_all <- renderText(paste(target_artist_name,
                                                " (", toOrdinal(numindex), ") has released ",
                                                format(num[numindex,]$total_num_songs, scientific=F),
                                                " unique songs on Spotify — more songs than ",
                                                round((NROW(artist_df)-numindex)/NROW(artist_df)*100,2),
                                                "% of the ",
                                                NROW(num),
                                                " Billboard artists.", sep= ""))
  
  output$num_songs_DT <- DT::renderDataTable({
    datatable(
      num2, caption = "Billboard artists ordered by number of released songs", 
      options = list(
        dom = 'frtip',
        pageLength = numindex,
        scrollX = TRUE,
        buttons = c()
      )
    ) %>% formatStyle(
      "name",
      target = 'row',
      backgroundColor = styleEqual(target_artist_name, highlight_color)
    )
  })
  
  output$num_songs_plot <- renderPlotly({
    fig <- plot_ly(num, x = ~total_num_songs, y = ~popularity, text = num$name)
    fig <- fig %>% add_markers(
      marker = list(color = num$color)
    )
    fig %>% layout(font=t)
  })
  
  #Within genre
  
  output$num_genre_checkboxes <- renderUI({
    checkboxGroupInput("num_genre_checkboxes", "Genres", choices = jb_genres, selected = jb_genres)
  })
  
  num_selected_genres <- reactive(input$num_genre_checkboxes)
  num_and_or_choice <- reactive(input$num_and_or)
  
  num_within_genres <- reactive(network_genre_selector(artist_vertices_color %>% filter(type == "billboard"), num_selected_genres(), num_and_or_choice()))
  
  output$num_genre_DT <- renderDataTable({
    
    if (is.null(num_selected_genres())){
      return(NULL)
    }
    
    within_genres <- num_within_genres()[,(names(num_within_genres()) %in% c("name","popularity", "total_num_songs", "genres"))] %>% arrange(-total_num_songs)
    col_order <- c("name", "total_num_songs", "genres", "popularity")
    within_genres <- within_genres[, col_order]
    rowindex <- which(within_genres$name == target_artist_name)
    datatable(
      within_genres,
      caption = "Billboard artists of selected genres ordered by number of released songs",
      options = list(
        dom = 'frtip',
        pageLength = 10,
        scrollX = TRUE,
        buttons = c('csv')
      )
    ) %>% formatStyle(
      "name",
      target = 'row',
      backgroundColor = styleEqual(target_artist_name, highlight_color)
    )
  })
  
  output$num_songs_plot_genre <- renderPlotly({
    
    if (is.null(num_selected_genres())){
      return(NULL)
    }
    
    fig <- plot_ly(num_within_genres(), x = ~total_num_songs, y = ~popularity, text = num_within_genres()$name)
    fig <- fig %>% add_markers(
      marker = list(color = num_within_genres()$color)
    )
    fig %>% layout(font=t)
  })
  
  output$num_songs_text_genre <- renderText({
    if (is.null(num_selected_genres())){
      return("")
    }
    
    within_genres <- num_within_genres() %>% arrange(-total_num_songs)
    rank <- which(within_genres$name == target_artist_name)
    
    ranking_string <- ifelse(rank == 1, "", toOrdinal(rank))
    
    return(paste(target_artist_name,
                 " has released the ", ranking_string,
                 " highest number of songs out of the ",
                 NROW(within_genres),
                 " Billboard artists in the selected genres.",
                 sep = ""))
  })
  

})
