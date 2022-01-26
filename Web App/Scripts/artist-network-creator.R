library(tidyverse)
library(stringr)
library(tm)
library(lubridate)
library(dplyr)
library(purrr)
library(plyr)
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

contains_certain_genre <- function(row, specific_genres){
  genre_string <- unlist(row)[5]
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

#________________ARTIST VERTICES________________

# Read in collaborations, artist_df, and new_artists.
setwd("/Users/lindsaymaggioncalda/Documents/J Balvin Project/j-balvin-spotifyr/Web App/collaboration-info")
collaboration_info_file <- 
  list.files("/Users/lindsaymaggioncalda/Documents/J Balvin Project/Web App/collaboration-info") %>% tail(n = 1)
collaboration_info <- read_delim(collaboration_info_file, delim=',', escape_double=FALSE, escape_backslash=TRUE)
collaboration_info <- collaboration_info[,!(names(collaboration_info) %in% c("X"))]
setwd("/Users/lindsaymaggioncalda/Documents/J Balvin Project/Web App/artist-df")
artist_df_file <- 
  list.files("/Users/lindsaymaggioncalda/Documents/J Balvin Project/j-balvin-spotifyr/Web App/artist-df") %>% tail(n = 1)
artist_df <- read.csv(artist_df_file)
artist_df <- artist_df[,!(names(artist_df) %in% c("X"))]
setwd("/Users/lindsaymaggioncalda/Documents/J Balvin Project/j-balvin-spotifyr/Web App/new-artists-df")
new_artists_file <- 
  list.files("/Users/lindsaymaggioncalda/Documents/J Balvin Project/j-balvin-spotifyr/Web App/new-artists-df") %>% tail(n = 1)
new_artists <- read.csv(new_artists_file)
new_artists <- new_artists[,!(names(new_artists) %in% c("X"))]

# Get edge ids.
collaborations <- filter(collaboration_info, id2 != "")
edge_ids <- data.frame(source = collaborations$id1, target = collaborations$id2) %>% unique()


# Get vertex information.
artist_vertices <- rbind(artist_df, new_artists) %>% arrange(-followers.total) %>% unique() %>% distinct(id, .keep_all=TRUE)

# TEST
artist_vertices <- artist_vertices[order(nrow(artist_vertices):1),]

# Collect genre data, add to vertices.
hip_hop_rap_rbnb <- apply(artist_vertices, 1, contains_certain_genre, c("hip hop", "rap", "brooklyn drill"))
rock <- apply(artist_vertices, 1, contains_certain_genre, c("rock", "metal", "punk"))
country <- apply(artist_vertices, 1, contains_certain_genre, c("country"))
pop <- apply(artist_vertices, 1, contains_certain_genre, c("pop"))
latin <- apply(artist_vertices, 1, contains_certain_genre, c("latin", "colombian", "mexica", "reggaeton", "mariachi", "samba", "urbano"))
edm <- apply(artist_vertices, 1, contains_certain_genre, c("techno", "edm", "house", "dubstep", "trance", "electronica", "chillwave", "rave"))
soul_jazz <- apply(artist_vertices, 1, contains_certain_genre, c("jazz", "soul", "funk", "r&b", "blues"))

artist_vertices <- artist_vertices %>% mutate(main_genre = case_when(
  edm ~ 'edm, techno, house',
  country ~ 'country',
  latin ~ 'latin',
  rock ~ 'rock',
  hip_hop_rap_rbnb ~ 'hip hop, rap',
  soul_jazz ~ 'jazz, soul, r&b',
  pop ~ 'pop',
  TRUE ~ 'other/unknown'))

# Make network.
artist_network <- graph_from_data_frame(d=unique(edge_ids), vertices = unique(artist_vertices), directed = FALSE)

# Color certain vertices.
main_genre_palette  <- c('#f58231', '#ffe119', '#3cb44b', '#dcbeff', '#4363d8', '#ffd8b1', '#aaffc3', "#f032e6")
main_genre_color <- main_genre_palette[as.numeric(as.factor(V(artist_network)$main_genre))]

# Generate network analysis figures.
artist_vertices$degree <- degree(artist_network)
artist_vertices$betweenness <- betweenness(artist_network)
artist_vertices$eigenvector <- evcent(artist_network)$vector

# Write artist_vertices file.
setwd("/Users/lindsaymaggioncalda/Documents/J Balvin Project/j-balvin-spotifyr/Web App/artist-vertices")
artist_vertices_date <- str_split(artist_df_file, "_")[[1]][2]
artist_vertices_file_name <- paste("artist-vertices", artist_vertices_date, ".csv", sep = "_")
#write.csv(artist_vertices, artist_vertices_file_name)


# Generate images.
#________PLAIN
seedy <- 26
set.seed(seedy)
setwd("/Users/lindsaymaggioncalda/Documents/J Balvin Project/j-balvin-spotifyr/Web App/www/plain-network")
plain_file_name <- paste("artist-network", artist_vertices_date, ".jpg", sep = "_")
jpeg(file = plain_file_name, width = 2000, height = 2000)
plot.igraph(artist_network, vertex.size = 1, edge.color = 'black', vertex.color = 'black', vertex.label="", vertex.frame.color = NA, edge.width = .075)
title("Collaborations of the Past Decade's Billboard Weekly Artist 100", cex.main = 4, col.main = "black")
dev.off()

#________COLORED BY MAIN GENRE - popularity
set.seed(seedy) 
my_legend <- levels(as.factor(V(artist_network)$main_genre))
V(artist_network)$color <- main_genre_color

V(artist_network)$size <- ((V(artist_network)$popularity) **2)/5000
setwd("/Users/lindsaymaggioncalda/Documents/J Balvin Project/j-balvin-spotifyr/Web App/www/color-network")
color_file_name <- paste("artist-network-color", artist_vertices_date, "popularity", ".jpg", sep = "_")
jpeg(file = paste(color_file_name), width = 2000, height = 2000)
plot.igraph(artist_network, 
            edge.color = 'black',
            vertex.label="", vertex.label.cex =50, vertex.frame.color = NA, edge.width = .045)
title("Collaborations of the Past Decade's Billboard Weekly Artist 100 - Sized by Popularity", cex.main = 4, col.main = "black")

# Add legend
legend("bottomleft", legend=my_legend, col = main_genre_palette, bty = "n", pch=20 , pt.cex = 2.5, cex = 3, text.col=main_genre_palette, horiz = FALSE, inset = c(0.05, 0.05))
dev.off()

#________COLORED BY MAIN GENRE - degree

set.seed(seedy)
my_legend <- levels(as.factor(V(artist_network)$main_genre))
V(artist_network)$color <- main_genre_color
V(artist_network)$size <- ((artist_vertices$degree)+50)/80
setwd("/Users/lindsaymaggioncalda/Documents/J Balvin Project/j-balvin-spotifyr/Web App/www/color-network")
color_file_name <- paste("artist-network-color", artist_vertices_date, "degree", ".jpg", sep = "_")
jpeg(file = paste(color_file_name), width = 2000, height = 2000)
plot.igraph(artist_network, 
            edge.color = 'black',
            vertex.label="", vertex.label.cex =50, vertex.frame.color = NA, edge.width = .045)
title("Collaborations of the Past Decade's Billboard Weekly Artist 100 - Sized by Degree", cex.main = 4, col.main = "black")

# Add legend
legend("bottomleft", legend=my_legend, col = main_genre_palette, bty = "n", pch=20 , pt.cex = 2.5, cex = 3, text.col=main_genre_palette, horiz = FALSE, inset = c(0.025, 0.025))
dev.off()


#________HIGHLIGHT J BALVIN
set.seed(seedy)
color_file_name <- paste("artist-network-color", artist_vertices_date, "balvin", ".jpg", sep = "_")
jpeg(file = paste(color_file_name), width = 2000, height = 2000)
V(artist_network)$color <- ifelse(V(artist_network)$name == "J Balvin", "red", "gray") ## if artist, make red, if not, gray.
plot(artist_network, edge.width = .045, edge.color = 'black', vertex.frame.color = NA, vertex.label = "")
dev.off()