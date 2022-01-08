

#____________________________________________________
#________________HELPER FUNCTIONS____________________
#____________________________________________________

# Gets information (ID, genres, name, popularity, number of followers) for the artists passed in.
get_new_artists <- function(artists){
  new_artists = NULL
  count <- 1
  while (count < length(artists)) {
    fifty_new <- get_artists(artists[count:(count+49)])
    new_artists = rbind(new_artists, fifty_new)
    count <- count + 50
  }
  new_artists <- new_artists[!is.na(new_artists$id), ]
  new_artists$genres <- sapply(new_artists$genres, genre_helper)
  keep_cols <- c("id", "genres", "name", "popularity", "followers.total")
  new_artists <- new_artists[,(names(new_artists) %in% keep_cols)]
  return(new_artists[, c(2, 3, 4, 5, 1)])
}

# Changes the genres column from a list to a string separated by semi-colons.
genre_helper <- function(genre){
  if (length(genre) == 0) {
    return(NA)
  }
  else {
    return(paste(genre, collapse=";"))
  }
}

#____________________________________________________
#_____________________MAIN FILE______________________
#____________________________________________________

# Read in collaborations and artist_df.
setwd("/Users/lindsaymaggioncalda/Documents/J Balvin Project/Web App/collaboration-info")
collaboration_info_file <- list.files("/Users/lindsaymaggioncalda/Documents/J Balvin Project/Web App/collaboration-info") %>% tail(n = 1)
collaboration_info <- read.csv(collaboration_info_file)
collaboration_info <- collaboration_info[,!(names(collaboration_info) %in% c("X", "total_songs"))]

setwd("/Users/lindsaymaggioncalda/Documents/J Balvin Project/Web App/artist-df")
artist_df_file <- list.files("/Users/lindsaymaggioncalda/Documents/J Balvin Project/Web App/artist-df") %>% tail(n = 1)
artist_df <- read.csv(artist_df_file)
artist_df <- artist_df[,!(names(artist_df) %in% c("X"))]

collaborations <- filter(collaboration_info, id2 != "")
edge_ids <- data.frame(source = collaborations$id1, target = collaborations$id2) %>% unique()

# Get info about artists that aren't in artist_df from Spotify API.
new <- setdiff(edge_ids$target, artist_df$id)
new_artists <- get_new_artists(new)
new_artists$type <- "non_billboard"

setwd("/Users/lindsaymaggioncalda/Documents/J Balvin Project/Web App/new-artists-df")
write.csv(new_artists, "new_artists_000TEST.csv")

new_artists[is.na(new_artists)] <- 0
new_artists <- mutate(new_artists, total_num_songs = NA)

new_artists_date <- str_split(artist_df_file, "_")[[1]][2]

# Write new artist info to CSV for later use.
new_artists_file <- paste("new-artists", new_artists_date, ".csv", sep = "_")
write.csv(new_artists, new_artists_file)

# NEXT: artist-vertices-getter