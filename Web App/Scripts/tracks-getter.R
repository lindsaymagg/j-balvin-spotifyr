# Retrieve all of J Balvin's released tracks (albums and singles) from Spotify API.

# Spotify credentials
Sys.setenv(SPOTIFY_CLIENT_ID = '111c591bacc340e2b10d5bf3d0ee0308')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'e84ab3fcac7e4821ba33a9e05b3baa43')
Sys.setenv(GENIUS_API_TOKEN = 'QrL3LZ6y6KEuHi1DdLL1F3VFdGH_5S02WX7UiiMEadYQF82LEfv_H_mghPn00hbq')
access_token <- get_spotify_access_token()
print("Credentials loaded")

print("Getting songs...")
jb_tracks_all <- get_artist_audio_features(
  artist = target_artist_id,
  include_groups = c("album", "single"),
  dedupe_albums = TRUE
) %>% drop_na("track_id") %>% drop_na("album_type")
# Write to CSV so I don't have to fetch songs every time I load this document.
jb_tracks_all$artist_ids <- sapply(jb_tracks_all$artists, compress_artist_ids)
jb_tracks_all$artists <- sapply(jb_tracks_all$artists, compress_artist_names)
# Drop columns holding information I don't need.
drop_cols <- c("artist_name", "artist_id", "album_id", "album_images", "album_release_date_precision", "analysis_url", "available_markets", "disc_number", "explicit","track_href","is_local","track_preview_url","track", "track_uri", "external_urls.spotify", "type", "time_signature", "mode", "loudness", "acousticness", "instrumentalness", "liveness", "key_name")
jb_tracks_all <- jb_tracks_all[,!(names(jb_tracks_all) %in% drop_cols)]
print("Got songs.")

print("Getting popularity...")
tracks_pop <- get_jb_tracks_pop(jb_tracks_all$track_id)
jb_tracks_all$track_popularity <- tracks_pop$popularity
print("Got popularity.")

file_name <- paste("jb-tracks-all", Sys.Date(), ".csv", sep = "_")
print(paste("File name:", file_name))

setwd("/Users/lindsaymaggioncalda/Documents/J Balvin Project/j-balvin-spotifyr/Web App/jb-tracks-all")
write.csv(jb_tracks_all, file_name)
print("Completed.")