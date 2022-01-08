library(lubridate)
library(devtools)
library(rvest)

# RUN THIS SCRIPT ON SUNDAYS.

#____________________________________________________
#________________HELPER FUNCTIONS____________________
#____________________________________________________

# Get Billboard Top 100 Artists for a single week.
get_week <- function(week){
  link <- paste("https://www.billboard.com/charts/artist-100/", toString(week), sep = "")
  print(link)
  page <- read_html(link)
  
  artist_names <- page %>% html_nodes("li h3#title-of-a-story.c-title") %>% html_text() %>%　trimws(which = c("both"), whitespace = "[ \t\r\n]")
  #print(artist_names)
  
  ranks_and_weeksonchart <- page %>% html_nodes("div ul li span.c-label") %>% html_text() %>%　trimws(which = c("both"), whitespace = "[ \t\r\n]") 
  #print(ranks_and_weeksonchart)
  ranks_and_weeksonchart <- ranks_and_weeksonchart[ranks_and_weeksonchart != "RE-\nENTRY"]
  ranks_and_weeksonchart <- ranks_and_weeksonchart[ranks_and_weeksonchart != "NEW"]
  ranks <- ranks_and_weeksonchart[seq(1, length(ranks_and_weeksonchart), 7)]
  #print(ranks)
  
  weeks_on_chart <- ranks_and_weeksonchart[seq(4, length(ranks_and_weeksonchart), 7)]
  #print(weeks_on_chart)
  
  this_week <- data.frame(week = week, rank = ranks, artist = artist_names, weeks_on_chart = weeks_on_chart)
  Sys.sleep(8)
  return(this_week)
}

#____________________________________________________
#_____________________MAIN FILE______________________
#____________________________________________________

setwd("/Users/lindsaymaggioncalda/Documents/J Balvin Project/Web App")

# If there's already a billboard file, load it and only get new dates.
if (file.exists("billboard-charts.csv")){
  billboard_charts <- read.csv("billboard-charts.csv")
  billboard_charts <- billboard_charts[,!(names(billboard_charts) %in% c("X"))]
  most_recent_week <- as.Date(billboard_charts$week[1])
  
  week <- Sys.Date()-1
  weeks <- c()
  while (week > most_recent_week) {
    weeks <- append(weeks, week)
    week <- week - 7
  }
  
  new_billboard_charts <- map_df(weeks, get_week)
  billboard_charts <- rbind(new_billboard_charts, billboard_charts)
  write.csv(billboard_charts, "billboard-charts-test.csv")
  print("Updated billboard-charts file.")
} else {
  # If there's no billboard file, start from scratch.
  # Web scrape to get all artists from Billboard Top 100 Weekly from past decade.
  
  # Get all weeks from past decade.
  end_week = Sys.Date()
  num_years = 10
  num_weeks = num_years*52
  weeks = c(end_week)
  
  for (i in 1:num_weeks) {
    next_week <- end_week - 7*i
    weeks <- append(weeks, next_week)
  }
  
  billboard_charts <- map_df(weeks, get_week)
  write.csv(billboard_charts, "billboard-charts.csv")
  print("Wrote new billboard-charts file.")
}
