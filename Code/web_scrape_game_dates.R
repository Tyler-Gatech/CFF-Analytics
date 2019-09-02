#Scrape ESPN for game ids for each week 
library(dplyr)
library(rvest)

#week1 data from ESPN in 2018
week1 <- read_html("https://www.espn.com/college-football/schedule/_/year/2018")

#week 2-15 data from ESPN in 2018
for(i in 2:15){
  name <- paste("week",i,sep="") 
  x <- read_html(paste0("https://www.espn.com/college-football/schedule/_/week/",i, "/year/2018"))
  assign(name, x) 
}

#creating a blank list
all_2018_data <- as.list(NA)

#combine all the weekly data
for(i in 1:15){
all_2018_data[[i]] <- get(paste0("week",i))
}

schedule_data <- as.list(NA)
#grab the weekly data from the "tbody"
for(i in 1:length(all_2018_data)){
  schedule_data[[i]] <- all_2018_data[[i]] %>%
    html_nodes(xpath = '//*[@id="sched-container"]') %>%
    html_nodes("tbody") 
}


#Get the tbody elements out of each weekly data
#Create a blank list
game_ids <- as.list(NA)

for (i in 1:length(schedule_data)){
#creates a blank list for the specific week list
  game_ids[[i]] <- as.list(NA)
  for(n in 1:length(schedule_data[[i]])){
#creates a blank list for each  dya of the weekly schedule
    game_ids[[i]][n] <- as.list(NA)
    
    #does a regex look for game ids
    game_ids[[i]][n] <- regmatches(
        schedule_data[[i]][n],gregexpr(
          "[0-9]{9}",schedule_data[[i]][n])
        )
  }
}

#Date pattern for regex search
date_pattern <- "(Jan(uary)?|Feb(ruary)?|Mar(ch)?|Apr(il)?|May|Jun(e)?|
                   Jul(y)?|Aug(ust)?|Sep(tember)?|Oct(ober)?|Nov(ember)?
         |Dec(ember)?)\\s+\\d{1,2}"

#grabbing dates for each game
dates_data <- as.list(NA)
for(i in 1:length(all_2018_data)){
  dates <- all_2018_data[[i]] %>%
    html_nodes(xpath = '//*[@id="sched-container"]') %>%
    html_nodes("h2") 
  dates_data[[i]] <- regmatches(dates,gregexpr(date_pattern,dates)) 
}





