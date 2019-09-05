#Scrape ESPN for game ids for each week from 2002 to 2019
library(dplyr)
library(rvest)

#Grab week1 data from ESPN for each year from 2002 to 2019
#This file will be used to determine how many weeks are in each year
#We will then iterate for each week in each each year
for (i in 2002:2019){
    name <- paste("week_1_year_",i,sep="") 
    x <- read_html(paste0("https://www.espn.com/college-football/schedule/_/week/1/year/",i))
    assign(name, x) 
}

#Determine the max # of weeks for each year
for(i in 2002:2019){
max_weeks <- get(paste0("week_1_year_",i)) %>%
  html_nodes(xpath = '//*[@id="sched-container"]') %>%
  html_nodes(xpath = '//*[@class="filters display-desktop"]') %>%
  html_nodes("ul") %>%
  html_nodes("li") %>%
  html_nodes("a")

week_vector <- as.vector(NA)

  for (n in 1:length(max_weeks)){
    first <- regmatches(max_weeks[n],gregexpr("Week\\s+\\d{1,2}",max_weeks[n]))
    week_vector[n] <- as.numeric(regmatches(first, gregexpr("\\d{1,2}",first)))
  }

#removing NAs
  week_vector <- week_vector[!is.na(week_vector)]

  assign(paste("week_vector_",i,sep="") , week_vector)
  
}

#removing week 6 for year 2016, for some reason that week is not currently working
#try link ("https://www.espn.com/college-football/schedule/_/week/6/year/2016")
week_vector_2016 <- week_vector_2016[!week_vector_2016==6]

#removing week 16,15 in 2004,2005-2007 dropdown option exists, but no games occur
#link https://www.espn.com/college-football/schedule/_/week/16/year/2004
week_vector_2004 <- week_vector_2004[!week_vector_2004==16]
week_vector_2006 <- week_vector_2006[!week_vector_2006==16]
week_vector_2007 <- week_vector_2007[!week_vector_2007==16]
week_vector_2008 <- week_vector_2008[!week_vector_2008==16]


week_vector_2005 <- week_vector_2005[!week_vector_2005==15]
week_vector_2006 <- week_vector_2006[!week_vector_2006==15]
week_vector_2007 <- week_vector_2007[!week_vector_2007==15]

#Grab game ids each for week for each year from 2002-2019
for(year in 2002:2019){

  #grab data for each week in that year
  for(week in get(paste0("week_vector_",year))){
    name <- paste("week_",week,"_year_",year,sep="") 
    x <- read_html(paste0("https://www.espn.com/college-football/schedule/_/week/",week, "/year/",year))
    assign(name, x) 
  }
  
  
}

# for(i in 1:16){
#   name <- paste("week",i,sep="") 
#   x <- read_html(paste0("https://www.espn.com/college-football/schedule/_/week/",i, "/year/",year))
#   assign(name, x) 
# }

#bowl games 
#"https://www.espn.com/college-football/schedule/_/week/1/year/2018/seasontype/3"
gameid_df_all <- as.data.frame(matrix(data = NA, nrow =0 , ncol = 4))
colnames(gameid_df_all) <- c("game_id","date","week","year")


#Originally built for just one year, so will reiterate through each year and combine at the end
for(year in 2002:2019){

  #creating a blank list
  all_data <- as.list(NA)
  
  #combine all the weekly data
  for(week in get(paste0("week_vector_",year))){
    all_data[[week]] <- get(paste0("week_",week,"_year_",year))
  }
  
  

  #grab the weekly data from the "tbody"
  schedule_data <- as.list(NA)
    
  for(week in get(paste0("week_vector_",year))){
    schedule_data[[week]] <- all_data[[week]] %>%
      html_nodes(xpath = '//*[@id="sched-container"]') %>%
      html_nodes("tbody") 
  }
  
  
#Get the tbody elements out of each weekly data. That is where the gameid is located
  #Create a blank list
  game_ids <- as.list(NA)
  
  for(week in get(paste0("week_vector_",year))){
  #creates a blank list for the specific week list
    game_ids[[week]] <- as.list(NA)
    for(n in 1:length(schedule_data[[week]])){
  #creates a blank list for each  day of the weekly schedule
      game_ids[[week]][n] <- as.list(NA)
      
      #does a regex look for game ids
      game_ids[[week]][n] <- regmatches(
          schedule_data[[week]][n],gregexpr(
            "[0-9]{9}",schedule_data[[week]][n])
          )
    }
  }
  

  
  #Date pattern for regex search
  date_pattern <- "(Jan(uary)?|Feb(ruary)?|Mar(ch)?|Apr(il)?|May|Jun(e)?|
                     Jul(y)?|Aug(ust)?|Sep(tember)?|Oct(ober)?|Nov(ember)?|Dec(ember)?)\\s+\\d{1,2}"
  
  #grabbing dates for each game
  dates_data <- as.list(NA)
  for(week in get(paste0("week_vector_",year))){
    dates <- all_data[[week]] %>%
      html_nodes(xpath = '//*[@id="sched-container"]') %>%
      html_nodes("h2") 
    dates_data[[week]] <- regmatches(dates,gregexpr(date_pattern,dates)) 
  }
  

  #dates_data
  #Combining Dates and Game Id data into a dataframe
  head(game_ids)
  head(dates_data)
  
  game_ids[[1]]
  dates_data[[1]]
  
  #counting the # of unique games
  game_ids_count <- 0
  for (i in game_ids){
    for (n in i)
      game_ids_count <- game_ids_count + length(n)
  }
  
  #creating a blank data frame
  gameid_df <- as.data.frame(matrix(data = NA, nrow =game_ids_count , ncol = 4))
  colnames(gameid_df) <- c("game_id","date","week","year")
  
  #setting a row counter
  rowvar <- 0 
  
  #looping through each list to extract element into data frame
  for(week in get(paste0("week_vector_",year))){
    for (n in 1:length(game_ids[[week]])){
      for (z in 1:length(game_ids[[week]][[n]])){
      rowvar <- rowvar + 1
      gameid_df$game_id[rowvar] <- game_ids[[week]][[n]][z]
      gameid_df$date[rowvar] <- as.character(dates_data[[week]][n])
      gameid_df$week[rowvar] <- week
      gameid_df$year[rowvar] <- year
      
#      print(paste0(rowvar, as.character(dates_data[[i]][n])))
    }
    }
  }

gameid_df[gameid_df$week==7,]
  
#combining into the main df
gameid_df_all <- rbind(gameid_df_all, gameid_df)

}

for(i in 2002:2019){
  print(head(gameid_df_all[gameid_df_all$year==i,]))
  print(tail(gameid_df_all[gameid_df_all$year==i,]))
}
#export as csv
write.csv(gameid_df, paste0("all_game_ids_2002_2019",".csv"), row.names = F)

#export as R file
saveRDS(gameid_df,paste0("all_game_ids_2002_2019",".rds"))


