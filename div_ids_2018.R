#The purpose of this code is to isolate the d1 teams from the non d1 teams as this may effect analysis

library(dplyr)

####Reading Data into R
data <- read.csv("/Users/tknight01/Desktop/Github/CFF/CFBPBPData/2018_CFB_Data.csv",header=TRUE,sep=",")


#quick check to make sure there aren't two teams with the same id
id_name_check <- data %>%
  select (activeid, activeteam) %>%
  distinct (activeid, activeteam) 

#Group by ID
id_count <- id_name_check %>%
  select (activeid) %>%
  group_by (activeid) %>%
  summarize (count = n())

#check/view duplicate ids
id_name_check[id_name_check$activeid %in% id_count$activeid[id_count$count>=2],]

#thats unfortunate, Not sure why ESPN would do this perhpas its b/c the teams 
#are in different divisions, but it seems sloppy. Let's check the other way around


#Group by name
name_count <- id_name_check %>%
  select (activeteam) %>%
  group_by (activeteam) %>%
  summarize (count = n())

#check/view duplicate teams
id_name_check[id_name_check$activeid %in% name_count$activeteam[name_count$count>=2],]

#it looks like the right method will be to group by name and id

#we'll do this by counting the number of games played
team_game_distinct <- data %>%
  select (activeid, activeteam, gameid) %>%
  group_by(activeid, activeteam, gameid) %>%
  distinct (activeid, activeteam, gameid)

#preview the result
head(team_game_distinct)

team_game_distinct[team_game_distinct$activeid==2440,]

team_game_ct <- team_game_distinct %>%
  group_by (activeteam) %>%
  summarize (game_ct_sum = n()) %>%
  arrange(desc(game_ct_sum))

#Use a hist to check where the cutoff should be
hist(team_game_ct$game_ct_sum)

#create a flag and save the result as a permanent
team_game_ct$d1 <- ifelse(team_game_ct$game_ct_sum>=8,1,0)

head(team_game_ct)

#converting factors to characters
team_game_ct <- data.frame(lapply(team_game_ct, as.character), stringsAsFactors=FALSE)

saveRDS(team_game_ct, "div_ids_2018.rds")

team_game_ct[team_game_ct$activeid==2440,]

team_game_ct[team_game_ct$activeteam=="Arkansas State Red Wolves",]

