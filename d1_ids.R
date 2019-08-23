#The purpose of this code is to isolate the d1 teams from the non d1 teams as this may effect analysis

library(dplyr)

####Reading Data into R
data <- read.csv("/Users/tknight01/Desktop/Github/CFF/CFBPBPData/2018_CFB_Data.csv",header=TRUE,sep=",")


team_game_distinct <- data %>%
  select (activeid, gameid) %>%
  group_by(activeid, gameid, game_ct = 1) %>%
  summarize (count = n())

#preview the result
head(team_game_distinct)

team_game_ct <- team_game_distinct %>%
  group_by (activeid) %>%
  summarize (game_ct_sum = sum(game_ct)) %>%
  arrange(desc(game_ct_sum))

#Use a hist to check where the cutoff should be
hist(team_game_ct$game_ct_sum)

#create a flag and save the result as a permanent
team_game_ct$d1 <- ifelse(team_game_ct$game_ct_sum>=8,1,0)

d1_ids <- as.vector(team_game_ct$activeid[team_game_ct$d1==1])

saveRDS(d1_ids, "d1_ids.rds")

