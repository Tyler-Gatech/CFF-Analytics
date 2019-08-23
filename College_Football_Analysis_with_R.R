####For this exercise I use the 2018_CFB_Data football file
library(ggplot2)
library(dplyr)
library(sqldf)
library(corrplot)


####Reading Data into R
data <- read.csv("/Users/tknight01/Desktop/Github/CFF/CFBPBPData/2018_CFB_Data.csv",header=TRUE,sep=",")

####Summary Views

#####To view the size of the data
dim(data)

#####To view the structure of the data
str(data)

#####Note R brings the data in as Integers, Numericals, Logicals, and Factors. 
######Factors are similar to strings with preset properties that enhance R's modeling functionality. Factors have 
###### set properties that can't be changed. For example if hair color has the factors of ("brown", "blonde", "red", "other")
###### you could not add in "blue". This creates memory efficiencies but obviously has limitations as well. Factors
###### also take on ordinal qualityies (e.g. "low", "med", "high"). 

##### in this data set, there is a field call "playstring", which is a description of
##### the plays. It's clearly not valuable to save this a factor, and takes up too much memory

##### let's say i just want to view a few strings, let's compare the speed

factor_time <- function(){
  start_time <- Sys.time()
  print(head(data$playstring))
  func_time <- (Sys.time()-start_time)
  return(func_time)
}

string_time <- function(){
  start_time <- Sys.time()
  print(head(as.character(data$playstring)))
  func_time <- (Sys.time()-start_time)
  return(func_time)
}

factor_time()
string_time()

head(as.character(data$playstring))

#####To view the summary of the data
summary(data)


##### view summary of data field
##### If wanted to do some analysis on two pt conversions and/or field goals, i may want to view what 
##### data is in those fields
table(data$pkresult)
summary(data$xp.result)
summary(data$two.pt.result)

##### Perhaps a simple question to ask would be to analyze if teams should go for 2? 
##### And a simple answer would be to calculate the expected value of going for 2?
##### Is it worth going for 2? 

#####Expected points for going for 1 would be success * 1 / attempts
ex1 <- sum(data$xp.result=='GOOD')/sum(data$xp.result %in% (c('GOOD','BLOCKED','MISSED')))

#####Expected points for going for 2 would be success * 2 / attempts
ex2 <- sum(data$two.pt.result=='GOOD')/sum(data$two.pt.result %in% (c('GOOD','FAILED')))*2

#####let's plot these side by side'
bp <- barplot(c(ex1,ex2), names = c("Expected 1","Expected 2"), main = "Go for 1 or 2?")
text(bp, 0, round(c(ex1,ex2),3),cex=1,pos=3) 


##### What would be even more interesting though is to analyze this data set and see if there are correlations between 
##### successfully converting a 2 point conversion and other factors. Naturally I could think of a few
##### 1. % success of running plays over 2 yards (i.e. the distance needed for a 2pt) during the game? on the season?
##### 2. % success of running plays inside the 10 yd line, where the defense is naturally more compact
##### 3. Quarter of the play?
##### 4. Spread in the game (i.e. is the team significantly better/worse)
##### 5. Previous success rate a 2 pt conversions
##### 6. Let's take a look at some of these various factors


##### To anlayze ". % success of running plays over 2 yards..." we need to manipulate the game by game data

##### Let's group the data by Game and Team and calc the % of runs that made it over 2 yds compared to all runs
##### We need to filter by running plays, so how do we do that? Let's see how playtyp maps to "runyds"

#let's see what play types are common where runyds are greater than 0 
pos_run <- data.frame(table(data$playtype[data$runyds > 0]))

#most are playtyp 5, but there are some other significant ones, including playtp 68
pos_run

#let's see what play types are common where runyds are not NA
any_run <- data.frame(table(data$playtype[!is.na(data$runyds)]))

#Note playtype 7, 20-1 are not in the original query of > 0 yds
unique(data$playtype[!is.na(data$runyds)])[!unique(data$playtype[!is.na(data$runyds)]) %in% data$playtype[data$runyds > 0]]

##### We'll probably want to see some of descriptions for each of these play types to correctly classify them, lets pull a couple random descriptions for each
for (i in pos_run$Var1){
  #subset of plays that match given playtype
  print(as.numeric(i))
  #print(data$playtype[sample(1:as.numeric(i),2)])
  #print(data$playstring[sample(1:as.numeric(i),2)])
}

as.character(data$playstring[data$playtype==24&!is.na(data$runyds)])

any_run
play_list <- as.numeric(as.vector(any_run$Var1))
play_list[6]

any_run$Freq[6]

for (i in 1:length(play_list)){
  #i = 20
  sample_elements <- sample(1:any_run$Freq[i], 2, replace = F)
  print(c("playtype: ",play_list[i], "volume: ", any_run$Freq[i], "run yds: ", as.character(data$runyds[!is.na(data$runyds)&data$playtype==play_list[i]])[sample_elements[1]]))
  print(as.character(data$playstring[!is.na(data$runyds)&data$playtype==play_list[i]])[sample_elements[1]])
  print(c("playtype: ",play_list[i], "volume: ", any_run$Freq[i], "run yds: ", as.character(data$runyds[!is.na(data$runyds)&data$playtype==play_list[i]])[sample_elements[2]]))
  print(as.character(data$playstring[!is.na(data$runyds)&data$playtype==play_list[i]])[sample_elements[2]])
  }


#Playtype mapping
# -1(volume: 5,  tag: team_recovery_fumble, group: run)
#  5(volume: 60585, tag: run_no_td, group: run) 
#  7(volume: 3493, tag: sack, group: pass)
#  9(volume: 741, tag: recoverd_fumble, group: run)
#  20(volume: 45, tag: safety, group: pass)
#  24(volume: 8, tag: lateral_fumble, group: runpass) look
#  29(volume: 558, tag: lost_fumble, group: run)
#  39(volume 22, tag: fumble, group: kickoff)
#  68(volume: 3095, tag: run_td, group: run)

#from here, we should keep 5, 9, 29, 68 as our running plays
 
data_runs <- data[data$playtype %in% c(5,9,29,68) & !is.na(data$runyds),]
head(data_runs)
hist(data_runs$runyds)

#data runs over 2 yds?
data_runs$run_gt_2 <- ifelse(data_runs$runyds >2,1,0)

#data_runs[data_runs$activeteam=="Western Carolina Catamounts",]
head(data_runs)

#team_toals
run_sum <- data_runs %>%
  group_by(activeteam, activeid) %>%
  summarize(run_gt_2 = sum(run_gt_2), count_run = n(), mean_run = mean(runyds)) %>%
  arrange(desc(run_gt_2))

run_sum$success <- run_sum$run_gt_2 / run_sum$count_run

#notice the two distinct groups, i think these are d1 vs d2 schools, hence the clear discrepancy
plot(run_sum$count_run, run_sum$success)

#let's create a flag
run_sum$min_flag <- ifelse(run_sum$count_run>=200,1,0)

#isolatet the d1 schools and save as permanent
d1_ids <- as.vector(run_sum$activeid[run_sum$min_flag==1])
#just to be sure, lets check total games by school

team_game_distinct <- data %>%
  select (activeid, gameid) %>%
  group_by(activeid, gameid, game_ct = 1) %>%
  summarize (count = n())

team_game_distinct[team_game_distinct$activeid==322,]

team_game_ct <- team_game_distinct %>%
  group_by (activeid) %>%
  summarize (game_ct_sum = sum(game_ct)) %>%
  arrange(desc(game_ct_sum))


hist(team_game_ct$game_ct_sum)

team_game_ct$d1 <- ifelse(team_game_ct$game_ct_sum>=8,1,0)

#perfect match
sum(team_game_ct$activeid[team_game_ct$d1==1] %in% d1_ids) / length(d1_ids)


#create a d1 id list
#recheck original 2pt conversion rate
#####Expected points for going for 1 would be success * 1 / attempts
ex1 <- sum(data$xp.result=='GOOD')/sum(data$xp.result %in% (c('GOOD','BLOCKED','MISSED')))

sum(data$xp.result=='GOOD')
&data$activeid %in% d1_ids)

#####Expected points for going for 2 would be success * 2 / attempts
ex2 <- sum(data$two.pt.result=='GOOD')/sum(data$two.pt.result %in% (c('GOOD','FAILED')))*2

#####let's plot these side by side'
bp <- barplot(c(ex1,ex2), names = c("Expected 1","Expected 2"), main = "Go for 1 or 2?")
text(bp, 0, round(c(ex1,ex2),3),cex=1,pos=3) 


#redo the plot 
plot(run_sum$count_run[run_sum$min_flag==1], run_sum$success[run_sum$min_flag==1])

run_sum_qual <- run_sum[run_sum$min_flag==1,]

#gather teams 2 pt conversion rate
#####Expected points for going for 2 would be success * 2 / attempts
data_2_pt <- data[data$two.pt.result %in% (c('GOOD','FAILED')),]
data_2_pt$good <- ifelse(data_2_pt$two.pt.result=='GOOD',1,0)

#summarize by team 
data_2_pt_sum <- data_2_pt %>%
  group_by(activeteam, activeid) %>%
  summarize(good_ct = sum(good), try_ct = n(), na.rm = TRUE) %>%
  arrange(desc(good_ct))

head(run_sum_qual)

data_2_pt_sum$pct_good = data_2_pt_sum$good_ct / data_2_pt_sum$try_ct


# Merging left and right: Data frames have columns "id" in common

combo <- sqldf("select A.*, B.* from run_sum_qual as A
                       left join data_2_pt_sum as B on A.activeid = B.activeid")
combo$good_ct <- ifelse(is.na(combo$good_ct),0,combo$good_ct)
combo$try_ct <- ifelse(is.na(combo$try_ct),0,combo$try_ct)

combo_clean <- combo[!is.na(combo$pct_good),]

combo_clean <- combo_clean %>%
  mutate(quantile = ntile(success, 8))

summary_by_run2 <- combo_clean %>%
  group_by(quantile) %>%
  summarize(count = n(), pct_good = sum(good_ct) /sum(try_ct), tries = sum(try_ct))

summary_by_run2

ggplot(data = summary_by_run2,  aes(quantile,pct_good)) + geom_bar(stat = "identity")
ggplot(data = summary_by_run2,  aes(quantile,tries)) + geom_bar(stat = "identity")

#This leads me to think the passing teams are more successful at 2pt conversions

#I did want to split out by d1 and d2, note d2 teams are 0/7 in 2018

sum(data$two.pt.result %in% c('GOOD','FAILED'))
sum(data$two.pt.result %in% c('GOOD','FAILED') & data$activeid %in% d1_ids)
sum(data$two.pt.result %in% c('GOOD','FAILED') & (!data$activeid %in% d1_ids))



#let's look at passing %  by 2pt success rate
pass_table <- data.frame(table(data$playtype[(!is.na(data$qb))&(!is.na(data$pass.yds))]))


#Examine as sample of these play types
pass_play_list <- as.numeric(as.vector(pass_table$Var1))

for (i in 1:length(pass_play_list)){
  #i = 20
  sample_elements <- sample(1:pass_table$Freq[i], min(pass_table$Freq[i],2), replace = F)
  print(c("playtype: ",pass_play_list[i], "volume: ", pass_table$Freq[i], "pass yds: ", as.character(data$pass.yds[!is.na(data$pass.yds)&data$playtype==pass_play_list[i]])[sample_elements[1]]))
  print(as.character(data$playstring[!is.na(data$pass.yds)&data$playtype==pass_play_list[i]])[sample_elements[1]])
  print(c("playtype: ",pass_play_list[i], "volume: ", pass_table$Freq[i], "pass yds: ", as.character(data$pass.yds[!is.na(data$pass.yds)&data$playtype==pass_play_list[i]])[sample_elements[2]]))
  print(as.character(data$playstring[!is.na(data$pass.yds)&data$playtype==pass_play_list[i]])[sample_elements[2]])
}


#playtypes
#3 incomplete pass
#9  complete pass, fumble
# 24 complete pass
# 26 intercepted
# 29 complete pass, fumble
# 36 pick 6
# 63, only 1, ignore
# 67 pass td
#  7(volume: 3493, tag: sack, group: pass)







        