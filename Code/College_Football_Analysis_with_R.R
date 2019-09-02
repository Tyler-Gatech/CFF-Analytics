####For this exercise I use the 2018_CFB_Data football file

library(ggplot2)
library(dplyr)
library(sqldf)
library(corrplot)


####Reading Data into R

#####To view the summary of the data note there are a lot of factors, these are good 
#####for modeling, but for text manipulation they can cause issues, so for now
#####I'll convert these into strings by using stringsAsFactors = F
data <- read.csv("/Users/tknight01/Desktop/Github/CFF/CFBPBPData/2018_CFB_Data.csv",header=TRUE,sep=",", stringsAsFactors = F)

#quick glance at the data
unique(data$gameid)

#first lets plot the number of games by team using dplyr and hist
game_totals <- data %>%
  group_by(activeteam, gameid) %>%
  distinct(activeteam, gameid) %>%
  group_by(activeteam) %>%
  summarize(game_ct = n())

#note the break at 0-2, these are likely d1 schools
hist(game_totals$game_ct)

####Summary Views

#####What is the size of the data
dim(data)

#####What is the structure of the data
str(data)

#What if we wanted to build a simple predictive model on the winner of each game? 
#What data can we use and how does it need to be manipulated

##1. The data would need to be summarized on a team or game level, let's summarize to a game level

data_game_level <- data %>%
  group_by(gameid, hometeam, home.spread, awayteam, away.spread, o.u) %>%
  summarize (final_home_score = max(homescore) , final_away_score = max(awayscore))

#home_win flag
data_game_level$home_win <- ifelse(data_game_level$final_home_score>data_game_level$final_away_score,1,0)

#calc the difference to spread
data_game_level$diff_to_spread <- data_game_level$home.spread + 
  data_game_level$final_home_score - data_game_level$final_away_score

#home_cover_flag (cover is when you beat the spread)
data_game_level$home_cover <- ifelse((data_game_level$final_home_score
                                     + data_game_level$home.spread
                                     - data_game_level$final_away_score)
                                     >0,1,0)
#push_flag (push is when you tie the spread)
data_game_level$push <- ifelse((data_game_level$final_home_score
                                      + data_game_level$home.spread
                                      == data_game_level$final_away_score),
                                     1,0)

data_game_level$away_cover <- 1-data_game_level$home_cover-data_game_level$push

#clean out the NA's on spread
dgl_clean <- data_game_level[!is.na(data_game_level$diff_to_spread),]

#What's the percent push?
sum(dgl_clean$push)/nrow(dgl_clean)

#what's the percent cover by 

head(as.data.frame(dgl_clean))

hist(data_game_level$diff_to_spread)

data_game_level$diff_to_spread <- data_game_level$home.spread + 
  data_game_level$final_home_score - data_game_level$final_away_score



plot(-data_game_level$home.spread, (data_game_level$final_home_score - data_game_level$final_away_score))
plot(-data_game_level$home.spread, (data_game_level$final_home_score - data_game_level$final_away_score))

##### view summary of data field
##### If wanted to do some analysis on two pt conversions and/or extra points goals, i may want to view what 
##### data is in those fields
table(data$pkresult)
summary(data$xp.result)
summary(data$two.pt.result)

head(as.character(data$playstring))
##### Perhaps a simple question to ask would be to analyze if teams should go for 2? or in what scenarios?
##### And a simple answer would be to calculate the expected value of going for 2?
##### Is it worth going for 2? 

#####Expected points for going for 1 would be success * 1 / attempts
ex1 <- sum(data$xp.result=='GOOD')/sum(data$xp.result %in% (c('GOOD','BLOCKED','MISSED')))

#####Expected points for going for 2 would be success * 2 / attempts
ex2 <- sum(data$two.pt.result=='GOOD')/sum(data$two.pt.result %in% (c('GOOD','FAILED')))*2

#####let's plot these side by side'
bp <- barplot(c(ex1,ex2), names = c("Expected 1","Expected 2"), main = "Go for 1 or 2?")
text(bp, 0, round(c(ex1,ex2),3),cex=1,pos=3) 


##### So at first glance, we could argue, we shouldn't go for two, due to the lower expeced value
##### What would be even more interesting though is to analyze this data set and see if there are correlations between 
##### successfully converting a 2 point conversion and other factors. Naturally I could think of a few
##### 1. % success of running plays over 2 yards (i.e. the distance needed for a 2pt) during the game? on the season?
##### 2. % success of running plays inside the 10 yd line, where the defense is naturally more compact
##### 3. Quarter of the play?
##### 4. Spread in the game (i.e. is the team significantly better/worse)
##### 5. Previous success rate of 2 pt conversions
##### 6. Let's take a look at some of these various factors


##### To anlayze ". % success of running plays over 2 yards..." we need to manipulate the play by play data

##### Let's group the data by Game and Team and calc the % of runs that made it over 2 yds compared to all runs
##### We need to filter by running plays, see Play_IDs.R

run_ids_2018 <- readRDS("run_ids_2018.rds")

#add the !is.na(data$runyds) in order to prevent pass fumbles (playtype 9 and 29)
data_runs <- data[data$playtype %in% run_ids_2018 & !is.na(data$runyds),]

head(data_runs)
hist(data_runs$runyds)

#data runs over 2 yds?
data_runs$run_gt_2 <- ifelse(data_runs$runyds >5,1,0)

#team_totals, originally included activeid here, but noticed an outlier on run counts
#arkansas state is mistakenly id'd as 2440 in one of their games
#this causes a major outlier, and thus I just go with activeteam
run_sum <- data_runs %>%
  group_by(activeteam) %>%
  summarize(run_gt_2 = sum(run_gt_2), count_run = n(), mean_run = mean(runyds)) %>%
  arrange(desc(run_gt_2))

run_sum$success_gt2 <- run_sum$run_gt_2 / run_sum$count_run

#notice the two distinct groups, i think these are d1 vs d2 schools, hence the clear discrepancy
plot(run_sum$count_run, run_sum$success_gt2)

#let's filter out the d1 schools, see d1_ids.R
#note filter has to be done on team since there is one duplicated id
run_sum_d1 <- run_sum[run_sum$activeteam %in% d1_ids_2018$activeteam,]

#re running
plot(run_sum_d1$count_run, run_sum_d1$success_gt2)
plot(run_sum_d1$count_run, run_sum_d1$mean_run)

#viewing the correlation between run count and success/mean
cor(run_sum_d1$count_run, run_sum_d1$success_gt2)
cor(run_sum_d1$count_run, run_sum_d1$mean_run)

#let's plot the correlation of success_yds based on volume running from 0 to 20
data_runs_d1 <- data_runs[data_runs$activeteam %in% d1_ids_2018$activeteam,]

#creating a blank vector
cor_by_yds <- as.vector(NA)

#run a loop for each value from 0 to 20
#i.e. we'll see the correlation between running plays over x (0 to 20, as a percent) and the volume of run plays for a team
for(i in 0:20){
  data_runs_d1$run_gt_i <- ifelse(data_runs_d1$runyds >i,1,0)
  
  data_run_sum <- data_runs_d1 %>%
    group_by(activeteam) %>%
    summarize(run_gt_i = sum(run_gt_i), count_run = n(), mean_run = mean(runyds)) %>%
    arrange(desc(run_gt_i))
  
  data_run_sum$success_gt_i <- data_run_sum$run_gt_i / data_run_sum$count_run
 
  cor_by_yds[i] <-  cor(data_run_sum$count_run, data_run_sum$success_gt_i)
}

plot(cor_by_yds)

#gather teams 2 pt conversion rate
#####Expected points for going for 2 would be success * 2 / attempts
data_2_pt <- data[data$two.pt.result %in% (c('GOOD','FAILED')),]
data_2_pt$good <- ifelse(data_2_pt$two.pt.result=='GOOD',1,0)

#summarize by team , note only 114 teams have attempted a 2pt, 107 d1
data_2_pt_sum <- data_2_pt %>%
  group_by(activeteam) %>%
  summarize(good_ct = sum(good), try_ct = n(), na.rm = TRUE) %>%
  arrange(desc(good_ct))

data_2_pt_sum$pct_good = data_2_pt_sum$good_ct / data_2_pt_sum$try_ct

boxplot(data_2_pt_sum$pct_good ~ data_2_pt_sum$try_ct)


# Merging left and right: Data frames have columns "id" in common

combo <- sqldf("select A.*, B.* from run_sum_d1 as A
                       inner join data_2_pt_sum as B
                        on A.activeteam = B.activeteam")
combo$good_ct <- ifelse(is.na(combo$good_ct),0,combo$good_ct)
combo$try_ct <- ifelse(is.na(combo$try_ct),0,combo$try_ct)

combo_clean <- combo[!is.na(combo$pct_good),]

combo_clean <- combo_clean %>%
  mutate(quantile = ntile(success_gt2, 8))

summary_by_run2 <- combo_clean %>%
  group_by(quantile) %>%
  summarize(count = n(), pct_good = sum(good_ct) /sum(try_ct), tries = sum(try_ct))

summary_by_run2

ggplot(data = summary_by_run2,  aes(quantile,pct_good)) + geom_bar(stat = "identity")
ggplot(data = summary_by_run2,  aes(quantile,tries)) + geom_bar(stat = "identity")

#This leads me to think the passing teams are more successful at 2pt conversions
#let's look at passing %  by 2pt success rate
pass_ids_2018 <- readRDS("pass_ids_2018.rds")

#d1 data
data_d1 <- data[data$activeteam %in% d1_ids_2018$activeteam,]

#change sack to passing yds
head(data_d1[data_d1$playtype==7,])
data_d1$pass.yds <- ifelse(data_d1$playtype==7,data_d1$runyds,data_d1$pass.yds)
data_d1$runyds <- ifelse(data_d1$playtype==7,NA,data_d1$runyds)

head(data_d1[data_d1$playtype==3,])

data_d1_pass <- data_d1[(data_d1$playtype %in% pass_ids_2018 & !is.na(data_d1$pass.yds)),]

#playtypes
table(data_d1_pass$playtype)

#a complete pass
data_d1_pass$pass_comp <- ifelse(data_d1_pass$playtype %in% c(9,24,29,67),1,0)

#pass % success (includes sacks) on third down
data_d1_pass$ind_1st <- ifelse(data_d1_pass$down==1,1,0)
data_d1_pass$ind_2nd <- ifelse(data_d1_pass$down==2,1,0)
data_d1_pass$ind_3rd <- ifelse(data_d1_pass$down==3,1,0)


#summarize by team 
pass_sum <- data_d1_pass %>%
  group_by(activeteam) %>%
  summarize(pass_comp_sum = sum(pass_comp),
            pass_att_sum = n(),
            pass_comp_1st_sum = sum(pass_comp*ind_1st),
            pass_att_1st_sum = sum(ind_1st),
            pass_comp_2nd_sum = sum(pass_comp*ind_2nd),
            pass_att_2nd_sum = sum(ind_2nd),
            pass_comp_3rd_sum = sum(pass_comp*ind_3rd),
            pass_att_3rd_sum = sum(ind_3rd),
            mean_pass_yds = mean(pass.yds),
            total_pass_yds = sum(pass.yds)) %>%
  arrange(desc(pass_comp_sum))


pass_sum[,]

pass_sum$ratio_pass_1st <- pass_sum$pass_att_1st_sum / pass_sum$pass_att_sum
pass_sum$ratio_pass_2nd <- pass_sum$pass_att_2nd_sum / pass_sum$pass_att_sum
pass_sum$ratio_pass_3rd <- pass_sum$pass_att_3rd_sum / pass_sum$pass_att_sum

#interesting downward trend of teams who pass the most tend to do more 
#of their passing on 1st down , less on 2nd, even less on 3rd. this is likely due to the fact
#that they get more 1st downs and have less pass ops. bettter to compare pass vs run ratio
plot(pass_sum$pass_att_sum, pass_sum$ratio_pass_1st)
plot(pass_sum$pass_att_sum, pass_sum$ratio_pass_2nd)
plot(pass_sum$pass_att_sum, pass_sum$ratio_pass_3rd)


#join to 2 pt data and plot
# Merging left and right: Data frames have columns "id" in common

combo_pass <- sqldf("select A.*, B.* from pass_sum_3rd as A
                       inner join combo_clean as B
                        on A.activeteam = B.activeteam")

#why is there only 107 teams? INner join on 2 pt conversions, not all teams attempted one
combo_pass <- combo_pass %>%
  mutate(comp_quantile = ntile(comp_pct, 8))

summary_by_pass <- combo_pass %>%
  group_by(comp_quantile) %>%
  summarize(count = n(), pct_good = sum(good_ct) /sum(try_ct), tries = sum(try_ct))

summary_by_pass

ggplot(data = summary_by_pass,  aes(comp_quantile,pct_good)) + geom_bar(stat = "identity")
ggplot(data = summary_by_pass,  aes(comp_quantile,tries)) + geom_bar(stat = "identity")

cor(combo_pass$pct_good, combo_pass$comp_pct)

head(combo_pass)
  
#        