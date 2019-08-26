####For this exercise I use the 2018_CFB_Data football file

library(ggplot2)
library(dplyr)
library(sqldf)
library(corrplot)


####Reading Data into R
data <- read.csv("/Users/tknight01/Desktop/Github/CFF/CFBPBPData/2018_CFB_Data.csv",header=TRUE,sep=",", stringsAsFactors = F)

#list of teams and d1_id flag from 2018, see div_ids_2018
div_ids_2018 <- readRDS("div_ids_2018.rds")
#filtering to d1 ids only
d1_ids_2018 <- div_ids_2018[div_ids_2018$d1==1,]

####Summary Views

#####To view the size of the data
dim(data)

#####To view the structure of the data
str(data)

#####To view the summary of the data
summary(data)

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
data_runs$run_gt_2 <- ifelse(data_runs$runyds >2,1,0)

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
#note filter has to be done on team and ids since there is one duplicated id
run_sum_d1 <- run_sum[run_sum$activeteam %in% d1_ids_2018$activeteam,]

#re running
plot(run_sum_d1$count_run, run_sum_d1$success_gt2)
plot(run_sum_d1$count_run, run_sum_d1$mean_run)

cor(run_sum_d1$count_run, run_sum_d1$success_gt2)
cor(run_sum_d1$count_run, run_sum_d1$mean_run)

#let's plot the correlation of success_yds based on volume running from 0 to 20
data_runs_d1 <- data_runs[data_runs$activeteam %in% d1_ids_2018$activeteam,]

cor_by_yds <- as.vector(NA)

#run a loop for each value from 0 to 20
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

#summarize by team 
data_2_pt_sum <- data_2_pt %>%
  group_by(activeteam) %>%
  summarize(good_ct = sum(good), try_ct = n(), na.rm = TRUE) %>%
  arrange(desc(good_ct))

data_2_pt_sum$pct_good = data_2_pt_sum$good_ct / data_2_pt_sum$try_ct


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







        