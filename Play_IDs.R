#the purpose of this code is to breakdown the playtype field in the cfb pbp data set
library(dplyr)

####Reading Data into R
data <- read.csv("/Users/tknight01/Desktop/Github/CFF/CFBPBPData/2018_CFB_Data.csv",header=TRUE,sep=",")


#let's see what play types are common where runyds are greater than 0 
pos_run <- data.frame(table(data$playtype[data$runyds > 0]))

#most are playtyp 5, but there are some other significant ones, including playtype 68
pos_run

#let's see what play types are common where runyds are greater than 0 
pos_run <- data.frame(table(data$playtype[data$runyds > 0]))

#most are playtyp 5, but there are some other significant ones, including playtype 68
pos_run

#let's see what play types are common where runyds are not NA (i.e. includes negative and 0 )
any_run <- data.frame(table(data$playtype[!is.na(data$runyds)]))

any_run
#Note playtype 7, 20-1 are not in the original query of > 0 yds
unique(data$playtype[!is.na(data$runyds)])[!unique(data$playtype[!is.na(data$runyds)]) %in% data$playtype[data$runyds > 0]]

##### We'll probably want to see some of descriptions for each of these play types to correctly classify them, lets pull a couple random descriptions for each
for (i in pos_run$Var1){
  #subset of plays that match given playtype
  print(as.numeric(i))
  #print(data$playtype[sample(1:as.numeric(i),2)])
  #print(data$playstring[sample(1:as.numeric(i),2)])
}

#looking at the 8 plays for playtype 24
as.character(data$playstring[data$playtype==24&!is.na(data$runyds)])

any_run
play_list <- as.numeric(as.vector(any_run$Var1))

#looking at a sample of each type of run plan
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
#  20(volume: 45, tag: safety, group: runpass)
#  24(volume: 8, tag: lateral_fumble, group: runpass) look
#  29(volume: 558, tag: lost_fumble, group: runpass)
#  39(volume 22, tag: fumble, group: runpass)
#  68(volume: 3095, tag: run_td, group: run)

#from here, we should keep 5, 68 as our clear running plays

#29 can be run play where fumble is lost
#9 can be  a run or pass where fumble is recoverd
check1 <- data[data$playtype %in% run_ids_2018 & is.na(data$runyds),]
table(check1$playtype)

#-1 is ambiguous, 7 is a sack designed passing play, 20 can be run or pass, 24 is a pass laterl
#39 is a fumble on a kickoff

#these are the run ids if data$runyds <> NA
run_ids_2018 <- c(5,9,29,68)

run_ids <- c(5,68)
run_group <- c("run","run")
run_desc <- c("non td run", "non td run")

any_ids <- c(9, 20, 29, 39)
any_group <- c("run or pass", "run or pass", "run or pass", "run or pass") 
any_desc <-  c("fumble recovered", "safety", "fumble lost", "fumble td" )

saveRDS(run_ids_2018, "run_ids_2018.rds")


#let's look at plays with na passing yds
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
#9  complete pass, fumble (if pass.yds <> NA)
# 24 complete pass
# 26 intercepted
# 29 complete pass, fumble (if pass.yds <> NA)
# 36 pick 6
# 63, only 1, ignore
# 67 pass td
#  7(volume: 3493, tag: sack, group: pass) (need to coerce to pass.yds from runyds)


#these are the pass ids if data$pass.yds <> NA, except for 7
pass_ids_2018 <- c(3,7,24,26,36,67)

pass_ids <- c(3,7,24,26,36,67)
pass_group <- rep("pass", length(pass_ids))
pass_desc <- c("incomplete", "sack", "complete_no_td", "interception_no_td", "interception_td", "complete_td")

ids <- c(run_ids, any_ids, pass_ids)
ids

leftovers <- unique(data$playtype)[!unique(data$playtype) %in% ids]

for (i in 1:length(leftovers)){
  #i = 20
  print(c("current:",leftovers[i]))
  print(c("playtype:", leftovers[i], " volume:", sum(data$playtype==leftovers[i])))
  print(c("sample:", sample(as.character(data$playstring[data$playtype == leftovers[i]]),1)))
  print(c("sample:", sample(as.character(data$playstring[data$playtype == leftovers[i]]),1)))
}



leftovers 

leftovers

special_ids <- c(53, 52, 32, 12, 
                 59, 60, 18, 17, 
                 34, 37, 40, 38,
                 63, 41 )
special_group <- c("kickoff", "punt", "kickoff", "kickoff",
                   "fg","fg","fg","punt",
                   "punt", "punt")
special_desc <- c("kickoff_touchback","punt_return", "kickoff_return_td", "kickoff_return",
                  "fg_good","fg_miss","fg_block","punt_block",
                  "punt_return_td", "punt_block_td" )


data[data$playstring=="Ty Mason 0 Yd Return of Blocked Punt (Ventell Bryant Pass to Kenny Yeboah for Two-Point Conversion)",]
data[3628:3630,]

head(data[data$playtype==36,])

7 vs 20?
  

i=1
head(as.character(data$playstring[data$playtype == leftovers[i]]))

sample(as.character(data$playstring[data$playtype == 40]),1)


sum(data$playtype==leftovers[i])


saveRDS(pass_ids_2018, "pass_ids_2018.rds")




#Let's map all other playtypes


c(5,9,29,68)
c(3,7,9,24,26,29,36,67)

all_play_ids <- unique(data$platype)
all_non_pass_ids <- all_play_ids[!all_play_ids %in% pass_ids_2018]
all_other_ids <- all_non_pass_ids[!all_non_pass_ids %in% run_ids_2018]

all_other_ids

(unique(data$playtype)[!unique(data$playtype) %in% pass_ids_2018]) %in% run_ids_2018



