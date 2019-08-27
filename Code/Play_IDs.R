#the purpose of this code is to breakdown the playtype field in the cfb pbp data set
library(dplyr)

####Reading Data into R
data <- read.csv("/Users/tknight01/Desktop/Github/CFF/CFBPBPData/2018_CFB_Data.csv",header=TRUE,sep=",")

#let's see what play types are common where runyds are not NA (i.e. includes negative and 0 )
any_run <- data.frame(table(data$playtype[!is.na(data$runyds)]))

#view
any_run

#looking at a sample of each type of run plan
for (i in 1:length(any_run$Freq)){
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
#  9(volume: 741, tag: recovered_fumble, group: run)
#  20(volume: 45, tag: safety, group: runpass)
#  24(volume: 8, tag: lateral, group: pass) look
#  29(volume: 558, tag: lost_fumble, group: runpass)
#  39(volume 22, tag: fumble_return, group: runpass)
#  68(volume: 3095, tag: run_td, group: run)

#from here, we should keep 5, 68 as our clear running plays

#29 can be run play where fumble is lost
#9 can be  a run or pass where fumble is recoverd
check1 <- data[data$playtype %in% run_ids_2018 & is.na(data$runyds),]
table(check1$playtype)

#-1 is ambiguous, 7 is a sack designed passing play, 20 can be run or pass, 24 is a pass laterl
#39 is a fumble on a kickoff

#tcreating a vector of explicit runs
run_ids <- c(-1,5,68)
run_group <- c("run","run","run")
run_desc <- c("run_fumble_recovered","run_non_td", "run_td")


#these ids were ambiguous as for pass or run
any_ids <- c(9, 20, 29, 39)
any_group <- c("run_or_pass", "run_or_pass", "run_or_pass", "run_or_pass") 
any_desc <-  c("rp_fumble_recovery", "rp_safety", "rp_fumble_lost", "rp_fumble_lost_td" )

#viewing a sample of the "any" group
for(i in any_ids){
  print(i)
  print(" ")
  print(sample(as.character(data$playstring[data$playtype==i]),2))
  print(" ")
}


#let's look at plays with non-na passing yds
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
# 63, only 1, interception
# 67 pass td
#  7(volume: 3493, tag: sack, group: pass) (need to coerce to pass.yds from runyds)


#these are the explicit pass ids (added 7, from run, since its a designed pass play that results in a sack)
pass_ids <- c(3,7,24,26,36, 63, 67)
pass_group <- rep("pass", length(pass_ids))
pass_desc <- c("pass_inc", "pass_sack", "pass_comp_no_td", "pass_int_no_td", "pass_int_td", "pass_int_no_td", "pass_comp_td")

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

special_ids <- c(53, 52, 32, 12, 
                 59, 60, 18, 17, 
                 34, 37, 40, 38,
                 41 )
special_group <- c("kickoff", "punt", "kickoff", "kickoff",
                   "fg","fg","fg","punt",
                   "punt", "punt", "fg", "fg",
                   "fg"
                   )
special_desc <- c("kickoff_touchback","punt_return", "kickoff_return_td", "kickoff_return",
                  "fg_good","fg_miss","fg_block","punt_block",
                  "punt_return_td", "punt_block_td", "fg_miss_return", "fg_block_td",
                  "fg_miss_return_td"
                  )

ids <- c(run_ids, any_ids, pass_ids, special_ids)
group <- c(run_group, any_group, pass_group, special_group)
desc <- c(run_desc, any_desc, pass_desc, special_desc)

play_mapping_2018 <- cbind(ids, group, desc)

#check to make sure all are accounted for
unique(data$playtype) %in% ids

#make permanent
saveRDS(play_mapping_2018, "play_mapping_2018.rds")

play_mapping_2018



