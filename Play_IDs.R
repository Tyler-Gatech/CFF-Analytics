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
#  39(volume 22, tag: fumble, group: kickoff)
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
saveRDS(run_ids_2018, "run_ids_2018.rds")
