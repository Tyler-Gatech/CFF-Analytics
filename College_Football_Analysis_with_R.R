####For this exercise I use the 2018_CFB_Data football file
library(ggplot2)
library(dplyr)

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

head(a$playstring)

head(as.character(a$playstring))

#####To view the summary of the data
summary(data)


#####view summary of data field
table(pkresult)
summary(xp.result)
summary(two.pt.result)

#####Perhaps a simple question to ask would be to analyze if teams should go for 2? 
#####And a simple answer would be to calculate the expected value of going for 2?
#####Is it worth going for 2? 

#####Expected points for going for 1
ex1 <- sum(data$xp.result=='GOOD')/sum(data$xp.result %in% (c('GOOD','BLOCKED','MISSED')))

#####Expected points for going for 2
ex2 <- sum(data$two.pt.result=='GOOD')/sum(data$two.pt.result %in% (c('GOOD','FAILED')))*2

#####let's plot these side by side'
bp <- barplot(c(ex1,ex2), names = c("Expected 1","Expected 2"), main = "Go for 1 or 2?")
text(bp, 0, round(c(ex1,ex2),3),cex=1,pos=3) 


##### What would even more interesting though is to analyze this data set and see if there are correlations between 
##### successfully converting a 2 point conversion and other factors. Naturally I could think of a few
##### 1. % success of running plays over 2 yards (i.e. the distance needed for a 2pt)? during the game? on the season?
##### 2. % success of running plays inside the 10 yd line, where the defense is naturally more compact
##### 3. Quarter of the play?
##### 4. Spread in the game
##### 5. Let's take a look at some of these various factors
##### 6. Previous success rate a 2 pt conversions


##### To anlayze ". % esuccess of running plays over 2 yards..." we need to manipulate the game by game data

##### Let's group the data by Game and Team and calc the % of runs over 2 yds
##### Filter by running plays, how do we do that? Let's see how playtyp maps to "runyds"

#let's see what play types are common where runyds are greater than 0 
pos_run <- data.frame(table(data$playtype[data$runyds > 0]))
pos_run

#let's see what play types are common where runyds are not NA
table(data$playtype[!is.na(data$runyds)])

##### We'll then want to see some of descriptions for each of these play types, lets pull a couple random descriptions for each
for (i in pos_run$Var1){
  #subset of plays that match given playtype
  print(as.numeric(i))
  #print(data$playtype[sample(1:as.numeric(i),2)])
  #print(data$playstring[sample(1:as.numeric(i),2)])
}

a <-data[data$playtype==5,][2]

a$playtype[2]
a$playstring[2]

?sample



        