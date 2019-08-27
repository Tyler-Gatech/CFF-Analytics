####Reading Data into R
data <- read.csv("/Users/tknight01/Desktop/Github/CFF/CFBPBPData/2018_CFB_Data.csv",header=TRUE,sep=",")

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