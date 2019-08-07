#Read in the data set
df <- read.csv("/Users/tknight01/Desktop/Github/CFF/CFBPBPData/2018_CFB_Data.csv")


head(df)


plot_Missing <- function(df, title = NULL){
  temp_df <- as.data.frame(ifelse(is.na(df), 0, 1))
  temp_df <- temp_df[,order(colSums(temp_df))]
  data_temp <- expand.grid(list(x = 1:nrow(temp_df), y = colnames(temp_df)))
  data_temp$m <- as.vector(as.matrix(temp_df))
  data_temp <- data.frame(x = unlist(data_temp$x), y = unlist(data_temp$y), m = unlist(data_temp$m))
  ggplot(data_temp) + geom_tile(aes(x=x, y=y, fill=factor(m))) + scale_fill_manual(values=c("white", "black"), name="Missing\n(0=Yes, 1=No)") + theme_light() + ylab("") + xlab("") + ggtitle(title)
}


plot_Missing(df[,colSums(is.na(df)) > 0, with = FALSE])

temp_df <- as.data.frame(ifelse(is.na(df), 0, 1))
temp_df <- temp_df[,order(colSums(temp_df))]
data_temp <- expand.grid(list(x = 1:nrow(temp_df), y = colnames(temp_df)))
data_temp$m <- as.vector(as.matrix(temp_df))
data_temp <- data.frame(x = unlist(data_temp$x), y = unlist(data_temp$y), m = unlist(data_temp$m))
ggplot(data_temp) + geom_tile(aes(x=x, y=y, fill=factor(m))) + scale_fill_manual(values=c("white", "black"), name="Missing\n(0=Yes, 1=No)") + theme_light() + ylab("") + xlab("") + ggtitle(title)

library(ggplot2)
