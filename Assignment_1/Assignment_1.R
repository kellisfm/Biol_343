Data=read.csv("DecodonAsexualityData.csv")

setwd()

class(Data)
dim(Data)
head(Data)
tail(Data)

Data$OvaryAreaMM=Data$OvaryWidthMM*Data$OvaryHeightMM


library(dplyr)
?group_by(data)
?summarise(.Data)
