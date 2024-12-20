install.packages("dplyr")
install.packages("utf8")

library(dplyr)
stats <- data.frame(player=c('A','B','C','D'),
                    runs=c(100,200,408,19),
                    wickets=c(17,20,NA,5))
filter(stats, runs>100)


stats <- data.frame(player=c('A','B','C','D','A','A'),
                    runs=c(100,200,408,19,56,100),
                    wickets=c(17,20,NA,5,2,17))
distinct(stats)#remove duplicate rows
distinct(stats,player, .keep_all =TRUE)#REMOVE DUPLICATE BASE ON A COLUMN


arrange(stats,runs)#order data based on runs
arrange(stats, desc(runs))# for desendind order

# for select function 
stats <- data.frame(player=c('A','B','C','D'),
                    runs=c(100,200,408,19),
                    wickets=c(17,20,NA,5))
select(stats,player,wickets)
rename(stats,runs_scored=runs)# rename a column

mutate(stats, avg=runs/4)# add clm that is avg
transmute(stats, avg=runs/4)# drp all clm and create new clm that is avg

stats <- data.frame(player=c('A','B','C','D'),
                    runs=c(100,200,408,19),
                    wickets=c(17,20,NA,5))
summarize(stats,sum(runs),mean(runs))
 vars <-c("mpg","hp","wt")
head(mtcars[vars]) 
summary(mtcars[vars])#
s<-mydata$Loan
sd(s)


mydata<-read.csv("C:Users/user/Desktop/DataScience/data_summer23-24.csv", header=TRUE, sep=",")
mydata
(is.na(mydata))

