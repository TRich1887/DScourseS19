library(tidyverse)

winsvsgoals <- read.csv("C:\\Users\\twric\\Desktop\\EPL Seasons Data.csv")
View(winsvsgoals)
ggplot(data = winsvsgoals)+geom_point(mapping = aes(x=Total.Goals,y=Total.Wins,color = Team,size = 3))+ggtitle("Wins Compared To Goals")+xlab("Total Goals")+ylab("Total Wins")

wvg <- lm(Total.Wins ~ Total.Goals,data = winsvsgoals)
summary(wvg)

ggplot(winsvsgoals, aes(x=season_a,y=wins_a))+geom_bar(stat = "identity", fill = "red")+ggtitle("Wins per Season")+xlab("Season")+ylab("Wins")
ggplot(data = winsvsgoals)+geom_line(mapping = aes(x=season_a,y=total_scoring_att_a, group = 1, size = 1), color = "blue")+ggtitle("Shots Per season")+xlab("Seaosn")+ylab("Shots on Goal")
