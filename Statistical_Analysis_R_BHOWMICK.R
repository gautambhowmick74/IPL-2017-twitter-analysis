library(readxl)
library(dplyr)
#Place .R and .csv in Default working directory of R. 
#Get working directory 
WD <- getwd() 
#Print working directory  
print(WD) 
#Set working directory 
if (!is.null(WD)) setwd(WD) 
#Load tweet records by date and team
tweets_by_date_path = paste(WD,"tweets_by_date.csv",sep="/")
tweets_by_date <- c(read.csv(tweets_by_date_path))
tweets_by_date_df<-data.frame(tweets_by_date)
#Grouping of tweets by team
grp_by_tweets<-tweets_by_date_df %>% 
              group_by(Team) %>% 
              summarise(Count = sum(Count))
#Bar plot team vs no. of tweets
tweet_barplot<-barplot(grp_by_tweets$Count,names.arg = c("DD","GL","KXIP","KKR","MI","RPS","RCB","SRH"),
        xlab="Team",
        ylab = "Count",
        las = 1,
        ylim = c(0,5000),
        col = c("steelblue1", "burlywood1", "slategray2", "orangered1", "palegreen","wheat1", "thistle", "lightblue1"))
text(x = tweet_barplot, y =grp_by_tweets$Count, label = grp_by_tweets$Count, pos = 3, cex = .9, col = "blue")
#Load supporter sentiment score
teams_path = paste(WD,"team_review.csv",sep="/")
teams <- c(read.csv(teams_path))
teams_df<-data.frame(teams)
#Barplot team vs popularity
tweet_barplot<-barplot(teams_df$PositiveCount,names.arg = c("DD","KKR","MI","SRH","KXIP","RPS","RCB","GL"),
                       xlab="Team",
                       ylab = "Popularity",
                       las = 1,
                       ylim = c(0,1000),
                       col = c("palegreen","wheat1", "thistle", "lightblue1","steelblue1", "burlywood1", "slategray2", "orangered1"))
#Filter KolkataKnightRiders and MumbaiIndians popularity
kkp_tweet<-teams_df[which(teams_df$Team.Name=="KolkataKnightRiders"),]
mi_tweet<-teams_df[which(teams_df$Team.Name=="MumbaiIndians"),]
#Filter supportes sentiment
kkp_tweet<-kkp_tweet[,2:4]
mi_tweet<-mi_tweet[,2:4]
#prop.test(c(kkp_tweet$PositiveCount,mi_tweet$PositiveCount),c(kkp_tweet$Total.Tweet,mi_tweet$Total.Tweet),alternative="two.sided")
#Perform t-test
t.test(kkp_tweet,mi_tweet,alternative="two.sided")

#Load batsman popularity
batsman_path = paste(WD,"batsman_review.csv",sep="/")
batsman <- c(read.csv(batsman_path))
batsman_df<-data.frame(batsman)
#Filter top 5 batsman
top_5_batsman<-batsman_df[1:5,]
#Barplot batsman vs. popularity
tweet_barplot<-barplot(top_5_batsman$Popularity.Count,names.arg = c("Gambhir","Kohli","Uthappa","Rana","Pollard"),
                       xlab="Batsman",
                       ylab = "Popularity",
                       las = 1,
                       ylim = c(0,350),
                       col = c("lightblue1","steelblue1", "burlywood1", "slategray2", "orangered1"))
#Load bowler popularity
bowler_path = paste(WD,"bowler_review.csv",sep="/")
bowlers <- c(read.csv(bowler_path))
bowlers_df<-data.frame(bowlers)
#Filter top 5 bowler
top_5_bowlers<-bowlers_df[1:5,]
#Barplot bowler vs. popularity
tweet_barplot<-barplot(top_5_bowlers$Popularity.Count,names.arg = c("Kumar","Morris","Malinga","Mishra","Yadav"),
                       xlab="Bowlers",
                       ylab = "Popularity",
                       las = 1,
                       ylim = c(0,400),
                       col = c("palegreen","wheat1", "thistle", "lightblue1","steelblue1"))



