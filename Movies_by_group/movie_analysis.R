library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(cluster)
library(factoextra)
library(gridExtra)
library(tidyr)
library(NbClust)
library(GGally)
library(hrbrthemes)
library(viridis)
library(forcats)
library(RColorBrewer)
library(plyr)
library(stringr)
library(ggrepel)
library(scales)
library(forcats)

#Import data
movies <- read_csv("~/Desktop/Project_3_SCV/movies.csv")

#Choose a palette
color <- brewer.pal(5, 'YlOrRd' )
display.brewer.pal(5, 'YlOrRd')
bigcolor <- brewer.pal(10,'YlOrRd')

#How many movies ? -> 7668
length(movies$name)

#How many companies ? -> 2385
as.factor(movies$company)

#How many genre ? -> 19 
as.factor(movies$genre) 

#Remove missing values 
clean_movie <- na.omit(movies)
sum(is.na(clean_movie))

#Gross/Budget 
clean_movie$gross_budget <- clean_movie$gross/clean_movie$budget

#Divide in two groups based on the genre: 
drama_movie <- clean_movie[which(clean_movie$genre == "Drama"),]
adventure_movie <- clean_movie[which(clean_movie$genre == "Adventure"),]
action_movie <- clean_movie[which(clean_movie$genre == "Action"),]
comedy_movie <- clean_movie[which(clean_movie$genre == "Comedy"),]
horror_movie <- clean_movie[which(clean_movie$genre == "Horror"),]
biography_movie <- clean_movie[which(clean_movie$genre == "Biography"),]
crime_movie <- clean_movie[which(clean_movie$genre == "Crime"),]
fantasy_movie <- clean_movie[which(clean_movie$genre == "Fantasy"),]
animation_movie <- clean_movie[which(clean_movie$genre == "Animation"),]
scifi_movie <- clean_movie[which(clean_movie$genre == "Sci-Fi"),]
family_movie <- clean_movie[which(clean_movie$genre == "Family"),]
mystery_movie <- clean_movie[which(clean_movie$genre == "Mystery"),]
romance_movie <- clean_movie[which(clean_movie$genre == "Romance"),]
western_movie <- clean_movie[which(clean_movie$genre == "Western"),]
thriller_movie <- clean_movie[which(clean_movie$genre == "Thriller"),]

#Summary_statistics in each categories of film:
#We consider only Action, Adventure, Animation, Biography, Comedy, Drama, Horror
#We remove Family, Fantasy, Mystery, Romance, Sci-Fi, Thriller, Western 
#Action: 
mean_action <- mean(action_movie$score)
sd_action <- sd(action_movie$score)
#Adventure: 
mean_adventure <- mean(adventure_movie$score)
sd_adventure <- sd(adventure_movie$score)
#Animation: 
mean_animation <- mean(animation_movie$score)
sd_animation <- sd(animation_movie$score)
#Biography: 
mean_biography <- mean(biography_movie$score)
sd_biography<- mean(biography_movie$score)
#Comedy: 
mean_comedy <- mean(comedy_movie$score)
sd_comedy <- sd(comedy_movie$score)
#Drama:
mean_drama <- mean(drama_movie$score)
sd_drama <- sd(drama_movie$score)
#Horror: 
mean_horror <- mean(horror_movie$score)
sd_horror <- sd(horror_movie$score)

combine <- rbind(action_movie,adventure_movie,animation_movie,biography_movie,comedy_movie,drama_movie,horror_movie)
ggplot(combine,aes(x=genre,y=score,fill=genre)) + geom_boxplot() + scale_fill_brewer(palette='YlOrRd') + labs(x="Genre",y="Score",fill="Colour") + theme(axis.text.x = element_text(angle=90))

#Histogram of each category: 
ggplot(combine,aes(x=genre,y=score,fill=genre)) + geom_col() + scale_fill_brewer(palette='YlOrRd') + labs(c="genre",y="Score") + theme(axis.text.x = element_text(angle=90))

#Cluster analysis in each genre: 

#Action:
data_action <- data.frame(action_movie$name,action_movie$score,action_movie$budget,action_movie$gross,action_movie$runtime)
data_cluster_action <- data_action[-1]

kluster <- kmeans(data_cluster_action,centers=5)
data_action$cluster <- as.factor(kluster$cluster)
table(kluster$cluster)
ggplot(data_action,aes(x=cluster,y=action_movie.score,col=cluster)) + geom_point(alpha=0.6) + geom_jitter() + labs(x="Clusters",y="Score",colour="clusters") + scale_color_brewer(palette='YlOrRd')

#Optimal number of cluster 
fviz_nbclust(data_cluster_action,kmeans,method="silhouette")  + labs(subtitle="Average Silhouette")

#Repeat clustering with two cluster: 
data_action_2 <- data.frame(action_movie$name,action_movie$score,action_movie$budget,action_movie$gross,action_movie$runtime)
data_cluster_action_2 <- data_action_2[-1]

kluster_2 <- kmeans(data_cluster_action_2,centers=2)
data_action_2$cluster <- as.factor(kluster_2$cluster)
table(kluster_2$cluster)
ggplot(data_action_2,aes(x=cluster,y=action_movie.score,col=cluster)) + geom_point(alpha=0.6) + geom_jitter() + labs(x="Clusters",y="Score",colour="clusters") + scale_color_brewer(palette='YlOrRd')

#Comedy:
data_comedy <- data.frame(comedy_movie$name,comedy_movie$score,comedy_movie$budget,comedy_movie$gross,comedy_movie$runtime)
data_cluster_comedy <- data_comedy[-1]

kluster_com <- kmeans(data_cluster_comedy,centers=5)
data_comedy$cluster <- as.factor(kluster_com$cluster)
table(kluster_com$cluster)
ggplot(data_comedy,aes(x=cluster,y=comedy_movie.score,col=cluster)) + geom_point(alpha=0.6) + geom_jitter() + labs(x="Clusters",y="Score",colour="clusters") + scale_color_brewer(palette='YlOrRd')

#Optimal number of cluster 
fviz_nbclust(data_cluster_comedy,kmeans,method="silhouette")  + labs(subtitle="Average Silhouette")

#Which genre have the highest revenue/budget ? 
gross_budget_action <- mean(action_movie$gross_budget)
gross_budget_action
gross_budget_adventure <- mean(adventure_movie$gross_budget)
gross_budget_adventure
gross_budget_romance <- mean(romance_movie$gross_budget)
gross_budget_romance
gross_budget_crime <- mean(crime_movie$gross_budget)
gross_budget_crime
gross_budget_biography <- mean(biography_movie$gross_budget)
gross_budget_biography
gross_budget_fantasy <- mean(fantasy_movie$gross_budget)
gross_budget_fantasy
gross_budget_animation <- mean(animation_movie$gross_budget)
gross_budget_animation
gross_budget_horror <- mean(horror_movie$gross_budget)
gross_budget_horror
gross_budget_thriller <- mean(thriller_movie$gross_budget)
gross_budget_thriller
gross_budget_family <- mean(family_movie$gross_budget)
gross_budget_family
gross_budget_western <- mean(western_movie$gross_budget)
gross_budget_western
gross_budget_SCIFI <- mean(scifi_movie$gross_budget)
gross_budget_SCIFI
gross_budget_drama <- mean(drama_movie$gross_budget)
gross_budget_drama

#Histogram on profit by genre
ggplot(clean_movie,aes(x=genre,y=gross,fill=genre)) + geom_col(fill="#BD0026") + labs(c="genre",y="Gross") + theme(axis.text.x = element_text(angle=90))

#Histogram on gross by genre
ggplot(clean_movie,aes(x=genre,y=budget,fill=genre)) + geom_col(fill="#BD0026") + labs(c="genre",y="Budget") + theme(axis.text.x = element_text(angle=90))

#Which genre is most prominent in regions ? 
#Number of countries: 50 
as.factor(clean_movie$country)

movie_France <- clean_movie[which(clean_movie$country == "France"),]
movie_NZ <- clean_movie[which(clean_movie$country == "New Zealand"),]
movie_Japan <- clean_movie[which(clean_movie$country == "Japan"),]
movie_HK <- clean_movie[which(clean_movie$country == "Hong Kong"),]
movie_Ireland <- clean_movie[which(clean_movie$country == "Ireland"),]
movie_german <- clean_movie[which(clean_movie$country == "Germany"),]
movie_Canada <- clean_movie[which(clean_movie$country == "Canada"),]
movie_Australia <- clean_movie[which(clean_movie$country == "Australia"),]
movie_China <- clean_movie[which(clean_movie$country == "China"),]
movie_UK <- clean_movie[which(clean_movie$country == "United Kingdom"),]

#Pie chart: 
#France
count_france <- count(movie_France,vars="genre")
count_france$percentage <- 100*count_france$freq /sum(count_france$freq)
count_france <- format(count_france,digits=2,justfy="none")
count_france<- type.convert(count_france)
count_france <- count_france[-c(8,10),]
ggplot(count_france,aes(x="",y=percentage,fill=genre)) + geom_bar(stat="identity") + coord_polar("y") + scale_fill_manual(values=c("Coral","Coral1","tan1","sienna","Coral4","Red3","Red4","orangered","tomate","tomato3")) +labs(x="",y="Genre") + geom_text(aes(x=1.6,label=paste0(percentage,"%")),position=position_stack(vjust=0.5))+ theme(panel.background = element_blank(),axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),axis.title = element_blank()) 

#German                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
count_german <- count(movie_german,vars="genre")
count_german$percentage <- 100*count_german$freq /sum(count_german$freq)
count_german <- format(count_german,digits=2,justfy="none")
count_german<- type.convert(count_german)
ggplot(count_german,aes(x="",y=percentage,fill=genre)) + geom_bar(stat="identity") + coord_polar("y") + scale_fill_manual(values=c("Coral","Coral1","tan1","sienna","Coral4","Red3","Red4")) +labs(x="",y="Genre") + geom_text(aes(x=1.6,label=paste0(percentage,"%")),position=position_stack(vjust=0.5))+ theme(panel.background = element_blank(),axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),axis.title = element_blank()) 

#UK 
count_UK <- count(movie_UK,vars="genre")
count_UK$percentage <- 100*count_UK$freq /sum(count_UK$freq)
count_UK <- format(count_UK,digits=2,justfy="none")
count_UK<- type.convert(count_UK)
count_UK <- count_UK[-c(3,8,9,10,11,12,13,14),]
ggplot(count_UK,aes(x="",y=percentage,fill=genre)) + geom_bar(stat="identity") + coord_polar("y") + scale_fill_manual(values=c("Coral","Coral1","tan1","sienna","Coral4","Red3","Red4","tomato","tomato3")) +labs(x="",y="Genre") + geom_text(aes(x=1.6,label=paste0(percentage,"%")),position=position_stack(vjust=0.5))+ theme(panel.background = element_blank(),axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),axis.title = element_blank()) 

#Correlation between score and budget in each categorie: 
cor(action_movie$score,action_movie$budget,method="pearson")
cor(adventure_movie$score,adventure_movie$budget,method="pearson")
cor(romance_movie$score,romance_movie$budget,method="pearson")
cor(comedy_movie$score,comedy_movie$budget,method="pearson")
cor(horror_movie$score,horror_movie$budget,method = "pearson")

