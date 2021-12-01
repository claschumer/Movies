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
library(lares)
library(knitr)
library(ggridges)


#Import data
movies <- read_csv("~/Desktop/Project_3_SCV/movies.csv")

#Choose a palette
color <- brewer.pal(9, 'YlOrRd' )
pal <- colorRampPalette(color)

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
sd_biography<- sd(biography_movie$score)
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


#Boxplot
ggplot(combine,aes(x=genre,y=score,fill=genre)) + geom_boxplot() + scale_fill_brewer(palette='YlOrRd') + labs(x="Genre",y="Score",fill="Colour") + theme(axis.text.x = element_text(angle=90))

#Histogram of each category: 
ggplot(clean_movie,aes(x=genre,y=score,fill=genre)) + geom_col() + scale_fill_manual(values=pal(15)) + labs(c="Genre",y="Score") + theme(axis.text.x = element_text(angle=90))

#Ridge Plot: 
ggplot(clean_movie) + geom_density_ridges(aes(x=score,y =factor(genre),fill=genre)) + scale_fill_manual(values=pal(15)) + labs(c="Genre",y="Score") 

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
ggplot(clean_movie,aes(x=genre,y=log(gross),fill=genre)) + geom_col() + labs(c="genre",y="Log of the Gross") + theme(axis.text.x = element_text(angle=90)) + scale_fill_manual(values= pal(15))

#Histogram on gross by genre
ggplot(clean_movie,aes(x=genre,y=log(budget),fill=genre)) + geom_col() + labs(c="Genre",y="Log of the Budget") + theme(axis.text.x = element_text(angle=90)) + scale_fill_manual(values= pal(15))

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
ggplot(count_france,aes(x="",y=percentage,fill=genre)) + geom_bar(stat="identity") + coord_polar("y") + scale_fill_manual(values=pal(10)) +labs(x="",y="Genre") + geom_text(aes(x=1.6,label=paste0(percentage,"%")),position=position_stack(vjust=0.5))+ theme(panel.background = element_blank(),axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),axis.title = element_blank()) 

#German                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
count_german <- count(movie_german,vars="genre")
count_german$percentage <- 100*count_german$freq /sum(count_german$freq)
count_german <- format(count_german,digits=2,justfy="none")
count_german<- type.convert(count_german)
ggplot(count_german,aes(x="",y=percentage,fill=genre)) + geom_bar(stat="identity") + coord_polar("y") + scale_fill_manual(values=pal(7)) +labs(x="",y="Genre") + geom_text(aes(x=1.6,label=paste0(percentage,"%")),position=position_stack(vjust=0.5))+ theme(panel.background = element_blank(),axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),axis.title = element_blank()) 

#UK 
count_UK <- count(movie_UK,vars="genre")
count_UK$percentage <- 100*count_UK$freq /sum(count_UK$freq)
count_UK <- format(count_UK,digits=2,justfy="none")
count_UK<- type.convert(count_UK)
count_UK <- count_UK[-c(3,8,9,10,11,12,13,14),]
ggplot(count_UK,aes(x="",y=percentage,fill=genre)) + geom_bar(stat="identity") + coord_polar("y") + scale_fill_manual(values=pal(9)) +labs(x="",y="Genre") + geom_text(aes(x=1.6,label=paste0(percentage,"%")),position=position_stack(vjust=0.5))+ theme(panel.background = element_blank(),axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),axis.title = element_blank()) 

#Correlation between score and budget in each categorie: 
cor(action_movie$score,action_movie$budget,method="pearson")
cor(adventure_movie$score,adventure_movie$budget,method="pearson")
cor(romance_movie$score,romance_movie$budget,method="pearson")
cor(comedy_movie$score,comedy_movie$budget,method="pearson")
cor(horror_movie$score,horror_movie$budget,method = "pearson")

#Create dummy variables for genre:
clean_movie$ind_genre <- rep(0,length(clean_movie$name))
for ( i in 1:length(clean_movie$genre)){
  if ( clean_movie$genre[i]== "Action"){clean_movie$ind_genre[i] = 1}
  if ( clean_movie$genre[i]== "Adventure"){clean_movie$ind_genre[i] = 2}
  if ( clean_movie$genre[i]== "Animation"){clean_movie$ind_genre[i] = 3}
  if ( clean_movie$genre[i]== "Biography"){clean_movie$ind_genre[i] = 4}
  if ( clean_movie$genre[i]== "Comedy"){clean_movie$ind_genre[i] = 5}
  if ( clean_movie$genre[i]== "Crime"){clean_movie$ind_genre[i] = 6}
  if (clean_movie$genre[i] == "Drama"){clean_movie$ind_genre[i] = 7}
  if ( clean_movie$genre[i]== "Family"){clean_movie$ind_genre[i] = 8}
  if ( clean_movie$genre[i]== "Fantasy"){clean_movie$ind_genre[i] = 9}
  if ( clean_movie$genre[i]== "Mystery"){clean_movie$ind_genre[i] = 10}
  if ( clean_movie$genre[i]== "Horror"){clean_movie$ind_genre[i] = 11}
  if ( clean_movie$genre[i]== "Romance"){clean_movie$ind_genre[i] = 12}
  if ( clean_movie$genre[i]== "Sci-Fi"){clean_movie$ind_genre[i] = 13}
  if ( clean_movie$genre[i]== "Thriller"){clean_movie$ind_genre[i] = 14}
  if ( clean_movie$genre[i]== "Western"){clean_movie$ind_genre[i] = 15}
}

#Indicator of biggest company: 
clean_movie$ind_company <- rep(0,length(clean_movie$genre))
count_company <- count(clean_movie$company)
big_company <- count_company$x[which(count_company$freq>50)]
middle_company <- count_company$x[which(count_company$freq< 50  & count_company$freq > 1)]
small_company <-count_company$x[which(count_company$freq == 1)]

for ( k in 1:length(big_company)){
  for(j in 1:length(clean_movie$name)){
    if(clean_movie$company[j] == big_company[k]){clean_movie$ind_company[j] = 1}
    }
}

for ( k in 1:length(middle_company)){
  for(j in 1:length(clean_movie$name)){
    if(clean_movie$company[j] == middle_company[k]){clean_movie$ind_company[j] = 2}
  }
}

for ( k in 1:length(small_company)){
  for(j in 1:length(clean_movie$name)){
    if(clean_movie$company[j] == small_company[k]){clean_movie$ind_company[j] = 1}
  }
}

#Indicator of USA:
clean_movie$ind_USA <- rep(0,length(clean_movie$name))
for(j in 1:length(clean_movie$rating)){
  if(clean_movie$country[j] == "United States"){clean_movie$ind_USA[j] = 1}
}

#Log of gross and budget because avec skweness ( talk in the introduction)
clean_movie$loggross <- log(clean_movie$gross)
clean_movie$logbudget <- log(clean_movie$budget)

#Indicator of big star: 
clean_movie$ind_star <- rep(0,length(clean_movie$genre))
count_star <- count(clean_movie$star)
big_star <- count_star$x[which(count_star$freq>20)]
middle_star <- count_star$x[which(count_star$freq< 50  & count_star$freq > 1)]
small_star <-count_star$x[which(count_star$freq == 1)]

for ( k in 1:length(big_star)){
  for(j in 1:length(clean_movie$name)){
    if(clean_movie$star[j] == big_star[k]){clean_movie$ind_star[j] = 1}
  }
}

for ( k in 1:length(middle_star)){
  for(j in 1:length(clean_movie$name)){
    if(clean_movie$star[j] == middle_star[k]){clean_movie$ind_star[j] = 2}
  }
}

for ( k in 1:length(small_star)){
  for(j in 1:length(clean_movie$name)){
    if(clean_movie$star[j] == small_star[k]){clean_movie$ind_star[j] = 1}
  }
}

#Use gower metric due to presence of factor: 
movie_cluster <- data.frame(clean_movie$name,clean_movie$score,clean_movie$ind_genre,clean_movie$ind_company,clean_movie$loggross,clean_movie$logbudget,clean_movie$ind_USA,clean_movie$ind_star)
movie_cluster_without_name <- movie_cluster[-c(1)]

gower_df <- daisy(movie_cluster_without_name,metric="gower")

cluster_number <- 2:18
sil_values <- map_dbl(cluster_number,function(x){pam_clusters=pam(as.matrix(gower_df),diss = TRUE,k=x) 
pam_clusters$silinfo$avg.width})
cluster_data <- data.frame(cluster=cluster_number,silhouette_width = sil_values)

ggplot(cluster_data,aes(cluster,silhouette_width)) + geom_point() + geom_line() + scale_x_continuous(breaks = c(1:18)) + labs(x="Clusters",y="Silhouette width")

#Check the number of movies in each cluster
pam_data = pam(gower_df,diss=TRUE,k=6)

movie_cluster$cluster <- pam_data$clustering

#Movies by cluster
ggplot(movie_cluster) + geom_bar(aes(cluster),fill = pal(6)) + scale_x_continuous(breaks = c(1:12))

#Density ridge for score and log gross:
ggplot(movie_cluster) + geom_density_ridges(aes(x=movie_cluster$clean_movie.score,y=factor(cluster),fill=factor(cluster))) + scale_fill_manual(values = pal(6)) + labs(x="cluster",y ="Genre",fill = "Clusters")  
ggplot(movie_cluster) + geom_density_ridges(aes(x=clean_movie.loggross,y=factor(cluster),fill=factor(cluster))) + ylab("cluster") + xlab("Log Gross") + scale_fill_manual(values = pal(6))

#Genre in each cluster
movie_cluster %>% group_by(cluster,clean_movie.ind_genre) %>% dplyr::summarise(n=n()) %>% ggplot() + geom_raster(aes(cluster,clean_movie.ind_genre, fill=factor(n))) + scale_x_continuous(breaks=c(1:6)) + scale_fill_manual(values = pal(47),name="n") +  scale_y_continuous(name = "Genre",breaks = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),labels=c("Action","Adventure","Animation","Biography","Comedy","Crime","Drama","Family","Fantasy","Mystery","Horror","Romance","Sci-Fi","Thriller","Western")) + theme(legend.position = "none")

#Check the profile of each cluster
clus4_average <- movie_cluster %>% group_by(cluster) %>% summarize_if(is.numeric,mean,na.rm=TRUE)
clus4_average <- rename(clus4_average, c("clean_movie.score" = " Score","clean_movie.ind_genre"="Genre ", "clean_movie.ind_company"="Company","clean_movie.loggross" = "Log Gross","clean_movie.logbudget"="Log Budget","clean_movie.ind_USA" ="Country","clean_movie.ind_star"="Star"))
ggparcoord(clus4_average, columns= c(2:8), groupColumn = "cluster",scale="uniminmax",order="skewness") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+ labs(x="Features",y="Values") + scale_color_viridis(option="rocket") 




