library(dplyr)
library(cluster)
library(factoextra)
library(gridExtra)
library(tidyr)
library(NbClust)
library(GGally)
library(tmap)
library(ggplot2)
library(grid)
library(lattice)
library(spData)
library(Rcpp)
library(RColorBrewer)
library(data.table)
library(tmaptools)
library(sf)
library(raster)
library(tibble)
library(tidyverse)
library(leaflet) 



rm(list = ls())

#Import data 
df <- read.csv('/Users/raphaelmirallie/Documents/GitHub/Movies/movies.csv')

#Get the country of origin
list_countries <- table(df$country)
list_countries <- sort(list_countries,decreasing = TRUE)
list_countries <- list_countries[c(-35)] #Remove NA

#Percentage of film from USA
list_countries['United States'] / sum(list_countries) #Output = 71%

#Number of film not from USA
sum(list_countries) - list_countries['United States'] #Output = 2188
#Data frame
list_countries <- as.data.frame.table(list_countries)


 
#-----------------------------------------------------------------------------
#Map with original data set -----

#World data 
data(World)
country <- World[,c('iso_a3','name')]
country <- st_drop_geometry(country)

#Add number_movies to country
Na <- setNames(data.frame(matrix(ncol = 1, nrow = 177)), c('Number_movies'))
country <- cbind(country,Na)
rm(Na)
for (c_world in country[[2]]){
  for (c_movie in list_countries[[1]]){
    if (c_movie == c_world){
      #print(list_countries[[2]][which(list_countries[[1]] == c_movie)])
      #print(country[[3]][which(country[[2]] == c_world)])
      country[[3]][which(country[[2]] == c_world)] <- list_countries[[2]][which(list_countries[[1]] == c_movie)] 
      } 
    }
}
rm(c_movie,c_world)

#Correcting missing elements
59 - (177 - sum(is.na(country$Number_movies)))

#write.csv(country,'~/Desktop/country.csv', row.names = FALSE)
#write.csv(list_countries,'~/Desktop/list_countries.csv', row.names = FALSE)

country[[3]][88] <- 35
country[[3]][42] <- country[[3]][42] + 12 
country[[3]][41] <- 8
country[[3]][136] <- country[[3]][136] + 2
country[[3]][104] <- 1


#Create world data
new_world <- cbind(World,country[[3]])
new_world <- new_world[-c(7,8),]
colnames(new_world)[16] <- c('movies')

map <- tm_shape(new_world) + tm_layout(frame = FALSE) + tm_polygons('movies',
                                         title = '',
                                         breaks = c(1,10,100,500,1000,6000),
                                         textNA='0 or missing',
                                         legend.reverse=TRUE)

map

#-----------------------------------------------------------------------------
#Map with external data set -----

df_thenumbers <- read.csv('/Users/raphaelmirallie/Documents/GitHub/Movies/Geography/thenumber_countries.csv')
thenumber_country <- as.numeric(str_remove(as.character(df_thenumbers$No..of.Movies),","))
df_thenumbers <- cbind(df_thenumbers,thenumber_country)

#Add number_movies to country
Na <- setNames(data.frame(matrix(ncol = 1, nrow = 177)), c('Number_movies'))
country <- cbind(country,Na)
rm(Na)
for (c_world in country[[2]]){
  for (c_movie in df_thenumbers[[1]]){
    if (c_movie == c_world){
      #print(list_countries[[2]][which(list_countries[[1]] == c_movie)])
      #print(country[[3]][which(country[[2]] == c_world)])
      country[[4]][which(country[[2]] == c_world)] <- df_thenumbers[[5]][which(df_thenumbers[[1]] == c_movie)] 
    } 
  }
}
rm(c_movie,c_world)

#write.csv(df_thenumbers,'~/Desktop/thenumber_country.csv', row.names = FALSE)
#write.csv(country,'~/Desktop/country_withna.csv', row.names = FALSE)

country[[4]][88] <- 1194
country[[4]][19] <- 23
country[[4]][22] <- 8
country[[4]][32] <- 1
country[[4]][35] <- 1
country[[4]][39] <- 13
country[[4]][41] <- 289
country[[4]][45] <- 22
country[[4]][89] <- 8
country[[4]][91] <- 2
country[[4]][171] <- 25
country[[4]][172] <- 23
country[[4]][136] <- 931

#Create world data
country <- country[-c(7,8),]
new_world <- cbind(new_world,country[[4]])

colnames(new_world)[17] <- c('thenumbers_movies')

map2 <- tm_shape(new_world) + tm_layout(frame = FALSE) + tm_polygons('thenumbers_movies',
                                         title = '',
                                         textNA='0 or missing',
                                         breaks = c(1,500,1000,5000,20000,25000),
                                         legend.reverse=TRUE)
map2

#The code below was replaced by the argument breaks in tm_polygons

#Na <- setNames(data.frame(matrix(ncol = 1, nrow = 175)), c('Legend_thenumbers'))
#country <- cbind(country,Na)
#for (i in  1:length(country[[1]])){
#  if (is.na(country[[4]][i])) {country[[5]][i] <- NA}
#    else if (0 < country[[4]][i] && country[[4]][i] < 200) { country[[5]][i] <- "1 to 200"}
#    else if (200 < country[[4]][i] && country[[4]][i]< 1000) { country[[5]][i] <- "200 to 1,000"}
#    else if (1000 <= country[[4]][i] && country[[4]][i]< 10000) { country[[5]][i] <- "1,000 to 5,000"}
#    else if (10000 <= country[[4]][i] && country[[4]][i]< 25000) { country[[5]][i] <- "10,000 to 25,000"}
#    else {country[[5]][i] <-"Error"}
#}

#new_world <- cbind(new_world,country[[5]])

#colnames(new_world)[18] <- c('thenumbers_movies_lab')

#map4 <- tm_shape(new_world) + tm_polygons('thenumbers_movies_lab',
#                                        title = '',
#                                        textNA='0',
#                                        legend.reverse=TRUE)

#-----------------------------------------------------------------------------
#Percentage per country -----


movie1 <- new_world$movies
movie1[is.na(movie1)] <- 0
names(movie1) <- new_world$name
movie1 <- movie1 / sum(movie1) * 100
movie1 <- sort(movie1, decreasing = TRUE)
movie1 <- movie1[1:20]
movie11 <- format(round(movie1, 2), nsmall = 2)
movie11 <- as.double(movie11)
names(movie11) <- names(movie1)

movie2 <- new_world$thenumbers_movies
movie2[is.na(movie2)] <- 0
names(movie2) <- new_world$name
movie2 <- movie2 / sum(movie2) * 100
movie2 <- sort(movie2, decreasing = TRUE)
movie2 <- movie2[1:20]
movie22 <- format(round(movie2, 2), nsmall = 2)
movie22 <- as.double(movie22)
names(movie22) <- names(movie2)

#write.csv(movie11,'~/Desktop/movie1.csv')
#write.csv(movie22,'~/Desktop/movie2.csv')

