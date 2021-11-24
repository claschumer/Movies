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
country[[3]][169] <- 0

#Create world data
new_world <- cbind(World,country[[3]])
new_world <- new_world[-c(7,8),]
colnames(new_world)[16] <- c('movies')

map <- tm_shape(new_world) + tm_polygons('movies',
                                         title = 'test',
                                         legend.reverse=TRUE)
map

list_countries
