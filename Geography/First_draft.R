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


rm(list = ls())

#Import data 
df <- read.csv('/Users/raphaelmirallie/Documents/GitHub/Movies/movies.csv')

#Get the country of origin
list_countries <- table(df$country)
list_countries <- sort(list_countries,decreasing = TRUE)

#Percentage of film from USA
list_countries['United States'] / sum(list_countries) #Output = 71%

#Number of film not from USA
sum(list_countries) - list_countries['United States'] #Output = 2193


