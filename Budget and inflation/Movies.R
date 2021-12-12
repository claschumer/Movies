library(readxl)
library(tidyverse)
library(stringr)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(cowplot)
library(colorspace)
library(ggrepel)
color <- brewer.pal(5,'YlOrRd')
color[1] <- '#f2f0f0'

data <- read.csv("D:\\Documents\\données\\movies.csv")
inflation <- read.csv("D:\\Documents\\données\\inflation_data.csv")

data <- filter(data, data$budget > 1 & data$gross > 1, data$year != 2020)

#analysis per decade
#data <- subset(data,  1980 <= data$`year` & data$`year`< 1989 )

#net profit
net <- data$gross-data$budget 

#inflation corrections

corr <- 1:42
for (i in 1:42) {
  corr[i] <- inflation$amount[43-i]
}
inflation <- cbind(inflation, corr)

#plot of inflation

p<-ggplot(inflation, aes(`year`, `corr`) ) + geom_point(color='red', size=2)
p + labs(x = "Year",y = "One dollar value in 2021") +
  theme(text = element_text(size=20))

inflation <- filter(inflation, year != 2021) #suppress year 2021

#correction of the finantial values with the inflation

data$budget <- data$budget*(inflation$corr[data$year-1979])
data$gross <- data$gross*(inflation$corr[data$year-1979])
net <- net*(corr[data$year-1979])

data <- cbind(data, net)

#log transformations
budgetlog <- log(data$budget)
grosslog <- log(data$gross)
votelog <- log(data$vote)

data <- cbind(data, budgetlog, grosslog, votelog)

#data <-subset(data,  1980 <= data$`year` & data$`year`< 1985 )

##beginning of analysis

#evolution of the average net profit per year

evolution <- aggregate(data$net, by=list(Category=data$year), FUN=mean)
evolution <- cbind(evolution, log(evolution$x))
p<-ggplot(evolution, aes(`Category`, `log(evolution$x)`) ) + geom_point(color='red', size=2)+ylim(0,20)
p + labs(x = "year",y = "log of the average net profit per movies")+ theme(text = element_text(size=20))+
  geom_smooth(method = "lm", se = FALSE)

fit <- lm( `Category` ~ log(evolution$x), data = evolution)
summary(fit)

#evolution of the total net profit per year
evolution <- aggregate(data$net, by=list(Category=data$year), FUN=sum)
evolution <- cbind(evolution, log(evolution$x))
p<-ggplot(evolution, aes(`Category`, `log(evolution$x)`) ) + geom_point(color='red', size=2)+ylim(0,25)
p + labs(x = "year",y = "log of the total net profit")+ theme(text = element_text(size=20))+
  geom_smooth(method = "lm", se = FALSE)

fit <- lm( `Category` ~ log(evolution$x), data = evolution)
summary(fit)

#evolution of the score per year
evolution <- aggregate(data$score, by=list(Category=data$year), FUN=mean)
p<-ggplot(evolution, aes(`Category`, `x`) ) + geom_point(color='red', size=2)
p + labs(x = "year",y = "avearge score")+ theme(text = element_text(size=20))+ylim(0,10)+
  geom_smooth(method = "lm", se = FALSE)

evol1 <- aggregate(data$budget, by=list(Category=data$year), FUN=sum)
evol1$x
evolbudget <- log(evol1$x)
evolbudget
evol2 <- aggregate(data$gross, by=list(Category=data$year), FUN=sum)
evol2$x
evolgross <- log(evol2$x)
evolgross

df <- data.frame(Category=rep(c("Budget", "Gross"), each=40),
year=rep(c(1980:2019),2),
logarithm =c(evolbudget, evolgross) )

ggplot(data=df, aes(x=year, y=logarithm, fill=Category)) +
  geom_bar(stat="identity", position=position_dodge())


#link between log budget and score
p<-ggplot(data, aes(`budgetlog`, `score`) ) + geom_point(color='red', size=2)
p + labs(x = "Budget",y = "Score") +
  theme(text = element_text(size=20)) +
  ylim(0,10)+
  geom_abline(intercept = 8.5, slope = 0 , color="blue", linetype="dashed", size=1.5)+
  geom_abline(intercept = 2.5, slope = 0 , color="yellow", linetype="dashed", size=1.5)

data_highscore <- subset(data,  data$score > 8.5)
data_highscore <- subset(data,  data$score <2.5)

#F-test
high <- filter(data, log(data$budget) >= 15)
middle <- filter(data, log(data$budget) >= 11.5 & log(data$budget) <15)
low <- filter(data, log(data$budget) < 11.5)

z <- unlist(high$score)
x <- unlist(middle$score)
y <- unlist(low$score)

var.test(x,y)  # test F
var.test(x,z)

#link between log budget and log gross
p<-ggplot(data, aes(`budgetlog`, `grosslog`) ) + geom_point(color='red', size=2)
p + labs(x = "Budget",y = "Gross") +
  theme(text = element_text(size=20))+
    geom_abline(intercept = 0, slope = 1 , color="blue", linetype="dashed", size=1.5)

#Anova test:


