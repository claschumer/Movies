library(readxl)
library(tidyverse)
library(stringr)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
color <- brewer.pal(5,'YlOrRd')
color[1] <- '#f2f0f0'

data <- read.csv("D:\\Documents\\données\\movies.csv")
inflation <- read.csv("D:\\Documents\\données\\inflation_data.csv")

#analysis per period:

datap1 <- subset(data,  1980 <= data$`year` & data$`year`< 1985 )
datap2 <- subset(data,  2000 <= data$`year` & data$`year` < 2020 )

#data <- datap1

net <- data$gross-data$budget #net profit

#inflation corrections

corr <- 1:41

for (i in 1:41) {
  corr[i] <- inflation$amount[42-i]
}
corr #indicator of inflation

data$budget <- data$budget*(corr[data$year-1979])
data$gross <- data$gross*(corr[data$year-1979])
net <- net*(corr[data$year-1979])

data <- cbind(data, net)

#log transformations

netlog <- log(net-min(net,na.rm=TRUE))
budgetlog <- log(data$budget)
grosslog <- log(data$gross)
votelog <- log(data$vote)

data <- cbind(data, budgetlog, grosslog, netlog, votelog)

#data <-subset(data,  1980 <= data$`year` & data$`year`< 1985 )

#link between log budget and score
p<-ggplot(data, aes(`budgetlog`, `score`) ) + geom_point(color='red', size=2)
p + labs(x = "Budget",y = "Score") +
  theme(text = element_text(size=20)) +
  ylim(0,10)+
  geom_abline(intercept = -14.5, slope = 2 , color="orange", linetype="dashed", size=1.5)+
  geom_abline(intercept = 8.5, slope = 0 , color="blue", linetype="dashed", size=1.5)

data_out1 <- subset(data,  data$score > -14.5+2*data$x1log)

#link between log budget and log gross
p<-ggplot(data, aes(`budgetlog`, `grosslog`) ) + geom_point(color='red', size=2)
p + labs(x = "Budget",y = "Gross") +
  theme(text = element_text(size=20)) 

#link between budget and net profit
p<-ggplot(data, aes(`budgetlog`, `netlog` )) + geom_point(color='red', size=2)
p + labs(x = "Budget",y = "Net profit indicator")+
  theme(text = element_text(size=20)) 

#link between budget and vote
p<-ggplot(data, aes(`budgetlog`, `votelog` )) + geom_point(color='red', size=2)
p + labs(x = "runtime",y = "score") +
  theme(text = element_text(size=20)) + scale_x_continuous(trans='log2')

#link between runtime and score
p<-ggplot(data, aes(`runtime`, `score` )) + geom_point(color='red', size=2)
p + labs(x = "runtime",y = "score") +
  theme(text = element_text(size=20)) + scale_x_continuous(trans='log2')

