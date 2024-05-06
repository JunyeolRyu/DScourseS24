library(tidyverse)

install.packages('ggthemes')
library(ggthemes)

install.packages('hexbin')
library(hexbin)

install.packages('datasets')
library(datasets)

install.packages('ggplot2')
library(ggplot2)

install.packages('gridExtra')
library(gridExtra)

install.packages('zoo')
library(zoo)


setwd("C:/Users/ryujy/OneDrive/바탕 화면/paper/직도입수입/0425")
LNG <- read.csv("Data_0506.csv")




common_theme <- function() {
  ptcolor <- 'grey20' 
  theme(
    plot.title=element_text(size=14, lineheight=0.8, color=ptcolor, hjust=0.5),
    axis.title.x=element_text(color=ptcolor),
    axis.title.y=element_text(color=ptcolor))
}


LNG$gas_type[LNG$gas_type == "가+직"] <- "combination"
LNG$gas_type[LNG$gas_type == "평균"] <- "KOGAS Average"
LNG$gas_type[LNG$gas_type == "직수입"] <- "Direct Import by Private"

###############Figure 2
ggplot(data=LNG, aes(x=JKM, y=fuelP)) +
  geom_point(aes(colour=gas_type), shape=15, size=1.5) +
  labs(x="JKM($/MMBTU)", y="Fuel Price") +
  common_theme() +
  theme(plot.title=element_text(color="#2255DD")) +
  theme(axis.text.x = element_text(size=5,face='bold'),
        axis.title = element_text(size=15),
        legend.position = "bottom",
        legend.text = element_text(size = 15))

###############Figure 3
ggplot(data=LNG, aes(x=JCC, y=fuelP)) +
  geom_point(aes(colour=gas_type), shape=15, size=1.5) +
  labs(x="JCC($/bbl)", y="Fuel Price") +
  common_theme() +
  theme(plot.title=element_text(color="#2255DD")) +
  theme(axis.text.x = element_text(size=5,face='bold'),
        axis.title = element_text(size=15),
        legend.position = "bottom",
        legend.text = element_text(size = 15))





LNG$Season[LNG$Season == "겨울"] <- "Winter"
LNG$Season[LNG$Season == "여름"] <- "Summer"
LNG$Season[LNG$Season == "봄"] <- "Spring"
LNG$Season[LNG$Season == "가을"] <- "Fall"


###############Figure 5
ggplot(LNG, aes(x=fuelP, fill=Season)) + 
  geom_histogram(binwidth = 1.2) +
  ggtitle("Fuel Price Histogram by Season") +
  labs(x="Fuel Price", y="Count") +
  theme(axis.title = element_text(size=10),
        title = element_text(size=10)) + 
  theme(axis.text.x = element_text(size=5,face='bold'),
        axis.title = element_text(size=20))+
  scale_fill_manual(values = c("Winter" = "royalblue1", "Summer" = "chartreuse1", "Spring" = "yellow2", "Fall" = "red1"))+  
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10)) +   theme(legend.title = element_blank(),
                                                          legend.position = "bottom")
'



