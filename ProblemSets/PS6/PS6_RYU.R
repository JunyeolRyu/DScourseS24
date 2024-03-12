install.packages("data.table")
installed.packages()
library("data.table")
packageVersion("data.table")

library(tidyverse)
library(ggthemes)
library(hexbin)
library(datasets)
library(ggplot2)
library(gridExtra)
library(zoo)

setwd("C:/Users/ryujy/OneDrive/munseo")
PS6_CSV <- read.csv("PS6_datafile_csv.csv")

view(PS6_CSV)
str(PS6_CSV)
summary(PS6_CSV)



##Figure 1
ggplot(PS6_CSV, aes(x=JKM, y=JCC)) + geom_point() + geom_jitter() + theme_minimal() +
  labs(x="JKM($/MMBTU)", y="JCC($/MMBTU")
ggsave("Figure1.png")


##Figure 2
common_theme <- function() {
  ptcolor <- 'grey20' 
  theme(
    plot.title=element_text(size=14, lineheight=0.8, color=ptcolor, hjust=0.5),
    axis.title.x=element_text(color=ptcolor),
    axis.title.y=element_text(color=ptcolor))
}


ggplot(data=PS6_CSV, aes(x=JKM, y=fuelP)) +
  geom_point(aes(colour=gas_type), shape=15, size=1.5) +
  labs(x="JKM($/MMBTU)", y="Fuel Price (₩)") +
  common_theme() +
  theme(plot.title=element_text(color="#2255DD")) +
theme(axis.text.x = element_text(size=5,face='bold'))
ggsave("Figure2.png")


##Figure 3
ggplot(PS6_CSV, aes(x=fuelP, fill=Season)) + 
  geom_histogram(binwidth = 0.9) +
  ggtitle("Fuel Price Histogram by Season") +
  labs(x="Fuel Price(₩)", y="Count") +
  theme(axis.title = element_text(size=10),
        title = element_text(size=10)) + 
  theme(axis.text.x = element_text(size=5,face='bold'))


ggsave("Figure3.png")






##Other Visualization


ggplot(PS6_CSV, aes(x=HH, y=JCC)) + geom_point() + geom_jitter() + theme_minimal()
ggplot(PS6_CSV, aes(x=HH, y=JKM)) + geom_point() + geom_jitter() + theme_minimal()


ggplot(data=PS6_CSV, aes(x=JCC, y=fuelP)) +
  geom_point(aes(colour=gas_type), shape=15, size=1.5) +
  labs(x="JKM($/MMBTU)", y="Fuel Price(₩)") +
  common_theme() +
  theme(plot.title=element_text(color="#2255DD"))



ggplot(data = PS6_CSV, aes(x = fuelP)) + geom_density() + theme_minimal() 



agg <- aggregate(JKM ~ time, data=PS6_CSV, mean)
colnames(agg) <- c("Time", "JKM_Price")
ggplot(data = agg, aes(x=Time, y=JKM_Price, group=1)) +
  geom_line(linetype=1, color="grey20") +   geom_point(color="red")

agg2 <- aggregate(JCC ~ time, data=PS6_CSV, mean)
colnames(agg2) <- c("Time", "JCC_Price")
ggplot(data = agg2, aes(x=Time, y=JCC_Price, group=1)) +
  geom_line(linetype=1, color="grey20") +   geom_point(color="red")




ggplot(data = PS6_CSV, aes(y = fuelP, x=as.factor(Season))) + geom_boxplot() + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(y = "Fuel Price", x = "Season")


ggplot(data=PS6_CSV) +
  geom_boxplot(mapping=aes(x=Genconame, y=fuelP))


ggplot(PS6_CSV, aes(x=Genconame, y=fuelP)) + 
  geom_boxplot(width=0.5) +
  coord_flip() +
  stat_summary(fun_y="mean", geom="point", shape=23, size=3, fill="white") + 
  labs(y = "Fuel Price", x = "Company Name")

ggplot(PS6_CSV, aes(x=Season, y=fuelP)) + 
  geom_violin() +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")





