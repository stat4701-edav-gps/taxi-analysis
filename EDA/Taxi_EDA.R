## R script for taxi data basic analysis
## Emilie
## 16 April 2015
## Uses July subset only, when complete we can try applying to all

rm(list=ls())

#packages
library(dplyr)
library(ggplot2)
library(reshape)
library(sp)
library(spdep)
library(psych)
library(prettyR)
library(pastecs)
options(scipen = 20) # Turn off scientific notation

# Read data
july <- read.csv("~/Desktop/R/taxi_2013_m07.csv")
head(july)

# New values
dbv <- july$dist_bldg_hgh
distr <- july$dist_roadbed
type <- july$type

# Check missing values and ranges
length(july$tuid) # trip id
tuid <- july$tuid[!is.na(july$tuid)] 
length(july$tuid)

length(distr) # distance to roadbed
distr <- distr[!is.na(dbv)] 
length(distr)

length(july$geoid) # block id
geoid <- july$geoid[!is.na(july$geoid)] 
length(july$gepid)

length(dbv) # average distributed building volume
dbv <- dbv[!is.na(dbv)] 
length(dbv)

length(type) # average distributed building volume
type <- dbv[!is.na(type)] 
length(type)

length(unique(july$type)) #pickup and dropoff types


# Summary stats table
taxi.vars <- cbind(dbv, distr)
stat.desc(taxi.vars)
summary(sapply(july[ , c("type")], as.factor))


# Check outliers
mean(dbv, trim = .05) #trimmed means - block building volume
mean(dbv, trim = .1)
mean(distr, trim = .05) #trimmed means - distance from roadbed
mean(distr, trim = .1)

boxplot(distr, #boxplot - distance to roadbed
        ylab='Distance to Roadbed', 
        main= 'Box Plot of Distance to Roadbed in NYC')

boxplot(dbv, #boxplot - block building volume
        ylab='Block Level DBV', 
        main= 'Box Plot of Average\n
        Distributed Building Volume by Block in NYC')

qqnorm(july$dist_roadbed, #qq plot - distance to roadbed
       ylab='Distance to Roadbed', 
       main= 'Q-Q Plot of Distance to Roadbed')

qqnorm(dbv, #qq plot - block building volume
       ylab='Distributed Building Volume', 
       main= 'Q-Q Plot of Average\n
        Distributed Building Volume by Block in NYC')


# Scatterplots
# No limits
p1 <- ggplot(july, aes(dist_roadbed, 
                         dist_bldg_hght))
p1 <- p1 + geom_point()
p1 <- p1 + stat_smooth(method="lm", se=FALSE)
p1 <- p1 + theme_bw() #remove background
p1 <- p1 + theme(panel.grid.major=element_blank(), 
                 panel.grid.minor=element_blank()) #remove grid
p1 <- p1 + theme(panel.border=element_blank()) #removes border
p1 <- p1 + theme(axis.line=element_line(colour="black")) #adds lines
p1

# With limits
p2 <- ggplot(july, aes(dist_roadbed, dist_bldg_hght))
p2 <- p2 + geom_point()
p2 <- p2 + stat_smooth(method="lm", se=FALSE)
p2 <- p2 + ylim(0,100) #scale y axis to 100
p2 <- p2 + xlim(0,100) #scale y axis to 100
p2 <- p2 + theme_bw() #remove background
p2 <- p2 + theme(panel.grid.major=element_blank(), 
                 panel.grid.minor=element_blank()) #remove grid
p2 <- p2 + theme(panel.border=element_blank()) #removes border
p2 <- p2 + theme(axis.line=element_line(colour="black")) #adds lines
p2

# Even lower limits
p3 <- ggplot(july, aes(dist_roadbed, dist_bldg_hght))
p3 <- p3 + geom_point()
p3 <- p3 + stat_smooth(method="lm", se=FALSE)
p3 <- p3 + ylim(0,25) #scale y axis to 100
p3 <- p3 + xlim(0,25) #scale y axis to 100
p3 <- p3 + theme_bw() #remove background
p3 <- p3 + theme(panel.grid.major=element_blank(), 
                 panel.grid.minor=element_blank()) #remove grid
p3 <- p3 + theme(panel.border=element_blank()) #removes border
p3 <- p3 + theme(axis.line=element_line(colour="black")) #adds lines
p3

# With Log Transformation
# note: do something with zeros?
# Worked better in the smaller dataset may be nuts in the full
p4 <- ggplot(july, aes(dist_roadbed, dist_bldg_hght))
p4 <- p4 + geom_point()
p4 <- p4 + stat_smooth(method="lm", se=FALSE)
p4 <- p4 + scale_x_log10()
p4 <- p4 + scale_y_log10()
p4 <- p4 + theme_bw() #remove background
p4 <- p4 + theme(panel.grid.major=element_blank(), 
                 panel.grid.minor=element_blank()) #remove grid
p4 <- p4 + theme(panel.border=element_blank()) #removes border
p4 <- p4 + theme(axis.line=element_line(colour="black")) #adds lines
p4

