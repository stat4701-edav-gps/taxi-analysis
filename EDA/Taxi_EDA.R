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
library(gmodels)
options(scipen = 20) # Turn off scientific notation

# Read data
july <- read.csv("~/Desktop/R/taxi_2013_m07.csv")
head(july)

# Sample dataset of 10000 observations
samp.july <- sample_n(july, 10000)
samp.july <- samp.july[complete.cases(samp.july), ]#drop NAs in sample

# New values
dbv <- samp.july$dist_bldg_hgh
distr <- samp.july$dist_roadbed
type <- samp.july$type

samp.july$dbv.f <- factor(samp.july$dist_bldg_hght)
samp.july$distr.f <- factor(samp.july$dist_roadbed)
is.factor(samp.july$dbv.f)
is.factor(samp.july$distr.f)
sapply(samp.july, is.numeric)

# Check missing values and ranges
length(july$tuid) # trip id
tuid <- july$tuid[!is.na(july$tuid)] 
length(july$tuid)

length(distr) # distance to roadbed
distr <- distr[!is.na(distr)] 
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

# Remove observations with 0 building height
samp.july <- subset(samp.july, samp.july$dist_bldg_hgh > 0)

# Summary stats
taxi.vars <- cbind(dbv, distr)
stat.desc(taxi.vars)
summary(sapply(samp.july[ , c("type")], as.factor))
quantile(samp.july$dist_roadbed) 
quantile(samp.july$dist_bldg_hght) 

# Check outliers
mean(dbv, trim = .05) #trimmed means - block building volume
mean(dbv, trim = .1)
mean(distr, trim = .05) #trimmed means - distance from roadbed
mean(distr, trim = .1)

boxplot(july$dist_roadbed, #boxplot - distance to roadbed
        ylab='Distance to Roadbed', 
        main= 'Box Plot of GPS Error (Distance to Roadbed)\nin NYC')

offroad <- subset(july, 
                  dist_roadbed >3)#sample that includes only observations outside the roadbed

boxplot(offroad$dist_roadbed, #boxplot - no errors
        ylab='Distance to Roadbed', 
        main= 'Box Plot of GPS Error (Distance to Roadbed)\nin NYC')


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
p1 <- ggplot(samp.july, aes(dist_roadbed, 
                         dist_bldg_hght))
p1 <- p1 + geom_point()
p1 <- p1 + stat_smooth(method="lm", se=FALSE)
p1 <- p1 + abline(0,1, col="blue")
p1 <- p1 + ylab='Distributed Building Height'
p1 <- p1 + xlab='Average distance to Roadbed'
p1 <- p1 + theme_bw() #remove background
p1 <- p1 + theme(panel.grid.major=element_blank(), 
                 panel.grid.minor=element_blank()) #remove grid
p1 <- p1 + theme(panel.border=element_blank()) #removes border
p1 <- p1 + theme(axis.line=element_line(colour="black")) #adds lines
p1

# With limits
p2 <- ggplot(samp.july, aes(dist_roadbed, dist_bldg_hght))
p2 <- p2 + geom_point()
p2 <- p2 + stat_smooth(method="lm", se=FALSE)
p1 <- p2 + abline(0,1) 
p2 <- p2 + ylim(0,100) 
p2 <- p2 + xlim(0,100) 
p2 <- p2 + ylab='Distributed Building Height'
p2 <- p2 + xlab='Average distance to Roadbed'
p2 <- p2 + theme_bw() 
p2 <- p2 + theme(panel.grid.major=element_blank(), 
                 panel.grid.minor=element_blank()) 
p2 <- p2 + theme(panel.border=element_blank())
p2 <- p2 + theme(axis.line=element_line(colour="black")) 
p2

# Even lower limits
p3 <- ggplot(samp.july, aes(dist_roadbed, dist_bldg_hght))
p3 <- p3 + geom_point()
p3 <- p3 + stat_smooth(method="lm", se=FALSE)
p3 <- p3 + ylim(0,25) #scale y axis to 25
p3 <- p3 + xlim(0,25) #scale y axis to 25
p3 <- p3 + abline(0,1)
p3 <- p3 + ylab='Distributed Building Height'
p3 <- p3 + xlab='Average distance to Roadbed'
p3 <- p3 + theme_bw() #remove background
p3 <- p3 + theme(panel.grid.major=element_blank(), 
                 panel.grid.minor=element_blank()) #remove grid
p3 <- p3 + theme(panel.border=element_blank()) #removes border
p3 <- p3 + theme(axis.line=element_line(colour="black")) #adds lines
p3

# With Log Transformation
# note: do something with zeros?
# Worked better in the smaller dataset may be nuts in the full
p4 <- ggplot(july.samp, aes(dist_roadbed, dist_bldg_hght))
p4 <- p4 + geom_point()
p4 <- p4 + stat_smooth(method="lm", se=FALSE)
p4 <- p4 + scale_x_log10()
p4 <- p4 + scale_y_log10()
p4 <- p4 + ylab='Distributed Building Height'
p4 <- p4 + xlab='Average distance to Roadbed'
p4 <- p4 + abline(0,1)
p4 <- p4 + theme_bw() #remove background
p4 <- p4 + theme(panel.grid.major=element_blank(), 
                 panel.grid.minor=element_blank()) #remove grid
p4 <- p4 + theme(panel.border=element_blank()) #removes border
p4 <- p4 + theme(axis.line=element_line(colour="black")) #adds lines
p4

# Sample restricted to observations outside the roadbed
offroad <- subset(samp.july, 
           dist_roadbed >0)#sample that includes only observations outside the roadbed

p6 <- ggplot(offroad, aes(dist_roadbed, 
                            dist_bldg_hght))
p6 <- p6 + geom_point()
p6 <- p6 + stat_smooth(method = "lm", se = FALSE)
p6 <- p6 + scale_x_log10()
p6 <- p6 + scale_y_log10()
p6 <- p6 + ylab = 'Distributed Building Height'
p6 <- p6 + xlab = 'Average distance to Roadbed'
p6 <- p6 + abline(0,1)
p6 <- p6 + theme_bw() #remove background
p6 <- p6 + theme(panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank()) #remove grid
p6 <- p6 + theme(panel.border = element_blank()) #removes border
p6 <- p6 + theme(axis.line = element_line(colour = "black")) #adds lines
p6

# Bivariates
ct <- samp.july
ct$height1.cat[samp.july$dist_bldg_hght<10] <- "low" #lowest 
ct$height1.cat[samp.july$dist_bldg_hght>=10] <- "high"
tbl <- table(ct$error, ct$height.cat) 
tbl2 <- CrossTable(ct$error, ct$height.cat) 
chisq.test(tbl) 

ct$height2.cat[samp.july$dist_bldg_hght<18] <- "low" #bottom half
ct$height2.cat[samp.july$dist_bldg_hght>=18] <- "high"
tbl <- table(ct$error, ct$height2.cat) 
ctbl1 <- CrossTable(ct$error, ct$height2.cat) 
chisq.test(tbl) 

ct$height2.cat[samp.july$dist_bldg_hght<18] <- "low" #bottom half
ct$height2.cat[samp.july$dist_bldg_hght>=18] <- "high"
tbl2 <- table(ct$error, ct$height2.cat) 
ctbl2 <- CrossTable(ct$error, ct$height2.cat) 
chisq.test(tbl) 

ct$height3.cat[samp.july$dist_bldg_hght<10] <- "low" #biggest diff
ct$height3.cat[samp.july$dist_bldg_hght>=34] <- "high"
tbl3 <- table(ct$error, ct$height3.cat) 
ctbl3 <- CrossTable(ct$error, ct$height3.cat) 
chisq.test(tbl) 

# Models
# linear
fit1 <- lm(samp.july$dist_bldg_hght ~ samp.july$dist_roadbed)
summary(fit1) # all data points

#Restricted to only observations off roadbed
fit2 <- lm(offroad$dist_bldg_hght ~ offroad$dist_roadbed)
summary(fit2) 

# remove the likely errors in error
offroad$dist_roadbed[offroad$dist_roadbed>=416] <- 0
offroad$dist_roadbed[offroad$dist_roadbed<3] <- 0
fit3 <- lm(offroad$dist_bldg_hght ~ offroad$dist_roadbed)
summary(fit3) # all data points

# Logistic
samp.july$error <- 0 + (samp.july$dist_roadbed>0)
logit <- glm(error ~ dist_bldg_hght, 
               data = samp.july, family = "binomial")
summary(logit)
confint(logit)
exp(coef(logit))
exp(cbind(OR = coef(logit), confint(logit)))

#highest lowest quartiles
offroad$height3.cat[offroad$dist_bldg_hght<10] <- 0 #biggest diff
offroad$height3.cat[offroad$dist_bldg_hght>=34] <- 1
logit <- glm(error ~ height3.cat, 
             data = offroad, family = "binomial")
summary(logit)

# Limit to manhattan
july3 <- july2[grep("36061", july2$geoid, ignore.case=T),]
july3$dist_roadbed[july3$dist_roadbed>=416] <- 0
fit4 <- lm(july3$dist_bldg_hght ~ july3$dist_roadbed)
summary(fit4) # all data points

#BigVis
devtools::install_github("hadley/bigvis")
