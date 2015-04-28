rm(list=ls())
options(scipen = 20)
taxi.lim <- read.csv("~/Desktop/git/taxi/taxi/data/nycb2010_taxi_2013_stats_bldg_cnt_pctcbbldg_LIMIT_third_bldg_1000_pts.csv")
library(ggplot2)

p4 <- ggplot(taxi.lim, aes(medbrdist, dist_bldg_hght))
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

taxi.lim1 <- subset(taxi.lim, medbrdist >0)
taxi.lim2 <- cbind(taxi.lim1, log(taxi.lim1$medbrdist))
fit3 <- lm(taxi.lim2$dist_bldg_hght ~ taxi.lim2$medbrdist)
summary(fit3) # all data points


#Signed log
signedlog10 = function(x) {
  ifelse(abs(x) <= 1, 0, sign(x)*log10(abs(x)))
}

taxi.lim2 <- cbind(taxi.lim, signedlog10(taxi.lim$medbrdist))
fit1 <- lm(taxi.lim2$dist_bldg_hght ~ taxi.lim2$medbrdist)
summary(fit1) # all data points

boxcox(taxi.lim)
p1 <- ggplot(taxi.lim2, aes(medbrdist, dist_bldg_hght))
p1 <- p1 + geom_point()
p1

p11 <- qplot(dist_bldg_hght, data=taxi.lim, geom="histogram")
p11

p12 <- qplot(avgbrdist_meters, data=taxi.lim, geom="histogram")
p12


data(taxi.lim)
attach(taxi.lim)

m <- npreg(avgbrdist_meters~dist_bldg_hght)

plot(m,plot.errors.method="asymptotic",
     plot.errors.style="band",
     ylim=c(11,15.2))

points(dist_bldg_hght,avgbrdist_meters,cex=.25)
