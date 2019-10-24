setwd("C:/Users/Nabz/Dropbox/moneyball data/data")


install.packages("ggplot2")
library(ggplot2)



baseball_data= read.csv("baseball.csv",header=T)
str(baseball_data)
head(baseball_data)


moneyball = subset(baseball_data, Year < 2002)
str(moneyball)

moneyball$RD = moneyball$RS - moneyball$RA
str(moneyball)

ggplot(data = moneyball, aes(x = RD, y = W)) + theme_bw() + scale_color_manual(values = c("grey", "red3")) + geom_hline(yintercept = c(85.0, 95.0), col = "green2", linetype = "longdash") +geom_point(aes(color = factor(Playoffs)), alpha = 0.5, pch = 16, size = 3.0) 

wins_rd = lm(W ~ RD, data = moneyball)
summary(wins_rd)



rs_reg1 = lm(RS ~ OBP + SLG + BA, data = moneyball)
summary(rs_reg1)

rs_reg2 = lm(RS ~ OBP + SLG, data = moneyball)
summary(rs_reg2)



ra_reg = lm(RA ~ OOBP + OSLG, data = moneyball)
summary(ra_reg)


#RS = -804.63 + (2737.77*OBP) + (1584.91*SLG)
RS = -804.63 + (2737.77*0.345) + (1584.91*0.439)
RS

#RA = -837.38 + (2913.60*OOBP) + (1514.29*OSLG)  
RA = -837.38 + 2913.60*0.308 + 1514.29*0.38 
RA

RD = RS - RA 
RD

W = 80.881 + (0.106 * RD)
W 