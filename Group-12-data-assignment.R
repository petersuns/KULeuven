#######################################################
#                                                     #
# KU Leuven                                           #
# Stastics IV                                         #
# Data Assignment I                                   #
# Group 12                                            #
# Shuo Sun & Eva Eggers, Axelle Bavre                 #
#                                                     #
#######################################################

# seting working directory
setwd("~/Google Drive (petersun)/KU LEUVEN/Master of Psychology/Statistics IV [P0T64a]")
# read the data file
topGear <- read.csv2("TopGearGroup199.csv")
# take a look of the data and it's structure
summary(topGear)
str(topGear)
# spot the repeated drivers for cluster effect  
table(summary(topGear$Celebrity))
topGear$Time <- as.numeric(as.character(topGear$Time))
mean(topGear$Time)
hist(topGear$Time)
topGear$Car.num  <- as.character(topGear$Car)

# create a new column Car.num using number 1,2,3,4 for four different cars
topGear$Car.num[topGear$Car.num == "Kia Cee'd"] <- 1
topGear$Car.num[topGear$Car.num == "Suzuki Liana"] <- 2
topGear$Car.num[topGear$Car.num == "Chevrolet Lacetti"] <- 3
topGear$Car.num[topGear$Car.num == "Vauxhall Astra"] <- 4

topGear$Car.num <-as.numeric(topGear$Car.num)
topGear$Time <- as.numeric(as.character(topGear$Time))
# a scaltter plot of time per condition
plot(jitter(topGear$Car.num, .2), topGear$Time,xaxt="n",xlab="Car Name",ylab="Time", main = "Time per car type",
     col=rgb(0,0,0,.6),cex=1,pch=16) 
axis(1,at=c(1,2,3,4),labels=c("Kia Cee'd","Suzuki Liana","Chevrolet Lacetti","Vauxhall Astra"))
mcond=tapply(topGear$Time,topGear$Car.num,mean)  
for (j in 1:4) lines((j)+c(-.15,.15),c(mcond[j],mcond[j]),lwd=2) 

# ANOVA test
topGear$Car.num <-as.factor(as.numeric(topGear$Car.num))
model <- aov(topGear$Time~topGear$Car.num)
sum_model <- summary(model)
sum_model
# complex contracts Kia Cee'd and Suzuki Liana against Chevrolet Lacetti and Vauxhall Astra
mcond=tapply(topGear$Time, topGear$Car.num, mean)
MSerr=sum_model[[1]]["Residuals","Mean Sq"] 
dferr=sum_model[[1]]["Residuals","Df"] 
nc=table(topGear$Car.num)
cc1 =c(-.5,-.5,.5,.5)
g1=sum(cc1*mcond)
seg1 = sqrt(MSerr)*sqrt(sum(cc1^2/nc))
t1=g1/seg1
p1=2*min(pt(t1,dferr), 1-pt(t1,dferr))
p1

# Confidence interval of g
left_side=g1-(qt(0.975,df=dferr)*seg1)
right_side=g1+(qt(0.975, df=dferr)*seg1)
# calculating effect size
SSEff=sum_model[[1]][[1,2]]
MSEfull=sum_model[[1]][[2,2]]/sum_model[[1]][[2,1]]
SSTot=sum_model[[1]][[1,2]] + sum_model[[1]][[2,2]]
R_2=SSEff/SSTot
R_2
w_2=(SSEff-(sum_model[[1]][[1,1]])*MSEfull)/(SSTot+MSEfull)
w_2

# Different conditions violate the independence assumption
table(topGear$Conditions)

# variance across four groups
var(topGear$Time[topGear$Car == "Kia Cee'd"])
var(topGear$Time[topGear$Car == "Suzuki Liana"])
var(topGear$Time[topGear$Car == "Chevrolet Lacetti"])
var(topGear$Time[topGear$Car == "Vauxhall Astra"])

# mean time across four gruops
mean(topGear$Time[topGear$Car == "Chevrolet Lacetti"])
mean(topGear$Time[topGear$Car == "Kia Cee'd"])
mean(topGear$Time[topGear$Car == "Suzuki Liana"])
mean(topGear$Time[topGear$Car == "Vauxhall Astra"])

# create a new column residual
topGear$Residual <- c(
 topGear$Time[topGear$Car == "Kia Cee'd"]-mean(topGear$Time[topGear$Car == "Kia Cee'd"]),
 topGear$Time[topGear$Car == "Suzuki Liana"]-mean(topGear$Time[topGear$Car == "Suzuki Liana"]),  topGear$Time[topGear$Car == "Chevrolet Lacetti"]-mean(topGear$Time[topGear$Car == "Chevrolet Lacetti"]),
 topGear$Time[topGear$Car == "Vauxhall Astra"]-mean(topGear$Time[topGear$Car == "Vauxhall Astra"]))

# 
#hist(topGear$Time, length(topGear$Car))

# a scaltter plot of residual per condition
topGear$Car.num <-as.numeric(topGear$Car.num)
plot(jitter(topGear$Car.num, .2),topGear$Residual,xaxt="n",xlab="Car Name",ylab="Residual",main = "Residual per car type",
     col=rgb(0,0,0,.6),cex=1,pch=16) 
axis(1,at=c(1,2,3,4),labels=c("Kia Cee'd","Suzuki Liana","Chevrolet Lacetti","Vauxhall Astra"))

#histogram plot of residual to cheak the normality assumpution 
hist(topGear$Residual, length(topGear$Car), main="Histogram of Residuals", xlab = "Residuals")

#QQ plot of residual to cheak the normality assumpution
qqnorm(topGear$Time-mean(topGear$Time))
qqline(topGear$Time-mean(topGear$Time))
x <- seq(-5,5,len = 100)
lines(x,x,col="red")

# remove outlier the 40th observation: Damian Lewis 129.1 Kia Cee'd Snow
topGear <- topGear[-c(40), ]
topGear$Car.num  <- as.character(topGear$Car.num)
model <- aov(topGear$Time~topGear$Car.num)
sum_model <- summary(model)
sum_model

# Normal condition only 
topGear$Car.num <-as.numeric(topGear$Car.num)

# subset a dataframe with the Normal condition only 
topGear <- subset(topGear, topGear$Conditions == "Normal")
plot(jitter(topGear$Car.num, .2), topGear$Time,xaxt="n",xlab="Car Name",ylab="Time",main="Time per car type under normal condition only",
     col=rgb(0,0,0,.6),cex=1,pch=16) 
axis(1,at=c(1,2,3,4),labels=c("Kia Cee'd","Suzuki Liana","Chevrolet Lacetti","Vauxhall Astra"))
mcond=tapply(topGear$Time,topGear$Car.num,mean)  
for (j in 1:4) lines((j)+c(-.15,.15),c(mcond[j],mcond[j]),lwd=2) 

# ANOVA for normal condition only
topGear$Car.num <-as.factor(topGear$Car.num)
model <- aov(topGear$Time~topGear$Car.num)
sum_model <- summary(model)
sum_model

# contracts
mcond=tapply(topGear$Time, topGear$Car.num, mean)
MSerr=sum_model[[1]]["Residuals","Mean Sq"] 
dferr=sum_model[[1]]["Residuals","Df"] 
nc=table(topGear$Car.num)
cc1 =c(.5,.5,-.5,-.5)
g1=sum(cc1*mcond)
seg1 = sqrt(MSerr)*sqrt(sum(cc1^2/nc))
t1=g1/seg1
p1=2*min(pt(t1,dferr), 1-pt(t1,dferr))
p1

# exploratory research to group Kia Ceed'd and Vauxhall Astra againt 
# Suzuku Liana and Chevrolet Lacetti 

# read the data file
topGear <- read.csv2("TopGearGroup199.csv")
topGear$Time <- as.numeric(as.character(topGear$Time))
topGear$Car.num  <- as.character(topGear$Car)

# create a new column Car.num using number 1,2,3,4 for four different cars
topGear$Car.num[topGear$Car.num == "Kia Cee'd"] <- 1
topGear$Car.num[topGear$Car.num == "Suzuki Liana"] <- 2
topGear$Car.num[topGear$Car.num == "Chevrolet Lacetti"] <- 3
topGear$Car.num[topGear$Car.num == "Vauxhall Astra"] <- 4

topGear$Car.num <-as.numeric(topGear$Car.num)
topGear$Time <- as.numeric(as.character(topGear$Time))

# ANOVA test
topGear$Car.num <-as.factor(as.numeric(topGear$Car.num))
model <- aov(topGear$Time~topGear$Car.num)
sum_model <- summary(model)
sum_model

# complex contracts Kia Cee'd and Suzuki Liana against Chevrolet Lacetti and Vauxhall Astra
mcond=tapply(topGear$Time, topGear$Car.num, mean)
MSerr=sum_model[[1]]["Residuals","Mean Sq"] 
dferr=sum_model[[1]]["Residuals","Df"] 
nc=table(topGear$Car.num)
cc1 =c(.5,-.5,-.5,.5)
g1=sum(cc1*mcond)
seg1 = sqrt(MSerr)*sqrt(sum(cc1^2/nc))
t1=g1/seg1
p1=2*min(pt(t1,dferr), 1-pt(t1,dferr))
p1

# Confidence interval of g
left_side=g1-(qt(0.975,df=dferr)*seg1)
right_side=g1+(qt(0.975, df=dferr)*seg1)

