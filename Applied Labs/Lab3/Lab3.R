# Module 3, Lab 1
# from "Data Science and Big Data Analytics" 

###################################################
# Lab 2 Part 1 – Obtain summary statistics for Household Income and visualize data:
###################################################

###################################################
# Step 1: Prepare  working environment for the Lab and load data files
###################################################
setwd("D:/BigData/Labs/LAB01")
options(digits=3) #What is this line doing?
ls()
load(file="Labs.Rdata")
ls()

rm(lab2)
ds <- lab1
colnames(ds) <- c("income", "rooms") 

###################################################
# Step 2: Examine Household Income
###################################################
summary(ds$income)
range(ds$income)
sd(ds$income)
var(ds$income)

plot(density(ds$income))  # right skewed

###################################################
# Step 3:  Examine the Number of Rooms
###################################################
summary(ds$rooms)
range(ds$rooms)
sd(ds$rooms)
plot(as.factor(ds$rooms))

###################################################
# Step 4: Removing Outliers
###################################################
(m <- mean(ds$income, trim=0.10) )

ds <- subset(ds, ds$income  >= 10000 & ds$income < 1000000)
summary(ds)


###################################################
# Step 5: Stratify a Variable - Household Income
###################################################
breaks <- c(0, 23000, 52000, 82000, 250000, 999999)
labels <- c("Poverty", "LowerMid", "UpperMid", "Wealthy", "Rich") 
wealth <- cut(ds$income, breaks, labels)
#Add wealth as a column to ds 
ds <- cbind(ds, wealth)
#Show the 1st few lines.
head(ds)

wt <- table(wealth)
percent <- wt/sum(wt)*100
wt <- rbind(wt, percent)
wt

nt <- table(wealth, ds$rooms)
print(nt)
plot(nt)   #Nice mosaic plot     

rm(wealth,breaks,labels)
save(ds, wt, nt, file="Census.Rdata")

###################################################
# Step 6: Plotting Histograms and Distributions 
###################################################    
library(MASS) #What is this doing?
with(ds, {
  hist(income, main="Distribution of Household Income",   freq=FALSE)
  lines(density(income), lty=2, lwd=2)
  xvals = seq(from=min(income), to=max(income),length=100)
  param = fitdistr(income, "lognormal")
  lines(xvals, dlnorm(xvals, meanlog=param$estimate[1],
          sdlog=param$estimate[2]), col="blue")
} )


#Now try the same thing with log10(income)
logincome = log10(ds$income)
hist(logincome, main="Distribution of Household Income", freq=FALSE)
lines(density(logincome), lty=2, lwd=2)  # line type (lty) 2 is dashed
xvals = seq(from=min(logincome), to=max(logincome), length=100)
param = fitdistr(logincome, "normal")
lines(xvals, dnorm(xvals, param$estimate[1],  param$estimate[2]), 
         lwd=2, col="blue")

###################################################
# Step 7: Compute Correlation between income and number of rooms
###################################################
with(ds, cor(income, rooms))
with(ds, cor(log(income), rooms) ) #This will give a better correlation
n = length(ds$income)
with(ds, cor(runif(n), rooms)) 

###################################################
# Step 8: Create a Boxplot - Distribution of income as a factor of number of rooms
###################################################
boxplot(income ~ as.factor(rooms), data=ds, range=0, outline=F, log="y",
          xlab="# rooms", ylab="Income")

boxplot(rooms ~ wealth, data = ds, main="Room by Wealth", Xlab="Category",
        ylab="# rooms")

###################################################
# Step 9: Exit R
###################################################
#If time permits, please continue to Part 2 and skip the following line
q()

