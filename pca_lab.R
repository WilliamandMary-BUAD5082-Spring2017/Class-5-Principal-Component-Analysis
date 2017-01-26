# Hello MSBA 2017! 
# For those who use R studio, please update before running this code

#_______________________________________________
## DIRECTORY
## | 1 | Libraries
## | 2 | Linear Regression
## | 3 | ISLR 6.5 Example
## | 4 | Principal Component Analysis Lab

#_______________________________________________
## | 1 | Libraries

# Make sure you have these packages before going through the example
rm(list=ls())
#install.packages("pls")
#install.packages("ggplot2")
require(ISLR)
library(pls)
library(ggplot2)
library(reshape2)

#_______________________________________________
## | 2 | Linear Regression

#_______________________________________________
## | 3 | ISLR 6.5 Example

# This example comes from Chapter 6 in ISLR 
set.seed(2)
# standardizing, cross-validation error for each M
pcr.fit = pcr(Salary~., data=Hitters, scale=TRUE, validation="CV")
summary(pcr.fit)
# reports the root mean squared error
validationplot(pcr.fit, val.type="MSEP")

set.seed(1)
# creates test and training sets
x = model.matrix(Salary~., Hitters)[,-1]
y = Hitters$Salary
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
y.test = y[test]
pcr.fit = pcr(Salary~., data=Hitters, subset=train, scale=TRUE, validation="CV")
validationplot(pcr.fit, val.type="MSEP")



states = row.names(USArrests)
states
str(USArrests)
#apply function to columns
apply(USArrests, 2, mean) 
apply(USArrests, 2, var)
# need to scale because assualt is a lot stronger
pr.out = prcomp(USArrests, scale=TRUE)
#names(pr.out)
pr.out
pr.out$center
pr.out$rotation
dim(pr.out$x)
biplot(pr.out, scale=0)
pr.out$rotation = -pr.out$rotation
pr.out$x = -pr.out$x
biplot(pr.out, scale=0)
pr.out$sdev
pr.var = pr.out$sdev^2
pr.var
pve = pr.var/sum(pr.var)
pve
plot(pve, xlab="Principal Component", ylab= "Proportion of Variance Exmplained", ylim = c(0,1), type="b")

#_______________________________________________
## | 4 | Lab

# Here is an additional example
# Average cost of different grains in England

#grains = read.csv("england_grains.csv", header=TRUE, row.names=1)
grains = read.csv("england_grains.csv", header=TRUE)
grains = grains[complete.cases(grains),]
grains_melt = melt(grains, id="Year")

# 2 plot the malt and wheat on the same axis
# qplot(x=Year, data=grains)
# ggplot(data=grains_melt),
#      aes(x=date, y=value, colour=variable)) +
#       geom_line()
