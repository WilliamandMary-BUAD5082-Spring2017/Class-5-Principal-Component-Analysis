# To MSBA 2017
# With love, from Group 11
# For those who use R studio, please update before running this code

#_______________________________________________
## DIRECTORY
## | 1 | Libraries
## | 2 | Eigenvectors, Eigenvalues, and Principal Component Analysis
## | 3 | Principal Component Regression Example 1
## | 4 | Principal Component Regression Example 2
## | 5 | Principal Component Regression Lab

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
## | 2 | Eigenvectors, Eigenvalues, and Principal Component Analysis
# courtesy of Professor David Murray

# Approximate Example used in Figure 6.15 Using 1) eigenvalues and eigenvectors and 2) built-in function prcomp()
# to find first principal component values

# Create some data of advertising versus population
set.seed(5082)
n<-30
pop<-rnorm(n,40,10)
eps1<-rnorm(n,0,1.5)
ad<-3 + 0.5*pop + eps1
par(mfrow=c(3,1))
plot(pop,ad,xlim=c(0,70),ylim=c(0,40),main='Raw x-data')
x<-matrix(c(ad,pop),nrow=n,byrow=F)
#Center x
i<-rep(1,n)
x<-x-i %*% t(i) %*% x*(1/n)  #Center

# Derive eigenvalues and eigenvectors
eigen.x<-eigen(cov(x))
(vals<-eigen.x$values)
(vecs<-eigen.x$vectors)

# Compute principal component scores using first eigenvector values (loadings)
z1<-x %*% vecs[,1]    #First PC - this is formula 6.19 - don't need to subtract means of pop and ad since data has been centered
z2<-x %*% vecs[,2]    #Second PC

# Get the same result using built-in function prcomp()
princomp.x<-prcomp(x)
(vectors<-princomp.x$rotation)
z1alt<-x %*% vectors[,1]
z2alt<-x %*% vectors[,2]

# Compare
head(data.frame(z1,z1alt,z2,z2alt))
 
# Plot the projection
plot(z1,rep(0,n),xlim=c(-30,30),ylim=c(-1,1),type='p',axes=F,col='blue',ylab='',main='First Principal Component')
axis(1, pos=0)

plot(z2,rep(0,n),xlim=c(-5,5),ylim=c(-1,1),type='p',axes=F,col='blue',ylab='',main='Second Principal Component')
axis(1, pos=0)



#________________________________________________
## | 3 | Principal Component Regression Example 1
# This example shows a basical example of multiple linear regression which will help us explore Principal Linear Regression

# Explore a multiple linear regression model with all the variables from Hitters
lm_fit = lm(Salary~., data=Hitters)
summary(lm_fit)
apply(Hitters, 2, mean) 
apply(Hitters, 2, var)
print(coef(lm_fit))

# This example comes from Chapter 6.7.1 in ISLR 

# Let's take a look at the PCR function
set.seed(2)
# The pcr() function allows us to standardize and print the cross-validation error for each M
pcr.fit = pcr(Salary~., data=Hitters, scale=TRUE, validation="CV")
# Percentage of variance explained: how much do the number of components explain
summary(pcr.fit)
# Reports the root mean squared error as a plot
validationplot(pcr.fit, val.type="MSEP")

# PCR with test and training sets
set.seed(1)
# creates test and training sets
x = model.matrix(Salary~., Hitters)[,-1]
y = Hitters$Salary
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
y.test = y[test]
pcr.fit = pcr(Salary~., data=Hitters, subset=train, scale=TRUE, validation="CV")
validationplot(pcr.fit, val.type="MSEP")
pcr.pred = predict(pcr.fit, x[test,], ncomp=7)
mean((pcr.pred-y.test)^2)
pcr.fit = pcr(y~x, scale=TRUE, ncomp=7)
summary(pcr.fit)



#________________________________________________
## | 4 | Principal Component Regression Example 2

# This example comes from Chapter 10.4 in ISLR 

states = row.names(USArrests)
str(states)
names(USArrests)
#apply function to columns
apply(USArrests, 2, mean) 
apply(USArrests, 2, var)
# need to scale because UrbanPop in percentages and Assualts is in counts
pr.out = prcomp(USArrests, scale=TRUE)
# understanding the prcomp result
names(pr.out)
# looking at the mean and st. dev after scaling
pr.out$center
pr.out$scale
# principal component loading vector, PC scores
pr.out$rotation
#dim(pr.out$x)
# plot of the two first principal components
biplot(pr.out, scale=0)
#unique up to a sign change?
pr.out$rotation = -pr.out$rotation
pr.out$x = -pr.out$x
biplot(pr.out, scale=0)
# explore the st. devs for principal components
pr.out$sdev
# compute the variance
pr.var = pr.out$sdev^2
pr.var
# proportion of each variance explained by each principal comp
pve = pr.var/sum(pr.var)
pve
# PVE for each component
plot(pve, xlab="Principal Component", ylab= "Proportion of Variance Explained", ylim = c(0,1), type="b")
plot(cumsum(pve), xlab="Principal Component", ylab= "Cumulative Proportion of Variance Explained", ylim = c(0,1), type="b")



#_______________________________________________
## | 5 | Principal Regression Analysis Lab

# Work together with your group members to understand this data set
# Average cost of different grains in England during early 1900s

#grains = read.csv("england_grains.csv", header=TRUE, row.names=1)
# 1. Import the dataset, combine two datasets, and remove NA cases [hint: complete.cases()]
# 2. Use ggplot2 to visualize the data
# 3. Look at the mean and variance across all the columns for the grains
# 4. Create a multiple linear regression model to understand the relationship between England GDP and grain prices
# 5. Use the pcr function to do principal component regression with the grains as variables
# 6. Print a plot showing the MSEP for each component
# 7. Use the prcomp function to do principal component regression
# 8. Reflection questions: 
#   a. What is the grain with the highest price on average?
#   b. Which component number had the smallest mse? 

# 1. Import the dataset and remove NA cases [hint: complete.cases()]
grains = read.csv("england_grains.csv", header=TRUE)
grains = grains[complete.cases(grains),]

# 2. Use ggplot2 to visualize the data
grains_melt = melt(grains, id="Year")

# 2 plot the malt and wheat on the same axis
# qplot(x=Year, data=grains)
# ggplot(data=grains_melt),
#      aes(x=date, y=value, colour=variable)) +
#       geom_line()

# Conduct a principal component regression on the grains dataset
pcr.fit = pcr(Salary~., data=grains, scale=TRUE, validation="CV")