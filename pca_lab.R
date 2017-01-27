# Hello MSBA 2017! 
# For those who use R studio, please update before running this code

#_______________________________________________
## DIRECTORY
## | 1 | Libraries
## | 2 | Linear Regression
## | 3 | ISLR 6.5 Example
## | 4 | Principal Component Analysis Lab
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
## | 2 | Linear Regression
lm_fit = lm(Salary~., data=Hitters)
#summary(lm_fit)
print(coef(lm_fit))
abline(lm_fit, col="black")
plot(lm_fit)

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

# # Here is an additional example
# # Average cost of different grains in England

# #grains = read.csv("england_grains.csv", header=TRUE, row.names=1)
# grains = read.csv("england_grains.csv", header=TRUE)
# grains = grains[complete.cases(grains),]
# grains_melt = melt(grains, id="Year")

# # 2 plot the malt and wheat on the same axis
# # qplot(x=Year, data=grains)
# # ggplot(data=grains_melt),
# #      aes(x=date, y=value, colour=variable)) +
# #       geom_line()

#_______________________________________________
## | 5 | Principal Component Regression Lab
set.seed(1000)
# validation error 
# cumulative percentage of variance explained
pcr_model = pcr(Sepal.Length~., data=iris, scale=TRUE, validation="CV")
summary(pcr_model)
validationplot(pcr_model)
validationplot(pcr_model, val.type="MSEP")
validationplot(pcr_model, val.type="R2")
predplot(pcr_model)
coefplot(pcr_model)
train = iris[1:120,]
y_test = iris[120:150, 1]
test = iris[120:150, 2:5]
pcr_model = pcr(Sepal.Length~., data=train, scale=TRUE, validation = "CV")
pcr_pred = predict(pcr_model, test, ncomp=3)
mean((pcr_pred - y_test)^2)

# build example where even and odd variables are bringing in noisy images
# of two different signals.
set.seed(6587)
mkData <- function(n) {
  for(group in 1:10) {
    # y is the sum of two effects yA and yB
    yA <- rnorm(n)
    yB <- rnorm(n)
    if(group==1) {
      d <- data.frame(y=yA+yB+rnorm(n))
      code <- 'x'
    } else {
      code <- paste0('noise',group-1)
    }
    yS <- list(yA,yB)
    # these variables are correlated with y in group 1,
    # but only to each other (and not y) in other groups
    for(i in 1:5) {
      vi <- yS[[1+(i%%2)]] + rnorm(nrow(d))
      d[[paste(code,formatC(i,width=2,flag=0),sep='.')]] <- ncol(d)*vi
    }
  }
  d
}
# make data
set.seed(23525)
dTrain <- mkData(1000)
dTest <- mkData(1000)
summary(dTrain[, c("y", "x.01", "x.02", "noise1.01", "noise1.02")])

goodVars <-  colnames(dTrain)[grep('^x.',colnames(dTrain))]
dTrainIdeal <- dTrain[,c('y',goodVars)]
dTestIdeal <-  dTrain[,c('y',goodVars)]

# do the PCA
dmTrainIdeal <- as.matrix(dTrainIdeal[,goodVars])
princIdeal <- prcomp(dmTrainIdeal,center = TRUE,scale. = TRUE)

# extract the principal components
rot5Ideal <- extractProjection(5,princIdeal)

# prepare the data to plot the variable loadings
rotfIdeal = as.data.frame(rot5Ideal)
rotfIdeal$varName = rownames(rotfIdeal)
rotflongIdeal = gather(rotfIdeal, "PC", "loading",
                       starts_with("PC"))
rotflongIdeal$vartype = ifelse(grepl("noise", 
                                     rotflongIdeal$varName),
                               "noise", "signal")

# plot the singular values
dotplot_identity(frame = data.frame(pc=1:length(princIdeal$sdev), 
                            magnitude=princIdeal$sdev), 
                 xvar="pc",yvar="magnitude") +
  ggtitle("Ideal case: Magnitudes of singular values")

dotplot_identity(rotflongIdeal, "varName", "loading", "vartype") + 
  facet_wrap(~PC,nrow=1) + coord_flip() + 
  ggtitle("x scaled variable loadings, first 5 principal components") + 
  scale_color_manual(values = c("noise" = "#d95f02", "signal" = "#1b9e77"))

# signs are arbitrary on PCA, so instead of calling predict we pull out
# (and alter) the projection by hand
projectedTrainIdeal <-
  as.data.frame(dmTrainIdeal %*% extractProjection(2,princIdeal),
                                 stringsAsFactors = FALSE)
projectedTrainIdeal$y <- dTrain$y
ScatterHistN(projectedTrainIdeal,'PC1','PC2','y',
               "Ideal Data projected to first two principal components")