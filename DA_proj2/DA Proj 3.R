#### DA Project 3 ####

# import all basic required packages
library(data.world)
library(tidyverse)
library(MASS)
library(ISLR)
library(dplyr)
library(SDSRegressionR)
library(shiny)
library(ggplot2)
library(modelr)
library(rms)
library(class)

# configure data.world and github
devtools::install_github("datadotworld/data.world-r", build_vignettes = TRUE)
data.world::set_config(save_config(auth_token = "eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJwcm9kLXVzZXItY2xpZW50OmphbGV4MTEiLCJpc3MiOiJhZ2VudDpqYWxleDExOjo5NDllZWZhNS03NTdlLTQyNGYtOTY5NS0zODAyNGYxZmRiOWYiLCJpYXQiOjE1MDUzMzY3NjUsInJvbGUiOlsidXNlcl9hcGlfd3JpdGUiLCJ1c2VyX2FwaV9yZWFkIiwidXNlcl9hcGlfYWRtaW4iXSwiZ2VuZXJhbC1wdXJwb3NlIjp0cnVlfQ.w5CB4mB4PZ592KVEHBJlcuFeZiibIXhQ7tMo3OBXUVDLSlZNvNnzD1Gp_JNXDuEh2XVlEiZNtD-oA2BoxYL8JA"))

# import data from data.world
project <- "https://data.world/jalex11/project-3"
math <- data.world::query(
data.world::qry_sql("SELECT * FROM studentMat"), dataset = project)

# require ISLR and run leaps library for regsubsets 
require(ISLR)
library(leaps)

# best model selction algorithm
regfit.full = regsubsets(g3 ~ ., data = math, nvmax = 20)
reg.summary = summary(regfit.full) 
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp")

# simple linear regression model
slReg <- lm(g3 ~ g2, data = math)
summary(slReg)

# multiple linear regression model 
mlReg <- lm(g3 ~ famrel + walc + absences + g1 + g2, data = math)
summary(mReg)

# Logistic Regression 

# logical indexing
math$paidClass <- NA
math$paidClass [math$paid == "true"] <- 1
math$paidClass [math$paid == "false"] <- 0


regfit.full = regsubsets(paidClass ~ .-paid, data = math, nvmax = 10, method = "forward")
summary(regfit.full) 

glm.fit <- glm(paidClass ~ mjob + fjob + reason + guardian + traveltime + studytime + failures + schoolsup + famsup + higher + internet + walc, data = math)
summary(glm.fit)

# odds ratios
exp(glm.fit$coef)
exp(confint.default(glm.fit)) # confidence intervals

logMod <- lrm(paidClass ~ mjob + fjob + reason + guardian + traveltime + studytime + failures + schoolsup + famsup + higher + internet + walc, math, family = "binomial")
logMod

glm.probs=predict(glm.fit, type="response") 
glm.probs[1:5]
glm.pred=ifelse(glm.probs > .5, 1, 0)
mean(glm.probs)

table(glm.pred, math$paidClass) # confusion matrix
mean(glm.pred == math$paidClass) # percentage correctly predicted
mean(glm.pred != math$paidClass) # percentage incorrectly predicted


# create subset for testing/training
trainingData = subset(math, sex = "M")
testData= subset(math, sex = "F")

regfit.full = regsubsets(studytime ~., data = math, nvmax = 10)
summary(regfit.full) 

qda.fit = qda(studytime ~ traveltime + walc + paid + romantic + fedu + g1 + paid + activities + sex, data = trainingData)
qda.fit

qda.class = predict(qda.fit, testData)
table(qda.class$class, testData$studytime)
mean(qda.class$class == testData$studytime)


lda.fit = lda(studytime ~ traveltime + walc + paid + romantic + fedu + g1 + paid + activities + sex, data = trainingData)
lda.fit

lda.pred = predict(lda.fit, testData)
table(lda.pred$class, testData$studytime)
mean(lda.pred$class == testData$studytime)

attach(math)
Xlag = cbind(studytime, traveltime, walc, fedu, g1, g2)
train = math$goout <= 3
knn.pred = knn(Xlag[train,], Xlag[!train,], studytime[train], k = 1)
table(knn.pred, studytime[!train])
mean(knn.pred == studytime[!train])


