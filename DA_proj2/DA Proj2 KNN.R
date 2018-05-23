# K Nearest Neighbors Model
# install/require packages
library(data.world)
library(tidyverse)
require(MASS)
require(ISLR)
require(dplyr)
library(SDSRegressionR)
library(ggplot2)
library(modelr)
library(rms)
library(class)

# logical indexing to create and modify new variables
crashes$newInterstate <- NA
crashes$newInterstate [crashes$interstate == "true"] <- 1
crashes$newInterstate [crashes$interstate == "false"] <- 0

attach(crashes)
Xlag = cbind(crash_crn, vehicle_count, person_count, maj_inj_count, newInterstate)
train = crashes$day_of_week > 4
knn.pred = knn(Xlag[train,], Xlag[!train,], rear_end[train], use.all = TRUE)
mean(knn.pred == rear_end[!train])

