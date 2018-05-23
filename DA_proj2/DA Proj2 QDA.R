### DA Project 2 Quadratic Discriminant Analysis Model ###

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

# configure data.world and github
devtools::install_github("datadotworld/data.world-r", build_vignettes = TRUE)
data.world::set_config(save_config(auth_token = "eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJwcm9kLXVzZXItY2xpZW50OmphbGV4MTEiLCJpc3MiOiJhZ2VudDpqYWxleDExOjo5NDllZWZhNS03NTdlLTQyNGYtOTY5NS0zODAyNGYxZmRiOWYiLCJpYXQiOjE1MDUzMzY3NjUsInJvbGUiOlsidXNlcl9hcGlfd3JpdGUiLCJ1c2VyX2FwaV9yZWFkIiwidXNlcl9hcGlfYWRtaW4iXSwiZ2VuZXJhbC1wdXJwb3NlIjp0cnVlfQ.w5CB4mB4PZ592KVEHBJlcuFeZiibIXhQ7tMo3OBXUVDLSlZNvNnzD1Gp_JNXDuEh2XVlEiZNtD-oA2BoxYL8JA"))

# import datasets 
project <- "https://data.world/jalex11/f-17-eda-project-2"
crashes <- data.world::query(
  data.world::qry_sql("SELECT * FROM 2016crashes"),
  dataset = project)

# create subset for testing/training
trainingData = subset(crashes, day_of_week > 3)
testData= subset(crashes, day_of_week < 4)

# Quadratic Discriminant Analysis 
qda.fit = qda(rear_end ~ vehicle_count + person_count + maj_inj_count + interstate, data = trainingData)
qda.fit
#crashes1=subset(crashes, crash_month == 1)
qda.class = predict(qda.fit, testData)
table(qda.class$class, testData$rear_end)
mean(qda.class$class == testData$rear_end)

