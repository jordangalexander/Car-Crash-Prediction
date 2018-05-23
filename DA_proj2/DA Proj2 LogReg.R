### DA Project 2 Logistic Regression Model ###

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

# logical indexing to modify existing data frame
crashes$newRearEnd <- NA
crashes$newRearEnd [crashes$rear_end == "true"] <- 1
crashes$newRearEnd [crashes$rear_end == "false"] <- 0

glm.fit <- glm(newRearEnd ~ vehicle_count + person_count + maj_inj_count + interstate, data = crashes)
summary(glm.fit)

# odds ratios
exp(glm.fit$coef)
exp(confint.default(glm.fit))

# predictive modeling
logMod <- lrm(newRearEnd ~ vehicle_count + person_count + maj_inj_count + interstate, crashes)
logMod
glm.probs=predict(glm.fit,type="response") 
glm.probs[1:5]
glm.pred=ifelse(glm.probs > 0.48, 1, 0)
mean(glm.probs)

# exploring model accuracy
table(glm.pred, crashes$newRearEnd) # confusion matrix
mean(glm.pred == crashes$newRearEnd) # percentage correctly predicted
mean(glm.pred != crashes$newRearEnd) # percentage incorrectly predicted

