### DA Project 2 Linear Discriminant Analysis Model ###

# install/require packages
library(tidyverse)
library(data.world)
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
testData = subset(crashes, day_of_week < 4)

## Linear Discriminant Analysis
lda.fit = lda(rear_end ~ vehicle_count + person_count + maj_inj_count + interstate, data = trainingData)
lda.fit
plot(lda.fit)

# predicitve modeling
lda.pred = predict(lda.fit, testData)
data.frame(lda.pred)[1:5,] # quick view of the df

# visualize the model
df = data.frame(lda.pred)
ggplot(df) + geom_boxplot(mapping = aes(x = class, y = LD1))

# explore the accuracy of the predictive modeling
table(lda.pred$class, testData$rear_end)
mean(lda.pred$class == testData$rear_end)
mean(lda.pred$class != testData$rear_end)

# The following is for the ROC curve discussion:

df1 = dplyr::bind_cols(testData, df) # binding not working!
df1 %>% dplyr::filter(rear_end == class) %>% dplyr::group_by(rear_end) %>% dplyr::summarise(n())
df1 %>% dplyr::filter(rear_end != class) %>% dplyr::group_by(rear_end) %>% dplyr::summarise(n())
df1 %>% dplyr::group_by(class) %>% dplyr::summarise(min(posterior.true), max(posterior.false), n())
df2 = df1 %>% dplyr::mutate(newClass = ifelse(posterior.true > .487, 'Rear-End', 'Not Rear End'))
df2 %>% dplyr::filter(rear_end == newClass) %>% dplyr::group_by(rear_end) %>% dplyr::summarise(n())
df2 %>% dplyr::filter(rear_end != newClass) %>% dplyr::group_by(rear_end) %>% dplyr::summarise(n())
table(lda.pred$class, testData$rear_end)
table(df2$newClass, testData$rear_end)
mean(lda.pred$class == testData$rear_end)


