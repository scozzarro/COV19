#######################################################################
# CO19 Project 
# 24 May 2020
# Gabrielmaria Scozzarro
#    This project born during the Christmas holidays to asses which was the most effective
#    gov measures during the pandemic and its effects on the new cases, deaths and recovered.
#######################################################################
library(tidyverse)
library(caret)
library(purrr)

# 1.0 New Cases prediction

# 1.1 Splitting Data in train and validation ----
val_index<- createDataPartition(Ita_gov_health$New_Cases, p = 0.2, list = FALSE)

validation<- Ita_gov_health[val_index,-c(3, 4)] #remove deaths and recovered data 

train<- Ita_gov_health[-val_index,-c(3,4)] #remove deaths and recovered data
train<- train[,-c(1,2)]

nzv <- nearZeroVar(train, saveMetrics= TRUE)
nzv

train<- train[,-which(colnames(train) == "Amendments to funeral and burial regulations")]


