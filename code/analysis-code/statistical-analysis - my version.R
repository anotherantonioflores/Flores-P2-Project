###############################

#required libraries
library(ggplot2) #for plotting
library(broom) #for cleaning up output from lm()
library(here) #for data loading/saving

data_location <- here::here("data","processed-data","processeddata1.rds")

#loading data. 
mydata <- readRDS(data_location)


######################################
#Data fitting/statistical analysis
######################################

############################
#### First model fit
# fit linear model using balance as outcome, age as predictor

lmfit1 <- lm(balance ~ age, mydata)  

# place results from fit into a data frame with the tidy function
lmtable1 <- broom::tidy(lmfit1)

#look at fit results
print(lmtable1)
summary(lmfit1)
# save fit results table  
table_file1 = here("results", "tables", "resulttable1.rds")
saveRDS(lmtable1, file = table_file1)
