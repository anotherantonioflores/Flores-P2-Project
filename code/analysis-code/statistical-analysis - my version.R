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
# fit linear model using height as outcome, weight as predictor

lmfit1 <- lm(Height ~ Weight, mydata)  

# place results from fit into a data frame with the tidy function
lmtable1 <- broom::tidy(lmfit1)

#look at fit results
print(lmtable1)

# save fit results table  
table_file1 = here("results", "tables", "resulttable1.rds")
saveRDS(lmtable1, file = table_file1)

############################
#### Second model fit
# fit linear model using height as outcome, weight and gender as predictor

lmfit2 <- lm(Height ~ Weight + Gender, mydata)  

# place results from fit into a data frame with the tidy function
lmtable2 <- broom::tidy(lmfit2)

#look at fit results
print(lmtable2)

# save fit results table  
table_file2 = here("results", "tables", "resulttable2.rds")
saveRDS(lmtable2, file = table_file2)

  