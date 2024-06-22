###############################

#required libraries
library(ggplot2) #for plotting
library(broom) #for cleaning up output from lm()
library(here) #for data loading/saving
library(jmv) # for performing MANCOVA

data_location <- here::here("data","processed-data","processeddata1.rds")

#loading data. 
mydata <- readRDS(data_location)


######################################
#Data fitting/statistical analysis
######################################

############################
#### First model fit
# fit linear model using balance as outcome, age as predictor

lmfit1 <- lm(balance ~ y_termSubscribed, mydata)  

# place results from fit into a data frame with the tidy function
lmtable1 <- broom::tidy(lmfit1)

#look at fit results
print(lmtable1)
summary(lmfit1)
# save fit results table  
table_file1 = here("results", "tables", "resulttable1.rds")
saveRDS(lmtable1, file = table_file1)


#Fitting several different linear models to explore the relationships among variables

lm(y_termSubscribed ~ education, data = mydata) %>% 
  summary()
#Significant, R^2: .5%

lm(y_termSubscribed ~ job, data = mydata) %>% 
  summary()
#Significant, R^2: 1.8%

lm(y_termSubscribed ~ day, data = mydata) %>% 
  summary()
#Significant, R^2: 1.2%

lm(y_termSubscribed ~ age, data = mydata) %>% 
  summary()
#Significant, R^2: .06%

lm(y_termSubscribed ~ month, data = mydata) %>% 
  summary()
#Significant, R^2: 6.75%
#Largest R^2 so far!

lm(y_termSubscribed ~ duration, data = mydata) %>% 
  summary()
#Significant, R^2 is at 15%
#Duration was noted as a predictor that could lead to overfitting. 
#We will most likely not use this one


#Trying Multi-Variable Linear Models

lm(y_termSubscribed ~ age + job + education + day + month, data = mydata) %>% 
  summary()
#Significant, R^2: 8.6%


### Logistic Regression
mylogit = glm(y_termSubscribed ~ job + education, data=mydata, family="binomial")
summary(mylogit)

logittable1 <- broom::tidy(mylogit)

table_file2 = here("results", "tables", "resulttable4.rds")
saveRDS(logittable1, file = table_file2)

#Factors that stand out: For jobs, it looks like 'retired', 'student', and 'unemployed' lead to 
#greater odds of subscribing to a deposit.
#For education, 'secondary', 'tertiary', and 'unknown' all have better odds than 'primary'


glm(y_termSubscribed ~ housing + loan + default, data=mydata, family="binomial") %>% 
  summary()

#Factors that stand out: Having a housing loan, personal loan, or defaulting previously all had 
#a negative effect on the odds of a customer subscribing to a term deposit



#I may explore manova/mancova as well. TBD






