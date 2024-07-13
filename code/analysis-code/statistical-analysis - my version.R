###############################

#required libraries
library(ggplot2) #for plotting
library(broom) #for cleaning up output from lm()
library(here) #for data loading/saving
library(jmv) # for performing MANCOVA
library(earth)
library(tidyr)
library(dplyr)
library(caret)
library(knitr)



#loading data. 
data_location <- here::here("data","processed-data","processeddata4.rds")
mydata <- readRDS(data_location)
summary(mydata)


#Basic Statistical Analysis
#Examining a couple logistic regression tests with two variables of interest


logitAge = glm(y_termSubscribed~age, data = mydata, family = "binomial")
summary(logitAge)

lrtable1 <- broom::tidy(logitAge)
basic_model1 = here("results", "tables", "basicmodel1.rds")
saveRDS(lrtable1, file = basic_model1)

logitJob = glm(y_termSubscribed~job, data=mydata, family = "binomial")
summary(logitJob)

lrtable2 <- broom::tidy(logitJob)
basic_model2 = here("results", "tables", "basicmodel2.rds")
saveRDS(lrtable2, file = basic_model2)

#Looks like both of the above predictors are significant


#Full Data Analysis


######################################
#Machine Learning Models Part 1
######################################
#Using Numeric Unfactored Variables
############################


#Setting Tran Control
ctrl <- trainControl(method = "cv")

#Splitting the data. 80% for training, 20% test.

set.seed(21)
split1 = sample(c(rep(0, 0.8 * nrow(mydata)), rep(1, 0.2*nrow(mydata))))


train_data = mydata[split1 == 0, ]
test_data = mydata[split1 == 1, ]

train_y = train_data$y_termSubscribed
test_y = test_data$y_termSubscribed


train_x = train_data %>% 
  select(!y_termSubscribed)

test_x = test_data %>% 
  select(!y_termSubscribed)


#nearZeroVar(d2, saveMetrics = TRUE)

#nzv(d2)




#################
#Modeling Section

# MARS model
set.seed(21)
marsmodel = train(x= train_x, y=train_y, 
                  method = "earth",
                  tuneGrid = expand.grid(degree=1, nprune=2:15),
                  trControl = ctrl)

marsmodel

plot(marsmodel)

marsImp = varImp(marsmodel, scale = FALSE)
plot(marsImp)


testResults = data.frame(obs=test_y)
testResults$MARS = predict(marsmodel, test_x)



### KNN 
set.seed(21)


knnModel = train(x=train_x, y=train_y, 
                 method = "knn",
                 preProc = c("center", "scale"),
                 tuneGrid = data.frame(k=1:20),
                 trControl= ctrl)


knnModel
plot(knnModel)

testResults$KNN = predict(knnModel, test_x)


#KNNImp = varImp(knnModel, scale = FALSE)
#plot(KNNImp)


#Logistic Regression Model

logregmodel = glm(y_termSubscribed~., data=train_data, family = "binomial")


summary(logregmodel)


logpred = predict(logregmodel, test_x, type = "response")
logpred1 = ifelse(logpred>0.5, 1, 0)
testResults$Logreg = logpred1






logpred2 = as.factor(logpred1)

cm_lr = confusionMatrix(logpred2, test_y)

cm_lr








performance1 = data.frame(MARS = postResample(pred = testResults$MARS,obs=test_y),
           KNN = postResample(pred = testResults$KNN,obs=test_y),
           LogReg = postResample(pred = testResults$Logreg,obs=test_y))






perftable = performance1

table_file1 = here("results", "tables", "perftable1.rds")
saveRDS(perftable, file = table_file1)


######################################
#Machine Learning Models Part 2
######################################
#Using Numeric FACRORED Variables
############################

data_location <- here::here("data","processed-data","processeddata3.rds")

#loading data. 
mydata <- readRDS(data_location)
summary(mydata)

#Splitting the data. 80% for training, 20% test.

set.seed(21)
split1 = sample(c(rep(0, 0.8 * nrow(mydata)), rep(1, 0.2*nrow(mydata))))


train_data = mydata[split1 == 0, ]
test_data = mydata[split1 == 1, ]

train_y = train_data$y_termSubscribed
test_y = test_data$y_termSubscribed


train_x = train_data %>% 
  select(!y_termSubscribed)

test_x = test_data %>% 
  select(!y_termSubscribed)


#nearZeroVar(d2, saveMetrics = TRUE)

#nzv(d2)


#################
#Modeling Section



# MARS model
set.seed(21)
marsmodel = train(x= train_x, y=train_y, 
                  method = "earth",
                  tuneGrid = expand.grid(degree=1, nprune=2:15),
                  trControl = ctrl)

marsmodel

plot(marsmodel)

marsImp = varImp(marsmodel, scale = FALSE)
plot(marsImp)

file_path <- here("results", "figures","importantMM2.png")
png(filename = file_path, width = 800, height = 600)
plot(marsImp)
dev.off()


testResults = data.frame(obs=test_y)
testResults$MARS = predict(marsmodel, test_x)



### KNN 
set.seed(21)


knnModel = train(x=train_x, y=train_y, 
                 method = "knn",
                 preProc = c("center", "scale"),
                 tuneGrid = data.frame(k=1:20),
                 trControl= ctrl)


knnModel
plot(knnModel)

testResults$KNN = predict(knnModel, test_x)


KNNImp = varImp(knnModel, scale = FALSE)
plot(KNNImp)

file_path <- here("results", "figures","importantKNN2.png")
png(filename = file_path, width = 800, height = 600)
plot(KNNImp)
dev.off()


#Logistic Regression Model

logregmodel = glm(y_termSubscribed~., data=train_data, family = "binomial")


summary(logregmodel)


logpred = predict(logregmodel, test_x, type = "response")
logpred1 = ifelse(logpred>0.5, 1, 0)
testResults$Logreg = logpred1






logpred2 = as.factor(logpred1)

cm_lr = confusionMatrix(logpred2, test_y)

cm_lr


performance2 = data.frame(MARS = postResample(pred = testResults$MARS,obs=test_y),
           KNN = postResample(pred = testResults$KNN,obs=test_y),
           LogReg = postResample(pred = testResults$Logreg,obs=test_y))

perftable = performance2

table_file2 = here("results", "tables", "perftable2.rds")
saveRDS(perftable, file = table_file2)


