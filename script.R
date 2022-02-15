rm(list = ls())

# load libraries
packages <- c("caret", "randomForest", "gbm", "janitor", "dplyr", "ggplot2")
sapply(packages, require, character.only=TRUE, quietly=TRUE)

# load trianing and testing datasets
data <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", na.strings = c("#DIV/0!", "NA"))
testing <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", na.strings = c("#DIV/0!", "NA"))

# remove empty columns from testing-dataset
testing <- remove_empty(testing, which = "cols")

# read the columnnames from the testing-dataset in a list
columnnames <- names(testing)

# remove the columname 'problem_id' from the list
columnnames <- columnnames[columnnames != "problem_id"]

# add the columname 'classe' to the list
columnnames <- c(columnnames, "classe")

# create a new dataframe from the selected columnnames
data <- select(data, columnnames)

# remove unnecessary columns from the dataframe
data <- subset(data, select = -c(X, user_name, raw_timestamp_part_1, raw_timestamp_part_2, num_window, new_window, cvtd_timestamp) )
data <- data %>% select(-contains(c('total')))

# convert 'classe'-variable to factor
data$classe <- as.factor(data$classe)

# print the structure of the dataframe
str(data)

# create training and validation-sets
trainIndex <- createDataPartition(data$classe, p=0.8, list=FALSE)
training <- data[ trainIndex,]
validation <- data[-trainIndex,]

set.seed(123)


# define independent variable
outcome <- "classe"

# define predictors
predictors <- c(".")
#variables <- c("roll_belt", "yaw_belt", "pitch_forearm", "magnet_dumbbell_z", "roll_forearm", "magnet_dumbbell_y", "pitch_belt", "magnet_belt_z")

# construct the formula from the outcome and the predictors
formula <- as.formula(
  paste(outcome, 
        paste(predictors, collapse = " + "), 
        sep = " ~ "))

tunegrid <- expand.grid(.mtry=c(2))
control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3)
modelRf <- train(formula, data=training, method="rf", metric='Accuracy', ntree=150, tuneGrid=tunegrid, trControl=control)
modelRf
plot(modelRf, main="Random Forest - Amount of variables randomly selected for treebuilding vs. Accuracy")
plot(modelRf$finalModel, main="Random Forest - Amount of trees vs. Error-rate")
plot(varImp(modelRf), main="Random Forest - Order of importance of used variables")

modelRf$finalModel

predictRf <- predict(modelRf, validation)
confusionMatrix(predictRf,validation$classe)

modelGbm <- train(formula, data=training, method="gbm", 
                  tuneGrid=expand.grid(
                    n.trees=150,
                    interaction.depth=3,
                    shrinkage=.1,
                    n.minobsinnode = 10
                  )
                  , verbose = FALSE)
modelGbm
#plot(modelGbm$finalModel)
plot(varImp(modelGbm),main="Gradient Boosting - Order of importance of used variables")


predictGbm <- predict(modelGbm, validation)
confusionMatrix(predictGbm,validation$classe)

modelRf <- train(classe ~ ., data=training, method="rf")
modelGbm <- train(classe ~ ., data=training, method="gbm", verbose = FALSE)
finalmodelRf <- train(classe ~ ., data=training, method="rf", tuneGrid=tunegrid, ntree=150)
finalmodelRf
finalmodelRf$finalModel
finalmodelGbm <- train(classe ~ ., data=training, method="gbm", 
                  tuneGrid=expand.grid(
                    n.trees=150,
                    interaction.depth=3,
                    shrinkage=.1,
                    n.minobsinnode = 10
                  )
                  , verbose = FALSE)
finalmodelGbm
finalmodelGbm$finalModel