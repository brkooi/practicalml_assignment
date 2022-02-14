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

# add the columname 'classe' from the list
columnnames <- c(columnnames, "classe")

# 
data <- select(data, columnnames)

data <- subset(data, select = -c(X, user_name, raw_timestamp_part_1, raw_timestamp_part_2, num_window, new_window, cvtd_timestamp) )
data <- data %>% select(-contains(c('total')))

# convert 'classe'-variable to factor
data$classe <- as.factor(data$classe)

str(data)

# create training and validation-sets
trainIndex <- createDataPartition(data$classe, p=0.8, list=FALSE)
training <- data[ trainIndex,]
validation <- data[-trainIndex,]

set.seed(33833)

# specifications of how to model,
# coming from somewhere else
outcome <- "classe"
variables <- c(".")
#variables <- c("roll_belt", "yaw_belt", "pitch_forearm", "magnet_dumbbell_z", "roll_forearm", "magnet_dumbbell_y", "pitch_belt", "magnet_belt_z")

# our modeling effort, 
# fully parameterized!
formula <- as.formula(
  paste(outcome, 
        paste(variables, collapse = " + "), 
        sep = " ~ "))

tunegrid <- expand.grid(.mtry=c(floor(sqrt(ncol(training))),25,50))
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
