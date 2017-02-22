#MODEL BUILDING

#Random forest cross-validation function
cross_validation_rf<-function(data,fold)
{
  
  myControl <- trainControl(
    method = "cv", 
    number = 10,
    repeats = 10, 
    verboseIter = TRUE
  )
  
  m<-(100/fold) 
  x<-NA
  for(i in 1:fold)
  {
    smp_size <- floor((0.01*(100-m)) * nrow(data))
    train_ind <- sample(seq_len(nrow(data)), size = smp_size)
    train <- data[train_ind, ]
    test <- data[-train_ind, ]
    rf_model <- train(
      Survived ~  agegroup + Pclass + Sex  + Parch + Fare + Embarked +Title + Family ,
      tuneGrid = data.frame(mtry = c(6,8)),
      data = train, 
      method = "ranger", 
      trControl = myControl,
      importance = 'impurity'
    )
    predict<-predict(rf_model,test)
    conf<-confusionMatrix(predict,test$Survived)
    x[i]<-conf$overall[1]
  }
  return(x)
}



#Cross validation - glmnet
cross_validation_glmnet<-function(data,fold)
{
  
  myControl <- trainControl(
    method = "cv", 
    number = 10,
    repeats = 10, 
    verboseIter = TRUE
  )
  
  m<-(100/fold) 
  x<-NA
  for(i in 1:fold)
  {
    smp_size <- floor((0.01*(100-m)) * nrow(data))
    train_ind <- sample(seq_len(nrow(data)), size = smp_size)
    train <- data[train_ind, ]
    test <- data[-train_ind, ]
    glm_model <- train(
      Survived ~  agegroup + Pclass + Sex + Parch + Embarked + Title + Family + faregroup + Mother + SibSp,
      method = "glmnet",
      tuneGrid = expand.grid(alpha = 0:1,
                             lambda = seq(0.0001, 1, length = 20)),
      data = train,
      trControl = myControl
    )
    predict<-predict(glm_model,test)
    conf<-confusionMatrix(predict,test$Survived)
    x[i]<-conf$overall[1]
  }
  return(x)
}

#agegroup + Pclass + Sex  + Parch + Fare + Embarked +Title + Family 

#GLMNET Model on complete training dataset

myControl <- trainControl(
  method = "cv", 
  number = 10,
  repeats = 10, 
  verboseIter = TRUE
)


glm_model <- train(
  Survived ~  Age + agegroup+ Pclass + Sex + Parch + Embarked + Title + Family + Fare + faregroup+ Mother + SibSp ,
  method = "glmnet",
  tuneGrid = expand.grid(alpha = 0:1,
                         lambda = seq(0.0001, 1, length = 20)),
  data = titanic_train,
  trControl = myControl
)


#Predicting from glmnet model
titanic_test$Survived<-NULL
my_prediction<-predict(glm_model,titanic_test)
titanic_test$Survived<-my_prediction

# Create a data frame with two columns: PassengerId & Survived where Survived contains my predictions.
submission_glm <- data.frame(PassengerID = titanic_test$PassengerId, Survived = titanic_test$Survived)

# Write the solution to a csv file 
write.csv(submission_glm, file = "C:/Users/rohit/Desktop/Spring' 16/Kaggle/Titanic/submission_glm2.csv", row.names = FALSE)


