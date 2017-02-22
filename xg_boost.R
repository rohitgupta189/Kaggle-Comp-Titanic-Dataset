
xg_data<-data

xg_data$Pclass<-as.numeric(xg_data$Pclass)
xg_data$Parch<-as.numeric(xg_data$Parch)
xg_data$SibSp<-as.numeric(xg_data$SibSp)
xg_data$FamilyID<-as.numeric(xg_data$FamilyID)

xg_data$Sex_num<-ifelse(xg_data$Sex=="male",1,0)
xg_data$embarked_num<-ifelse(xg_data$Embarked=="C",1,ifelse(xg_data$Embarked=="Q",2,3))
xg_data$title_num<-ifelse(xg_data$Title=="Miss",1,ifelse(xg_data$Title=="Mr",2,ifelse(xg_data$Title=="Mrs",3,4)))
xg_data$family_num<-ifelse(xg_data$Family=="Single",1,ifelse(xg_data$Family=="Mid Family",2,3))
xg_data$agegroup_num<-ifelse(xg_data$agegroup=="less than 5",1,ifelse(xg_data$agegroup=="5-55",2,3))
xg_data$mother_num<-ifelse(xg_data$Mother=="Mother",1,2)

xg_train<-xg_data[xg_data$ind=="Train",]


xg_train<-xg_train[,c("PassengerId","Survived","Age","Pclass","Sex_num","Fam_size",
                    "embarked_num","title_num","Fare","mother_num","FamilyID")]


cross_validation_xgboost<-function(data,fold)
{
  
  m<-(100/fold) 
  x<-NA
  xgb_params = list(
    
    colsample_bytree = 0.4,
    subsample = 0.8,
    eta = 0.01, 
    objective = 'binary:logistic',
    max_depth =9 ,
    alpha = 1,
    gamma = 2,
    min_child_weight = 1
  )
  for(i in 1:fold)
  {
    smp_size <- floor((0.01*(100-m)) * nrow(data))
    train_ind <- sample(seq_len(nrow(data)), size = smp_size)
    train_ind <- sample(seq_len(nrow(xg_train)), size = smp_size)
    
    train <- xg_train[train_ind, ]
    train1<-train
    train<- as.matrix(train, rownames.force=NA)
    train <- as(train, "sparseMatrix")
    dtrain <- xgb.DMatrix(data=train[,-c(1,2)],label=train[,2])
    test <- xg_train[-train_ind, ]
    test1<-test
    test<- as.matrix(test, rownames.force=NA)
    test <- as(test, "sparseMatrix")
    dtest <- xgb.DMatrix(data=test[,-c(1,2)],label=test[,2])
    
    best_n_rounds=2000 # try more rounds
    
    #train data
    gb_dt=xgb.train(xgb_params,dtrain,nrounds = as.integer(best_n_rounds))
    
    #Predict using xgboost
    pred<-predict(gb_dt,dtest)
    
    perc_62<-quantile(pred,prob=0.62,na.rm=T)
    pred_buck<-ifelse(pred>perc_62,1,0)
    
    match_ind<-ifelse(test1$Survived==pred_buck,1,0)
    match<-sum(match_ind)
    total<-nrow(test1)
    
    accuracy<-match/total
    
    x[i]<-accuracy
  }
  return(x)
}
















