# Data set ----------------------------------------------------------------
dropPredictors <- function(matrix_x, dic_vars){
  dataset_aux <- as.data.frame(matrix_x)
  
  nom_vars <- NULL
  ord_vars <- NULL
  num_vars <- NULL
  for(i in 1:dim(dataset_aux)[2]){
    if(dic_vars[which(dic_vars[,1] == colnames(dataset_aux)[i]),2] == "num"){
      dataset_aux[,i] <- as.numeric(dataset_aux[,i])
      num_vars <- c(num_vars,colnames(dataset_aux)[i])
    } else if(dic_vars[which(dic_vars[,1] == colnames(dataset_aux)[i]),2] == "ord"){
      dataset_aux[,i] <- factor(dataset_aux[,i])
      ord_vars <- c(ord_vars,colnames(dataset_aux)[i])
    } else if(dic_vars[which(dic_vars[,1] == colnames(dataset_aux)[i]),2] == "nom"){
      dataset_aux[,i] <- factor(dataset_aux[,i])
      nom_vars <- c(nom_vars,colnames(dataset_aux)[i])
    }
  }
  
  col_to_remove <- which(sapply(dataset_aux,function(x){
      # Drop variables if more than 10% of the observations are missing
      sum(is.na(x))/length(x) > 0.10 |
      # Drop factors with only one level
      ifelse(is.factor(x)==TRUE,length(levels(droplevels(x))),1000) < 2 |
      # Dropping predictors that are the same for all observations
      length(unique(na.omit(x))) == 1 |
      # Dropping predictors that are nearly uniform (uninformative)
      any(table(x)/length(x) > 0.99)
  }   
  ))
  
  pred_result <- list()
  if(length(col_to_remove) == 0){
    pred_result[['matrix_x']] <- dataset_aux
    pred_result[['nom_vars']] <- nom_vars
    pred_result[['ord_vars']] <- ord_vars
  } else{
    pred_result[['matrix_x']] <- dataset_aux[-col_to_remove]
    pred_result[['nom_vars']] <- as.character(na.omit(nom_vars[which(nom_vars != colnames(dataset_aux)[col_to_remove])]))
    pred_result[['ord_vars']] <- as.character(na.omit(ord_vars[which(ord_vars != colnames(dataset_aux)[col_to_remove])]))
  }
    
  return(pred_result)
}

# Method name: partitionData (random split)
partition <- function(y, matrix_x, perc) {
  # Using caret to partition data and build data frames for training and testing
  set.seed(100)
  index          <- createDataPartition(y, p=perc, list=FALSE)
  
  list_partition <- list()
  # Get training data
  list_partition[['Ytrain']] <- data.frame(y = y[index])
  list_partition[['Xtrain']] <- matrix_x[index,]
  # Get testing data
  list_partition[['Ytest']] <- data.frame(y = y[-index])
  list_partition[['Xtest']] <- matrix_x[-index,]

  return(list_partition)
}

# Method name: partitionData (time split)
partition_date <- function(y, matrix_x, date, perc) {
  # Using caret to partition data and build data frames for training and testing
  set.seed(100)
  date_aux       <- paste0(substr(date,1,4),substr(date,6,7))
  table_aux      <- cumsum(table(date_aux))/length(date_aux)
  index_aux      <- Closest(table_aux,perc,which=TRUE)
  date_list      <- sort(unique(date_aux))[1:index_aux]
  index          <- which(date_aux%in%date_list)
  
  list_partition <- list()
  # Get training data
  list_partition[['Ytrain']] <- data.frame(y = y[index])
  list_partition[['Xtrain']] <- matrix_x[index,]
  # Get testing data
  list_partition[['Ytest']] <- data.frame(y = y[-index])
  list_partition[['Xtest']] <- matrix_x[-index,]
  
  return(list_partition)
}

# Modeling ----------------------------------------------------------------

# Use caret to find the best models using cross-validation and MSE as the selection criterion
# Save probability histograms for all non-KRLS models
trainModels <- function(data_set, algorithms) {
  # Control function to use for machine learning
  myControl <- trainControl(method="cv",number = 8,
                            verboseIter = TRUE,classProbs = TRUE
                            #preProcOptions = list(scale=FALSE),
                            #summaryFunction = my_metric
                            )
  models <- list()
  # Train all the models
  for (a in algorithms) {
    if (a == 'ranger'){
      models[[a]] <- train(y ~ ., data = data_set,
                           method = a,
                           maximize = FALSE,
                           preProcess = c("center","scale","zv","nzv","corr"),
                           trControl = myControl,
                           tuneGrid = expand.grid(mtry = 5,
                                                  #max.depth = 5,
                                                  #num.trees = 1000,
                                                  splitrule = "gini",
                                                  min.node.size = 1
                                                  )
                           )
    } else if (a == 'xgbTree'){
      models[[a]] <- train(y ~ ., data = data_set,
                           method = a,
                           maximize = FALSE,
                           preProcess = c("center","scale","zv","nzv","corr"),
                           trControl = myControl,
                           tuneGrid = expand.grid(nrounds=250, 
                                                  max_depth=4,
                                                  eta=0.3, gamma=0, 
                                                  colsample_bytree=c(0.6,1), 
                                                  min_child_weight=1, 
                                                  subsample=c(0.5,1)),
                           verbosity = 0
                           )
    } else if (a == 'mlpML'){
      models[[a]] <- train(y ~ ., data = data_set,
                           method = a,
                           preProcess = c("center","scale","zv","nzv","corr"),
                           trControl = myControl,
                           tuneGrid = expand.grid(
                             layer1 = 25,
                             layer2 = 50,
                             layer3 = 25)
                           )
    } else if (a == 'mlp'){
      models[[a]] <- train(y ~ ., data = data_set,
                           method = a,
                           preProcess = c("center","scale","zv","nzv","corr"),
                           trControl = myControl,
                           tuneGrid = expand.grid(size = 25)
      )
    } else if (a == 'lasso'){
      models[[a]] <- train(y ~ . + .^2, data = data_set,
                           method = 'glmnet',
                           maximize = FALSE,
                           preProcess = c("center","scale","zv","nzv","corr"),
                           trControl = myControl,
                           tuneGrid = expand.grid(alpha=0,lambda=0.001)
      )
    } else if (a == 'logistic'){
      models[[a]] <- train(y ~ . + .^2, data = data_set,
                           method = 'glmnet',
                           maximize = FALSE,
                           preProcess = c("center","scale","zv","nzv","corr"),
                           trControl = myControl,
                           tuneGrid = expand.grid(alpha=0,lambda=0)
      )
    } else {
      models[[a]] <- train(y ~ . + .^2, data = data_set,
                           method = a,
                           maximize = FALSE,
                           preProcess = c("center","scale","zv","nzv","corr"),
                           trControl = myControl,
                           tuneGrid = expand.grid(alpha=0.5,lambda=0.001)
                           #tuneLength=5
                           )
    }
  }
  return(models)
}

dataBalance <- function(data_set,pct){
  #n_death    <- sum(data_set$y == 'dead')
  n_death    <- sum(data_set$y == 'avoidable_death' |
                    data_set$y == 'unavoidable_death')
  num_sample <- n_death * (1-pct)/pct
  
  data_alive   <- sample_n(data_set[which(data_set$y == 'alive'),],num_sample)
  data_balance <- rbind(data_alive,data_set[which(data_set$y == 'avoidable_death' |
                                                  data_set$y == 'unavoidable_death'),])

  return(data_balance)
}