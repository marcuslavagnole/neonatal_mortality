## Train the algorithms
### Algorithms: 'glmnet','lasso','logistic','xgbTree','ranger'
methods1  <- c('glmnet','lasso','logistic','xgbTree','ranger')
mod_train1<- trainModels(data_balance,methods1)
### Algorithms: 'mlp','mlpML'
#### Adjust train sample for neural networks models
adjust_character <- c('sinasc_GRAVIDEZ','sinasc_PARTO','sinasc_ESCMAE',
                      'sinasc_IDANOMAL','sinasc_RACACORMAE','sinasc_ESTCIVMAE',
                      'sinasc_munResUf')
for(j in adjust_character){
  data_balance[,j] <- gsub("[^[:alnum:]]", "",data_balance[,j])
  data_balance[,j] <- as.factor(iconv(data_balance[,j],to="ASCII//TRANSLIT"))
}
#### Train neural networks models
methods2 <- c('mlp','mlpML')
mod_train2 <- trainModels(data_balance,methods2)
### Final object
mod_train <- append(mod_train1,mod_train2)
saveRDS(mod_train,file='aux_obj/train_models.rds')
### Clean the environment
rm(list = c('data_balance','mod_train1','mod_train2'))


## Make predictions
fitted_models <- c(methods1,methods2)
### Adjusttest sample for neural networks models
adjust_character <- c('sinasc_GRAVIDEZ','sinasc_PARTO','sinasc_ESCMAE',
                      'sinasc_IDANOMAL','sinasc_RACACORMAE','sinasc_ESTCIVMAE',
                      'sinasc_munResUf')
test_sample_x <- partition_aux[['Xtest']]
for(j in adjust_character){
  test_sample_x[,j] <- gsub("[^[:alnum:]]", "",test_sample_x[,j])
  test_sample_x[,j] <- as.factor(iconv(test_sample_x[,j],to="ASCII//TRANSLIT"))
}
### Make predictions for the test sample considering the probabilities
pred_model_alt <- list()
for(i in fitted_models){
  if(i %in% c('mlp','mlpML')){
    pred_model_alt[[i]] <- predict(mod_train[[i]],test_sample_x,type = "prob")
  } else{
    pred_model_alt[[i]] <- predict(mod_train[[i]],partition_aux[['Xtest']],type = "prob")
  }
}
pred_model_aux <- list()
for(j in fitted_models){
  if(i %in% c('mlp','mlpML')){
    pred_model_aux[[j]] <- cbind(pred_model_alt[[j]],as.character(partition_aux[['Ytest']]$y),test_sample_x)
  } else{
    pred_model_aux[[j]] <- cbind(pred_model_alt[[j]],as.character(partition_aux[['Ytest']]$y),partition_aux[['Xtest']])
  }
}
### Sort by risk of death
pred_model <- list()
for(k in fitted_models){
  pred_model[[k]] <- pred_model_aux[[k]][order(pred_model_aux[[k]][,2],decreasing=TRUE),]
}
saveRDS(pred_model,file='aux_obj/test_models.rds')
### Clean the environment
rm(list=c('pred_model_alt','pred_model_aux'))
`