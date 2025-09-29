## Set working directory to file location and load utils
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

## Inputs to the analysis
model <- 'xgbTree'
intervals  <- seq(0.0,1,0.05)
pred_model <- readRDS('aux_obj/test_models.rds')

## Run the bootstrap analysis
M <- 1000 # Number of bootstrap samples
pred_model <- pred_model[[model]]
rm(list=c('pred_model'))
### Calculate auxiliary quantities
n_test       <- dim(pred_model)[1]
pesos_aux    <- (table(pred_model$`as.character(partition_aux[["Ytest"]]$y)`)/n_test)
index_alive  <- which(pred_model$`as.character(partition_aux[["Ytest"]]$y)`=='alive')
index_avoid  <- which(pred_model$`as.character(partition_aux[["Ytest"]]$y)`=='avoidable_death')
index_unavoid<- which(pred_model$`as.character(partition_aux[["Ytest"]]$y)`=='unavoidable_death')
### Auxiliary objects
risk_metric = matrix(NA,length(intervals),M)
### Bootstrap
for(k in 1:M){
  bootstrap_sample = sample(c('alive','avoidable_death','unavoidable_death'),n_test,replace=TRUE,prob=pesos_aux)
  bootstrap_alive  = sample(index_alive,table(bootstrap_sample)[1],replace=TRUE)
  bootstrap_avoid  = sample(index_avoid,table(bootstrap_sample)[2],replace=TRUE)
  bootstrap_unavoid= sample(index_unavoid,table(bootstrap_sample)[3],replace=TRUE)
  
  bootstrap_pred   = pred_model[c(bootstrap_alive,bootstrap_avoid,bootstrap_unavoid),c(2,4)]
  bootstrap_pred   = bootstrap_pred[order(bootstrap_pred[,1],decreasing=TRUE),]
  total_deaths     = sum(bootstrap_pred$`as.character(partition_aux[["Ytest"]]$y)`=='avoidable_death')
  
  for (m in 1:length(intervals)){
    pred_model_risk <- bootstrap_pred[(1:(intervals[m]*dim(bootstrap_pred)[1])),]
    risk_metric[m,k]<-sum(pred_model_risk$`as.character(partition_aux[["Ytest"]]$y)`=='avoidable_death')/total_deaths
  }
}
saveRDS(risk_metric,file='results/risk_measure_bootstrap.rds')