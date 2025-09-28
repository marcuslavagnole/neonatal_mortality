dir.create('results')
## Predictive performance: Traditional metrics
outsample <- NULL
for(i in fitted_models){
  confusion_aux  <- NULL
  if(i %in% c('mlp','mlpML')){
    pred_model_out <- predict(mod_train[[i]],test_sample_x)
    confusion_aux  <- confusionMatrix(data = pred_model_out, 
                                      reference = partition_aux[['Ytest']]$y)
  } else{
    pred_model_out <- predict(mod_train[[i]],partition_aux[['Xtest']])
    confusion_aux  <- confusionMatrix(data = pred_model_out, 
                                      reference = partition_aux[['Ytest']]$y)
  }
  outsample      <- rbind(outsample,c(confusion_aux[[3]][1:2],confusion_aux[[4]]))
}
rownames(outsample) <- c('Elastic Net', 'LASSO', 'Logistic Regression',
                         'XGBoost', 'Random Forest', 'Neural Network 1L',
                         'Neural Network 3L')
saveRDS(outsample,file='results/trad_metrics.rds')


## Risk metric
intervals    <- seq(0.0,1,0.05)
total_deaths <- sum(pred_model[[1]]$`as.character(partition_aux[["Ytest"]]$y)`=='avoidable_death')
risk_metric  <- list()
for(l in fitted_models){
  risk_metric[[l]] <- NULL
  for (m in 1:length(intervals)){
    pred_model_risk <- NULL
    pred_model_risk <- pred_model[[l]][(1:(intervals[m]*dim(pred_model[[l]])[1])),]
    risk_metric[[l]][m]<-sum(pred_model_risk$`as.character(partition_aux[["Ytest"]]$y)`=='avoidable_death')/total_deaths
  }
}
### Organize the results into a data frame
risk_analysis <- data.frame(model = c(rep('Neural Network',length(intervals[-1])),
                                      rep('Logistic Regression',length(intervals[-1])),
                                      rep('LASSO Regression',length(intervals[-1])),
                                      rep('Elastic Net Regression',length(intervals[-1])),
                                      rep('XGBoost',length(intervals[-1])),
                                      rep('Random Forest',length(intervals[-1]))
                                      ),
                            range=c(100*intervals[-1],
                                    100*intervals[-1],
                                    100*intervals[-1],
                                    100*intervals[-1],
                                    100*intervals[-1],
                                    100*intervals[-1]
                                    ), 
                            measure=c(100*risk_metric[['mlp']][-1],
                                      100*risk_metric[['logistic']][-1],
                                      100*risk_metric[['lasso']][-1],
                                      100*risk_metric[['glmnet']][-1],
                                      100*risk_metric[['xgbTree']][-1],
                                      100*risk_metric[['ranger']][-1]
                                      )
                            )
### Summarize the results in a figure
risk_fig <- ggplot(risk_analysis, aes(x = range, y = measure, color = model)) + 
  geom_point() + geom_line() + theme_classic() +
  xlab("Threshold (%) of the highest predicted risk of neonatal death") + ylab("Inclusion of actual neonatal deaths (%)") +
  theme(legend.title= element_blank(),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size=14),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(size=1),
        axis.ticks.length=unit(0.2,"cm")) +
  scale_x_continuous(limits=c(5,100),breaks=seq(5,100,5),
                     labels=c('',10,'',20,'',30,'',40,'',50,'',60,'',70,'',80,'',90,'',100))+
  guides(color = guide_legend(reverse=TRUE))

ggsave("results/risk_measure.pdf", plot = risk_fig, device = "pdf", width = 9, height = 5, units = "in")
### Clean the environment
rm(list=c('confusion_aux','outsample','risk_metric','risk_analysis','pred_model_risk','risk_fig'))
