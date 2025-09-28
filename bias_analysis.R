## Inputs to the analysis
model <- 'xgbTree'
intervals_vuln <- seq(0.00,1,0.05)
total_deaths   <- sum(
  (pred_model[[model]]$`as.character(partition_aux[["Ytest"]]$y)`=='avoidable_death') )


## Analysis by maternal ethnicity
### Proportion of neonatal deaths (nonwhite mothers)
total_deaths_nonwhite <- sum(
  (pred_model[[model]]$`as.character(partition_aux[["Ytest"]]$y)`=='avoidable_death') &
  (pred_model[[model]]$sinasc_RACACORMAE=='Amarela' |
   pred_model[[model]]$sinasc_RACACORMAE=='Indígena' |
   pred_model[[model]]$sinasc_RACACORMAE=='Parda' |
   pred_model[[model]]$sinasc_RACACORMAE=='Preta'))
cut_value <- total_deaths_nonwhite/total_deaths
### Proportion of neonatal deaths by threshold (nonwhite mothers)
non_white         <- NULL
total_deaths_perc <- NULL
ci_upper_nonwhite <- NULL
ci_lower_nonwhite <- NULL
test_nonwhite     <- NULL
for (j in 1:(length(intervals_vuln)-1)){                     
  index_vuln   <- c(1:(intervals_vuln[j+1]*dim(pred_model[[model]])[1]))
  total_deaths_perc[j] <- sum((pred_model[[model]]$`as.character(partition_aux[["Ytest"]]$y)`[index_vuln]=='avoidable_death'))
  non_white[j] <- sum((pred_model[[model]]$`as.character(partition_aux[["Ytest"]]$y)`[index_vuln]=='avoidable_death') & 
                      (pred_model[[model]]$sinasc_RACACORMAE[index_vuln]=='Amarela' |
                       pred_model[[model]]$sinasc_RACACORMAE[index_vuln]=='Indígena' |
                       pred_model[[model]]$sinasc_RACACORMAE[index_vuln]=='Parda' |
                       pred_model[[model]]$sinasc_RACACORMAE[index_vuln]=='Preta'))/total_deaths_perc[j]
  ci_upper_nonwhite[j] <- non_white[j] + 1.96 * sqrt(non_white[j]*(1-non_white[j])/total_deaths_perc[j])
  ci_lower_nonwhite[j] <- non_white[j] - 1.96 * sqrt(non_white[j]*(1-non_white[j])/total_deaths_perc[j])
  aux_test <- prop.test(non_white[j]*total_deaths_perc[j], total_deaths_perc[j], 
                        p = cut_value,     
                        alternative = "two.sided",
                        correct = TRUE)
  test_nonwhite[j] <- aux_test$p.value
}
### Organize the results into a data frame
ci_upper_nonwhite[j] <- non_white[j] 
ci_lower_nonwhite[j] <- non_white[j] 
race <- data.frame(threshold  = seq(5,100,5),
                   percentage = non_white,
                   ci_lower   = ci_lower_nonwhite,
                   ci_upper   = ci_upper_nonwhite)
race$threshold <- factor(race$threshold)
### Summarize the results in a figure
fig1 <- ggplot() +   
  geom_pointrange(race, mapping=aes(x=threshold, y=100*percentage, ymin=100*ci_lower, ymax=100*ci_upper), size=0.5, color="black", fill="white", shape=22) + 
  theme_classic() +
  geom_hline(yintercept = 100*cut_value) +
  labs(x="Threshold (%) of the highest predicted risk of neonatal death",y="Non-white (%)",title='A) Maternal ethnicity') +
  theme(title = element_text(size=22),
        legend.title = element_blank(),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 18),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(size=1),
        axis.ticks.length=unit(0.2,"cm")) +
  scale_y_continuous(breaks=seq(100*round(min(race$ci_upper)-0.1,2),100*round(max(race$ci_upper)+0.1,2),1)) +
  scale_x_discrete(breaks = seq(5,100,5),
                   labels= c('',10,'',20,'',30,'',40,'',50,'',60,'',70,'',80,'',90,'',100))

ggsave("results/bias_nonwhite.pdf", plot = fig1, device = "pdf", width = 9, height = 5, units = "in")


## Analysis by maternal education
### Proportion of neonatal deaths (0-7 years of education)
total_deaths_highschool_less <- sum(
  (pred_model[[model]]$`as.character(partition_aux[["Ytest"]]$y)`=='avoidable_death') &
  (pred_model[[model]]$sinasc_ESCMAE=='0' |
   pred_model[[model]]$sinasc_ESCMAE=='1 a 3 anos' |
   pred_model[[model]]$sinasc_ESCMAE=='4 a 7 anos' )
  )
cut_value <- total_deaths_highschool_less/total_deaths
### Proportion of neonatal deaths by threshold (0-7 years of education)
highschool_less          <- NULL
total_deaths_perc        <- NULL
ci_upper_highschool_less <- NULL
ci_lower_highschool_less <- NULL
test_highschool_less     <- NULL
for (j in 1:(length(intervals_vuln)-1)){                     
  index_vuln   <- c(1:(intervals_vuln[j+1]*dim(pred_model[[model]])[1]))
  total_deaths_perc[j] <- sum((pred_model[[model]]$`as.character(partition_aux[["Ytest"]]$y)`[index_vuln]=='avoidable_death'))
  highschool_less[j] <- sum((pred_model[[model]]$`as.character(partition_aux[["Ytest"]]$y)`[index_vuln]=='avoidable_death') & 
                            (pred_model[[model]]$sinasc_ESCMAE[index_vuln]=='0' |
                             pred_model[[model]]$sinasc_ESCMAE[index_vuln]=='1 a 3 anos' |
                             pred_model[[model]]$sinasc_ESCMAE[index_vuln]=='4 a 7 anos' ))/total_deaths_perc[j]
  ci_upper_highschool_less[j] <- highschool_less[j] + 1.96 * sqrt(highschool_less[j]*(1-highschool_less[j])/total_deaths_perc[j])
  ci_lower_highschool_less[j] <- highschool_less[j] - 1.96 * sqrt(highschool_less[j]*(1-highschool_less[j])/total_deaths_perc[j])
  aux_test <- prop.test(highschool_less[j]*total_deaths_perc[j], total_deaths_perc[j], 
                        p = cut_value,     
                        alternative = "two.sided",
                        correct = TRUE)
  test_highschool_less[j] <- aux_test$p.value
}
### Organize the results into a data frame
ci_upper_highschool_less[j] <- highschool_less[j] 
ci_lower_highschool_less[j] <- highschool_less[j] 
education <- data.frame(threshold = seq(5,100,5),
                        percentage = highschool_less,
                        ci_lower = ci_lower_highschool_less,
                        ci_upper = ci_upper_highschool_less)
education$threshold <- factor(education$threshold)
### Summarize the results in a figure
fig2 <- ggplot() +   
  geom_pointrange(education, mapping=aes(x=threshold, y=100*percentage, ymin=100*ci_lower, ymax=100*ci_upper), size=0.5, color="black", fill="white", shape=22) + 
  theme_classic() +
  geom_hline(yintercept = 100*cut_value) +
  labs(x="Threshold (%) of the highest predicted risk of neonatal death",y="0-7 years of schooling (%)",title='B) Maternal education') +
  theme(title = element_text(size=22),
        legend.title = element_blank(),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 18),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(size=1),
        axis.ticks.length=unit(0.2,"cm")) +
  scale_y_continuous(breaks=seq(100*round(min(education$ci_upper)-0.1,2),100*round(max(education$ci_upper)+0.1,2),1)) +
  scale_x_discrete(breaks = seq(5,100,5),
                   labels= c('',10,'',20,'',30,'',40,'',50,'',60,'',70,'',80,'',90,'',100))

ggsave("results/bias_education.pdf", plot = fig2, device = "pdf", width = 9, height = 5, units = "in")


## Analysis by the age of the mother
### Proportion of neonatal deaths (young mothers)
total_deaths_sixteen_less <- sum(
  (pred_model[[model]]$`as.character(partition_aux[["Ytest"]]$y)`=='avoidable_death') &
  (pred_model[[model]]$sinasc_IDADEMAE <17)
  )
cut_value <- total_deaths_sixteen_less/total_deaths
### Proportion of neonatal deaths by threshold (young mothers)
sixteen_less          <- NULL
total_deaths_perc     <- NULL
ci_upper_sixteen_less <- NULL
ci_lower_sixteen_less <- NULL
test_sixteen_less     <- NULL
for (j in 1:(length(intervals_vuln)-1)){                     
  index_vuln   <- c(1:(intervals_vuln[j+1]*dim(pred_model[[model]])[1]))
  total_deaths_perc[j] <- sum((pred_model[[model]]$`as.character(partition_aux[["Ytest"]]$y)`[index_vuln]=='avoidable_death'))
  
  sixteen_less[j] <- sum((pred_model[[model]]$`as.character(partition_aux[["Ytest"]]$y)`[index_vuln]=='avoidable_death') & 
                         (pred_model[[model]]$sinasc_IDADEMAE[index_vuln]<17 ))/total_deaths_perc[j]
  ci_upper_sixteen_less[j] <- sixteen_less[j] + 1.96 * sqrt(sixteen_less[j]*(1-sixteen_less[j])/total_deaths_perc[j])
  ci_lower_sixteen_less[j] <- sixteen_less[j] - 1.96 * sqrt(sixteen_less[j]*(1-sixteen_less[j])/total_deaths_perc[j])
  aux_test <- prop.test(sixteen_less[j]*total_deaths_perc[j], total_deaths_perc[j], 
                        p = cut_value,     
                        alternative = "two.sided",
                        correct = TRUE)
  test_sixteen_less[j] <- aux_test$p.value
}
### Organize the results into a data frame
ci_upper_sixteen_less[j] <- sixteen_less[j]
ci_lower_sixteen_less[j] <- sixteen_less[j]
age <- data.frame(threshold = seq(5,100,5),
                  percentage = sixteen_less,
                  ci_lower = ci_lower_sixteen_less,
                  ci_upper = ci_upper_sixteen_less)
age$threshold <- factor(age$threshold)
### Summarize the results in a figure
fig3 <- ggplot() +   
  geom_pointrange(age, mapping=aes(x=threshold, y=100*percentage, ymin=100*ci_lower, ymax=100*ci_upper), size=0.5, color="black", fill="white", shape=22) + 
  theme_classic() +
  geom_hline(yintercept = 100*cut_value) +
  labs(x="Threshold (%) of the highest predicted risk of neonatal death",y="0-16 years old (%)",title='C) Age of the mother') +
  theme(title = element_text(size=22),
        legend.title = element_blank(),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 18),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(size=1),
        axis.ticks.length=unit(0.2,"cm")) +
  scale_y_continuous(breaks=seq(100*round(min(age$ci_upper)-0.1,2),100*round(max(age$ci_upper)+0.1,2),0.5)) +
  scale_x_discrete(breaks = seq(5,100,5),
                   labels= c('',10,'',20,'',30,'',40,'',50,'',60,'',70,'',80,'',90,'',100))

ggsave("results/bias_age.pdf", plot = fig3, device = "pdf", width = 9, height = 5, units = "in")


## Analysis by marital status
### Proportion of neonatal deaths (solo mothers)
total_deaths_solo <- sum(
  (pred_model[[1]]$`as.character(partition_aux[["Ytest"]]$y)`=='avoidable_death') &
  (pred_model[[1]]$sinasc_ESTCIVMAE=='Separada judicialmente' |
   pred_model[[1]]$sinasc_ESTCIVMAE=='Solteira' |
   pred_model[[1]]$sinasc_ESTCIVMAE=='Viúva' )
  )
cut_value <- total_deaths_solo/total_deaths
### Proportion of neonatal deaths by threshold (solo mothers)
solo              <- NULL
total_deaths_perc <- NULL
ci_upper_solo     <- NULL
ci_lower_solo     <- NULL
test_solo         <- NULL
for (j in 1:(length(intervals_vuln)-1)){                     
  index_vuln   <- c(1:(intervals_vuln[j+1]*dim(pred_model[[model]])[1]))
  total_deaths_perc[j] <- sum((pred_model[[model]]$`as.character(partition_aux[["Ytest"]]$y)`[index_vuln]=='avoidable_death'))
  solo[j] <- sum((pred_model[[model]]$`as.character(partition_aux[["Ytest"]]$y)`[index_vuln]=='avoidable_death') & 
                 (pred_model[[model]]$sinasc_ESTCIVMAE[index_vuln]=='Separada judicialmente' |
                  pred_model[[model]]$sinasc_ESTCIVMAE[index_vuln]=='Solteira' |
                  pred_model[[model]]$sinasc_ESTCIVMAE[index_vuln]=='Viúva' ))/total_deaths_perc[j]
  ci_upper_solo[j] <- solo[j] + 1.96 * sqrt(solo[j]*(1-solo[j])/total_deaths_perc[j])
  ci_lower_solo[j] <- solo[j] - 1.96 * sqrt(solo[j]*(1-solo[j])/total_deaths_perc[j])
  aux_test <- prop.test(solo[j]*total_deaths_perc[j], total_deaths_perc[j], 
                        p = cut_value,     
                        alternative = "two.sided",
                        correct = TRUE)
  test_solo[j] <- aux_test$p.value
}
### Organize the results into a data frame
ci_upper_solo[j] <- solo[j] 
ci_lower_solo[j] <- solo[j] 
marital <- data.frame(threshold = seq(5,100,5),
                      percentage = solo,
                      ci_lower = ci_lower_solo,
                      ci_upper = ci_upper_solo)
marital$threshold <- factor(marital$threshold)
### Summarize the results in a figure
fig4 <- ggplot() +   
  geom_pointrange(marital, mapping=aes(x=threshold, y=100*percentage, ymin=100*ci_lower, ymax=100*ci_upper), size=0.5, color="black", fill="white", shape=22) + 
  theme_classic() +
  geom_hline(yintercept = 100*cut_value) +
  labs(x="Threshold (%) of the highest predicted risk of neonatal death",y="Solo (%)",title='D) Marital status') +
  theme(title = element_text(size=22),
        legend.title = element_blank(),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 18),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(size=1),
        axis.ticks.length=unit(0.2,"cm")) +
  scale_y_continuous(breaks=seq(100*round(min(marital$ci_upper)-0.1,2),100*round(max(marital$ci_upper)+0.1,2),1)) +
  scale_x_discrete(breaks = seq(5,100,5),
                   labels= c('',10,'',20,'',30,'',40,'',50,'',60,'',70,'',80,'',90,'',100)) 

ggsave("results/bias_marital.pdf", plot = fig4, device = "pdf", width = 9, height = 5, units = "in")
### Clean the environment
rm(list=c('education','marital','age','race','aux_test','fig1','fig2','fig3','fig4'))
