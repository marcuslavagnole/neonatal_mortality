## Inputs to the analysis
model <- 'xgbTree'
intervals_vuln <- seq(0.00,1,0.05)


## Analysis by maternal ethnicity
### Proportion by threshold
non_white         <- NULL
ci_upper_nonwhite <- NULL
ci_lower_nonwhite <- NULL
for (j in 1:(length(intervals_vuln)-1)){ 
  index_vuln   <- c(1:(intervals_vuln[j+1]*dim(pred_model[[model]])[1]))
  non_white[j] <- sum(pred_model[[model]]$sinasc_RACACORMAE[index_vuln]=='Amarela' |
                      pred_model[[model]]$sinasc_RACACORMAE[index_vuln]=='Indígena' |
                      pred_model[[model]]$sinasc_RACACORMAE[index_vuln]=='Parda' |
                      pred_model[[model]]$sinasc_RACACORMAE[index_vuln]=='Preta')/length(index_vuln)
  ci_upper_nonwhite[j] <- non_white[j] + 1.96 * sqrt(non_white[j]*(1-non_white[j])/length(index_vuln))
  ci_lower_nonwhite[j] <- non_white[j] - 1.96 * sqrt(non_white[j]*(1-non_white[j])/length(index_vuln))
}
### Organize the results into a data frame
ci_upper_nonwhite[j] <- non_white[j] 
ci_lower_nonwhite[j] <- non_white[j]
race <- data.frame(threshold  = seq(5,100,5),
                   percentage = non_white,
                   ci_lower   = ci_lower_nonwhite,
                   ci_upper   = ci_upper_nonwhite)
race$threshold <- factor(race$threshold)
### Reference proportion
cut_value <- sum(pred_model[[model]]$sinasc_RACACORMAE=='Amarela' |
                 pred_model[[model]]$sinasc_RACACORMAE=='Indígena' |
                 pred_model[[model]]$sinasc_RACACORMAE=='Parda' |
                 pred_model[[model]]$sinasc_RACACORMAE=='Preta')/dim(pred_model[[model]])[1]
cut_value_upper <- cut_value + 1.96 * sqrt(cut_value*(1-cut_value)/dim(pred_model[[model]])[1])
cut_value_lower <- cut_value - 1.96 * sqrt(cut_value*(1-cut_value)/dim(pred_model[[model]])[1])
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
  scale_y_continuous(breaks=seq(100*round(cut_value_lower-0.1,2),100*round(max(race$ci_upper)+0.1,2),2)) +
  scale_x_discrete(breaks = seq(5,100,5),
                   labels= c('',10,'',20,'',30,'',40,'',50,'',60,'',70,'',80,'',90,'',100))

ggsave("results/proportion_nonwhite.pdf", plot = fig1, device = "pdf", width = 9, height = 5, units = "in")


## Analysis by maternal education
### Proportion by threshold
highschool_less          <- NULL
ci_upper_highschool_less <- NULL
ci_lower_highschool_less <- NULL
for (j in 1:(length(intervals_vuln)-1)){                     
  index_vuln         <- c(1:(intervals_vuln[j+1]*dim(pred_model[[model]])[1]))
  highschool_less[j] <- sum(pred_model[[model]]$sinasc_ESCMAE[index_vuln]=='0' |
                            pred_model[[model]]$sinasc_ESCMAE[index_vuln]=='1 a 3 anos' |
                            pred_model[[model]]$sinasc_ESCMAE[index_vuln]=='4 a 7 anos')/length(index_vuln)
  ci_upper_highschool_less[j] <- highschool_less[j] + 1.96 * sqrt(highschool_less[j]*(1-highschool_less[j])/length(index_vuln))
  ci_lower_highschool_less[j] <- highschool_less[j] - 1.96 * sqrt(highschool_less[j]*(1-highschool_less[j])/length(index_vuln))
}
### Organize the results into a data frame
ci_upper_highschool_less[j] <- highschool_less[j]
ci_lower_highschool_less[j] <- highschool_less[j]
education <- data.frame(threshold = seq(5,100,5),
                        percentage = highschool_less,
                        ci_lower = ci_lower_highschool_less,
                        ci_upper = ci_upper_highschool_less)
education$threshold <- factor(education$threshold)
### Reference proportion
cut_value <- sum(pred_model[[model]]$sinasc_ESCMAE=='0' |
                 pred_model[[model]]$sinasc_ESCMAE=='1 a 3 anos' |
                 pred_model[[model]]$sinasc_ESCMAE=='4 a 7 anos')/dim(pred_model[[model]])[1]
cut_value_upper <- cut_value + 1.96 * sqrt(cut_value*(1-cut_value)/dim(pred_model[[model]])[1])
cut_value_lower <- cut_value - 1.96 * sqrt(cut_value*(1-cut_value)/dim(pred_model[[model]])[1])
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
  scale_y_continuous(breaks=seq(100*round(cut_value_lower-0.1,2),100*round(max(education$ci_upper)+0.1,2),2)) +
  scale_x_discrete(breaks = seq(5,100,5),
                   labels= c('',10,'',20,'',30,'',40,'',50,'',60,'',70,'',80,'',90,'',100))

ggsave("results/proportion_education.pdf", plot = fig2, device = "pdf", width = 9, height = 5, units = "in")


## Analysis by the age of the mother
### Proportion by threshold
sixteen_less          <- NULL
ci_upper_sixteen_less <- NULL
ci_lower_sixteen_less <- NULL
for (j in 1:(length(intervals_vuln)-1)){                     
  index_vuln         <- c(1:(intervals_vuln[j+1]*dim(pred_model[[model]])[1]))
  sixteen_less[j] <- sum(pred_model[[model]]$sinasc_IDADEMAE[index_vuln] < 17)/length(index_vuln)
  ci_upper_sixteen_less[j] <- sixteen_less[j] + 1.96 * sqrt(sixteen_less[j]*(1-sixteen_less[j])/length(index_vuln))
  ci_lower_sixteen_less[j] <- sixteen_less[j] - 1.96 * sqrt(sixteen_less[j]*(1-sixteen_less[j])/length(index_vuln))
}
### Organize the results into a data frame
ci_upper_sixteen_less[j] <- sixteen_less[j]
ci_lower_sixteen_less[j] <- sixteen_less[j]
age <- data.frame(threshold = seq(5,100,5),
                  percentage = sixteen_less,
                  ci_lower = ci_lower_sixteen_less,
                  ci_upper = ci_upper_sixteen_less)
age$threshold <- factor(age$threshold)
### Reference proportion
cut_value <- sum(pred_model[[model]]$sinasc_IDADEMAE < 17)/dim(pred_model[[model]])[1]
cut_value_upper <- cut_value + 1.96 * sqrt(cut_value*(1-cut_value)/dim(pred_model[[model]])[1])
cut_value_lower <- cut_value - 1.96 * sqrt(cut_value*(1-cut_value)/dim(pred_model[[model]])[1])
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
  scale_y_continuous(breaks=seq(100*round(cut_value_lower-0.1,2),100*round(max(age$ci_upper)+0.1,2),1)) +
  scale_x_discrete(breaks = seq(5,100,5),
                   labels= c('',10,'',20,'',30,'',40,'',50,'',60,'',70,'',80,'',90,'',100))

ggsave("results/proportion_age.pdf", plot = fig3, device = "pdf", width = 9, height = 5, units = "in")


## Analysis by marital status
### Proportion by threshold
solo          <- NULL
ci_upper_solo <- NULL
ci_lower_solo <- NULL
for (j in 1:(length(intervals_vuln)-1)){                     
  index_vuln <- c(1:(intervals_vuln[j+1]*dim(pred_model[[model]])[1]))
  solo[j] <- sum(pred_model[[model]]$sinasc_ESTCIVMAE[index_vuln]=='Separada judicialmente' |
                 pred_model[[model]]$sinasc_ESTCIVMAE[index_vuln]=='Solteira' |
                 pred_model[[model]]$sinasc_ESTCIVMAE[index_vuln]=='Viúva')/length(index_vuln)
  ci_upper_solo[j] <- solo[j] + 1.96 * sqrt(solo[j]*(1-solo[j])/length(index_vuln))
  ci_lower_solo[j] <- solo[j] - 1.96 * sqrt(solo[j]*(1-solo[j])/length(index_vuln))
}
### Organize the results into a data frame
ci_upper_solo[j] <- solo[j] 
ci_lower_solo[j] <- solo[j] 
marital <- data.frame(threshold = seq(5,100,5),
                      percentage = solo,
                      ci_lower = ci_lower_solo,
                      ci_upper = ci_upper_solo)
marital$threshold <- factor(marital$threshold)
### Reference proportion
cut_value <- sum(pred_model[[model]]$sinasc_ESTCIVMAE=='Separada judicialmente' |
                   pred_model[[model]]$sinasc_ESTCIVMAE=='Solteira' |
                   pred_model[[model]]$sinasc_ESTCIVMAE=='Viúva')/dim(pred_model[[model]])[1]
cut_value_upper <- cut_value + 1.96 * sqrt(cut_value*(1-cut_value)/dim(pred_model[[model]])[1])
cut_value_lower <- cut_value - 1.96 * sqrt(cut_value*(1-cut_value)/dim(pred_model[[model]])[1])
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
  scale_y_continuous(breaks=seq(100*round(cut_value_lower-0.1,2),100*round(max(marital$ci_upper)+0.1,2),2)) +
  scale_x_discrete(breaks = seq(5,100,5),
                   labels= c('',10,'',20,'',30,'',40,'',50,'',60,'',70,'',80,'',90,'',100))

ggsave("results/proportion_marital.pdf", plot = fig4, device = "pdf", width = 9, height = 5, units = "in")
