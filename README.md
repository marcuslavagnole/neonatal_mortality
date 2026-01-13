This repository provides the R routines used in the article [Reducing Inequalities Using an Unbiased Machine Learning Approach to Identify Births with the Highest Risk of Preventable Neonatal Deaths](). It is a joint work with [Antonio Pedro Ramos](https://tomramos.github.io/), Fabio Caldieraro, and [Raphael Saldanha](https://rfsaldanha.github.io/), published in _Population Health Metrics_.

**Abstract**: The paper proposes a data-driven approach that combines several risk factors and provides digested information that identifies which neonates are at the highest risk of preventable death. By utilizing administrative databases from the Brazilian health ministry, we train machine-learning algorithms, test their performance on unseen data, and evaluate them using a new policy-oriented metric that computes the proportion of deaths captured by setting the threshold levels of the highest predicted mortality risk. The results suggest that high proportions of neonatal deaths are captured with low threshold levels. In addition, it is investigated how the most vulnerable groups would be impacted by a targeting policy based on this metric, and the findings indicate that these groups wouldn't be disadvantaged.

The repo includes:

- **main.R**: run the following files to reproduce the main results.
  - **auxiliary_functions_ML.R**: auxiliary functions used in the analysis;
  - **data_preparation.R**: feature engineering and train-test split;
  - **ML_train_predict.R**: train the algorithms and make predictions;
  - **ML_performance.R**: evaluate predictive performance (traditional and policy-oriented metrics);
  - **bias_analysis.R**: evaluate the impact on disadvantaged populations;
  - **proportion_analysis.R**: evaluate the proportion of individuals selected.
- [database.rds](https://drive.google.com/file/d/1a8iEv5HARQxLT9VgUK6e5RA0TNdJLPJa/view?usp=drive_link): link to download the data set.
- **var_dictionary.rds**: features type.
- **bootstrap_testsample.R**: stratified bootstrap to produce confidence intervals. 
