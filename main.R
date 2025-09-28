## Loading libraries
library(dplyr)
library(Amelia)
library(caret) 

## Set working directory to file location and load utils
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source('auxiliary_functions_ML.R')

## Run the analysis
source('data_preparation.R')
source('ML_train_predict.R')
source('ML_performance.R')
source('bias_analysis.R')
source('proportion_analysis.R')