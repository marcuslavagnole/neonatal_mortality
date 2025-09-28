## Load and Filter the data set
data_base <- readRDS(file = 'database.rds') %>%
  filter( # Exclude death records with no birth record
    ( !(is.na(sinasc_NUMERODN) & ! is.na(sim_NUMERODO)) ) & 
      # Exclude birth recorda with no date
      ( !is.na(sinasc_NUMERODN) & !is.na(sinasc_DTNASC) )   &
      # Exclude records before 2015
      ( format(sinasc_DTNASC, format = "%Y")>=2015 )        &
      # Exclude records where the difference between birth and death date are negative
      ( (sim_DTOBITO - sinasc_DTNASC)>=0 | is.na(sim_DTOBITO) )
  )


## Create the response variable
### Neonatal death
data_base$mortality_neonatal <- 0
data_base$mortality_neonatal[
  which((data_base$sim_DTOBITO - data_base$sinasc_DTNASC)<28)] <- 1
### Neonatal avoidable death
data_base$mortality_neonatal_evit <- 0
data_base$mortality_neonatal_evit[
  which(data_base$mortality_neonatal==1 & 
        data_base$classe_prin_causa_evit=='Causas evitáveis')] <- 1
### Neonatal unavoidable death 
data_base$mortality_neonatal_evit[
  which(data_base$mortality_neonatal==1 & 
        (data_base$classe_prin_causa_evit=='Causas de morte mal-definidas' | 
         data_base$classe_prin_causa_evit=='Outras causas de morte'))] <- 2


## Feature Engineering
### Select features of interest
nice_vars <- c("sinasc_LOCNASC",    # Place of delivery (hospital, other health facility, residence, others)
               "cnes_VINC_SUS",     # Health facility type (public or private)
               "sinasc_IDADEMAE",   # Age of the mother (in years)
               "sinasc_SEXO",       # Sex
               "sinasc_APGAR1",     # 1-min Apgar score
               "sinasc_APGAR5",     # 5-min Apgar score               
               "sinasc_PESO",       # birth weight (grams)
               "sinasc_SEMAGESTAC", # Gestational age (in weeks)
               #"sinasc_GESTACAO",   # Week of gestation (by ranges)
               "sinasc_GRAVIDEZ",   # Type of pregnancy (single, double or triple or more)
               "sinasc_PARTO",      # Type of delivery (vaginal or cesarean)
               "sinasc_ESCMAE",     # Maternal education
               "sinasc_IDANOMAL",   # Presence of congenital anomaly (yes/no)
               "sinasc_RACACORMAE", # Maternal ethnicity
               "sinasc_CONSPRENAT", # Antenatal visits
               "sinasc_MESPRENAT",  # Month of first antenatal visit
               "sinasc_TPAPRESENT", # Type of presentation (cephalic, breech, transversal or other), 
               "sinasc_STTRABPART", # Induced labor (yes/no)
               "sinasc_TPNASCASSI", # Professional that assisted the labor (physician, nurse, midwife or others)
               "sinasc_QTDFILVIVO", # Number of previous live births
               "sinasc_QTDFILMORT", # Number of previous fetal losses and abortions
               "sinasc_QTDGESTANT", # Number of previous pregnancies
               "sinasc_QTDPARTNOR", # Number of previous vaginal deliveries
               "sinasc_QTDPARTCES", # Number of previous cesarean deliveries
               "sinasc_ESTCIVMAE",  # Marital status
               "sinasc_munResUf"    # State of residence
)
### Read feature dictionary
var_dic <- readRDS('var_dictionary.rds')
### Format and drop predictors we do not want for our model
design_matrix <- dropPredictors(data_base[nice_vars], var_dic)
### Impute missing values
impute_result <- amelia(design_matrix[['matrix_x']],
                        noms=design_matrix[['nom_vars']],
                        ords=design_matrix[['ord_vars']],m=1)


## Create data objects for training and testing
dir.create('aux_obj')
### Split into train/test data 
partition_aux <- partition(factor(data_base$mortality_neonatal_evit, 
                                  labels=c("alive","avoidable_death",
                                           "unavoidable_death")),
                           impute_result$imputations[[1]],0.8)
#partition_aux <- partition_date(factor(data_base$mortality_neonatal_evit,
#                                       labels=c("alive","avoidable_death",
#                                                "unavoidable_death")),
#                                impute_result$imputations[[1]],
#                                data_base$sinasc_DTNASC,0.8)
saveRDS(partition_aux,file='aux_obj/data_partition.rds')
### Training data
data_train   <- cbind(partition_aux[['Ytrain']],partition_aux[['Xtrain']])
### Balance training data
data_balance <- dataBalance(data_train,0.14)
saveRDS(data_balance,file='aux_obj/data_balance.rds')
### Clean the environment
rm(list = ls()[!(ls() %in% c('trainModels','data_balance','partition_aux'))])
