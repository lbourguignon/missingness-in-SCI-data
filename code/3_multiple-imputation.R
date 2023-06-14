################################################################################
# SCI - Handling missing data project 
# L. Bourguignon & L.P. Lukas 
# First version : 06.07.2021
# Last update : 14.06.2023
# ------------------------------------------------------------------------------
# MULTIPLE IMPUTATION
################################################################################

################################################################################
# Data and argument loading
################################################################################

args = commandArgs(trailingOnly=TRUE)
simulations <- args[1]
subset <- args[2]
outcome <- args[3]
variables <- c('lower01', outcome, 'ais1')
patterns = c('MCAR', 'MAR', 'MNAR')

################################################################################
# Sourcing additional scripts
################################################################################

source("./0_functions.R")



for (sim in simulations){
  for (i in subset){
    data <- read.csv(paste0('/cluster/scratch/blucie/NA_SCI/subsets_imputed/', sim, '/', outcome, '/subset_', i ,'_imputed_n500.csv'))
    data <- select(data, -c('X'))
    data[data == ''] <- NA
    for (var in variables){
      for (pat in patterns){
        #print(var)
        #print(pat)
        
        if (var == 'ais1'){
          imputations <- c('polr')
          data_temp <- data[, c(paste0(var, '_', pat), 'lower01', 'level', 'age', 'sexcd', outcome)]
          data_temp$lower01 <- as.numeric(data_temp$lower01)
          data_temp$lower52 <- as.numeric(data_temp[[outcome]])
          data_temp[paste0(var, '_', pat)] <- lapply(data_temp[paste0(var, '_', pat)], factor)
          position = 1
        } else if (var == 'lower01') {
          imputations <- c('pmm', 'rf', 'norm.predict')
          data_temp <- data[, c('ais1', paste0(var, '_', pat), 'level', 'age', 'sexcd', outcome)]
          data_temp$ais1 <- as.factor(data_temp$ais1)
          data_temp$lower52 <- as.numeric(data_temp[[outcome]])
          data_temp[paste0(var, '_', pat)] <- lapply(data_temp[paste0(var, '_', pat)], as.numeric)
          position = 2
        } else if (var == outcome) {
          imputations <- c('pmm', 'rf', 'norm.predict')
          data_temp <- data[, c(paste0(var, '_', pat), 'ais1', 'lower01', 'level', 'age', 'sexcd')]
          data_temp$lower01 <- as.numeric(data_temp$lower01)
          data_temp$ais1 <- as.factor(data_temp$ais1)
          data_temp[paste0(var, '_', pat)] <- lapply(data_temp[paste0(var, '_', pat)], as.numeric)
          position = 1
        } else {
          print('Invalid variable to be imputed, please select one among "ais1", "lower01" and "lower52" or "lower26"')
        }
        
        data_temp$level <- as.factor(data_temp$level)

        for (imp in imputations){
          print(imp) 
          met <- rep('', dim(data_temp)[2])
          met[position] <- imp
          predictorMatrix <- matrix(1, ncol = length(names(data_temp)), nrow = length(names(data_temp)))
          rownames(predictorMatrix) <- names(data_temp)
          colnames(predictorMatrix) <- names(data_temp)
          diag(predictorMatrix) <- 0
          predictorMatrix[,grepl(outcome, colnames(predictorMatrix))] <- 0
          #print(predictorMatrix)
          output_mice <- mice(data_temp, seed = 9,
                              method = met,
                              m = 25,
                              predictorMatrix = predictorMatrix)
          
          col_to_select = paste0(var, '_', pat)
          if (var == 'ais1'){ # formulas are slightly different depending on if the outcome variable has NAs or not
              formula <- paste0(outcome, " ~ lower01 + level + ", col_to_select, " + age + sexcd")
          } else if (var == 'lower01'){
              formula <- paste0(outcome, " ~ ", col_to_select, "+ level + ais1 + age + sexcd")
          } else if (var == outcome){
              formula <- paste0(col_to_select," ~ lower01 + level + ais1 + age + sexcd")
          }
          
          start.time.mice <- Sys.time()
          #print(start.time.mice)
          fit_mice <- with(output_mice, lm(formula = as.formula(formula)))
          pool_mice <- summary(pool(fit_mice))
          end.time.mice <- Sys.time()
          time.taken.mice <- end.time.mice - start.time.mice
          print(time.taken.mice)
          start.time.pool <- Sys.time()
          if (var =='ais1'){
            completedData <- complete(output_mice, 'broad')
            subset_completedData <- select(completedData, contains("ais1"))
            data[paste0(var, '_', pat, '_', imp)] <- apply(subset_completedData, 1 ,function(x) names(which.max(table(x))))
          } else {
            completedData <- complete(output_mice, 'long')
            a <- aggregate(completedData[, paste0(var, '_', pat)], by = list(completedData$.id), FUN = mean)
            data[paste0(var, '_', pat, '_', imp)] <- a$x
          }
          end.time.pool <- Sys.time()
          time.taken.pool <- end.time.pool - start.time.pool
          print(time.taken.pool) 
          write.csv(pool_mice,
              paste0("/cluster/scratch/blucie/NA_SCI/fitpool_results/", sim ,'/', outcome, '/subset_', i, '_imputed_mice_n500_', imp, '_', var, '_', pat, '.csv'),
              row.names = FALSE)

          #print("Time taken for fit-pool approach:", time.taken.mice)
          #print("Time taken for pooling in pool-fit approach:", time.taken.pool)

        }
      }
    }

    write.csv(data,
              paste0("/cluster/scratch/blucie/NA_SCI/subsets_imputed/", sim ,'/', outcome, '/subset_', i, '_imputed_mice_n500.csv'),
              row.names = FALSE)
    
     
  }
}
