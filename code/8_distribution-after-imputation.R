source("./0_functions.R")

args = commandArgs(trailingOnly=TRUE)
outcome <- args[1]

if (outcome == 'lower52'){
  variables <- c('ais1', 'lower01', 'lower52')
} else if (outcome == 'lower26'){
  variables <- c('lower26')
}   

for (var in variables){
  
  df_dist <- as.data.frame(matrix(ncol=7, nrow=0))
  colnames(df_dist) <-c("Variable", "Pattern", "Imputation", "SygenAll", "SygenCompleteNAanalysis", "EMSCI", "balanced")

  simulations <- c('SygenAll', 'SygenCompleteNAanalysis', 'EMSCI', 'balanced')
  patterns = c('MCAR', 'MAR', 'MNAR')
  imputations = c('lr', 'knn', 'RF', 'SVM_linear', 'SVM_rbf', 'mean')
    if (var == outcome){
        imputations = append(imputations, c('norm.predict', 'pmm', 'rf', 'last_obs'))
    } else if (var == 'lower01'){
        imputations = append(imputations, c('norm.predict', 'pmm', 'rf'))
    } else if (var == 'ais1') {
        imputations = append(imputations, 'polr')
    }

  nb_rows = length(patterns)*length(imputations)
  for(row in 1:nb_rows) {
    df_dist[row,'Variable'] = var
    df_dist[row,'Pattern'] = rep(patterns, each=length(imputations))[row]
    df_dist[row,'Imputation'] = rep(imputations, 3)[row]
  }

  for (sim in simulations){
    vec_dist <- c()
    for (pat in patterns){
    	for (imp in imputations){
	      count_dist = 0
	      for (i in c(1:500)){
	      #for (i in c(1:2)){
                #if (outcome == 'lower52'){
                #   df_imputed_all <- read.csv(paste0("/cluster/scratch/blucie/NA_SCI/subsets_imputed/", sim, "/subset_", as.character(i), '_imputed_mice_n500.csv'))
                #} else if (outcome == 'lower26'){
                df_imputed_all <- read.csv(paste0("/cluster/scratch/blucie/NA_SCI/subsets_imputed/", sim, "/", outcome, "/subset_", as.character(i), "_imputed_mice_n500.csv"))
                #}
	        df_imputed_all[df_imputed_all == ''] <- NA
	        true <- df_imputed_all[[var]]
	        test <- df_imputed_all[[paste0(var, '_', pat, '_', imp)]]
	         
	        if (var == 'ais1'){
	          test <- test[!is.na(test)]
	          expected <- as.vector(table(true)/sum(table(true)))
	          x <- as.vector(table(test))
	          if (length(x) == 3){
	            x[4] = 0
	          }
	          temp <- chisq.test(x=x, p=expected)$p.value
	          if (temp < 0.05){
	            count_dist = count_dist + 1
	          }
	        } else if (grepl('lower', var, fixed = TRUE)){
                  print(paste0(var, '_', pat, '_', imp))
	          temp <- ks.test(true, test, simulate.p.value = T)$p.value
	          if (temp < 0.05){
	            count_dist = count_dist + 1
	          }
	        }
	        
	      }
	      #print('--------------------------------------------')
	      #print(sim)
	      #print(paste('Distribution test:', sim, var, pat, imp, count_dist))
	      #print('--------------------------------------------')
	      vec_dist <- c(vec_dist, count_dist)
	    }
    }
  for(line in 1:length(vec_dist)) {
   df_dist[line,sim] = vec_dist[line]
  }
 }
 print('--------------------------------------------')
 print(paste0('Number of samples for which the hypothesis of a similar distribution for samples imputed and without NA is rejected - ', var))
 #print(df_dist)
 write.csv(df_dist, paste0("./important_output/count_distribution-shift_imputed_outcome", outcome, '_', var, "_after-imputation.csv"), row.names = FALSE)
 print('--------------------------------------------')
 
}

