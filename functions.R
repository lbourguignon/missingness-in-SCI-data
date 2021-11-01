################################################################################
# SCI - Handling missing data project
# L. Bourguignon
# First version : 06.07.2021
# Last update : 12.10.2021
# ------------------------------------------------------------------------------
# FUNCTIONS
################################################################################

# ------------------------------------------------------------------------------

# Function to subset columns in vec from dataframe df
# Output is a dataframe
subset_col <- function(vec, df){
  return(df[vec])
}

# ------------------------------------------------------------------------------

# Function to add string week to all elements of vec as pre- or suffix
# based on order (take values 'before' or 'after')
# Output is a vector
vector_var <- function(vec, week, order){
  if (order == 'before'){
    return(paste(week, vec, sep=""))
  }
  if (order == 'after'){
    return(paste(vec, week, sep=""))
  }
}

# ------------------------------------------------------------------------------

# Function to introduce NAs completely at random from column col in dataframe df
# Output is a vector
generate_missing_col_CAR <- function(data, prop, col){
  vec <- c(data[[col]])
  is.na(vec) <- sample(length(vec), prop*length(vec))
  return (vec)
}

# ------------------------------------------------------------------------------

# Function to introduce NAs at random (more missingness in male population)
# from column col in dataframe df
# Output is a dataframe with columns ptid and paste0(col, '_MAR')
generate_missing_col_AR <- function(data, prop, col){
  data_female <- data[data$sexcd == 1,]
  data_male <- data[data$sexcd == 2,]
  vec_female <- c(data_female[[col]])
  vec_male <- c(data_male[[col]])
  weight <- (prop * (length(vec_female) + length(vec_male)))/(length(vec_female) + 4*length(vec_male))
  is.na(vec_female) <- sample(length(vec_female), weight*length(data_female$sexcd))
  is.na(vec_male) <- sample(length(vec_male), weight*4*length(data_male$sexcd))
  data_female$missing_col <- vec_female
  data_male$missing_col <- vec_male
  df_combined <- rbind(data_female, data_male)
  df_combined <- df_combined[c('ptid', 'missing_col')]
  colnames(df_combined)[which(names(df_combined) == "missing_col")] <- paste0(col, '_MAR')
  return (df_combined)
}

# ------------------------------------------------------------------------------

# Function to introduce NAs not at random (more AIS D are missing) 
# from column col in dataframe `df`
# Output is a vector 
ais_NAR <- function(x, weight_a, weight_b, weight_c, weight_d){
  if (x == 'AIS A'){
    x <- sample(c('AIS A', NA), 
                1, 
                replace = F,
                prob = c(1-weight_a, weight_a))
  } else if (x == 'AIS B'){
    x <- sample(c('AIS B', NA), 
                1, 
                replace = F,
                prob = c(1-weight_b, weight_b))
  } else if (x == 'AIS C'){
    x <- sample(c('AIS C', NA), 
                1, 
                replace = F,
                prob = c(1-weight_c, weight_c))
  } else if (x == 'AIS D'){
    x <- sample(c('AIS D', NA), 
                1, 
                replace = F,
                prob = c(1-weight_d, weight_d))
  }
}

# ------------------------------------------------------------------------------

lems_NAR_0quantile <- function(x, proba_lowlems, proba_highlems){
  if (x == 0){
    x <- sample(c(0, NA),
                1,
                replace = F,
                prob = c(1-proba_lowlems, proba_lowlems))
  } else if (x > 0){
    x <- sample(c(x, NA),
                1,
                replace = F,
                prob = c(1-proba_highlems, proba_highlems))
  }
}

# ------------------------------------------------------------------------------

lems_NAR_none0quantile <- function(x, proba_lowlems, proba_highlems){
  if (x < quantile){
    x <- sample(c(0, NA),
                1,
                replace = F,
                prob = c(1-proba_lowlems, proba_lowlems))
  } else if (x >= quantile){
    x <- sample(c(x, NA),
                1,
                replace = F,
                prob = c(1-proba_highlems, proba_highlems))
  }
}

# ------------------------------------------------------------------------------

generate_missing_col_NAR <- function(data, prop, col){
  if (col == 'ais1'){

    data$test <- data[[col]]

    weight_a = prop/(4*table(data[[col]], useNA='always')[4]/dim(data)[1] +
                       3*table(data[[col]], useNA='always')[3]/dim(data)[1] +
                       2*table(data[[col]], useNA='always')[2]/dim(data)[1] +
                       1*table(data[[col]], useNA='always')[1]/dim(data)[1])[[1]]
    weight_b = 2*weight_a
    weight_c = 3*weight_a
    weight_d = 4*weight_a
    data$test <- apply(data['test'], 1, ais_NAR, weight_a, weight_b, weight_c, weight_d)

  } else if (col %in% c('lower01', 'lower52')){

    data$test <- data[[col]]
    quantile = quantile(data[[col]], probs = prop)[[1]]

    if (quantile == 0){
      vec_quantile <- quantile(data[[col]], probs = seq(0, 1, by= 0.01))
      indx <- which(vec_quantile!=0)[1]-2
      indx <- indx/100
      proba_lowlems <- (prop)/(4*(1-indx)+indx)
      proba_highlems <- proba_lowlems*4 # the probability of having NA when lems is high is four times higher than when lems is low

      data$test <- apply(data['test'], 1, lems_NAR_0quantile, proba_lowlems, proba_highlems)

    } else {
      proba_lowlems <- (prop)/(4*(1-prop)+prop)
      proba_highlems <- proba_lowlems*4 # the probability of having NA when lems is high is four times higher than when lems is low

      data$test <- apply(data['test'], 1, lems_NAR_none0quantile, proba_lowlems, proba_highlems)
    }
  }

  vec <- c(data[['test']])

  return (vec)
}

# ------------------------------------------------------------------------------

introduce_missingness <- function(data, cols, patterns, prop){
  if ('MCAR' %in% patterns){
    for (col in cols){
      data[paste0(col, '_MCAR')] <- generate_missing_col_CAR(data, prop, col)
    }
  }
  if ('MAR' %in% patterns){
    for (col in cols){
      data_temp <- generate_missing_col_AR(data, prop, col)
      data = merge(data, data_temp)
    }
  }
  if ('MNAR' %in% patterns){
    for (col in cols){
      data[paste0(col, '_MNAR')] <- generate_missing_col_NAR(data, prop, col)
    }
  }
  return(data)
}

# ------------------------------------------------------------------------------

prepare_df_lm <- function(data_sub, var, pat, variables){
  data_temp <- subset_col(c('age', 'sexcd', 'level',
                            variables[-(match(var, variables))],
                            paste0(var, '_', pat)),
                          data_sub)
  return (data_temp)
}

# ------------------------------------------------------------------------------

run_lm <- function(data_temp, var, pat, variables, imput){
  if (var != 'lower52'){
    formula <- paste0(grep('lower52', variables, value=TRUE)," ~ .")
  } else if (var == 'lower52'){
    formula <- paste0(paste0(grep(var, variables, value=TRUE), '_', pat)," ~ .")
  }
  result_summary <- summary(lm(formula, data = data_temp))
  extracted_summary <- extract_summary(result_summary, paste0(var, '_', pat, '_', imput))
  return (extracted_summary)
}

# ------------------------------------------------------------------------------

result_imputations <- function(data, var, patterns, imp){
  data_copy <- data
  baseline <- summary(lm(lower52 ~ lower01 + level + ais1 + age + sexcd, data = data))

  df_results <- data.frame(matrix(, nrow=8, ncol=0))
  df_results$variables <- rownames(baseline$coef)
  df_results$coef <- baseline$coefficients[,1]
  df_results$pvalues <- baseline$coefficients[,4]
  df_results$model <- c(rep('baseline', 8))

  variables <- c('lower01', 'lower52', 'ais1')

  data_sub <- subset_col(c('ptid', 'age', 'sexcd', 'level', 'ais1', 'lower01', 'lower52',
                           names(data)[grepl(paste0(var, '_') , names(data))]),
                         data)

  if ('case_deletion' %in% imp){
    for (pat in patterns){
      data_temp <- prepare_df_lm(data_sub, var, pat, variables)
      extracted_summary <- run_lm(data_temp, var, pat, variables, 'case_deletion')
      ### Find a solution for the non matching variables names for the plots
      df_results <- rbind(df_results, extracted_summary)
    }
  }

  if ('mean' %in% imp || 'majority' %in% imp){
    for (pat in patterns){
      data_temp <- prepare_df_lm(data_sub, var, pat, variables)

      if (sapply(data_temp, typeof)[paste0(var, '_', pat)][[1]] == 'double'){
        replaceby <- mean(data_temp[paste0(var, '_', pat)][[1]], na.rm = T)
      } else if (sapply(data_temp, typeof)[paste0(var, '_', pat)][[1]] == 'character'){
        replaceby <- tail(names(sort(table(data_temp[paste0(var, '_', pat)]))), 1)
      }

      data_copy[paste0(var, '_', pat, '_mean/majority')] <- data_temp[paste0(var, '_', pat)] %>% replace(is.na(.), replaceby)
      data_temp[paste0(var, '_', pat)] <- data_temp[paste0(var, '_', pat)] %>% replace(is.na(.), replaceby)
      extracted_summary <- run_lm(data_temp, var, pat, variables, 'mean/majority')
      ### Find a solution for the non matching variables names for the plots
      df_results <- rbind(df_results, extracted_summary)
    }
  }

  if ('regression' %in% imp){
    for (pat in patterns){
      data_temp <- prepare_df_lm(data_sub, var, pat, variables)

      if (var != 'lower52'){
        data_temp_nooutcome <- data_temp %>% select(-contains("lower52"))
      } else if (var == 'lower52'){
        data_temp_nooutcome <- data_temp
      }

      data_temp_nooutcome_complete <- data_temp_nooutcome[complete.cases(data_temp_nooutcome), ]
      data_temp_nooutcome_missing <- data_temp_nooutcome[is.na(data_temp_nooutcome[paste0(var, '_', pat)]),]


      if (sapply(data_temp, typeof)[paste0(var, '_', pat)][[1]] == 'double'){
        formula <- paste0(paste0(grep(var, variables, value=TRUE), '_', pat)," ~ .")
        model <- lm(formula, data = data_temp_nooutcome_complete)
      } else if (sapply(data_temp, typeof)[paste0(var, '_', pat)][[1]] == 'character'){
        #data_temp_nooutcome_complete[paste0(var, '_', pat)] <- factor(data_temp_nooutcome_complete[paste0(var, '_', pat)])
        formula <- paste0(paste0(grep(var, variables, value=TRUE), '_', pat)," ~ .")
        model <- multinom(formula, data = data_temp_nooutcome_complete, family = 'poisson')
      }

      data_temp_nooutcome_missing_imputed <- data_temp_nooutcome_missing
      data_temp_nooutcome_missing_imputed[paste0(var, '_', pat)] <- predict(model, newdata = data_temp_nooutcome_missing)
      data_temp_nooutcome_imputed <- rbind(data_temp_nooutcome_complete, data_temp_nooutcome_missing_imputed)

      ## not sure this part is working ##
      data_temp_nooutcome_imputed$index <- as.numeric(row.names(data_temp_nooutcome_imputed))
      data_temp_nooutcome_imputed <- data_temp_nooutcome_imputed[order(data_temp_nooutcome_imputed$index), ]
      if (var != 'lower52'){
        data_temp_nooutcome_imputed$lower52 <- sygen_analysis_subset$lower52
      }
      data_temp_nooutcome_imputed <- subset(data_temp_nooutcome_imputed, select = -c(index))

      data_copy[paste0(var, '_', pat, '_regression')] <- data_temp_nooutcome_imputed[paste0(var, '_', pat)]
      extracted_summary <- run_lm(data_temp_nooutcome_imputed, var, pat, variables, 'regression')

      df_results <- rbind(df_results, extracted_summary)
    }
  }

  df_results$variables <- str_remove(df_results$variables, "_MCAR")
  df_results$variables <- str_remove(df_results$variables, "_MAR")
  df_results$variables <- str_remove(df_results$variables, "_MNAR")
  df_results$variables <- str_replace(df_results$variables, "ais1", 'AIS A-')
  df_results$variables <- str_replace(df_results$variables, "level", 'cervical-')
  df_results$variables <- str_replace(df_results$variables, "sexcd2", 'male-female')

  return(list(df_results, data_copy))

}

# ------------------------------------------------------------------------------

extract_summary <- function(summary, name){
  df_results <- data.frame(matrix(, nrow=length(rownames(summary$coef)), ncol=0))
  df_results$variables <- rownames(summary$coef)
  df_results$coef <- summary$coefficients[,1]
  df_results$pvalues <- summary$coefficients[,4]
  df_results$model <- c(rep(name, length(rownames(summary$coef))))
  return (df_results)
}

# ------------------------------------------------------------------------------

aisgrade_levelfactor <- function(col){
  col[col == '1'] <- 'AIS A'
  col[col == '2'] <- 'AIS B'
  col[col == '3'] <- 'AIS C'
  col[col == '4'] <- 'AIS D'
  return(col)
}

# ------------------------------------------------------------------------------

plot_imputation_results <- function(results_data, dist_data, var, patterns, imp){

  if (levels(factor(results_data$model))[1] != 'baseline'){
    reduced_levels <- levels(factor(results_data$model))[levels(factor(results_data$model)) != 'baseline']
    results_data$model <- factor(results_data$model,
                                 levels = c("baseline", reduced_levels))
  }

  colors <- list('MCAR' = c('#000000', '#DC1C13', '#F07470', '#F6BDC0'),
              'MAR' = c('#000000', '#0000FF', '#7879FF', '#BFBFFF'),
              'MNAR' = c('#000000', '#2EB62C', '#83D475', '#C5E8B7'))

  plots_distribution <- list()
  cbPalette <- c('black')
  for (pat in patterns){
    if (var == 'ais1'){
      plot_temp <- plot_AIS_distribution(dist_data, pat, colors[pat][[1]])
    } else if (var == 'lower01'){
      plot_temp <- plot_lems01_distribution(dist_data, pat, colors[pat][[1]])
    } else if (var == 'lower52'){
      plot_temp <- plot_lems52_distribution(dist_data, pat, colors[pat][[1]])
    }
    plots_distribution <- list(plots_distribution, pat = plot_temp)
    cbPalette <- c(cbPalette, rep(colors[pat][[1]][3], length(imp)))
  }

  shapes <- c(8, rep(c(0:(length(imp)-1)), length(patterns)))

  coef_plot <- ggplot(results_data) +
    geom_point(aes(x = coef, y = variables, colour = model, shape = model)) +
    scale_shape_manual(values = shapes) +
    scale_colour_manual(values = cbPalette)

  pvalue_plot <- ggplot(results_data) +
    geom_point(aes(x = log(pvalues), y = variables, colour = model, shape = model)) +
    scale_shape_manual(values = shapes) +
    geom_vline(xintercept = log(0.05)) +
    scale_colour_manual(values = cbPalette)

  final_plots <- list()
  for (plot in c(coef_plot, pvalue_plot)){
    final_plot <- ggarrange(plot,
                            ggarrange(plots_distribution[1][[1]],
                                      plots_distribution[2][[1]],
                                      plots_distribution[3][[1]],
                                      nrow = length(imp), labels = toupper(letters[2:(length(imp)+1)])
                            ),
                            ncol = 2,
                            labels = "A")
    final_plots <- append(final_plots, final_plot)
  }

  #final_plots <- list(coef_plot, pvalue_plot)

  return(final_plots)
}

# ------------------------------------------------------------------------------

plot_lems01_distribution <- function(dist_data, missing, col){
  vec <- c("lower01", paste0("lower01_", missing), paste0("lower01_", missing, "_mean/majority"), paste0("lower01_", missing, "_regression"))
  subset_ais <- subset_col(c('ais1', vec), dist_data)

  subset_ais$ID <- seq.int(nrow(subset_ais)[1])

  long_ais <- melt(subset_ais,
                   id.vars = c("ID", 'ais1'),
                   variable.name = "LEMS")

  plot <- ggplot(long_ais, aes(x=value, fill=LEMS)) +
    geom_histogram(position="dodge") +
    facet_grid(rows = vars(ais1), cols = vars(LEMS), scales = 'free')

  return (plot)
}

plot_AIS_distribution <- function(dist_data, missing, col){
  vec <- c("ais1", paste0("ais1_", missing), paste0("ais1_", missing, "_mean/majority"), paste0("ais1_", missing, "_regression"))
  subset_ais <- subset_col(vec, dist_data)

  subset_ais$ID <- seq.int(nrow(subset_ais)[1])

  long_ais <- melt(subset_ais,
                   id.vars = "ID",
                   variable.name = "AIS")

  plot <- ggplot(long_ais, aes(value, fill=AIS)) +
    geom_bar(position = "dodge") +
    scale_fill_manual(values=col)

  return (plot)
}


plot_lems52_distribution <- function(missing, col){
  vec <- c("lower52", paste0("lower52_", missing), paste0("lower52_", missing, "_mean/majority"), paste0("lower01_", missing, "_regression"))
  subset_ais <- subset_col(vec, sygen_analysis_subset)
  if (missing == 'MCAR'){
    subset_ais[paste0("lower52_", missing, "_regression")] <- sygen_analysis_subset_lower52_MCAR_imputed$lower52_MCAR
  } else if (missing == 'MAR'){
    subset_ais[paste0("lower52_", missing, "_regression")] <- sygen_analysis_subset_lower52_MAR_imputed$lower52_MAR
  } else if (missing == 'MNAR'){
    subset_ais[paste0("lower52_", missing, "_regression")] <- sygen_analysis_subset_lower52_MNAR_imputed$lower52_MNAR
  } else {
    print("Error in type of missingness. Valid choices are MCAR, MAR and MNAR.")
  }

  subset_ais$ID <- seq.int(nrow(subset_ais)[1])

  long_ais <- melt(subset_ais,
                   id.vars = "ID",
                   variable.name = "LEMS")

  plot <- ggplot(long_ais, aes(x=value, y=LEMS)) +
    geom_violin() +
    stat_summary(fun.data=data_summary)

  return (plot)
}
