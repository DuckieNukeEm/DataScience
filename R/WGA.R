https://www.statmethods.net/advstats/bootstrapping.html
library(tidyverse)
library(xray)
library(readxl)
library(randomForest)
library(oaxaca)
library(cowplot)
library(caret)


#####
#
# Pulls out the names of the variables that are numiercal in nature
#
#####

n_v = function(df){ 
  
  vars = names(df)
  n_vars = vars[sapply(df[,vars], class) %in% c('numeric','integer')]
  
  return(n_vars)
}

#####
#
# Figures out the contribution weight of each variables in the Full model to the primary variables in the base model
# Methodology found at https://web.stanford.edu/~diamondr/UberPayGap.pdf, pg. 28 
#
#####

coef_contribution = function(full_lm_model, base_lm_model, df, verbose = F){
  name_of_primary = names(coef(base_lm_model))[2]
  
  df_var = data.frame(Full_Model = full_lm_model$coefficients,
                      Full_SE = summary(full_lm_model)$coefficients[,2],
                      Aux_Model = 0,
                      BaseValue =  as.vector(coef(base_lm_model)[2])
  )
  
  
  df_var$var = rownames(df_var)
  
  for (i in rownames(df_var)){
    if (i == '(Intercept)') {
      next()
    } 
    if(i == name_of_primary){
      df_var[i,'Aux_Model'] = 1
      next()
    }
    
    lm_fit = lm(paste(c(i,name_of_primary), collapse = " ~ "), data = df %>% select_(name_of_primary,i))
    if(verbose){
      print(i)
      print(summary(lm_fit))
    }
    df_var[i,'Aux_Model'] = as.vector(lm_fit$coefficients[2])
    
  }
  
  df_var$Contribution = df_var$Full_Model*df_var$Aux_Model/df_var$BaseValue
  
  return(df_var)
}

#####
#
# Function that takes a lm and returns the formula used
#
#####

get_formula = function(lrm, for_dummy = FALSE){
  if(for_dummy){
    out_form = paste(c("~",paste(c(lrm$terms[[2]], lrm$terms[[3]]), collapse = " + ")), collapse = "")
  } else {
    out_form = paste(c(lrm$terms[[2]],"~", lrm$terms[[3]]), collapse = " ")
  }
  return(as.formula(out_form))
}

#####
#
# Takes a linear regression and producess a data frame that LM Model
# with the proper dummy variables broekn out
#
#####


dummy_for_oaxaca = function(lrm, df){
  #dummiefies the variables so we can get adjustments
  dummy_str = get_formula(lrm, T)
  
  df_dummy = caret::dummyVars(dummy_str, data = df, sep = "")
  df_pred = predict(df_dummy, newdata = df)
  return(data.frame(df_pred))
  
}

#####
#
# Takes a linear regression and producess a data frame that has the dummy variables already built into it
#
#####


generate_df_oaxaca = function(lm1, df_join, df, join = 'var'){
  #adds information to the output dataframe for oaxaca method
  df_lm = df_join %>%
    left_join(
      data.frame(var = names(coef(lm1)), 
                 Beta =  coef(lm1),
                 stringsAsFactors = F),
      by = join
    ) %>%
    left_join(
      dummy_for_oaxaca(lm1, df ) %>%
        summarise_all(mean, na.rm = T) %>% gather(var, value),
      by = join
    )  %>% 
    mutate(Beta = ifelse(is.na(Beta), 0, Beta ),
           value = ifelse(is.na(value), 1, value )
    )
  return(df_lm)
}



tf_oaxaca = function(formula, var_of_sep, df, verbose = F){
  #implements the twofold oxacaa decomposition. 
  # Does it for both sides (A on B, B on A) and also against POOLED
  splitter = names(table(df[,var_of_sep], exclude = F))
  lm1 = lm(formula = formula, df[df[,var_of_sep] == splitter[1],] ) #for Group 1
  lm2 = lm(formula = formula, df[df[,var_of_sep] == splitter[2],] ) # for group 2
  lmp = lm(formula = formula, df ) # For the pooled Group w/o indicator
  lmpp = lm(paste(c(lm1$terms[[2]], '~', lm1$terms[[3]], '+' ,var_of_sep), collapse = ' '), df) # pooled group w indicator
  
  output_df = data.frame( var = unique(c(
    names(coef(lm1)), 
    names(coef(lm2))
  )),
  stringsAsFactors = F)
  
  #putting together Group 1 df
  df_lm1 =  generate_df_oaxaca(lm1, output_df, df[df[,var_of_sep] == splitter[1],] )
  names(df_lm1) = c('var','Beta_0','X_Bar_0')
  
  #putting Together Group 2 DF
  df_lm2 =  generate_df_oaxaca(lm2, output_df, df[df[,var_of_sep] == splitter[2],] )
  names(df_lm2) = c('var','Beta_1','X_Bar_1')
  
  #Putting Together the Pooled group (with Group Variable)
  df_lmp =  generate_df_oaxaca(lmp, output_df, df )
  names(df_lmp) = c('var',
                    'Beta_Pooled',
                    'X_Bar_Pooled')
  
  #Putting Together the Pooled group (with Group Variable (with indicator))
  df_lmpp =  generate_df_oaxaca(lmpp, output_df, df )
  names(df_lmpp) = c('var',
                     'Beta_Pooled_WI',
                     'X_Bar_Pooled_WI')
  
  #Joing it all together
  output_df = output_df %>%
    left_join(
      df_lm1, 
      by = 'var'
    ) %>%
    left_join(
      df_lm2,
      by = 'var'
    ) %>%
    left_join(
      df_lmp,
      by= 'var'
    ) %>%
    left_join(
      df_lmpp,
      by= 'var'
    ) %>%
    mutate(Explained_X1 = (X_Bar_1 - X_Bar_0) * Beta_0,
           Unexplained_X1 = (Beta_1 - Beta_0) * X_Bar_1,
           Explained_X2 = (X_Bar_1 - X_Bar_0) * Beta_1,
           Unexplained_X2 = (Beta_1 - Beta_0) * X_Bar_0,
           Explained_Pooled = (X_Bar_1 - X_Bar_0) * Beta_Pooled,
           Unexplained_Pooled_X1 = (Beta_0  - Beta_Pooled ) * X_Bar_0,
           Unexplained_Pooled_X2 = (Beta_1 - Beta_Pooled) * X_Bar_1,
           Explained_Pooled_WI = (X_Bar_1 - X_Bar_0) * Beta_Pooled_WI,
           Unexplained_Pooled_WI_X1 = (Beta_0 - Beta_Pooled_WI) * X_Bar_0,
           Unexplained_Pooled_WI_X2 = (Beta_1 - Beta_Pooled_WI) * X_Bar_1 
    )
  
  #fixing the names
  new_names = names(output_df) %>% 
    tibble() %>% 
    mutate(var = gsub('X1', splitter[1], `.`), 
           var = gsub('X2', splitter[2], var)) %>% 
    select(var) 
  
  names(output_df) = new_names$var
  
  
  return(output_df)
  
}

######
#
# Bootstrapping the Coeff Contributions! Actual estimation 
#
######

Bootstrap_list = function(df, n){
  out_list = lapply(1:n, function(y) sample(x = nrow(df), nrow(df), replace = T))
  return(out_list)
  
}

BootStrap_lm = function(lm, df, bootstrapped_list, n = 100){
  lm_formula = get_formula(lm)
  return(lapply(1:n function(x) coef(lm(lm_formula, data = df, subset = bootstrapped_list[[x]]))))
  
}


coef_contribution = function(full_lm_model, base_lm_model, df, verbose = F){
  name_of_primary = names(coef(base_lm_model))[2]
  
  df_var = data.frame(Full_Model = full_lm_model$coefficients,
                      Full_SE = summary(full_lm_model)$coefficients[,2],
                      Aux_Model = 0,
                      BaseValue =  as.vector(coef(base_lm_model)[2])
  )
  
  subset_coef = function(lm, lm_formula, df, subset_n){
    tt = coef(lm(lm_formula, data = df, subset = subset_n))
    return(tt)
            }
  
  df_var$var = rownames(df_var)
  
  for (i in rownames(df_var)){
    if (i == '(Intercept)') {
      next()
    } 
    if(i == name_of_primary){
      df_var[i,'Aux_Model'] = 1
      next()
    }
    
    lm_fit = lm(paste(c(i,name_of_primary), collapse = " ~ "), data = df %>% select_(name_of_primary,i))
    if(verbose){
      print(i)
      print(summary(lm_fit))
    }
    df_var[i,'Aux_Model'] = as.vector(lm_fit$coefficients[2])
    
  }
  
  df_var$Contribution = df_var$Full_Model*df_var$Aux_Model/df_var$BaseValue
  
  return(df_var)
}
