###Packages
library(tidyverse)
library(randomForest)
library(gbm)
library(xgboost)
library(caret)

########
#
# Loading Data and basic transofrmation of the data
#
#########


#df <-  read_csv("E:/Data/Black Friday/train.csv")  %>%
df <-  read_csv("Classes/Analytics Vidhya/2_1 Black Friday/Data/train.csv")  %>%
  mutate(
    Gender = factor(Gender),
    Age = str_replace(Age,"-","_"),
    Age = ifelse(Age == '55+','55_Plus', Age),
    Age = factor(Age, levels = c('0_17','18_25','26_35','36_45','46_50','51_55','55_Plus')),
    Age_N = case_when(
      Age == '0_17' ~ 17,
      Age == '18_25' ~ 25,
      Age == '26_35' ~ 35,
      Age == '36_45' ~ 45,
      Age == '46_50' ~ 55, #Keeping with the 10 year jump
      Age == '51_55' ~ 55,
      Age == '55_Plus' ~ 65,
      TRUE ~ 65),
    
    Occupation = factor(Occupation),
    City_Category = factor(City_Category),
    Years_In_City = ifelse(Stay_In_Current_City_Years == '4+', '4_Plus', Stay_In_Current_City_Years),
    Years_In_City = factor(Years_In_City),
    Marital_Status = factor(Marital_Status),
    PC_1 = factor(Product_Category_1),
    PC_2 = ifelse(is.na(Product_Category_2), 99, Product_Category_2),
    PC_2 = factor(PC_2),
    PC_3 = ifelse(is.na(Product_Category_3), 99, Product_Category_3),
    PC_3 = factor(PC_3)
  ) %>%
  select(-Stay_In_Current_City_Years,
         -Product_Category_1,
         -Product_Category_2,
         -Product_Category_3
  ) 



# dt <-  read_csv("E:/Data/Black Friday/test.csv") %>%
dt <-  read_csv("Classes/Analytics Vidhya/2_1 Black Friday/Data/test.csv") %>%
  mutate(
    Gender = factor(Gender),
    Age = str_replace(Age,"-","_"),
    Age = ifelse(Age == '55+','55_Plus', Age),
    Age = factor(Age, levels = c('0_17','18_25','26_35','36_45','46_50','51_55','55_Plus')),
    Age_N = case_when(
      Age == '0_17' ~ 17,
      Age == '18_25' ~ 25,
      Age == '26_35' ~ 35,
      Age == '36_45' ~ 45,
      Age == '46_50' ~ 55, #Keeping with the 10 year jump
      Age == '51_55' ~ 55,
      Age == '55_Plus' ~ 65,
      TRUE ~ 65),
    
    Occupation = factor(Occupation),
    City_Category = factor(City_Category),
    Years_In_City = ifelse(Stay_In_Current_City_Years == '4+', '4_Plus', Stay_In_Current_City_Years),
    Years_In_City = factor(Years_In_City),
    Marital_Status = factor(Marital_Status),
    PC_1 = factor(Product_Category_1),
    PC_2 = ifelse(is.na(Product_Category_2), 99, Product_Category_2),
    PC_2 = factor(PC_2),
    PC_3 = ifelse(is.na(Product_Category_3), 99, Product_Category_3),
    PC_3 = factor(PC_3)
  ) %>%
  select(-Stay_In_Current_City_Years,
         -Product_Category_1,
         -Product_Category_2,
         -Product_Category_3
  ) 

######
#
# Creating Test and Train set (70/30) split
#
######
set.seed(1985)
train_sample = sample(1:nrow(df), floor(nrow(df) * 0.7))

df_train = df[train_sample,]
df_test = df[-train_sample,]

df_train_2 =
  inner_join(
    df_train,
    #####
    #creating a flag for outlier records
    #####
    df_train %>% 
      #group_by(Product_ID,Age,Occupation, Years_In_City, Gender, Marital_Status) %>%
      group_by(Product_ID) %>%
      summarize(
        lower_quant = quantile(Purchase, probs = 0.25),
        upper_quant = quantile(Purchase, probs = 0.75)
      ),
    by = c('Product_ID')
    
  ) %>%
  mutate( L_limit = lower_quant - 1.50 * (upper_quant - lower_quant),
          U_limit = upper_quant + 1.50 * (upper_quant - lower_quant),
          Outlier_Purchase = ifelse(Purchase < L_limit | Purchase > U_limit, 1,0)
  ) %>%
  select(-L_limit, -U_limit, -lower_quant, -upper_quant)

  
df_train_2 =
  df_train_2 %>%
  inner_join(
    #####
    #User Information
    #####
    df_train_2 %>%
      group_by(User_ID) %>%
      #User level Data
      summarise(Avg_User_Spend = mean(Purchase),
                Med_User_Spend = median(Purchase),
                Total_User_Bought = n(),
                Num_of_PC_entered = n_distinct(PC_1)
      ) %>% ungroup(),
    by = 'User_ID'
    
  )  %>%
    
  inner_join(
    #####
    #Product Information
    #####
    df_train_2 %>% 
      filter(Outlier_Purchase == 0) %>%
      group_by(Product_ID) %>%
      summarize(
        Avg_Prod_Price = mean(Purchase),
        Med_Prod_Price = median(Purchase),
        Total_Prod_Bought = n(),
        sd_Prod_Price = sd(Purchase)
      ),
    by = c('Product_ID')
    
  ) %>%
  mutate( sd_Prod_Price = ifelse(is.na(sd_Prod_Price) | is.infinite(sd_Prod_Price), 0, sd_Prod_Price),
          Sd_from_Avg = ifelse(sd_Prod_Price == 0, 0, (Purchase - Avg_Prod_Price)/sd_Prod_Price)
  ) %>%
  inner_join(
    #####
    #USer, PC Level Data
    #####
    df_train_2 %>% 
      filter(Outlier_Purchase == 0) %>%
      group_by(User_ID, PC_1) %>%
      summarise(
        Avg_PC1_User_Price = mean(Purchase),
        Med_PC1_User_Price = median(Purchase),
        Total_PC1_User_Bought =n()
      ) %>%
      ungroup(),
    by = c('User_ID', 'PC_1')
    
  ) %>%
  #####
#Creating new vars based on the USer, PC Level data
#####
mutate(Prev_PC_Bought = ifelse(is.na(Total_PC1_User_Bought)| Total_PC1_User_Bought == 0, 0, Total_PC1_User_Bought- 1),
       Other_Avg_PC1_User_Price = ifelse(Total_PC1_User_Bought == 1, 0 ,
                                         (Avg_PC1_User_Price * Total_PC1_User_Bought - Purchase)/(Total_PC1_User_Bought - 1) 
       )
) %>%
  #######
# This next part is going to be long, what I'm going to build is a metric that tells what % of the time do
# they  pay above or below the median price.
#######
inner_join(
  inner_join(
    df_train_2 %>%
      filter(Outlier_Purchase == 0) %>%
      group_by(PC_1) %>%
      summarise(
        m_price_pc1 = mean(Purchase)
      ) %>% ungroup(),
    df_train_2 %>%
      group_by(User_ID, PC_1) %>%
      summarise(
        user_mean_price_PC1 = mean(Purchase)
      ) %>% ungroup(),
    by = 'PC_1') %>%
    mutate(Above_ = ifelse(user_mean_price_PC1 > m_price_pc1, 1, 0),
           per_diff_from_m = (user_mean_price_PC1/m_price_pc1 - 1),
           avg_diff_from_m = user_mean_price_PC1 - m_price_pc1) %>%
    group_by(User_ID) %>%
    summarise(
      Above_Per = mean(Above_),
      per_diff_from_m = mean(per_diff_from_m, na.rm = T),
      avg_diff_from_m = mean(avg_diff_from_m, na.rm = T)
    ) %>% ungroup(),
  by = 'User_ID'
) %>%
  mutate(sign_of_avg_diff_from_m = sign(avg_diff_from_m),
         per_diff_from_m = abs(per_diff_from_m),
         avg_diff_from_m = abs(avg_diff_from_m))




df_test_2 = df_test %>%
  left_join(
    df_train_2 %>%
      select(User_ID, 
             Avg_User_Spend, 
             Med_User_Spend,
             Total_User_Bought, 
             Above_Per, 
             per_diff_from_m, 
             avg_diff_from_m,
             sign_of_avg_diff_from_m) %>%
      distinct(),
    by = 'User_ID' 
  ) %>%
  mutate(
    Avg_User_Spend = ifelse(is.na(Avg_User_Spend),0, Avg_User_Spend),
    Med_User_Spend = ifelse(is.na(Med_User_Spend),0, Med_User_Spend),
    Total_User_Bought = ifelse(is.na(Total_User_Bought), 0,Total_User_Bought),
    Above_Per = ifelse(is.na(Above_Per), 0.5, Above_Per),
    per_diff_from_m = ifelse(is.na(per_diff_from_m), 0, per_diff_from_m),
    avg_diff_from_m = ifelse(is.na(avg_diff_from_m),0,avg_diff_from_m),
    sign_of_avg_diff_from_m = ifelse(is.na(sign_of_avg_diff_from_m), 0,sign_of_avg_diff_from_m )
  ) %>%
  left_join(
    df_train_2 %>%
      select(User_ID,
             PC_1,
             Total_PC1_User_Bought) %>%
      distinct(),
    by = c('User_ID','PC_1')
  ) %>%
  mutate(Total_PC1_User_Bought = ifelse(is.na(Total_PC1_User_Bought),0,Total_PC1_User_Bought) ) %>%
  
  left_join(
    df_train_2 %>%
      select(Product_ID,Avg_Prod_Price,Med_Prod_Price,Total_Prod_Bought) %>%
      distinct(),
    by = 'Product_ID'
  ) %>%
  mutate(
    Avg_Prod_Price = ifelse(is.na(Avg_Prod_Price),0, Avg_Prod_Price),
    Med_Prod_Price = ifelse(is.na(Med_Prod_Price),0, Med_Prod_Price),
    Total_Prod_Bought = ifelse(is.na(Total_Prod_Bought),0, Total_Prod_Bought)
  ) %>%
  inner_join(
    df_test %>%
      group_by(User_ID) %>%
      summarise(Num_of_PC_entered = n_distinct(PC_1)) %>%
      ungroup(),
    by = 'User_ID'
  )


######
#
# Linear Model
#
######


lm_df = lm(log(Purchase) ~ 
             log(Avg_Prod_Price) +
             log(Avg_User_Spend) + 
             log(avg_diff_from_m) +
             Age_N  + 
             #   Occupation + 
             City_Category + 
             Marital_Status + 
             #    Years_In_City + 
             PC_1  +  
             per_diff_from_m + 
             Total_PC1_User_Bought +
             Above_Per +
             #     Num_of_PC_entered +
             sign_of_avg_diff_from_m  
           #Avg_User_Spend # + PC_2Alt
           
           , df_train_2 %>% filter(Outlier_Purchase == 0 )
)
summary(lm_df)


df_test_2$lm_Pred_value =  exp(predict(lm_df, newdata = df_test_2))

df_train_2$lm_Pred_value = exp(predict(lm_df, newdata = df_train_2))

sqrt(mean((df_train_2$lm_Pred_value - df_train_2$Purchase)^2))
# 2510.825
sqrt(mean((df_test_2$lm_Pred_value - df_test_2$Purchase)^2))
# 2578.433

df_test_2 =
    inner_join(
      df_test_2,
      df_train_2 %>% group_by(PC_1) %>% summarise(mv = mean(Purchase)) %>% ungroup(),
      by = 'PC_1'
    ) %>%
  mutate(
    lm_Pred_value = ifelse(lm_Pred_value == 0, mv, lm_Pred_value)) %>%
  select( -mv)

sqrt(mean((df_test_2$lm_Pred_value - df_test_2$Purchase)^2))
# 2571.433



######
#
# Gradiant Linear Boosting
#
######


gbm_df = gbm(log(Purchase) ~ 
               log(Avg_Prod_Price) +
               log(Avg_User_Spend) + 
               log(avg_diff_from_m) +
               
               Age_N  + 
                  Occupation + 
               City_Category + 
               Marital_Status + 
                   Years_In_City + 
               PC_1  +
               per_diff_from_m + 
               Total_PC1_User_Bought +
               Above_Per +
               #     Num_of_PC_entered +
               sign_of_avg_diff_from_m 
             #   log(Avg_Prod_Price) 
             #Avg_User_Spend # + PC_2Alt
             
             , df_train_2 %>% filter(Outlier_Purchase == 0),
             distribution = 'gaussian',
             n.trees = 3000, interaction.depth = 2, shrinkage = 0.1
)

summary(gbm_df)

df_test_2$gbm_Pred_value =  exp(predict(gbm_df, newdata = df_test_2, n.trees = 3000))

df_train_2$gbm_Pred_value = exp(predict(gbm_df, newdata = df_train_2, n.trees = 3000))


sqrt(mean((df_train_2$gbm_Pred_value - df_train_2$Purchase)^2))
#2495
sqrt(mean((df_test_2$gbm_Pred_value - df_test_2$Purchase)^2))
#2543


df_test_2 =
  inner_join(
    df_test_2,
    df_train_2 %>% group_by(PC_1) %>% summarise(mv = mean(Purchase)) %>% ungroup(),
    by = 'PC_1'
  ) %>%
  mutate(
    gbm_Pred_value = ifelse(gbm_Pred_value <100, mv, gbm_Pred_value)
        ) %>%
  select( -mv)

sqrt(mean((df_test_2$gbm_Pred_value - df_test_2$Purchase)^2))

#2537


######
#
# Random Forest 
#
######

t_train_sample = sample(1:nrow(df_train_2 %>% filter(Outlier_Purchase == 0)), 30000, replace = F)



rf_df = randomForest(Purchase ~ 
                      Avg_Prod_Price +
                      Avg_User_Spend + 
                      avg_diff_from_m +
                       
                       Age_N  + 
                       Occupation + 
                       City_Category + 
                       Marital_Status + 
                       Years_In_City + 
                       PC_1  +
                       per_diff_from_m + 
                   #    Total_PC1_User_Bought +
                       Above_Per +
                       #Num_of_PC_entered +
                       sign_of_avg_diff_from_m,
                     data = df_train_2  %>% filter(Outlier_Purchase == 0) ,
                     subset = t_train_sample,
                     importance = T,
                     ntree = 700)
varImpPlot(rf_df)

df_test_2$rf_Pred_value = predict(rf_df, newdata = df_test_2)                             

df_train_2$rf_Pred_value = predict(rf_df, newdata = df_train_2 )                            




sqrt(mean((df_train_2$rf_Pred_value - df_train_2$Purchase)^2))
#2470.2
#2514
sqrt(mean((df_test_2$rf_Pred_value - df_test_2$Purchase)^2))
#2567.2
#2591

######
#
# Error Evaluation 
#
######


sqrt(mean((df_test_2$lm_Pred_value - df_test_2$Purchase)^2))
sqrt(mean((df_test_2$gbm_Pred_value - df_test_2$Purchase)^2))
sqrt(mean((df_test_2$rf_Pred_value - df_test_2$Purchase)^2))

sqrt(mean(( (
  df_test_2$rf_Pred_value +
    df_test_2$gbm_Pred_value +
    df_test_2$lm_Pred_value)/3
  - df_test_2$Purchase)^2))


######
#
# Knn - yea, this is just a bad method
#
######


df_train_2_hot = predict(
  dummyVars(~ Purchase + 
              Avg_Prod_Price +
              Avg_User_Spend + 
              avg_diff_from_m +
              
              Age_N  + 
                 Occupation + 
              City_Category + 
              Marital_Status + 
                  Years_In_City + 
              PC_1  +
              per_diff_from_m + 
              Total_PC1_User_Bought +
              Above_Per +
               Num_of_PC_entered +
              sign_of_avg_diff_from_m, 
              data = df_train_2),
  newdata =  df_train_2) %>%
  data.frame()

df_test_2_hot = predict(
  dummyVars(~Purchase + 
              Avg_Prod_Price +
              Avg_User_Spend + 
              avg_diff_from_m +
              
              Age_N  + 
                 Occupation + 
              City_Category + 
              Marital_Status + 
                  Years_In_City + 
              PC_1  +
              per_diff_from_m + 
              Total_PC1_User_Bought +
              Above_Per +
               Num_of_PC_entered +
              sign_of_avg_diff_from_m,
            data = df_test_2),
  newdata =  df_test_2 ) %>%
  data.frame() 


df_train_2_hot = df_train_2_hot[,intersect(names(df_train_2_hot), names(df_test_2_hot))]
df_test_2_hot = df_test_2_hot[,intersect(names(df_train_2_hot), names(df_test_2_hot))]



knn_df = FNN::knn.reg(train = df_train_2_hot[df_train_2$Outlier_Purchase == 0 ,setdiff(names(df_train_2_hot), 'Purchase')],
                      y = df_train_2_hot$Purchase,
                      test = df_train_2_hot[,setdiff(names(df_train_2_hot), 'Purchase')],
                      k = 15)
df_train_2$knn_Pred_value = knn_df$pred
plot(df_train_2_hot$Purchase, knn_df$pred)


knn_df_test = FNN::knn.reg(train = df_train_2_hot[df_train_2$Outlier_Purchase == 0 ,setdiff(names(df_train_2_hot), 'Purchase')],
                      test = df_test_2_hot[,setdiff(names(df_test_2_hot), 'Purchase')],
                      y = df_train_2_hot$Purchase,
                      k = 15)


df_test_2$knn_Pred_value = knn_df_test$pred


sqrt(mean((df_train_2$knn_Pred_value - df_train_2$Purchase)^2))
#2463.7
sqrt(mean((df_test_2$knn_Pred_value - df_test_2$Purchase)^2))
#2539



######
#
# Populating the test set to submit
#
######



df_final = dt %>%
  left_join(
    df_train_2 %>%
      select(User_ID, 
             Avg_User_Spend, 
             Med_User_Spend,
             Total_User_Bought, 
             Above_Per, 
             per_diff_from_m, 
             avg_diff_from_m,
             sign_of_avg_diff_from_m) %>%
      distinct(),
    by = 'User_ID' 
  ) %>%
  mutate(
    Avg_User_Spend = ifelse(is.na(Avg_User_Spend),0, Avg_User_Spend),
    Med_User_Spend = ifelse(is.na(Med_User_Spend),0, Med_User_Spend),
    Total_User_Bought = ifelse(is.na(Total_User_Bought), 0,Total_User_Bought),
    Above_Per = ifelse(is.na(Above_Per), 0.5, Above_Per),
    per_diff_from_m = ifelse(is.na(per_diff_from_m), 0, per_diff_from_m),
    avg_diff_from_m = ifelse(is.na(avg_diff_from_m),0,avg_diff_from_m),
    sign_of_avg_diff_from_m = ifelse(is.na(sign_of_avg_diff_from_m), 0,sign_of_avg_diff_from_m )
  ) %>%
  left_join(
    df_train_2 %>%
      select(User_ID,
             PC_1,
             Total_PC1_User_Bought) %>%
      distinct(),
    by = c('User_ID','PC_1')
  ) %>%
  mutate(Total_PC1_User_Bought = ifelse(is.na(Total_PC1_User_Bought),0,Total_PC1_User_Bought),
         PC_1 = factor(PC_1)) %>%
  
  left_join(
    df_train_2 %>%
      select(Product_ID,Avg_Prod_Price,Med_Prod_Price,Total_Prod_Bought) %>%
      distinct(),
    by = 'Product_ID'
  ) %>%
  mutate(
    Avg_Prod_Price = ifelse(is.na(Avg_Prod_Price),0, Avg_Prod_Price),
    Med_Prod_Price = ifelse(is.na(Med_Prod_Price),0, Med_Prod_Price),
    Total_Prod_Bought = ifelse(is.na(Total_Prod_Bought),0, Total_Prod_Bought)
  ) %>%
  inner_join(
    dt %>%
      group_by(User_ID) %>%
      summarise(Num_of_PC_entered = n_distinct(PC_1)) %>%
      ungroup(),
    by = 'User_ID'
  ) %>%
  



df_final$lm_Pred_value = exp(predict(lm_df, newdata = df_final))
df_final$gbm_Pred_value = exp(predict(gbm_df, newdata = df_final, n.tree = 3000))
df_final$rf_Pred_value = predict(rf_df, newdata = df_final)


df_final =
  inner_join(
    df_final,
    df_train_2 %>% group_by(PC_1) %>% summarise(mv = mean(Purchase)) %>% ungroup(),
    by = 'PC_1'
  ) %>%
  mutate(
    lm_Pred_value = ifelse(lm_Pred_value == 0, mv, lm_Pred_value),
    gbm_Pred_value = ifelse(gbm_Pred_value == 0, mv, gbm_Pred_value)) %>%
  select( -mv)

df_final_output = df_final %>% mutate(Purchase = (lm_Pred_value + gbm_Pred_value + rf_Pred_value)/3 ) %>%
        select(User_ID, Product_ID, Purchase)



write_csv(df_final_output, "Classes/Analytics Vidhya/submittion.csv")