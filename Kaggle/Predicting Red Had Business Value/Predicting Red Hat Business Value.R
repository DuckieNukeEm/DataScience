#####################
#
#Loading Required Packages
#
#####################


#https://www.analyticsvidhya.com/blog/2016/01/xgboost-algorithm-easy-steps/
#https://github.com/amunategui/BetterCrossValidation/blob/master/CrossValidation2.R

    require(xgboost)
    require(caret)
    require(dplyr)
    require(tidyr)
    require(data.table)
    require(ggplot2)
    require(FeatureHashing)
		library(e1071)
#####################
#
#Loading function to make my life just that much easier
#
#####################
  var_type = function(df, t = c("n","c","l"))  
  {
    if(length(t) > 1) {stop("give me an expreasion for me to use ya dingbat!")
    } else if(t == "c") { 
        var_list = names(df)[sapply(df, class) %in% c('factor','character')]
      
    } else if(t =='l') {
      var_list = names(df)[sapply(df, class) == 'logical']
      
    } else if(t=='n') {
        var_list = names(df)[sapply(df, class) %in% c('numeric','integer')]
    } else {var_list = NA}
    
    return(var_list)
      
  }

    

#####################
#
# Loading all the data
#
#####################
    
    df_test = fread('./Data/Kaggle Data/Predicting Red Hat Business Value/Zip/act_test.csv',sep = ",", header = T, stringsAsFactors = T )
    df = fread('./Data/Kaggle Data/Predicting Red Hat Business Value/Zip/act_train.csv',sep = ",", header = T, stringsAsFactors = T)
    peeps = fread('./Data/Kaggle Data/Predicting Red Hat Business Value/Zip/people.csv',sep = ",", header = T, stringsAsFactors = T)
    
    df$date = as.Date(df$date)
    df_test$date = as.Date(df_test$date)
    #df$outcome = df$outcome
    peeps$date = as.Date(peeps$date)
  	peeps$char_38 = as.integer(peeps$char_38)  
    
#####################
#
# quick peek at the data
#
#####################
    
    str(df_test)
    str(df)
    str(peeps)
    
    summary(df_test)
    summary(df)
    summary(peeps)
    
  #any duplicat peeps
    length(unique(peeps$people_id)) == nrow(peeps)
    
  #creating training data    
    df = data.frame(inner_join(df, peeps, by = 'people_id'))
    
    df_test = data.frame(inner_join(df_test, peeps, by ='people_id'))
   	
   #creating some new vars
    df$Days_to_activity = as.integer(df$date.x - df$date.y)
    df_test$Days_to_activity = as.integer(df_test$date.x - df_test$date.y)
    
    df$Days_from_start = as.integer(df$date.x - min(df$date.x))
    df_test$Days_from_start = as.integer(df_test$date.x - min(df_test$date.x))
  #did we loose any data
    nrow(df) == nrow(a_train)
    nrow(df_test) == nrow(a_test)
    
    
  #check out this little funky POS,
    df %>%	unite(jj, date.x, group_1) %>%
    	group_by(jj) %>%
    	summarise(WIN = round(mean(outcome),2), n_count = n() ) %>%
    	group_by(WIN) %>%
    	summarise(n_dist = n_distinct(jj), n_count = sum(n_count))
 
   #knowing that, we can easily take care of some of the test variables
    df_test$outcome =  left_join(df_test,
    														 df %>% select(date.x, group_1, outcome) %>% unique(),
    														 by = c("date.x","group_1")) %>% 
    										mutate(outcome = ifelse(is.na(outcome),2,outcome)) %>% 
    										select(outcome)
 
#####################
#
#Looking at each var in relation to the outcome
#
#####################
    to_outcome = function(x){
    	df %>% group_by_(x) %>%
    				summarise(WIN = mean(outcome), n_count = n())
    	
    }
    df %>% group_by(char_11) %>%
    	summarise(WIN = mean(outcome), n)
    
    
       
#####################
#
# creating a training set and test set
#
#####################
    #creating a test and training set 
    set.seed(103)
    #valid = 	unique(df_peeps)[sample(1:length(unique(df_peeps)), 30000)]
    valid = df %>% select(people_id) %>% distinct() %>% sample_n(30000, replace = F)
    valid$test = 1
    df = left_join(df, valid, by = "people_id")
    df[is.na(df$test), 'test'] = 0
    
 

#####################
#
# redicomg the number of factors in group 1
#
#####################    
    zo_factors = df %>% group_by(group_1) %>% 
				    			summarise(WIN = mean(outcome), n_count = n(), d_dates = n_distinct(date.x)) %>%
				    			filter(WIN %in% c(0,1) , n_count >= 3, d_dates >= 2) %>%
				    			distinct()
    			
    zo_factors = df %>% group_by(group_1) %>% 
    	summarise(WIN = mean(outcome), n_count = n()) %>%
    	filter(WIN %in% c(0,1) , n_count >= 3) %>%
    	distinct()
    
       
    
   
#####################
#
# preeping the data for mainframe use
#
#####################
   #preeping data
    xgb_hm_train = hashed.model.matrix(~. , data= df[df$test == 0 ,setdiff(names(df),c("outcome","date.x","date.y","group_1","people_id","activity_id"))], hash.size = 2^22)
    #xgb_hm_y = hashed.model.matrix(~. , data= df[df$test == 0 ,"outcome"], hash.size = 2^22)
    xgb_hm_test = hashed.model.matrix(~. , data= df[df$test == 1 ,setdiff(names(df),c("outcome","date.x","date.y","group_1","people_id","activity_id"))], hash.size = 2^22)
  	#making a data split  
    xgb_hm_train = xgb.DMatrix(xgb_hm_train, label = df[df$test==0,"outcome"])
    xgb_hm_test = xgb.DMatrix(xgb_hm_test, label = df[df$test==1,"outcome"])
    
  #if we want to test only on the date.x and group 1  
    xgb_hm_train = hashed.model.matrix(~. , data= df[df$test == 0 ,c("group_1","Days_from_start","activity_category")], hash.size = 2^22)
    xgb_hm_test = hashed.model.matrix(~. , data= df[df$test == 1  ,c("group_1","Days_from_start","activity_category")], hash.size = 2^22)
    #making a data split  
    xgb_hm_train = xgb.DMatrix(xgb_hm_train, label = df[df$test==0,"outcome"])
    xgb_hm_test = xgb.DMatrix(xgb_hm_test, label = df[df$test==1,"outcome"])
    
    
  #trying to reduce down the number of variables  
    df_r = df %>% select(outcome, date.x, group_1) %>% rename(datex = date.x) %>% distinct()
   df_r$test = runif(nrow(df_r))
   df_r$test = ifelse(df_r$test <= 0.8, 0 ,1)
   df_r$datex = factor(df_r$datex)
    xgb_hm_train = hashed.model.matrix(~. , data= df_r[df_r$test == 0 ,setdiff(names(df_r),c("outcome","test"))], hash.size = 2^22)
    #xgb_hm_y = hashed.model.matrix(~. , data= df[df$test == 0 ,"outcome"], hash.size = 2^22)
    xgb_hm_test = hashed.model.matrix(~. , data= df_r[df_r$test == 1 ,setdiff(names(df_r),c("outcome","test"))], hash.size = 2^22)
    #making a data split  
    xgb_hm_train = xgb.DMatrix(xgb_hm_train, label = df_r[df_r$test==0,"outcome"])
    xgb_hm_test = xgb.DMatrix(xgb_hm_test, label = df_r[df_r$test==1,"outcome"])
    
#####################
#
# running cv xgboost
#
#####################    
    param <- list(objective = "binary:logistic",
                  eval_metric = "auc",
                  booster = "gblinear",
    							max_depth = 4,
                  eta = 0.03)
    	gc() #clean up the goddamn garbage!
    #t = xgb.cv(params = param, data = xgb_rad_matrix, nrounds = 100,  nfold = 10)
    t = xgb.train(data = xgb_hm_train,
    							param, 
    							nrounds = 300, 
    							watchlist = list(model = xgb_hm_train, valid = xgb_hm_test),
    							early.stop.round = 2)
    
    t = xgb.cv(params = param, data = xgb_hm_test, nrounds = 100, nfold = 10, metrics = 'auc')

#####################
#
# running naive bayes
#
#####################
    						df$date.x = factor(df$date.x)
    						
    						train_control <- trainControl(method="cv", number=10)
    						grid <- expand.grid(.fL=c(0), .usekernel=c(FALSE))
    					# train the model
    						n_b_cv <- train(outcome ~ group_1 + datex, 
    													 					data=df_r %>% filter(test == 1),
    													 					trControl=train_control, 
    													 					method="nb")
    						
    n_b = 	naiveBayes(outcome ~ group_1 + date.x, data = df %>% filter(test == 0))
    df_t = df %>% filter(test == 1) %>%
    							mutate(act = ifelse(outcome == 1, 1, 0)) %>%
    							select(act)
    
    df_t$pred = predict(n_b, df %>% filter(test == 1))
    df_t$pred = as.numeric(df_t$pred) - 1
    
    
############
#    lets see if we can reduce the number of factors in group 1
############

    df %>% select(group_1, outcome) %>%
    			mutate(act = ifelse(outcome == 1, 1, 0)) %>%
    			group_by(group_1) %>%
    			summarise(WIN = round(mean(act),1), n_count = n() ) %>%
    			group_by(WIN) %>%
    			summarise(n_distinct = n_distinct(group_1), n_sum = sum(n_count), n_std = sd(n_count)) 
  
    
    df %>% select(date.x, outcome) %>%
    	mutate(act = ifelse(outcome == 1, 1, 0)) %>%
    	mutate(date.x = week(date.x)) %>%
    	group_by(date.x) %>%
    	summarise(WIN = round(mean(act),1), n_count = n() ) %>%
    	group_by(WIN) %>%
    	summarise(n_distinct = n_distinct(date.x), n_sum = sum(n_count), n_std = sd(n_count)) 
    
    
    df %>% select(date.x, outcome) %>%
    	mutate(act = ifelse(outcome == 1, 1, 0)) %>%
    	group_by(date.x) %>%
    	summarise(WIN = mean(act), n_count = n() ) %>%
    	ggplot(aes(x = date.x, y = WIN)) + geom_line()
    
    
    
    
    
    
    

#####################
#
# Partial Least Squares Discriminant analysis
#
#####################   
    
    library(caret)
     plsda(x = df[df$test == 0, setdiff(names(df),c("outcome","Days_from_start","date.y","people_id","activity_id"))],
     			y = factor(df[df$test == 0, "outcome"]),
     			scale = T,
     			probMethod = 'Bayes',
     			ncomp = 4)
    
    ctrl = trainControl(method = "LGCOV",
    										summaryFunction = twoClassSummary,
    										classProbs = T,
    										savePredictions = T
    										)
    
    plsFit = train(x =xgb_hm_train,
    							 y = factor(df[df$test ==0,"outcome"]),
    							 method = 'pls',
    							 tuneGrid = expand.grid(.ncomp = 1:10),
    							 preProc = c("center","scale"),
    							 metrix = "ROC",
    							 trControl = ctrl)