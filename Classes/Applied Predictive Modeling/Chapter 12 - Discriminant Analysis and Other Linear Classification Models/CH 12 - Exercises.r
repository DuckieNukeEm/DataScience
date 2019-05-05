################
################
##
## Chapter 12: Discriminant Analysis and other linear classifcations
##                      		Excercises
##
###############
###############

####
#
# 12.1
#
####

# The hepatic injury data set was described in the introductory chapter
# and contains 281 unique compounds, each of which has been classified as
# causing no liver damage, mild damage, or severe damage (Fig. 1.2). These
# compounds were analyzed with 184 biological screens (i.e., experiments) to
# assess each compound’s effect on a particular biologically relevant target in
# the body. The larger the value of each of these predictors, the higher the
# activity of the compound. In addition to biological screens, 192 chemical
# fingerprint predictors were determined for these compounds. Each of these
# predictors represent a substructure (i.e., an atom or combination of atoms
# within the compound) and are either counts of the number of substructures
# or an indicator of presence or absence of the particular substructure. The
# objective of this data set is to build a predictive model for hepatic injury so
# that other compounds can be screened for the likelihood of causing hepatic
# injury. Start R and use these commands to load the data:
#   > library(caret)
#   > library(AppliedPredictiveModeling)
#   > data(hepatic)
#   > # use ?hepatic to see more details
#   
#   The matrices bio and chem contain the biological assay and chemical fingerprint
# predictors for the 281 compounds, while the vector injury contains
# the liver damage classification for each compound.
# (a) Given the classification imbalance in hepatic injury status, describe how
# you would create a training and testing set.
    'I would keep the ratios the same in both sets, however, Ill be using class weights when
    I build the models so that more importance is given to the imbalanced class as for the balanced class'
# (b) Which classification statistic would you choose to optimize for this exercise
# and why?
    ' I would use the AUC to determine class predicitve mdoel, as it doesnt depened on prior probabilities'
  

# (c) Split the data into a training and a testing set, pre-process the data,
# and build models described in this chapter for the biological predictors
# and separately for the chemical fingerprint predictors. Which model has
# the best predictive ability for the biological predictors and what is the
# optimal performance? Which model has the best predictive ability for
# the chemical predictors and what is the optimal performance? Based on
# these results, which set of predictors contains the most information about
# hepatic toxicity?
    
    
    ####
    #
    #Cleaning the data
    #
    ####
    data(hepatic)
    r_bio = numeric(0)
    for( i in 1:length(names(bio))){
      if(length(unique(bio[,i])) < 2) {r_bio = c(r_bio, i)}
      
    }
    r_chem = numeric(0)
    for( i in 1:length(names(chem))){
      if(length(unique(chem[,i])) <2 ) {r_chem = c(r_chem,i)}
      
    }
    
    bio_cor = cor(bio[,names(bio)[-r_bio]])
    highcorr = findCorrelation(bio_cor, 0.5)
    r_bio = names(bio)[-c(highcorr, r_bio)]
    
    chem_cor = cor(chem[,names(chem)[-r_chem]])
    highcorr = findCorrelation(chem_cor, 0.5)
    r_chem = names(chem)[-c(highcorr, r_chem)]
    
    
    
    ####
    #
    #creating and splitting the data set
    #
    ####
    library(AppliedPredictiveModeling)
    data(hepatic)
    df = cbind(injury, bio[,r_bio], chem[,r_chem])
    df_weights = ifelse(df$injury == 'Mild', 1.9379 , ifelse(df$injury == 'None', 2.650,9))
    train = createDataPartition(df$injury, p = 0.8)[[1]]
    
    ####
    #
    #rscaling and spliting the data
    #
    ####
 
        df_scale = df
        df_scale[,setdiff(names(df),"injury")] = scale(df[, setdiff(names(df),"injury")])
       
        
        df_training = df_scale[train,]
        df_test = df_scale[-train,]
       
                        
    ####
    #
    #Building my Control Model now
    #
    ####
    ctrl = trainControl(
                     method = 'LGOCV',
                      classProbs = T
        #             summaryFunction = multiClassSummary
                      )
    #####
    #
    # Running GLM
    #
    #####
    
      glm_train = train(bio[train,r_bio],
                        y = injury[train],
                        method = 'glm',
                        metric = 'Kappa',
                        weights = df_weights[train],
                        trControl = ctrl)
      glm_model = glm(injury ~., data = df_training[,c("injury", r_bio)], family = 'binomial')
      
      train(x = bio[train,r_bio], 
            y = injury[train],
            method = "sparseLDA",
            tuneGrid = expand.grid(lambda = c(.1),
                                   NumVars = c(1:20, 50, 75, 100, 250, 500, 750, 1000)),
            preProc = c("center", "scale"),
            metric = "ROC")
      
      library(rms)
      lrm_train = lrm(injury~., data = df_training[,setdiff(c("injury",r_bio),"Z170")], weights = df_weights[train])
    #####
    #
    # Running as glmnet model - BIO
    #
    #####  
      library(glmnet)
      glmnet_model = cv.glmnet(x = as.matrix(bio[train, r_bio]),
                            y = injury[train],
                            weights = df_weights[train],
                            family = 'multinomial')
           cbind(injury[-train],  predict(glmnet_model ,
                                     newx = as.matrix(bio[-train, r_bio]),
                                     s = glmnet_model$lambda.min,
                                     type = "class"))
           
           confusionMatrix(data = injury[-train],
                           predict(glmnet_model ,
                                   newx = as.matrix(bio[-train, r_bio]),
                                   s = glmnet_model$lambda.min,
                                   type = "class"),
                            positive = "Severe")
      
    #####
    #
    # Running LDA - BIO
    #
    #####  
      LDA_train = train( bio[train,r_bio],
                         y = injury[train],
                        preProc = c("center","scale"),
                        weights = df_weights[train],                
                        method = 'lda',
                        metric = 'Kappa',
                        trControl = ctrl)
      
      
      
      cbind(injury[-train],  predict(LDA_train ,
                                     newdata = bio[-train, r_bio],
                                     type = "raw"))
      
      confusionMatrix(data = injury[-train],
                      predict(LDA_train ,
                              newdata = bio[-train, r_bio],
                              type = "raw"),
                      positive = "Severe")
      #####
      #
      # Running PLSD - BIO
      #
      #####  
      PLSD =train( bio[train,r_bio],
                   y = injury[train],
0                   preProc = c("center","scale"),
                   weights = df_weights[train],  
                   tuneGrid = expand.grid(.ncomp = 1:10),
                   method = 'pls',
                   metric = 'Kappa',
                   trControl = ctrl)
    #####
    #
    # Shrunke Method
    #
    #####
      nscFit <- train(bio[train,r_bio],
                      y = injury[train],
                      method = "pam",
                      preProc = c("center", "scale"),
                      tuneGrid = data.frame(threshold = seq(0, 25, length = 30)),
                      metric = "ROC",
                      trControl = ctrl)
      
    cbind(injury[-train],predict(PLSD, newdata = bio[-train, r_bio] ))
    
    confusionMatrix(data = injury[-train],
                    predict(PLSD, newdata = bio[-train, r_bio] ),
                    positive = "Severe")
# (d) For the optimal models for both the biological and chemical predictors,
# what are the top five important predictors?
        varImp(PLSD)
# (e) Now combine the biological and chemical fingerprint predictors into one
# predictor set. Retrain the same set of predictive models you built from
# part (c). Which model yields best predictive performance? Is the model
# performance better than either of the best models from part (c)? What
# are the top five important predictors for the optimal model? How do these
# compare with the optimal predictors from each individual predictor set?
'pretty damn well'
# (f) Which model (either model of individual biology or chemical fingerprints
# or the combined predictor model), if any, would you recommend using to
# predict compounds’ hepatic toxicity? Explain.
'PLDR just cuase'
####
#
# 12.2
#
####

# In Exercise 4.4, we described a data set which contained 96 oil samples
# each from one of seven types of oils (pumpkin, sunflower, peanut, olive,
# soybean, rapeseed, and corn). Gas chromatography was performed on each
# sample and the percentage of each type of 7 fatty acids was determined. We
# would like to use these data to build a model that predicts the type of oil
# based on a sample’s fatty acid percentages.
      library(caret)
      data(oil)
# (a) Like the hepatic injury data, these data suffer from extreme imbalance.
# Given this imbalance, should the data be split into training and test sets?
    'no, there is not enough data in the rows to actually perform a split on the data'
    'however, we will attempt an upsample to try and determine the outcome'
# (b) Which classification statistic would you choose to optimize for this exercise
# and why?
    'AUC as it will take into account a lot of the information'
# (c) Of the models presented in this chapter, which performs best on these
# data? Which oil type does the model most accurately predict? Least
# accurately predict?
    
    df = data.frame(upSample(fattyAcids, y = oilType, yname = 'Oil'))
    
    glmnet_model = glmnet(Oil ~.,
                             data = df,
                             family = 'multinomial')
        
    LDA_train = train( Oil ~.,
                       data =df,
                       preProc = c("center","scale"),
                       method = 'lda',
                       metric = 'ROC',
                       trControl = ctrl)
    
    PLSD =train( Oil ~.,
                data =df,
                preProc = c("center","scale"),
                tuneGrid = expand.grid(.ncomp = 1:10),
                method = 'pls',
                metric = 'Kappa',
                trControl = ctrl)
    glm_train = train(Oil ~.,
                      data = df,
                      method = 'glm',
                      metric = 'Kappa',
                      trControl = ctrl)
    
    nscFit <- train(Oil ~.,
                    data = df,
                    method = "pam",
                    preProc = c("center", "scale"),
                    tuneGrid = data.frame(threshold = seq(0, 25, length = 30)),
                    metric = "ROC",
                    trControl = ctrl)
    
    
####
#
# 12.3
#
####

# The web site17 for the MLC++ software package contains a number of
# machine learning data sets. The “churn” data set was developed to predict
# telecom customer churn based on information about their account. The data
# files state that the data are “artificial based on claims similar to real world.”
# The data consist of 19 predictors related to the customer account, such
# as the number of customer service calls, the area code, and the number of
# minutes. The outcome is whether the customer churned.

# The data are contained in the C50 package and can be loaded using:
#   > library(C50)
#   > data(churn)
#   > ## Two objects are loaded: churnTrain and churnTest
#   > str(churnTrain)
#   > table(churnTrain$Class)

# (a) Explore the data by visualizing the relationship between the predictors
# and the outcome. Are there important features of the predictor data
# themselves, such as between-predictor correlations or degenerate distributions?
# Can functions of more than one predictor be used to model the
# data more effectively?
  'see attached R Code'
# (b) Fit some basic models to the training set and tune them via resampling.
# What criteria should be used to evaluate the effectiveness of the models?
 
# (c) Use lift charts to compare models. If you wanted to identify 80% of the
# churning customers, how many other customers would also be identified?
