################
################
##
## Chapter 12: Discriminant Analysis and other linear classifcations
##					Computing
##
###############
###############

  #loading Base libraries
      load("grantData.RData")
      
      library(caret)
      library(doMC)
      registerDoMC(12)
      library(plyr)
      library(reshape2)
    #note, you have to create the f grandDAta, I'm surrpised they haven't created a link somewhere to just download that POS

  #so,two sets of data, the full set
      head(fullSet)
  #and the reduced set
      head(reducedSet)

####################################################
#
# Identifiy collineraties
#
####################################################
  
    #we can identify exterme collinarites by using the trim.matrix function, which takes a square, symmetric matrix
    #like...oh, I don't know, a COVARIANCE MATRIX
      #creating cov matrix
        reducedCovMat = cov(training[,reducedSet])
        
        library(subselect)
        trimmingResults = trim.matrix(reducedCovMat)
        names(trimmingResults)
        #see if any predictors were eliminated:
        trimmingResults$names.discarded
        
        #nothing, but when we apply it to the fulls et
        fullCovMat = cov(training[,fullSet])
        fullSetResults = trim.matrix(fullCovMat)
        fullSetResults$names.discarded
        
    #we can also use the findLinearCombos in the caret package, but it does not require a square matrix
        
####################################################
#
# Train Controll
#
####################################################     
      
      # In order to delveop these models, we need to control out train in caret works, thus we will use the traincontrol
      # functions, specifying to use class probabilites (classProbs) 
      # also, the twoClassSmmary function can calculate the ROC for us,
      #so
        crtl = trainControl(cummaryFunction = twoClassSummary,
                            classProbs = T)
        
      #now, we want to remove some sets of data, so the index argument of the trainControll can do this for us, and the method
        crtl = trainControl(method = "LGOCV",
                            summaryFunction = twoClassSummary,
                            classProbs = T,
                            index = list(TrainSet = pre2008))
      #amd finally, we can tell it to save the predictions
        crtl = trainControl(method = "LGOCV",
                            summaryFunction = twoClassSummary,
                            classProbs = T,
                            index = list(TrainSet = pre2008),
                            savePredictions = T)
        
####################################################
#
# Classifications using Logistic Regression
#
#################################################### 
    set.seed(476)
    # we will be using the GLM function in the base R package
      levels(training$Class)
      modelFit = glm(Class ~ Day,
                     data = training[pre2008,],
                     family = binomial)
      
      modelFit
      
    # so glm treates the second factors as the point of intrest, so an increase in day indicates an INCREASE
    # in uncessful grants, we need to take one minus to get the flip probability
      successProb = 1 - predict(modelFit,
                                newdata = data.frame(Day = c(10, 150, 300, 350)),
                                type = "response")
      successProb
    #Now going to add an non linearm tearm to the model
      daySquareModel = glm(Class ~ Day + I(Day^2),
                           data = training[pre2008,],
                           family = binomial)
      daySquareModel
      
    #We can also create a logit model with the rms packages(Regression Modeling Strategies)
      #the rcs is a spline
      library(rms)
      rcsFit = lrm(Class ~ rcs(Day), data = training[pre2008,])
      rcsFit
      
      #notice on the output how Day, Day', Day'', and Day''' all have very small p-values, thsi is highly
      #suggestive of needing to use those powers
      
      #so now, lets predict it
      dayProfile = Predict(rcsFit,
                           Day = 0:365,
                           #flip the prediction to get the model fo succesfful grants
                           fun = function(x) -x)
      plot(dayProfile, ylab = "Log Odds")
      
      
    #Now, what is nice is that we can use the train function if we want too :)
      #preppin data with squared terms
      training$Day2 =  training$Day^2
      fullSet = c(fullSet, "Day2")
      reducedSet = c(reducedSet, "Day2")
      
      library(caret)
      set.seed(476)
      lrfull = train(training[,fullSet],
                     y = training$Class,
                     method = "glm",
                     metric = "ROC",
                     trControl = crtl)
      lrfull
      
    #lets createa  model witha  smaller predictor set
      lrReduced = train(training[,reducedSet],
                        y = training$Class,
                        method = "glm",
                        metric = "ROC",
                        trControl = crtl)
      lrReduced
      
    #lets look at the predictions
      head(lrReduced$pred)
    #notice the varialbes paramater, when train saves predicition, it will aslo save the pramaeters
      
      #lets compute the confustion matrix
      confusionMatrix(data = lrReduced$pred$pred,
                      reference = lrReduced$pred$obs)
      
      #lets compute the ROC curve
      reducedRoc = roc(response = lrReduced$pred$obs,
                       predictor = lrReduced$pred$successful,
                       levels = rev(levels(lrReduced$pred$obs)))
      plot(reducedRoc, legacy.axes = T)
      
      auc(reducedRoc)
      
####################################################
#
# Linear Discriminant Analysisi            
#
####################################################
      
  #we will use the lda function in the MASS packages
      library(MASS)
    #but first, lets' scale and cetner the data
      grantPrePRocess = preProcess(training[pre2008, reducedSet])
      grantPrePRocess
      
      scaledPre2008 = predict(grantPrePRocess,
                              newdata = training[pre2008, reducedSet])
      scaled2008HoldOUt = predict(grantPrePRocess,
                                   newdata = training[-pre2008, reducedSet])
      
      ldaModel = lda(x = scaledPre2008,
                     grouping = training$Class[pre2008])
      
      head(ldaModel$scaling)
      
      ldaHoldOutPredictions <- predict(ldaModel, scaled2008HoldOUt)
      
    #the predicted class, posterior prob, and linear discriminat values are all contarined in the predicted model
    #however, when there are only two classes, there is no training that needs to be done to dtermine the optimal number
    #of Linear discirmant vectors, thus, we now turn to the train function
      ldaFit1 = train(x = training[, reducedSet],
                      y = training$Class,
                      method = 'lda',
                      preProc = c("center","scale"),
                      metric = "ROC",
                      trControl = crtl) 
      ldaFit1
      
      ldaTestClasses = predict(ldaFit1,
                               newdata = testing[,reducedSet])
      ldaTestProbs = predict(ldaTestClasses,
                             newdata = testing[,reducedSet],
                             type = "prob")
      
####################################################
#
# Partial LEast Squares Discriminan ANalysisi
#
####################################################
  #you can use pls function inthe pls packages do to PLSDA, the caret packages aslo has a similar function, plsda
    plsdaModel = plsda(x = training[pre2008, reducedSet],
                       y = training[pre2008, 'Class'],
                       #time to scale
                       scale = T,
                       #lets use Bayes method do calculate probabilitly
                       probMethod = "Bayes",
                       #the number of components
                       ncomp = 4)    
    plsPred = predict(plsdaModel,
                      newdata = training[-pre2008, reducedSet])
    head(plsPred)
      
    plsProbs = predict(plsdaModel,
                       newdata = training[-pre2008, reducedSet],
                       type = "prob")
    head(plsProbs)
    
    #again, we can use the train function
    plsFit2 <- train(x = training[, reducedSet],
                     y = training$Class,
                     method = "pls",
                     tuneGrid = expand.grid(.ncomp = 1:10),
                     preProc = c("center","scale"),
                     metric = "ROC",
                     trControl = crtl)
    #and now, variable imporatnac
      plsImpGrant = varImp(plsFit2, scale = F)
      plsImpGrant
      
    #and to plot it
      plot(plsImpGrant, top = 20, scales = list(y = list(cex = .95)))
      
####################################################
#
# Penalized Models                           
#
####################################################
      #glmnet can do pinalized mode,
      library(glmnet)
      glmnetModel = glmnet(x = as.matrix(training[,fullSet]),
                           y = training$Class,
                           family = 'binomial')
      #compute the prediction for threee diffrence levels of regularization
      predict(glmnetModel,
              newx = as.matrix(training[1:5, fullSet]),
              s = c(0.05, 0.1, 0.2),
              type = "class")
      
      #which predictors were used in the model
      predict(glmnetModel,
              newx = as.matrix(training[1:5, fullSet]),
              s = c(0.05, 0.1,0.2),
              type = "nonzero")
      
      #now lets use the a train packages
        #tuning parameters
      glmnGrid = expand.grid(.alpha = c(0,.1,.2,.4,.6,.8,1),
                             .lambda = seq(.01, .2, length = 40))
      
      glmnTuned = train(training[,fullSet],
                        y = training$Class,
                        method = "glmnet",
                        tuneGrid = glmnGrid,
                        prePRoc = c("center","scale"),
                        metric = "ROC",
                        trControl = crtl)
      
      #heatmap
      plot(glmnTuned, plotType= 'level')
      
  #now for peanlized LDA functions, you can either used the sparseLDA or the penalizedLDA packages, but the mian
  #function to use is sda in the spareLDA Package,
    #the ridfe is controlled by  lambda, with the laso penalty by the stop (positive is a hard number, negative is the number of test)
      library(sparseLDA)
      spareseLdaModel = sda(x = as.matrix(training[,fullSet]),
                            y = training$Class,
                            lambda = 0.1,
                            stop = -6)
      
   #The argument method = "sparseLDA" can be used with train.

####################################################
#
# Nearest SHurnken Centriods                 
#
####################################################   
    #we will beusing the pamr.train function in the pamr package
      inputData = list(x = t(training[,fullSet]), y = training$Class)
      
    #creating the model
      library(pamr)
      nscModel = pamr.train(data = inputData)
   #now to predict the model
      exampleData = t(training[1:5, fullSet])
      pamr.predict(nscModel, newx = exampleData, threshold = 5)
  #which paramaters were used at this threshold?
      thresh17cars = pamr.predict(nscModel, 
                                  newx = exampleData,
                                  threshold = 17,
                                  type = "nonzero")
      fullSet[thresh17Vars]
  ## We chose the specific range of tuning parameters here:
    nscGrid <- data.frame(.threshold = 0:25)
    set.seed(476)
    nscTuned <- train(x = training[,fullSet],
                           y = training$Class,
                           method = "pam",
                           preProc = c("center", "scale"),
                           tuneGrid = nscGrid,
                           metric = "ROC",
                           trControl = crtl)
  #now we can use the predictor function to see the predictors
    predictors(nscTuned)
  #as well as the variable importance
    carImp(nscTuned, scale = F)
