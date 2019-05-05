################
################
##
## Chapter 5: Measuing PErformance in Classificaiton Models
##					Computing
##
###############
###############

####################################################
#
# Performance evaluations
#
####################################################
  
  #Loading Data  
    library(AppliedPredictiveModeling)
    set.seed(975)
    simulatedTrain = quadBoundaryFunc(500)
    simulatedTest = quadBoundaryFunc(1000)
    
    head(simulatedTrain)
  
    #going to use randomForest and qiadratic discriminat models to fit the data
      library(randomForest)
      rfModel = randomForest(class ~ X1 + X2,
                             data = simulatedTrain,
                             ntree = 2000)
      
      library(MASS)
      qdaModel= qda(class ~ X1 + X2,
                    data = simulatedTrain)
    #when predicting with QDA, the output has two lists, one is class (where it tell you the class)
    #the other is posterior, or the probabiltity
      qdaTrainPred = predict(qdaModel, simulatedTrain)
      names(qdaTrainPred)
      head(qdaTrainPred$class)
      head(qdaTrainPred$posterior)
      
      qdaTestPred = predict(qdaModel, simulatedTest)
      simulatedTrain$QDAprob = qdaTrainPred$posterior[,"Class1"]
      simulatedTest$QDAprob = qdaTestPred$posterior[,"Class1"]
	
    #to get the same information from random forest, we need to use the type = prob arguement
      rfTestPred = predict(rfModel, simulatedTest, type = "prob")
      head(rfTestPred)
      
      simulatedTest$RFprob = rfTestPred[,"Class1"]
      simulatedTest$rfclass = predict(rfModel, simulatedTest)
      
      
####################################################
#
# Sensitivity and Specificity
#
####################################################
      
  #Caret has a function for computing sensitivity and specifcity (now they tell me, bloody hell)
    library(caret)
    sensitivity(data = simulatedTest$rfclass,
                reference = simulatedTest$class,
                positive = "Class1")
    
    specificity(data = simulatedTest$rfclass,
                reference = simulatedTest$class,
                positive = "Class1")

    #we can also computer other values
      posPredValue(data = simulatedTest$rfclass,
                   reference = simulatedTest$class,
                   positive = "Class1")
      
      negPredValue(data = simulatedTest$rfclass,
                   reference = simulatedTest$class,
                   positive = "Class2")
      #changing the prevelaince manually
      posPredValue(data = simulatedTest$rfclass,
                   reference = simulatedTest$class,
                   positive = "Class1",
                   prevalence = 0.9)
      
####################################################
#
# Confusion Matrx
#
####################################################     
    
  #the confusionMatrix in caret does just that
      caret::confusionMatrix(data=simulatedTest$rfclass,
                      reference = simulatedTest$class,
                      positive = "Class1")
  #this also works with more than two classes, noice
      
####################################################
#
# Reciever Operating Characteristic Curbes
#
####################################################      
  
  #the pROC package can create these curves and various stats
  #however, it is a little tricky, as you have to create the roc package first
    library(pROC)
    rocCurve = roc(response = simulatedTest$class,
                   predictor = simulatedTest$RFprob,
                   levels = rev(levels(simulatedTest$class))) #assumes the second level is of intrest, se 
                                                              # had to reverse it
    auc(rocCurve)
    ci(rocCurve)
    plot(rocCurve, legacy.axes = T)  
                  #by default, the x-axis goes backwards, used the option legacy.axes = t
                  # to get 10spec on the x-asix moving form 0 to 1
                  #also, another curve cn be added using the add=T the next time plot.auc is used
  
####################################################
#
# Lift Charts
#
####################################################       

    #the lift curve can be created from the lift function in the caret package
    #it takes a formula where the true class is on the left hand side
      
      labs = c(RFprob = "Random Forest",
               QDAprob = "Quadratic Discriminant Analysis")
      liftCurve = lift(class ~ RFprob + QDAprob, 
                       data = simulatedTest,
                       labels = labs)
      #NOW< TO PLOT 'EM
      xyplot(liftCurve,
             auto.key = list(columns = 2,
                             lines = T,
                             points = F))
      
####################################################
#
# Calibrating Probabilities                
#
####################################################   
  
  #We can use the caliration.plot in the PresenceAbsence package or the calibration function in caret
      calCurve = calibration(class ~ RFprob + QDAprob, data = simulatedTest)
      calCurve
      
      xyplot(calCurve, auto.key = list(columns = 2))
      
  #now we are going to calibrate using a logit regression 
  #NOTE the glm() function models the prob of the second factor level, so the function relevel() is used to
  # temporarily reverse the factors levels
      sigmoidalCal = glm(relevel(class, ref = "Class2") ~ QDAprob,
                         data = simulatedTrain,
                         family = binomial)
      coef(summary(sigmoidalCal))
      
      #now running the data through the reblance model to do the actual recallibration
      sigmoidProbs = predict(sigmoidalCal,
                             newdata = simulatedTest[,"QDAprob", drop = F],
                             type = "response")
      simulatedTest$QDAsigmoid = sigmoidProbs
      
      #now triyng naive bayes from the klaR package
      BayesCal = klaR::NaiveBayes(class ~ QDAprob,
                                  data = simulatedTrain,
                                  useKernal = T) #allows MB to be flexble
      #the predict function with NB also produces two list, the classs and the associate probs
      BayesProbs = predict(BayesCal,
                           newdata = simulatedTest[,"QDAprob", drop = F])
      
      simulatedTest$QDABayes = BayesProbs$posterior[,"Class1"]
      
      #checking the probs values before and after calibrations
      head(simulatedTest)
      
    #lets take a look atthe -p0t\
      calCurve2 <- calibration(class ~ QDAprob + QDABayes + QDAsigmoid,data = simulatedTest)
      xyplot(calCurve2)
      