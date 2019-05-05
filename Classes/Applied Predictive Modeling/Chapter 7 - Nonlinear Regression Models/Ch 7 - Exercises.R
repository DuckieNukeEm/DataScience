################
################
##
## Chapter 7: NonLinear Regression Models                           
##                Excercises
##
###############
###############

####
#
# 7.1
#
####

# Simulate a single predictor and a nonlinear relationship, such as a sin
# wave shown in Fig. 7.7, and investigate the relationship between the cost, epsilon,
# and kernel parameters for a support vector machine model:
    set.seed(100)
    x <- runif(100, min = 2, max = 10)
    y <- sin(x) + rnorm(length(x)) * .25
    sinData <- data.frame(x = x, y = y)
    plot(x, y)
    ## Create a grid of x values to use for prediction
    dataGrid <- data.frame(x = seq(2, 10, length = 100))
    
# (a) Fit different models using a radial basis function and different values of
# the cost (the C parameter) and epsilon. Plot the fitted curve. For example:
      library(kernlab)
      rbfSVM <- ksvm(x = x, y = y, data = sinData,
                      kernel ="rbfdot", kpar = "automatic",
                      C = 10000, epsilon = .0001)
      modelPrediction <- predict(rbfSVM, newdata = dataGrid)
       ## This is a matrix with one column. We can plot the
       ## model predictions by adding points to the previous plot
       points(x = dataGrid$x, y = modelPrediction[,1],
               type = "l", col = "green")
     ## Try other parameters

# (b) The σ parameter can be adjusted using the kpar argument, such as
# kpar = list(sigma = 1). Try different values of σ to understand how this
# parameter changes the model fit. How do the cost, epsilon, and σ values affect
# the model?
       
       plot(x, y)
       
       rbfSVM <- ksvm(x = x, y = y, data = sinData,
                      kernel ="rbfdot", kpar = list(sigma = 4000),
                      C = 1, epsilon = .1
                      )
       modelPrediction <- predict(rbfSVM, newdata = dataGrid)

       points(x = dataGrid$x, y = modelPrediction[,1],
              type = "l", col = "green")
####
#
# 7.2
#
####       
       
       
# Friedman (1991) introduced several benchmark data sets create by simulation.
# One of these simulations used the following nonlinear equation to
# create data:
#       y = 10 sin(πx1x2) + 20(x3 − 0.5)2 + 10x4 + 5x5 + N(0, σ2)
# where the x values are random variables uniformly distributed between [0, 1]
# (there are also 5 other non-informative variables also created in the simulation).
# The package mlbench contains a function called mlbench.friedman1 that
# simulates these data:
    library(mlbench)
    set.seed(200)
    trainingData <- mlbench.friedman1(200, sd = 1)
    trainingData$x <- data.frame(trainingData$x)
    featurePlot(trainingData$x, trainingData$y)
    ## or other methods.
    ## This creates a list with a vector 'y' and a matrix
    ## of predictors 'x'. Also simulate a large test set to
    ## estimate the true error rate with good precision:
    testData <- mlbench.friedman1(5000, sd = 1)
    testData$x <- data.frame(testData$x)
    #Tune several models on these data. For example:
    library(caret)
     knnModel <- train(x = trainingData$x,
                        y = trainingData$y,
                        method = "knn",
                        preProc = c("center", "scale"),
                        tuneLength = 10)
    knnModel
   
    knnPred <- predict(knnModel, newdata = testData$x)
    ## The function 'postResample' can be used to get the test set
    ## perforamnce values
     postResample(pred = knnPred, obs = testData$y)
     
     
     ###
     # Nueral Net
     ###
     ctrl <- trainControl(method = "cv", number = 10)
     
     
      tooHigh = findCorrelation(cor(trainingData$x))
      trainX = ifelse(length(tooHigh) > 0 , trainingData$x[,-tooHigh],trainingData$x)
      nnetGrid <- expand.grid(.decay = c(0, 0.01, .1),
                               .size = c(1:10))
      nnetTune <- train(trainingData$x, trainingData$y,
                         method = "nnet",
                         tuneGrid = nnetGrid,
                         trControl = ctrl,
                         preProc = c("center", "scale"),
                         linout = TRUE,
                         trace = FALSE,
                         MaxNWts = 10 * (ncol(trainingData$x) + 1) + 10 + 1,
                         maxit = 500)
      
        v = predict(nnetTune, testData$x)
         defaultSummary(data.frame(pred = v, obs = testData$y)  )
         
      ###
      #MARS
      ###
         marsGrid <- expand.grid(.degree = 1:2, .nprune = 2:38)
        set.seed(100)
         marsTuned <- train(trainingData$x, trainingData$y,
                               method = "earth",
                             
                                 tuneGrid = marsGrid,
                               trControl = trainControl(method = "cv"))
         varImp(marsTuned)
         v = predict(marsTuned, testData$x)
         defaultSummary(data.frame(pred = v, obs = testData$y)  )
         sqrt(mean((v - testData$y)^2))
     ###
     #SVM
     ### 
         svmRTuned <- train(trainingData$x, trainingData$y,
                             method = "svmRadial",
                             preProc = c("center", "scale"),
                             tuneLength = 14,
                             trControl = trainControl(method = "cv")) 
         v = predict(svmRTuned, testData$x)
         defaultSummary(data.frame(pred = v, obs = testData$y)  )
     # Which models appear to give the best performance? Does MARS select the
     # informative predictors (those named X1–X5)?
         'Mars actualled did a pretty damn good job and did select the right variables'
         
 
####
#
# 7.3
#
####       

# For the Tecator data described in the last chapter, build SVM, neural
# network, MARS, and KNN models. Since neural networks are especially sensitive
# to highly correlated predictors, does pre-processing using PCA help the
# model?
  library(caret)
         data(tecator)
         #absorp
         #endpoints
  train_v = sample(nrow(absorp), 0.8*nrow(absorp))
  train_x = data.frame(absorp[train_v,], y=  endpoints[train_v,1])
  test_x = data.frame(absorp[-train_v,], y=  endpoints[-train_v,1])
      ###
      #KNN 
      ###
          knnModel <- train(y ~.,
          									#x =  train_x,
                            #y = train_y,
          									data = train_x,
                            method = "knn",
                            preProc = c("center", "scale"),
                            tuneLength = 10)
         
          
          postResample(pred = predict(knnModel, newdata =  test_x), obs = test_x$y)
      
      
      ###
      # Nueral Net
      ###
              ctrl <- trainControl(method = "cv", number = 10)
              
              nnetGrid <- expand.grid(.decay = c(0, 0.01, .1),
                                      .size = c(1:10),)
              nnetTune_pca <- train( y ~.,
             										data = train_x,
                                method = "nnet",
                                tuneGrid = nnetGrid,
                                trControl = ctrl,
                                preProc = c("center", "scale","pca"),
                                linout = TRUE,
                                trace = FALSE,
                                MaxNWts = 10 * (ncol(train_x)) + 10 + 1,
                                maxit = 500)
              
           
              defaultSummary(data.frame(pred = predict(nnetTune_pca,  test_x), obs = test_x$y)  )
              
        ###
        # Nueral Net - non PCA
        ###
              # ctrl <- trainControl(method = "cv", number = 10)
              # 
              # nnetGrid <- expand.grid(.decay = c(0, 0.01, .1),
              #                         .size = c(1:10),
              # 												.bag = F)
              # nnetTune <- train( y ~.,
              # 									 data = train_x,
              # 									 method = "avNNet",
              # 									 tuneGrid = nnetGrid,
              # 									 trControl = ctrl,
              # 									 preProc = c("center", "scale"),
              # 									 linout = TRUE,
              # 									 trace = FALSE,
              # 									 MaxNWts = 10 * (ncol(train_x)) + 10 + 1,
              # 									 maxit = 500)
              # 
              # 
              # defaultSummary(data.frame(pred = predict(nnetTune,  test_x), obs = test_x$y)  )
              
      ###
      #MARS
      ###
      marsGrid <- expand.grid(.degree = 1:2, .nprune = 2:38)
      set.seed(100)
      marsTuned <- train(y~.,
      									 data = train_x,
                         
                         method = "earth",
                         
                         tuneGrid = marsGrid,
                         trControl = trainControl(method = "cv"))
      varImp(marsTuned)
 			mars_pred = data.frame(pred =  predict(marsTuned,  test_x), obs = test_x$y)
 			names(mars_pred) = c("pred","obs")
      defaultSummary(mars_pred)
     
      ###
      #SVM
      ### 
      svmRTuned <- train(y ~.,
                         data = train_x,
                         method = "svmRadial",
                         preProc = c("center", "scale"),
                         tuneLength = 14,
                         trControl = trainControl(method = "cv")) 
      
      defaultSummary(data.frame(pred =  predict(svmRTuned,  test_x), obs = test_x$y)  )
      
  
  
  
  
####
#
# 7.4
#
####     

# Return to the permeability problem outlined in Exercise 6.2. Train several
# nonlinear regression models and evaluate the resampling and test set
# performance.
      library(AppliedPredictiveModeling)
      data(permeability)
      #absorp
      #endpoints
      fingerprints[,-nearZeroVar(fingerprints)]
      
      train_v = sample(nrow(fingerprints), 0.8*nrow(fingerprints))
      train_x = data.frame(fingerprints[train_v,-nearZeroVar(fingerprints)], y=  permeability[train_v])
      test_x = data.frame(fingerprints[-train_v,-nearZeroVar(fingerprints)], y=  permeability[-train_v])
      ###
      #KNN 
      ###
			      knnModel <- train(y ~.,
			      									#x =  train_x,
			      									#y = train_y,
			      									data = train_x,
			      									method = "knn",
			      									preProc = c("center", "scale"),
			      									tuneLength = 10)
			      
			      postResample(pred = predict(knnModel, newdata =  test_x), obs = test_x$y)
			      
      ###
      # Nueral Net
      ###
			      ctrl <- trainControl(method = "cv", number = 10)
			      nnetGrid <- expand.grid(.decay = c(0, 0.01, .1),
			      												.size = c(1:10))
			      nnetTune_pca <- train( y ~.,
			      											 data = train_x,
			      											 method = "nnet",
			      											 tuneGrid = nnetGrid,
			      											 trControl = ctrl,
			      											 preProc = c("center", "scale","pca"),
			      											 linout = TRUE,
			      											 trace = FALSE,
			      											 MaxNWts = 10 * (ncol(train_x)) + 10 + 1,
			      											 maxit = 500)
			      
			      defaultSummary(data.frame(pred = predict(nnetTune_pca,  test_x), obs = test_x$y)  )
			      
      ###
      # Nueral Net - non PCA
      ###
#       			ctrl <- trainControl(method = "cv", number = 10)
# 			      nnetGrid <- expand.grid(.decay = c(0, 0.01, .1),
# 			      												.size = c(1:10))
# 			      nnetTune_ <- train( y ~.,
# 			      										data = train_x,
# 			      										method = "nnet",
# 			      										tuneGrid = nnetGrid,
# 			      										trControl = ctrl,
# 			      										preProc = c("center", "scale"),
# 			      										linout = TRUE,
# 			      										trace = FALSE,
# 			      										MaxNWts = 10 * (ncol(train_x)) + 10 + 1,
# 			      										maxit = 500)
# 			      
# 			      
# 			      defaultSummary(data.frame(pred = predict(nnetTune,  test_x), obs = test_x$y)  )
# 			      
      ###
      #MARS
      ###
		      marsGrid <- expand.grid(.degree = 1:2, .nprune = 2:38)
		      set.seed(100)
		      marsTuned <- train(y~.,
		      									 data = train_x,
		      									 method = "earth",
		      									 tuneGrid = marsGrid,
		      									 trControl = trainControl(method = "cv"))
		      varImp(marsTuned)
		      mars_pred = data.frame(pred =  predict(marsTuned,  test_x), obs = test_x$y)
		      names(mars_pred) = c("pred","obs")
		      defaultSummary(mars_pred)
		      
      ###
      #SVM
      ### 
		      svmRTuned <- train(y ~.,
		      									 data = train_x,
		      									 method = "svmRadial",
		      									 preProc = c("center", "scale"),
		      									 tuneLength = 14,
		      									 trControl = trainControl(method = "cv")) 
		  
		      defaultSummary(data.frame(pred =  predict(svmRTuned,  test_x), obs = test_x$y)  )
		      
		      
		     print('knn') 
		      postResample(pred = predict(knnModel, newdata =  test_x), obs = test_x$y)
		     print('nnet_PCA')
		     	defaultSummary(data.frame(pred = predict(nnetTune_pca,  test_x), obs = test_x$y)  )
		     print('MARS')
		     defaultSummary(mars_pred)
		     print('SVM')
		      defaultSummary(data.frame(pred =  predict(svmRTuned,  test_x), obs = test_x$y)  )
		      
      
      
      
      
# (a) Which nonlinear regression model gives the optimal resampling and test
# set performance?
			'The model that gives the best performance is SVM, with an Rsquared of 55%'
# (b) Do any of the nonlinear models outperform the optimal linear model you
# previously developed in Exercise 6.2? If so, what might this tell you about
# the underlying relationship between the predictors and the response?
		'yes, the SVM does, which means theere is quiet a sqgwiggly linear boundy'
# (c) Would you recommend any of the models you have developed to replace
# the permeability laboratory experiment?
		'damn straight!'


####
#
# 7.5
#
####       


# Exercise 6.3 describes data for a chemical manufacturing process. Use
# the same data imputation, data splitting, and pre-processing steps as before
# and train several nonlinear regression models.
		library(AppliedPredictiveModeling)
		data(ChemicalManufacturingProcess)
		df = ChemicalManufacturingProcess
		for(i in 1:ncol(ChemicalManufacturingProcess)){
			print(c(i,mean(df[,i], na.rm = T)	))
			df[is.na(df[,i]),i] = mean(df[,i], na.rm = T)	
			
		}
		names(df) = c("y", names(df)[2:ncol(df)])
		
		train_v = sample(nrow(df), 0.8*nrow(df))
		train_x = data.frame(df[train_v,])
		test_x = data.frame(df[-train_v,])
		###
		#KNN 
		###
		knnModel <- train(y ~.,
											#x =  train_x,
											#y = train_y,
											data = train_x,
											method = "knn",
											preProc = c("center", "scale"),
											tuneLength = 10)
		
		postResample(pred = predict(knnModel, newdata =  test_x), obs = test_x$y)
		
		###
		# Nueral Net
		###
		ctrl <- trainControl(method = "cv", number = 10)
		nnetGrid <- expand.grid(.decay = c(0, 0.01, .1),
														.size = c(1:10))
		nnetTune_pca <- train( y ~.,
													 data = train_x,
													 method = "nnet",
													 tuneGrid = nnetGrid,
													 trControl = ctrl,
													 preProc = c("center", "scale","pca"),
													 linout = TRUE,
													 trace = FALSE,
													 MaxNWts = 10 * (ncol(train_x)) + 10 + 1,
													 maxit = 500)
		
		defaultSummary(data.frame(pred = predict(nnetTune_pca,  test_x), obs = test_x$y)  )
		 			      
		###
		#MARS
		###
		marsGrid <- expand.grid(.degree = 1:2, .nprune = 2:38)
		set.seed(100)
		marsTuned <- train(y~.,
											 data = train_x,
											 method = "earth",
											 tuneGrid = marsGrid,
											 trControl = trainControl(method = "cv"))
		varImp(marsTuned)
		mars_pred = data.frame(pred =  predict(marsTuned,  test_x), obs = test_x$y)
		names(mars_pred) = c("pred","obs")
		defaultSummary(mars_pred)
		
		###
		#SVM
		### 
		svmRTuned <- train(y ~.,
											 data = train_x,
											 method = "svmRadial",
											 preProc = c("center", "scale"),
											 tuneLength = 14,
											 trControl = trainControl(method = "cv")) 
		
		defaultSummary(data.frame(pred =  predict(svmRTuned,  test_x), obs = test_x$y)  )
		
		
		print('knn') 
		postResample(pred = predict(knnModel, newdata =  test_x), obs = test_x$y)
		print('nnet_PCA')
		defaultSummary(data.frame(pred = predict(nnetTune_pca,  test_x), obs = test_x$y)  )
		print('MARS')
		defaultSummary(mars_pred)
		print('SVM')
		defaultSummary(data.frame(pred =  predict(svmRTuned,  test_x), obs = test_x$y)  )
		
		
		
		
# (a) Which nonlinear regression model gives the optimal resampling and test
# set performance?
	'MARS'
# (b) Which predictors are most important in the optimal nonlinear regression
# model? Do either the biological or process variables dominate the
# list? How do the top ten important predictors compare to the top ten
# predictors from the optimal linear model?
			varImp(marsTuned)
			'and it;s mostly based on manufacturing data'
# (c) Explore the relationships between the top predictors and the response for
# the predictors that are unique to the optimal nonlinear regression model.
# Do these plots reveal intuition about the biological or process predictors
# and their relationship with yield? 
    
                        'yulp'