################
################
##
## Chapter 4: Over-Fitting and Model Tuning
##					Computing
##
###############
###############

####################################################
#
# Data Splitting
#
####################################################

	#load the required package and data
		library(AppliedPredictiveModeling)
		data(twoClassData)
	
	#the predictors are stored in a data frame called predictors
	#the outcome classes are containted in the vector classes
		str(predictors)
		str(classes)
		
	#we can normal use sample to sampe, but we will use
	#createDataPartition in CARET for a stratisfied sample
		library(caret)
		set.seed(1)
		trainingRows = createDataPartition(classes, 
																			p=0.8,
																			list = FALSE)
		head(trainingRows)
		
		#now we are going to subset each df
			trainingPredictors = predictors[trainingRows,]
			trainingClass = classes[trainingRows]
			
			testPredictors = predictors[-trainingRows,]
			testClasses = classes[-trainingRows]
		#to generate a test set using maximum dissimilarity sampling
		#the caret function maxdissim can be used to sequentially 
		#sample the data
		
####################################################
#
# Resampling
#
####################################################				
	
	#the carat package has various functions for data splitting
	# createDataParitition can do this with times being populated
			set.seed(1)
			repeatedSplits = createDataPartition(trainingClass, 
																					p = 0.80,
																					times = 3)
			str(repeatedSplits)
			
	#Similarlly, caret has the following function
			#createResample (for bootstrapping)
			#createFolds (for k-fold cross validation)
			#createMultiFolds (For repeated cross validation)
			
		cvSplits = createFolds(trainingClass, 
													 k = 10,
													 returnTrain = T)
		str(cvSplits)
		fold1 = cvSplits[[1]] #get the first set of data
		
		cvPredictors = trainingPredictors[fold1,]
		cvClasses1 = trainingClass[fold1]
		nrow(trainingPredictors)
		nrow(cvPredictors)
		
####################################################
#
# Basic Model Building in R
#
####################################################				
	
	#R has two prediciton interface
		#1 a formula interface Y~x+z+d+f bla bla bla, not really efficnet
		#matrix style where you pass in a matrix of indepndent vars and  a
		#vector of dpen vars
		
	#lets look at the later
		trainingPredictors = as.matrix(trainingPredictors)
		knnFit = knn3(x = trainingPredictors, y =trainingClass, k = 5)
		knnFit
		
	#now to predictg
		testPredictions = predict(knnFit, 
															newdata = testPredictors,
															type = "class")
		head(testPredictions)
		str(testPredictions)
		
####################################################
#
# Determination of Tuning Parameters
#
####################################################				

	#while the e1071 packages as the tune function (for 4 typs of medels)
	#and the errorest function in ipred can do singime model estimation
	#we will focust on the train function in caret
	
	#loading library and dataset
		library(caret)
		data("GermanCredit")
		
	#there i s a bit of handwaving going on here, and the authors
	#point us to the GermanCreditTrain and GermanCreditTEst in the
	#AppliedPredictiveModeling packages
		library(AppliedPredictiveModeling)
		data('GermanCreditTrain')
		
		
		#yea.....that shits not there at all, fine! Working blind
		#first, lets run the basic training model
			set.seed(1056)
			svmFit = train(Class ~.,
										 data = GermanCreditTrain,
										 method = 'svmRadial')
			
		#okay, lets now preproces the data by centering it and scaling it
		#we will use the preProc arguement
			svmFit = train(Class~.,
										 data = GermanCreditTrain,
										 method = 'svmRadial',
										 preProc = c("center","scale"))
		#now lets add a cost function (the TuneLneght = 10)
		#would make the cost value from 2^-2...2^7 
			svmFit = train(Class ~.,
										 data = GermanCreditTrain,
										 method = 'svmRadial',
										 preProc = c("center","scale"),
										 tuneLength = 10)
		#Now, we can also control how we want the samples to be choosen
		#by using the traincontl arugemnt
			svmFit = train(Class ~.,
										 data = GermanCreditTrain,
										 method = 'svmRadial',
										 preProc = c("center","scale"),
										 tuneLength = 10,
										 trControl = trainControl(method = 'repeatedcv',
										 												 repeats = 5))
		#so, how does our model looks?
			plot(svmFit, scales = list(x=list(log=2)))
			
			
		#Now here is somethign really cool, we can compare similiar modesl
		#and if we use the same tuining parmaets, CARET willl do it for use :)
			#building another model
				logisticReg = train(Class~.,
														data = GermanCreditTrain,
														method = 'glm',
														trControl = trainControl(method = "repeatedcv",
																										 repeats = 5))
				
			#now compraing the two models (tahkns caret :)
				resamp = resamples(list(SVM = svmFit, Logistic = logisticReg))
				summary(resamp)
			#and now, to see how they differ
				modelDiffrence = diif(resamp)
				summary(ModelDiffrence)
				