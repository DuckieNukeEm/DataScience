#############################################
########         Chapter 8           ########
########  Bagging and Random Forest  ########
########          8.3.3              ########
#############################################


verbose = TRUE
#This is the print function for the verbose
vprint <- function(...){
  if(verbose) {print(...)}
}


vprint("Loading the package the contains the Trees")
  library(tree)

vprint("loading the MASS package, and using the Boston data set")
  library(MASS)
  bos <- Boston

vprint("Loading the random forest package")
  library(randomForest)

vprint("creating a training set with the Boston Data")
  set.seed(2)
  boston.train <- sample(1:nrow(bos), nrow(bos)/2)

vprint("Doing Bagging Right now")  
  bag.boston = randomForest(medv~.,data = bos,  subset = boston.train, mtry = 13, importance = TRUE)
  #mtry = 13, which tells randomForest to consider 13 variables at each try.
  #bc the boston set only has 13 variables, it's going do a full consideration of all variables at the junction
  #thus, making it a bagging performance

vprint("What do we have")    
  bag.boston
  
vprint("testing it's predictive nature")
  yhat.bag = predict(bag.boston, bos[-boston.train,])
  boston.test = bos$medv[-boston.train]
  plot(yhat.bag, boston.test)
  abline(0,1)
  mean((yhat.bag-boston.test)^2)
  
  
vprint("let's change the number of tree's built")  
  bag.boston = randomForest(medv~.,data=bos, subset = boston.train, mtry=13,ntree=25)
  yhat.bag = predict(bag.boston, newdata=bos[-boston.train,])
  plot(yhat.bag, boston.test)
  abline(0,1)
  mean((yhat.bag-boston.test)^2)
  
vprint("and changing the number of variables used (this will shift from bagging to random forest")  
  rf.boston = randomForest(medv~.,data=bos, subset = boston.train, mtry = 6, importance =T)
  yhat.bag = predict(rf.boston, newdata=bos[-boston.train,])
  plot(yhat.bag, boston.test)
  abline(0,1)
  mean((yhat.bag-boston.test)^2)

vprint("lets see the importance of each variable")    
  importance(rf.boston)
  varImpPlot(rf.boston)
