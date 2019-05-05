#############################################
########         Chapter 8           ########
########         Boosting            ########
########          8.3.4              ########
#############################################


verbose = TRUE
#This is the print function for the verbose
vprint <- function(...){
  if(verbose) {print(...)}
}


vprint("Loading the package the contains the Trees")
  library(tree)

vprint("Loading the gbm package for boostability")
  library(gbm)

vprint("loading the MASS package, and using the Boston data set")
  library(MASS)
  bos <- Boston

vprint("creating a training set with the Boston Data")
  set.seed(1)
  boston.train <- sample(1:nrow(bos), nrow(bos)/2)

vprint("Doing boosting")  
  boost.bos = gbm(medv~., data = Boston[boston.train,], distribution = "gaussian", n.trees = 5000, interaction.depth = 4)
  #recall that boosting iterativly grows successive trees based on the remaind from the previous tree
  #distribtuion = gaussian is for regression
  #distribution = bernoulli is for classification
  
vprint("What do we have")    
  summary(boost.bos)
  #can see the relative influence of each variable
  
vprint("testing it's predictive nature")
  yhat.bag = predict(boost.bos, bos[-boston.train,], n.tree = 5000)
  boston.test = bos$medv[-boston.train]
  plot(yhat.bag, boost.bos)
  abline(0,1)
  mean((yhat.bag-boston.test)^2)
  
 
vprint("adjusting the shrinkage parmater")   
  boost.bos = gbm(medv~., data =bos[boston.train, ], distribution = "gaussian", n.trees = 5000, interaction.depth = 4, shrinkage = 0.2, verbose =F)
  yhat.bag = predict(boost.bos, bos[-boston.train,], n.tree = 5000)
  boston.test = bos$medv[-boston.train]
  plot(yhat.bag, boost.bos)
  abline(0,1)
  mean((yhat.bag-boston.test)^2)
