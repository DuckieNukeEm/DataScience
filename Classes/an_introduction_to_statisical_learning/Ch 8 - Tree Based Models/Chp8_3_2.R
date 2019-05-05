#############################################
########         Chapter 8           ########
########   fitting Regression tree   ########
########          8.3.2              ########
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

vprint("creating a training set with the Boston Data")
  set.seed(1)
  boston.train <- sample(1:nrow(bos), nrow(bos)/2)
  
vprint("building the boston tree on the trianing set")
  tree.boston = tree(medv~.,data = bos, subset = boston.train)
  summary(tree.boston)
  
vprint("Let's plot what we have")
  plot(tree.boston)
  text(tree.boston, pretty = 0)
  
vprint("Let's see if prunning the tree will help...even though we didnt
       test it against the test set")
  cv.boston = cv.tree(tree.boston) #we are using regression, hence no class
  plot(cv.boston$size, cv.boston$dev, type="b")
  #nope

vprint("if we wanted to prune, this is what we would do")
  #notice the prune.tree instead of prune.misclass
  prune.boston = prune.tree(tree.boston, best = 5)
  plot(prune.boston)
  text(prune.boston, pretty = 0)
   
vprint("What the Hell. Let's cross validate for poops n chuckles")
  pred.boston = predict(tree.boston, newdata = bos[-boston.train,])
  boston.test = bos$medv[-boston.train]
  plot(pred.boston, boston.test)
  abline(0,1)
  #let's calculate the mean squared error
  mean((pred.boston - boston.test)^2)
  # The sqrt root will tell use average diveregence
  sqrt(mean((pred.boston - boston.test)^2))
    #this says that, on average, the prediction comes within 5K of the true value
  #or the mean absolute error
  mean(abs(((pred.boston - boston.test))))
  