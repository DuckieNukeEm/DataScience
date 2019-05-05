#############################################
########         Chapter 8           ########
########          Applied            ########
########                             ########
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
set.seed(1)
boston.train <- sample(1:nrow(bos), nrow(bos)/2)


vprint("7. In the lab, we applied random forests to the Boston data using mtry=6
        and using ntree=25 and ntree=500. Create a plot displaying the test
       error resulting from random forests on this data set for a more comprehensive
       range of values for mtry and ntree. You can model your
       plot after Figure 8.10. Describe the results obtained.")
      result.data <- as.data.frame(matrix(0, ncol = 3, nrow = 1000))    
      names(result.data) <- c("MSE","mtry","tree.size")
      mtry.sample <- sample(1:13, 1000, replace = T)
      tree.sample <- sample(100:2000, 1000, replace = T)
      boston.test = bos$medv[-boston.train]
      i = 1
      train.bos = bos[boston.train,]
      test.bos = bos[-boston.train,]
      for (i in 1:1000)
      {
        vprint(i)
        rf.boston = randomForest(medv~.,data=train.bos,  mtry = mtry.sample[i], ntree=tree.sample[i])
      yhat.bag = predict(rf.boston, test.bos)
      
      result.data[i,] = c(mean((yhat.bag-boston.test)^2), mtry.sample[i], tree.sample[i])
      }
      
      #intrestingly enough, the range of tree's doesn't have as much of an impact as the number of variables used
      #in fact, I'de say it has a very low impact on the MSE, the MSE is really affected by mtry
      
  vprint("8. In the lab, a classification tree was applied to the Carseats data set after
converting Sales into a qualitative response variable. Now we will
         seek to predict Sales using regression trees and related approaches,
         treating the response as a quantitative variable.
         (a) Split the data set into a training set and a test set.")
          vprint("loading the ISLR package, and carseat data set")
    library(ISLR)
         cs <- data.frame(Carseats)
         #craeting a new variable for Carseats that will say if the data was high or low
         cs$High = factor(ifelse(Carseats$Sales  <= 8, "No", "Yes"))
          cs.train <- sample(1:nrow(cs), nrow(cs)/2)
         vprint("(b) Fit a regression tree to the training set. Plot the tree, and interpret
         the results. What test error rate do you obtain?")
         tree.cs = tree(High~.,data = cs, subset = cs.train)
         summary(tree.boston)
         
         vprint("Let's plot what we have")
         plot(tree.boston)
         text(tree.boston, pretty = 0)

         vprint("(c) Use cross-validation in order to determine the optimal level of
         tree complexity. Does pruning the tree improve the test error
         rate?
         (d) Use the bagging approach in order to analyze this data. What
         test error rate do you obtain? Use the importance() function to
         determine which variables are most important.
         334 8. Tree-Based Methods
         (e) Use random forests to analyze this data. What test error rate do
         you obtain? Use the importance() function to determine which
         variables are most important. Describe the effect of m, the number
         of variables considered at each split, on the error rate
         obtained.
         ")    
      

