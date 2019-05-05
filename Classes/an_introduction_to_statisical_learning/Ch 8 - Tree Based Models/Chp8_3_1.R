#############################################
########         Chapter 8           ########
######## fitting classification tree ########
########          8.3.1              ########
#############################################


verbose = TRUE
#This is the print function for the verbose
vprint <- function(...){
  if(verbose) {print(...)}
}


#all data sets are contained in trees

vprint("Loading the package the contains the Trees")
    library(tree)

vprint("loading the ISLR package, and carseat data set")
    library(ISLR)
    cs <- data.frame(Carseats)
  #craeting a new variable for Carseats that will say if the data was high or low
    cs$High = factor(ifelse(Carseats$Sales <= 8, "No", "Yes"))
    
vprint("doing a tree to predicted High based on everything but sales")
  tree.carseats = tree(High~.-Sales, data = cs)
  
vprint("let's explore what the Tree prduced shall we?")
  summary(tree.carseats)
  #we see that 7 variables were actually used,
  #with 27 nodes built
  #and an error training rate of 9% ("misclassification error rate)

vprint("Let's plot this MOFO")  
  plot(tree.carseats)
  text(tree.carseats, pretty = 0)
    #pretty defines how many chars of the factor text values to show (0 = all)
  
#So far so good, however, the ebst way to ealuate IF a tree was very very good at what it did or very very bad
#we must not depened on the misclassification rate, we must build two sets, a training set, and 
# a test set, build the tree on the trainingi set, then run it on the test set to see truly how it worked out
# basically we are trying to avoide overfitting
  
#the predict function can help use with this
  
vprint("Now creating a training & test set")
  set.seed(2)
  cs.train = sample(1:nrow(cs), 200) #selecting 200 randome rows from cs
  cs.test = cs[-cs.train,]
    
vprint("building a tree on a trained test set and testing it's predictive powers")
  tree.carseats = tree(High~.-Sales, data=cs, subset = cs.train)
  tree.predict = predict(tree.carseats, cs.test, type = "class") #"class" tells r to return the correct number of predicitec classes 
    
vprint("Lets see how well we did")  
  table(tree.predict, cs$High[-cs.train])
  #tree.predict No Yes
  #         No  86  27
  #         Yes 30  57
  # (86+57)/(86+57+30+27) = 0.715
  #71.5% was corectled classified
  
  
  
#Lets now see if pruning the tree will help improve accuracy
#cv.tree will do the cross validation, 
# and prune.misclass will be the function that actuall does the prunning
  
vprint("figuring out how many nodes we need in the tree")
  set.sed(3)
  cv.carseats = cv.tree(tree.carseats, FUN =prune.misclass)
  cv.carseats
  #dev is cross-validation error rate,
  #looks like 9 nodes provideds the lowest dev,
  #you can see it in the following plots
  par(mfrow = c(1,2))
  plot(cv.carseats$size, cv.carseats$dev, type ="b")
  plot(cv.carseats$k, cv.carseats$dev, type ="b")
  
vprint("time to prune this tree")  
  par(mfrow = c(1,1))
  prune.carseats = prune.misclass(tree.carseats, best=9)
  plot(prune.carseats)
  text(prune.carseats, pretty =0)
  
  
#but how well did the pruning do? Again, we need to test this on our test data set
vprint("testing how well the prune tree worked")  
  tree.predict = predict(prune.carseats, cs.test, type = "class") #"class" tells r to return the correct number of predicitec classes 
  table(tree.predict, cs$High[-cs.train])
  #(94+60)/200 = 77
  #Hot damn, we improved it by 6% points!
  

  