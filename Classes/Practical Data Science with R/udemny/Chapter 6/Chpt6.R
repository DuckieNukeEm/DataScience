#load('KDD2009.Rdata')

###
#defining some global level vars
###
outcome = 'churn'
pos = '1'

###
#lodaing data
###
  d <- read.table('orange_small_train.data.gz',header=T,sep = '\t', na.strings=c('NA','')) #base data
  churn = read.table('orange_small_train_churn.labels.txt', header=F,sep='\t') #Loading in a data for churn (customers to leave)
  appetency <- read.table('orange_small_train_appetency.labels.txt',header=F, sep='\t') #Loading data for appetency (Wbuy new products or services
  upselling = read.table('orange_Small_train_upselling.labels.txt', header = F, sep ='\t') #buy upgrades during sale for more profits


###
#joining the two new data sets to d
###
  d$churn = churn$V1
  d$appetency = appetency$V1
  d$upselling = upselling$V1
  outcomes = c('churn','appetency','upselling')

###
#setting the seed for reproducability
###
  set.seed(729375)

###
#setting random numbers to generating a training (training = train + calibration) and test set
###
  d$rgroup = runif(dim(d)[1])
  dTrainAll = subset(d, rgroup<=0.9)
  dTest = subset(d,rgroup>0.9)
  
  useForCal <- rbinom(n=dim(dTrainAll)[[1]], size = 1, prob = 0.1)>0
  dCal = subset(dTrainAll, useForCal)
  dTrain = subset(dTrainAll, !useForCal)
  
###
#figuring out which variables are catigorical or numerical variables
##
  vars <- setdiff(colnames(dTrainAll),c(outcomes,"rgroup"))
  catVars = vars[sapply(dTrainAll[,vars],class) %in% c('factor','character')]
  numericVars = vars[sapply(dTrainAll[,vars],class) %in% c('numeric','integer')]
  
###
#Cleanming up uneed variables
###
  rm(list=c('d','churn','appetency','upselling'))
  
  
  
#####
# 6.2 plotting churn grouped by Var 218
####
  table218 = table(Var218=dTrain[,'Var218'],
                  churn=dTrain[,outcome],
                  useNA='ifany')
  print(table218)
  print("Churn ratio")
  print(table218[,2]/(table218[,1]+table218[,2]))
  
  mkPredC = function(outCol,varCol,appCol){
    
    pPos <- sum(outCol==pos)/length(outCol) #how often a var is positve in training
    naTab <- table(as.factor(outCol[is.na(varCol)])) #for NA Factor A appread X times, Factor B appred Y times
    pPosWna <- (naTab/sum(naTab))[pos] #% of times NA appeared pos
    vTab <- table(as.factor(outCol),varCol) #for each non - NA Factor A appread X times, Factor B appred Y times
    pPosWv <- (vTab[pos,]+1.0e-3*pPos)/(colSums(vTab)+1.0e-3)  #% of times FActor A appeared pos
    pred <- pPosWv[appCol] #everytime appCol isn't NA, give it % of PposWv
    pred[is.na(appCol)] <- pPosWna #everytime appcol IS Na, give it % from pPosWna
    pred[is.na(pred)] <- pPos #Just incase anything is left over
    pred #fin
  }
  
###
# now using the function over all of our testing training data
###  
  for(v in catVars) {
    #for every row in each data set, we will apply the probability of each
    #it occuring catigorcal factor appearings 
    pi <- paste('pred',v,sep='')
    dTrain[,pi] <- mkPredC(dTrain[,outcome],dTrain[,v],dTrain[,v])
    dCal[,pi] <- mkPredC(dTrain[,outcome],dTrain[,v],dCal[,v])
    dTest[,pi] <- mkPredC(dTrain[,outcome],dTrain[,v],dTest[,v])
  }
  
###
#okay, great but we gotta test the predictive power of this using
#Area under the curve, this function will test it
###
  
  library('ROCR')
  calcAUC <- function(predcol,outcol) {
    perf <- performance(prediction(predcol,outcol==pos),'auc')
    as.numeric(perf@y.values)
  }
  
###
#now TO TEST
#if auc >= 0.8, need to test the calibration of it
###
  for (v in catVars){
    pi <- paste('pred',v,sep='')
    aucTrain <- calcAUC(dTrain[,pi],dTrain[,outcome])
      if(aucTrain >= 0.8)
      {aucCalc = calcAUC(dCal[,pi], dCal[,outcome])
      print(sprintf("%s, trainAUC: %4.3f calibrationAUC: %4.3f",
                    pi,aucTrain,aucCalc))
      }
    
  }

###
#Doesn't seem to work, but that's okay
###
    
  
######
#ONTO NUMERICAL VARIABLES
#####

###
#Going to cut each numeric variable into 10 groups, change them to factors and do the above
###
  
  mkPredN = function(outCol,varCol,appCol) {
    cuts = unique(as.numeric(quantile(varCol, probs = seq(0,1,0.1), na.rm = T)))
    varC = cut(varCol,cuts)
    appCol = cut(appCol, cuts)
    mkPredC(outCol, varC, appCol)
  }

    for(v in numericVars) {
      pi <- paste('pred',v,sep='')
      dTrain[,pi] <- mkPredN(dTrain[,outcome],dTrain[,v],dTrain[,v])
      dTest[,pi] <- mkPredN(dTrain[,outcome],dTrain[,v],dTest[,v])
      dCal[,pi] <- mkPredN(dTrain[,outcome],dTrain[,v],dCal[,v])
      aucTrain <- calcAUC(dTrain[,pi],dTrain[,outcome])
      if(aucTrain>=0.55) {
        aucCal <- calcAUC(dCal[,pi],dCal[,outcome])
        print(sprintf("%s, trainAUC: %4.3f calibrationAUC: %4.3f",
                      pi,aucTrain,aucCal))
      }
    }
  
###
#Looks good, now we want to do some cross validation
###
  fCross <- function() {
    useForCalRep <- rbinom(n=dim(dTrainAll)[[1]],size=1,prob=0.1)>0
    predRep <- mkPredC(dTrainAll[!useForCalRep,outcome],
                       dTrainAll[!useForCalRep,var],dTrainAll[useForCalRep,var])
    calcAUC(predRep,dTrainAll[useForCalRep,outcome])
  } 
    aucs <- replicate(100,fCross())
    

    
    
###
#BUILDING MODELS USING MANY VARIABLES 6.3
###
    