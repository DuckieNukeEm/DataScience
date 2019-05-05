###lodaing needed packages
  require("ggplot2")
  source("./Code/Git/DataScience/DataScience/Kaggle/SpeedDating/R Code/Speed Dating Functions.R")
###load the Kaggle Data Set
 data = read.csv("./Data/Kaggle Data/SpeedDating/Speed Dating Data.csv")
 
####Looking at various immages
####Originally, I thought we had to split hte ddata set between waves 6~10 and the other waves
####but the reserachers already took care of that for us :)
  data_100 = data
  
 ### So, what do YOU value
 par(mfrow = c(2,3))
 sapply(c("attr1_1","sinc1_1","intel1_1","fun1_1","amb1_1","shar1_1"), function(x) q_hist(data_100,x))
 
  #by gender
  par(mfrow = c(2,6))
  sapply(c("attr1_1","sinc1_1","intel1_1","fun1_1","amb1_1","shar1_1"), function(x) q_hist(data_100[data_100$gender == 0, ] ,x, col ="pink", freq = F))
  sapply(c("attr1_1","sinc1_1","intel1_1","fun1_1","amb1_1","shar1_1"), function(x) q_hist(data_100[data_100$gender == 1, ] ,x, col ="pink", freq = F))
 
 
 
####SO, what does YOUR DATE Value
  par(mfrow = c(2,3))
   sapply(c("attr2_1","sinc2_1","intel2_1","fun2_1","amb2_1","shar2_1"), function(x) q_hist(data_100,x))
 
   #by Gender
    par(mfrow = c(6,2))
    sapply(c("attr2_1","sinc2_1","intel2_1","fun2_1","amb2_1","shar2_1"), function(x) q_hist(data_100[data_100$gender == 0, ] ,x, col ="pink", freq = F, mainmore = "Females"))
    sapply(c("attr2_1","sinc2_1","intel2_1","fun2_1","amb2_1","shar2_1"), function(x) q_hist(data_100[data_100$gender == 1, ] ,x, col ="light blue", freq = F, mainmore = "Males"))
 
### How do YOU Measure up?
    par(mfrow = c(2,3))
    sapply(c("attr3_1","sinc3_1","intel3_1","fun3_1","amb3_1"), function(x) q_hist(data_100,x,breaks = 1:10))
    
    #by Gender
    par(mfrow = c(2,5))
    sapply(c("attr3_1","sinc3_1","intel3_1","fun3_1","amb3_1"), function(x) q_hist(data_100[data_100$gender == 0, ] ,x, col ="pink", freq = F, breaks = 1:10, mainmore = "Females"))
    sapply(c("attr3_1","sinc3_1","intel3_1","fun3_1","amb3_1"), function(x) q_hist(data_100[data_100$gender == 1, ] ,x, col ="light blue", freq = F, breaks = 1:10, mainmore ="Males"))
    
###How do YOU Think other PRECIEVE YOU
    ####SO, what does YOUR DATE Value
    par(mfrow = c(2,3))
    sapply(c("attr5_1","sinc5_1","intel5_1","fun5_1","amb5_1"), function(x) q_hist(data_100,x, breaks = 1:10))
    
    #by Gender
    par(mfrow = c(2,5))
    sapply(c("attr5_1","sinc5_1","intel5_1","fun5_1","amb5_1"), function(x) q_hist(data_100[data_100$gender == 0, ] ,x, col ="pink", breaks = 1:10, freq = F, mainmore = "Femalse"))
    sapply(c("attr5_1","sinc5_1","intel5_1","fun5_1","amb5_1"), function(x) q_hist(data_100[data_100$gender == 1, ] ,x, col ="light blue", breaks = 1:10, freq = F, mainmore = "Males"))
    
    
#### Let's take a look at how you think and how your partner think
 sapply(c("attr2_1","sinc2_1","intel2_1","fun2_1","amb2_1","shar2_1"), function(x) q_hist(data_10,x,breaks=seq(0,40,2)))
 sapply(c("attr2_1","sinc2_1","intel2_1","fun2_1","amb2_1","shar2_1"), function(x) q_hist(data_100,x))
 
 par(mfrow = c(2,6))
 sapply(c("attr2_1","sinc2_1","intel2_1","fun2_1","amb2_1","shar2_1"), function(x) q_hist(data_100[data_100$gender == 0, ] ,x, col ="pink", freq = F))
 sapply(c("attr2_1","sinc2_1","intel2_1","fun2_1","amb2_1","shar2_1"), function(x) q_hist(data_100[data_100$gender == 1, ] ,x, col ="pink", freq = F))
 
 
####Lets see how you comparisosn to what you think other people compare bob
  #for data_10
    par(mfrow = c(2,3))
    q_hist(data_100[data_100$gender==0,], "attr1_1")
 
### And how do you stack up?
 par(mfrow=c(2,2))
 sapply(c("attr3_1","sinc3_1","intel3_1","fun3_1"), function(x) q_hist(data_10,x,breaks = seq(0,10,2)))
 sapply(c("attr3_1","sinc3_1","intel3_1","fun3_1"), function(x) q_hist(data_100,x,breaks = seq(0,10,2)))
 
 
 
 
 
 
  #####Cleaning dat
  na_count = data.frame(apply(data,2,function(x) sum(is.na(x))))
  na_count$value = rownames(na_count)
  cd = data[,na_count[na_count[,1] <= 500,2]]
  rcd = cd[complete.cases(cd),]
  rrcd=rcd[,setdiff(names(rcd),c("field","undergra","mn_sat","tuition","from","zipcode","income","career","dec","dec_o"))]
  rrcd$randomid = runif(nrow(rrcd))
  train_data = rrcd[rrcd$randomid<=0.6,]
  test_data = rrcd[rrcd$randomid > 0.8,]
  cv_data = rrcd[rrcd$randomid <= 0.8 & rrcd$randomid > 0.6,]
  rf = randomForest(match ~., data = train_data, importance = T)
  gl = glm(match~., data = rrcd,family = "binomial")
  cv_data$predict = predict(rf,cv_data)
  table(cv_data$match, ifelse(cv_data$predict >= 0.5,1,0))
  cv_data$predict = predict(gl,cv_data)
  table(cv_data$match, ifelse(cv_data$predict >= 0.5,1,0))
  
  
  #### trying some clusting analysis
  t = data[,c("attr1_1","sinc1_1","intel1_1","fun1_1","amb1_1","shar1_1", "attr2_1","sinc2_1","intel2_1","fun2_1","amb2_1","shar2_1","attr3_1","sinc3_1","intel3_1","fun3_1","amb3_1","attr5_1","sinc5_1","intel5_1","fun5_1","amb5_1", "match")]
  t$rand_id = runif(nrow(t), min = 0 , max = 1)
  t_train = t[t$rand_id <= 0.2, -ncol(t)]
  t_train = data.frame(apply(t_train, 2, remove_na))
	names(t_train) = names(t[,-ncol(t)])
  t_train_std = data.frame(apply(t_train, 2, standardize))
  names(t_train_std) = names(t_train)
  t_train_std$match = t_train$match
  t_dist = dist(t_train_std)
  t_hclust = hclust(t_dist, "ward.D2")
  plot(t_hclust)
  t_train_std$cluset = cutree(t_hclust,3)
  t_train$cluster = cutree(t_hclust,3)
  aggregate(t_train, by = list(t_train$cluster), mean)
  
  library(sparcl)
  ColorDendrogram(t_hclust, y = cutree(t_hclust,3), labels = names(cutree(t_hclust,3)), main = "My Simulated Data", 
  								branchlength = 80)