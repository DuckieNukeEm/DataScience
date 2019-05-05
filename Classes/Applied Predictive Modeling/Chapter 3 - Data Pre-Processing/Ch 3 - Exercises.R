###############
#
# Chapter 3: DAta pre-processing
#					Excercises
#
###############

# 
# 3.1. The UC Irvine Machine Learning Repository6 contains a data set related
# to glass identification. The data consist of 214 glass samples labeled as one
# of seven class categories. There are nine predictors, including the refractive
# index and percentages of eight elements: Na, Mg, Al, Si, K, Ca, Ba, and Fe.
# The data can be accessed via:
# 	> library(mlbench)
# 	> data(Glass)
# 	> str(Glass)
# 
# (a) Using visualizations, explore the predictor variables to understand their
# distributions as well as the relationships between predictors.
		require(ggplot2)
		require(mlbench)
		data(Glass)
		df = Glass
		
	#vissulizing each variable now
		hist(df$RI) 	#Gaussian wtih long right tail (log?)
		hist(df$Na)   #Gaussian
		hist(df$Mg)   #Bimodal at the extreemes
		hist(df$Al)   #gaussian
		hist(df$Si)   #Gaussian
		hist(df$K)    #Huge right tail, log
		hist(df$Ca)   #guassian iwth a long tabil
		hist(df$Ba)   #Hure right tail, log
		hist(df$Fe)   #Ditto
		plot(df$Type)
		boxplot(df[,1:9])
		element_cor = cor(df[,1:9])
		corrplot::corrplot(element_cor, order = 'hclust')
# (b) Do there appear to be any outliers in the data? Are any predictors skewed?
			#see above
		
# (c) Are there any relevant transformations of one or more predictors that
# might improve the classification model?
		log transform all the variable that have tails, then scale them approporalte would be a good first step

# 3.2. The soybean data can also be found at the UC Irvine Machine Learning
# Repository. Data were collected to predict disease in 683 soybeans. The 35
# predictors are mostly categorical and include information on the environmental 
# conditions (e.g., temperature, precipitation) and plant conditions (e.g., left
# spots, mold growth). The outcome labels consist of 19 distinct classes.
# 
# The data can be loaded via:
# 	> library(mlbench)
# 	> data(Soybean)
# 	> ## See ?Soybean for details
# 	
# (a) Investigate the frequency distributions for the categorical predictors. Are
# any of the distributions degenerate in the ways discussed earlier in this
# chapter?
		data("Soybean")
		head(Soybean)
		str(Soybean)
 	library(dplyr)
	library(ggplot2)
	summary(Soybean)
	#creating afunction to do an easiy plot for me
	dig_plot = function(df, var_name){
		str = paste(c("Distribution of", var_name), collapse = " ")
		df %>%
			rename_('Var' = var_name) %>%
			count(Var) %>%
			ggplot(aes (x = reorder(Var,n), y = n)) +
			geom_bar(stat = "identity", aes(fill = n)) + 
			coord_flip() +
			ggtitle(str)
	}
		
	char_vars = names(Soybean)[sapply(Soybean, class) %in% c('factor','character')]
	
	for (i in 1:NROW(char_vars)){
		print(Soybean %>% dig_plot(names(Soybean[,char_vars])[i]))
		cat ("Press [enter] to continue")
		line <- readline()	
	}

# (b) Roughly 18 % of the data are missing. Are there particular predictors that
# are more likely to be missing? Is the pattern of missing data related to
# the classes?
 				df = Soybean
 				df$missing = complete.cases(df[,-c(1,2)])
 			
# (c) Develop a strategy for handling missing data, either by eliminating
# predictors or imputation.
			will use the k-nn to imput the missing data, it would seem to me that similar plants under similar conditions
			would be similar

# 3.3. Chapter 5 introduces Quantitative Structure-Activity Relationship
# (QSAR) modeling where the characteristics of a chemical compound are used
# to predict other chemical properties. The caret package contains a QSAR
# data set from Mente and Lombardo (2005). Here, the ability of a chemical
# to permeate the blood-brain barrier was experimentally determined for 208
# compounds. 134 descriptors were measured for each compound.
# (a) Start R and use these commands to load the data:
# 	> library(caret)
# 	> data(BloodBrain)
# 	> # use ?BloodBrain to see more details
# 	
# The numeric outcome is contained in the vector logBBB while the predictors are in the data frame bbbDescr.
		library(caret)
		data(BloodBrain)
		df = BloodBrain
		str(df)
# (b) Do any of the individual predictors have degenerate distributions?
		I cant seeme to load the package 
# (c) Generally speaking, are there strong relationships between the predictor data? If so, how could correlations 
#in the predictor set be reduced?
# Does this have a dramatic effect on the number of predictors available for
# modelin
		Unknown, but we can easily reduce the number of variables in the pradictore by kicking out any variables that have a corr of greater than .75
		g