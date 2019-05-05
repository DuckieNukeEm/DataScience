################
################
##
## Chapter 4: Over-Fitting and Model Tuning
##					Excercises
##
###############
###############

 
# 4.1. Consider the music genre data set described in Sect. 1.4. The objective
# for these data is to use the predictors to classify music samples into the
# appropriate music genre.
# (a) What data splitting method(s) would you use for these data? Explain.
		I would use a stratified data splitting method to keep the balance of the dependent
		variables the same between the training sets and the test sets 
		
		Now, if I really had my way, I would us cv methods to make sure it was nice and tight
		
# (b) Using tools described in this chapter, provide code for implementing your
# approach(es).
		trainingRows = createDataPartition(music$outcome, 
																			 p=0.8,
																			 list = FALSE)
		
# 4.2. Consider the permeability data set described in Sect. 1.4. The objective
# for these data is to use the predictors to model compounds' permeability.
# (a) What data splitting method(s) would you use for these data? Explain.
		I woud just use a CV distribution, while the outcome is highly skewed, we can still
		us a cv method that should, given many itturations always hold true to the final output
# (b) Using tools described in this chapter, provide code for implementing your
# approach(es).
		createFolds(trainingClass, 
								k = 10,
								returnTrain = T)
 
# 4.3. Partial least squares (Sect. 6.3) was used to model the yield of a chemical
# manufacturing process (Sect. 1.4). The data can be found in the AppliedPredictiveModeling 
# package and can be loaded using
# 
# 	> library(AppliedPredictiveModeling)
# 	> data(ChemicalManufacturingProcess)
# 
# The objective of this analysis is to find the number of PLS components
# that yields the optimal R2 value (Sect. 5.1). PLS models with 1 through 10
# components were each evaluated using five repeats of 10-fold cross-validation
# and the results are presented in the following table:
#(a) Using the "one-standard error"method, what number of PLS components
# provides the most parsimonious model?
		3 compoments, the best model we have uses 5 components for a mean of 0.545 and st_error of 0.038, so taking the diffrence
		0.545 - 0.038 = 0.507, which contains the 3 compoents

# (b) Compute the tolerance values for this example. If a 10 % loss in R2 is
# acceptable, then what is the optimal number of PLS components?
		x/0.545 - 1 = .1
		x/0.545 = .9
		x = 0.545*.9 = .4905
		=> 2 components 
# (c) Several other models (discussed in Part II) with varying degrees of complexity were trained and tuned and the 
# results are presented in Fig. 4.13.
# If the goal is to select the model that optimizes R2, then which model(s)
# would you choose, and why?
		SVM, becuase R^2 is at the max, duh!
# (d) Prediction time, as well as model complexity (Sect. 4.8) are other factors
# to consider when selecting the optimal model(s). Given each model's prediction time, model complexity, and R2 estimates, which model(s) would
# you choose, and why?
	I would choose the boosted linear regression, while its not a fully optimized R^2, it is the second optimized model, but also
		has a relativly short run time, which is very very noice!
			
# 4.4. Brodnjak-Vonina et al. (2005) develop a methodology for food laboratories to determine the type of oil from a sample. In their procedure, they used
# a gas chromatograph (an instrument that separate chemicals in a sample) to
# measure seven different fatty acids in an oil. These measurements would then
# be used to predict the type of oil in a food samples. To create their model,
# they used 96 samples2 of seven types of oils.
# These data can be found in the caret package using data(oil). The oil
# types are contained in a factor variable called oilType.The types are pumpkin
# (coded as A), sunflower (B), peanut (C), olive (D), soybean (E), rapeseed (F)
# and corn (G). In R,
# 	> data(oil)
# 	> str(oilType)
# 	> table(oilType)
# 
# (a) Use the sample function in base R to create a completely random sample
# of 60 oils. How closely do the frequencies of the random sample match
# the original samples? Repeat this procedure several times of understand
# the variation in the sampling process.
		c(round(table(oilType)/NROW(oilType),2), round = 0)
		
		cvSplits = createFolds(oilType,
													 k = 10,
													 returnTrain = T)
		
		oil_df = data.frame(round(table(oilType)/NROW(oilType),2), round = 0)
		
		for (i in 1:10){
			d = data.frame(round(
												table(oilType[cvSplits[[i]]])/NROW(cvSplits[[i]]),2), round = i)
		#	names(d) = names(oil_df)
		oil_df = cbind(oil_df, d[,2])
			
		}
# (b) Use the caret package function createDataPartition to create a stratified
# random sample. How does this compare to the completely random samples?
		repeatedSplits = createDataPartition(oilType, 
																				 p = 0.80,
																				 times = 3)
		for (i in 1:3){
			d = data.frame(round(
				table(oilType[repeatedSplits[[i]]])/NROW(repeatedSplits[[i]]),2), round = i)
			#	names(d) = names(oil_df)
			oil_df = cbind(oil_df, d[,2])
			
		}
		it held the ratios constant
# (c) With such a small samples size, what are the options for determining
# performance of the model? Should a test set be used?
			nah, cv that shit
# (d) One method for understanding the uncertainty of a test set is to use a
# confidence interval. To obtain a confidence interval for the overall accuracy, 
# the based R function binom.test can be used. It requires the user
# to input the number of samples and the number correctly classified to
# calculate the interval. For example, suppose a test set sample of 20 oil
# samples was set aside and 76 were used for model training. For this test
# set size and a model that is about 80% accurate (16 out of 20 correct),
# the confidence interval would be computed using
# 	> binom.test(16, 20)
# In this case, the width of the 95% confidence interval is 37.9%. Try
# different samples sizes and accuracy rates to understand the trade-off
# between the uncertainty in the results, the model performance, and the
# test set size.




