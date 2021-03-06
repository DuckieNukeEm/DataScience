################
################
##
## Chapter 5: Measuring Performance in Regression Models
##					Computing
##
###############
###############


####################################################
#
# Performance evaluations
#
####################################################

	#lets create two vectors, those we have obseved, and those that we have 'predicited
			observed <- c(0.22, 0.83, -0.12, 0.89, -0.23, -1.30, -0.15, -1.4,
										0.62, 0.99, -0.18, 0.32, 0.34, -0.30, 0.04, -0.87,
										0.55, -1.30, -1.15, 0.20)
			
			predicted <- c(0.24, 0.78, -0.66, 0.53, 0.70, -0.75, -0.41, -0.43,
										 0.49, 0.79, -1.19, 0.06, 0.75, -0.07, 0.43, -0.42,
										-0.25, -0.64, -1.26, -0.07)
			residualValues <- observed - predicted
			
			summary(residualValues)
			
	#its always a good idea to plot htese values
			axisRange = extendrange(c(observed,predicted))
			
			plot(observed, predicted,
					 ylim = axisRange,
					 xlim = axisRange)
			abline(0,1,col = 'darkgrey', lty = 2)
			
			plot(predicted, residualValues, ylab = 'residuals')
			abline(h = 0, col = 'darkgrey', lty = 2)
			
	#we can use the R2 & RMSE  packg in caret to get some stats
			R2(predicted, observed)
			RMSE(predicted, observed)
			
	#as previous mentioned, there are various ways to calc R2
		# the base packages can calcualte the Spearman's rank corelation
		#cor
			cor(predicted, observed)
		#rank correlation
			cor(predicted, observed, method = 'spearman')
		