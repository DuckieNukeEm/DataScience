####
#
# We will be using Boston Housing Data, we will use it to predict the housing prices in Boston
#
####

	#loading package that contains the data set
		library(MASS)
		bh = Boston
	
	#renameing Var for ease
		names(bh) = c("crime_pc", "zoned_land_25kft","industry_prt","by_river","NOX_PP10M","AvgRoom","port_built_1940",
									"dist_to_work","dist_to_highway","tax_per_10k","Teach-pupil","black","perc_poor","Median_value_1k")
		bh$by_river = factor(bh$by_river)
####
#
# Now, let's look at the dat
#
####
	#Dim of the data frame
		str(bh)
	#looking at hte first 10 records
		head(bh)
		
	#looking at a summary of the data
		summary(bh)
		
#####
#
# Plotting the Data
#
####
		hist(bh$crime_pc)
		hist(log(bh$crime_pc))
			# bh$crime_pc = log(bh$crime_pc)
		
		hist(bh$zoned_land_25kft)
		hist(log(bh$zoned_land_25kft))
			# bh$zoned_land_25kft = log(bh$zoned_land_25kft)
		
		hist(bh$industry_prt)
		hist(log(bh$industry_prt))
		
		hist(bh$NOX_PP10M)
		hist(log(bh$NOX_PP10M))
			# bh$NOX_PP10M = log(bh$NOX_PP10M)
		hist(bh$AvgRoom)
		
		hist(bh$port_built_1940)
		
		
		hist(bh$dist_to_work)
		hist(log(bh$dist_to_work))
			# bh$dist_to_work = log(bh$dist_to_work)
		
		hist(bh$dist_to_highway)
		
		hist(bh$tax_per_10k)
		
		hist(bh$`Teach-pupil`)
		
		hist(bh$black)
		
		hist(bh$perc_poor)
		
		hist(bh$Median_value_1k)

#####
#
# Looking at correlations
#
####
		
	#Loading correlation package
		library(corrplot)
		
	#plotting correlations
		corrplot.mixed(corr = cor(bh[,-4]), upper = "ellipse", order = "AOE")
		
####
#
# Now, let's look at the dat
#
####

	#running a base model
		base_model = lm(Median_value_1k ~., data = bh)
		summary(base_model)
	#running a stepwise regression
		step_model = step(base_model,direction = "both")
		summary(step_model)
		
		
		
	#running Lasso
		library(glmnet)
		
	#converting into matrix
		
		bh_m = scale(model.matrix( ~., data = bh))
	#removing intercept
		bh_m = bh_m[,-1]
	#running crossvalidation with lasso
		lasso_model = cv.glmnet(x = bh_m[,1:13], y = bh_m[,14], alpha = 1)
	#plotting view
		plot(lasso_model$glmnet.fit, "lambda", label = T)
		plot(lasso_model)
		
	#lets look at the optimal MSE
		coef(lasso_model, s = lasso_model$lambda.1se)
		
	#taking a look the lm of the model
		lasso_lm_model = lm(Median_value_1k ~ 
							 								crime_pc + 
							 								by_river + 
							 								NOX_PP10M +
							 								AvgRoom + 
							 								dist_to_work + 
							 								`Teach-pupil` + 
							 								black + 
							 								perc_poor, data = bh)
		summary(lasso_lm_model)