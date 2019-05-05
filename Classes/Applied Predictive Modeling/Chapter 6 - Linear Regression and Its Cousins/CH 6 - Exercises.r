################
################
##
## Chapter 6: Linear Regression and it's Cousins                   
##                      		Excercises
##
###############
###############

####
#
# 6.1
#
####
	# Infrared (IR) spectroscopy technology is used to determine the chemical makeup of a substance. The theory of IR spectroscopy holds that unique
	# molecular structures absorb IR frequencies differently. In practice a spectrometer fires a series of IR frequencies into a sample material, and the device
	# measures the absorbance of the sample at each individual frequency. This
	# series of measurements creates a spectrum profile which can then be used to
	# determine the chemical makeup of the sample material.
	# A Tecator Infratec Food and Feed Analyzer instrument was used to analyze
	# 215 samples of meat across 100 frequencies. A sample of these frequency profiles is displayed in Fig. 6.20. In addition to an IR profile, analytical chemistry
	# determined the percent content of water, fat, and protein for each sample.
	# If we can establish a predictive relationship between IR spectrum and fat
	# content, then food scientists could predict a sample's fat content with IR
	# instead of using analytical chemistry. This would provide costs savings, since
	# analytical chemistry is a more expensive, time-consuming process:
		
		# (a) Start R and use these commands to load the data:
		# > library(caret)
		# > data(tecator)
		# > # use ?tecator to see more details
		# The matrix absorp contains the 100 absorbance values for the 215 samples,
		# while matrix endpoints contains the percent of moisture, fat, and protein
		# in columns 1-3, respectively.
				library(caret)
				data(tecator)
		
		# (b) In this example the predictors are the measurements at the individual frequencies. 
		# Because the frequencies lie in a systematic order (850 ~ 1,050nm),
		# the predictors have a high degree of correlation. Hence, the data lie in a
		# smaller dimension than the total number of predictors (215). Use PCA
		# to determine the effective dimension of these data. What is the effective
		# dimension?
				transform = preProcess(data.frame(absorp),
													method = c('center','scale','pca'))
				transform
				'we just need 2 variables nwo to capture 95% of all variance'
				absorp_pca = predict(transform, data.frame(absorp))
				
				#non CARET way
				transform = prcomp(data.frame(absorp),
							 						center = TRUE,
							 						scale. = TRUE) 
				summary(transform)
				plot(transform, type = '1')
		# (c) Split the data into a training and a test set, pre-process the data, and
		# build each variety of models described in this chapter. For those models with tuning parameters, 
		#	what are the optimal values of the tuning
		# parameter(s)?
				data_compare = data.frame(method = character(0), RMSE = numeric(0), Rsquared = numeric(0), stringsAsFactors = F)
				train_s = sample(1:nrow(absorp), nrow(absorp)*.8)
				resultz = function(name, data_df, FUN,...){
										t = c( name, 
													 defaultSummary(
																					data.frame(obs = data_df[,"fat"],
																 										pred = predict(FUN,data_df)
																										),
																					...
													 								)
													)
										return(t)
				}
				
				
				df = data.frame(endpoints[,2], absorp)
				names(df) = c("fat","X1", names(df)[3:101])
				
				df_pca = data.frame(endpoints[,2], predict(transform, data.frame(absorp)))
				names(df_pca) = c("fat","PCA_1","PCA_2")
				#OLS
					ols_n = lm(fat ~ ., data = df[train_s,])
						data_compare[1,] = resultz("OLS_N", df[-train_s,], ols_n)
					
					ols_pca = lm(fat ~., data = df_pca[train_s,])
						data_compare[2,] = resultz("OLS_PCA", df_pca[-train_s,], ols_pca)
					
					caret_ols = train(fat ~.,
														data = df[train_s,],
														method = 'lm',
														trControl = trainControl(method = 'cv',
																				 number = 5))
						data_compare[3,] = resultz("OLS_CARET", df[-train_s,], caret_ols)
					
					#rls
					library(MASS)
						rlm_pca = rlm(fat~., data = df_pca[train_s,])
							data_compare[4,] = resultz("rlm_pca", df_pca[-train_s,], rlm_pca)
						caret_rlm = train(fat ~.,
															data = df_pca[train_s,],
															method = 'rlm',
															trControl = trainControl(method = 'cv',
																											 number = 5))
							data_compare[5,] = resultz("RLM_CARET", df_pca[-train_s,], caret_rlm)
				#PLS
						library(pls)
						pls_n = plsr(fat~., data=df_pca[train_s,])
							data_compare[6,] = resultz("PLS", df_pca[-train_s,], pls_n, ncomp = 2)
						caret_pls = train(fat ~.,
															data = df[train_s,],
															method = "pls",
															tuneLength = 20,
															trControl = trainControl(method = 'cv',
																											 number = 5),
															preProc = c("center","scale"))
							data_compare[7,] = resultz("PLS_CARET", df[-train_s,], caret_pls)
				#honestly, I'm not a big fan of the packages this book uses for ridge regression
				#I'de rather use glmnet for ridge regression......fucker
					library(glmnet)
						#lasso
							lasso_n = cv.glmnet(x = as.matrix(df[train_s,2:ncol(df)]),
														 y = as.matrix(df[train_s,1]),
														 alpha = 1)
							data_compare[8,] = resultz("Lasso", df[-train_s,], lasso_n)
							
							lasso_caret = train(fat~.,
																	data = df[train_s,],
																	method = "lasso",
																	trControl = trainControl(method = 'cv',
																													 number = 5),
																	preProc = c("center","scale"))
							data_compare[8,] = resultz("lasso", df[-train_s,], lasso_caret)
							# (d) Whic
							
							
							
							ridge_caret = train(fat~.,
																	data = df[train_s,],
																	method = "ridge",
																	trControl = trainControl(method = 'cv',
																													 number = 5),
																	preProc = c("center","scale"))
							data_compare[9,] = resultz("ridge", df[-train_s,], ridge_Caret)
		# (d) Which model has the best predictive ability? Is any model significantly
		# better or worse than the others?
			data_compare
		# (e) Explain which model you would use for predicting the fat content of a
		# sample.
				"PLS_CARET got some prettty damn good results~"

####
#
# 6.2
#
####
	# 
	# Developing a model to predict permeability (see Sect. 1.4) could save significant resources for a pharmaceutical company, while at the same time more
	# rapidly identifying molecules that have a sufficient permeability to become a
	# drug:
	
		#(a) Start R and use these commands to load the data:
		 	library(AppliedPredictiveModeling)
			data(permeability)
		# The matrix fingerprints contains the 1,107 binary molecular predictors for the 165 compounds, while permeability contains permeability
		# response.
		 
		# (b) The fingerprint predictors indicate the presence or absence of substructures of a molecule and are often sparse meaning that relatively few of the
		# molecules contain each substructure. Filter out the predictors that have
		# low frequencies using the nearZeroVar function from the caret package.
		# How many predictors are left for modeling?
		 	fp_2 = fingerprints[,-nearZeroVar(fingerprints)]
		 	ncol(fp_2)
		# (c) Split the data into a training and a test set, pre-process the data, and
		# tune a PLS model. How many latent variables are optimal and what is
		# the corresponding resampled estimate of R2?
		 	fp_p = data.frame(permeability, scale(fp_2))
		 	train_s = sample(1:nrow(fp_p), nrow(fp_p)*0.7)
		 	pls_n = plsr(permeability~., data=fp_p[train_s,])
		 	plot(pls_n)
		 	caret_pls = train(permeability ~.,
		 										data = fp_p[train_s,],
		 										method = "pls",
		 										tuneLength = 20,
		 										trControl = trainControl(method = 'cv',
		 																						 number = 5))
		 	plot(caret_pls)
		 	caret_pls
		 	'looks like we want 8 PLS components, with an rsquared of .457'
		 	
		# (d) Predict the response for the test set. What is the test set estimate of R2?
		 	defaultSummary(
		 		data.frame(obs = fp_p[-train_s,1],
		 							 pred = predict(caret_pls,fp_p[-train_s,], 	ncomp = 8)
		 		)
		
		 	)
		 	
		 	
		 	fp_p[-train_s,], caret_pls)
		# (e) Try building other models discussed in this chapter. Do any have better
		# predictive performance?
				resultz = function(name, data_df, FUN,...){
					t = c( name, defaultSummary(
								 											data.frame(obs = data_df[,"permeability"], pred = predict(FUN,data_df))
								 
								 )
					)
					return(t)
				}
				
			#OLS
				ols_n = lm( permeability ~ ., data = fp_p[train_s,])
				data_compare[1,] = resultz("OLS_N", fp_p[-train_s,], ols_n)
			#rls
				caret_rlm = train(permeability ~.,
													data = fp_p[train_s,],
													method = 'rlm',
													trControl = trainControl(method = 'cv',
																								 number = 5),
													preProc = c("center","scale","pca"))
				data_compare[2,] = resultz("RLM_CARET", fp_p[-train_s,], caret_rlm)
			#PLS
				caret_pls = train(permeability ~.,
													data = fp_p[train_s,],
													method = "pls",
													tuneLength = 20,
													trControl = trainControl(method = 'cv',
																									 number = 5),
													preProc = c("center","scale"))
				data_compare[3,] = resultz("PLS_CARET", fp_p[-train_s,], caret_pls)
			#lasso
				lasso_caret = train(permeability~.,
														data = fp_p[train_s,],
														method = "lasso",
														trControl = trainControl(method = 'cv',
																										 number = 5),
														preProc = c("center","scale"))
				data_compare[4,] = resultz("lasso", fp_p[-train_s,], lasso_caret)
			#ridge
				ridge_caret = train(permeability~.,
														data = fp_p[train_s,],
														method = "ridge",
														trControl = trainControl(method = 'cv',
																										 number = 5),
														preProc = c("center","scale"))
				data_compare[5,] = resultz("ridge", fp_p[-train_s,], ridge_caret)
		
		data_compare[1:5,]				
		# (f) Would you recommend any of your models to replace the permeability
		# laboratory experiment?
					'Hell no, we ar egetting some pretty bad results"'
####
#
# 6.3
#
####

	# A chemical manufacturing process for a pharmaceutical product was
	# discussed in Sect. 1.4. In this problem, the objective is to understand the relationship between biological measurements of the raw materials (predictors),
	# measurements of the manufacturing process (predictors), and the response of
	# product yield. Biological predictors cannot be changed but can be used to
	# assess the quality of the raw material before processing. On the other hand,
	# manufacturing process predictors can be changed in the manufacturing process. Improving product yield by 1 % will boost revenue by approximately
	# one hundred thousand dollars per batch:
	 
		# (a) Start R and use these commands to load the data:
				library(AppliedPredictiveModeling)
				data(chemicalManufacturing)
			"THe hell....this data set doesn't excited anymore. The hell is with that?!?!?"
			"dear god, they changed the name from chemicalManufacturing to chemicalManufacturingProcess"
			
				data(ChemicalManufacturingProcess)
		# The matrix processPredictors contains the 57 predictors (12 describing
		# the input biological material and 45 describing the process predictors)
		# for the 176 manufacturing runs. yield contains the percent yield for each
		# run.
	
		# (b) A small percentage of cells in the predictor set contain missing values. Use
		# an imputation function to fill in these missing values (e.g., see Sect. 3.8).
				df = ChemicalManufacturingProcess
				for(i in 1:ncol(ChemicalManufacturingProcess)){
					print(c(i,mean(df[,i], na.rm = T)	))
					df[is.na(df[,i]),i] = mean(df[,i], na.rm = T)	
					
				}
		# (c) Split the data into a training and a test set, pre-process the data, and
		# tune a model of your choice from this chapter. What is the optimal value
		# of the performance metric?
		 	train_s = sample(1:nrow(df), 0.7 * nrow(df))
				
		# (d) Predict the response for the test set. What is the value of the performance
		# metric and how does this compare with the resampled performance metric
		# on the training set?
		 			pls = train(Yield~.,
		 											data = df[train_s,],
		 											method = "pls",
		 											trControl = trainControl(method = 'cv',
		 																							 number = 5),
		 											preProc = c("center","scale"))
		 			
		 			defaultSummary(
		 				data.frame(obs = df[-train_s,1], pred = predict(pls,df[-train_s,]))
		 				
		 			)
		# (e) Which predictors are most important in the model you have trained? Do
		# either the biological or process predictors dominate the list?
		 			plot(varImp(pls))
		 			'pretty much manufacturing proccess domunate the list'
		# (f) Explore the relationships between each of the top predictors and the response. How could this information 
		# 			be helpful in improving yield in future
		# runs of the manufacturing process?
		 			for(i in row.names(varImp(pls)$importance)){
		 				
		 				plot(formula(paste(c("Yield",i), collapse = "~")),df)
		 				cat ("Press [enter] to continue")
		 				line <- readline()
		 			}