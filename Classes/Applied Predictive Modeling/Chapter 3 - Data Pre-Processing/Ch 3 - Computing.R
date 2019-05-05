###############
#
# Chapter 3: DAta pre-processing
#					Computing
#
###############


####
# loading required packages
####
	library(AppliedPredictiveModeling)
	library(corrplot)
	library(e1071)
	library(lattice)



		#aoriois can be used to find any function in a package currently loaded
			apropos("confusion")
			
		#Rsite search cna look across all packages online; it will open a webpage of its results
			RSiteSearch("confusion",restrict = "functions")
		
	####################################################
	#
	# Segmenting the data
	#
	####################################################
			
		#loading Dat
			df = segmentationOriginal
		
		#subsetting the data
			segData = subset(df, Case == "Train")
			
		#the Class, Cell, and case will be saved into seperate variables, then removed from the set
			cellID = segData$Cell
			class = segData$Class
			case = segData$Case
			
			segData = segData[,-(1:3)]
		
		#we will also remove any column with the name 'status' in it
			statusColNum = grep("Status",names(segData))
			statusColNum
			
			segData = segData[,-statusColNum]
		
####################################################
#
# Transformations
#
####################################################
			
		#we can use the skewness function in e1071 to calculate skewness
			library(e1071)
			
			#applying skewness to one variable
				skewness(segData$AngleCh1)
			#applying it to all variables
				skewValues = apply(segData, 2, skewness)
				head(skewValues)
			#we can then look at the historgam of these skews to get a feeling for which ones to focust on
				
		#the caret packages can find the appropate llamda and apply the transforation
				library(caret)
				
				Ch1AreaTrans = BoxCoxTrans(segData$AreaCh1)
				Ch1AreaTrans
				
			#lets see how the original comapres to the transofmred value
				#original
					head(segData$AreaCh1)
				#now the tranfromed
					predict(Ch1AreaTrans, head(segData$AreaCh1))
				# testing to see if it's worked
					(819^(-0.9) - 1)/(-0.9) == predict(Ch1AreaTrans, head(segData$AreaCh1))[1]
					
			
		#We can also use the preProcess from the Caret packages to perfrom transformation
		#for a set of variables, the base transformation is PCA, note we will cause the 
	  #it can also scale and center the variables for us
			
			pcaObject = prcomp(segData,
												 center = T,
												 scale. = T)
			#caluclating the culumative variavence of each componet
				pcaObject$sd^2/sum(pcaObject$sd^2) * 100
			
			#the transformed values are stored in 'x'
				head(pcaObject$x[,1:5])
				
			# we can also use 'rotate' to see the loadings on each PC
				head(pcaObject$rotation[,1:5])
			
		#the CARET Packages also has prePRocess, which can transform, center, scale or impute,
		#as well as  apply spatial sign transofmraiotn and feature extraction
			trans = preProcess(segData, method = c("BoxCox","center","scale","pca"))	
			trans
			
			#we will need to use the predict function to actally apply the tranformation
			transformed = predict(trans, segData)
			head(transformed)
		#Caret also have spatialSign to calculate the spatial sign
		#the impute package and perform K-NN for missing data (impute.knn)

			
####################################################
#
# Filtering      
#
####################################################

		#the Caret package has nearZeroVar which will return column number of near
		#nearZEroVariance
			nearZeroVar(segData)
			
		#we can similarly use the cor function  to find correlation
			correlation = cor(segData)
			
			dim(correlation)
			correlation[1:4,1:4]
			
		#to visually examine the correlation function, we can use corrplot
		#from the corrplot packages
			library(corrplot)
			corrplot(correlation, order = "hclust")  #DUDE thats fucking awesom
			
		#to filter based on correlation, we can use the find Correlation function
			highCorr =findCorrelation(correlation, cutoff = 0.75)
			length(highCorr)
			head(highCorr)
			
			filteredSegData = segDat[,-highCorr]
			
####################################################
#
# Creating Dummy Variables
#
####################################################
			
	#we will be working with the cars data set from CARET
		data(cars)
		df = cars
	#subsetting it to only be sedsns
		carSubset = subset(df[,c(1:2,17)], sedan == 1)
		carSubset$Type = "sedan"
		carSubset = carSubset[,c(1,2,4)]
		head(df)
		#EEEEEH can't get it to work, so here is the code anyways
		
	#creating a dummyvars function to create dummy variables
		simpleMod = dummyVars(~Milage + Type,
													data = carSubset,
													##remove var name from
													##the column name
													levelsOnly = T)
		simpleMod
		
		#don't forget, we gotta use pred to make it work
			predict(simpleMod, head(carSubset))
			
	#what if we wanted to assume there was a joint effect between varaibls
		withInteractions = dummyVars(~Milage + Type +Milage:Type,
																 data = carSubset,
																 levelsOnly = T)
		withInteractions
		predict(withInteractions, head(carSubset))
		
			