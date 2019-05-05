#entropy is used to find the entropy on each individual units of a vector
entropy <- function(x) {
	return((x/sum(x))*log(x/sum(x),2)*-1)
	}

#twin entropy is used to calculate the the entropy for each variable between two vectors
#target is the target variable in question, it will default to the first column
#source is the variable you want to measure the entropy ON
#x is the data set
twinentropy <- function(x, Target = names(x)[1], Source){	
	t <- table(x[,c(Target, Source)])
	t <- apply(t,2,entropy)
	t[which(is.na(t))]<-0
	return(t)
}

#TotalEntropy is used to calculate the total entropy between two vectors.

totalentropy <- function(x, Target = names(x)[1], Source){	
	t <- twinentropy(x, Target, Source)
	sumt <- apply(table(x[,c(Target, Source)]),2,sum)
	sumt <- sumt/(sum(sumt))
	sum(apply(t,2,sum) * sumt)
}

#ventropy is used to find the entropy of the entiretire vector

ventropy <- function(x) { 
	return(-1*sum(eentropy(x)))
	}
