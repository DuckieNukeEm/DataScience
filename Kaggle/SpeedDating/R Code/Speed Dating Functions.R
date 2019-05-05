#functions for KAggle Compititions

g_hist = function(data, var){
  #a quick function to a plot a hist using ggplot
  (ggplot(data = data, aes(data[,var])) + 
     geom_histogram(breaks = seq(0,100, b = 5), col = "black", aes(fill=..count..)) + 
     scale_fill_gradient("Count", low = "green", high = "red") +
     labs(title = paste(c("Histogram for ", var), collapse = "")) +
     labs(x = var, y = "Count") )
}

q_hist = function(data, var, breaks = seq(0,100, b = 5), adds = FALSE, col = "blue", freq = NULL, mainmore = NULL ){
  #a fucntion to plot a hist using plot
  hist(data[,var],
       breaks = breaks,
       col = col,
       freq = freq,
       border = "black",
       main = paste(c("Histogram for", var, "of", mainmore), collapse = " "),
       xlab = var,
       ylab = "Counts",
       add = adds
  )
  
}

q_scatter = function(data, x_var, y_var, col = "blue"){
  #quick function to plot a scatter plot
  plot(data[,y_var]~data[,x_var],  
       col = col,
       main = paste(c("Scatter Plot for", y_var, "and", x_var), collapse = " "),
       xlab = x_var,
       ylab = y_var
  )
}


recall = function(true, pred){
  t = table(true, pred)
  t[4]/(t[4] + t[2])
  
}


precision = function(true, pred){
  t = table(true, pred)
  t[4]/(t[4] + t[3])
}

remove_na = function(x) {
	#removes NA's
	if(class(x) %in% c("double","integer","complex","numeric") & class(x)!="factor")
	{
		x[is.na(x)|is.nan(x)|is.infinite(x)] = mean(x, na.rm = T)
		return(x) 
	}
	return(x)
}

standardize = function(x){
  #this function standardizes the data to more user friendly. 
  if(class(x) %in% c("double","integer","complex","numeric") & class(x)!="factor"){

  	return( (x - mean(x))/sd(x))
  	
  }	else if(class(x) %in% c("charcter","factor")){
  	
  		if(is.facet(x)){
  				x = as.character(x)
				}
  		df = data.frame(table(x)/NROW(x))
  		for( t in unique(x))
  		{
  			x[x==t] = as.numeric(df[df$x==t,"Freq"])
  		}
  		return( (x - mean(x))/sd(x))
  		
  } else {
  	
  	return(x)
  	} 	
  	
}



wssplot <- function(data, nc=15, seed=1234){
	wss <- (nrow(data)-1)*sum(apply(data,2,var))
	for (i in 2:nc){
		set.seed(seed)
		wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	plot(1:nc, wss, type="b", xlab="Number of Clusters",
			 ylab="Within groups sum of squares")}

VarType = function(df){
	#this function reads in a data frame, then returns if each var is a Char or a Numeric
	out_df = data.frame( VarName = names(df))
	out_df$VarType = as.vector(sapply(df, class))
	out_df$VarType = ifelse(out_df$VarType %in% c('factor','character'), 'Char', ifelse(out_df$VarType %in% c('numeric','integer'), 'Int', 'Other'))
	return(out_df)
	
}