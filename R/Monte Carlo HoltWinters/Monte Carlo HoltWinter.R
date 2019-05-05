###############
#
#The Holtz Winter Jitter Bug
#
###############

'Okay, so I had a brain fart, and I want to test this out and see what becomes of it: So, HoltzWinter is a relativly
goot TS predictve model for the next x periods, but its not great. Its prone to out lieres and the most recent point 
will cause it to shift spectacularly. IE, it has very high variance, and relativly low bais. So how do we solve for the
high variance? Well, will jitter all the data points by the SD deviation leading up to that point for the past X data
points. For each itteration of the jitter, we will build a HW method off of it and forecast the results.
The final results will be the average of this result'

'The second step we could take would be to not only jitter the data, but ALSO RANDOMLY REMOVE POINTS, this, on first blush
should have the benefit of showing when there is an influencial data point. If we excldued points that arent influencial,
then the models should now vary, but if we exluded points that ARE Influencial, then the vairance of the model should increase,
maybe, I will need to think about it'


###
#
# loading library 
#
###

		library(forecast)

###
#
# Function to generate a random TS Series 
#
###
	sin_ts = function(n, freq, amp = 1, error_coef = amp/4 , error_Type = c('uniform','gaussian','none')){
				
					#building the correct error term function		
					if(error_Type == 'uniform'){ c.error = runif(n)
					} else if (error_Type == 'gaussian') {c.error = rnorm(n)
					} else {c.error = 1}
					#building the correct sin input function
					ts_value = seq(0,2*pi, length.out = freq)
					ts_value = c(rep(ts_value, n/freq), ts_value[1:(n %% freq)])
					
		
					y1 = amp*sin(ts_value) + c.error*error_coef							
					return(ts(y1, freq = freq))
					}

###
#
# The following function moves along the line and creates a SD for each point based on the past X points
#
###	
	SD_v = function(x, historical_length = 10, SD_adjustment = 1){
		#1D10T check
			if(historical_length > length(x)) {stop('the historical_length must be shorter than the actual length of the ts')}
		#for the first 10 points, it will base those outcomes on the first 10 points
		v_out = rep(sd(x[1:historical_length]),historical_length)
		running_sd = function(y){
			return(sd(x[(y- historical_length - 1 ):y]))
		}
		
		v_out = c(v_out,sapply((historical_length + 1):length(x), FUN = running_sd, USE.NAMES = F))*SD_adjustment
		return(v_out)
	}
	
###
#
# The following function will no jigger each point along a ts based on the SD Vector
#
###	
	jigger_line_all = function(x, sd_v, SD_adjustment = 1){
		#1D10T check
		if(!is.ts(x)) {stop('x must be a ts')}
		if(length(x)!= length(sd_v)) {stop('sd_v and x must be the same length')}

			p_jigger = function(value){
				return(rnorm(1, x[value], sd = sd_v[value] * SD_adjustment))
			}

			out = sapply(1:length(x), p_jigger, USE.NAMES = F)			
			return(ts(out, start = tsp(x)[1], frequency = tsp(x)[3]))
	}
	
	jigger_line = function(x, sd_v, SD_adjustment = 1, historical_length = 10, Random_point =T){
		#1D10T check
		if(!is.ts(x)) {stop('x must be a ts')}
		if(length(x)!= length(sd_v)) {stop('sd_v and x must be the same length')}
		
		len_x = length(x)
		non_consider = 1:(len_x - historical_length)
		consider = (len_x - historical_length + 1):len_x
		# 
		# p_jigger = function(value){
		# 	return(rnorm(1, x[value], sd = sd_v[value] * SD_adjustment * (historical_length - (len_x - value))/historical_length
		# 							 ))
		# }
		p_jigger = function(value){
			return(rnorm(1,  sd = sd_v[value] * SD_adjustment))
		}
		if(Random_point == F){	
					j = sapply(consider, p_jigger, USE.NAMES = F)			
					out = c(x[non_consider], x[consider] + cumsum(j))
		} else {
			#pt = sample(consider,1)
			pt = sample(consider,1)
			j = p_jigger(pt)
			j = x[pt:len_x] - j
			out = c(x[1:(pt-1)], j)
			
		}
					return(ts(out, start = tsp(x)[1], frequency = tsp(x)[3]))
	}
	
###
#
# The followgn fucntion takes a ts and fits a HW method to it and forecast it out  
#
###		
	fit_HW = function(x, h = 10){
		hw_out = HoltWinters(x)
		forecast_hw = forecast(hw_out, h = h)
		df_out = data.frame(mean = as.numeric(x), 
												lower_ci =c(rep(NA,tsp(x)[3]), as.numeric(hw_out$fitted[,1])),
												upper_ci = c(rep(NA,tsp(x)[3]), as.numeric(hw_out$fitted[,1])))
		df_out = rbind(df_out, data.frame(mean = forecast_hw$mean, lower_ci = forecast_hw$lower[,2], upper_ci = forecast_hw$upper[,2]))
		return(df_out)
		
	}
	
	###
	#
	# putting it all together for one ts seriesso we just have one function that does it all
	#
	###
		random_hw = function(x, historical_length = 10, SD_adjustment = 1, h = 10, return_value = 'mean'){
			sd_v = SD_v(x, historical_length = historical_length, SD_adjustment = SD_adjustment)
			lined_has_been_jiggered_buddy = jigger_line(x, sd_v = sd_v, historical_length = historical_length)
			line_has_been_fit_HW_foo = fit_HW(lined_has_been_jiggered_buddy, h = h)
			
			
			return(line_has_been_fit_HW_foo[,return_value])
		}
		
###
#
# putting it all together so we just have one function that does it all
#
###

	hw_random_forecast = function(x, n, historical_length = 10, SD_adjustment = 1, h = 10, return_value = 'mean'){
		
			jigger_matrix = as.matrix(replicate(n, random_hw(x, 
														historical_length = historical_length,
														SD_adjustment = SD_adjustment,
														h = h,
														return_value = return_value)))
			matrix_names =  colnames(jigger_matrix, do.NULL = F, prefix = "Run")
			
			jigger_matrix = cbind( mean = rowSums(jigger_matrix)/n, jigger_matrix)
			jigger_matrix = data.frame(cbind(x = c(as.numeric(x), jigger_matrix[(length(x)+1):NROW(jigger_matrix), 'mean']), jigger_matrix ))
			names(jigger_matrix) = c("x","mean", matrix_names)
			return(jigger_matrix)}
	
###
#
# creating a plotting function so we can see what it looks like
#
###
	plot_das_jigger = function(x, ranSample = FALSE) {
		plot(x[,1], ylim = c(min(x),max(x)))
		
		if(ranSample == T)
		{ranSample = sample(3:ncol(x), 10)}
		else {ranSample = 3:ncol(x)}

		for(i in ranSample){
			lines(x[,i], col = 'light grey', lwd = 0.2)
		}
		
		lines(x[,1], col = 'blue', lwd = 2)
	}
	
	
	
	
	

	t = 	sapply(rev(0:14), function(i) hw_random_forecast(ts(eee_ts[1:(length(eee_ts) - i)], frequency =26), 
																												30, 
																												historical_length = 26, 
																												SD_adjustment = .5,
																												h = (104 - (14-i)))[,1])
	
	t= sapply(rev(0:14), function(i) as.numeric(forecast(HoltWinters(ts(eee_ts[1:(length(eee_ts) - i)], frequency =26)),
																									 h = 104 )$mean))
	plot(t[15:nrow(t),1])
	for( i in rev(2:ncol(t))){	lines(t[(15-i):nrow(t),i])}
	t = cb
	t = cbind(rowSums(t)/15, rowSums(t)/15, t )
	plot_das_jigger(t)