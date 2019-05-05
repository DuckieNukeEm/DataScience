##################################
#
#           Ch.2 Exercises 
#
#################################
library(survival)

"2.1. Using the survexp.us data described in Example 2.2, 
plot the hazard functions for men and women in 1940 and 2000. Comment on the change in mortality
rates in children."

plot(survexp.us[,"female","1940"])
lines(survexp.us[,"male","1940"])
lines(survexp.us[,"male","2000"], col = 'red')
lines(survexp.us[,"female","2000"], col = 'green')

"2.2. Find the mean age of death separately for men and women for 1940 and 2000."


mean_age = function(time, hazard){
	tm.diff = diff(time)
	survMale <- exp(-cumsum(hazard*tm.diff)*365.24)
	return(sum(survMale*tm.diff))
	
}

tm <- c(0, # birth
				1/365, # first day of life
				7/365, # seventh day of life
				28/365, # fourth week of life
				1:110)


mean_age(tm, survexp.us[,"female","1940"])
mean_age(tm, survexp.us[,"male","1940"])
mean_age(tm, survexp.us[,"female","2000"])
mean_age(tm, survexp.us[,"male","2000"])

"2.3. The data set survexp.usr in the survival package is a four dimensional array
of hazards in format similar to the survexp.us data set, with race (black or white)
in the added dimension. Plot the hazard functions for black males and white males
for 1940 and 2000."

plot(survexp.usr[,"male","black","1940"])
lines(survexp.usr[,"male","white","1940"], col = "light blue")
lines(survexp.usr[,"male","black","2000"], col = "light green")
lines(survexp.usr[,"male","white","2000"], col = "light gray")



"2.4. Consider the survival data in Exercise 1.1. Assuming that these observations
are from an exponential distribution, find llamda and an estimate of var./ O ."

Person     = c('P1','P2','P3','P4','P5')
PersonYears = c(  5,   5,   4,   3,   1)
Event =       c(  0,   0,   1,   1,   1)

cancer_5 =data.frame(Person, PersonYears, Event)

log_like_exp = function(Num_of_deaths, Time_of_patiens){
						llamda = Num_of_deaths/Time_of_patiens
						var = llamda/Time_of_patiens
						return(c(llamda, var))
}

log_like_exp(sum(cancer_5[,3]), sum(cancer_5[,2]))

"2.5. Consider a survival distribution with constant hazard llamda = 0.07 from t = 0
until t = 5 and then hazard llamda = 0.14 for t > 5. (This is known as a piecewise
constant hazard.) Plot this hazard function and the corresponding survival function
for 0 < t < 10. What is the median survival time?"

t = c(0.07, 0.07, 0.07, 0.07, 0.07, 0.14, 0.14, 0.14, 0.14 )
plot(t)

(exp(-0.07*1) - exp(-0.07*5))/0.07 + (exp(-0.14*6) - exp(-0.14*9))/0.14


"2.6. Another parametric survival distribution is the log-normal distribution. Use the
density and cumulative distribution R functions dlnorm and plnorm to compute
and plot the lognormal hazard functions with the parameter meanlog taking the
values 0, 1, and 2, and with sdlog fixed at 0.25. Describe the risk profile a disease
would have if it followed one of these hazard functions."

log_norm_haz = function(x, meanlog, sdlog) plnorm(x, meanlog = meanlog, sdlog = sdlog)/plnorm(x, meanlog = meanlog, sdlog =sdlog, lower.tail = F)

curve(log_norm_haz(x, meanlog = 0, sdlog = 0.25), from=0, to =5, ylab = 'Haz', xlab = 'Time',col = 'Red')
curve(log_norm_haz(x, meanlog = 1, sdlog = 0.25), from=0, to =5, ylab = 'Haz', xlab = 'Time',col = 'Blue', add = T)
curve(log_norm_haz(x, meanlog = 2, sdlog = 0.25), from=0, to =5, ylab = 'Haz', xlab = 'Time',col = 'Grey', add = T)


library(survival)