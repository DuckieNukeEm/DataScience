# Keplan-Mieir estimate

library(survival)
tt <- c(7,6,6,5,2,4)
cens <- c(0,1,0,0,1,1)
Surv(tt, cens)

# creating the estimator
result.km <- survfit(Surv(tt, cens) ~ 1, conf.type="log-log")
result.km
summary(results.km)
plot(results.km)

### creating the Nelson-Altschuler estimate
result.fh = survfit(Surv(tt,cens)~1, conf.type = "log-log", type = "fh" )
summary(result.fh)

### application using gastricXelox dataset

timeMonths <- gastricXelox$timeWeeks*7/30.25
delta <- gastricXelox$delta
result.km <- survfit(Surv(timeMonths, delta) ~ 1,conf.type="log-log")
plot(result.km, conf.int=T, mark="|", xlab="Time in months", ylab="Survival probability")
title("Progression-free Survival in Gastric Cancer Patients")


###median follow up time
delta.followup <- 1 - delta
survfit(Surv(timeMonths, delta.followup) ~ 1)

####Using a kernal to estimate the hazard function
install.packages('muhaz')
library(muhaz)

t.vec <- c(7,6,6,5,2,4)
cens.vec <- c(0,1,0,0,1,1)
result.simple <- muhaz(t.vec, cens.vec, max.time=8, bw.grid=2.25, bw.method="global", b.cor="none")
plot(result.simple)


#### applying this to the GastrixXlox dataset, but first we are going to split the data set up
result.pe5 <- pehaz(timeMonths, delta, width=5, max.time=20)
plot(result.pe5, ylim=c(0,0.15), col="black")
result.pe1 <- pehaz(timeMonths, delta, width=1, max.time=20)
lines(result.pe1)


###Now computing a smoothed hazard function
result.smooth <- muhaz(timeMonths, delta, bw.smooth=20, b.cor="left", max.time=20)
lines(result.smooth)


#### now that we have a smoothed hazard function, we can compute the smooothed Survival functions

haz <- result.smooth$haz.est
times <- result.smooth$est.grid
surv <- exp(-cumsum(haz[1:(length(haz)-1)]*diff(times)))

###Lets compuare our smoothed hazard function to the KM estimate
	result.km <- survfit(Surv(timeMonths, delta) ~ 1, conf.type="none")
plot(result.km, conf.int=T, mark="|", xlab="Time in months",xlim=c(0,30), ylab="Survival probability")
lines(surv ~ times[1:(length(times) - 1)])

####LEFT TRUNCATION
tt <- c(7, 6, 6, 5, 2, 4)
status <- c(0, 1, 0, 0, 1, 1)
backTime <- c(-2, -5, -3, -3, -2, -5)
tm.enter <- -backTime
tm.exit <- tt - backTime
result.left.trunc.km <- survfit(Surv(tm.enter, tm.exit, status,+ type="counting") ~ 1, conf.type="none")
summary(result.left.trunc.km)
result.left.trunc.naa <- survfit(Surv(tm.enter, tm.exit, status, type="counting") ~ 1, type="fleming-harrington", conf.type="none")
 summary(result.left.trunc.naa)
 
 
 
##################################
#
#           Ch.3 Exercises 
#
#################################
 
 "3.1. Refer to Fig. 3.2. Find the median survival, and a 95 % confidence interval for
 the median. Explain why the upper limit of the confidence interval is undefined."
 
 tt <- c(7,6,6,5,2,4)
 cens <- c(0,1,0,0,1,1)
 Surv(tt, cens)
 result.km <- survfit(Surv(tt, cens) ~ 1, conf.type="log-log")
 plot(result.km)
 result.km
 
 
the reaosn why the upper 95% CI doesnt exists is becuase the model stops at Year 7, but what we can see is that 
the upper interval would beyond year 7, which is a NGJ.
 
 
 "3.2. In Fig. 3.3, find the first and third quartiles, and 95 % confidence intervals for
 these quartiles. If any of these quantities are undefined, explain."
 
timeMonths <- gastricXelox$timeWeeks*7/30.25
delta <- gastricXelox$delta
result.km <- survfit(Surv(timeMonths, delta) ~ 1,conf.type="log-log")
plot(result.km, conf.int=T, mark="|", xlab="Time in months", ylab="Survival probability")
title("Progression-free Survival in Gastric Cancer Patients")

p_surv = function(s, p){
	s = summary(s)
	t_index = match(max(s$surv[s$surv <= p]),s$surv)
	p_time = s$time[t_index]
	ll_p = log(-log(p))
	
	p_values = function(s, t, p){
		numerator = log(-log(s$surv[t])) - log(-log(p))
		denominator =  sum(s$n.event[1:t]/(s$n.risk[1:t]*(s$n.risk[1:t]-s$n.event[1:t])))/log(s$surv[t])^2
		val = numerator/denominator	
		return(val)
	}
	p_range =  sapply(1:NROW(s$surv), function(x) p_values(s, x, p))
	t_min = match(min(p_range[p_range >= -1.96]), p_range)
	t_max = match(max(p_range[p_range <= 1.96]), p_range)
	return(list(point = s$time[t_index], lowerCI95 = s$time[t_min], upperCI95 = s$time[t_max] ))
}

p_surv(result.km, .75)
p_surv(result.km, .25) #ERRROR
numerator = log(-log(s$surv[t_index])) - log(-log(p)); denominator =  sum(s$n.event[1:t_index]/(s$n.risk[1:t_index]*(s$n.risk[1:t_index]-s$n.event[1:t_index])))/log(s$surv[t_p_index])^2; val = numerator/denominator


 "3.3. Find a smooth hazard function estimate for the gastric cancer data using kernel
 width bw.grid = 20. Explain reason for the multiple peaks in the estimate."

result.simple <- muhaz(gastricXelox$timeWeeks, gastricXelox$delta, max.time=217, bw.grid=20, bw.method="global", b.cor="none")
plot(result.simple)

The hazard rate increases, then drops, then increase, and drops again, this results in humps in the data


 "3.4. Estimate the survival distribution for men, conditional on reaching the age of
 68, ignoring the left truncation times. Discuss the bias of this estimate by comparing
 to the estimate presented in Sect. 3.4."

dd = ChanningHouse[ChanningHouse$sex == "Male" & ChanningHouse$entry/12 >= 68,4:5]
old_men = survfit(Surv(dd$time, dd$cens)~1)
old_men_haz = muhaz(dd$time, dd$cens, max.time=137)

haz = old_men_haz$haz.est
times = old_men_haz$est.grid
ss = exp(-cumsum(haz[1:(length(haz)-1)] * diff(times)))
plot(old_men_haz)
plot(old_men)
lines(ss~times[1:(length(times)-1)])

The survival is nowhere near as sharp as I would expect it to be....:/