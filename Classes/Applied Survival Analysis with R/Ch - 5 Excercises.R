##################################
#
#           Ch.5 Exercises 
#
#################################
library(survival)
"5.1. Consider the data set aml, which is included in the survival package. This
is a study of whether or not maintenance therapy increases survival among patients
with acute myelogenous leukemia, with survival time measured in weeks. The basic
Cox model may be fitted as follows:"
	result <- coxph(Surv(time, status) ~ x, data=aml)
"Create a coarser time variable by expressing it in months instead of weeks as
follows:"
	time.months <- cut(aml$time, breaks=seq(0,161,4), labels=F)
	
"Now re-fit the model, modeling ties using the Breslow, Efron, and exact methods.
Which approximate method gives a result closest to that from the exact method?"

	
	c_efron = coxph(Surv(time.months, aml$status) ~ aml$x, method = "efron")
	c_ebreslow = coxph(Surv(time.months, aml$status) ~ aml$x, method = "breslow")
	c_exact = coxph(Surv(time.months, aml$status) ~ aml$x, method = "exact")
	
	summary(c_efron)
	summary(c_ebreslow)
	summary(c_exact)
	
	
"5.2. Consider again the synthetic data in Table 4.1, discussed in Example 5.1
in Sect. 5.2. Use the basehaz function to obtain an estimate of the baseline
cumulative hazard function. Use this to compute the predicted survival curves for
the control and experimental groups based on the proportional hazards model we
fitted in Sect. 5.2."
s = c(6,7,10,15,19,25)
cen = c(1,0,1,1,0,1)
treat = c(0,0,1,0,1,1)

base_haz = basehaz(coxph(Surv(s, cen)~treat, method = "exact"), centered = F)
summary(base_haz)	


S_0 = exp(-cumsum(base_haz$hazard[1:5] * diff(base_haz$time) ))
S_1 = S_0^exp(-1.326129)

plot(S_0)
lines(S_1)