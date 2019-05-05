###################################
#
#           Ch.9 Exercises 
#
##################################


"9.1. Using the “ashkenazi” data of Sect. 9.1, use “coxme” to fit a random effects
model without the “mutant” fixed effect term. How does the estimate of the variance
of the random effect from this model compare to that from the model that includes
“mutant” as a fixed effect?"
  library(asaur)
  #install.packages("coxme")
  library(coxme)

summary(coxme(Surv(age, brcancer) ~ (1|famID), data = ashkenazi))
summary(coxme(Surv(age, brcancer) ~ mutant + (1|famID), data = ashkenazi))
pchisq(2*(-3564.622 + -3577.01), 1, lower.tail = F)


"9.2. Using the “diabetes” data of Sect. 9.1, fit the interaction model using (1)
the frailty option of “coxph”, using both the gamma and gaussian random effects
options, and (2) using the “cluster” option in “coxph”. Compare the estimated
parameters and standard errors to those from the “coxme” model."
  #install.packages('timereg')
  library(timereg)

  fra_cox_gamma = coxph(Surv(time, status) ~ treat + as.factor(adult) + treat*as.factor(adult) + frailty(id),
                  data = diabetes)
  fra_cox_gaussian = coxph(Surv(time, status) ~ treat + as.factor(adult) + treat*as.factor(adult) + frailty(id, dist = 'normal'),
                 data = diabetes)
  ra_cox_cluster = coxph(Surv(time, status) ~ treat + as.factor(adult) + treat*as.factor(adult) + cluster(id,
                          data = diabetes)


"9.3. Again using the “diabetes” data, use “coxme” to fit a model without the
interaction term. Test for the importance of the interaction term using both a Wald
test and a likelihood ratio test."

result.coxme <- coxme(Surv(time, status) ~ treat +
                        as.factor(adult) + (1 | id),
                       data=diabetes)


"9.4. Repeat the calculations of the cumulative incidence functions for death from
prostate cancer and from other causes from Sect. 9.2.3, but use the age group 75–84
instead of 85 and above."
ps = prostateSurvival
ps$status.prost = ifelse(ps$status == 1, 1, 0)
ps$status.other = ifelse(ps$status == 2, 1, 0)

prostateSurvival.highrisk <- ps %>% filter(grade == "poor", ageGroup == "75-79", stage == "T2")

 status.prost <- {prostateSurvival.highrisk$status == 1}
 status.other <- {prostateSurvival.highrisk$status == 2}
 result.prostate.km <- survfit(Surv(survTime, event=status.prost) ~ 1, data=prostateSurvival.highrisk)

 result.other.km <- survfit(Surv(survTime, event=status.other) ~ 1, data=prostateSurvival.highrisk)
surv.other.km <- result.other.km$surv
time.km <- result.other.km$time/12
surv.prost.km <- result.prostate.km$surv
cumDist.prost.km <- 1 - surv.prost.km


#install.packages("mstate")
library(mstate)

ci.prostate <- Cuminc(time=prostateSurvival.highrisk$survTime, status=prostateSurvival.highrisk$status)

ci1 <- ci.prostate$CI.1 # CI.1 is for prostate cancer
ci2 <- ci.prostate$CI.2 # CI.2 is for other causes
times <- ci.prostate$time/12 # convert months to years
Rci2 <- 1 - ci2

 plot(Rci2 ~ times, type="s", ylim=c(0,1), lwd=2, col="green", xlab="Time in years", ylab="Survival probability")
 lines(ci1 ~ times, type="s", lwd=2, col="blue")
 lines(surv.other.km ~ time.km, type="s",  col="lightgreen", lwd=1)
 lines(cumDist.prost.km ~ time.km, type="s",      col="lightblue", lwd=1)
