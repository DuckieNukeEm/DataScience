###################################
#
#           Ch.10 Exercises 
#
##################################

"10.1. Consider the “hepatoCellular” data in the “asaur” package. Use the method
of Sect. 10.3.1 to assess how appropriate a Weibull distribution is for (a) overall
survival, and (b) recurrence-free survival."

library(asaur)
library(survival)

os = survfit(Surv(OS,Recurrence) ~ 1, data = hepatoCellular)
survEst = log(-log(os$surv))
survTime = log(os$time)

plot(survEst ~ survTime)
abline(lm(survEst ~ survTime))


os = survfit(Surv(RFS,Recurrence) ~ 1, data = hepatoCellular)
survEst = log(-log(os$surv))
survTime = log(os$time)

plot(survEst ~ survTime)
abline(lm(survEst ~ survTime))

"10.2. Test for the effect of CXCL17 on overall survival. Which of the three
measures is the best predictor? Repeat for recurrence-free survival."

os <- survreg(Surv(OS, Recurrence) ~ CXCL17T + CXCL17P + CXCL17N, dist="weibull", data = hepatoCellular)
summary(os)

RFS <- survreg(Surv(RFS, Recurrence) ~ CXCL17T + CXCL17P + CXCL17N, dist="weibull", data = hepatoCellular)
summary(RFS)



"10.3. Using the covariates with complete data, use the “step” function to find a wellfitting
model with low AIC for overall survival. Repeat for recurrence-free survival.
Which covariates are included in both models?"


os <- survreg(Surv(OS, Recurrence) ~ Number + Age + Gender + HBsAg  + Cirrhosis  + ALT + AST+ AFP+             
                          Tumorsize  +Tumordifferentiation + Vascularinvasion  +   Tumormultiplicity   +
                          Capsulation    +      TNM           +       BCLC                + 
                          Death          +                   +     Recurrence     +      CXCL17T +             
                          CXCL17P     +         CXCL17N, data = hepatoCellular,
                        dist = "weibull")
result.os <- step(os, scope=list(upper=~ Number + Age + Gender + HBsAg  + Cirrhosis  + ALT + AST+ AFP+             
                                                 Tumorsize  +Tumordifferentiation + Vascularinvasion  +   Tumormultiplicity   +
                                                 Capsulation    +      TNM           +       BCLC                + 
                                                 Death          +                  +     Recurrence     +      CXCL17T +             
                                                 CXCL17P     +         CXCL17N, lower=~1) )

rfs <- survreg(Surv(RFS, Recurrence) ~ Number + Age + Gender + HBsAg  + Cirrhosis  + ALT + AST+ AFP+             
                Tumorsize  +Tumordifferentiation + Vascularinvasion  +   Tumormultiplicity   +
                Capsulation    +      TNM           +       BCLC                + 
                Death          +                   +     Recurrence     +      CXCL17T +             
                CXCL17P     +         CXCL17N, data = hepatoCellular,
              dist = "weibull")
result.rfs <- step(rfs, scope=list(upper=~ Number + Age + Gender + HBsAg  + Cirrhosis  + ALT + AST+ AFP+             
                                   Tumorsize  +Tumordifferentiation + Vascularinvasion  +   Tumormultiplicity   +
                                   Capsulation    +      TNM           +       BCLC                + 
                                   Death          +                +     Recurrence     +      CXCL17T +             
                                   CXCL17P     +         CXCL17N, lower=~1) )

result.os
result.rfs

"10.4. Using the “ashkenazi” data in the “asaur” package, fit a Weibull distribution
to the women with the “wild type” (non-mutant) BRCA genotype, matching the
Kaplan-Meier survival curve at ages 45 and 65. Then predict the probability that a
woman with the wild type BRCA genotype will develop breast cancer before the
age of 70."
