###################################
#
#           Ch.6 Exercises 
#
##################################

"6.1. The data set hepatocelluar is in the asaur package. It contains 17 clinical
and biomarker measurements on 227 patients, as well as overall survival and
time to recurrence, both recorded in months [42, 43]. There are three measures of 
CXCL17 activity, CXCL17T (intratumoral), CXCL17P (peritumoral), and
CXCL17N (nontumoral). There is a particular interest in whether they are related to
overall and also recurrence-free survival. Which of the three is most strongly related
for each survival outcome? For the one most strongly related with survival, fit a
spline model and plot it, as we did in Sect. 6.5. Does this suggest that categorizing
CXCL17 would be appropriate?"
library(asaur)
model_0 = coxph(Surv(OS, Recurrence)~1, data = hepatoCellular)

model_17T = coxph(Surv(OS, Recurrence)~CXCL17T, data = hepatoCellular)
model_17P = coxph(Surv(OS, Recurrence)~CXCL17P, data = hepatoCellular)
model_17N = coxph(Surv(OS, Recurrence)~CXCL17N, data = hepatoCellular)
model_all =  coxph(Surv(OS, Recurrence)~CXCL17P + CXCL17T + CXCL17N, data = hepatoCellular)

model_17T
model_17P
model_17N

anova(model_0, model_17T)
anova(model_0, model_17P)
anova(model_0, model_17N)

anova(model_all, model_17T)
anova(model_all, model_17P)
anova(model_all, model_17N)

"6.2. For the covariates with complete data (in Columns 1-22), use stepwise
regression with AIC to identify the best model for (a) overall survival, and
(b) recurrence-free survival."

modelAll.coxph <- coxph(Surv(OS, Recurrence) ~ Number + Age + Gender + HBsAg  + Cirrhosis  + ALT + AST+ AFP+             
													Tumorsize  +Tumordifferentiation + Vascularinvasion  +   Tumormultiplicity   +
													Capsulation    +      TNM           +       BCLC                + 
													Death          +      RFS             +     Recurrence     +      CXCL17T +             
													CXCL17P     +         CXCL17N, data = hepatoCellular)
result.step <- step(modelAll.coxph, scope=list(upper=~ Number + Age + Gender + HBsAg  + Cirrhosis  + ALT + AST+ AFP+             
																							 	Tumorsize  +Tumordifferentiation + Vascularinvasion  +   Tumormultiplicity   +
																							 	Capsulation    +      TNM           +       BCLC                + 
																							 	Death          +      RFS             +     Recurrence     +      CXCL17T +             
																							 	CXCL17P     +         CXCL17N, lower=~1) )

result.step
