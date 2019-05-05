##################################
#
#           Ch.4 Exercises 
#
#################################

"4.1. Using the pharmacoSmoking data, compare the two treatments using the
Prentice modification of the Gehan test, and compare your results to those from
the log-rank test."

survdiff(Surv(pharmacoSmoking$ttr, pharmacoSmoking$relapse) ~ pharmacoSmoking$grp)

survdiff(Surv(pharmacoSmoking$ttr, pharmacoSmoking$relapse) ~ pharmacoSmoking$grp, rho = 1)

no diffrence

"4.2. Again using the pharmacoSmoking data, carry out a log-rank test comparing
the two treatments stratifying on employment status."

survdiff(Surv(pharmacoSmoking$ttr, pharmacoSmoking$relapse) ~ pharmacoSmoking$grp + strata(pharmacoSmoking$employment))

survdiff(Surv(pharmacoSmoking$ttr, pharmacoSmoking$relapse) ~ pharmacoSmoking$grp + strata(pharmacoSmoking$employment), rho = 1)


"4.3. Using the pancreatic data set, which has no censored observations, compare
the two groups using a Wilcoxon rank-sum test, using the wilcox.test function
in base R. Compare your results to those from the log-rank and Prentice-modified
Gehan tests."
wilcox.test(pancreatic2$pfs ~ pancreatic2$stage)

survdiff(Surv(pancreatic2$pfs)~pancreatic2$stage)
survdiff(Surv(pancreatic2$pfs)~pancreatic2$stage, rho = 0)
survdiff(Surv(pancreatic2$pfs)~pancreatic2$stage, rho = 1)

"4.4. Again using the pancreatic data set, compare the two groups using overall
survival as the outcome, using both the log-rank test and the Prentice modification
of the Gehan test. Do these two tests yield different results?"

survdiff(Surv(pancreatic2$os)~pancreatic2$stage, rho = 0)
survdiff(Surv(pancreatic2$os)~pancreatic2$stage, rho = 1)