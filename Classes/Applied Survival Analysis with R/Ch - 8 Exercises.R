###################################
#
#           Ch.8 Exercises 
#
##################################


"8.1. Encode the log of the partial likelihood in Eq. 8.1.1 into an R function, and
find the maximum using “optim” (as in Sect. 5.2). Verify that the result matches that
from the “coxph” procedure in Sect. 8.1."

log_L = function(b){
  L = log(1) - log(5 + exp(b)) + log(1) - log(4 + exp(b)) + b - log(2 + 2*exp(b))+log(1) - log(2 + exp(b)) + b - log(1 + exp(b))
  L
}

optim(par = 0, fn = log_L, method = "L-BFGS-B", control=list(fnscale = -1), lower = -3, upper = 1)


Yup, it matches


"8.2. Consider the following synthetic time dependent data:"
  i_id = c(1,2,3,4,5,6)   #Patient ID
  wait.time = c(12,0,0,18,0,17) #time before transplant
  futime = c(58,8,37,28,35,77)  #followup time
  fustat = rep(1,6)   #Dead or alive (apparently all dead...?)
  transplant = c(1,0,0,1,0,1) #did they recieve a transplant
  
  transplant_df = data.frame( i_id, wait.time, futime, fustat, transplant)

"First model the data ignoring the wait time. Then transform the data into startstop
format, then use that form of the data to model “transplant” as a time dependent
covariate. Write out the partial likelihood for these data, and use this partial
likelihood to find the maximum partial likelihood estimate of the coefficient for
transplant. Compare your answer to the results of “coxph”."
  summary(coxph(Surv(futime, fustat) ~ transplant, data  = transplant_df))
  
  t_df = tmerge(transplant_df, transplant_df, 
        id = i_id, death = event(futime, fustat),
        transpl = tdc(wait.time))

  summary(coxph(Surv(tstart, tstop, death) ~ transpl, data = t_df[,-(2:5)]))
  
  L = function(b){ 1/6 * exp(b)/(2+3*exp(b)) * 1/(2 + 2*exp(b)) * 1/(1 + 2*exp(b)) * exp(b)/(2*exp(b))}
  optim(par = 0, fn = L, method = "L-BFGS-B", control=list(fnscale = -1), lower = -3, upper = 1)
  
  
  
"8.3. For the pancreatic data, construct a Schoenfeld residual plot and loess smooth
curve for an identity transform, using transform = “identity” in the coxph.zph
function. Then fit a linear time transfer function, as in Sect. 8.2.1, and plot the fitted
line on the residual plot."
  library(asaur)
  pan_df = pancreatic2
  pan_df$stage.n = ifelse(pan_df$stage == "M",1,0)
   t= coxph(Surv(pfs)~stage.n, data = pan_df)
   
   sch_t = cox.zph(t, transform="identity")
   
   t_adj = coxph(Surv(pfs) ~ stage.n + tt(stage.n) , data = pan_df, tt = function(x,t,...) x*log(t))
   
   plot(sch_t)
   abline(Coef(t_adj), col = "red")
