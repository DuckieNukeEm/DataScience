  
#Loading Package and Data set
  library('mboost')
  data("bodyfat",package = "TH.data")

#running anormal regression on teh data
  lm1 = lm(DEXfat ~ hipcirc + kneebreadth + anthro3a, data = bodyfat)
  coef(lm1)
  
#now using mboost to see what model it specifies
  glm1 = glmboost(DEXfat ~ hipcirc + kneebreadth + anthro3a, data = bodyfat)
  coef(glm1)
  coef(glm1, off2int = T)

#trying the every variable (IE a ~. (pronunced 'twiddle dot' hehe))
  glm2 = glmboost(DEXfat ~.,data = bodyfat)
  coef(glm2, off2int = T)
  
#OR, we can build the direction with the paste function, I'm not a fan of this but it works
  preds = names(bodyfat[,names(bodyfat)!= 'DEXfat'])
  (fm = as.formula(paste('DEXfat~',paste(preds, collapse = '+'))))
  glm2 = glmboost(fm, data=bodyfat)
  coef(glm2,off2int = T)
  #notice how not all the variables are shown, we can show it using which = ""
  coef(glm2, which = "", off2int = T)

#let's plot how each variable worked out
  plot(glm2)
  #that plots dominated by one line, so lets zoom in to really get a good fit
  plot(glm2, ylim = range(coef(glm2, which = preds)))
  
#Now going to try and apply nonlinear terms to see if anything pops
#bbs defaults to diffrence of 2, which goes to x^2 I believe
  #well....techincally it penalizes variation from a linear model, but who really cares 
  gam1 = gamboost(DEXfat ~ bbs(hipcirc) + bbs(kneebreadth) + bbs(anthro3a), data = bodyfat)
  par(mfrow = c(1,3))
  plot(gam1)
  
#now doing decompositions
  #first gotta create a term to fit an intercept to
    bodyfat$int = rep(1,length(bodyfat$DEXfat))
  #now gotte mean center every variables we going to use (#BLOODYHELL)
    bodyfat2 = bodyfat
    bodyfat2$hipcirc = bodyfat2$hipcirc - mean(bodyfat2$hipcirc)
    bodyfat2$kneebreadth = bodyfat2$kneebreadth - mean(bodyfat2$kneebreadth)
    bodyfat2$anthro3a = bodyfat2$anthro3a - mean(bodyfat2$anthro3a)
    gam2 = gamboost(DEXfat ~ bols(int, intercept = F) +  bols(hipcirc, intercept = F) + bbs(hipcirc, center = T, df=1) + bols(kneebreadth , intercept = F) + bbs(kneebreadth,center = T, df=1) + bols(anthro3a , intercept = F)+ bbs(anthro3a, center = T, df=1), data = bodyfat2)
    #at this point, I would like to point out that DF is the DEgree of Freedom, IE the number of Vars, so in this case df= 1 is x^s (I believe) and df = 4 goes up to x^5
    par(mfrow = c(2,4))
    plot(gam2)  
  #Adamnit, now it's working, guess I solved the problem
  #okay, lets review, we did we split the vars up into two parts the bols and bbx. according to one paper, this is known as decomposition
    #and in terms of the theory, it better seperates out what effects what by having a specific linear term and a speficic NON-linear term,
    #thus, if either the liner or non linear term fail to be selected, then you know if its either non linear or liner (Respectivly)
    #Now, why did we have to center the mthe 3 variables?
    #It's becuase gamboost will force the variables through the intercept (if intercept is turned off, which it is). If the data doesn't go therouh
    #the intercept, then clearly, this won't produce a very good, and potential, not a very accurate model. 
    
    
#Okay, now we don't want the model to be overfit, so we want to stop at hte correct level
#of itteration
    bodyfat = bodyfat[,names(bodyfat)!='int']
    gam3 = gamboost(DEXfat ~., baselearner = 'bbs', data =bodyfat, control =  boost_control(trace = T))
    set.seed(123)
  #now we are doing Cross validation
    (cvm = cvrisk(gam3))
  #optimal numbe ris 38
    par(mfrow = c(1,1))
    
  #p  rogramtically extract optimal number
    mstop(cvm)
  #better yet set it right into the model
    gam3[mstop(cvm)]
    summary(gam3)
    par(mfrow=c(2,4))
    plot(gam3)

#additonaly, we can extract how each compont is working. BEHOLD
    summary(extract(bbs(bodyfat$hipcirc)))