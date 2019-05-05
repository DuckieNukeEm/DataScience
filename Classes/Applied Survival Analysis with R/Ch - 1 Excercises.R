##################################
#
#           Ch.1 Exercises 
#
#################################

# 1.1. Consider a simple example of five cancer patients who enter a clinical trial as
# illustrated in the following diagram:

	Person     = c('P1','P2','P3','P4','P5')
	PersonYears = c(  5,   5,   4,   3,   1)
	Event =       c(  0,   0,   1,   1,   1)
	
	cancer_5 =data.frame(Person, PersonYears, Event)
# 	
# Re-write these survival times in terms of patient time, and create a simple data
# set listing the survival time and censoring indicator for each patient. 
How many patients died? 3
How many person-years are there in this trial? 18
What is the death rate per person-year? 3/18


# 1.2. For the "gastricXelox" data set, use R to determine how many patients had the
# event (death or progression), the number of person-weeks of follow-up time, and
# the event rate per person-week.

librray(asaur)
sum(gastricXelox[,1]) # number of person-weeks
sum(gastricXelox[,2])/sum(gastricXelox[,1]) #PErson Deaths