##########
#
# Loading Packages
#
##########

	require(ggplot2)
	require(dplyr)
	require(tidyr)

##########
#
# reading in dat afile
#
##########

	elf = read.csv('d:/data/WOW/WOWAH_elf.csv', stringsAsFactors = F)
	
	
	
	SubstrRight <- function(x, n){
		substr(x, nchar(x)-n+1, nchar(x))
	}
	
	SubstrLeft = function (string,char){
		substr(string,1,char)
	}
	
yrmnth_to_months = function(D){
	D = as.integer(SubstrLeft(D,4))*12 + as.integer(SubstrRight(D,2))
	return(D)
}	
	
##########
#
# Checking the data
#
##########
	nrow(elf)
	summary(elf)
	sum(complete.cases(elf))
	unique(SubstrLeft(elf$timestamp,8))
	#% of characters type
		table(elf$class)/nrow(elf)
	
	
		
##########
#
# Reformating Data
#
##########
		
	#Date Time Adjustment
		elf$Date = as.Date(elf$timestamp, format = '%m/%d/%y %H:%M:%S')
		elf$Time =  sapply(elf$timestamp, substrRight, 8, USE.NAMES = F) 
		elf = elf[,c("Date","Time","char","guild","level","class") ]
	#reducing it down
		elf_cd = distinct(elf[c("Date","char","guild","level","class")])
		
	#adding in Tenure
		elf_cd = left_join(elf_cd, elf_cd %>% group_by(char) %>% summarise(Start_dt = min(Date)), by = 'char')
		elf_cd = data.frame(elf_cd %>% group_by(char, Date) %>% mutate(Tenure = Date - Start_dt))
		elf_cd = elf %>% 
							group_by(Date, char, guild, level, class) %>% 
							summarise(Playtime = (n()/6.0)) %>%
							right_join(elf_cd, by=c("Date","char","guild","level","class"))
	
##########
#
#  Retention
#
##########
if(TRUE){
	#Start VAlues
		elf_start = elf_cd %>% filter(Start_dt == Date)
	#DAU
		dau = elf_cd %>%  mutate(cohort = format(Start_dt, '%Y%m'))%>% group_by(Date,cohort) %>% summarise(dau = n_distinct(char)) %>% group_by()
		dau_class = elf_cd %>% group_by(Date, class) %>% summarise(dau = n_distinct(char)) %>% group_by()
		dau_level = elf_cd %>% group_by(Date, level) %>% summarise(dau = n_distinct(char)) %>% group_by() %>% mutate(LevelRange = cut(level, c(0,20,40,60,90)))
		
		#plot
			ggplot(dau, aes(Date, dau)) + geom_point() + geom_line() + geom_smooth()
			ggplot(dau_class, aes(Date, dau, color = class)) + geom_line()
			ggplot(dau_level, aes(Date, dau, color = LevelRange)) + geom_smooth()

	#MAU
		mau = data.frame(elf_cd %>% group_by(Date) %>% distinct(Date) %>% select(Date) %>% group_by()	)
		mau$mau = sapply(mau$Date, function(x) 
																									elf_cd%>%
																									filter(Date >= as.Date(x) - 30 & Date <= as.Date(x)) %>% 
																									summarise(mau = n_distinct(char)) %>% select(mau)
																									, USE.NAMES = F)
		
	 #retention
		elf_r =  left_join(elf_start[,c("Start_dt","char", "class")], elf_cd %>% 
											 																	 filter(Date - 1 == Start_dt)%>%
											 																	 mutate(char_1 = 1) %>%
											 																	select(Start_dt, char, char_1, Date), by = c("Start_dt", "char")) 
		elf_r =  left_join(elf_r, elf_cd %>%
											 																	filter(Date - 3 == Start_dt) %>%
											 																	mutate(char_3 = 1) %>%
											 																	select(Start_dt, char, char_3, Date), by = c("Start_dt", "char")) 
		elf_r =  left_join(elf_r, elf_cd %>%
											 																	 filter(Date - 7 == Start_dt) %>%
											 																	 mutate(char_7 = 1) %>%
											 																	 select(Start_dt, char, char_7, Date), by = c("Start_dt", "char"))
		elf_r =  left_join(elf_r, elf_cd %>%
											 																		filter(Date - 30 == Start_dt) %>%
											 																		mutate(char_30 = 1) %>%
											 																		select(Start_dt, char, char_30, Date), by = c("Start_dt", "char"))	
		elf_r = elf_r[,c('Start_dt', 'char', 'class', 'char_1', 'char_3', 'char_7', 'char_30')]
		elf_r[is.na(elf_r$char_1),]$char_1 = 0
		elf_r[is.na(elf_r$char_3),]$char_3 = 0
		elf_r[is.na(elf_r$char_7),]$char_7 = 0
		elf_r[is.na(elf_r$char_30),]$char_30 = 0
		
		#putting rettentoin into two tables, one straight up, the other one by class
			#straight up
				retention =elf_r %>% 
										group_by(Start_dt) %>%
										summarise( ret_1 = mean(char_1),
															 ret_3 = mean(char_3),
															 ret_7 = mean(char_7),
															 ret_30 = mean(char_30))
				retention$DayOfWeek = weekdays(as.Date(retention$Start_dt))
				
				ggplot(retention %>% group_by(Start_dt, ret_1 ), aes(Start_dt, ret_1)) + 
					geom_line() + 
					geom_line(aes(Start_dt,ret_3), color = 'purple') +
					geom_line(aes(Start_dt,ret_7), color = 'blue')  +
					geom_line(aes(Start_dt,ret_30), color = 'red')  +
					ggtitle("1, 3, 7, and 30 day rention based on Start Date") +
					xlab("Starting Date") +
					ylab("Retention %") +
					theme()
				
				ggplot(retention, aes(Start_dt, ret_1, color = DayOfWeek)) + geom_line()
			#by class
				retention_c =elf_r %>% 
					group_by(Start_dt, class) %>%
					summarise( ret_1 = mean(char_1),
										 ret_3 = mean(char_3),
										 ret_7 = mean(char_7),
										 ret_30 = mean(char_30))
				retention$DayOfWeek = weekdays(as.Date(retention$Start_dt))
				
				ggplot(retention %>% group_by(Start_dt, ret_1 ), aes(Start_dt, ret_1)) + 
					geom_line() + 
					geom_line(aes(Start_dt,ret_3), color = 'purple') +
					geom_line(aes(Start_dt,ret_7), color = 'blue')  +
					geom_line(aes(Start_dt,ret_30), color = 'red')  +
					ggtitle("1, 3, 7, and 30 day rention based on Start Date") +
					xlab("Starting Date") +
					ylab("Retention %") +
					theme()
				
				ggplot(retention_c, aes(Start_dt, ret_1)) + geom_line()
	#going to add Sessions count
}
		
		
##########
#
#  Fretention Anlysis
#
##########		
		#creating the df needed for retention analysi
		#The onel below is used for a Traingle
			elf_ra = elf_cd %>% group_by(char, class) %>% 
						mutate(cohort = format(Start_dt, '%Y%m'), YearMonth = format(Date, '%Y%m')  ) %>%
						group_by (class, cohort, YearMonth) %>%
						summarise(CharCount = n_distinct(char))
			
			elf_ra = elf_ra %>% filter(cohort == YearMonth) %>%
											select(cohort, CharCount, class) %>% 
											rename(cohort_size = CharCount ) %>%
							right_join(elf_ra, by = c("cohort","class")) 
						
			
			#creating the df needed for retention analysi
			#The onel beelow is used for Retention analysis
			
							
	elf_ra = elf_cd %>% group_by(char, class) %>% 
		mutate(cohort = format(Start_dt, '%Y%m'), YearMonth = format(Date, '%Y%m')  ) %>%
		mutate(Tenure = yrmnth_to_months(YearMonth) - yrmnth_to_months(cohort)) %>%
		group_by (class, cohort, Tenure) %>%
		summarise(CharCount = n_distinct(char), TotalPlayTime = sum(Playtime))
	
	elf_ra = elf_ra %>% filter(Tenure == 0 ) %>%
		select(cohort, CharCount, class) %>% 
		rename(cohort_size = CharCount ) %>%
		right_join(elf_ra, by = c("cohort","class")) 
	
	elf_ra %>% group_by(cohort, Tenure) %>% 
		summarise(cohort_size = sum(cohort_size), CharCount = sum(CharCount), TotalPlayTime = sum(TotalPlayTime)) %>%
		group_by(cohort, Tenure) %>%
		mutate(Retention = CharCount/cohort_size, AveragePlayTime = TotalPlayTime/cohort_size) %>%
		#write.csv(file="c:/scripts/retention.csv", row.names = F)
		mutate(Retention = round(ifelse(is.na(Retention),0,Retention),2)) %>%
		filter(Tenure > 0 ) %>%
		data.frame() 	%>%
		ggplot(aes(Tenure, cohort,  fill = Retention, label = Retention)) + 
		geom_tile() +
		geom_text(aes(size = 1)) +
		scale_fill_gradient(low = 'light blue', high = 'dark blue')
	#What the fuck is going on from 200709 to 200902?
	
	elf_ra = elf_cd %>% group_by(char, guild) %>% 
		mutate(cohort = format(Start_dt, '%Y%m'), YearMonth = format(Date, '%Y%m')  ) %>%
		mutate(Tenure = yrmnth_to_months(YearMonth) - yrmnth_to_months(cohort)) %>%
		group_by (guild, cohort, Tenure) %>%
		summarise(CharCount = n_distinct(char))
	
	
	elf_ra %>% group_by(cohort, Tenure) %>% 
		summarise(Cohort_Size = sum(cohort_size), CharCount2 = sum(CharCount)) %>%
		group_by(cohort, Tenure) %>%
		transmute(Retention = CharCount2/Cohort_Size) %>%	
		spread(Tenure, Retention) %>%
		write.csv(file="c:/scripts/test.csv", row.names = F)
	
	elf_cd %>% mutate(cohort = format(Start_dt, '%Y%m'),
										YearMonth = format(Date, '%Y%m'), 
										guild = ifelse(guild < 0, 0, 1)) %>%
		mutate(Tenure = yrmnth_to_months(YearMonth) - yrmnth_to_months(cohort)) %>%
		group_by(cohort, Tenure, guild) %>%
		summarize(CharCount = n_distinct(char))
	
	
##########
#
#  Funnel
#
##########
if(TRUE){
	elf_f = elf %>% group_by(char, Date, level, class) %>% summarise(Play_Length = (n()/6.0))
	elf_fl = elf_f %>% group_by(char, level, class) %>% summarise(Avg_Play_length = mean(Play_Length), n_days = n())
						elf_cd %>% 
						mutate(cohort = format(Start_dt, '%Y%m') ) %>%
						group_by(level,cohort) %>%
						summarize(CharCount = n_distinct(char),
											TotalPlayTime = sum(Playtime)) %>%
						mutate(AvgPlayTime = TotalPlayTime/CharCount) %>%
							filter(level != 70 & level != 80) %>%
					#	group_by(level,cohort)%>%
					#	summarize(CharCount = sum(CharCount)) %>%			 
						ggplot(aes(level, AvgPlayTime, col = cohort)) + geom_line()
	
	
	
}		
		
				