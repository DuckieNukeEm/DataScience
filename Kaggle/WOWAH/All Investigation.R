#########
#
# Loading packages and basic funtioncs
#
#########

require(ggplot2)
require(dplyr)
require(tidyr)
require(survival)
require(data.table)
require(archetypes)


##########
#
# Loading Functions
#
##########

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

replace_na_with_last<-function(x,a=!is.na(x)){
	x[which(a)[c(1,1:sum(a))][cumsum(a)+1]]
}

scree_plot = function(data, clust = 1:8, ...){
	wss <- (nrow(data)-1)*sum(apply(data,2,var))
	for (i in 2:max(clust)){
		wss[i] <- sum(kmeans(data,centers=i,...)$withinss)	
	}
	
	plot(clust, wss, type="b", xlab="Number of Clusters",
			 ylab="Within groups sum of squares")
}	


#########
#
# Loading the Data & formating the data
#
#########

	#loading Data
		df = data.frame(fread('d:/data/WOW/All.csv', stringsAsFactors = F))
		
	#ofrmating the Dates	
		df$Start_dt = as.Date(df$Start_dt)
		df$Date = as.Date(df$Date)
	#adding cohort and yearmonth
		df$cohort = format(df$Start_dt, '%Y%m')
		df$YearMonth = format(df$Date, '%Y%m')
	# some weird fucking class in this data, removing that shit
		df = df%>% filter( class %in% c("Hunter","Shaman","Warlock","Warrior",
																			 "Rogue","Druid","Mage","Priest","Paladin", "Death Knight") )
	#adding tenure
		df = df %>% mutate(tenure = as.integer(Date - Start_dt))
	#adding in a count of eachrow by person
		df = df %>% arrange(Date, char, level, guild, class, zone, race) %>% mutate(Index = 1) %>% data.table()
		df[,Index := cumsum(Index), by =c('char')]
		
	#creating an index to for char date
		df_char_date = df %>% select(char, Date) %>%  
											distinct()	%>%
											arrange(char,Date) %>% 
											mutate(Date_Index = 1) %>% 
											data.table()
		df_char_date[,Date_Index:= cumsum(Date_Index),by='char']
	
	#if we want to subdivided the levels into buckets	
		level_buckets = df %>% group_by(level) %>% summarise(n_count =n())
		level_buckets = sqldf("select
														level,
														case when level < 10 then '1-9'
																when level < 20 then '10-19'
																when level < 30 then '20-29'
																when level < 40 then '30-39'
																when level < 50 then '40-49'
																when level < 60 then '50-59'
																when level < 70 then '60-69'
																when level < 80 then '70-79'
																else '80+'
														end as Level_Buckets
													from
														level_buckets
														")
		
##########
#
# Basic DAU 
#
#########
		
	#In Total
		df %>% group_by(Date) %>%
					summarise(n_count = n_distinct(char)) %>%
					ggplot(aes(Date, n_count, by = 1)) + geom_line()
		
	#By Race	
				df %>% group_by(Date, race) %>% 
									summarise(n_count = n_distinct(char)) %>%
									rbind(df %>% group_by(Date) %>%
										 			summarise(n_count = n_distinct(char)) %>%
										 			mutate(race = 'Total') %>%
										 			select(Date, race, n_count)
												) %>%
						filter(race != 'Total') %>%			 		
						ggplot(aes(Date, n_count, col = race )) + geom_line()
							#something intresting is going on with the Troll,
							#it seems have slowly fallen out of favor after the first release
							#then slammed into an all time high at the end of 2008
							#Orc is showing a similar shift with the new release
					
	#by class				
			df %>% group_by(Date, class) %>% 
						summarise(n_count = n_distinct(char)) %>%
						rbind(df %>% group_by(Date) %>%
										summarise(n_count = n_distinct(char)) %>%
										mutate(class = 'Total') %>%
										select(Date, class, n_count)
						)	%>%		
						filter(class != 'Total') %>%
						ggplot(aes(Date, n_count, col = class)) + geom_line() + geom_smooth()
			
			
	#by level (not doing a total bind on this)			
	df %>% group_by(Date, level) %>% 
					summarise(n_count = n_distinct(char)) %>%
					inner_join(level_buckets, by = 'level')  %>%
					ggplot(aes(Date, n_count, col = Level_Buckets)) + geom_line()
	
	
##########
#
# Net Flow of Users
#
#########
		#basic View	
			rbind(df %>%
							group_by(Start_dt) %>%
							summarise(n_count = n_distinct(char)) %>%
							rename(Date = Start_dt) %>%
							mutate(type = "Start"),
								 df %>%
									group_by(char) %>%
									summarise(Date = max(Date)) %>%
								 	group_by(Date) %>%
								 	summarise(n_count = n_distinct(char)) %>%
									mutate(type = "End")
			) %>%
				ggplot(aes(Date, n_count, col = type)) + geom_line()
		#starting by race
			#join
				df %>% group_by(Start_dt, race) %>%
							summarise(n_count = as.double(n_distinct(char))) %>%
						ggplot(aes(Start_dt, n_count, col = race)) + geom_line() + facet_wrap(~race)
			#left	
				inner_join(df %>% select(Date, char, race) %>% distinct(),
										df %>% group_by(char) %>% summarise(Date = max(Date)),
										by = c('char','Date')
									) %>%
					group_by(Date, race) %>%
					summarise(n_count = n_distinct(char)) %>%
					ggplot(aes(Date, n_count, col = race)) + geom_line() + facet_wrap(~race)
			#net change
				rbind( #join
										df %>% select(Start_dt, race, char) %>%
											rename(Date = Start_dt) %>%
											group_by(Date, race) %>% 
											summarise(n_count = as.double(n_distinct(char))) %>%
											mutate(type = 'Joined'),
						 #left
										inner_join(df %>% select(Date, char, race) %>% distinct(),
															 df %>% group_by(char) %>% summarise(Date = max(Date)),
															 by = c('char','Date')
														) %>%
									group_by(Date, race) %>%
									summarise(n_count = n_distinct(char)) %>%
									mutate(type = 'Left')
									
				) %>% spread(type, n_count) %>%
				mutate(net_change = Joined - Left) %>%
				ggplot(aes(Date, net_change, col = race)) + geom_line() + facet_wrap(~race)
				
		#by class
			#join
				df %>% group_by(Start_dt, class) %>%
					summarise(n_count = as.double(n_distinct(char))) %>%
					ggplot(aes(Start_dt, n_count, col = class)) + geom_line() + facet_wrap(~class)
			#left	
				inner_join(df %>% select(Date, char, class) %>% distinct(),
									 df %>% group_by(char) %>% summarise(Date = max(Date)),
									 by = c('char','Date')
									) %>%
					group_by(Date,class) %>%
					summarise(n_count = n_distinct(char)) %>%
					ggplot(aes(Date, n_count, col = class)) + geom_line() + facet_wrap(~class)
			#net change
				rbind( #join
							df %>% select(Start_dt, class, char) %>%
								rename(Date = Start_dt) %>%
								group_by(Date, class) %>% 
								summarise(n_count = as.double(n_distinct(char))) %>%
								mutate(type = 'Joined'),
					#left
							inner_join(df %>% select(Date, char, class) %>% distinct(),
												 df %>% group_by(char) %>% summarise(Date = max(Date)),
												 by = c('char','Date')
													) %>%
							group_by(Date, class) %>%
							summarise(n_count = n_distinct(char)) %>%
							mutate(type = 'Left')
					
				) %>% spread(type, n_count) %>%
					mutate(net_change = Joined - Left) %>%
					ggplot(aes(Date, net_change, col = class)) + geom_line() + facet_wrap(~class)

				
##########
#
# Days from keyboard
#
#########

				
				inner_join(inner_join(df %>% select(char, Date) %>% distinct(),
															df_char_date, by = c('char', 'Date')) %>%
									 	rename(s_date = Date), 
									 
									 inner_join(df %>% select(char, Date, race) %>% distinct(),
									 					 df_char_date, by = c('char', 'Date')) %>%
									 	mutate(Date_Index = Date_Index - 1) %>%
									 	rename(e_date = Date),
									 
									 by = c("char", "Date_Index")
				) %>%
					mutate(Dfk = as.integer(e_date - s_date)) %>%
					filter(e_date > as.Date('2006-01-01'),
								 race == 'Blood Elf',
								 e_date == as.Date('2007-04-03')) 
					
					ggplot(aes(e_date,avg_dfk, col = race)) + geom_line()
				
				
				
	###
	#basic
	###
					
			inner_join(inner_join(df %>% select(char, Date) %>% distinct(),
													 df_char_date, by = c('char', 'Date')) %>%
									rename(s_date = Date), 
								inner_join(df %>% select(char, Date) %>% distinct(),
								 						df_char_date, by = c('char', 'Date')) %>%
								mutate(Date_Index = Date_Index - 1) %>%
								rename(e_date = Date),
								by = c("char", "Date_Index")) %>%
			mutate(Dfk = as.integer(e_date - s_date)) %>%
			group_by(e_date) %>%
			summarise(avg_dfk = mean(Dfk),
								n_count = n_distinct(char)) %>%
			filter(e_date > as.Date('2006-01-01')) %>%	
			ggplot(aes(e_date,avg_dfk, by = 1)) + geom_line()
	###
	# by class
	###
			
			inner_join(inner_join(df %>% select(char, Date) %>% distinct(),
														df_char_date, by = c('char', 'Date')) %>%
								 	rename(s_date = Date), 
								 inner_join(df %>% select(char, Date, class) %>% distinct(),
								 					 df_char_date, by = c('char', 'Date')) %>%
								 	mutate(Date_Index = Date_Index - 1) %>%
								 	rename(e_date = Date),
								 by = c("char", "Date_Index")) %>%
				mutate(Dfk = as.integer(e_date - s_date)) %>%
				group_by(e_date, class) %>%
				summarise(avg_dfk = mean(Dfk),
									n_count = n_distinct(char)) %>%
				filter(e_date > as.Date('2006-01-01'),
							 class != 'Death Knight') %>%	
				
				ggplot(aes(e_date,avg_dfk, col = class)) + geom_smooth()
	###
	# by race
	###
	
			inner_join(inner_join(df %>% select(char, Date) %>% distinct(),
														df_char_date, by = c('char', 'Date')) %>%
				   			 	rename(s_date = Date), 
								 
								 inner_join(df %>% select(char, Date, race) %>% distinct(),
								 					 df_char_date, by = c('char', 'Date')) %>%
								 	mutate(Date_Index = Date_Index - 1) %>%
								 	rename(e_date = Date),
								 
								 by = c("char", "Date_Index")
								) %>%
				mutate(Dfk = as.integer(e_date - s_date)) %>%
				group_by(e_date, race) %>%
				summarise(avg_dfk = mean(Dfk),
									n_count = n_distinct(char)) %>%
				filter(e_date > as.Date('2006-01-01')) %>%	
				ggplot(aes(e_date,avg_dfk, col = race)) + geom_smooth()
			
##########
#
# Basic Retention Analysis 
#
#########
	
	#by cohort
			df %>% group_by(cohort, YearMonth)  %>%
						summarise(n_count = n_distinct(char)) %>%
						mutate(Tenure = yrmnth_to_months(YearMonth) - yrmnth_to_months(cohort)) %>%
						inner_join(df %>% group_by(cohort, YearMonth) %>%
											 				summarise(start_count = n_distinct(char)) %>%
											 				filter(cohort == YearMonth) %>%
											 				select(cohort, start_count),
											 by = 'cohort') %>%
						mutate(Retention = n_count/start_count) %>%
						ggplot(aes(Tenure, Retention, col = cohort)) + geom_line()
	#by class (using level as the time)
	df %>% group_by(cohort, YearMonth)  %>%
		summarise(n_count = n_distinct(char)) %>%
		mutate(Tenure = yrmnth_to_months(YearMonth) - yrmnth_to_months(cohort)) %>%
		inner_join(df %>% group_by(cohort, YearMonth) %>%
							 	summarise(start_count = n_distinct(char)) %>%
							 	filter(cohort == YearMonth) %>%
							 	select(cohort, start_count),
							 by = 'cohort') %>%
		mutate(Retention = n_count/start_count) %>%
		select(cohort, Tenure, Retention) %>%
		spread(Tenure, Retention)
	#by level as the time period
	df %>% 
		mutate(Tenure = level) %>%
		filter(cohort > '200512') %>%
		group_by(cohort, Tenure)  %>%
		summarise(n_count = n_distinct(char)) %>%
		
		inner_join(df %>% group_by(cohort, YearMonth) %>%
							 	filter(cohort == YearMonth) %>%
							 	summarise(start_count = n_distinct(char)) %>%
							 	
							 	select(cohort, start_count),
							 by = 'cohort') %>%
		mutate(Retention = n_count/start_count) %>%
		select(cohort, Tenure, Retention) %>%
		spread(Tenure, Retention) %>%
		write.table( file = "c:/scripts/blah.csv", sep =",", row.names = F)
	
		
	#by class
	df %>% 
		mutate(Tenure = yrmnth_to_months(YearMonth) - yrmnth_to_months(cohort)) %>%
		group_by(Tenure, class)  %>%
		summarise(n_count = n_distinct(char)) %>%
		
		inner_join(df %>% 
							 			filter(cohort == YearMonth) %>%
							 	group_by(class) %>%
							 	summarise(start_count = n_distinct(char)),
							 by = 'class') %>%
		mutate(Retention = n_count/start_count) %>%
		ggplot(aes(Tenure, Retention, col = class)) + geom_line()
	#by race
	df %>% 
		mutate(Tenure = yrmnth_to_months(YearMonth) - yrmnth_to_months(cohort)) %>%
		group_by(Tenure, race)  %>%
		summarise(n_count = n_distinct(char)) %>%
		
		inner_join(df %>% 
							 	filter(cohort == YearMonth) %>%
							 	group_by(race) %>%
							 	summarise(start_count = n_distinct(char)),
							 by = 'race') %>%
		mutate(Retention = n_count/start_count) %>% 
		filter(Tenure > 2) %>%
		ggplot(aes(Tenure, Retention, col = race)) + geom_line()

	#by race (measured via level)
	df %>% 
		mutate(Tenure = level) %>%
		group_by(Tenure, race)  %>%
		summarise(n_count = n_distinct(char)) %>%
		
		inner_join(df %>% 
							 	filter(cohort == YearMonth) %>%
							 	group_by(race) %>%
							 	summarise(start_count = n_distinct(char)),
							 by = 'race') %>%
		mutate(Retention = n_count/start_count) %>%
		ggplot(aes(Tenure, Retention, col = race)) + geom_line()
	
	
	
##########
#
# Survival analysis       
#
#########
	
			stop_date = as.Date('2007-08-01')
			###By aCt date#
					s_df = df %>% filter(Date <= stop_date) %>%
												group_by(char, cohort ) %>% 
												summarise(Maxlevel = max(level),
																	Playtime = sum(Playtime),
																	n_Zone = n_distinct(zone),
																	race = max(race),
																	class = max(class),
																	end_date = max(Date),
																	n_play = n_distinct(Date),
																	maxTenure = max(tenure),
																	guild = max(guild)
																	) %>%
											mutate(left = ifelse(end_date <= as.Date('2007-07-22'), 1, 0),
														 guild = ifelse(guild == -1, 0 , 1))
							
						s_df$ran = runif(nrow(s_df))
						ss_df = s_df %>% filter(ran < 0.1)
						s_surv = Surv(ss_df$maxTenure, ss_df$left)
						#s_fit = survfit(s_surv ~ Maxlevel + Playtime + n_Zone + race + class+ n_play, data = ss_df)		
						s_fit = survfit(s_surv ~ race , data = ss_df)		
						
			#By level
					l_df = df %>% filter(Date <= stop_date) %>%
												group_by(char, level) %>%
												summarise( Playtime = sum(Playtime),
																	 n_zone = n_distinct(zone),
																	 race = max(race),
																	 class = max(class),
																	 n_play = n_distinct(Date),
																	 guild = max(guild)
												) %>%
												mutate(guild = ifelse(guild == -1, 0, 1)) %>%
												inner_join(df %>% filter(Date <= stop_date) %>%
																	 		group_by(char) %>%
																	 		summarise(max_date = max(Date))
																	 , by = c('char')) %>%
												mutate(left = ifelse(max_date <= as.Date('2007-07-22'), 1, 0))
					l_df$ran = runif(nrow(l_df))
					ll_df = l_df %>% filter(ran < 0.1)											 	
					l_surv = Surv(ll_df$level, ll_df$left)						 	
																	 	
																	 	
																	 	
																	 	
																	 		
																	 )
				
				
				
				
	
##########
#
# Archetyal analaysis     
#
#########
	
	char_v = df %>% select(char) %>% unique() %>% data.frame()
	char_v$ran = runif(nrow(char_v))
	
	ss = char_v %>% filter(ran < .02) %>% select(char)
		
	big_mother_matrix = 	expand.grid(tenure = 	seq(0, inner_join(df, ss, by='char') %>% mutate(tenure = as.integer(tenure)) %>% select(tenure)  %>% max()),
																	 char = (df %>% inner_join(ss, by='char') %>% select(char) %>% distinct()))
	
	#populating matrix
		big_mother_matrix = df %>% select(char, level, tenure) %>%
																		distinct() %>%
																		mutate(tenure = as.integer(tenure)) %>%
																		group_by(char, tenure) %>%
																		summarise(level = max(level)) %>%
																		data.frame() %>%
																	left_join(big_mother_matrix, ., by = c('char','tenure')) %>%
																	inner_join(ss, by = 'char') #subsetting right away
		
		
		
	#filling in the na's
		big_mother_matrix$level = replace_na_with_last(big_mother_matrix$level)
	
		
	#rotating matrix
		bmm = big_mother_matrix %>% spread(tenure, level)
		
	#renaming
		names(bmm)[2] = 'Zero'
		bmm = bmm %>% filter(Zero <= 20)
		names(bmm)[2] = "0"
		
		
	######clustering
		#hierach
			hclust = hclust(dist(bmm), 'ward.D2')
			bmm$h_clust = cutree(hclust,7)
		#kmeans
			
		#archatypical
			
			