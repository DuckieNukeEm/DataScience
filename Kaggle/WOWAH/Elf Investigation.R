	
if(TRUE){	

##########
#
# Loading Packages
#
##########

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
#	t_ext = function(df, by)
	
}
##########
#
# reading in data file
# and cleaning it up	
#
##########
	
	#Reading in the data
		elf = data.frame(fread('d:/data/WOW/WOWAH_elf.csv', stringsAsFactors = F))
	#Date Time Adjustment
		elf$Date = as.Date(elf$timestamp, format = '%m/%d/%y %H:%M:%S')
		elf = elf[,c("Date","char","guild","level","class") ]
	#reducing it down
		elf_cd = distinct(elf[c("Date","char","guild","level","class")])
	
	#adding in Tenure
		elf_cd = left_join(elf_cd, elf_cd %>% group_by(char) %>% summarise(Start_dt = min(Date)), by = 'char')
		elf_cd = data.frame(elf_cd %>% group_by(char, Date) %>% mutate(Tenure = Date - Start_dt))
		elf_cd$Tenure = as.integer(elf_cd$Tenure)
	#adding in playtime (in hours)	
		elf_cd = data.frame(elf %>% 
			group_by(Date, char, guild, level, class) %>% 
			summarise(Playtime = (n()/6.0)) %>%
			right_join(elf_cd, by=c("Date","char","guild","level","class")))
	#adding in cohort
		elf_cd = elf_cd %>% mutate(cohort = format(Start_dt, '%Y%m'), YearMonth = format(Date, '%Y%m'))
	#adding in stop date 
		elf_cd = data.frame(left_join(elf_cd, elf_cd %>%
											 	group_by(char) %>%
											 	summarise(EndDate = max(Date)), by = 'char'))
	if(FALSE){
	#incase you need it adding a count to the file
		elf_cd = data.table(elf_cd)
		elf_cd[,Index := 1]
		elf_cd[,Index:=cumsum(Index), by = 'char']
		elf_cd = data.frame(elf_cd)
	}
##########
#
# answering some basic questions from the variables perspective
#
##########
	
		
		#How guilds affects retention
		elf_cd %>%
			mutate(In_Guild = ifelse(guild == -1, 'N', 'Y')) %>%
			group_by(Start_dt, Date, In_Guild)%>%
			summarise(UsersCount = n_distinct(char)) %>%
			left_join( 
				(elf_cd %>%
				 	#mutate(In_Guild = ifelse(guild == -1, 'N', 'Y')) %>%
				 	filter(Start_dt == Date) %>%
				 	group_by(Start_dt)%>%
				 	summarise(StartCount = n_distinct(char))
				), by = c("Start_dt")) %>%
			mutate(Retention = UsersCount/StartCount) %>%
			ggplot(aes(Date, Retention, col = In_Guild)) + geom_line()
		
		
		
	###	
	#Guilds
	###
		#USers in  guilds
			#year Month		
			elf_cd %>%
				mutate(In_Guild = ifelse(guild == -1, 'N', 'Y')) %>%
				group_by(YearMonth, In_Guild)%>%
				summarise(UsersCount = n_distinct(char)) %>%
				spread(In_Guild, UsersCount) %>%
				mutate(GuildPer = Y/(Y+N)) %>%
				data.frame() %>%
				ggplot(aes(YearMonth,GuildPer, group = 1)) + geom_line()
				
			#Date
			elf_cd %>%
				mutate(In_Guild = ifelse(guild == -1, 'N', 'Y')) %>%
				group_by(Date, In_Guild)%>%
				summarise(UsersCount = n_distinct(char)) %>%
				ggplot(aes(Date,UsersCount, col = In_Guild)) + geom_line()
			
			#Date Per
			elf_cd %>%
				mutate(In_Guild = ifelse(guild == -1, 'N', 'Y')) %>%
				group_by(Date, In_Guild)%>%
				summarise(UsersCount = n_distinct(char)) %>%
				spread(In_Guild, UsersCount) %>%
				mutate(GuildPer = Y/(Y+N)) %>%
				data.frame() %>%
				ggplot(aes(Date,GuildPer, group = 1)) + geom_line()
			
			#date -> playtime
			elf_cd %>%
				mutate(In_Guild = ifelse(guild == -1, 'N', 'Y')) %>%
				group_by(Date, In_Guild)%>%
				summarise( PlayTime = sum(Playtime)) %>%
				spread(In_Guild, PlayTime) %>%
				mutate(PlayTimeGuildPer = Y/(Y+N)) %>%
				data.frame() %>%
				ggplot(aes(Date,PlayTimeGuildPer, group = 1)) + geom_line()
		#When they joined a guilde
			elf_cd %>%
				mutate(In_Guild = ifelse(guild == -1, 'N', 'Y')) %>%
				filter(In_Guild == 'Y') %>%
				group_by(char, guild) %>%
				summarise(Guild_Date = as.Date(min(Date))) %>%
				group_by(Guild_Date) %>% 
 				summarise(N_Join = n_distinct(char)) %>%
				ggplot(aes(Guild_Date, N_Join, group = 1)) + geom_line()
		#when they left a guild
		
		#How long in guilds
			#days PLayed
			elf_cd %>%
				mutate(In_Guild = ifelse(guild == -1, 'N', 'Y')) %>%
				group_by(cohort, char, In_Guild) %>%
				summarise(Time_in_Guild = n_distinct(Date), YearMonth_c = n_distinct(YearMonth) ) %>%
				group_by(cohort) %>%
				summarise(GuildMember = n_distinct(char), AGT = mean(Time_in_Guild/YearMonth_c)) %>%
				ggplot(aes(cohort, AGT, group = 1)) + geom_line()
			#Average Time
			
			
		#How guilds affects retention
			elf_cd %>%
				mutate(In_Guild = ifelse(guild == -1, 'N', 'Y'), Tenure = yrmnth_to_months(YearMonth) - yrmnth_to_months(cohort) ) %>%
				
				group_by(cohort, Tenure, In_Guild)%>%
				summarise(UsersCount = n_distinct(char)) %>%
				right_join( 
						(elf_cd %>%
						 	#mutate(In_Guild = ifelse(guild == -1, 'N', 'Y')) %>%
						 	filter(cohort == YearMonth) %>%
						 	group_by(cohort)%>%
						 	summarise(StartCount = n_distinct(char))
						), by = c('cohort')) %>%
				group_by(Tenure, In_Guild) %>%
				summarize(UsersCount = sum(UsersCount), StartCount = sum(StartCount)) %>%
			mutate(Retention = UsersCount/StartCount) %>%
				filter(Tenure > 0) %>%
				ggplot(aes(Tenure, Retention, col = In_Guild)) + geom_line()
		#Colleated against dua
		#average Number of guilds per person
			elf_cd %>%
				filter(guild > -1) %>%
				group_by(char, level) %>%
				summarise(GuildCount = n_distinct(guild)) %>%
				group_by(level) %>%
				summarise(GuildCount = mean(GuildCount)) %>%
				ggplot(aes(level, GuildCount)) + geom_line()
			
			elf_cd %>%
				mutate(In_Guild = ifelse(guild == -1, 'N', 'Y')) %>%
				group_by(level, In_Guild) %>%
				summarise(n_char = n_distinct(char)) %>%
				ggplot(aes(level, n_char, col = In_Guild)) + geom_line()
			
			
			#average length of time in guild
			

	###
	#Class
	###
		#How class affects retention
			elf_cd %>%
				group_by(class, Tenure, Start_dt ) %>%
				summarise(UserCount = n_distinct(char)) %>%
				right_join(
					(elf_cd %>%
					 	filter(Start_dt == Date) %>%
					 	group_by(Start_dt, class) %>%
					 	summarise(StartCount = n_distinct(char))),
					by = c("Start_dt", "class")
				) %>%
				group_by(class, Tenure) %>%
				summarise(N_User = sum(UserCount), N_Start = sum(StartCount)) %>%
				mutate(retention = N_User / N_Start) %>%
				filter(Tenure > 0) %>%
				ggplot(aes(Tenure, retention, col = class)) + geom_line()
			
		#how class affects playtime
			elf_cd %>%
				group_by(class) %>%
				summarise(UserCount = n_distinct(char), PlayTime = sum(Playtime)) %>%
				mutate(AvgPlayTime = PlayTime/UserCount) %>%
				ggplot(aes(class, AvgPlayTime)) + geom_bar(stat = "identity")
		#Class breakdown by date
			elf_cd %>% 
				group_by(Date, class) %>%
				
				filter(class != 'Death Knight') %>%
				summarise(UserCount = n_distinct(char)) %>%
				
			right_join( elf_cd %>% 
										group_by(Date) %>%
										filter(class != 'Death Knight') %>%
										summarise(TotalUserCount = n_distinct(char)), by = "Date") %>%
				mutate(ClassPer = UserCount/TotalUserCount) %>%
				ggplot(aes(Date, ClassPer, col = class)) + geom_smooth( ) + geom_line()
	
		#how class affects guilds enrollment
				elf_cd %>%
					mutate(In_Guild = ifelse(guild == -1, 'N', 'Y')) %>%
				#	filter(EndDate == as.Date('2009-01-10'), class != 'Death Knight') %>%
					group_by(class, Tenure, In_Guild ) %>%
					summarise(UserCount = n_distinct(char)) %>%
					spread(In_Guild, UserCount) %>%
					mutate(GuildPer = Y/(Y+N)) %>%
					
				ggplot(aes(Tenure, GuildPer, col = class)) + geom_smooth()
		
		#how often do people switch classes?
				elf_cd %>% 
					group_by(char) %>%
					summarise(class_count = n_distinct(class)) %>%
					filter(class_count > 1 ) %>%
					ggplot(aes(class_count)) + geom_bar()
				
				
		#how often classe are switched
			#when do peeps witch classes (IE at what level do they swithc)
				t = elf_cd %>% group_by(char, level, class) %>% summarise(n_cound = n()) %>% data.table()
				#t[,Index:=1]
				#t[,Index:=cumsum(Index), by = "char"]
				
				t %>% mutate(Index = Index - 1) %>%
					 rename(l_level = level, l_class = class ) %>%
					select(char, l_level, l_class, Index) %>%
						left_join(t,., by=c('char', 'Index')) %>%
				filter(class != l_class & level < 15) %>%
				group_by(level, class) %>%
				summarise(n_char = n_distinct(char)) %>%
				ggplot(aes(level, n_char, col = class)) + geom_line()			 
		#classes and locations
		#colleatted against DUA
			elf_cd %>%
				group_by(Date, class) %>%
				summarise(dau = n_distinct(char)) %>%
				ggplot(aes(Date, dau, col = class)) + geom_smooth()
	###
	#Location
	###
		
	####average Tenure
			elf_cd %>% group_by(char, class) %>% summarize(max_tenure = max(Tenure)) %>%
			group_by(max_tenure, class) %>%
			summarise(n_count = n_distinct(char)) %>%
				left_join(elf_cd %>% group_by(class) %>% summarise(Start_ct = n_distinct(char)), by = 'class') %>%
			mutate(per = n_count/Start_ct) %>%
			filter(max_tenure < 10) %>%
			ggplot(aes(max_tenure, per, col =class)) + geom_line()
		
##########
#
# answering some basic questions from the questions perspective
#
##########

	###
	# Impact no DAU,MAU, DAU/MAU
	###
		#dau
			elf_cd %>% group_by(Date) %>% summarise(n_count = n()) %>%
				ggplot(aes(Date, n_count)) + geom_line()
		#cohort
			elf_cd %>% group_by(Date, cohort) %>% summarise(n_count = n_distinct(char)) %>%
				ggplot(aes(Date, n_count, col = cohort)) + geom_line()
		#class
			
			elf_cd %>% group_by(Date, class) %>% summarise(n_count = n_distinct(char)) %>%
				ggplot(aes(Date, n_count, col = class)) + geom_line()
		#Race
		#Guilde
			
			elf_cd %>% 			mutate(In_Guild = ifelse(guild == -1, 'N', 'Y')) %>%
				group_by(Date, In_Guild) %>% summarise(n_count = n_distinct(char)) %>%
				ggplot(aes(Date, n_count, col = In_Guild)) + geom_line()
		#level
			
			elf_cd %>% group_by(Date, level) %>% summarise(n_count = n_distinct(char)) %>%
				ggplot(aes(Date, n_count, col = level)) + geom_line()
		#Location
		
	###
	# Impact no retention
	###	
		#cohort
			elf_cd %>% 
							mutate(Tenure = yrmnth_to_months(YearMonth) - yrmnth_to_months(cohort)) %>%
							group_by(Tenure, cohort) %>%
				
						summarise(n_count = n_distinct(char)) %>%
						left_join(.,
											elf_cd %>% 
												filter(YearMonth == cohort) %>%
												group_by(cohort) %>%
												summarise(StartCount = n_distinct(char)) 
											, by = 'cohort') %>%
							mutate(Retention = n_count/StartCount) %>% 
				ggplot(aes(Tenure, Retention, col = cohort)) + geom_line()
			
		#class
			
			elf_cd %>% 
				mutate(Tenure = yrmnth_to_months(YearMonth) - yrmnth_to_months(cohort)) %>%
				group_by(Tenure, class) %>%
				
				summarise(n_count = n_distinct(char)) %>%
				left_join(.,
									elf_cd %>% 
										#filter(YearMonth == cohort) %>%
										group_by(class) %>%
										summarise(StartCount = n_distinct(char)) 
									, by = 'class') %>%
				mutate(Retention = n_count/StartCount) %>% 
				ggplot(aes(Tenure, Retention, col = class)) + geom_line()
		#race
		#guilde
		#level
		#location
	
	###		
	#   MAU
	####		
				u_date = elf_cd %>% select(Date) %>% unique() %>% data.frame() 
				u_c = elf_cd %>% select(Date, class, char) %>% distinct()
				t = sqldf('select
										b.Date,
										"All" as Class,
										count(distinct a.char) as mau
							from
							u_c a,
							u_date b
						where a.date between b.date - 30 and b.date
							group by b.Date
							')
				t = rbind(t, sqldf('select
										b.Date,
													 a.class as Class,
													 count(distinct a.char) as mau
													 from
													 u_c a,
													 u_date b
													 where a.date between b.date - 30 and b.date
													 group by b.Date, a.class
													 '))
				t%>%ggplot(aes(Date, mau, col = Class)) + geom_line()
				
				d = elf_cd %>% mutate( Class = 'All') %>% 
												group_by(Date, Class) %>% 
												summarise(dau = n_distinct(char) )
				d = rbind(d, elf_cd %>% mutate( Class = class) %>% 
										group_by(Date,  Class) %>% 
										summarise(dau = n_distinct(char) ) )
				
				ggplot(d, aes(Date, dau, col = Class)) + geom_line()
				 
				td = inner_join(t, d, by = c("Date","Class")) %>%
							mutate(sticky = dau/mau)
				
				td %>%
					#filter(Class == 'All') %>% 
					ggplot(aes(Date, sticky, col = Class)) + geom_line()
				
#######
# 
# level regression
#
######## 
				
				
				
#######
# 
# cluster analysis
#
######## 
	##
	#level analysis
	##			
		#subselecting 
			big_mother_matrix = expand.grid(Tenure = seq(0,max(elf_cd$Tenure)), char = unique(elf_cd$char))
			
		#populating matrix
			big_mother_matrix = elf_cd %>% select(char, level, Tenure) %>%
																		 distinct() %>%
																			group_by(char, Tenure) %>%
																			summarise(level = max(level)) %>%
																			data.frame() %>%
																left_join(big_mother_matrix, ., by = c('char','Tenure'))
			
		#filling in the na's
		 big_mother_matrix$level = replace_na_with_last(big_mother_matrix$level)
		
		#rotating matrix
			big_mother_matrix = big_mother_matrix %>% spread(Tenure, level)
			
		#renaming
			names(big_mother_matrix)[2] = 'Zero'
			big_mother_matrix = big_mother_matrix %>% filter(Zero <= 20)
			names(big_mother_matrix)[2] = "0"
			
		#	
			big_mother_matrix$ran = runif(nrow(big_mother_matrix))
			
		
		#clusting time omtoher fuckers
			#hclust	
				bmm_s = big_mother_matrix %>% filter(ran <= 0.1)
				bmm_s_d = dist(bmm_s[,2:650])
				bmm_s_h = hclust(bmm_s_d, "ward.D2")
				plot(bmm_s_h)
				#looks like 3 distinct groups
				bmm_s$cluster_hclust = cutree(bmm_s_h, 3)
				aggregate(bmm_s[,1:50], by = list(bmm_s$cluster), FUN = mean)
			#kmeans clusters
					scree_plot(bmm_s[,2:650])
					#looks like we need 3 or 4 clusters, going with 3
					bmm_s$cluster_kmeans = 	kmeans(bmm_s[,2:650], centers = 3)$cluster
			##archatypial analysis
					#	arch = stepArchetypes(bmm_s[2:650], k = 1:8)
				  arch = archetypes(bmm_s[,2:650],3)	
				  barplot(arch)
				  bmm_s$cluster_arch = max.col(coef(arch))
				  
				  bmm_s %>% select(cluster_arch, cluster_kmeans, cluster_hclust) %>%
				  	ggplot(aes(cluster_kmeans, cluster_hclust, col =cluster_arch, )) + geom_point() + geom_jitter()

				  
				  
				  