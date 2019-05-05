##########
#
# Loading Packages
#
##########

  require(sqldf)
  require(RH2)
  require(ggplot2)
  require(dplyr)
  require(tidyr)
	require(data.table)
  require(arules)
##########
#
# Loading Functions
#
##########

SubstrRight = function (string, char){
  substr(string,nchar(string)-(char-1),nchar(string))
}

Substreft = function (string,char){
  substr(string,1,char)
}

##########
#
# Reading in Data
#
##########
if (FALSE){
  #Auction Data
    d_a = read.csv('./Data/Games/Glitch/CSV and HTML files/economicData/allGlitchAuctions.csv', stringsAsFactors = F)
  #Sales Display Boxes data
    d_sdb = read.csv('./Data/Games/Glitch/CSV and HTML files/economicData/allGlitchSDBsales.csv', stringsAsFactors = F )
  #Street prices
    d_sp =  read.csv('./Data/Games/Glitch/CSV and HTML files/economicData/glitchStreetPricesDecember.csv', stringsAsFactors = F )
    
##########
#
# Formating Data
#
##########
  
  #Creating a Specific Date Field
    d_a$created_dt = as.Date(d_a$created)
    d_a$expires_dt = as.Date(d_a$expires)
    d_a$disappeared_dt = as.Date(d_a$disappeared)
  
  #Creating a Specific Timestamp field
    d_a$created_ts = format(as.POSIXct(d_a$created) ,format = "%H:%M:%S") 
    d_a$expires_ts = format(as.POSIXct(d_a$expires_ts) ,format = "%H:%M:%S") 
    d_a$disappeared_ts = format(as.POSIXct(d_a$expires) ,format = "%H:%M:%S") 
    
  #Creating Category class_tsid crosswalk
    tsid_to_category = d_a %>% group_by(class_tsid, category) %>% summarise(MaxCreateDt = max(created_dt))
    tsid_to_category = sqldf("select class_tsid, category, max(created_dt) as MaxCreateDt from d_a group by 1,2")
    tsid_to_category2 = sqldf("select a.class_tsid, a.category 
                             from tsid_to_category a 
                             where MaxCreateDt = (select max(b.MaxCreateDt) 
                                                  from tsid_to_category b
                                                  where a.class_tsid = b.class_tsid
                                                  and a.category = b.category )")
  #removing uneeded fields original Date-TS Fields  
    write.table(d_a, './Data/Games/Glitch/CSV and HTML files/economicData/allGlitchAuctions_processed.csv', sep = "|", row.names = F)
    
}

		d_a = data.frame(fread( './Data/Games/Glitch/CSV and HTML files/economicData/allGlitchAuctions_processed.csv', stringsAsFactors = F))
		d_a$created_dt = as.Date(d_a$created_dt)
		d_a$expires_dt = as.Date(d_a$expires_dt)
		d_a$disappeared_df = as.Date(d_a$disappeared_df)
		
    d_a = d_a[,setdiff(names(d_a),c("created","expires","disappeared", "category"))]
##########
#
# Basic Metrics
#
########## 
	
    	#daily active users
  			#dplyer  
				  d_a %>% group_by(created_dt) %>% 
		    								summarise(n_char = n_distinct(player)) %>%
		    								ggplot(aes(created_dt, n_char, by = 1)) + geom_line()
				#SQL 
		    	dau = sqldf("select
    								created_dt,
									strftime('%Y', created_dt * 3600 * 24, 'unixepoch') || strftime('%m', created_dt) as YearMonth ,
    							count(distinct player) as DAU
    							from d_a
    							group by 1,2")
    			plot(dau)
    			
    			
			#Monthy average Users
    	u_date = d_a %>% select(created_dt) %>% distinct()	
    	u_char = d_a %>% group_by(player, created_dt) %>% distinct()
    	mau = sqldf("select
    								u.created_dt as Date,
    								count(distinct c.player) as n_count
    							from
    							u_date u,
    							u_char c
    							where
    							c.created_dt between u.created_dt - 30 and u.created_dt
    							group by 1")		
    	ggplot(mau, aes(Date, n_count, by = 1)) + geom_line()
    	
    	ploat(mau)
    	dau_mau = left_join(mau,dau,by = "created_dt")
    #1/7/30 day retention
    		#creating a table of Created_Dt, MinSTartDt and PlayerCount
    			df_ret = data.frame(d_a %>% group_by(player) %>% summarise( Min_Startdt = min(created_dt)))
    			df_ret = left_join(d_a[,c("player","created_dt")], df_ret, by="player" )
    			start_values = data.frame(df_ret %>% group_by(Min_Startdt) %>% summarise(StartingCount = NROW(unique(player))))
    			df_ret = data.frame(df_ret %>% group_by(created_dt, Min_Startdt) %>% summarise(PlayerCount = NROW(unique(player))))
    	  #making the triangle
    			ret = df_ret %>% spread(created_dt, PlayerCount)
    	
    		#one day retention
    			  df_ret_1 = df_ret[as.Date(df_ret$Min_Startdt) + 1 == as.Date(df_ret$created_dt) , ]
    			  df_ret_1 = left_join(df_ret_1, start_values, by = "Min_Startdt")
    			  df_ret_1$prct_1 = df_ret_1$PlayerCount/df_ret_1$StartingCount
    		#7 day retention
    			  df_ret_7 = df_ret[as.Date(df_ret$Min_Startdt) + 7 == as.Date(df_ret$created_dt) , ]
    			  df_ret_7 = left_join(df_ret_7, start_values, by = "Min_Startdt")
    			  df_ret_7$prct_7 = df_ret_7$PlayerCount/df_ret_7$StartingCount
    		#30 day retention
    			  df_ret_30 = df_ret[as.Date(df_ret$Min_Startdt) + 30 == as.Date(df_ret$created_dt) , ]
    			  df_ret_30 = left_join(df_ret_30, start_values, by = "Min_Startdt")
    			  df_ret_30$prct_30 = df_ret_30$PlayerCount/df_ret_30$StartingCount
    		#combing them together based on Create Date
    			  df_ret_all = left_join(df_ret_1[,c(1,5)], df_ret_7[,c(1,5)], by = "created_dt")
    			  df_ret_all = left_join(df_ret_all, df_ret_30[,c(1,5)], by = "created_dt")
    			  df_ret_all[is.na(df_ret_all$prct_7),3] = 0
    			  df_ret_all[is.na(df_ret_all$prct_30),4] = 0
    			  df_ret_all$day = weekdays(as.Date(df_ret_all$created_dt))
    			  aggregate(df_ret_all, by = list(df_ret_all$day), FUN = mean)
    		#combining them together based on StartDate	  
    			  df_ret_all_start = left_join(df_ret_1[,c(2,5)], df_ret_7[,c(2,5)], by = "Min_Startdt")
    			  df_ret_all_start = left_join(df_ret_all_start, df_ret_30[,c(2,5)], by = "Min_Startdt")
    			  df_ret_all_start[is.na(df_ret_all_start$prct_7),3] = 0
    			  df_ret_all_start[is.na(df_ret_all_start$prct_30),4] = 0
    			  df_ret_all_start$day = weekdays(as.Date(df_ret_all_start$Min_Startdt))
    			  aggregate(df_ret_all_start, by = list(df_ret_all_start$day), FUN = mean)
    		#adding in min player count
    			df_ret = left_join(df_ret_s, data.frame(df_ret %>% group_by(Min_Startdt) %>% summarise(StartingCount = sum(PlayerCount))), by = "Min_Startdt")
    	
    
    			

        	retention = sqldf("select
            											year(a.min_Startdt)||case when week(a.min_Startdt) < 10 then '0' else '' end || week(a.min_Startdt) as JoinedYearMonth,
            											year(a.created_dt)||case when week(a.created_dt) < 10 then '0' else '' end || week(a.created_dt) as PlayedYearMonth,
        													sum(a.playerCount) as PlayerCount,
        													sum(a.playercount) / sum(distinct(a.startingCount)*1.00) as Perc
            										from
        													df_ret a
        												where
        													a.min_Startdt > '2011-11-30'
        												group by 
        													year(a.min_Startdt)||case when week(a.min_Startdt) < 10 then '0' else '' end || week(a.min_Startdt) ,
            											year(a.created_dt)||case when week(a.created_dt) < 10 then '0' else '' end || week(a.created_dt)
        												", method = "name__class"
            										)
	
               retnetion_prct = sqldf("select
               													a.JoinedYearMonth,
               													a.PlayedYearMonth,
            														a.PlayerCount/b.TotalPlayerCount as Prct
            													from
            														retention_2 a,
            														(select
            															b.JoinedYearMonth,
            															sum(b.PlayerCount) as TotalPlayerCount
            														from 
            															retention_2 b
            														where
            															b.PlayedYearMonth = (Select min(c.PlayedYearMonth)
            																									from 
            																											retention_2 c
            																									where c.JoinedYearMonth = b.JoinedYearMonth)
            															group by 
            																b.JoinedYearMonth
            															) b
            														where
            														a.JoinedYearMonth = b.JoinedYearMonth
               												")
   				ggplot(retention_2 , aes(JoinedYearMonth, PlayedYearMonth)) +
															 geom_tile(aes(fill = PlayerCount) ) +
															scale_fill_gradient(low = 'white', high = 'red')
					
					ggplot(retnetion_prct , aes(JoinedYearMonth, PlayedYearMonth)) +
						geom_tile(aes(fill = Prct) ) +
						scale_fill_gradient(low = 'white', high = 'red')
    	
##########
#
# Usre Info
#
##########    
			    #creating the basic player profile
						player_day = unique(d_a %>%select(created_dt,player))
					#adding in tenure	
						player_day = left_join(player_day, player_day %>% group_by(player) %>% summarise(StartDt = min(created_dt)), by = "player")
						player_day$tenure = as.numeric(player_day$created_dt - player_day$StartDt)
					#adding in Active Sales, Sold, Cancled, expired
						player_day = left_join(player_day, d_a%>%filter(state_id == 1)%>%group_by(created_dt, player)%>%summarise(Active = sum(count)), by = c("player","created_dt"))
							player_day[is.na(player_day$Active),"Active"] = 0
						player_day = left_join(player_day, d_a%>%filter(state_id == 2)%>%group_by(created_dt, player)%>%summarise(Sales = sum(count)), by = c("player","created_dt"))
							player_day[is.na(player_day$Sales),"Sales"] = 0
						player_day = left_join(player_day, d_a%>%filter(state_id == 3)%>%group_by(created_dt, player)%>%summarise(Cancled = sum(count)), by = c("player","created_dt"))
							player_day[is.na(player_day$Cancled),"Cancled"] = 0
						player_day = left_join(player_day, d_a%>%filter(state_id == 4)%>%group_by(created_dt, player)%>%summarise(Expired = sum(count)), by = c("player","created_dt"))
							player_day[is.na(player_day$Expired),"Expired"] = 0
					#plotting saes by player tenure
						#plot(player_day %>% group_by(created_dt)%>%summarise(AvgTenure = mean(tenure))
						#the FUCK? I think I found out why  7 and 30 day retnention shifted after a point in time!!!!!!!!!!
						#investigate this more!
							
						
						
						
#############
#
# Rule Mining
#
#############
						
						
						#cprepping the rule set datta
							t = d_a %>% group_by(player, created_dt, class_tsid) %>% summarise(count = sum(count))
							t = d_a %>% select(player, created_dt, class_tsid, count)
							utils::write.csv(t,'./Data/Games/Glitch/Transation_info.csv', row.names = F)
						#reading the rule set data	
							trans = read.transactions('./Data/Games/Glitch/Transation_info.csv', format = 'single', sep = ",", cols = c('player', 'class_tsid'), rm.duplicates = T)
						#size of each item
							t_s = size(trans)
							ggplot(data.frame(count = t_s)) + geom_density(aes(x = count)) + scale_x_log10()
						#getting the relativepercentage of each time
							t_f = itemFrequency(trans)
							t_f_count = t_f/sum(t_f) *sum(t_s)
							t_f_order = sort(t_f_count, decreasing = T)
						#now, time to fget the rules biatches
										#Recall:
										# support = that item/total item set
										# confidnec = when someone buys x, they also buy why CONFIDNCE% of the time (out of all the times they bought x)
							t_apri = apriori(trans, parameter = list(support = 0.1, confidence = 0.5))
						#looking at the rules 'confidnece' test so to speak
								#summary(interestMeasure(t_apri, method = c("coverage", "fishersExactTest"), transactions =  trans))
						#lets actually take a look at the rules now	
							inspect((sort(t_apri[is.redundant(t_apri)], by="ntransactions")))
							
							