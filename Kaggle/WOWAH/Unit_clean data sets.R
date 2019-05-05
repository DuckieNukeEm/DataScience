if(TRUE){
require(dplyr)
require(tidyr)
require(data.table)
	
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
}

file_list = c('wowah_elf.csv', 'wowah_orc.csv', 'wowah_tauren.csv', 'wowah_troll.csv','wowah_undead.csv')
Final = data.frame(
			Date = as.Date('1900-01-01'),
			char = as.integer(0),
			guild = as.integer(0),
			level = as.integer(0),
			class = as.character(0),
			race = as.character(0),
			zone = as.character(0),
			Playtime = as.numeric(0)
)
for (a in file_list){
		#Reading in the data
			print(paste(c("reading in", a), collapse = " "))
			elf = read.csv(file.path('d:/data/WOW/',a), stringsAsFactors = F)
			
		#Date Time Adjustment
			print(paste(c("Formating Date", a), collapse = " "))
			elf$Date = as.Date(elf$timestamp, format = '%m/%d/%y %H:%M:%S')
		#	print(paste(c("Formating Time", a), collapse = " "))
		#	elf$Time =  sapply(elf$timestamp, SubstrRight, 8, USE.NAMES = F) 
			elf = elf[,c("Date","char","guild","level","class","race","zone") ]
		#reducing it down
			elf_cd = distinct(elf[c("Date","char","guild","level","class","race","zone")])
		
		#adding in Tenure and Start_dt
			#print(paste(c("getting Start Date", a), collapse = " "))
			#elf_cd = left_join(elf_cd, elf_cd %>% group_by(char) %>% summarise(Start_dt = min(Date)), by = 'char')

		#adding in playtime (in hours)	
			print(paste(c("getting playlength", a), collapse = " "))
			elf_cd = elf %>% 
				group_by(Date, char, guild, level, class, race, zone) %>% 
				summarise(Playtime = (n()/6.0)) %>%
				right_join(elf_cd, by=c("Date","char","guild","level","class","zone","race"))
			print(paste(c("rbind bitches", a), collapse = " "))
			Final = rbind(Final, elf_cd)
			rm(elf_cd)
			rm(elf)
			print(paste(c("rm done", a), collapse = " "))
			print(nrow(Final))
}
Final = Final[-1,c('Date','char','guild','level','class','race','zone','Playtime')]
Final = left_join(Final, Final %>% group_by(char) %>% summarise(Start_dt = min(Date)), by = 'char')
Final = data.frame(Final)
write.table(Final, 'd:/data/WOW/All.csv', sep="," ,row.names = F,quote= F )
