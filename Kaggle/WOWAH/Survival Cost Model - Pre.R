#########
#
# Loading packages and basic funtioncs
#
#########

"Introduction"

"Day one: having completed the survival analysis using R  I would now like to apply it to the WOW data set,
to answer tha basic question

1) Is there a diffrence in the rat eof survival between class/Race?
2) Does being enrolled in a guild change survival rate?
3) Can we predict when someone will leave based on the characteriscts of the race?
"

'Also had an idea, we can calc the LTV of each player by the following
1) the first month was of playing was the cost of the game 20$
1)if purchased gameplay 1mo at a time, then it was 15$/m
2) if purchased 3mo at a time, then it was 14$/m,
3) if purchased 6mo at a time, then it was 13$/m'

'Therefor, the first month of game play was 20$, then each consecutive month a variable price,
depending on how long they paid for. We can assume that in month 2, a play paid for 1mo only 15$
in month three, they paid for 3 month in advanced, REGARDLESS IF THEY STAYED ON FOR THE THREE MONTHS,
and in month in month 6, they paid for the 6month in advanced, agian, regarless if they stayed on or not.
so it would look like this

Mo|$ Amt
01|20
02|15
03|14x3 = 45$
04|0
05|0
06|13*6 = 78$
07|0
08|0
09|0
10|0
11|0
12|13*6 = 78$

so What? Well, dipshit integrate from 0 to t the survival function TIME the cost function (above)
and that will get you your answer for LTV'

'Furthermore, becuase this was from ONE server, you might be able to figure out how much it cost on a monthly
base to run that surver, therefor, you can find the cost of each player!'

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
  require(plotly)

#########
#
# Loading the Data & formating the data
#
#########

  #loading Data
    if(.Platform$OS == 'unix'){ #linux?
    df = data.frame(fread('/media/asmodi/0123-4567/Non OneDrive/Data/WOW/All.csv', stringsAsFactors = F))
    } else { #or non linux (AKA Windows)
    df = data.frame(fread('d:/Non OneDrive/Data/WOW/All.csv', stringsAsFactors = F))
    }
  #ofrmating the Dates	
    df$Start_dt = as.Date(df$Start_dt)
    df$Date = as.Date(df$Date)
  #adding cohort and yearmonth
    df$cohort = format(df$Start_dt, '%Y%m')
    df$Month = floor(as.integer(df$Date - df$Start_dt)/30)
    df$Week = floor(as.integer(df$Date - df$Start_dt)/7)
    
  # some weird fucking class in this data, removing that shit
    df = df%>% filter( class %in% c("Hunter","Shaman","Warlock","Warrior",
                                    "Rogue","Druid","Mage","Priest","Paladin", "Death Knight") )
  #adding tenure
    df = df %>% mutate(tenure = as.integer(Date - Start_dt))
  
  #creating a Index to try dating thing on
    df %>% arrange(char,Date) %>% 
           group_by(char, Date) %>% 
           summarise(race = max(race),
                     guild = max(ifelse(guild > 0, 1, guild)),
                     zone = max(zone))
  #fixing df so that instead of having multiple records per day, what they ended the day at is what they
  #were for that day
    df = df %>% arrange(char, Date, race, level, guild, zone) %>%
               mutate(Index = 1) %>%
               group_by(char) %>%
               mutate(Index = cumsum(Index)) 
              
    df = inner_join(df %>% select(Date, 
                                  char, 
                                  Playtime, 
                                  Start_dt, 
                                  cohort, 
                                  Month, 
                                  Week, 
                                  tenure),
                    df %>% group_by(Date, char) %>%
                          
          
  
  #creating an index to for char date
    df_char_date = df %>% select(char, Date) %>%  
      distinct()	%>%
      arrange(char,Date) %>% 
      mutate(Date_Index = 1) %>% 
      data.table()
    df_char_date[,Date_Index:= cumsum(Date_Index),by='char']
    
  ###WHOAWHOAWHAOWHAWHOAWHOA
    "This CUmsum is brilliant, think about it,
      ever row should be 'unique' in some way, diffrent Date, advenced a level, changed guildes, blah blah blah
      So all we have to do is have a basic count index of 1 on each row, (IE, each row has a colum that one has the number 1 in it)
    THEN we can easily modify the data base to we want to cut it, (IE, by Date and Char, By Char and Guild, etc)
      once the modification to the new data is done, we still have a count of 1 one each new row, then we cumsum
      it by the respective groupings (Date and Char, Char and Guild) and it will give our our fucking INDEX
      #BRILLIANT_MOTHER_FUCKER!!!!!!!!"
  
  df_char_date = left_join(df_char_date, df_char_date %>% 
                          mutate(Date_Index = Date_Index + 1) %>%
                          rename(PrevDate = Date), by = c('char','Date_Index'))
  
  