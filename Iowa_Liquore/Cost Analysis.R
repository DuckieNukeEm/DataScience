#loading required packages
  library("plyr")
  library("dplyr")
  library("data.table")
#reading in data
  
  ils <- fread(file.path(getwd(),"Data/Liquore Sales/Reduced_Liquor_Sales.csv"))
  if(names(ils)[1]=="X"){#if there are row names on export, get rid of em
    ils <- ils[,-1]
  }
  
  
  ils$Date <- as.Date(ils$Date)
  
  
#renaming the fields
  names(ils) <- c("Date","Zipcode","County_num",
  								"Category","Vendor","Item_D",
  								"Bottle_ML","BottleCost","BottleRetail",
  								"SoldAmount","TotalSales")
  if(FALSE){
    #change to true if you want to split off the grey goose vodka
    gg <- ils[substr(ils$Item_D,1,4) == "Grey",]
  }
  
#creating a quick price to buy ratio
  ptb <- ils %>%  group_by(Bottle_ML,BottleRetail,Category) %>% summarise(sold = sum(SoldAmount)/n())
  hea
  