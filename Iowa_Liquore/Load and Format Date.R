#Load Package
	library(dplyr)
	library(tidyr)
#Loading the Data
  ils = read.csv(file.choose(), stringsAsFactors=FALSE)
#formatting the data
  ils = ils %>%
        mutate(State.Bottle.Cost = as.numeric(gsub("\\$","",State.Bottle.Cost)),
               State.Bottle.Retail = as.numeric(gsub("\\$","",State.Bottle.Retail)),
               Sale..Dollars. = as.numeric(gsub("\\$","",Sale..Dollars.)),
               Date = as.Date(Date, format = '%m/%d/%Y')
               )
  ils$Item.Number = ils$Item.Number * 1000 + ils$Pack #building pack size into the item number
#Creating Refrence Files
	 #Store Infromation
	  	Store = ils %>% select(Store.Number, Store.Name, Address, City, Zip.Code, County, Store.Location) %>% 
		   								mutate(Address = tolower(Address),
		  											 Store.Name = tolower(Store.Name),
		  											 City = tolower(City),
		  											 County = tolower(County),
		  											 County = gsub("\\'","",County), #fixing the O'brian and Obrian issues
		  											 County = ifelse(County == 'buena vist', 'buena vista',  #Fixing the buena vist and buena vista issue
		  											 									ifelse(County == 'pottawatta' , 'pottawattamie', County)), #ditto
		  											 Store.Location = gsub(".*\\((.*)\\).*", "\\1", Store.Location),  #getting Lat/Long 
		  										   Store.Location = ifelse(substr(Store.Location, nchar(Store.Location), nchar(Store.Location)) == "\n","",Store.Location), #removing blankes	 
		  											 Lat = substr(Store.Location,1,regexpr(",",Store.Location)-1),
		  											 Long = substr(Store.Location,regexpr(",",Store.Location)+1,nchar(Store.Location)),
		  											 RowNumber = row_number()
		   								) %>%
	  	                select(-(Store.Location)) %>%
		  								filter(County != '') %>% 
		  								distinct()
		  
		  
		  Store = inner_join(Store,
		  									 Store %>% select(Store.Name, Store.Number,RowNumber) %>%
		  									 				group_by(Store.Number) %>%
		  									 				summarise(Store.Name = max(Store.Name),
		  									 				          RowNumber = max(RowNumber)),
		  									 by = c("Store.Name", "Store.Number","RowNumber")) %>%
		                    select(-(RowNumber))
		  
  #Vendor List	
		  Vendor = ils %>% select(Vendor.Number, Vendor.Name) %>% 
		  								distinct() 
		  Vendor = inner_join(Vendor,
		  										Vendor %>% group_by(Vendor.Number) %>%
		  											summarise(Vendor.Name = max(Vendor.Name)),
		  										by = c("Vendor.Name","Vendor.Number"))
	#Category
	  	Category <- ils %>% select(Category, Category.Name) %>%
	  											distinct()
	  	
	  	Category = inner_join(Category,
	  												Category %>% group_by(Category) %>%
	  													summarise(Category.Name = max(Category.Name)),
	  												by = c("Category", "Category.Name"))
  #Item
		  Item <- ils %>% select(Item.Number, Item.Description) %>%
		  								distinct()
		  
		  Item = inner_join(Item,
		  									Item %>% group_by(Item.Number) %>%
		  										summarise(Item.Description = max(Item.Description)),
		  									by = c("Item.Number","Item.Description"))
	  
#Reducing the data
  ils = ils %>% rename(Bottle.ml = Bottle.Volume..ml.,
  											Total.Sales = Sale..Dollars.) %>% 
  							 mutate(State.Revenue = State.Bottle.Retail - State.Bottle.Cost) %>%
									select(Date,
									Store.Number,
									Category,
									Vendor.Number,
									Item.Number,
									Bottle.ml,
									Bottles.Sold,
									State.Revenue,
									State.Bottle.Cost,
									Total.Sales
									) 
						
#Saving the Data
  #save.dir <- file.path()
  save.dir = file.path(paste(c(getwd(),"/R/Data/ILS"), collapse = ""))
  write.csv(ils, file.path(save.dir,"Reduced_Liquor_Sales.csv"), row.names = F)
  write.csv(Store, file.path(save.dir,"Liquor_Store.csv"), row.names = F)
  write.csv(Vendor, file.path(save.dir,"Liquor_Vendor.csv"), row.names = F)
  write.csv(Category, file.path(save.dir,"Liquor_Category.csv"), row.names = F)
  write.csv(Item, file.path(save.dir,"Liquor_Item.csv"), row.names = F)
  rm(ils, Store,Vendor,Category,Item)