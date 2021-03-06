
### Data preparation###

#This script needs to be sourced from the shiny script to load all relevant packages

#save timestamp
time                     <- Sys.time()
attr(time, "tzone")      <- "UTC"

#read population data
pop          <- read.csv("API_SP.POP.TOTL_DS2_en_csv_v2_821007.csv", skip = 4, stringsAsFactors = FALSE)
pop          <- setNames(pop[,c("Country.Code","X2018")],c("ISO3","pop_2018"))
pop          <- rbind(pop, c("TWN",23588932)) 
pop$pop_2018 <- as.numeric(pop$pop_2018)

#list all expected csv files in Johns Hopkins github repo
datadays <- as.character(format(as.Date(as.Date("01-22-2020", format = "%m-%d-%Y"):Sys.Date(), format = "%m-%d-%Y", origin="01-01-1970"),"%m-%d-%Y"))

#get all current data files
datafiles <- gsub(".csv","",list.files("Data"))

#get last file index
last_file <- match(datafiles[length(datafiles)], datadays)

#get all missing files
missing   <- datadays[!is.element(datadays, datafiles)]

#get days to update: 
#-10 days backwards from last downloaded file to most recent available date
#-10 random days drawn with highe probability on most recent dates
#-all missing dates
to_update <- unique(c(datadays[c(sample((last_file-11),10, prob=1:(last_file-11), replace = FALSE),(last_file-10):length(datadays))], missing))

#create data folder
if(!dir.exists("Data")){
  dir.create("Data")
}

#get date of last general update
last_update <- as.Date(read.table("last_general_update.txt",stringsAsFactors = FALSE)[1,1])
general_update <- FALSE


#check whether general update is necessary
if(last_update < Sys.Date()) {
  general_update <- TRUE #set condition for loop
  write.table(Sys.Date(),"last_general_update.txt", row.names = FALSE, col.names = FALSE) #update last update file
}

#download data for individual days
for (dayfile in 1:length(to_update)) {
  
  
  if(general_update == TRUE){
    
    #download file
    tryCatch(download.file(url=paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",to_update[dayfile],".csv"), destfile = paste0("Data/",to_update[dayfile],".csv")),error = function(e){})
    
  }else if((general_update == FALSE) & (!file.exists(paste0("Data/",datadays[dayfile],".csv")))){
  
    #download file
    tryCatch(download.file(url=paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",to_update[dayfile],".csv"), destfile = paste0("Data/",to_update[dayfile],".csv")),error = function(e){})

  }

}
  

#get all data files including the recently updated
datafiles <- list.files("Data")

#bind data
for (day in 1:length(datafiles)) {

  #check if file contains data
  if(file.size(paste0("Data/",datadays[day],".csv"))>0){
  
  #read downloaded file
  daily       <- as.data.frame(data.table::fread(paste0("Data/",datadays[day],".csv")))
  
  #clean variable names
  names(daily)<- gsub("/|_| |itude","",names(daily))
  
  #create day variable
  daily$day   <- datadays[day]
  
  #bind all daily data
  if(day > 1) {
    
    all <- plyr::rbind.fill(all, daily)
    
  }else{
    
    all <- daily
    
  }
  }
}


#rename country variable
names(all)[2] <- "Country"

#create Date variable
all$day <- as.Date(all$day, format = "%m-%d-%Y")

#clean country names
all[all$Country=="Russia","Country"] <- "Russian Federation"
all[all$Country=="Mainland China","Country"] <- "China"
all[all$Country=="Republic of Ireland ","Country"] <- "Ireland"
all[all$Country=="Taiwan*","Country"] <- "Taiwan"
all[all$Country=="Republic of Korea","Country"] <- "Korea, Republic of"
all[all$Country=="South Korea","Country"] <- "Korea, Republic of"
all[all$Country=="Korea, South","Country"] <- "Korea, Republic of"
all[all$Country=="Vietnam","Country"] <- "Viet Nam"
all[all$Country=="US","Country"] <- "United States"
all[all$Country=="UK","Country"] <- "United Kingdom"
all[all$Country=="Cote d'Ivoire","Country"] <- "United Kingdom"
all[all$Country=="Ivory Coast","Country"] <- "Côte d'Ivoire"
all[all$Country=="Taiwan*","Country"] <- "Taiwan, Province of China"
all[all$Country=="Taiwan","Country"] <- "Taiwan, Province of China"
all[all$Country=="Taipei and environs","Country"] <- "Taiwan, Province of China"
all[all$Country=="Iran","Country"] <- "Iran, Islamic Republic of"
all[all$Country=="Iran (Islamic Republic of)","Country"] <- "Iran, Islamic Republic of"
all[all$Country=="Czech Republic","Country"] <- "Czechia"
all[all$Country=="Macao SAR","Country"] <- "Macao"
all[all$Country=="Macau","Country"] <- "Macao"
all[all$Country=="Hong Kong SAR","Country"] <- "Hong Kong"
all[all$Country=="Congo (Kinshasa)","Country"] <- "Congo, The Democratic Republic of the"
all[all$Country=="Saint Barthelemy","Country"] <- "Saint Barthélemy"
all[all$Country=="Bolivia","Country"] <- "Bolivia, Plurinational State of"
all[all$Country=="Brunei","Country"] <- "Brunei Darussalam"
all[all$Country=="Republic of Moldova","Country"] <- "Moldova, Republic of"
all[all$Country=="Moldova","Country"] <- "Moldova, Republic of"
all[all$Country=="Vatican City","Country"] <- "Holy See (Vatican City State)"
all[all$Country=="Holy See","Country"] <- "Holy See (Vatican City State)"
all[all$Country=="Republic of Ireland","Country"] <- "Ireland"
all[all$Country=="Palestine","Country"] <- "Palestine, State of"
all[all$Country=="occupied Palestinian territory","Country"] <- "Palestine, State of"
all[all$Country=="West Bank and Gaza","Country"] <- "Palestine, State of"
all[all$Country=="St. Martin","Country"] <- "Saint Martin (French part)"
all[all$Country=="Saint Martin","Country"] <- "Saint Martin (French part)"
all[all$Country=="Reunion","Country"] <- "Réunion"
all[all$Country=="North Ireland","Country"] <- "United Kingdom"
all[all$Country=="Channel Islands","Country"] <- "United Kingdom"
all[all$Country=="Cruise Ship","Country"] <- "Others"
all[all$Country=="Venezuela","Country"] <- "Venezuela, Bolivarian Republic of"
all[all$Country=="Curacao","Country"] <- "Curaçao"
all[all$Country=="Congo (Brazzaville)","Country"] <- "Congo"
all[all$Country=="Republic of the Congo","Country"] <- "Congo"
all[all$Country=="Bahamas, The","Country"] <- "Bahamas"
all[all$Country=="The Bahamas","Country"] <- "Bahamas"
all[all$Country=="Gambia, The","Country"] <- "Gambia"
all[all$Country=="The Gambia","Country"] <- "Gambia"
all[all$Country=="Tanzania","Country"] <- "Tanzania, United Republic of"
all[all$Country=="Syria","Country"] <- "Syrian Arab Republic"
all[all$Country=="Cape Verde","Country"] <- "Cabo Verde"
all[all$Country=="East Timor","Country"] <- "Timor-Leste"
all[all$Country=="Laos","Country"] <- "Lao People's Democratic Republic"
all[all$Country=="Burma","Country"] <- "Myanmar"

#get ISO-Codes from ISOCodes package
ISO3        <- ISOcodes::ISO_3166_1[,c("Alpha_3", "Alpha_2","Name")]
names(ISO3) <- c("ISO3","ISO2","Country")
ISO3        <- rbind(ISO3, c("KOS","XK","Kosovo")) #XK is name in flag repo, KOS result of rworldmap::rwmGetISO3("Kosovo")

#add flag colors
flagcolors <- readRDS("flagcolors.rds")

ISO3 <- merge(ISO3, flagcolors, by="ISO2")

#compute basic counts of cases, deaths and recovered per country
countries   <- setNames(aggregate.data.frame(list(all$Confirmed, all$Deaths, all$Recovered), by=list(all$'Country', all$day), FUN = sum),c("Country","Day","Cases","Deaths","Recovered"))

#add ISO-codes for mapping and flags
countries   <- merge(countries, ISO3, by= "Country", all=TRUE)

#clear country names to plain english names from countrycode package
countries$Country  <- ifelse(!is.na(countrycode::countrycode(countries$ISO3, "iso3c", "country.name")),countrycode::countrycode(countries$ISO3, "iso3c", "country.name"),countries$Country)

#fill up countries with no cases to complete dataset
countries <- tidyr::complete(countries, nesting(Country, ISO3, ISO2, color_1, color_2, color_3), Day )
countries <- countries[!is.na(countries$Day),]

#add population information from workdbank
countries        <- merge(countries, pop, by= "ISO3", all.x=TRUE)

#fill up NAs of Basic Variables if there are cases
countries[is.na(countries$Deaths)    & !is.na(countries$Cases),"Deaths"]    <- 0
countries[is.na(countries$Recovered) & !is.na(countries$Cases),"Recovered"] <- 0

#compute active cases
countries$Active <- countries$Cases - countries$Deaths - countries$Recovered

#compute ratios
countries$D2C    <- ifelse(countries$Cases>=100,countries$Deaths/countries$Cases * 100,NA)
countries$A2C    <- ifelse(countries$Cases>=100,countries$Active/countries$Cases * 100,NA)
countries$D2O    <- ifelse(countries$Cases>=100,countries$Deaths/(countries$Recovered + countries$Deaths) * 100,NA)

#compute per capita ratios
countries$CpC    <- countries$Cases/(countries$pop_2018/1000000)
countries$DpC    <- countries$Deaths/(countries$pop_2018/1000000)
countries$ApC    <- countries$Active/(countries$pop_2018/1000000)
countries$RpC    <- countries$Recovered/(countries$pop_2018/1000000)

#compute growth rates
countries <- countries[order(countries$Day),]

#list countries
countrylist <- unique(countries$Country)

#intitiate variables
countries$Delta_Cases  <- NA
countries$Delta_Deaths <- NA
countries$Delta_Active <- NA
countries$CdG          <- NA
countries$DdG          <- NA
countries$AdG          <- NA

#iterate over countries
for(country in 1:length(countrylist)){
  
  #select country to calculate growth rates for
  focus <- countries[which(countries$Country==countrylist[country]),]
  
  #calculate daily change
  focus[,"Delta_Cases"]  <- c(NA,diff(focus[,"Cases"]))
  focus[,"Delta_Deaths"] <- c(NA,diff(focus[,"Deaths"]))
  focus[,"Delta_Active"] <- c(NA,diff(focus[,"Active"]))
  
  #calculate growth rates
  focus[,"CdG"] <- ifelse(focus$Cases>=100, as.vector(focus[,"Delta_Cases"]/dplyr::lag(focus[,"Cases"]))*100,NA)
  focus[,"DdG"] <- ifelse(focus$Cases>=100, as.vector(focus[,"Delta_Deaths"]/dplyr::lag(focus[,"Deaths"]))*100,NA)
  focus[,"AdG"] <- ifelse(focus$Cases>=100, as.vector(focus[,"Delta_Active"]/dplyr::lag(focus[,"Active"]))*100,NA)
  
  #create empty dataset
  if(country==1){
    all_countries <- focus[0,]
  }
  
  #collect
  all_countries <- rbind(all_countries, focus)
  
}

#return to dataframe
countries <- as.data.frame(all_countries)

#list variables for which colorcodes should be computed
display_vars <- c("Cases", "Active", "Deaths", "Recovered", "D2C", "A2C", "D2O", "CpC","DpC","ApC","RpC","Delta_Cases", "Delta_Deaths", "Delta_Active","CdG","DdG","AdG")

#define relative change vars
rc_vars      <- c("CdG","DdG","AdG")

#define variables that don't go below zero
min_vars <- c("Delta_Cases","Delta_Deaths","DdG","CdG")

#iterate over variables to colorcode and create legend
for(i in 1:length(display_vars)){
  
  #duplicate dataset
  coloring <- countries
  
  #replace infinite numbers with non infinite max or min for color coding
  coloring[is.infinite(coloring[,display_vars[i]]) & coloring[,display_vars[i]] >0 ,display_vars[i]] <-  max(coloring[!is.infinite(coloring[,display_vars[i]]),display_vars[i]],na.rm = TRUE)
  coloring[is.infinite(coloring[,display_vars[i]]) & coloring[,display_vars[i]] <0 ,display_vars[i]] <-  min(coloring[!is.infinite(coloring[,display_vars[i]]),display_vars[i]],na.rm = TRUE)
  
  #if value can't logically be lower than zero, set zero as minimum for coloring
  if(is.element(display_vars[i],min_vars)){
  
  #for change of cases and deaths, make 0 the min
  coloring[which(coloring[,display_vars[i]] < 0) ,display_vars[i]] <- 0
  
  }
  
  #set coloring for count variables
  if(!is.element(display_vars[i], rc_vars)){
  
  #create color scale basic variable by log on normalized data > 0
  colordomain          <- log(BBmisc::normalize(coloring[,display_vars[i]], method = "range",margin =2,  range = c(1,100)))
  
  #create coloring scheme
  pal                  <- leaflet::colorBin(palette = "RdYlGn", bins = 10, domain = colordomain, reverse = TRUE)
  
  }
  
  #set coloring for relative change variables
  if(is.element(display_vars[i], rc_vars)){
    
    if(!is.element(display_vars[i], min_vars)){
    #create color scale basic variable by individually assigned breaks
      colordomain          <- cut(coloring[,display_vars[i]],c(min(coloring[,display_vars[i]],na.rm = TRUE),-5,-1,0,1,5,10,20,50,max(coloring[,display_vars[i]], na.rm=TRUE)),include.lowest = TRUE, right= TRUE, labels = FALSE)
    }else{
      colordomain          <- cut(coloring[,display_vars[i]],c(0,1,2,5,10,20,50,max(coloring[,display_vars[i]], na.rm=TRUE)),include.lowest = TRUE, right= TRUE, labels = FALSE)
    }
    
    #create coloring scheme
    pal                  <- leaflet::colorBin(palette = "RdYlGn",bins = 10, domain = colordomain, reverse = TRUE)
    
  }
  
  
  
  
  #use color code on variable
  countries[,paste0(display_vars[i],"_color")] <- pal(colordomain)

  #create legends based on color
  legend_raw <- countries[,c(display_vars[i],paste0(display_vars[i],"_color"))]
  
  #get upper and lower borders
  legend <- setNames(cbind(aggregate(legend_raw[,1], by=list(legend_raw[,2]), FUN = min),aggregate(legend_raw[,1], by=list(legend_raw[,2]), FUN = max)[,2]),c("color","min","max"))
  
  #create label
  legend$label <- ifelse(legend$min==legend$max,format(round(legend$min,2), big.mark =","),paste0(format(round(legend$min,2), big.mark =",")," to ", format(round(legend$max,2), big.mark =",")))
  
  #order and remove redundant columns
  legend <- legend[order(legend$min),c("color","label")]
  
  #code for na
  legend[legend$color=="#808080","label"] <- ("no value")
  
  #create legend by name
  assign(paste0("legend_",display_vars[i]), legend)
  
}
