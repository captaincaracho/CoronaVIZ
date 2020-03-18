
library(data.table)
library(plyr)
library(dplyr)
library(ISOcodes)
library(countrycode)
library(tidyr)
library(leaflet)
library(BBmisc)

#download live data
download.file(url="https://github.com/CSSEGISandData/COVID-19/archive/master.zip", destfile = "COVID-19-master.zip")

#unzip downloaded data
unzip("COVID-19-master.zip")

#read poluation data
pop <- read.csv("API_SP.POP.TOTL_DS2_en_csv_v2_821007.csv", skip = 4)
pop <- setNames(pop[,c("Country.Code","X2018")],c("ISO3","pop_2018"))


#list files with datat
files <- list.files("COVID-19-master/csse_covid_19_data/csse_covid_19_daily_reports", full.names = TRUE)[!grepl("README",list.files("COVID-19-master/csse_covid_19_data/csse_covid_19_daily_reports", full.names = TRUE))]


#read data
for (day in 1:length(files)) {
  
  daily     <- as.data.frame(data.table::fread(files[day]))
  daily$day <- gsub("COVID-19-master/csse_covid_19_data/csse_covid_19_daily_reports/|.csv","",files[day])
  
  if(day > 1) {
    
    all <- plyr::rbind.fill(all, daily)
    
  }else{
    
    all <- daily
    
  }
}


#Names

names(all)[2] <- "Country"

#Create Date
all$day <- as.Date(all$day, format = "%m-%d-%Y")

#Clean Country-Names
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
all[all$Country=="St. Martin","Country"] <- "Saint Martin (French part)"
all[all$Country=="Saint Martin","Country"] <- "Saint Martin (French part)"
all[all$Country=="Reunion","Country"] <- "Réunion"
all[all$Country=="North Ireland","Country"] <- "United Kingdom"
all[all$Country=="Channel Islands","Country"] <- "United Kingdom"
all[all$Country=="Cruise Ship","Country"] <- "Others"
all[all$Country=="Venezuela","Country"] <- "Venezuela, Bolivarian Republic of"
all[all$Country=="Curacao","Country"] <- "Curaçao"
all[all$Country=="Congo (Brazzaville)","Country"] <- "Congo"


#Join ISO-Codes
ISO3        <- ISOcodes::ISO_3166_1[,c("Alpha_3", "Alpha_2","Name")]
names(ISO3) <- c("ISO3","ISO2","Country")
ISO3        <- rbind(ISO3, c("KOS","XK","Kosovo")) #XK is name in flag repo, KOS result of rworldmap::rwmGetISO3("Kosovo")

#compute basic per country counts
countries        <- setNames(aggregate.data.frame(list(all$Confirmed, all$Deaths, all$Recovered), by=list(all$'Country', all$day), FUN = sum),c("Country","Day","Cases","Deaths","Recovered"))

#add iso codes for mapping
countries        <- merge(countries, ISO3, by= "Country", all=TRUE)

#clear country names
countries$Country  <- ifelse(!is.na(countrycode::countrycode(countries$ISO3, "iso3c", "country.name")),countrycode::countrycode(countries$ISO3, "iso3c", "country.name"),countries$Country)

#fill up countries with no cases
countries <- tidyr::complete(countries, nesting(Country, ISO3, ISO2), Day )
countries <- countries[!is.na(countries$Day),]

#add population
countries        <- merge(countries, pop, by= "ISO3", all.x=TRUE)

#fill up NAs of Basic Variables
countries[is.na(countries$Deaths)    & !is.na(countries$Cases),"Deaths"] <- 0
countries[is.na(countries$Recovered) & !is.na(countries$Cases),"Recovered"] <- 0

#compute active cases
countries$Active <- countries$Cases - countries$Deaths - countries$Recovered

#compute ratios
countries$D2C    <- ifelse(countries$Cases>=100,countries$Deaths/countries$Cases * 100,NA)
countries$A2C    <- ifelse(countries$Cases>=100,countries$Active/countries$Cases * 100,NA)
countries$D2O    <- ifelse(countries$Cases>=100,countries$Deaths/(countries$Recovered + countries$Deaths) * 100,NA)

#compute per capita ratios
countries$CpC    <- ifelse(countries$Cases>=100,countries$Cases/(countries$pop_2018/1000000),NA)
countries$DpC    <- ifelse(countries$Cases>=100,countries$Deaths/(countries$pop_2018/1000000),NA)
countries$ApC    <- ifelse(countries$Cases>=100,countries$Active/(countries$pop_2018/1000000),NA)
countries$RpC    <- ifelse(countries$Cases>=100,countries$Recovered/(countries$pop_2018/1000000),NA)

#compute growth rates
countries <- countries[order(countries$Day),]

countrylist <- unique(countries$Country)

countries$CdG <- NA
countries$DdG <- NA
countries$AdG <- NA


for(country in 1:length(countrylist)){
  
  #select country to calculate growth rates for
  focus <- countries[which(countries$Country==countrylist[country]),]

  #calculate growth rates
  focus[,"CdG"] <- ifelse(focus$Cases>=100, as.vector(c(NA,diff(focus[,"Cases"]))/dplyr::lag(focus[,"Cases"]))*100,NA)
  focus[,"DdG"] <- ifelse(focus$Cases>=100, as.vector(c(NA,diff(focus[,"Deaths"]))/dplyr::lag(focus[,"Deaths"]))*100,NA)
  focus[,"AdG"] <- ifelse(focus$Cases>=100, as.vector(c(NA,diff(focus[,"Active"]))/dplyr::lag(focus[,"Active"]))*100,NA)
  
  #create empty dataset
  if(country==1){
    all_countries <- focus[0,]
  }
  
  #collect
  all_countries <- rbind(all_countries, focus)
  
}
                                                                               
                                                                               
#return to dataframe
countries <- as.data.frame(all_countries)

#produce colored bins

display_vars <- c("Cases", "Active", "Deaths", "Recovered", "D2C", "A2C", "D2O", "CpC","DpC","ApC","RpC","CdG","DdG","AdG")

for(i in 1:length(display_vars)){

  #use log to evenly distribute colors, plus minimum to ensure all cases are positive, plus one too be sure they are not equal to 0
  
  coloring <- countries
  
  coloring[is.infinite(coloring[,display_vars[i]]),display_vars[i]] <-  max(coloring[!is.infinite(coloring[,display_vars[i]]),display_vars[i]],na.rm = TRUE)
  
  colordomain          <- log(BBmisc::normalize(coloring[,display_vars[i]], method = "range",margin =2,  range = c(1,100)))
  
  pal                  <- leaflet::colorBin(palette = "RdYlGn", bins = 10, domain = colordomain, reverse = TRUE)
  
  countries[,paste0(display_vars[i],"_color")] <- pal(colordomain)

}

