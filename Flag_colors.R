
library(RImagePalette)
library(png)
library(ISOcodes)
library(imager)
library(data.table)

#function to extrect colors
getHexPrimaries <- function(img, pcnt.threshold = 0.01){
  
  #convert cimg to workable format
  channel.labels <- c('R','G','B','A')[1:dim(img)[4]]
  img <- as.data.table(as.data.frame(img))
  img[,channel := factor(cc ,labels=channel.labels)]
  img <- dcast(img, x+y ~ channel, value.var = "value")
  
  #sort by unique rgb combinations and identify the primary colours
  colours.sorted <- img[, .N, by=list(R,G,B)][order(-N)]
  colours.sorted[ , primary := N/sum(N) > pcnt.threshold]
  
  #convert to hex
  hex.primaries <- 
    apply(colours.sorted[primary==TRUE], 1, function(row){
      hex <- rgb(row[1], row[2], row[3], maxColorValue=1)
      hex
    })
  
  hex.primaries
}



#load ISO3 dataset from prepare.R

#Download flags
for(i in 1:nrow(ISO3)){
 tryCatch(download.file(paste0("https://raw.githubusercontent.com/captaincaracho/world_countries/master/flags/32x32/",tolower(ISO3[i,"ISO2"]),".png"),paste0("Flags/",tolower(ISO3[i,"ISO2"]),".png"), mode = "wb"),error = function(e){})
}

for(i in 1:nrow(ISO3)){
  print(ISO3$Country[i])
  tryCatch(ISO3[i,4:6] <- getHexPrimaries(load.image(paste0("Flags/",tolower(ISO3[i,"ISO2"]),".png")), pcnt.threshold = 0.01)[1:3],error = function(e){})
}


ISO3 <- flagcolors

ISO3 <- ISO3[,c(2,4:6)]
names(ISO3) <- c("ISO2","color_1","color_2","color_3")

#Eliminate white as first color
ISO3$color_4 <- ISO3$color_1

ISO3$color_switch <- ifelse(ISO3$color_1=="#FFFFFF", TRUE, FALSE)
ISO3$color_1 <- ifelse(ISO3$color_switch== TRUE, ISO3$color_2, ISO3$color_1)
ISO3$color_2 <- ifelse(ISO3$color_switch== TRUE, ISO3$color_4, ISO3$color_2)


ISO3 <- ISO3[,c(1:4)]



saveRDS(ISO3,"flagcolors.rds")
