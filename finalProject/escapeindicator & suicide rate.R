library(dplyr)
library(readr)
library(magrittr)

chocolatedata <- chocolate

choco <-
  chocolatedata %>%
  select(Value)

cho <- data.frame(choco)
chocovalue <- cho$Value[c(4,5,6,7,10,12,13,14,15,16,17)]
sdd <- sd(chocovalue)
chocosl <- rnorm(500,7.219412,1.435143)

for (i in 1:11){
  chocovalue[i] <- chocovalue[i] - 7.219412
}

coco <- chocovalue/sdd

## 
cof <- coffee

coff <-
  cof %>%
  select(Location,Average,Mean)

coffeedata <- data.frame(coff)

cofcountry <- coff$Location
cofaverage <- coff$Average

cofsl <- rnorm(500,6.3,2.716406)
std <- sd(cofaverage)

coff <- coff$Mean[c(1:11)]
coff <- (coff - 6.3)/std


##
suicide <- DP_LIVE_17122017085049593

suicide %>%
  select(LOCATION, Value) %>%
  mutate(nSample = 100000)

suicide <- as.data.frame(suicide)

sui <- data.frame(suicide$LOCATION,suicide$Value)

sui <- suicide$Value[c(4,5,6,7,10,12,13,14,15,16,17)]
suisu <- (sui - 12.24828)/5.494648

suirandom <- rnorm(500,12.24828,5.494648)
##
alcohol <- DP_LIVE_03012018140047777

alcho <- rep(NA,11)
al <- rep(NA,54)

country <- c("CZE","DNK","FIN","DEU","ISL",
             "LUX","MEX","NLD","NOR","POL","PRT")

for (k in 1:11){
  val = 0
  for (i in 1:length(alcohol$LOCATION)){
    if (alcohol$LOCATION[i] == country[k]){
      val = val + alcohol$Value[i]
    }
    
  }
  
  alcho[k] <- val/54
}

escapeindicate <- -0.0460704 * coff + 0.2558978 * coco + 0.06208627 * alcho/1.22


cor(escapeindicate,sui,method = "pearson")
plot(sui,escapeindicate)
abline(lm(escapeindicate~sui),col = "red")


library(rgl)
aa=runif(41,min=1,max=length(colors()))
col= c(1:11)
plot3d(year,suirandom,escapeindicate,col=col,size=5)