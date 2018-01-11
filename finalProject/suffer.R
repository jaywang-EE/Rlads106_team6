#定義痛苦指數(通貨膨脹率加上失業率)

library(dplyr)
library(readr)
library(magrittr)
library(rgl)

inflation <- UNdata_Export_20171218_164359168
unemployed <- UNdata_Export_20171218_160340724

un <- unemployed$Mean[c(1:30)]

for (i in 1:30){
  un[i] <- un[i] - 7.080521  
}

unn <- un[-22]
unn <- unn/sd(unn)

for (i in 1:30){
  infvalue[i] <- infvalue[i] - 9.216869
}

inff <- infvalue[-22]
inff <- inff/sd(inff)

#將inff(inflation標準化)和unn(unemployed標準化)透過相關係數加權得到"痛苦指數"
suffervalue <- -0.2701114 * inff + 0.2104976 * unn

cor(suffervalue, sui)
plot(suffervalue, sui)
abline(lm(sui~suffervalue),col = "red")
#相關係數:0.3338466

year <- c(1969:2015
          aa=runif(41,min=1,max=length(colors()))
          col= c(1:30)
          plot3d(year,suidata,sufferindicate,col=col,size=5)