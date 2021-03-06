library(dplyr)
library(readr)
library(magrittr)

unemployed <- UNdata_Export_20171218_160340724

unemploy <-
  unemployed %>%
  select(`Country or Area`, Value)

unem <- data.frame(unemploy)

unemcountry <- unem$Country.or.Area[c(885,1294,3007,8309,8673,10785,12238,13514,15230,15521,18322,20498,22625,23472,24998,27394,27771,30945,31859,32908,36896,4155,5788,10327,17054,31233,31665,19740,20171,6672)]
unemvalue <- unem$Value[c(885,1294,3007,8309,8673,10785,12238,13514,15230,15521,18322,20498,22625,23472,24998,27394,27771,30945,31859,32908,36896,4155,5788,10327,17054,31233,31665,19740,20171,6672)]
undata <- rnorm(500,7.080521,3.738648)


data.frame(unemcountry,unemvalue)


un <- unemployed$Mean[c(1:30)]

unnn <- sd(un)

for (i in 1:30){
un[i] <- un[i] - 7.080521  
}

unn <- un[-22]
unn <- unn/unnn
#-----------------------------------------------------------------------------

inflation <- UNdata_Export_20171218_164359168

inflate <-
  inflation %>%
  select(`Country or Area`,Central)

inf <- data.frame(inflate)

infdata <- rnorm(500,9.216869,42.71984)

infcountry <- inf$Country.or.Area[c(359,414,698,2101,2126,2845,3143,3243,3781,3805,4304,5240,5749,6307,6655,7173,7198,7798,8006,8493,9213,1082,1610,2579,4145,7821,7896,4810,5066,1888)]
infvalue <- inf$Central[c(1:30)]
  
data.frame(infcountry,infvalue)

infff <- sd(infvalue) 

for (i in 1:30){if (i != 22){
infvalue[i] <- infvalue[i] - 9.216869

}
}


inff <- infvalue[-22]
inff <- inff/sd(inff)

suffervalue <- -0.3249177 * inff + -0.09592613 * unn
sufferindicate <- -0.3249177 * infdata + -0.09592613 * undata

#-----------------------------------------------------------------------------

suicide <- DP_LIVE_17122017085049593

suicide %>%
  select(LOCATION, Value) %>%
  mutate(nSample = 100000)

suicide <- as.data.frame(suicide)

sui <- data.frame(suicide$LOCATION,suicide$Value)
suidata <- rnorm(500,12.24828,5.494648)

sui <- suicide$Value[-22]
#-----------------------------------------------------------------------------

cor(sui,suffervalue)
plot(sui,suffervalue)
abline(lm(suffervalue~sui),col = "red")

suffer <- -0.3249177 * inflation$Value +  -0.09592613 * unemployed$Value
year <- c(1969:2015)

library(rgl)
aa=runif(41,min=1,max=length(colors()))
col= c(1:30)
plot3d(year,suidata,sufferindicate,col=col,size=5)
