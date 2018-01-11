library(dplyr)
library(readr)
library(magrittr)

unemployed <- UNdata_Export_20171218_160340724

unemploy <-
unemployed %>%
  select(`Country or Area`, Mean)

unem <- data.frame(unemploy)

unemcountry <- unem$Country.or.Area[c(885,1294,3007,8309,8673,10785,12238,13514,15230,15521,18322,20498,22625,23472,24998,27394,27771,30945,31859,32908,36896,4155,5788,10327,17054,31233,31665,19740,20171,6672)]
unemvalue <- unem$Mean[c(1:30)]

data.frame(unemcountry,unemvalue)


value1 <- c()
count <- 1
for (i in 1:length(unemployed$`Country or Area`)) 
{
  if(unemployed$`Country or Area`[i] == "Costa Rica" && unemployed$Sex[i] == "Rates, total")
  {
    value1[count] = unemployed$Value[i]
    count = count + 1
  }
}
mean(value1)

gro <- c()

k <- 1
for (i in 1:29){
  

gro[i] <- value1[k]/value1[k+1]
k <- k + 1
}

gro[30] <- 0.9821429
#-----------------------------------------------------------------------------

suicide <- DP_LIVE_17122017085049593

suicide %>%
  select(LOCATION, Value) %>%
  mutate(nSample = 100000)

suicide <- as.data.frame(suicide)

sui <- data.frame(suicide$LOCATION,suicide$Value)

sui



#-----------------------------------------------------------------------------

cor(gro, sui$suicide.Value)
plot(gro, sui$suicide.Value)
abline(lm(sui$suicide.Value~gro),col = "red")
