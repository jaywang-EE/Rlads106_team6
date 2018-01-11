library(dplyr)
library(readr)
library(magrittr)

inflation <- UNdata_Export_20171218_164359168

inflate <-
  inflation %>%
  select(`Country or Area`,Mean)

inf <- data.frame(inflate)

infcountry <- inf$Country.or.Area[c(359,414,698,2101,2126,2845,3143,3243,3781,3805,4304,5240,5749,6307,6655,7173,7198,7798,8006,8493,9213,1082,1610,2579,4145,7821,7896,4810,5066,1888)]
infmean <- inf$Mean[c(1:30)]
  
data.frame(infcountry,infvalue)

#-----------------------------------------------------------------------------

suicide <- DP_LIVE_17122017085049593

suicide %>%
  select(LOCATION, Value) %>%
  mutate(nSample = 100000)

suicide <- as.data.frame(suicide)

sui <- data.frame(suicide$LOCATION,suicide$Value)

sui

#-----------------------------------------------------------------------------

cor(infmean, sui$suicide.Value)
plot(infmean, sui$suicide.Value)
abline(lm(sui$suicide.Value~infmean),col = "red")
