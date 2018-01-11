library(dplyr)
library(readr)
library(magrittr)

hdi <- HDI1990


hdi %>%
  select(MeanGrowth)

HDIValue <- hdi$MeanGrowth[c(1:30)]
#-----------------------------------------------------------------------------

suicide <- DP_LIVE_17122017085049593
avesuicide <- multidata

suicide %>%
  select(avg) %>%
  mutate(nSample = 100000)

avesuicide <- as.data.frame(suicide)
avesuicide$avg


#-----------------------------------------------------------------------------

cor(HDIValue, avesuicideR$avg)
plot(HDIValue, avesuicide$avg)
abline(lm(avesuicide$avg~HDIValue),col = "red")
