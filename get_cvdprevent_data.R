# Load libraries ----
# ~~~~~~~~~~~~~~~~~~~

library(httr)
library(jsonlite)
library(tidyverse)

# Get CVDPrevent data for storyboard ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Based on the slide deck from Chief Pharmacist at NHS Somerset ICB the following indicators will be downloaded

# AF - CVDP001AF, CVDP002AF (1)
# CKD - CVDP001CKD, CVDP002CKD, CVDP005CKD, CVDP006CKD, CVDP007CKD
# CVD & CHOL- CVDP001CVD, CVDP003CHOL, CVDP006CHOL, CVDP007CHOL, CVDP009CHOL, CVDP010CHOL
# HYP - CVDP001HYP, CVDP004HYP, CVDP007HYP
# FH - CVDP002FH
# (1) Missing for Sep-2023 due to data quality

# The indicators will downloaded for 

# England, NHSE Region, Integrated Care Board (ICB), 
# Primary Care Network (PCN) and Practice

# Note: Not all data is available at all 5 levels of organisation above

timePeriodList = c(10)
systemLevelList = c(1, 6, 7, 4, 5)
indicatorIDList = c(1, 8, 12, 11, 7, 13, 9, 14, 4, 19,
                    22, 29, 30, 31, 32, 34, 23)

# AF data
qry <- sprintf('https://api.cvdprevent.nhs.uk/indicator/%d/rawDataJSON?timePeriodID=%d&systemLevelID=%d', indicatorIDList[1], timePeriodList[1], systemLevelList[1])
res <- GET(qry)
df_CVDP001AF

qry <- sprintf('https://api.cvdprevent.nhs.uk/indicator/%d/rawDataJSON?timePeriodID=%d&systemLevelID=%d', indicatorIDList[1], timePeriodList[1], systemLevelList[1])
res <- GET(qry)
df_data <- fromJSON(rawToChar(res$content))$indicatorRawData %>% 
  filter(CategoryAttribute=='Persons' & MetricCategoryTypeName %in% c('Sex','Sex - Age Standardised') & MetricCategoryName == 'Persons') %>%
  select(AreaCode, AreaName, 
         IndicatorCode, IndicatorShortName, IndicatorName,
         MetricCategoryTypeName, MetricCategoryName,
         Numerator, Denominator,
         LowerConfidenceLimit, Value, UpperConfidenceLimit)
qry <- sprintf('https://api.cvdprevent.nhs.uk/indicator/%d/rawDataJSON?timePeriodID=%d&systemLevelID=%d', indicatorIDList[6], timePeriodList[1], systemLevelList[1])
res <- GET(qry)
df_data <- fromJSON(rawToChar(res$content))$indicatorRawData
  


res = GET("http://api.open-notify.org/astros.json")

rawToChar(res$content)

data = fromJSON(rawToChar(res$content))


http://api.open-notify.org/iss-now.json

# /timePeriod
res = GET("https://api.cvdprevent.nhs.uk/timePeriod")
write.csv(fromJSON(rawToChar(res$content))$timePeriodList %>% select(TimePeriodID, TimePeriodName), 'timePeriod.csv')

# /area/systemLevel
res = GET("https://api.cvdprevent.nhs.uk/area/systemLevel?timePeriodID=10")
write.csv(fromJSON(rawToChar(res$content))$systemLevels, 'systemLevel.csv')

# /indicator/list [England - to Sep 2023]
# res = GET("https://api.cvdprevent.nhs.uk/indicator/list?timePeriodID=10&systemLevelID=1")
res = GET("https://api.cvdprevent.nhs.uk/indicator/list?timePeriodID=9&systemLevelID=1")
df_indicatorList <- fromJSON(rawToChar(res$content))$indicatorList %>% 
  select(IndicatorID, IndicatorCode, IndicatorShortName) %>%
  mutate(systemLevel = 'ENG', .before = 1)

# /indicator/list [Region - to Sep 2023]
# res = GET("https://api.cvdprevent.nhs.uk/indicator/list?timePeriodID=10&systemLevelID=6")
res = GET("https://api.cvdprevent.nhs.uk/indicator/list?timePeriodID=9&systemLevelID=6")
df_indicatorList <- df_indicatorList %>% 
  bind_rows(
    fromJSON(rawToChar(res$content))$indicatorList %>% 
      select(IndicatorID, IndicatorCode, IndicatorShortName) %>%
      mutate(systemLevel = 'RGN', .before = 1)
  )

# /indicator/list [ICB - to Sep 2023]
# res = GET("https://api.cvdprevent.nhs.uk/indicator/list?timePeriodID=10&systemLevelID=7")
res = GET("https://api.cvdprevent.nhs.uk/indicator/list?timePeriodID=9&systemLevelID=7")
df_indicatorList <- df_indicatorList %>% 
  bind_rows(
    fromJSON(rawToChar(res$content))$indicatorList %>% 
      select(IndicatorID, IndicatorCode, IndicatorShortName) %>%
      mutate(systemLevel = 'ICB', .before = 1)
  )

# /indicator/list [PCN - to Sep 2023]
# res = GET("https://api.cvdprevent.nhs.uk/indicator/list?timePeriodID=10&systemLevelID=4")
res = GET("https://api.cvdprevent.nhs.uk/indicator/list?timePeriodID=9&systemLevelID=4")
df_indicatorList <- df_indicatorList %>% 
  bind_rows(
    fromJSON(rawToChar(res$content))$indicatorList %>% 
      select(IndicatorID, IndicatorCode, IndicatorShortName) %>%
      mutate(systemLevel = 'PCN', .before = 1)
  )

# /indicator/list [Practice - to Sep 2023]
# res = GET("https://api.cvdprevent.nhs.uk/indicator/list?timePeriodID=10&systemLevelID=5")
res = GET("https://api.cvdprevent.nhs.uk/indicator/list?timePeriodID=9&systemLevelID=5")
df_indicatorList <- df_indicatorList %>% 
  bind_rows(
    fromJSON(rawToChar(res$content))$indicatorList %>% 
      select(IndicatorID, IndicatorCode, IndicatorShortName) %>%
      mutate(systemLevel = 'PRAC', .before = 1)
  )

write.csv(
  df_indicatorList %>% 
    mutate(value = 1) %>% 
    pivot_wider(names_from = 'systemLevel', values_from = 'value') %>%
    replace_na(list(ENG = 0, RGN = 0, ICB = 0, PCN = 0, PRAC = 0)) %>%
    arrange(IndicatorID),
  #  'indicatorList.csv')
  'indicatorListTmp.csv')


# CVDP001AF
# CVDP001CKD
# CVDP001CVD
# CVDP001HYP
# CVDP002AF # Missing for Sep-2023 due to data quality
# CVDP002CKD
# CVDP002FH
# CVDP003CHOL
# CVDP004HYP
# CVDP005CKD
# CVDP006CHOL
# CVDP006CKD
# CVDP007CHOL
# CVDP007CKD
# CVDP007HYP
# CVDP009CHOL
# CVDP010CHOL

timePeriodList = c(10)

systemLevelList = c(1, 6, 7, 4, 5)

indicatorIDList = c(1, 8, 12, 11, 7, 13, 9, 14, 4, 19,
                    22, 29, 30, 31, 32, 34, 23)

res = GET("https://api.cvdprevent.nhs.uk/indicator/1/rawDataJSON?timePeriodID=10&systemLevelID=1")
write.csv(fromJSON(rawToChar(res$content))$indicatorRawData, 'temp.csv')
write.csv()
