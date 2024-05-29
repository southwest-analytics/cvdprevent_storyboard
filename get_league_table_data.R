# 0. Load libraries and define functions ----
# ═══════════════════════════════════════════

library(httr)
library(jsonlite)
library(tidyverse)

# 1. Get CVDPrevent data ----
# ═══════════════════════════

# Get latest time period
qry <- 'https://api.cvdprevent.nhs.uk/timePeriod'
res <- GET(qry)
latest_period_id <- max(fromJSON(rawToChar(res$content))$timePeriodList$TimePeriodID)

# Get the system levels
qry <- sprintf('https://api.cvdprevent.nhs.uk/area/systemLevel?timePeriodID=%d', latest_period_id)
res <- GET(qry)
df_system_levels <- fromJSON(rawToChar(res$content))$systemLevels

# Get the indicators available for ICB ...
qry <- sprintf('https://api.cvdprevent.nhs.uk/indicator/list?timePeriodID=%d&systemLevelID=%d', latest_period_id, df_system_levels$SystemLevelID[df_system_levels$SystemLevelName=="ICB"])
res <- GET(qry)
df_icb_indicators <- fromJSON(rawToChar(res$content))$indicatorList

# ... and PCN
qry <- sprintf('https://api.cvdprevent.nhs.uk/indicator/list?timePeriodID=%d&systemLevelID=%d', latest_period_id, df_system_levels$SystemLevelID[df_system_levels$SystemLevelName=="PCN"])
res <- GET(qry)
df_pcn_indicators <- fromJSON(rawToChar(res$content))$indicatorList

# * 1.1 - Get the denominator data CVDP003CHOL ----
# ─────────────────────────────────────────────────

# ICB Level
qry <- sprintf('https://api.cvdprevent.nhs.uk//indicator/%d/rawDataJSON?timePeriodID=%d&systemLevelID=%d', 
               df_icb_indicators$IndicatorID[df_icb_indicators$IndicatorCode=='CVDP003CHOL'],
               latest_period_id, 
               df_system_levels$SystemLevelID[df_system_levels$SystemLevelName=="ICB"])
res <- GET(qry)
df_icb_cvdp003chol <- fromJSON(rawToChar(res$content))$indicatorRawData

# ICB Level
qry <- sprintf('https://api.cvdprevent.nhs.uk//indicator/%d/rawDataJSON?timePeriodID=%d&systemLevelID=%d', 
               df_icb_indicators$IndicatorID[df_icb_indicators$IndicatorCode=='CVDP003CHOL'],
               latest_period_id, 
               df_system_levels$SystemLevelID[df_system_levels$SystemLevelName=="ICB"])
res <- GET(qry)
df_icb_cvdp003chol <- fromJSON(rawToChar(res$content))$indicatorRawData

# PCN Level
qry <- sprintf('https://api.cvdprevent.nhs.uk//indicator/%d/rawDataJSON?timePeriodID=%d&systemLevelID=%d', 
               df_icb_indicators$IndicatorID[df_icb_indicators$IndicatorCode=='CVDP003CHOL'],
               latest_period_id, 
               df_system_levels$SystemLevelID[df_system_levels$SystemLevelName=="PCN"])
res <- GET(qry)
df_pcn_cvdp003chol <- fromJSON(rawToChar(res$content))$indicatorRawData

# 2. Get Open Prescribing data ----
# ═════════════════════════════════

# * 2.1. Get numerator data Inclisiran 0212000AM ----
# ───────────────────────────────────────────────────

# ICB Level
qry <- 'https://openprescribing.net/api/1.0/spending_by_org/?org_type=icb&code=0212000AM&format=json'
res <- GET(qry)
df_icb_inclisiran <- fromJSON(rawToChar(res$content))

# PCN Level
qry <- 'https://openprescribing.net/api/1.0/spending_by_org/?org_type=pcn&code=0212000AM&format=json'
res <- GET(qry)
fromJSON(rawToChar(res$content))
df_pcn_inclisiran <- fromJSON(rawToChar(res$content))

# 3. Get Organisational Mapping Data ----
# ═══════════════════════════════════════

df_hin_icb_lu <- read.csv('C:/Data/HIN/hin_icb_lu.csv')

df_org_map <- read.csv('C:/Data/NHSD/GPREGPDS/gp-reg-pat-prac-map.csv')

# 4. Calculate Proxy Performance ----
# ═══════════════════════════════════

df_icb_performance <- df_icb_cvdp003chol %>% 
  filter(CategoryAttribute=='Persons' &
           MetricCategoryName=='Persons' & 
           MetricCategoryTypeName=='Sex') %>% 
  mutate(AreaCode = if_else(AreaCode=='E54000052', 'E54000063',
                            if_else(AreaCode=='E54000053', 'E54000064', AreaCode))) %>%
  mutate(ICB_ONS_CODE = AreaCode, DENOMINATOR = Denominator, .keep = 'none') %>%
  left_join(
    df_icb_inclisiran %>% 
      mutate(date = as.Date(date)) %>% 
      filter(date >= as.Date('2023-04-01') & 
               date < as.Date('2024-04-01')) %>%
      group_by(row_id) %>%
      summarise(items = sum(items)) %>%
      ungroup() %>% 
      mutate(ICB_ODS_CODE = row_id, NUMERATOR = items, .keep = 'none') %>%
      left_join(df_hin_icb_lu, by = 'ICB_ODS_CODE'),
    by = 'ICB_ONS_CODE') %>%
  select(5, 6, 1, 3, 7, 4, 2) %>%
  mutate(ITEMS_PER_COHORT = NUMERATOR / DENOMINATOR)

write.csv(df_icb_performance, 'icb_performance.csv')

  df_org_map %>% filter(ONS_ICB_CODE =='E54000052')

