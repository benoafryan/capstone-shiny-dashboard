options(scipen = 999)
library(purrr) 
library(tidyverse)
library(DT)
library(jsonlite)
library(lubridate)
library(glue)
library(plotly)
library(ggplot2)
library(xts)
library(dygraphs)
library(scales)

ispu <- fromJSON("ispu_jkt_2016_2021.json")

ispu_clean <- ispu %>% 
  relocate(pm25, .after = pm10) %>% 
  mutate_at(vars(pm10,pm25,so2,co,o3,no2,max), as.numeric) %>% 
  mutate(tanggal = ymd(tanggal)) %>% 
  select(-c(lokasi_spku,critical,max, categori))

ispu_complete <- ispu_clean %>%
  mutate(maks = pmax(pm10,pm25,so2,co,o3,no2, na.rm = TRUE),
         crit = pmap_chr(select(., pm10:no2), ~ c(...) %>%  #extract column names of critical values
                           which.max %>%
                           names ),
         crit = as.factor(crit),
         ispu_pm10 =  case_when(pm10 >= 0 & pm10 <= 50 ~ (50-0)/(50-0)*(pm10-0)+0,
                                pm10 > 50 & pm10 <= 150 ~ (100-50)/(150-50)*(pm10-50)+50,
                                pm10 > 150 & pm10 <= 350 ~ (200-100)/(350-150)*(pm10-150)+100,
                                pm10 > 350 & pm10 <= 420 ~ (300-200)/(420-350)*(pm10-350)+200,
                                pm10 > 420 & pm10 <=500 ~ (300)/(500-420)*(pm10-420)+300),
         ispu_o3 =  case_when(o3 >= 0 & o3 <= 120 ~ (50-0)/(120-0)*(o3-0)+0,
                              o3 > 120 & o3 <= 235 ~ (100-50)/(235-120)*(o3-120)+50,
                              o3 > 235 & o3 <= 400 ~ (200-100)/(400-235)*(o3-235)+100,
                              o3 > 400 & o3 <= 800 ~ (300-200)/(800-400)*(o3-400)+200,
                              o3 > 800 & o3 <= 1000 ~ (300)/(1000-800)*(o3-800)+300),
         ispu_so2 =  case_when(so2 > 0 & so2 <= 52 ~ (50-0)/(52-0)*(so2-0)+0,
                               so2 > 52 & so2 <= 180 ~ (100-50)/(180-52)*(so2-52)+50,
                               so2 > 180 & so2 <= 400 ~ (200-100)/(400-180)*(so2-180)+100,
                               so2 > 400 & so2 <= 800 ~ (300-200)/(800-400)*(so2-400)+200,
                               so2 > 800 & so2 <= 1200 ~ (300)/(1200-800)*(so2-800)+300),
         ispu_co =  case_when(co >= 0 & co <= 4000 ~ (50-0)/(4000-0)*(co-0)+0,
                              co > 4000 & co <= 8000 ~ (100-50)/(8000-4000)*(co-4000)+50,
                              co > 8000 & co <= 15000 ~ (200-100)/(15000-8000)*(co-8000)+100,
                              co > 15000 & co <= 30000 ~ (300-200)/(30000-15000)*(co-15000)+200,
                              co > 30000 & co <= 45000 ~ (300)/(45000-30000)*(co-30000)+300),
         ispu_no2 =  case_when(no2 >= 0 & no2 <= 80 ~ (50-0)/(80-0)*(no2-0)+0,
                               no2 > 80 & no2 <= 200 ~ (100-50)/(200-80)*(no2-80)+50,
                               no2 > 200 & no2 <= 1130 ~ (200-100)/(1130-200)*(no2-200)+100,
                               no2 > 1130 & no2 <= 2260 ~ (300-200)/(2260-1130)*(no2-1130)+200,
                               no2 > 2260 & no2 <= 3000 ~ (300)/(3000-2260)*(no2-2260)+300),
         ispu_pm25 =  case_when(pm25 >= 0 & pm25 <= 15.5 ~ (50-0)/(15.5-0)*(pm25-0)+0,
                                pm25 > 15.5 & pm25 <= 55.4 ~ (100-50)/(55.4-15.5)*(pm25-15.5)+50,
                                pm25 > 55.4 & pm25 <= 150.4 ~ (200-100)/(150.4-55.4)*(pm25-55.4)+100,
                                pm25 > 150.4 & pm25 <= 250.4 ~ (300-200)/(250.4-150.4)*(pm25-150.4)+200,
                                pm25 > 250.4 & pm25 <= 500 ~ (300)/(500-250.4)*(pm25-250.4)+300),
         ispu_max =  pmax(ispu_pm10,ispu_o3,ispu_so2,ispu_co,ispu_no2,ispu_pm25, na.rm = TRUE),
         ispu_cat =  case_when(ispu_max > 0 & ispu_max <= 50 ~ "BAIK",
                               ispu_max >50 & ispu_max <=100 ~ "SEDANG",
                               ispu_max > 100 & ispu_max <= 200 ~ "TIDAK SEHAT",
                               ispu_max > 200 & ispu_max <= 300 ~ "SANGAT TIDAK SEHAT",
                               ispu_max > 300 ~ "Berbahaya"),
         ispu_cat =  as.factor(ispu_cat))

ispu_count <- ispu_complete %>% 
  group_by(tahun = as.factor(year(tanggal))) %>% 
  select(c(ispu_cat)) %>% 
  count(ispu_cat) %>% 
  pivot_wider(names_from = ispu_cat, values_from = n) %>%
  ungroup() %>% 
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))



ispu_select <- ispu_count %>% 
  add_row(tahun = "All", 
          BAIK = sum(ispu_count$BAIK), 
          SEDANG = sum(ispu_count$SEDANG), 
          `TIDAK SEHAT` = sum(ispu_count$`TIDAK SEHAT`), 
          `SANGAT TIDAK SEHAT` = sum(ispu_count$`SANGAT TIDAK SEHAT`)) 

allParam <- ispu_complete %>% 
  group_by(tahun = floor_date(tanggal, "year")) %>%
  summarise("PM\u2081\u2080" = mean(pm10),
            PM25 = mean(pm25, na.rm = TRUE),
            "SO\u2082" = mean(so2),
            CO = mean(co),
            "O\u2083" = mean(o3),
            "NO\u2082" = mean(no2)) %>% 
  ungroup() %>% 
  pivot_longer(-tahun, names_to = "Parameter", values_to = "Values") %>% 
  mutate(Parameter = as.factor(Parameter),
         label = glue("Tahun : {year(tahun)}
                                  {Parameter} : {scales::comma(Values)}")) %>% 
  arrange(tahun, -Values)