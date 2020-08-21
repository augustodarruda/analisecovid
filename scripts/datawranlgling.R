#packages

library(datacovidbr)    # import data from datacovidbr package
library(dplyr)          # data procesing
library(ggplot2)        # data plotting
library(tidyr)          # organize data
library(lubridate)      # manipulate dates
library(RColorBrewer)   # color pallets
library(magrittr)       # pipes
library(data.table)     # manipulate data
library(inspectdf)      # library to inspect data frames
library(scales)         # change scales in plots

# importing data
df <- brMinisterioSaude()

# renaming some columns
df <-
  df %>%
  rename(epiweek = semanaEpi,
         pop = populacaoTCU2019,
         cases = casosAcumulado,
         daily_cases = casosNovos,
         deaths = obitosAcumulado,
         daily_deaths = obitosNovos,
         daily_recovered = Recuperadosnovos,
         daily_tracking = emAcompanhamentoNovos)

col_fac <- c("regiao","estado","municipio","nomeRegiaoSaude")

# columns to factor
df <-
  df %<>%
  mutate_each_(funs(factor(.)),col_fac)

# column codRegiaoSaude and  to integer
df$codRegiaoSaude <- as.integer(df$codRegiaoSaude)
df$`interior/metropolitana`<- as.integer(df$`interior/metropolitana`)

# columns full with NA's
df$daily_recovered <- NULL
df$daily_tracking <- NULL

# preparing the dataset to the 5 regions of Brazil
df_region <- df %>% 
  filter(is.na(municipio) & regiao != "Brasil" & pop > 0) %>% # selecting data per region
  group_by(date,regiao) %>% #goup_by region
  summarise(cases = sum(cases),
            daily_cases = sum(daily_cases),
            deaths = sum(deaths),
            daily_deaths = sum(daily_deaths),
            pop = sum(pop)) %>% # Selecionando colunas de dados
  select(date,regiao,cases,daily_cases,deaths,daily_deaths,pop)

# national data for later join
df_br <- df %>%
  filter(regiao == "Brasil") %>%
  select(date,regiao,cases,daily_cases,deaths,daily_deaths,pop)

# full join
df_region <- full_join(df_region, df_br, by = 'date')

# percentages to the total
df_region <- 
  df_region %>%
  mutate(pct_total_cases = (cases.x/cases.y)) %>%
  mutate(pct_daily_cases = (daily_cases.x/daily_cases.y)) %>%
  mutate(pct_total_deaths = (deaths.x/deaths.y)) %>%
  mutate(pct_daily_deaths = (daily_deaths.x/daily_deaths.y))

# weekly avereges
df_region <- df_region %>%
  group_by(regiao.x) %>%
  mutate(weekly_avg_pct_daily_cases = frollmean(pct_daily_cases,7)) %>%
  mutate(weekly_avg_daily_cases = frollmean(daily_cases.x,7)) %>%
  mutate(weekly_avg_pct_daily_deaths = frollmean(pct_daily_deaths,7)) %>%
  mutate(weekly_avg_daily_deaths = frollmean(daily_deaths.x,7)) %>%
  # per capita numbers
  mutate(cases_100k = (cases.x/pop.x)*100000) %>%
  mutate(daily_cases_100k = (daily_cases.x/pop.x)*100000)%>%
  mutate(deaths_100k = (deaths.x/pop.x)*100000) %>%
  # weekly average per capita
  mutate(weekly_avg_daily_cases_100k = (weekly_avg_daily_cases/pop.x)*100000) %>%
  mutate(weekly_avg_daily_cases_mil = (weekly_avg_daily_cases/pop.x)*1000000) %>%
  mutate(weekly_avg_daily_deaths_100k = (weekly_avg_daily_deaths/pop.x)*100000) %>%
  mutate(weekly_avg_daily_deaths_mil = (weekly_avg_daily_deaths/pop.x)*1000000) %>%
  mutate(RM_weekly_avg_daily_cases_100k = frollmean(weekly_avg_daily_cases_100k,7)) %>%
  mutate(RM_weekly_avg_daily_cases_mil = frollmean(weekly_avg_daily_cases_mil,7)) %>%
  mutate(RM_weekly_avg_daily_deaths_100k = frollmean(weekly_avg_daily_deaths_100k,7))