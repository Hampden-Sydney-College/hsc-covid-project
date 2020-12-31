library(rvest)
library(tidyverse)

web_page <- read_html("https://wallethub.com/edu/states-coronavirus-restrictions/73818")

tbls <- html_nodes(web_page, "table")

tbls_ls <- 
web_page %>% 
  html_nodes("table") %>% 
  .[1:1] %>% 
  html_table(fill = TRUE)

restrictions <- 
tbls_ls[[1]] %>% 
  as_tibble() %>% 
  janitor::clean_names() 

covid_cum <- 
  covid19.analytics::covid19.JHU.data() %>% 
  janitor::clean_names()

covid_cum %>% 
  filter(country_region == "US") %>% 
  select(admin2, state = province_state, confirmed, deaths, case_fatality_ratio) %>% 
  group_by(state) %>% 
  mutate(cases = sum(confirmed),
         deaths = sum(deaths),
         cfr = deaths / cases * 100) %>% 
  left_join(restrictions) %>% 
  ggplot(aes(x = total_score, y = cfr)) + 
  geom_point() + 
  geom_smooth(method = "lm")

covid_cum %>% 
  filter(country_region == "US") %>% 
  select(admin2, state = province_state, confirmed, deaths, case_fatality_ratio) %>% 
  group_by(state) %>% 
  mutate(cases = sum(confirmed),
         deaths = sum(deaths),
         cfr = deaths / cases * 100) %>% 
  left_join(restrictions) %>% 
  lm(cfr ~ total_score, .) %>% 
  jtools::summ(scale = TRUE)

covid_cum %>% 
  filter(country_region == "US") %>% 
  select(admin2, state = province_state, confirmed, deaths, case_fatality_ratio) %>% 
  group_by(state) %>% 
  mutate(cases = sum(confirmed),
         deaths = sum(deaths),
         cfr = (deaths / cases) * 100) %>% 
  left_join(restrictions) %>% 
  left_join(state_pop) %>% 
  mutate(cases_cap = cases / pop * 100000) %>% 
  ggplot(aes(x = total_score, y = cases_cap)) + 
  geom_point() + 
  geom_smooth(method = "lm")
  
state_pop <- 
  here::here("data", "co-est2019-alldata.csv") %>% 
    read_csv() %>% 
    janitor::clean_names() %>%
    filter(county == "000") %>% 
    select(state = stname, pop = popestimate2019) 

covid_cum %>% 
  filter(country_region == "US") %>% 
  select(admin2, state = province_state, confirmed, deaths, case_fatality_ratio) %>% 
  group_by(state) %>% 
  mutate(cases = sum(confirmed),
         deaths = sum(deaths),
         cfr = deaths / cases * 100) %>% 
  left_join(restrictions) %>% 
  left_join(state_pop) %>% 
  mutate(cases_cap = cases / pop) %>% 
  lm(cases_cap ~ total_score, .) %>% 
  summary()