pacman::p_load(tidyverse,sf, strayr)

#download seifa sa1 data

seifa_sa1_url <- "https://www.abs.gov.au/statistics/people/people-and-communities/socio-economic-indexes-areas-seifa-australia/2021/Statistical%20Area%20Level%201%2C%20Indexes%2C%20SEIFA%202021.xlsx"
curl::curl_download(url = seifa_sa1_url,quiet = FALSE,
                    destfile = "data/seifa_2021_sa1.xlsx")

# Clean up seifa data
irsad <- readxl::read_xlsx("data/seifa_2021_sa1.xlsx",sheet = "Table 3",skip = 5) %>% .[-c(4,8)] %>% 
  setNames(c("sa1","pop","israd_scores","aus_rank","aus_decile","aus_percentile","state","state_rank","state_decile","state_percentile")) %>% 
  mutate(sa1 = as.character(sa1))

#read sa1 sf
sa1<-strayr::read_absmap("sa12021")

#join sa1_sf with seifa data
seifa_21_joined<- sa1 %>% inner_join(irsad, by = c("sa1_code_2021" = "sa1"))

# summary
summary<-
seifa_21_joined %>% st_drop_geometry() %>% filter(aus_percentile >=80 | aus_percentile <=20) %>% 
  mutate(category = if_else(aus_percentile >= 80,"Top20","Bottom20")) %>% 
  group_by(category,gcc_name_2021) %>% 
  summarise(across(c(areasqkm_2021,pop),sum),
            sa1_cnt = n()) %>% 
  rename(gcc_name = gcc_name_2021)

# summary redefined
summary %>% select(category,gcc_name,pop) %>% 
  pivot_wider(names_from = category,values_from = pop) %>% 
  mutate(ratio = Top20/Bottom20) %>% arrange(desc(ratio)) 
