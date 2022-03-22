library(haven)
library(tidyverse)
library(readxl)
tv <- read_dta("data/ZA6808_v1-0-0.dta")
print <- read_dta("data/ZA6809_v1-0-0.dta")

topics_raw =read_excel("data/ZA680x_topics_raw.xlsx")


#edit topics df
topics.final = 
  topics_raw %>% 
  setNames(c("id","topic")) %>% 
  mutate(id = zoo::na.locf(id)) %>% 
  filter(!is.na(topic)) %>% 
  mutate(topic_id = str_extract(id,"\\d\\d\\d\\d")) %>% 
  filter(!is.na(topic_id)) %>% 
  group_by(topic_id) %>% 
  mutate(description = paste0(topic,collapse = ", ")) %>% 
  ungroup() %>% 
  distinct(topic_id,.keep_all = T) %>% 
  select(topic_id,description) %>% 
  mutate(mod = as.numeric(topic_id) %% 10,
         meta_id = floor(as.numeric(topic_id)/10)*10)

# topics.join =
#   topics %>% 
#   left_join(topics.final) %>% 
#   distinct(topic_id,.keep_all = TRUE)



meta_topics = topics.final %>% 
  mutate(topic_id = as.numeric(topic_id)) %>% 
  mutate(meta_id = floor(topic_id/10)*10,
         main_id = floor(topic_id/100)*100,
         area_id = floor(topic_id/1000)*1000)

#Area data
areas =  meta_topics %>%
  mutate(stay = topic_id == area_id) %>% 
  filter(stay) %>% 
  slice(1:3) %>% 
  select(topic_id,description)

#Main data
main =  meta_topics %>%
  mutate(stay = topic_id == main_id) %>% 
  filter(stay) %>% 
  select(topic_id,description) %>% 
  anti_join(areas) 


#meta data
meta = meta_topics %>%
  select(topic_id,description) %>% 
  anti_join(main) %>% 
  anti_join(areas) 

meta_topics= meta_topics %>% 
  select(meta_id:area_id) %>% 
  left_join(areas %>% 
              setNames(c("area_id","area_desc"))) %>% 
  left_join(main%>% 
              setNames(c("main_id","main_desc"))) %>% 
  left_join(meta%>% 
              setNames(c("meta_id","meta_desc"))) %>% 
  filter(!is.na(meta_desc))


saveRDS(topics,"data/GLES/topics.rds")
test = readRDS("C:/Users/Felix/Desktop/Master/4. Semester/Thesis/Daten/Analyse/data/GLES/topics.rds")

#only look at policy topics
#basic info 


tv_policy = tv %>% 
  select(id_sdg:v05,v08,b02,id_bei,b05,b06:b09,b10,b15,b16,b18a:b18d) %>% 
  mutate(meta_top = floor(b16/10)*10) %>%
  mutate(main_top = floor(b16/100)*100)

tv_label =lapply(tv_policy, attr, "label") %>% unlist()

print_policy = print %>% 
  select(id_asg:v05,id_bei,b06,b08:b10,b15,b16,b18a:b18d) %>% 
  mutate(meta_top = floor(b16/10)*10) %>%
  mutate(main_top = floor(b16/100)*100)

print_label =lapply(print_policy, attr, "label") %>% unlist()

# most prominent policy topics
#tv 
prominent.policy.tv =
  tv_policy %>% 
  filter(b16>=0) %>% 
  group_by(meta_top) %>% 
  summarise(sendungen = n_distinct(id_sdg),
            beitrag = n()) %>% 
  arrange(desc(beitrag)) %>% 
  left_join(topics %>% 
              select(meta_id,meta_desc) %>% 
              distinct(),by = c("meta_top"="meta_id"))

#most prominet: 
# 3140 (in 64 Sendungen und 133 Beiträgen),
# 3178 (in 108 Sendungen und 127 Beiträgen)
# 3753 (in 75 Sendungen mit 85 Beiträgen)
# Asyl und Zuwander = 3750-3757

#meta tops
#140 177 Deutschlands Rolle/Stellung in der Welt allgemein
#64 133 Gipfeltreffen allgemein, Hier auch Berichte zu G20 in Hamburg
#93 109 Zuwanderung allgemein, Auch: Einbürgerungstest

#print
prominent.policy.print =
  print_policy %>% 
  filter(b16>=0) %>% 
  group_by(meta_top) %>% 
  summarise(sendungen = n_distinct(id_asg),
            beitrag = n()) %>% 
  arrange(desc(beitrag)) %>% 
  left_join(topics %>% 
              select(meta_id,meta_desc) %>% 
              distinct(),by = c("meta_top"="meta_id"))

#most prominet: 
# 3140 (in 64 Sendungen und 133 Beiträgen),
# 3178 (in 108 Sendungen und 127 Beiträgen)
# 3753 (in 75 Sendungen mit 85 Beiträgen)
# Asyl und Zuwander = 3750-3757

#meta tops
#3170 200 asg 301 beitr Deutschlands Rolle/Stellung in der Welt allgemein
#3750 172 asg 245 beitr Zuwanderung allgemein, Auch: Einbürgerungstest
#3140 83 asg 188 beitr Gipfeltreffen allgemein, Hier auch Berichte zu G20 in Hamburg

#create date var
print_policy = 
  print_policy %>% 
  mutate(month = ifelse(v04<10,paste0("0",v04),v04),
         date = paste0("20",v03,"-",month,"-",v05) %>% as.POSIXct()) %>% 
  select(-c(month,v03,v04,v05))

tv_policy = 
  tv_policy %>% 
  mutate(month = ifelse(v04<10,paste0("0",v04),v04),
         date = paste0("20",v03,"-",month,"-",v05) %>% as.POSIXct()) %>% 
  select(-c(month,v03,v04,v05))

saveRDS(print_policy,"data/GLES/print_policy.rds")
saveRDS(tv_policy,"data/GLES/tv_policy.rds")
