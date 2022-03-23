##############################
### MEDIA DATA PREPARATION ###
##############################

# Packages
library(haven)
library(tidyverse)
library(readxl)
#install.packages("zoo")

# Data
tv <- read_dta("data/raw/ZA6808_v1-0-0.dta")
print <- read_dta("data/raw/ZA6809_v1-0-0.dta")
topics_raw =read_excel("data/raw/ZA680x_topics_raw.xlsx")


##########
# TOPICS #
##########

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
  dplyr::select(topic_id,description) %>% 
  mutate(mod = as.numeric(topic_id) %% 10,
         meta_id = floor(as.numeric(topic_id)/10)*10)

# Data on Meta Topics and Policy Area

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
  dplyr::select(topic_id,description)

#Main data
main =  meta_topics %>%
  mutate(stay = topic_id == main_id) %>% 
  filter(stay) %>% 
  dplyr::select(topic_id,description) %>% 
  anti_join(areas) 


#Meta data
meta = meta_topics %>%
  dplyr::select(topic_id,description) %>% 
  anti_join(main) %>% 
  anti_join(areas) 

meta_topics= meta_topics %>% 
  dplyr::select(meta_id:area_id) %>% 
  left_join(areas %>% 
              setNames(c("area_id","area_desc"))) %>% 
  left_join(main%>% 
              setNames(c("main_id","main_desc"))) %>% 
  left_join(meta%>% 
              setNames(c("meta_id","meta_desc"))) %>% 
  filter(!is.na(meta_desc))


#######################################
# IDENTIFY IMMIGRATION_RELATED ISSUES #
#######################################

# SAVE IDS OF ISSUES RELATED TO IMMIGRATION
immigration = c(3750,3751,3752,3753,3756,3757,3411,3413)

# 3750 "Zuwanderung allgemein, Auch: Einbürgerungstest"
# 3751 "Integration speziell"
# 3752 "Begrenzung Zuwanderung speziell, Hier auch „Obergrenze“"
# 3753 "Asyl speziell, Hier auch: „Flüchtlingskrise“, Hier auch Bearbeitung von Asylanträgen und Abschiebung von Asylbewerbern"
# 3756 "Sonderthema Integration von Muslimen/dem Islam"
# 3757 "Sonderthema Zuwanderung „Wirtschaftsflüchtlinge“"
# 3411 "Ausländer-Kriminalität speziell"
# 3413 "Islamismus speziell, Auch IS-Rückkehrer"


###########
# TV DATA #
###########

# Add meta ID
tv_policy = tv %>% 
  dplyr::select(id_sdg:v05,v08,b02,id_bei,b05,b06:b09,b10,b15,b16,b18a:b18d) %>% 
  mutate(meta_top = floor(b16/10)*10) %>%
  mutate(main_top = floor(b16/100)*100)

# Add date variable
tv_policy = 
  tv_policy %>% 
  mutate(month = ifelse(v04<10,paste0("0",v04),v04),
         date = paste0("20",v03,"-",month,"-",v05) %>% as.POSIXct()) %>% 
  dplyr::select(-c(month,v03,v04,v05))


##############
# PRINT DATA #
##############

# Add meta ID
print_policy = print %>% 
  dplyr::select(id_asg:v05,id_bei,b06,b08:b10,b15,b16,b18a:b18d) %>% 
  mutate(meta_top = floor(b16/10)*10) %>%
  mutate(main_top = floor(b16/100)*100)

# Add date variable
print_policy = 
  print_policy %>% 
  mutate(month = ifelse(v04<10,paste0("0",v04),v04),
         date = paste0("20",v03,"-",month,"-",v05) %>% as.POSIXct()) %>% 
  dplyr::select(-c(month,v03,v04,v05))


# Statistics on Immifration related contributions

# TV
tv_policy %>% 
  filter(b16 %in% immigration) %>% 
  mutate(n = n()) %>% 
  mutate(sdg = length(unique(id_sdg))) %>% 
  distinct(n,sdg)

print_policy %>% 
  filter(b16 %in% immigration) %>% 
  mutate(n = n()) %>% 
  mutate(asg = length(unique(id_asg))) %>% 
  distinct(n,asg)

#############
# SAVE DATA #
#############

saveRDS(print_policy,"data/print_policy.rds")
saveRDS(tv_policy,"data/tv_policy.rds")
saveRDS(meta_topics,"data/media_meta_topics.rds")
saveRDS(immigration,"data/immi_topic_vec.rds")




