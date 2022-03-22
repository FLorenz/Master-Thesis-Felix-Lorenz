#############################
### BUILD TIMESERIES DATA ###
#############################

# Packages
library(tidyverse)

# Data
tv_data = readRDS("data/tv_policy.rds")
print_data = readRDS("data/print_policy.rds")
immi_topic = readRDS("data/immi_topic_vec.rds")
tweets = readRDS("data/raw/full_data_fin.rds")
immi_df = readRDS("data/immi_tweets_red.rds")
partycolor = readRDS("data/raw/partycolor.rds")
summary = readRDS("data/raw/summary.rds")

##############
# MEDIA DATA #
##############

# TV
tv_ts =tv_data %>% 
  mutate(immi = ifelse(b16 %in% immi_topic,T,F)) %>% 
  group_by(date) %>% 
  summarise(total = n(),
            immi_count = sum(immi),
            share= immi_count/total) %>% 
  ungroup()


# Print
print_ts =print_data %>% 
  mutate(immi = ifelse(b16 %in% immi_topic,T,F)) %>% 
  group_by(date) %>% 
  summarise(total = n(),
            immi_count = sum(immi),
            share = immi_count/total) %>% 
  ungroup()

################
# TWITTER DATA #
################

#Round Date before usage
immi_df = immi_df %>% 
  mutate(created_at = lubridate::round_date(as.POSIXct(created_at),"day"))

tweets = tweets %>% 
  mutate(created_at = lubridate::round_date(as.POSIXct(created_at),"day"))


immi_party = immi_df %>% 
  filter(!retweet) %>% 
  left_join(summary %>% 
              dplyr::select(author_id,party)) %>% 
  dplyr::select(created_at,party) %>% 
  mutate(party = ifelse(party %in% c(1,5),8,party)) %>% 
  left_join(partycolor) %>% 
  dplyr::select(-c(party,hex)) %>% 
  group_by(created_at,partyname) %>% 
  table() %>% 
  data.frame() %>% 
  mutate(created_at = as.POSIXct(created_at))


total_party = tweets %>% 
  filter(!retweet) %>% 
  select(id,author_id,created_at) %>% 
  left_join(summary %>% 
              select(author_id,party),by = c("author_id")) %>%
  mutate(party = ifelse(party %in% c(1,5),8,party)) %>% 
  group_by(created_at,party) %>% 
  summarise(total = n()) %>% 
  mutate(party = as.character(party)) %>% 
  left_join(partycolor) %>% 
  ungroup()

total_party_var = total_party %>% 
  left_join(immi_party) %>% 
  mutate(share = Freq/total) %>% 
  pivot_wider(id_cols = c(created_at,partyname),names_from = partyname,values_from = c(share,Freq,total),names_sep=".")


#############
# FULL DATA #
#############
full_data = total_party_var %>% 
  dplyr::select(c(created_at:`share.CDU/CSU`)) %>% 
  left_join(tv_ts %>% 
              dplyr::select(date,share) %>% 
              setNames(c("created_at","share.tv"))) %>% 
  left_join(print_ts %>% 
              dplyr::select(date,share) %>% 
              setNames(c("created_at","share.print")))

########################################
# IMPUTE DATA FOR NEWSPAPERS ON SUNDAY #
########################################

#Possibility 1: Replace sundays with mondays?

db.monday = full_data
db.monday$lead.print.share = lead(db.monday$share.print)

db.monday = db.monday %>% 
  mutate(share.print=ifelse(is.na(share.print),lead.print.share,share.print)) %>% 
  dplyr::select(-lead.print.share)


#Possibility 2: Impute mean of the week  before 
db.impute = full_data

share = db.impute$share.print
pointer = 1
for(i in seq_along(share)){
  if(is.na(share[i])){
    mean1=mean(share[c(pointer:(i-1))])
    share[i]=mean1
    pointer = i+1
  }
}
db.impute$share.print = share


#############
# SAVE DATA #
#############

saveRDS(db.impute,"data/db.impute.rds")
saveRDS(db.monday,"data/db.monday.rds")
