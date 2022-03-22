#########################################
### SELECT IMMIGRATION RELATED TWEETS ###
#########################################


########################################################
# REMARK:                                              #
########################################################
# This script cannot be executed properly because the  #
# Twitter guidelines prohibit sharing the full tweets. #
# Please use data/immi_tweets.rds when replicating     #
########################################################



# Packages
library(tidyverse)
library(quanteda)
library(tidytext)

# Data
tweets = readRDS("data/full_data_fin.rds")
summary = readRDS("data/summary.rds")

###################################################
# SEARCH IMMIGRATION RELATED TWEETS WITH QUANTEDA #
###################################################

# Create text corpus of Tweets
party_corp = corpus(tweets,docid_field = "id")

# Tokenize the words in the corpus
party_tokens  = tokens(party_corp,remove_punct = T, remove_symbols = T,remove_url = T,
                       remove_numbers = T,remove_separators = T) %>% 
  tokens_tolower()

# Create a document-feature matrix for later lookup
party_dfm = party_tokens %>% dfm()

# Remove Stopwords for more efficient lookup
party_dfm = party_dfm %>% 
  dfm_remove(stopwords("german"))



# Create dictionary with relevant terms
immi_words <- list(immigration2 = c(
  "immigr*", "flüchtl*", "flucht*", "migran*", "einwander*",
  "syr*", "zuwander*", "*refugee*", "*grenz*", "zuwander*",
  "islam*","muslim*","moslem*","abschieb*","herkunf*","*usländer",
  "*asyl*","*ussengrenze*","*ußengrenze*"
)
)

immi_dictionary <- dictionary(immi_words)

# Search for Tweets containing words in dictionary
immi_dfm <- party_dfm %>%
  dfm_lookup(immi_dictionary) 


immi_df = immi_dfm %>% 
  tidy() %>%
  arrange(desc(count)) 

# Tweets with relation to immigration
immi_tweets = tweets %>% 
  filter(id %in% immi_df$document)

# Reduced data for publication
immi_tweets_red = immi_tweets %>% 
  mutate(retweet = substr(text,1,2)=="RT") %>% 
  select(created_at,id,author_id,retweet)

#############
# SAVE DATA #
#############
saveRDS(immi_tweets,"immi_tweets.rds")
saveRDS(immi_tweets_red,"immi_tweets_red.rds")
