##########################
# DESCRIPTIVE STATISTICS #
##########################

##########################
# LOAD PACKAGES AND DATA #
##########################

# PACKAGES
library(tidyverse)
library(showtext)

# DATA
immi_df = readRDS("data/immi_tweets_red.rds")
summary = readRDS("data/raw/summary.rds")
partycolor = readRDS("data/raw/partycolor.rds")
all_cand <- read_csv("data/raw/candidates_full.csv")
share_data = readRDS("data/db.impute.rds") 
tv_policy = readRDS("data/tv_policy.rds")
print_policy = readRDS("data/print_policy.rds")
tweet_data = readRDS("data/raw/full_data_fin.rds")
tweet_data$created_at = lubridate::round_date(as.POSIXct(tweet_data$created_at),"day")

# CHOSE FONT FOR PLOTS
font_add_google("Noto Serif","noto")
showtext_auto()

#INITIALISE COLOR VECTOR
colvec = partycolor$hex
names(colvec)=partycolor$partyname

# HELPER DATAFRAME FOR LATER JOINS
datevec = unique(tweet_data$created_at) %>% 
  sort() 
date_dummy = data.frame(created_at = rep(datevec,each = length(partycolor$party)),
                        party = rep(partycolor$party,length(datevec))) %>% 
  filter(!party %in% c(1,5))

##############################
# How many Tweets per Party? #
##############################

# Total:
plot0 = summary %>% 
  group_by(party) %>% 
  summarise(retweets = sum(n),
            total = sum(total)) %>% 
  mutate(non_rt = total - retweets) %>% 
  pivot_longer(c(retweets,non_rt),
               names_to = "type",
               values_to = "n") %>% 
  left_join(partycolor) %>% 
  ggplot(aes(fct_reorder(partyname,n),n,fill = type))+
  geom_bar(stat="identity",position = "stack",color = "black")+
  labs(x=NULL,y=NULL)+
  scale_fill_manual(values = c("black","grey"),name= "Type",labels = c("Original Tweets","Retweets"))+
  coord_flip()+
  ggthemes::theme_clean(base_family = "noto")+
  theme(axis.text.x = element_text(size =14),
        axis.text.y = element_text(size =14),
        legend.text=element_text(family = "noto"),
        legend.title=element_text(family = "noto"),
        plot.background=element_blank())

ggsave("plots/descriptive/tweets_party.pdf",plot0,width = 148.5, height = 150, units = "mm",
       device = cairo_pdf())

dev.off()

#######################
# Tweet per candidate #
#######################

plot1 = summary %>% 
  group_by(party) %>% 
  summarise(retweets = sum(n)/n(),
            total = sum(total)/n()) %>% 
  mutate(non_rt = total - retweets) %>% 
  pivot_longer(c(retweets,non_rt),
               names_to = "type",
               values_to = "n") %>% 
  left_join(partycolor) %>% 
  ggplot(aes(fct_reorder(partyname,n),n,fill = type))+
  geom_bar(stat="identity",position = "stack",color ="black")+
  scale_fill_manual(values = c("black","grey"),name= "Type",labels = c("Original Tweets","Retweets"))+
  coord_flip()+
  labs(x=NULL,y=NULL)+
  ggthemes::theme_clean(base_family = "noto")+
  theme(axis.text.x = element_text(size =14),
        axis.text.y = element_text(size =14),
        legend.text=element_text(family = "noto"),
        legend.title=element_text(family = "noto"),
        plot.background=element_blank())

ggsave("plots/descriptive/tweets_party_avg.pdf",plot1,width = 148.5, height = 150, units = "mm",
       device = cairo_pdf())

dev.off()

#####################
# Most active users #
#####################

plot2 =summary %>% 
  dplyr::select(firstname,name,party,author_id,total,n) %>% 
  mutate(original = total -n) %>% 
  pivot_longer(c(n,original),
               names_to = "type",
               values_to = "n") %>% 
  left_join(partycolor) %>% 
  arrange(desc(total)) %>% 
  slice(1:(20*2)) %>% 
  mutate(full = paste0(name," (",partyname,")")) %>% 
  ggplot(aes(fct_reorder(full,total),n,fill = type))+
  geom_bar(stat = "identity",position = "stack",color = "black")+
  scale_fill_manual(values = c("black","grey"),name= "Type",labels = c("Original Tweets","Retweets"))+
  coord_flip()+
  labs(x=NULL,y=NULL)+
  ggthemes::theme_clean(base_family = "noto")+
  theme(axis.text.x = element_text(size =14),
        axis.text.y = element_text(size =14),
        legend.text=element_text(family = "noto"),
        legend.title=element_text(family = "noto"),
        plot.background=element_blank())



#############
# Timelines #
#############

##############################
# Full Timeline (all tweets) #
##############################


tweet_data$day = tweet_data$created_at
tl_plot = tweet_data %>% 
  select(day,id,author_id) %>% 
  left_join(summary %>% 
              dplyr::select(author_id,party),by = c("author_id")) %>% 
  distinct(id,.keep_all = T) %>% 
  group_by(day,party) %>% 
  summarise(n=n()) %>%
  mutate(party = as.character(party)) %>% 
  left_join(partycolor) %>% 
  ggplot(aes(day,n))+
  geom_line(aes(color = partyname), show.legend = FALSE)+
  geom_vline(xintercept = as.POSIXct("2017-09-03"), linetype = "dashed")+
  facet_wrap(partyname ~ .,ncol =2)+
  scale_color_manual(values = colvec)+
  labs(x=NULL,y=NULL)+
  theme(text = element_text(family  = "noto"),
        axis.text.x = element_text(angle = 45,hjust=1, size =14),
        axis.text.y = element_text( size =14),
        plot.background=element_blank(),
        strip.text = element_text(
          size = 14,face = "bold"),
        legend.position="none",
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = 'solid',
                                        colour = "grey"),
        strip.background =element_rect(fill="lightgrey"))


ggsave("plots/descriptive/tl_total_v2.pdf",tl_plot,width = 297, height = 150, units = "mm",
       device = cairo_pdf())

dev.off()

########################
# Immigration specific #
########################

#remove retweets
immi_no_rt =
  immi_df %>% 
  filter(!retweet)

ts_immi = tweet_data %>% 
  filter(id %in% immi_no_rt$id) %>% 
  left_join(summary %>% 
              dplyr::select(author_id,party),by = c("author_id")) %>% 
  group_by(created_at,party) %>% 
  summarise(immi=n()) %>% 
  mutate(party = as.character(party)) %>% 
  left_join(date_dummy,.) %>% 
  left_join(partycolor) %>% 
  mutate(immi = ifelse(is.na(immi),0,immi)) %>% 
  ggplot(aes(created_at,immi))+
  geom_line(aes(color = partyname), show.legend = FALSE)+
  geom_vline(xintercept = as.POSIXct("2017-09-03"), linetype = "dashed")+
  facet_wrap(partyname ~ .,ncol =2)+
  scale_color_manual(values = colvec)+
  labs(x=NULL,y=NULL)+
  theme(text = element_text(family  = "noto"),
        axis.text.x = element_text(angle = 45,hjust=1, size =14),
        axis.text.y = element_text( size =14),
        plot.background=element_blank(),
        strip.text = element_text(
          size = 14,face = "bold"),
        legend.position="none",
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = 'solid',
                                        colour = "grey"),
        strip.background =element_rect(fill="lightgrey"))


ggsave("plots/descriptive/tl_immi.pdf",ts_immi,width = 297, height = 150, units = "mm",
       device = cairo_pdf())

dev.off()

###################
# Share of Tweets #
###################


ts_share =share_data %>% 
  select(-c(share.tv,share.print)) %>% 
  pivot_longer(share.SPD:`share.CDU/CSU`) %>% 
  set_names(c("day","partyname","share")) %>% 
  mutate(partyname = gsub("share\\.","",partyname)) %>% 
  ggplot(aes(day,share))+
  geom_line(aes(color = partyname), show.legend = FALSE)+
  #geom_vline(xintercept = as.POSIXct("2017-09-03"), linetype = "dashed")+
  facet_wrap(partyname ~ .,ncol =2)+
  scale_color_manual(values = colvec)+
  labs(x=NULL,y=NULL)+
  #ggthemes::theme_clean(base_family = "noto")+
  theme(text = element_text(family  = "noto"),
        axis.text.x = element_text(angle = 45,hjust=1, size =14),
        axis.text.y = element_text( size =14),
        plot.background=element_blank(),
        strip.text = element_text(
          size = 14,face = "bold"),
        legend.position="none",
        #panel.border = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = 'solid',
                                        colour = "grey"),
        strip.background =element_rect(fill="lightgrey"))

ggsave("plots/descriptive/tl_share.pdf",ts_share,width = 297, height = 150, units = "mm",
       device = cairo_pdf())

dev.off()



################################
# Distribution of Twitter User #
################################

# Total Numberof Candidates
all_mod = all_cand %>% 
  select(twlink,party) %>% 
  mutate(party_org = party,
         party = ifelse(party %in% c(1,5),8,party),
         party = as.character(party)) %>% 
  group_by(party) %>% 
  summarise(total = n(),
            tw = sum(!is.na(twlink))) %>% 
  left_join(partycolor)

all_mod2 = 
  summary %>% 
  group_by(party) %>% 
  summarise(exist = n())

all_mod3 = 
  tweet_data %>% 
  select(id,author_id) %>% 
  left_join(summary %>% 
              select(author_id,party),by = c("author_id")) %>% 
  distinct(author_id,party) %>% 
  group_by(party) %>% 
  summarise(sample = n())

all_final = 
  all_mod %>% 
  left_join(all_mod2) %>% 
  left_join(all_mod3) %>% 
  pivot_longer(c(total,tw,sample))

partycolor_mod = 
  partycolor %>% 
  arrange(party) %>% 
  filter(!party %in% c(1,5))

#create gradient color vector 
colors_gradient = lapply(partycolor_mod$hex,function(x){
  colfunc <- colorRampPalette(c(x, "white"))
  colvec = colfunc(4)
  return(colvec[1:3])
}) %>% unlist()
all_final$color = colors_gradient
myColors = colors_gradient
colScale <- scale_fill_manual(name = "col_name",values = myColors)

party_names <- c(
  `0` = "CDU/CSU",
  `2` = "SPD",
  `3` = "Left",
  `4` = "Greens",
  `6` = "FDP",
  `7` = "AfD"
)

# Chage party number
final_data =all_final %>% 
  mutate(party = ifelse(party == 8,0,party)) %>% 
  arrange(party)


# Final Plot
desc_plot = final_data %>% 
  ggplot(aes(fct_reorder(name,desc(value)),value, group=party))+
  geom_bar(stat="identity",
           position = "dodge",
           fill = final_data$color,
           color = "black",show.legend = F) +
  facet_grid(~party, switch = "x", scales = "free_x", space = "free_x",labeller = as_labeller(party_names))+
  geom_text(aes(label = glue::glue("{value}")),size =4,vjust = -.2)+
  scale_x_discrete(labels=c("total" = "Total", "tw" = "With Twitter Account",
                            "sample" = "In Sample"))+
  labs(y="Number of Candidates",x=NULL)+
  ggthemes::theme_clean(base_family = "noto")+
  theme(axis.text.x = element_text(angle = 45,hjust=1, size =11),
        axis.title.y = element_text(size =11),
        plot.background=element_blank(),
        strip.text = element_text(
          size = 14,face = "bold"))


ggsave("plots/descriptive/distribution.pdf",desc_plot,width = 297, height = 150, units = "mm",
       device = cairo_pdf())

dev.off()

################################
# Timeline Share of Media Data #
################################


media = share_data %>% 
  select(created_at,share.tv,share.print) %>% 
  pivot_longer(c(share.tv,share.print))

media$name_fac = as.factor(media$name)
levels(media$name_fac) = c("Print Media","Television")

media_share = ggplot(media,aes(x=created_at,y=value))+
  geom_line(aes(color = name_fac),show.legend = F)+
  facet_wrap(name_fac ~ .,ncol =1)+
  labs(x=NULL,y=NULL)+
  theme(text = element_text(family  = "noto"),
        axis.text.x = element_text(angle = 45,hjust=1, size =14),
        axis.text.y = element_text( size =14),
        plot.background=element_blank(),
        strip.text = element_text(
          size = 14,face = "bold"),
        legend.position="none",
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = 'solid',
                                        colour = "grey"),
        strip.background =element_rect(fill="lightgrey"))

ggsave("plots/descriptive/media_share.pdf",media_share,width = 297, height = 90, units = "mm",
       device = cairo_pdf())

dev.off()
