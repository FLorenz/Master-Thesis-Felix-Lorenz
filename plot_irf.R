##################################
### FUNCTION FOR PLOTTING IRFS ###
##################################

# Packages
library(tidyverse)

# Data
partycolor = readRDS("data/raw/partycolor.rds")

#colvec 
par_col =partycolor$hex
names(par_col)=partycolor$partyname

# Function to plote irfs of the two media channels
media.response = function(datalist,response_of){
  dataname = deparse(substitute(datalist))
  type = gsub("irf.","",dataname)
  type = gsub(".7.7","",type)
  
  reponse = response_of %>% 
    gsub("share.","",.)
  
  plotname = "print_response_monday.pdf"
  plotname = glue::glue("{reponse}_reponse_{type}.pdf")
  
  print(type)
  data = datalist %>% 
    bind_rows() %>% 
    filter((out=="share.tv"&cov=="share.print")|(cov=="share.tv"&out=="share.print"))
  
  plot = data %>% 
    filter(out ==response_of) %>% 
    filter(day %in% c(1,2,3)) %>% 
    mutate(party = gsub("\\.","/",party)) %>% 
    filter(data_type == "Effect of a one time 10 percentage point attention increase at day 0") %>% 
    ggplot(aes(x=party,y = pe, ymin = -1, ymax = 5)) +
    geom_hline(yintercept = 0,color = "red")+
    geom_segment(aes(x=party,xend=party,y=lwr,yend=upr),color="black",size=2)+
    geom_point(aes(y=pe),show.legend = F,color="grey") +
    coord_flip()+
    facet_grid(cols = vars(day))+
    labs(x=NULL,y=NULL)+
    theme(text = element_text(family  = "noto"),
          axis.text.x = element_text(angle = 45,hjust=1, size =14),
          axis.text.y = element_text( size =14),
          plot.background=element_blank(),
          strip.text = element_text(
            size = 14,face = "bold"),
          legend.position="none",
          #panel.border = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(color = "black", fill = NA, size = 1),
          panel.grid.major = element_line(linetype = 'solid',
                                          colour = "lightgrey"),
          strip.background =element_rect(fill="lightgrey"))
  
  ggsave(glue::glue("plots/{plotname}"),plot,width = 297, height = 90, units = "mm",
         device = cairo_pdf())
  dev.off()
}


# Function to plot party responses
party.response = function(datalist,response_of,impulse_of){
  
  dataname = deparse(substitute(datalist))
  type = gsub("irf.","",dataname)
  type = gsub(".7.7","",type)
  
  reponse = response_of %>% 
    gsub("share.","",.)
  impulse = impulse_of %>% 
    gsub("share.","",.)
  
  plotname = glue::glue("{reponse}_reponse_to_{impulse}_{type}.pdf")
  
  data = datalist %>% 
    bind_rows() %>% 
    filter(!((out=="share.tv"&cov=="share.print")|(cov=="share.tv"&out=="share.print")))
  
  variables = unique(data$out)
  
  if(response_of == "party"){
    index = which(!unique(data$out) %in% c("share.print","share.tv"))
    out_var = unique(data$out)[index]
  }
  else{
    out_var = response_of 
  }
  if(impulse_of == "party"){
    index = which(!unique(data$out) %in% c("share.print","share.tv"))
    cov_var = unique(data$out)[index]
  }
  else{
    cov_var = impulse_of
  }
  
  plot = data %>% 
    filter(day %in% c(1,2,3)) %>% 
    filter(!party %in% c("FDP","Linke")) %>% 
    filter(out %in% out_var) %>% 
    filter(cov %in% cov_var) %>% 
    filter(data_type == "Effect of a one time 10 percentage point attention increase at day 0") %>% 
    mutate(party = gsub("\\.","/",party)) %>% 
    ggplot(aes(x=party,y = pe, ymin = -1, ymax = 5)) +
    geom_hline(yintercept = 0,color = "red")+
    geom_segment(aes(x=party,xend=party,y=lwr,yend=upr,color = party),size=2)+
    geom_point(aes(y=pe),show.legend = F,color="grey") +
    coord_flip()+
    facet_grid(cols = vars(day))+
    labs(x=NULL,y=NULL)+
    scale_color_manual(values = par_col)+
    theme(text = element_text(family  = "noto"),
          axis.text.x = element_text( size =14),
          axis.text.y = element_text( size =14),
          plot.background=element_blank(),
          strip.text = element_text(
            size = 14,face = "bold"),
          legend.position="none",
          #panel.border = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(color = "black", fill = NA, size = 1),
          panel.grid.major = element_line(linetype = 'solid',
                                          colour = "lightgrey"),
          strip.background =element_rect(fill="lightgrey"))
  
  ggsave(glue::glue("plots/{plotname}"),plot,width = 297, height = 90, units = "mm",
         device = cairo_pdf())
  dev.off()  
  
}

# Function to plot party responses
party.response.full = function(datalist,response_of,impulse_of){
  
  dataname = deparse(substitute(datalist))
  type = gsub("irf.","",dataname)
  type = gsub(".7.7","",type)
  
  reponse = response_of %>% 
    gsub("share.","",.)
  impulse = impulse_of %>% 
    gsub("share.","",.)
  
  plotname = glue::glue("{reponse}_reponse_to_{impulse}_{type}_full.pdf")
  
  data = datalist %>% 
    bind_rows() %>% 
    filter(!((out=="share.tv"&cov=="share.print")|(cov=="share.tv"&out=="share.print")))
  
  variables = unique(data$out)
  
  if(response_of == "party"){
    index = which(!unique(data$out) %in% c("share.print","share.tv"))
    out_var = unique(data$out)[index]
  }
  else{
    out_var = response_of 
  }
  if(impulse_of == "party"){
    index = which(!unique(data$out) %in% c("share.print","share.tv"))
    cov_var = unique(data$out)[index]
  }
  else{
    cov_var = impulse_of
  }
  
  plot = data %>% 
    filter(day %in% c(1,2,3)) %>% 
    #filter(!party %in% c("FDP","Linke")) %>% 
    filter(out %in% out_var) %>% 
    filter(cov %in% cov_var) %>% 
    filter(data_type == "Effect of a one time 10 percentage point attention increase at day 0") %>% 
    mutate(party = gsub("\\.","/",party)) %>% 
    ggplot(aes(x=party,y = pe, ymin = -1, ymax = 5)) +
    geom_hline(yintercept = 0,color = "red")+
    geom_segment(aes(x=party,xend=party,y=lwr,yend=upr,color = party),size=2)+
    geom_point(aes(y=pe),show.legend = F,color="grey") +
    coord_flip()+
    facet_grid(cols = vars(day))+
    labs(x=NULL,y=NULL)+
    scale_color_manual(values = par_col)+
    theme(text = element_text(family  = "noto"),
          axis.text.x = element_text( size =14),
          axis.text.y = element_text( size =14),
          plot.background=element_blank(),
          strip.text = element_text(
            size = 14,face = "bold"),
          legend.position="none",
          #panel.border = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(color = "black", fill = NA, size = 1),
          panel.grid.major = element_line(linetype = 'solid',
                                          colour = "lightgrey"),
          strip.background =element_rect(fill="lightgrey"))
  
  ggsave(glue::glue("plots/full/{plotname}"),plot,width = 297, height = 90, units = "mm",
         device = cairo_pdf())
  dev.off()  
  
}
