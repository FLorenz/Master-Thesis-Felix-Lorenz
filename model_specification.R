####################################
### ESTIMATE MODEL, PLOT RESULTS ###
####################################

# Data
db.impute = readRDS("data/db.impute.rds")
db.monday = readRDS("data/db.monday.rds")

# Packages
library(xtable)
library(here)
source(here("functions_model_creation.R"))
library(showtext)

# Font for plots
#font_add_google("Noto Serif","noto")
showtext_auto()
##########################
### TRANSFORM THE DATA ###
##########################

db.impute.logit = lapply(db.impute[c(2:9)],logit_transf) %>% 
  data.frame()
db.impute.logit$created_at = db.impute$created_at

db.monday.logit = lapply(db.monday[c(2:9)],logit_transf) %>% 
  data.frame()
db.monday.logit$created_at = db.monday$created_at

#################################
### CREATE LIST OF DATAFRAMES ###
#################################

db.impute.list = lapply(seq_along(db.impute.logit[1:6]),function(k){
  df = db.impute.logit
  temp = df[,c(7,8,k)]
  return(temp)
  print(names(db.impute.logit)[k])
})

db.monday.list = lapply(seq_along(db.monday.logit[1:6]),function(k){
  df = db.monday.logit
  temp = df[,c(7,8,k)]
  return(temp)
  print(names(db.impute.logit)[k])
})

####################
### STATIONARITY ###
####################

dickey_fuller.impute =lapply(db.impute.logit[1:8], function(x){
  temp = adf.test(x,k=1)
  
})
dickey_fuller.monday =lapply(db.monday.logit[1:8], function(x){
  temp = adf.test(x,k=1)
  
})


####################
### TABLE OUTPUT ###
####################


names_vec = c("SPD","Linke","Gruene","FDP","AfD","CDU/CSU","TV News","Print Media")

# EXTRACT  DICKEY FULLER
table_output = function(datalist,names_vec){
  #extract data
  dickey_value =lapply(datalist, function(x){
    return(x["statistic"])
  }) %>% unlist() 
  dickey_p =lapply(datalist, function(x){
    return(x["p.value"])
  }) %>% unlist()
  temp = data.frame(names_vec,dickey_value,dickey_p)
  
  temp = temp %>% 
    mutate(dickey_value = round(dickey_value,3)) %>% 
    mutate(dickey_value = ifelse(dickey_p<=0.01,paste0(dickey_value,"***"),
                                 ifelse(dickey_p<=0.05,paste0(dickey_value,"**"),dickey_value)),
           dickey_p = ifelse(dickey_p<=0.01,paste0("< ",dickey_p),round(dickey_p,3)))
  
  #build structure
  res <-structure(c(temp$dickey_value,temp$dickey_p), 
                  .Dim = c(nrow(temp), 2), 
                  .Dimnames = list(names_vec,
                                   c("Dickey Fuller", "p-Value")))
  return(res)
}

impute = table_output(dickey_fuller.impute,names_vec)
monday = table_output(dickey_fuller.monday,names_vec)

stargazer(monday,title = "Augmented Dickey-Fuller Test", 
          colnames = TRUE, 
          notes = "Sig. Levels: ${}^{***} p < .01$, ${}^{**} p < .05$, ${}^{*} p < .1$")



mod_stargazer("tables/table-1.tex",impute,type = "latex",
              title = "Augmented Dickey-Fuller Test", 
              colnames = TRUE, 
              notes = "Sig. Levels: ${}^{***} p < .01$, ${}^{**} p < .05$, ${}^{*} p < .1$")

mod_stargazer("tables/table-4.tex",monday,type = "latex",
              title = "Augmented Dickey-Fuller Test", 
              colnames = TRUE, 
              notes = "Sig. Levels: ${}^{***} p < .01$, ${}^{**} p < .05$, ${}^{*} p < .1$")



#####################
### LAG STRUCTURE ###
#####################

lag.impute =lapply(db.impute.list,function(x){
  lagselect <- VARselect(x, lag.max = 3, type = "const")
  return(lagselect)
  
})

lag.monday =lapply(db.monday.list,function(x){
  lagselect <- VARselect(x, lag.max = 3, type = "const")
  return(lagselect)
  
})
names_vec = c("SPD","Linke","Gruene","FDP","AfD","CDU/CSU")

create_lag_dataframe = function(datalist,names_vec){
  f=lapply(seq_along(datalist),function(d){
    temp = datalist[[d]]["criteria"]  %>% 
      data.frame()
    names(temp) = c("P=1","P=2","P=3")
    temp = temp %>% 
      t() %>% 
      data.frame()
    temp = temp %>% 
      dplyr::select(AIC.n.,HQ.n.,FPE.n.) 
    temp$party = names_vec[d]
    temp$p= rownames(temp)
    rownames(temp)=NULL
    temp=temp %>% 
      pivot_wider(id_cols = party,values_from =c(AIC.n.:FPE.n.),names_from = p,names_sort = T )
    return(temp)
  }) %>% bind_rows()
  
  return(f)
}
impute.lag.df = create_lag_dataframe(lag.impute,names_vec = names_vec)
monday.lag.df = create_lag_dataframe(lag.monday,names_vec = names_vec)

monday.lag.df =monday.lag.df %>% 
  mutate_if(is.numeric,function(x){
    round(x,3)
  })

rownames(monday.lag.df)=NULL

impute.lag.df =impute.lag.df %>% 
  mutate_if(is.numeric,function(x){
    round(x,3)
  })

rownames(impute.lag.df)=NULL


mod_stargazer(output.file = "tables/table-5.tex",monday.lag.df,summary=F)
mod_stargazer(output.file = "tables/table-2.tex",impute.lag.df,summary=F)


##########################
### ESTIMATE THE MODEL ###
##########################

model.monday.lag1 = lapply(db.monday.list,function(x){
  temp = VAR(x,lag.max = 1,season = NULL,type = "none",ic = "AIC")
  return(temp)
})


model.impute.lag1 = lapply(db.impute.list,function(x){
  temp = VAR(x,lag.max = 1,season = NULL,type = "none",ic = "AIC")
  return(temp)
})


# TABLE OF VAR OUTPUT
var_table = function(datalist,type){
  lapply(seq_along(datalist),function(i){
    names = names(datalist[[i]][["varresult"]])
    party = names[3] %>% 
      gsub("share\\.","",.) %>% 
      gsub("\\.","/",.)
    t=mod_stargazer(output.file = glue::glue("tables/var_{names[3]}_{type}.tex"),
                    datalist[[i]][["varresult"]][1],
                    datalist[[i]][["varresult"]][2],
                    datalist[[i]][["varresult"]][3],
                    out="latex",
                    covariate.labels = c("TV News","Print Media",party),
                    column.labels = c("TV News","Print Media",party),
                    model.numbers	= FALSE,
                    dep.var.caption = NULL,
                    dep.var.labels.include = FALSE,
                    title = glue::glue("Raw VAR results for {party} model"))
    return(t)
  })
}
var_table(model.impute.lag1,"impute")
var_table(model.monday.lag1,"monday")

###################################
### AUTOCORRELATION/SERIAL TEST ###
###################################
serial.impute.lag1 = lapply(model.impute.lag1,function(x){
  temp = serial.test(x,lags.pt = 1, type = "PT.asymptotic")
  return(temp)
})


serial.monday.lag1 = lapply(model.monday.lag1,function(x){
  temp = serial.test(x,lags.pt = 1, type = "PT.asymptotic")
  return(temp)
})

table_output_ac = function(datalist,names_vec){
  #extract data
  ac_value =lapply(datalist, function(x){
    return(x[["serial"]]["statistic"])
  }) %>% unlist() 
  ac_p =lapply(datalist, function(x){
    return(x[["serial"]]["p.value"])
  }) %>% unlist()
  temp = data.frame(names_vec,ac_value,ac_p)
  
  temp = temp %>% 
    mutate(ac_value = round(ac_value,3)) %>% 
    mutate(ac_value = ifelse(ac_p<=0.01,paste0(ac_value,"***"),
                             ifelse(ac_p<=0.05,paste0(ac_value,"**"),ac_value)),
           ac_p = ifelse(ac_p<=0.01,paste0("< ",ac_p),round(ac_p,3)))
  
  #build structure
  res <-structure(c(temp$ac_value), 
                  .Dim = c(nrow(temp), 1), 
                  .Dimnames = list(names_vec,
                                   c("Chi-squared")))
  return(res)
}

impute = table_output_ac(serial.impute.lag1,names_vec)
monday = table_output_ac(serial.monday.lag1,names_vec)

mod_stargazer("tables/table-3.tex",impute,type = "latex",
              title = "Portmanteau Test (asymptotic)", 
              colnames = TRUE, 
              notes = "Sig. Levels: ${}^{***} p < .01$, ${}^{**} p < .05$, ${}^{*} p < .1$")

mod_stargazer("tables/table-6.tex",monday,type = "latex",
              title = "Portmanteau Test (asymptotic)", 
              colnames = TRUE, 
              notes = "Sig. Levels: ${}^{***} p < .01$, ${}^{**} p < .05$, ${}^{*} p < .1$")



#########################
### GRANGER CAUSALITY ###
#########################
granger.impute = lapply(model.impute.lag1,function(x){
  y_names = x[["y"]] %>% colnames()
  cause.tv = causality(x, cause = y_names[1])
  cause.print = causality(x, cause = y_names[2])
  cause.tweet = causality(x, cause = y_names[3])
  
  result = list(cause.tv,cause.print,cause.tweet)
  return(result)
})

granger.monday = lapply(model.monday.lag1,function(x){
  y_names = x[["y"]] %>% colnames()
  cause.tv = causality(x, cause = y_names[1])
  cause.print = causality(x, cause = y_names[2])
  cause.tweet = causality(x, cause = y_names[3])
  
  result = list(cause.tv,cause.print,cause.tweet)
  return(result)
})
granger.monday %>% unlist()
granger.impute %>% unlist()

# No Granger Causality at all

#####################
### CALC AND PLOT ###
#####################

irf.impute.7.7 = lapply(model.impute.lag1, function(x){
  y_names = x[["y"]] %>% colnames()
  partyname = y_names[3]
  partyname = gsub("share\\.","",partyname)
  temp = process_irf(x,partyname,7,7)
  temp$party = partyname
  return(temp)
})

irf.monday.7.7 = lapply(model.monday.lag1, function(x){
  y_names = x[["y"]] %>% colnames()
  partyname = y_names[3]
  partyname = gsub("share\\.","",partyname)
  temp = process_irf(x,partyname,7,7)
  temp$party = partyname
  return(temp)
})

###############################################
### HOW THE MEDIA IS INFLUENCING EACH OTHER ###
###############################################

###################
### IMPUTE DATA ###
###################
here()
source(here("plot_irf.R"))
media.response(irf.impute.7.7, response_of = "share.tv")
media.response(irf.impute.7.7, response_of = "share.print")


media.response(irf.monday.7.7, response_of = "share.tv")
media.response(irf.monday.7.7, response_of = "share.print")

#######################
### PARTY RESPONSES ###
#######################

party.response(irf.impute.7.7,response_of = "party",impulse_of = "share.print")
party.response(irf.impute.7.7,response_of = "party",impulse_of = "share.tv")

party.response(irf.impute.7.7,response_of = "share.print",impulse_of = "party")
party.response(irf.impute.7.7,response_of = "share.tv",impulse_of = "party")

party.response(irf.monday.7.7,response_of = "party",impulse_of = "share.print")
party.response(irf.monday.7.7,response_of = "party",impulse_of = "share.tv")

party.response(irf.monday.7.7,response_of = "share.print",impulse_of = "party")
party.response(irf.monday.7.7,response_of = "share.tv",impulse_of = "party")


