#####################################################
### FUNCTIONS FOR CALCULATING AND PLOTTING MODELS ###
#####################################################


# Packages
library(tidyverse)
library(vars)
library(boot)
library(rio)
library("mFilter")
library(tseries)
library("TSstudio")
library(stargazer)

#functions to transform
logit_transf = function(x){
  mini = min(x[x > 0])/2
  temp = ifelse(x==0,mini,x)
  result = log(temp / (1-temp))
}

#### IMPULSE RESPONSE FUNCTIONS ####
## LOGIT ##

# Function for processing IRFs
create_irf_data = function(Model,n.ahead){
  irf.total <- irf(Model, n.ahead = n.ahead, cumulative = TRUE)
  variables <- names(irf.total$irf)
  var_irfs <- irf.total
  # - a list with the elements of interest from the IRF object
  elements_to_pull <- c("irf", "Upper", "Lower")
  irf_data <- NULL
  for (el in elements_to_pull) {
    new_irf_info <- irf.total[el][[1]]
    for (out in variables) {
      new_irf_var_data <- as.data.frame(new_irf_info[out][[1]])
      # - take inverse logit to transform the effects to percentage point changes
      new_irf_var_data_transf <- as.data.frame(
        sapply(1:ncol(new_irf_var_data), function(j)
          inv.logit(new_irf_var_data[,j]) - 0.5))
      colnames(new_irf_var_data_transf) <- colnames(new_irf_var_data)
      new_irf_var_data_long <- new_irf_var_data_transf %>%
        gather(cov, value)
      new_irf_var_data_long$out <- out
      new_irf_var_data_long$day <- rep(1:nrow(new_irf_var_data), 
                                       length(unique(new_irf_var_data_long$cov)))
      new_irf_var_data_long$e_type <- el
      irf_data <- rbind(irf_data, new_irf_var_data_long)
    }
  }
  irf_data$e_type <- recode(irf_data$e_type,
                            `irf` = "pe",
                            `Lower` = "lwr", 
                            `Upper` = "upr")
  return(irf_data)
}

cum_shock = function(irf,days){
  new_irf_data <- NULL
  variables <- unique(irf$cov)
  
  # - deciding the number of days to simulate
  DAYS <- days
  
  irf_data <- irf %>%
    filter(day <= (DAYS + 1))
  # - iterating through covariates
  for (covariate in variables) {
    # -iterating through outcomes
    for (outcome in variables) {
      # - skipping when covariate and response are the same
      if (covariate != outcome) {
        # - initializing a cummulative-shocks matrix for this scenario: two 3-dim 
        #     matrix, one matrix for the covariate and one matrix for the response,
        #     and one dimension for the point estimate and the two other dimensions
        #     for the lower and upper bounds of the estimate
        cov_mat <- array(0, dim = c(DAYS, DAYS, 3))
        out_mat <- array(0, dim = c(DAYS, DAYS, 3))
        
        # - pull the full 15-day IRFs for the endogenous covariate
        cov_resp <- irf_data %>%
          filter(cov == covariate, out == covariate) %>%
          # - remove 1st row: it's part of the set-up (repsonse at day 0)
          filter(day != 1) %>%
          mutate(day = day -1)
        
        # - pull the full 15-day IRFs for the particular outcome variable
        out_resp <- irf_data %>%
          filter(cov == covariate, out == outcome) %>%
          # - remove 1st row: it's part of the set-up (repsonse at day 0)
          filter(day != 1) %>%
          mutate(day = day -1)
        
        # - transforming the 15-day IRFs for the covariate and outcome to a wide
        #   3-column format (one column per estimate type: pe, lwr, upr)
        or_cov_resp <- cov_resp %>%
          dplyr::select(day, value, e_type) %>%
          spread(e_type, value) %>%
          dplyr::select(-day)
        
        or_out_resp <- out_resp %>%
          dplyr::select(day, value, e_type) %>%
          spread(e_type, value) %>%
          dplyr::select(-day)
        
        # - fill up the first rows of the scenario matrices with the original 
        #   1-day shock responses
        cov_mat[1,,1:3] <- or_cov_resp %>%
          as.matrix()
        out_mat[1,,1:3] <- or_out_resp %>%
          as.matrix()
        
        for (i in 2:DAYS) {
          # - iterating through the rest of the 15 days, beyond the 1st one
          # - chekcing first how much attention the covariate group is predicted 
          #   to pay to the issue in day i-1
          cov_att_pe <- sum(cov_mat[,(i-1),2])
          
          # - calculating how big a new shock needs to be in order for the 
          #   covariate group to keep its attention to 100%
          cov_new_shock <- 1 - cov_att_pe
          
          # - re-scaling the original 100 percentage point shock to the new shock
          cov_new_resp <- or_cov_resp[1:(DAYS-(i-1)),] * cov_new_shock
          out_new_resp <- or_out_resp[1:(DAYS-(i-1)),] * cov_new_shock
          
          # - adding the response to this new shock to the scenario matrices
          cov_mat[i,i:DAYS,1:3] <- cov_new_resp %>%
            as.matrix()
          out_mat[i,i:DAYS,1:3] <- out_new_resp %>%
            as.matrix()
        }
        # - saving the output for this cov --> out 
        new_rows <- rbind(
          data.frame(
            cov = covariate,
            value = colSums(out_mat[,,1]),
            out = outcome,
            day = 1:DAYS,
            e_type = "lwr",
            data_type = "structural"),
          data.frame(
            cov = covariate,
            value = colSums(out_mat[,,2]),
            out = outcome,
            day = 1:DAYS,
            e_type = "pe",
            data_type = "structural"),
          data.frame(
            cov = covariate,
            value = colSums(out_mat[,,3]),
            out = outcome,
            day = 1:DAYS,
            e_type = "upr",
            data_type = "structural")
        )
        new_irf_data <- rbind(new_irf_data, new_rows)
      }
    }
  }
  return(new_irf_data)
}

# Function to merge IRF data
merge_data = function(cum_irf,irf_data,partyname){
  irf_data = irf_data
  new_irf_data = cum_irf
  party = partyname
  
  # - merging this new type of IRFs with the "regular" 1-time-shock IRFs
  irf_data$data_type <- "one_time_shock"
  irf_data <- irf_data %>%
    # - correct the original data for the fact that day 1 is just pre-setting
    filter(day != 1) %>% 
    mutate(day = day -1)
  
  all_irf_data <- rbind(irf_data, new_irf_data)
  
  # - removing from the dataset cases in which covariate and outcome are the same
  all_irf_data <- all_irf_data %>%
    filter(cov != out)
  
  # - a wide version of the dataset, with a separate column for each estimate type
  all_irf_data_wide <- all_irf_data %>%
    spread(e_type, value)
  
  # - save a copy of this dataset (uncomment to overwrite the file)
  #write.csv(all_irf_data_wide, paste0(data_path, "all_irf_data_wide.csv"),
  #          row.names = FALSE)
  
  # - simulate one-time and structural shocks of 10 instead of 100 percentage pts.
  #   Present the results in 0-100 scale instead of 0-1
  all_irf_data_wide <- all_irf_data %>%
    mutate(value = (value / 10) * 100) %>%
    spread(e_type, value)
  all_irf_data_wide$cov <- recode(all_irf_data_wide$cov,
                                  `print.logit` = "Print Media",
                                  `tv.logit` = "TV Media",
                                  `tweet.logit` =paste0(party," on Twitter"))
  
  all_irf_data_wide$out <- recode(all_irf_data_wide$out,
                                  `print.logit` = "Print Media",
                                  `tv.logit` = "TV Media",
                                  `tweet.logit` = paste0(party," on Twitter"))
  
  # - better labels for the data type
  all_irf_data_wide$data_type <- recode(
    all_irf_data_wide$data_type,
    `one_time_shock` =  "Effect of a one time 10 percentage point attention increase at day 0",
    `structural` = "Effect of a structural 10 percentage point attention increase at day 0")
  
  return(all_irf_data_wide)
} 


# Function to wrap processing

process_irf = function(var_model,partyname,n.ahead,days){
  irf_data = create_irf_data(var_model,n.ahead)
  new_data = cum_shock(irf_data,days)
  #return(new_data)
  full_data = merge_data(new_data,irf_data,partyname)
  return(full_data)
}




# Function to wrap plotting

plot_wrapper = function(data,type,limits){
  result = data %>% 
    filter(data_type==type) %>% 
    ggplot(
      aes(x = day, y = pe, ymin = lwr, ymax = upr)) +
    geom_segment(aes(x=day,xend=day,y=lwr,yend=upr),color="gray")+
    geom_point(aes(y=pe),show.legend = F,size =4) +
    geom_line(aes(y=pe),show.legend = F,linetype="dotted")+
    geom_segment(aes(x=day-0.1,xend=day+0.1,y=upr,yend=upr),color="darkgrey")+
    geom_segment(aes(x=day-0.1,xend=day+0.1,y=lwr,yend=lwr),color="darkgrey")+
    geom_hline(yintercept = 0,color = "red")+
    facet_grid(cols = vars(cov),rows = vars(out))+
    ylim(limits[1],limits[2])+
    theme(
      panel.spacing = unit(1.05, "lines"),
      legend.position = "bottom",
      panel.background = element_blank(),
      panel.grid.major = element_line(colour = "gray90", linetype = "solid"),
      axis.text = element_text(size = 18),
      axis.text.y = element_text(hjust=0),
      strip.text = element_text(size = 20),
      panel.border = element_rect(colour = "black", fill = FALSE),
      strip.background = element_rect(colour = "black"),
      axis.title = element_text(size = 16),
      legend.text = element_text(size = 16)
    )
  return(result)
}

plot_results_onetime = function(data,n.ahead,lags,party,simulate_days){
  limits = c(-5,5)
  type = "Effect of a one time 10 percentage point attention increase at day 0"
  str = "onetime"
  png(glue::glue("data/plots/onetime/{n.ahead}day_{lags}lags_{party}_simulate{simulate_days}_{str}.png"), width = 1600, height = 700)
  print(plot_wrapper(data,type,limits))
  dev.off()  
}
plot_results_structural = function(data,n.ahead,lags,party,simulate_days){
  limits = c(-12,12)
  type = "Effect of a structural 10 percentage point attention increase at day 0"
  str = "structural"
  png(glue::glue("data/plots/structural/{n.ahead}day_{lags}lags_{party}_simulate{simulate_days}_{str}.png"), width = 1600, height = 700)
  print(plot_wrapper(data,type,limits))
  dev.off()  
}



plot_all = function(datalist,name){
  string = name
  n.ahead = str_extract(string, "(?<=day)\\d*(?=\\_)")
  lags = str_extract(string, "(?<=lag)\\d*(?=\\_)")
  simulate_days = str_extract(string, "(?<=simulate)\\d*(?=\\b)")
  lapply(seq_along(datalist),function(x){
    name = str_extract(names(datalist[x]),"(?<=\\.).*(?=\\_)")
    plot_results_onetime(datalist[[x]],
                         n.ahead=n.ahead,
                         lags=lags,
                         party = name ,simulate_days)
    plot_results_structural(datalist[[x]],
                            n.ahead=n.ahead,
                            lags=lags,
                            party = name ,simulate_days)
  })
}


# Function wrapper for all functions defined above

calc_and_plot = function(modellist,n.ahead,simulate_days){
  string = deparse(substitute(modellist))
  lags = as.numeric(str_extract(string,"\\d"))
  temp = lapply(seq_along(modellist),function(x){
    res = process_irf(modellist[[x]],names(modellist[x]),n.ahead = n.ahead,days = simulate_days)
    return(res)
  })
  names(temp)=names(modellist)
  name = glue::glue("day{n.ahead}_lag{lags}_simulate{simulate_days}")
  plot_all(temp,name)
  return(temp)
}

# Function to produce stargazer plot as file

mod_stargazer <- function(output.file, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=output.file, append=TRUE)
}




