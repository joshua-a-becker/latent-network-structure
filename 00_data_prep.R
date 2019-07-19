rm(list=ls());gc()
require(DescTools)
require(tidyverse)
library(readxl)
library(httr)
library(ggplot2)

### DOWNLOAD DELPHI DATA

lorenz_url = "http://www.pnas.org/highwire/filestream/606236/field_highwire_adjunct_files/1/sd01.xls"
if(!file.exists("Data/lorenz_et_al.xls")) {
  GET(lorenz_url, write_disk(tf <- "Data/lorenz_et_al.xls", overwrite=T))  
}
lorenz2011 <- read_excel("Data/lorenz_et_al.xls") %>%
  mutate(
    pre_influence = E1
    , post_influence = E5
    , dataset="lorenz2011"
    , truth=Truth
    , trial= paste0(Information_Condition, Session_Date)
    , question=Question
    , network= fct_recode(Information_Condition, "Decentralized" = "full", "Solo" = "no", "Decentralized"="aggregated")  
    , communication="Numeric"
  ) %>% 
  subset(network=="Decentralized") %>%
  group_by(trial, question) %>%
  mutate(
    soc_info = mean(pre_influence, na.rm=T)
  )


becker2017 = read.csv(url("http://www.pnas.org/highwire/filestream/30360/field_highwire_adjunct_files/1/pnas.1615978114.sd01.csv")
                      , stringsAsFactors=F) %>%
  mutate(
    trial=group_number
    , dataset="becker2017"
    , pre_influence=response_1
    , post_influence=response_3
    , communication="Numeric"
    , question=task
  ) %>% 
  subset(network=="Decentralized") %>%
  group_by(trial, question) %>%
  mutate(
    soc_info = mean.neighbor.time1
  )
  

becker2019 = read.csv(url("https://raw.githubusercontent.com/joshua-a-becker/wisdom-of-partisan-crowds/master/Becker%20Centola%20Porter%20-%20Wisdom%20of%20Partisan%20Crowds%20-%20Supplementary%20Dataset.csv")) %>%
  mutate(
    trial=paste0(set,pair_id,network,experiment,party)
    , dataset="becker2019"
    , pre_influence=response_1
    , post_influence=response_3
    , question=q
    , network= fct_recode(network, "Decentralized" = "Social", "Solo" = "Control")  
    , communication="Numeric"
  ) %>% 
  subset(network=="Decentralized") %>%
  group_by(question, trial) %>%
  mutate(
    soc_info = mean(pre_influence, na.rm=T)
  )



#### DOWNLOAD GURCAY DATA
question_lookup = read.csv("Data/question_lookup.csv", stringsAsFactors=F, header=F,fileEncoding="UTF-8-BOM") %>%
  `colnames<-`(c("question","true.values")) %>%
  mutate(
    question=tolower(question)
    ,true.values=round(true.values,2)
  )

chats=read.csv("Data/chatlog.csv", stringsAsFactors=F) %>%
  mutate(
    subject.no = as.numeric(sapply(strsplit(Rs,";"), "[", 1))
    , Qs=tolower(Qs)
    , question = unlist(lapply(Qs, FUN=function(x){names(which(sapply(question_lookup$question, grepl, x)))}))
  )


gurc_d <- read.csv("Data/GURCAY_et_al_newDataApr30.csv") %>% 
  group_by(question.no, group) %>% 
  mutate(
    valid = !is.na(est1) & !is.na(est2)
    , pre_influence = est1
    , post_influence=est2
  ) %>% 
  subset(valid) %>% 
  mutate(
    mu1 = mean(est1, na.rm=T)
    , med1 = median(est1, na.rm=T)
    , alpha_raw = (abs(pre_influence-mu1)-abs(post_influence-mu1)) / abs(pre_influence-mu1)
    , alpha_med = (abs(pre_influence-med1)-abs(post_influence-med1)) / abs(pre_influence-med1)
    , alpha = ifelse(alpha_raw<0, 0, alpha_raw)
    , alpha = ifelse(alpha_raw>1, 1, alpha_raw)
    , alpha_rev = 1-alpha
    , toward_truth = ifelse((est1 < mean(est1) & mu1 <= true.values) | (est1 > mu1 & mu1 >= true.values), "Away","Toward")
    , group_number = group
    , true.values = round(true.values, 2)
    , truth=true.values
  ) %>%
  merge(question_lookup, by="true.values") %>%
  mutate(
      err_pre = abs(pre_influence - truth)
    , err_post = abs(post_influence - truth)
    , improve = err_pre > err_post
    , err_norm = abs(err_pre/truth)
    , trial=group_number
  )

chat_sum_individ = chats %>%
  group_by(subject.no, question) %>%
  summarize(
    count_chat = length(Rs)
  ) %>%
  rowwise %>%
  subset(subject.no %in% gurc_d$subject.no) %>%
  mutate(
    group_number = unique(gurc_d$group[gurc_d$subject.no==subject.no])
  )

chat_sum = chat_sum_individ %>%
  group_by(group_number, question) %>%
  summarize(
    gini_chat = Gini(count_chat)
  )

gurc_d_chat = merge(
    gurc_d 
  , chat_sum_individ
  , by=c("subject.no","question")
) %>% mutate(
  err1 = abs(est1-true.values)
  , communication="Discussion"
)


gurc_aggreg = gurc_d %>% 
  ### retain only people who answered both times
  ### retain only social conditions
  subset(valid & condition!="C") %>%
  group_by(condition, question, group_number,trial) %>%
  summarize(
      N = length(est1)
    , truth=unique(true.values)
    , gini_alpha = Gini(alpha_rev[is.finite(alpha_rev)], na.rm=T)
    , gini_alpha_trunc = DescTools::Gini(alpha_rev[is.finite(alpha_rev) & alpha_rev<=1], na.rm=T)
    , mu1 = mean(est1/truth)
    , mu2 = mean(est2/truth)
    , med1 = median(est1)
    , err_mu1 = abs(mu1 - 1)
    , err_mu2 = abs(mu2 - 1)
    
    ### change individ err
    , improve_ind = mean(improve)
    
    ### change in error of mean
    , change_mu = abs(mu2-mu1)/truth
    , change_err_mu = (err_mu2 - err_mu1)/truth
    
    ###
    , majority_away_truth = ifelse((med1 < mu1 & mu1 <= truth) | (med1 > mu1 & mu1 >= truth), "Away","Toward")
    , prop_away_truth = mean(toward_truth=="Away")
    , prop_away_truth_round=round(prop_away_truth,1)
    , prop_toward = 1-prop_away_truth
    ### did the mean improve?
    , mu_improved = ifelse(change_err_mu<0, "Improved", "Worse")
    , dataset="gurcay"
    
    ### correlations
    , alpha_cor = cor.test(alpha_rev[is.finite(alpha_rev)], err_norm[is.finite(alpha_rev)], na.rm=T)$estimate
    , pct_move = mean(pre_influence!=post_influence)
    , pct_reactionary = mean(alpha_raw<0)
  ) %>%
  merge(chat_sum, by=c("question","group_number")) %>%
  mutate(
      prop_toward = 1-prop_away_truth
    , communication="Discussion"
  )


cols=c("pre_influence","post_influence","truth","question","trial","network","dataset", "communication","soc_info")

d_delphi = rbind(
  becker2017[,cols] %>% as.data.frame
  , lorenz2011[,cols] %>% as.data.frame
  , becker2019[,cols] %>% as.data.frame
)  %>%
  group_by(trial, question, dataset, communication) %>%
  mutate(
      mu1 = mean(pre_influence, na.rm=T)
    , med1 = median(pre_influence, na.rm=T)
    , err_pre = abs(pre_influence - truth)
    , err_post = abs(post_influence - truth)
    , improve = err_pre > err_post
  ) %>%
  subset(!is.na(pre_influence) & !is.na(post_influence)) %>%
  mutate(
      alpha_raw = (abs(pre_influence-soc_info)-abs(post_influence-soc_info)) / abs(pre_influence-mu1)
    #, alpha_raw = ifelse(pre_influence==post_influence, 0, alpha_raw)
    , alpha = ifelse(alpha_raw<0, 0, alpha_raw)
    , alpha = ifelse(alpha_raw>1, 1, alpha_raw)
    , alpha_rev = 1-alpha
    , mu1 = mean(pre_influence)
    , toward_truth = ifelse((pre_influence < mean(pre_influence) & mu1 <= truth) | (pre_influence > mu1 & mu1 >= truth), "Away","Toward")
    , err_norm = abs(err_pre/truth)
  )

delph_aggreg = d_delphi %>%
  group_by(trial, question, dataset, communication) %>%
  summarize(
      gini_alpha = DescTools::Gini(alpha_rev[is.finite(alpha_rev)], na.rm=T)
    , gini_alpha_trunc = DescTools::Gini(alpha_rev[is.finite(alpha_rev) & alpha_rev<=1], na.rm=T)
    , truth=unique(truth)
    
    ### change individ err
    , improve_ind = mean(improve)
    
    ,N=length(pre_influence)
    
    ## calc mean
    , mu1 = mean(pre_influence)
    , mu2 = mean(post_influence)
    
    , med1=median(pre_influence)
    
    ## error of mean
    , err_mu1 = abs(mu1 - truth)
    , err_mu2 = abs(mu2 - truth)
    , change_err_mu = mean(err_mu2 - err_mu1)/truth
    , mu_improved = ifelse(change_err_mu<0, "Improved","Worse")
    
    ## organizing stats
    , majority_away_truth = ifelse((med1 < mu1 & mu1 <= truth) | (med1 > mu1 & mu1 >= truth), "Away","Toward")
    , prop_toward = mean(toward_truth=="Toward")
    , pct_move = mean(pre_influence!=post_influence)
    , pct_reactionary = mean(alpha_raw<0)
    ### correlations
    , alpha_cor = cor.test(alpha_rev[is.finite(alpha_rev)], err_norm[is.finite(alpha_rev)], na.rm=T)$estimate
    
  ) %>%
  mutate(
    prop_toward_round=round(prop_toward,1)
    , improve=(change_err_mu<0)*1
  )


cols=c("N","mu_improved","gini_alpha","question","trial","improve_ind"
       , "mu1","mu2","truth", "change_err_mu", "err_mu1"
       , "communication","prop_toward","dataset","alpha_cor"
       , "pct_move", "pct_reactionary", "gini_alpha_trunc")

all_aggreg = rbind(
  gurc_aggreg %>% 
    select(cols) %>% data.frame
  , delph_aggreg[,cols] %>% data.frame
) %>%
  group_by(communication) %>%
  mutate(
    gini_alpha_quantile = ifelse(gini_alpha>median(gini_alpha, na.rm=T), "High","Low") %>% factor(levels=c("Low","High"))
    , gini_alpha_quantile_trunc = ifelse(gini_alpha_trunc>median(gini_alpha_trunc, na.rm=T), "High","Low") %>% factor(levels=c("Low","High"))
    , majority_toward = ifelse(prop_toward>0.5, "Majority Toward","Majority Away")
    , improve_amt = change_err_mu/truth
    , err_mu1_pct = err_mu1/truth
    , mu_toward_truth =(mu1 < truth & mu2 > mu1) | (mu1 > truth & mu2 < mu1)
    , improve=mu_improved=="Improved"
  ) %>%
  ungroup


### convenient for plotting
nice_theme = function() {
  theme_test() +
    theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1.2)))
}
