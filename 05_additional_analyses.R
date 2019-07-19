source("00_data_prep.R")

library(stargazer)
library(lme4)


### ATTEMPT TO MEASURE EFFECT OF GINI CENTRALIZATION
### AND 
### RELATIVE EFFECT OF TWO CENTRALIZATIONS IN DISCUSSION

gini_mod1 = lme4::glmer(mu_improved=="Improved" ~ 
                          prop_toward +
                          alpha_cor +
                          gini_chat*prop_toward + 
                          (1 | trial)
                        , data=gurc_aggreg
                        , family="binomial"
                        #, control=lme4::glmerControl(optimizer="bobyqa")
)


gini_mod2 = lme4::glmer(mu_improved=="Improved" ~ 
                          prop_toward +
                          alpha_cor+
                          gini_alpha+
                          gini_chat +
                          gini_alpha*prop_toward +
                          gini_chat*prop_toward + 
                          (1 | trial)
                        , data=gurc_aggreg
                        , family="binomial"
                        #, control=lme4::glmerControl(optimizer="bobyqa")
)

gini_mod3 = lme4::glmer(mu_improved=="Improved" ~ 
                          prop_toward +
                          alpha_cor+
                          gini_alpha +
                          #gini_chat*prop_toward + 
                          gini_alpha*prop_toward +
                          (1 | trial)
                        , data=all_aggreg %>% subset(communication=="Numeric")
                        , family="binomial"
                        #, control=lme4::glmerControl(optimizer="bobyqa")
)


stargazer(gini_mod1, gini_mod2, gini_mod3
          , type="html", out="Figures/gini_interactions.html"
          , dep.var.caption ="Outcome: Group Improved" 
          , dep.var.labels = ""
          , column.labels=c("Discussion","Numeric Exchange")
          , column.separate=c(2,1)
          , star.cutoffs=c(0.05,0.01,0.001)
          , omit="group_number"
)


### covariance matrices

Hmisc::rcorr(subset(all_aggreg, communication=="Numeric") %>% select(prop_toward,alpha_cor,gini_alpha) %>%
               as.matrix)


Hmisc::rcorr(gurc_aggreg %>% select(prop_toward,alpha_cor,gini_alpha, gini_chat) %>%
               as.matrix)


sjPlot::sjt.corr(subset(all_aggreg, communication=="Numeric") %>% select(prop_toward,alpha_cor,gini_alpha)
                 , file="Figures/numeric_corr.html"
                 , digits=2
                 , fade.ns=F)

sjPlot::sjt.corr(gurc_aggreg %>% select(prop_toward,alpha_cor,gini_alpha, gini_chat)
         , file="Figures/disc_corr.html"
         , digits=2
         ,fade.ns=F)
