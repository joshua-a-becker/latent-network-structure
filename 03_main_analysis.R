source("00_data_prep.R")

library(stargazer)
library(lme4)

all_aggreg$communication=factor(all_aggreg$communication, levels=c("Numeric","Discussion"))


### BASIC TEST

basic_mod1 = lme4::glmer(mu_improved=="Improved" ~ prop_toward+
                   (1 | trial)
                 , data=all_aggreg %>% subset(communication=="Discussion")
            , family="binomial"
                 #, control=lme4::glmerControl(optimizer="bobyqa")
)

basic_mod2 = lme4::glmer(mu_improved=="Improved" ~ prop_toward+alpha_cor +
              (1 | trial)
            , data=all_aggreg %>% subset(communication=="Discussion")
            , family="binomial"
            #, control=lme4::glmerControl(optimizer="bobyqa")
)


basic_mod3 = lme4::glmer(mu_improved=="Improved" ~ prop_toward+
              (1 | trial)
            , data=all_aggreg %>% subset(communication=="Numeric")
            , family="binomial"
            #, control=lme4::glmerControl(optimizer="bobyqa")
)


basic_mod4 = lme4::glmer(mu_improved=="Improved" ~ prop_toward + alpha_cor +
              (1 | trial)
            , data=all_aggreg %>% subset(communication=="Numeric")
            , family="binomial"
            #, control=lme4::glmerControl(optimizer="bobyqa")
)


stargazer(basic_mod1, basic_mod2, basic_mod3, basic_mod4
          , out="Figures/basic_test.html", type="html"
          , column.labels=c("Discussion","Numeric Exchange")
          , column.separate=c(2,2)
          , dep.var.labels.include=F
          , dep.var.caption="Binomial outcome:  mean closer to truth"
          , covariate.labels = c("<p>&phi;</p>","Stub./Acc. Corr.")
          )



### TWO SAMPLE COMPARISONS


lme4::glmer(mu_improved=="Improved" ~ majority_toward +
              (1 | trial)
            , data=all_aggreg %>% subset(communication=="Discussion" & prop_toward!=0.5) 
            , family="binomial"
            #, control=lme4::glmerControl(optimizer="bobyqa")
) %>% summary


lme4::glmer(mu_improved=="Improved" ~ communication+alpha_cor+
              (1 | trial)
            , data=all_aggreg %>% subset(prop_toward>0.5) 
            , family="binomial"
            #, control=lme4::glmerControl(optimizer="bobyqa")
) %>% summary

lme4::glmer(mu_improved=="Improved" ~ communication+alpha_cor+
              (1 | trial)
            , data=all_aggreg %>% subset(prop_toward>0.75) 
            , family="binomial"
            #, control=lme4::glmerControl(optimizer="bobyqa")
) %>% summary

lme4::glmer(mu_improved=="Improved" ~ communication+alpha_cor+
              (1 | trial)
            , data=all_aggreg %>% subset(prop_toward<0.5) 
            , family="binomial"
            #, control=lme4::glmerControl(optimizer="bobyqa")
) %>% summary





with(all_aggreg %>% subset(prop_toward>0.5),
     table(improve, communication)
) %>%
  prop.table(2)

with(all_aggreg %>% subset(prop_toward>0.75),
     table(improve, communication)
) %>%
  #prop.test
  prop.table(2) 


with(all_aggreg %>% subset(prop_toward<0.5),
     table(improve, communication)
) %>%
  prop.table(2) 





