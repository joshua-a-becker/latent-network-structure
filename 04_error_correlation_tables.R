source("00_data_prep.R")



gurc_d_alpha = 
  gurc_d_chat %>%
  subset(is.finite(alpha_rev)) %>%
  ungroup %>%
  mutate(
    stubborn = alpha_rev>median(alpha_rev)
    , count_center = count_chat>median(count_chat)
    , group_number = factor(group_number.x)
    , stay_put=pre_influence==post_influence
    , alpha_quant = cut(alpha_rev,
                        breaks=quantile(alpha_rev), include.lowest=T) %>% as.numeric
    , count_quant = cut(count_chat,
                        breaks=quantile(count_chat), include.lowest=T) %>% as.numeric
  )

d_delphi_alpha = d_delphi %>%
  subset(is.finite(alpha_rev)) %>% 
  ungroup %>%
  mutate(
    stubborn = alpha_rev>median(alpha_rev)
    , stay_put=pre_influence==post_influence
    , alpha_quant = cut(alpha_rev,
                        breaks=quantile(alpha_rev), include.lowest=T) %>% as.numeric
  )


mod.delphi=  lmer(log(abs(err_pre/truth)+0.01) ~ alpha_quant +
                    (1 | trial)
                  , data=d_delphi_alpha
) 


mod.discussion1=lmer(log(abs(err_pre/truth)+0.01) ~ count_quant +
                       (1 | group_number)
                     , data=gurc_d_alpha
)

mod.discussion2=lmer(log(abs(err_pre/truth)+0.01) ~ alpha_quant +
                       (1 | group_number)
                     , data=gurc_d_alpha
)

mod.discussion3=lmer(log(abs(err_pre/truth)+0.01) ~ alpha_quant + count_quant +
                       (1 | group_number)
                     , data=gurc_d_alpha
)




stargazer(mod.discussion1, mod.discussion2, mod.discussion3, mod.delphi
          ,type="html"
          ,out="Figures/error_correlation.html"
          , dep.var.caption = "% Error (Logged)"
          , dep.var.labels=""
          #, covariate.labels=c("Stubbornness","Talkative","Intercept")
          , star.cutoffs=c(0.05,0.01,0.001)
          , column.labels=c("Discussion","Delphi")
          , column.separate=c(3,1)
          , model.numbers=F
          , omit="group|trial"
          , digits=2
)
