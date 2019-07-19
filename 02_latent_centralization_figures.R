source("00_data_prep.R")



ag_sum = all_aggreg %>% 
  subset(prop_toward!=0.5) %>%
  mutate(
    #gini_alpha_quantile = cut(gini_alpha, quantile(gini_alpha))
    pct_reactionary = round(pct_reactionary,1)
  ) %>% 
  group_by(communication, majority_toward, gini_alpha_quantile) %>%
  summarize(
     N = length(mu_improved)
     , improve=mean(improve)
    , improve_amt = mean(improve_amt)
    , mu_toward_truth = mean(mu_toward_truth)
    , alpha_cor = mean(alpha_cor, na.rm=T)
  )

ag_sum %>% 
  ggplot(aes(x=gini_alpha_quantile
                   , y=improve
             , color=majority_toward, group=majority_toward
             )) +
  scale_y_continuous(lim=c(0,1))+
  geom_hline(yintercept=0.5, linetype="dashed")+
  facet_wrap(.~communication, scales="free")+
  geom_point(size=3)+
  geom_line(size=1.5)+
  labs(y="Probability of Improving", x="Latent Centralization\n(Gini - Stubbornness)"
  , color="") +
  nice_theme()

ggsave("Figures/Evidence for Latent Network Structure - Stubbornness.png", width=5, height=2.75, dpi=300)  


ag_sum %>% 
  ggplot(aes(x=gini_alpha_quantile
             , y=alpha_cor
             , color=majority_toward, group=majority_toward
  )) +
  scale_y_continuous(lim=c(-1,1))+
  geom_hline(yintercept=0, linetype="dashed")+
  facet_wrap(.~communication, scales="free")+
  geom_point(size=3)+
  geom_line(size=1.5)+
  labs(y="Accuracy/Stubbornness Corr.", x="Latent Centralization\n(Gini - Stubbornness)"
       , color="") +
  nice_theme()

ggsave("Figures/collinearities.png", width=5, height=2.75, dpi=300)  


gurc_aggreg %>%
  mutate(
    gini_chat_quantile = ifelse(gini_chat>median(gini_chat, na.rm=T), "High","Low")
    , majority_toward = ifelse(prop_toward>0.5, "Majority Toward","Majority Away")
  ) %>%
  ungroup %>%
  subset(prop_toward!=0.5) %>%
  group_by(majority_toward, gini_chat_quantile, communication) %>%
  summarize(
    improve = mean(mu_improved=="Improved")
  ) %>%
  ungroup %>%
  mutate(
    gini_chat_quantile=factor(gini_chat_quantile, levels=c("Low","High"))
  ) %>%
  ggplot(aes(x=gini_chat_quantile, y=improve
                   , color=majority_toward, group=majority_toward
             )) +
  scale_y_continuous(lim=c(0,1))+
  stat_summary(fun.y="mean", geom="point", size=3)+
  stat_summary(fun.y="mean", geom="line", size=1.5)+
  facet_wrap(.~communication)+
  labs(y="Probability of Improving", x="Latent Centralization\n(Gini - Talkativeness)"
       , color="") +
  theme_test() +
  geom_hline(yintercept=0.5, linetype="dashed") + 
  theme(strip.background=element_rect(fill="white", color=NA),
        strip.text=element_text(face="bold", size=rel(1.2)))


ggsave("Figures/Evidence for Latent Network Structure - Talkativeness.png", width=3.5, height=2.75, dpi=300)  


d_delphi %>%
  ungroup %>%
  mutate(
    alpha_rev=cut(alpha_rev, c(0,0.25,0.5,1,2,3,max(alpha_rev)), include.lowest=T)
  ) %>%
  group_by(alpha_rev) %>%
  summarize(
    twd=mean(toward_truth=="Toward")
  ) %>%
  ggplot(aes(x=alpha_rev, y=twd)) +
    stat_summary(fun.y="mean", geom="point")



d_delphi$count_chat=NA
cols=c("communication","toward_truth","count_chat","alpha_rev","trial","question")


dz = 
  rbind(
    d_delphi[,cols] %>% as.data.frame
    , gurc_d_chat[,cols] %>% as.data.frame
  ) %>%
  merge(
    all_aggreg[,c("trial","question","gini_alpha","gini_alpha_quantile")]
    , by=c("question","trial")
  ) %>%
  group_by(trial, question, communication) %>%
  mutate(
      alpha_rank = rank(alpha_rev)
    , alpha_rank = round(alpha_rank/max(alpha_rank),1)
    , count_rank = rank(count_chat)
    , count_rank = round(count_rank/max(count_rank),1)
  ) %>% 
  ungroup %>%
  mutate(
    total_alpha_rank = rank(alpha_rev)
    , total_alpha_rank = round(total_alpha_rank/max(total_alpha_rank),1)
    , toward_truth=toward_truth=="Toward"
  )

dz %>% 
  Rmisc::summarySE(measurevar="toward_truth", groupvars=c("alpha_rank","communication","gini_alpha_quantile")) %>%
  ggplot(aes(x=alpha_rank, y=toward_truth)) +
  stat_summary(fun.y="mean", geom="point") +
  stat_summary(fun.y="mean", geom="line") +
  facet_wrap(gini_alpha_quantile~communication)+
  #geom_errorbar(aes(ymin=toward_truth-ci, ymax=toward_truth+ci)) +
  nice_theme()


dz %>% 
  Rmisc::summarySE(measurevar="toward_truth", groupvars=c("count_rank","communication","gini_alpha_quantile")) %>%
  ggplot(aes(x=count_rank, y=toward_truth)) +
  stat_summary(fun.y="mean", geom="point") +
  stat_summary(fun.y="mean", geom="line") +
  facet_wrap(gini_alpha_quantile~communication)+
  #geom_errorbar(aes(ymin=toward_truth-ci, ymax=toward_truth+ci)) +
  nice_theme()


dz %>% 
  Rmisc::summarySE(measurevar="toward_truth", groupvars=c("total_alpha_rank","communication")) %>%
  ggplot(aes(x=total_alpha_rank, y=toward_truth)) +
  stat_summary(fun.y="mean", geom="point") +
  facet_wrap(.~communication)+
  geom_errorbar(aes(ymin=toward_truth-ci, ymax=toward_truth+ci)) +
  nice_theme()


ggplot(dz, aes(x=total_alpha_rank, y=(toward_truth=="Toward")*1)) +
  stat_summary(fun.y="mean", geom="point")

dz %>%
  mutate(alpha_rev=round(alpha_rev,1)) %>%
  ggplot(aes(x=alpha_rev, y=alpha_rank)) +
    stat_summary(fun.y="mean", geom="point")
  