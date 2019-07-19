source("00_data_prep.R")

pct_labels = function(x) { paste0(x*100,"%")}

d_sum = all_aggreg %>% 
  subset(prop_toward!=0.5) %>%
  mutate(
    majority_toward = cut(prop_toward, c(0,0.25,0.5,0.75,1), include.lowest=T)
  ) %>% 
  group_by(communication, majority_toward) %>%
  summarize(
      lower = 1-binom.test(table(improve))$conf.int[2]
    , upper = 1-binom.test(table(improve))$conf.int[1]
    , N=length(improve)
    , improve=mean(improve)
  )

levels(d_sum$majority_toward) = c("0-25%", "25-50%", "50-75%", "75-100%")

label_location =(d_sum$lower) + c(0.3,-0.055, -0.055, -0.055,
                                  0.6, 0.3, 0.25, 0.255)

ggplot(d_sum
       , aes(x=majority_toward, y=improve)) +
    geom_hline(yintercept=0.5, linetype="dashed") +
    geom_point(position=position_dodge(0.5), size=4) +
    geom_errorbar(aes(ymin=lower, ymax=upper)
                  , size=1.15, width=0, position=position_dodge(0.5))+
    geom_label(aes(label=paste0(N), y=label_location), size=3.5, position=position_dodge(0.5), label.padding=unit(0.15,"lines"))+
    scale_y_continuous(expand = c(0,0), 
                       lim=c(-0.01,1.01)
                       , labels=pct_labels)+
    guides(color=F)+
    labs(x="Proportion Toward Truth", y="", color="") +
    facet_wrap(.~communication, scales="free")+
    nice_theme()
ggsave("Figures/effect_of_distribution.png", width=6, height=3)

