library(tidyverse)
library(easysorter)
x <- readr::read_csv("~/Desktop/2021_Pallotto/data/2021_tubulinalleles.csv")

fullregressed <- easysorter::regress(x)

statsabz<- fullregressed %>%
  aov(phenotype ~ strain, data = .)%>%
  rstatix::tukey_hsd()

ABZplot <- fullregressed %>%
  #dplyr::filter(condition == "DMSO")%>%
  dplyr::mutate(strain = factor(strain, levels = c("tN2","tbb1del","b1del","ben1tbb1del")))%>%
  ggplot()+
  aes(strain, phenotype)+
  geom_boxplot(aes(fill=strain),outlier.shape = NA)+
  ylab("Regressed Animal Length")+
  geom_text(aes(x="tbb1del",y=210, label="****"))+
  geom_text(aes(x="ben1tbb1del",y=210, label="****"))+
  geom_text(aes(x="b1del",y=210, label="****"))+
  geom_jitter(width = 0.1)+
  cowplot::theme_cowplot(12)+
  scale_fill_manual(values = c("tN2"="orange","tbb1del"="grey","ben1tbb1del"="grey","b1del"="grey"))+
  scale_x_discrete(labels = c("tN2"="WT","tbb1del" = "Δtbb-1","b1del"="Δben-1","ben1tbb1del"="Δben-1\nΔtbb-1"))+
  theme(legend.position = "None",
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 12),
        strip.text = element_text(size = 12, face = "bold"),
        axis.title.x = element_blank())

ggsave("~/Desktop/2021_Pallotto/plots/figure3.png", units = "in", plot = ABZplot, width = 8, height = 6)

z <- readr::read_csv("~/Desktop/2021_Pallotto/data/2021_tubulinallelescontrol.csv")

statsDMSO<- z %>%
  dplyr::filter(condition=="DMSO")%>%
  aov(phenotype ~ strain, data = .)%>%
  rstatix::tukey_hsd()

DMSOplot <- z %>%
  dplyr::filter(condition == "DMSO")%>%
  dplyr::mutate(strain = factor(strain, levels = c("tN2","ECA882","tbb1del","b1del","ben1tbb1del")))%>%
  ggplot()+
  aes(strain, phenotype)+
  geom_boxplot(aes(fill=strain),outlier.shape = NA)+
  ylab("Regressed Animal Length")+
  cowplot::theme_cowplot(12)+
  geom_jitter(width = 0.1)+
  geom_text(aes(x="tbb1del",y=110, label="****"))+
  geom_text(aes(x="ben1tbb1del",y=110, label="****"))+
  geom_text(aes(x="b1del",y=110, label="****"))+
  geom_text(aes(x="ECA882",y=110, label="ns"))+
  scale_fill_manual(values = c("tN2"="orange","tbb1del"="grey","ben1tbb1del"="grey","b1del"="grey","ECA882"="grey"))+
  scale_x_discrete(labels = c("tN2"="WT","tbb1del" = "Δtbb-1","b1del"="Δben-1\ne1880","ben1tbb1del"="Δben-1\nΔtbb-1","ECA882"="Δben-1\nECA882"))+
  theme(legend.position = "None",
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 12),
        strip.text = element_text(size = 12, face = "bold"),
        axis.title.x = element_blank())
ggsave("~/Desktop/2021_Pallotto/plots/supplementalfigure_4.png", units = "in", plot = DMSOplot, width = 8, height = 6)





