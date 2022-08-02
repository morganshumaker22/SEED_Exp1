
library(pacman) 
p_load(tidyverse, dplyr, devtools, ggplot2, gplots, skimr, markdown, rmarkdown, broom, ggridges, readxl, BSDA, scales, ggthemes, readr, plotrix, papaja)


## Importing Data 
Exp1_Graphics <- read_csv("Exp1_Graphics.csv")


## Figure 1
Exp1_Graphics <- Exp1_Graphics %>% 
  mutate(condition = case_when(Condition == 0 ~ "Errorless",
                               Condition == 1 ~ "Errorful",
                               TRUE ~ as.character(Condition))) %>% 
  mutate(accuracy = as.numeric(Accuracy))

ggplot(Exp1_Graphics, aes(y=accuracy, x=condition, fill = condition)) + 
  geom_bar(stat= "identity", width = 1, color = "black")+ facet_wrap(~AgeGroup2)+ylim(0,1)+
  theme_apa(base_size = 12, base_family = "")+
  scale_fill_manual(values=c("black", "white"))+
  labs(x = "Study Condition", y = "Proportion of Correctly Recalled Words")+
  theme(legend.position="none")+
  theme(axis.line = element_line(colour = "black", size = .3))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+
  theme(text = element_text(size = 12, family = "Arial", color = "black"))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
  theme(panel.spacing.x = unit(0, "null"))+
  theme(plot.title = element_text(size=12))+geom_errorbar(aes(ymin = "lower", ymax = "upper"), width = 0.2)


MetaGraph <- read_csv("MetaGraph.csv")

MetaGraph <- MetaGraph %>% 
  mutate(Estimate = fct_recode(Better_Performance, Estimated = "Predicted"))

ggplot(MetaGraph, aes(x = Condition, y = Proportion, fill = Estimate))+
  geom_col(position = "fill", color = "black")+facet_wrap(~Age_Group)+ylim(0,1)+
  theme_apa(base_size = 12, base_family = "")+
  scale_fill_manual(values=c("black", "white"))+
  labs(x = "Study Condition", y = "Proportion", 
       fill = "Performance")+
  theme(axis.line = element_line(colour = "black", size = .3))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+
  theme(text = element_text(size = 12, family = "Arial", color = "black"))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
  theme(panel.spacing.x = unit(0, "null"))+
  theme(plot.title = element_text(size=12))
  




