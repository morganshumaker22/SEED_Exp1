
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
  geom_bar(stat= "identity", width = 1)+ facet_wrap(~AgeGroup2)+ylim(0,1)+
  theme_apa(base_size = 12, base_family = "")+
  scale_fill_manual(values=c("black", "grey"))+
  labs(x = "Study Condition", y = "Proportion of Correctly Recalled Words")+
  theme(legend.position="none")


MetaGraph <- read_csv("MetaGraph.csv")

ggplot(MetaGraph, aes(x = Condition, y = Proportion, fill = Better_Performance ))+
  geom_col()+facet_wrap(~Age_Group)+ylim(0,1)+
  theme_apa(base_size = 12, base_family = "")+
  scale_fill_manual(values=c("black", "grey"))+
  labs(x = "Study Condition", y = "Proportion", 
       fill = "Performance")
  

