
library(pacman) 
p_load(tidyverse, dplyr, devtools, ggplot2, gplots, skimr, markdown, rmarkdown, broom, ggridges, readxl, BSDA, scales, ggthemes, readr, plotrix)


## Importing Data 
Exp1_Graphics <- read_csv("Exp1.csv")


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
  labs(x = "Study Condition", y = "Proportion of Correctly Recalled Words", 
       title = "Final Test Performance for Younger and Older Adults")+
  theme(legend.position="none")



