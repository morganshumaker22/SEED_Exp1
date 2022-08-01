library(pacman) 
p_load(tidyverse, psych, readr, dplyr, devtools, ggplot2, gplots, skimr, markdown, rmarkdown, broom, ggridges, readxl, BSDA, scales, ggthemes, readr, plotrix, papaja)

Exp1_Graphics <- Exp1_Graphics %>% 
  mutate(Repetition = case_when(Repetition == "FALSE" ~ 0,
                                Repetition == "TRUE" ~ 1,
                                Repetition ~ as.numeric(Repetition))) %>% 
  mutate(Imagery = case_when(Imagery == "FALSE" ~ 0,
                                Imagery == "TRUE" ~ 1,
                                Imagery ~ as.numeric(Imagery))) %>% 
  mutate(Action = case_when(Action == "FALSE" ~ 0,
                             Action == "TRUE" ~ 1,
                             Action ~ as.numeric(Action))) %>%
  mutate(Keyword = case_when(Keyword == "FALSE" ~ 0,
                             Keyword == "TRUE" ~ 1,
                             Keyword ~ as.numeric(Keyword))) %>%
  mutate(Story = case_when(Story == "FALSE" ~ 0,
                           Story == "TRUE" ~ 1,
                           Story ~ as.numeric(Story))) %>% 
  mutate(Other = case_when(Other  == "FALSE" ~ 0,
                           Other  == "TRUE" ~ 1,
                           Other  ~ as.numeric(Other )))
write_csv(Exp1_Graphics, "/Users/morgan/Desktop/TCU/Hargis Lab/Projects/SEED/Exp 1 Data/SEED_Exp1/Exp1_Graphics.csv")


#Analyses for Age Group on Study Strategies
t.test(Exp1_Graphics$Imagery ~ Exp1_Graphics$AgeGroup, var.equal=TRUE)
t.test(Exp1_Graphics$Action ~ Exp1_Graphics$AgeGroup, var.equal=TRUE)
t.test(Exp1_Graphics$Keyword ~ Exp1_Graphics$AgeGroup, var.equal=TRUE)
t.test(Exp1_Graphics$Story ~ Exp1_Graphics$AgeGroup, var.equal=TRUE)
t.test(Exp1_Graphics$Repetition ~ Exp1_Graphics$AgeGroup, var.equal=TRUE)
t.test(Exp1_Graphics$Other ~ Exp1_Graphics$AgeGroup, var.equal=TRUE)
t.test(Exp1_Graphics$Metacog ~ Exp1_Graphics$AgeGroup, var.equal=TRUE)



