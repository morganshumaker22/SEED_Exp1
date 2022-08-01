library(pacman) 
p_load(tidyverse, dplyr, devtools, ggplot2, gplots, skimr, markdown, rmarkdown, broom, ggridges, readxl, BSDA, scales, ggthemes, readr, plotrix, papaja)

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
