SEED Experiment 1
================
Morgan Shumaker

``` r
library(pacman) 
p_load(tidyverse, dplyr, devtools, ggplot2, gplots, skimr, markdown, rmarkdown, broom, ggridges, readxl, BSDA, scales, ggthemes)
```

## Importing Data

``` r
SEED_Exp1 <- read_xlsx("R_Exp1_SEED.xlsx")
```

``` r
SEED_Exp1_wide <- read_xlsx("R_Exp1SEED_wide.xlsx")
```

## Errorless

To run a z-test with this package (BSDA), the data needs to be in a data
frame so that R recognizes it as an object:

``` r
SEED_Exp1_wide <- data.frame(SEED_Exp1_wide)
```

*Create data frames*

``` r
  Older_Errorless <- data.frame(SEED_Exp1_wide$Older_Errorless)
```

``` r
  Younger_Errorless <- data.frame(SEED_Exp1_wide$Younger_Errorless)
```

*Z test*

``` r
  z.test(x = Older_Errorless, y = Younger_Errorless, alternative = "two.sided", mu = 0, sigma.x = 15, sigma.y = 15)
```

    ## 
    ##  Two-sample z-Test
    ## 
    ## data:  Older_Errorless and Younger_Errorless
    ## z = -0.0053851, p-value = 0.9957
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -7.677102  7.635032
    ## sample estimates:
    ##  mean of x  mean of y 
    ## -0.6310349 -0.6099996

## Errorful

*Create data frames*

``` r
  Older_Errorful <- data.frame(SEED_Exp1_wide$Older_Errorful)
```

``` r
  Younger_Errorful <- data.frame(SEED_Exp1_wide$Younger_Erroful)
```

*Z test*

``` r
  z.test(x = Older_Errorful, y = Younger_Errorful, alternative = "two.sided", mu = 0, sigma.x = 15, sigma.y = 15)
```

    ## 
    ##  Two-sample z-Test
    ## 
    ## data:  Older_Errorful and Younger_Errorful
    ## z = -2.273e-07, p-value = 1
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -7.656068  7.656066
    ## sample estimates:
    ##     mean of x     mean of y 
    ## -4.625933e-07  4.252953e-07

## Graphics

``` r
SEED_Plots <- read_xlsx("SEED_Exp1_RPlots.xlsx")
```

*Creating proportion of people who did better in errorful*

``` r
SEED_errorful <- SEED_Plots %>%
  group_by(AgeGroup) %>%
  count(BetterErrorful)
```

``` r
rename(SEED_errorful, totaln = n)
```

    ## # A tibble: 4 × 3
    ## # Groups:   AgeGroup [2]
    ##   AgeGroup BetterErrorful totaln
    ##      <dbl>          <dbl>  <int>
    ## 1        1              0      8
    ## 2        1              1     22
    ## 3        2              0      8
    ## 4        2              1     21

``` r
Errorful_Young <- SEED_errorful %>%
  filter(AgeGroup == 1) %>%
  mutate(totaln = (sum(n))) 
```

``` r
Errorful_Old <- SEED_errorful %>%  
  filter(AgeGroup == 2) %>%
  mutate(totaln = sum(n))
```

``` r
ErrorfulTotal <- full_join(Errorful_Young, Errorful_Old)
```

    ## Joining, by = c("AgeGroup", "BetterErrorful", "n", "totaln")

``` r
ErrorfulTotal <- ErrorfulTotal %>% 
  mutate(errorfulprop = n/totaln)
```

*Creating proportion of people who did better in errorless*

``` r
SEED_errorless <- SEED_Plots %>%
  group_by(AgeGroup) %>%
  count(BetterErrorless)
```

``` r
rename(SEED_errorless, totaln = n)
```

    ## # A tibble: 4 × 3
    ## # Groups:   AgeGroup [2]
    ##   AgeGroup BetterErrorless totaln
    ##      <dbl>           <dbl>  <int>
    ## 1        1               0     26
    ## 2        1               1      4
    ## 3        2               0     22
    ## 4        2               1      7

``` r
Errorless_Young <- SEED_errorless %>%
  filter(AgeGroup == 1) %>%
  mutate(totaln = (sum(n))) 
```

``` r
Errorless_Old <- SEED_errorless %>%  
  filter(AgeGroup == 2) %>%
  mutate(totaln = sum(n))
```

``` r
ErrorlessTotal <- full_join(Errorless_Young, Errorless_Old)
```

    ## Joining, by = c("AgeGroup", "BetterErrorless", "n", "totaln")

``` r
ErrorlessTotal <- ErrorlessTotal %>% 
  mutate(errorlessprop = n/totaln)
```

*Combining data frames*

``` r
BetterIn_Total <- full_join(ErrorfulTotal, ErrorlessTotal)
```

    ## Joining, by = c("AgeGroup", "n", "totaln")

## Creating Metacognition Errorless

``` r
Meta_errorless <- SEED_Plots %>%
  group_by(AgeGroup) %>%
  count(Metacognition)
```

``` r
rename(Meta_errorless, totaln = n)
```

    ## # A tibble: 4 × 3
    ## # Groups:   AgeGroup [2]
    ##   AgeGroup Metacognition totaln
    ##      <dbl>         <dbl>  <int>
    ## 1        1             0     14
    ## 2        1             1     16
    ## 3        2             0     18
    ## 4        2             1     11

``` r
MetaErrorless_Y <- Meta_errorless %>%
  filter(AgeGroup == 1) %>%
  mutate(totaln = (sum(n))) 
```

``` r
MetaErrorless_O <- Meta_errorless %>%  
  filter(AgeGroup == 2) %>%
  mutate(totaln = sum(n))
```

``` r
MetaErrorlessTotal <- full_join(MetaErrorless_Y, MetaErrorless_O)
```

    ## Joining, by = c("AgeGroup", "Metacognition", "n", "totaln")

``` r
MetaErrorlessTotal <- MetaErrorlessTotal %>% 
  mutate(metaerrorless_prop = n/totaln)
```

## Creating Metacognition Errorful

``` r
Meta_errorful <- SEED_Plots %>%
  group_by(AgeGroup) %>%
  count(Metacognition)
```

``` r
rename(Meta_errorful, totaln = n)
```

    ## # A tibble: 4 × 3
    ## # Groups:   AgeGroup [2]
    ##   AgeGroup Metacognition totaln
    ##      <dbl>         <dbl>  <int>
    ## 1        1             0     14
    ## 2        1             1     16
    ## 3        2             0     18
    ## 4        2             1     11

``` r
MetaErrorful_Y <- Meta_errorful %>%
  filter(AgeGroup == 1) %>%
  mutate(totaln = (sum(n))) 
```

``` r
MetaErrorful_O <- Meta_errorful %>%  
  filter(AgeGroup == 2) %>%
  mutate(totaln = sum(n))
```

``` r
MetaErrorfulTotal <- full_join(MetaErrorful_Y, MetaErrorful_O)
```

    ## Joining, by = c("AgeGroup", "Metacognition", "n", "totaln")

``` r
MetaErrorfulTotal <- MetaErrorfulTotal %>% 
  mutate(metaerrorfulprop = n/totaln)
```

``` r
MetacognitionTotal <- full_join(MetaErrorfulTotal, MetaErrorlessTotal)
```

    ## Joining, by = c("AgeGroup", "Metacognition", "n", "totaln")

## Creating Full Data Set

``` r
SEEDTotal <- full_join(MetacognitionTotal, BetterIn_Total)
```

    ## Joining, by = c("AgeGroup", "n", "totaln")

## To change labels on graph

``` r
SEED_Plots <- SEED_Plots %>%
  mutate(Metacog = case_when(Metacognition == "0" ~ "Errorless",
                             Metacognition == "1" ~ "Errorful",
                             TRUE ~ as.character(Metacognition)))
```
