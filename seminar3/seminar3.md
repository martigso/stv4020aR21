---
title: "Seminar 3"
output:
  html_document:
    keep_md: yes
    self_contained: no
    keep_html: no
---

I dag skal vi fortsette med OLS og databehandling:
1. Hvordan plotter vi resultater fra OLS?
2. Hvordan bruker vi R til å sjekke om forutsetningene for OLS holder?
3. Hvordan slår vi sammen flere datasett? 

Først: er det noen spørsmål til det vi gikk gjennom i går? Dersom du synes manipulering av data er vanskelig så kan det hjelpe å ta en titt på kapittel seks i **Lær deg R**. Dersom du er nysgjerrig på flere måter å omkode variabler på så kan du kikke på kapittel 5 i [**R for Data Science**](https://r4ds.had.co.nz/transform.html#add-new-variables-with-mutate). Og ikke glem: internett er din venn når du skal lære R. 

## Hvordan plotte resutlater fra OLS? 
I dag skal vi plotte resultatene og gjøre regresjonsdiagnostikk på modellen fra **Burnside and Dollar** - samme artikkel som vi har brukt tidligere i uka og samme som dere repliserte i oppgaven i går. Først laster vi inn pakker, data og kjører modellen.  


```r
library(tidyverse)
```

```
## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --
```

```
## v ggplot2 3.3.3     v purrr   0.3.4
## v tibble  3.1.2     v dplyr   1.0.6
## v tidyr   1.1.3     v stringr 1.4.0
## v readr   1.4.0     v forcats 0.5.1
```

```
## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
# Laster inn data
load("./aid.RData")

# Gjør de nødvendige omkodingene som dere gjorde i oppgaven
aid <- aid %>% 
  mutate(log_gdp_pr_capita = log(gdp_pr_capita),
         period_fac = as.factor(period),
         region = ifelse(fast_growing_east_asia == 1, "East Asia",
                         ifelse(sub_saharan_africa == 1, "Sub-Saharan Africa", "Other")),
         region = factor(region, levels = c("Other", "Sub-Saharan Africa", "East Asia")))

# Kjører modellen og bevarer informasjon om missing med na.action = "na.exclude"
m5 <- lm(data = aid, 
         gdp_growth ~ log_gdp_pr_capita + ethnic_frac*assasinations + 
           institutional_quality + m2_gdp_lagged + region + policy*aid +
           period_fac, 
         na.action = "na.exclude")

# Printer resultatene i en tabell
library(stargazer)
```

```
## 
## Please cite as:
```

```
##  Hlavac, Marek (2018). stargazer: Well-Formatted Regression and Summary Statistics Tables.
```

```
##  R package version 5.2.2. https://CRAN.R-project.org/package=stargazer
```

```r
stargazer(m5, type = "text")
```

```
## 
## =====================================================
##                               Dependent variable:    
##                           ---------------------------
##                                   gdp_growth         
## -----------------------------------------------------
## log_gdp_pr_capita                   -0.600           
##                                     (0.393)          
##                                                      
## ethnic_frac                         -0.424           
##                                     (0.810)          
##                                                      
## assasinations                       -0.449           
##                                     (0.301)          
##                                                      
## institutional_quality              0.687***          
##                                     (0.175)          
##                                                      
## m2_gdp_lagged                        0.012           
##                                     (0.016)          
##                                                      
## regionSub-Saharan Africa           -1.872***         
##                                     (0.681)          
##                                                      
## regionEast Asia                     1.307*           
##                                     (0.731)          
##                                                      
## policy                             0.712***          
##                                     (0.244)          
##                                                      
## aid                                 -0.021           
##                                     (0.178)          
##                                                      
## period_fac3                         -0.013           
##                                     (0.620)          
##                                                      
## period_fac4                        -1.414**          
##                                     (0.629)          
##                                                      
## period_fac5                        -3.470***         
##                                     (0.641)          
##                                                      
## period_fac6                        -2.010***         
##                                     (0.661)          
##                                                      
## period_fac7                        -2.256***         
##                                     (0.708)          
##                                                      
## ethnic_frac:assasinations            0.792           
##                                     (0.620)          
##                                                      
## policy:aid                          0.186*           
##                                     (0.101)          
##                                                      
## Constant                             3.391           
##                                     (2.945)          
##                                                      
## -----------------------------------------------------
## Observations                          270            
## R2                                   0.394           
## Adjusted R2                          0.356           
## Residual Std. Error            2.873 (df = 253)      
## F Statistic                10.297*** (df = 16; 253)  
## =====================================================
## Note:                     *p<0.1; **p<0.05; ***p<0.01
```
Så plotter vi effekten av institusjonell kvalitet på vekst i BNP (GDP). Vi går ikke veldig nøye inn på dette nå, men les gjerne [denne guiden til regresjonsplot](https://github.com/liserodland/stv4020aR/blob/master/Materiell%20fra%20tidl%20semestre/docs/Regresjonsplot.md). For å plotte en regresjonslinje så oppretter vi først et datasett der vi holder alle uavhengige variabler bortsett fra den vi vil plotte effekten til konstante. Her velger jeg å la `institutional_quality` variere fra minimums- til maksimumsverdien og setter resten av variablene til gjennomsnitt eller modusverdi. Neste steg er å predikere verdier for det nye datasettet basert på modellen vår ved hjelp av `predict()`. `predict()` tar datasettet vi har laget og gir oss blant annet predikerte verdier og konfidensintervaller basert på modellen vår. For å få datasettet vi skal bruke til plotting så binder vi resultatet av `predict` sammen med datasettet vi lagde. For at `predict()` skal gi likt antall observasjoner som vi har i datasettet vårt så er det viktig å bevare informasjon om de observasjonene som har missing. Dette gjør vi med argumentet `na.action = "na.exclude` i `lm()`. 


```r
# Lager datasettet
snitt_data <- data.frame(log_gdp_pr_capita = mean(aid$log_gdp_pr_capita, na.rm = TRUE),
                         ethnic_frac = mean(aid$ethnic_frac, na.rm = TRUE),
                         assasinations = mean(aid$assasinations, na.rm = TRUE),
                         institutional_quality = c(seq(min(aid$institutional_quality, na.rm = TRUE),
                                                   max(aid$institutional_quality, na.rm =TRUE), by = 0.5)),
                         m2_gdp_lagged = mean(aid$m2_gdp_lagged, na.rm = TRUE),
                         region = "Other",
                         policy = mean(aid$policy, na.rm = TRUE),
                         aid = mean(aid$aid, na.rm = TRUE),
                         period_fac = "4")

# Bruker predict
predict(m5, newdata = snitt_data, se = TRUE)
```

```
## $fit
##         1         2         3         4         5         6         7         8 
## 0.3620244 0.7054454 1.0488664 1.3922874 1.7357084 2.0791294 2.4225504 2.7659714 
##         9        10 
## 3.1093924 3.4528134 
## 
## $se.fit
##         1         2         3         4         5         6         7         8 
## 0.5780223 0.5366767 0.5071121 0.4914594 0.4910508 0.5059230 0.5348029 0.5755858 
##         9        10 
## 0.6259494 0.6837801 
## 
## $df
## [1] 253
## 
## $residual.scale
## [1] 2.872583
```

```r
# Legger predikerte verdier inn i snitt_data
snitt_data <- cbind(snitt_data, predict(m5, newdata = snitt_data, se = TRUE, interval = "confidence"))
snitt_data
```

```
##    log_gdp_pr_capita ethnic_frac assasinations institutional_quality
## 1           7.440955   0.4738369     0.3974164              2.270833
## 2           7.440955   0.4738369     0.3974164              2.770833
## 3           7.440955   0.4738369     0.3974164              3.270833
## 4           7.440955   0.4738369     0.3974164              3.770833
## 5           7.440955   0.4738369     0.3974164              4.270833
## 6           7.440955   0.4738369     0.3974164              4.770833
## 7           7.440955   0.4738369     0.3974164              5.270833
## 8           7.440955   0.4738369     0.3974164              5.770833
## 9           7.440955   0.4738369     0.3974164              6.270833
## 10          7.440955   0.4738369     0.3974164              6.770833
##    m2_gdp_lagged region   policy     aid period_fac   fit.fit     fit.lwr
## 1         28.415  Other 1.160546 1.75757          4 0.3620244 -0.77632391
## 2         28.415  Other 1.160546 1.75757          4 0.7054454 -0.35147747
## 3         28.415  Other 1.160546 1.75757          4 1.0488664  0.05016746
## 4         28.415  Other 1.160546 1.75757          4 1.3922874  0.42441462
## 5         28.415  Other 1.160546 1.75757          4 1.7357084  0.76864047
## 6         28.415  Other 1.160546 1.75757          4 2.0791294  1.08277224
## 7         28.415  Other 1.160546 1.75757          4 2.4225504  1.36931761
## 8         28.415  Other 1.160546 1.75757          4 2.7659714  1.63242141
## 9         28.415  Other 1.160546 1.75757          4 3.1093924  1.87665708
## 10        28.415  Other 1.160546 1.75757          4 3.4528134  2.10618737
##     fit.upr    se.fit  df residual.scale
## 1  1.500373 0.5780223 253       2.872583
## 2  1.762368 0.5366767 253       2.872583
## 3  2.047565 0.5071121 253       2.872583
## 4  2.360160 0.4914594 253       2.872583
## 5  2.702776 0.4910508 253       2.872583
## 6  3.075487 0.5059230 253       2.872583
## 7  3.475783 0.5348029 253       2.872583
## 8  3.899521 0.5755858 253       2.872583
## 9  4.342128 0.6259494 253       2.872583
## 10 4.799439 0.6837801 253       2.872583
```
Variabelen som heter `fit.fit` er de predikerte verdiene. `fit.lwr` og `fit.upr` er nedre og øvre grense for et 95 % konfidensintervall. `se.fit` er standardfeilen. 


Lager plot:

```r
library(ggplot2)
ggplot(snitt_data, aes(x = institutional_quality, y = fit.fit)) + # Setter institusjonell kvalitet på x-aksen og predikert verdi på y-aksen
  geom_line() +                                                   # Sier at jeg vil ha et linjediagram
  scale_y_continuous(breaks = seq(-12, 12, 2)) +                  # Bestemmer verdier og mellomrom på y-aksen
  geom_ribbon(aes(ymin = fit.lwr, ymax = fit.upr, color = NULL), alpha = .2) + # Legger til konfidensintervall på plottet
  labs(x = "Kvalitet på institusjoner", y = "Forventet GDP vekst", color = "Policy", fill = "Policy") # Setter tittel på akser og plot
```

![](seminar3_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Dette kan, og bør, også gjøres når det er samspill i modellen. Samspill er vanskelig å tolke i en tabell og jeg synes derfor det er fint å plotte disse. Når vi skal plotte samspill så lar vi begge variablene som er en del av samspillsleddet variere, mens resten er konstante. Vi lar den ene variabelen være `x`, mens vi bruker den andre til å fylle ut argumentet `color`. I tilfellet med to kontinuerlige variabler må en gjøre den ene om til en faktorvariabel slik jeg gjør med policy under. 


```r
# Lager plot data
snitt_data_sam <- data.frame(log_gdp_pr_capita = mean(aid$log_gdp_pr_capita, na.rm = TRUE),
                         ethnic_frac = mean(aid$ethnic_frac, na.rm = TRUE),
                         assasinations = mean(aid$assasinations, na.rm = TRUE),
                         institutional_quality = mean(aid$institutional_quality, na.rm = TRUE),
                         m2_gdp_lagged = mean(aid$m2_gdp_lagged, na.rm = TRUE),
                         region = "Other",
                         policy = c(rep(-1, 9), rep(0, 9), rep(1, 9)),
                         aid = rep(0:8, 3),
                         period_fac = "4")

# Predikerer verdier (løser likningen for modellen)
predict(m5, newdata = snitt_data_sam, se = TRUE)
```

```
## $fit
##           1           2           3           4           5           6 
##  0.08409744 -0.12290215 -0.32990173 -0.53690132 -0.74390091 -0.95090049 
##           7           8           9          10          11          12 
## -1.15790008 -1.36489967 -1.57189925  0.79654817  0.77576457  0.75498097 
##          13          14          15          16          17          18 
##  0.73419737  0.71341377  0.69263017  0.67184656  0.65106296  0.63027936 
##          19          20          21          22          23          24 
##  1.50899890  1.67443129  1.83986367  2.00529606  2.17072844  2.33616083 
##          25          26          27 
##  2.50159321  2.66702559  2.83245798 
## 
## $se.fit
##         1         2         3         4         5         6         7         8 
## 0.7202650 0.6279388 0.6266786 0.7169646 0.8707719 1.0608217 1.2709582 1.4927225 
##         9        10        11        12        13        14        15        16 
## 1.7216269 0.5782802 0.5284831 0.5362879 0.5994488 0.7032047 0.8325135 0.9772843 
##        17        18        19        20        21        22        23        24 
## 1.1315980 1.2920401 0.5183695 0.4898167 0.5059652 0.5629815 0.6502018 0.7572607 
##        25        26        27 
## 0.8769218 1.0046925 1.1378441 
## 
## $df
## [1] 253
## 
## $residual.scale
## [1] 2.872583
```

```r
# Lagrer predikerte verdier i plot datasettet
snitt_data_sam <- cbind(snitt_data_sam, predict(m5, newdata = snitt_data_sam, se = TRUE, interval = "confidence"))
snitt_data_sam
```

```
##    log_gdp_pr_capita ethnic_frac assasinations institutional_quality
## 1           7.440955   0.4738369     0.3974164              4.607119
## 2           7.440955   0.4738369     0.3974164              4.607119
## 3           7.440955   0.4738369     0.3974164              4.607119
## 4           7.440955   0.4738369     0.3974164              4.607119
## 5           7.440955   0.4738369     0.3974164              4.607119
## 6           7.440955   0.4738369     0.3974164              4.607119
## 7           7.440955   0.4738369     0.3974164              4.607119
## 8           7.440955   0.4738369     0.3974164              4.607119
## 9           7.440955   0.4738369     0.3974164              4.607119
## 10          7.440955   0.4738369     0.3974164              4.607119
## 11          7.440955   0.4738369     0.3974164              4.607119
## 12          7.440955   0.4738369     0.3974164              4.607119
## 13          7.440955   0.4738369     0.3974164              4.607119
## 14          7.440955   0.4738369     0.3974164              4.607119
## 15          7.440955   0.4738369     0.3974164              4.607119
## 16          7.440955   0.4738369     0.3974164              4.607119
## 17          7.440955   0.4738369     0.3974164              4.607119
## 18          7.440955   0.4738369     0.3974164              4.607119
## 19          7.440955   0.4738369     0.3974164              4.607119
## 20          7.440955   0.4738369     0.3974164              4.607119
## 21          7.440955   0.4738369     0.3974164              4.607119
## 22          7.440955   0.4738369     0.3974164              4.607119
## 23          7.440955   0.4738369     0.3974164              4.607119
## 24          7.440955   0.4738369     0.3974164              4.607119
## 25          7.440955   0.4738369     0.3974164              4.607119
## 26          7.440955   0.4738369     0.3974164              4.607119
## 27          7.440955   0.4738369     0.3974164              4.607119
##    m2_gdp_lagged region policy aid period_fac     fit.fit    fit.lwr   fit.upr
## 1         28.415  Other     -1   0          4  0.08409744 -1.3343814 1.5025763
## 2         28.415  Other     -1   1          4 -0.12290215 -1.3595553 1.1137510
## 3         28.415  Other     -1   2          4 -0.32990173 -1.5640730 0.9042695
## 4         28.415  Other     -1   3          4 -0.53690132 -1.9488805 0.8750779
## 5         28.415  Other     -1   4          4 -0.74390091 -2.4587859 0.9709841
## 6         28.415  Other     -1   5          4 -0.95090049 -3.0400666 1.1382656
## 7         28.415  Other     -1   6          4 -1.15790008 -3.6609059 1.3451058
## 8         28.415  Other     -1   7          4 -1.36489967 -4.3046446 1.5748453
## 9         28.415  Other     -1   8          4 -1.57189925 -4.9624451 1.8186466
## 10        28.415  Other      0   0          4  0.79654817 -0.3423081 1.9354044
## 11        28.415  Other      0   1          4  0.77576457 -0.2650221 1.8165512
## 12        28.415  Other      0   2          4  0.75498097 -0.3011763 1.8111382
## 13        28.415  Other      0   3          4  0.73419737 -0.4463480 1.9147427
## 14        28.415  Other      0   4          4  0.71341377 -0.6714669 2.0982945
## 15        28.415  Other      0   5          4  0.69263017 -0.9469093 2.3321696
## 16        28.415  Other      0   6          4  0.67184656 -1.2528022 2.5964953
## 17        28.415  Other      0   7          4  0.65106296 -1.5774890 2.8796149
## 18        28.415  Other      0   8          4  0.63027936 -1.9142448 3.1748035
## 19        28.415  Other      1   0          4  1.50899890  0.4881299 2.5298679
## 20        28.415  Other      1   1          4  1.67443129  0.7097938 2.6390688
## 21        28.415  Other      1   2          4  1.83986367  0.8434235 2.8363038
## 22        28.415  Other      1   3          4  2.00529606  0.8965689 3.1140232
## 23        28.415  Other      1   4          4  2.17072844  0.8902308 3.4512261
## 24        28.415  Other      1   5          4  2.33616083  0.8448232 3.8274984
## 25        28.415  Other      1   6          4  2.50159321  0.7745967 4.2285898
## 26        28.415  Other      1   7          4  2.66702559  0.6883994 4.6456518
## 27        28.415  Other      1   8          4  2.83245798  0.5916051 5.0733109
##       se.fit  df residual.scale
## 1  0.7202650 253       2.872583
## 2  0.6279388 253       2.872583
## 3  0.6266786 253       2.872583
## 4  0.7169646 253       2.872583
## 5  0.8707719 253       2.872583
## 6  1.0608217 253       2.872583
## 7  1.2709582 253       2.872583
## 8  1.4927225 253       2.872583
## 9  1.7216269 253       2.872583
## 10 0.5782802 253       2.872583
## 11 0.5284831 253       2.872583
## 12 0.5362879 253       2.872583
## 13 0.5994488 253       2.872583
## 14 0.7032047 253       2.872583
## 15 0.8325135 253       2.872583
## 16 0.9772843 253       2.872583
## 17 1.1315980 253       2.872583
## 18 1.2920401 253       2.872583
## 19 0.5183695 253       2.872583
## 20 0.4898167 253       2.872583
## 21 0.5059652 253       2.872583
## 22 0.5629815 253       2.872583
## 23 0.6502018 253       2.872583
## 24 0.7572607 253       2.872583
## 25 0.8769218 253       2.872583
## 26 1.0046925 253       2.872583
## 27 1.1378441 253       2.872583
```

```r
# Plotter
ggplot(snitt_data_sam, aes(x = aid, y = fit.fit, 
                       group = factor(policy), 
                       color = factor(policy), 
                       fill = factor(policy))) +
  geom_line() +
  scale_y_continuous(breaks = seq(-12, 12, 2)) +
  geom_ribbon(aes(ymin = fit.lwr, ymax = fit.upr, color = NULL), alpha = .2) +
  labs(x = "Bistandsnivå", y = "Forventet GDP vekst", color = "Policy", fill = "Policy")
```

![](seminar3_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
Vi skal ikke bruke snitt_data mer så jeg fjerner objektene fra environment:


```r
rm(snitt_data, snitt_data_sam)
```


## Hvordan slår vi sammen flere datasett? 
Når vi skal slå sammen ulike datasett må vi først tenke gjennom hvordan vi kan få en felles nøkkel som lar oss knytte sammen informasjon om observasjonene fra de to datasettene. Dette kan gjøres på flere nivåer. Vi jobber videre med aid-datasettet. 


```r
aid
```

```
## # A tibble: 331 x 22
##    country period periodstart periodend code  gdp_growth gdp_pr_capita
##    <chr>    <dbl>       <dbl>     <dbl> <chr>      <dbl>         <dbl>
##  1 ARG          2        1970      1973 ARG2        1.70          5637
##  2 ARG          3        1974      1977 ARG3        1.08          6168
##  3 ARG          4        1978      1981 ARG4       -1.12          5849
##  4 ARG          5        1982      1985 ARG5       -2.55          5487
##  5 ARG          6        1986      1989 ARG6       -1.10          5624
##  6 ARG          7        1990      1993 ARG7        4.26          4706
##  7 BOL          2        1970      1973 BOL2        1.30          1661
##  8 BOL          3        1974      1977 BOL3        2.96          1838
##  9 BOL          4        1978      1981 BOL4       -1.49          2015
## 10 BOL          5        1982      1985 BOL5       -4.32          1864
## # ... with 321 more rows, and 15 more variables: economic_open <dbl>,
## #   budget_balance <dbl>, inflation <dbl>, ethnic_frac <dbl>,
## #   assasinations <dbl>, aid <dbl>, fast_growing_east_asia <dbl>,
## #   sub_saharan_africa <dbl>, central_america <dbl>, policy <dbl>,
## #   m2_gdp_lagged <dbl>, institutional_quality <dbl>, log_gdp_pr_capita <dbl>,
## #   period_fac <fct>, region <fct>
```

Ser dere noen variabler her vi kunne brukt som felles nøkkel?

Hvilken variabel vi bruker som nøkkel vil avhenge av variablene i det andre datasettet. Er variablene på landnivå, årnivå, land-år-nivå, region eller noe helt annet? Vi skal nå se på hvordan vi kan slå sammen aid-datasettet med et datasett om konflikt. 

Jeg har lastet ned versjon tid av Varieties of democracy datasettet fra V-den sin [nettside](https://www.v-dem.net/en/data/data-version-10/). I V-dem er det en variabel som heter `v2pepwrsoc`. Denne variabelen måler hvor jevnt makt er fordelt mellom sosiale grupper. Jeg har lastet opp en redusert versjon av V-dem datasettet på github. Det kan du lese inn direkte fra [denne lenken](https://raw.githubusercontent.com/liserodland/stv4020aR/master/H20-seminarer/Innf%C3%B8ringsseminarer/data/Vdem_10_redusert.csv).


```r
equality <- read_csv("https://raw.githubusercontent.com/liserodland/stv4020aR/master/H20-seminarer/Innf%C3%B8ringsseminarer/data/Vdem_10_redusert.csv")
```

```
## 
## -- Column specification --------------------------------------------------------
## cols(
##   country_name = col_character(),
##   country_text_id = col_character(),
##   country_id = col_double(),
##   year = col_double(),
##   v2pepwrsoc = col_double()
## )
```

```r
summary(equality$v2pepwrsoc)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -2.9150 -0.6210  0.2460  0.3182  1.2780  3.2060
```

```r
equality
```

```
## # A tibble: 5,163 x 5
##    country_name country_text_id country_id  year v2pepwrsoc
##    <chr>        <chr>                <dbl> <dbl>      <dbl>
##  1 Mexico       MEX                      3  1966     -0.028
##  2 Mexico       MEX                      3  1967     -0.028
##  3 Mexico       MEX                      3  1968     -0.028
##  4 Mexico       MEX                      3  1969     -0.028
##  5 Mexico       MEX                      3  1970     -0.028
##  6 Mexico       MEX                      3  1971     -0.028
##  7 Mexico       MEX                      3  1972     -0.028
##  8 Mexico       MEX                      3  1973     -0.028
##  9 Mexico       MEX                      3  1974     -0.028
## 10 Mexico       MEX                      3  1975     -0.028
## # ... with 5,153 more rows
```

```r
# Vi ser at V-dem har en variabel som heter country_text_id og year
# Kanskje vi kan bruke disse?

# Bruker en logisk test og %in% for å sjekke om det finnes en match for alle land i aid-datasettet:
table(aid$country %in% equality$country_text_id)
```

```
## 
## FALSE  TRUE 
##     6   325
```

```r
# Ikke alle matcher. 
```
Når ikke alle observasjonen har en match så kan dette kan enten løses manuelt eller ved hjelp av andre datasett eller R-pakker. 


```r
# For å løse det manuelt så kan du bruke denne koden til å identifisere de som ikke matcher:
aid %>% 
  select(country) %>%  # Velger country variabelen i aid
  anti_join(equality, by = c("country" = "country_text_id")) %>% # Bevarer de verdiene i equality som ikke er aid. 
  unique()
```

```
## # A tibble: 1 x 1
##   country
##   <chr>  
## 1 ZAR
```

```r
# En nyttig pakke dersom dere kommer over dette problemet kan være countrycode
```

Vi kommer ikke til å bruke tid i seminar på å rette opp i dette, men her finner dere et eksempel på hvordan det kunne vært løst. Vi går derfor videre vel vitende om at vi ikke klarte å matche alle observasjonen (dette anbefaler jeg **ikke** å gjøre i hjemmeoppgaven). Det er fortsatt en ting vi må gjøre før vi kan slå datasettene sammen. V-dem-datasettet inneholder land-år-observasjoner, mens aid-datasettet inneholder land-periode-observasjoner. Vi må derfor lage en periode-variabel i equality-datasettet. 


```r
# Oppretter periode-variabel i V-dem datasettet, slik at jeg er klar til å merge. Verdiene til period-variabelen går fra 1-8, jeg vil gi de samme periodene (datasettet inneholder imidlertid bare data for periode 2-7). Her bruker jeg et en egenskap ved `as.numeric` på en faktor som ofte fører til feil i kode for å gjøre dette raskt:
table(aid$periodstart, aid$period)
```

```
##       
##         2  3  4  5  6  7
##   1970 56  0  0  0  0  0
##   1974  0 56  0  0  0  0
##   1978  0  0 56  0  0  0
##   1982  0  0  0 56  0  0
##   1986  0  0  0  0 54  0
##   1990  0  0  0  0  0 53
```

```r
table(aid$periodend, aid$period)
```

```
##       
##         2  3  4  5  6  7
##   1973 56  0  0  0  0  0
##   1977  0 56  0  0  0  0
##   1981  0  0 56  0  0  0
##   1985  0  0  0 56  0  0
##   1989  0  0  0  0 54  0
##   1993  0  0  0  0  0 53
```

```r
# Det kommer ikke tydelig frem her, men datasettet gikk opprinnelig fra 1966-1998
# Dersom jeg bruker 1966, 1970, 1974, 1978, 1982, 1986, 1990 og 1994 som kuttpunkt,
# bør jeg få de samme gruppene i V-dem-datasettet som i aid

periodcutpoints <-  unique(c(aid$periodstart)) # henter ut ovennevnt årtsall med unique()
# Her buker jeg funksjonen cut(), jeg kunne også brukt ifelse(), men cut() er raskere her.
equality$period <- cut(equality$year, periodcutpoints)
table(equality$year, equality$period)
```

```
##       
##        (1970,1974] (1974,1978] (1978,1982] (1982,1986] (1986,1990]
##   1966           0           0           0           0           0
##   1967           0           0           0           0           0
##   1968           0           0           0           0           0
##   1969           0           0           0           0           0
##   1970           0           0           0           0           0
##   1971         158           0           0           0           0
##   1972         158           0           0           0           0
##   1973         158           0           0           0           0
##   1974         158           0           0           0           0
##   1975           0         158           0           0           0
##   1976           0         157           0           0           0
##   1977           0         157           0           0           0
##   1978           0         157           0           0           0
##   1979           0           0         157           0           0
##   1980           0           0         157           0           0
##   1981           0           0         157           0           0
##   1982           0           0         157           0           0
##   1983           0           0           0         157           0
##   1984           0           0           0         157           0
##   1985           0           0           0         157           0
##   1986           0           0           0         157           0
##   1987           0           0           0           0         157
##   1988           0           0           0           0         157
##   1989           0           0           0           0         158
##   1990           0           0           0           0         172
##   1991           0           0           0           0           0
##   1992           0           0           0           0           0
##   1993           0           0           0           0           0
##   1994           0           0           0           0           0
##   1995           0           0           0           0           0
##   1996           0           0           0           0           0
##   1997           0           0           0           0           0
```

```r
# Tabell viser at jeg må justere periodcutpoints for å få rett

periodcutpoints <- periodcutpoints - 1
table(periodcutpoints)
```

```
## periodcutpoints
## 1969 1973 1977 1981 1985 1989 
##    1    1    1    1    1    1
```

```r
periodcutpoints <- c(1965, periodcutpoints, 1993, 1997) # legger til tre kuttpunkt for å få med periode 1, 7 og 8

# Forsøker på nytt:
equality$period <- cut(equality$year, periodcutpoints)
table(equality$year,equality$period)
```

```
##       
##        (1965,1969] (1969,1973] (1973,1977] (1977,1981] (1981,1985] (1985,1989]
##   1966         156           0           0           0           0           0
##   1967         156           0           0           0           0           0
##   1968         156           0           0           0           0           0
##   1969         156           0           0           0           0           0
##   1970           0         156           0           0           0           0
##   1971           0         158           0           0           0           0
##   1972           0         158           0           0           0           0
##   1973           0         158           0           0           0           0
##   1974           0           0         158           0           0           0
##   1975           0           0         158           0           0           0
##   1976           0           0         157           0           0           0
##   1977           0           0         157           0           0           0
##   1978           0           0           0         157           0           0
##   1979           0           0           0         157           0           0
##   1980           0           0           0         157           0           0
##   1981           0           0           0         157           0           0
##   1982           0           0           0           0         157           0
##   1983           0           0           0           0         157           0
##   1984           0           0           0           0         157           0
##   1985           0           0           0           0         157           0
##   1986           0           0           0           0           0         157
##   1987           0           0           0           0           0         157
##   1988           0           0           0           0           0         157
##   1989           0           0           0           0           0         158
##   1990           0           0           0           0           0           0
##   1991           0           0           0           0           0           0
##   1992           0           0           0           0           0           0
##   1993           0           0           0           0           0           0
##   1994           0           0           0           0           0           0
##   1995           0           0           0           0           0           0
##   1996           0           0           0           0           0           0
##   1997           0           0           0           0           0           0
##       
##        (1989,1993] (1993,1997]
##   1966           0           0
##   1967           0           0
##   1968           0           0
##   1969           0           0
##   1970           0           0
##   1971           0           0
##   1972           0           0
##   1973           0           0
##   1974           0           0
##   1975           0           0
##   1976           0           0
##   1977           0           0
##   1978           0           0
##   1979           0           0
##   1980           0           0
##   1981           0           0
##   1982           0           0
##   1983           0           0
##   1984           0           0
##   1985           0           0
##   1986           0           0
##   1987           0           0
##   1988           0           0
##   1989           0           0
##   1990         172           0
##   1991         173           0
##   1992         174           0
##   1993         175           0
##   1994           0         175
##   1995           0         175
##   1996           0         175
##   1997           0         175
```

```r
equality$period <- as.numeric(as_factor(equality$period))

table(equality$year,equality$period)
```

```
##       
##          1   2   3   4   5   6   7   8
##   1966 156   0   0   0   0   0   0   0
##   1967 156   0   0   0   0   0   0   0
##   1968 156   0   0   0   0   0   0   0
##   1969 156   0   0   0   0   0   0   0
##   1970   0 156   0   0   0   0   0   0
##   1971   0 158   0   0   0   0   0   0
##   1972   0 158   0   0   0   0   0   0
##   1973   0 158   0   0   0   0   0   0
##   1974   0   0 158   0   0   0   0   0
##   1975   0   0 158   0   0   0   0   0
##   1976   0   0 157   0   0   0   0   0
##   1977   0   0 157   0   0   0   0   0
##   1978   0   0   0 157   0   0   0   0
##   1979   0   0   0 157   0   0   0   0
##   1980   0   0   0 157   0   0   0   0
##   1981   0   0   0 157   0   0   0   0
##   1982   0   0   0   0 157   0   0   0
##   1983   0   0   0   0 157   0   0   0
##   1984   0   0   0   0 157   0   0   0
##   1985   0   0   0   0 157   0   0   0
##   1986   0   0   0   0   0 157   0   0
##   1987   0   0   0   0   0 157   0   0
##   1988   0   0   0   0   0 157   0   0
##   1989   0   0   0   0   0 158   0   0
##   1990   0   0   0   0   0   0 172   0
##   1991   0   0   0   0   0   0 173   0
##   1992   0   0   0   0   0   0 174   0
##   1993   0   0   0   0   0   0 175   0
##   1994   0   0   0   0   0   0   0 175
##   1995   0   0   0   0   0   0   0 175
##   1996   0   0   0   0   0   0   0 175
##   1997   0   0   0   0   0   0   0 175
```

```r
# Ser fint ut
```
Da har vi forhåpentligvis variabler som kan fungere som nøkler i begge datasettene. Neste steg er å endre på datastrukturen i datasettet `equality`, slik at den blir lik som i `aid`. For å få til dette, må vi endre observasjonene i `equality` til land-perioder. Dette kan vi gjøre med `group_by` og `summarise()`. På dette stadiet, må vi tenke datastruktur, og gjøre metodologiske valg om hvordan vi skal operasjonalisere informasjonen om konflikter. Under viser jeg to muligheter. I hjemmeoppgaven er dette et punkt der jeg vil anbefale at du tenker grundig gjennom de metodologiske implikasjonene av valgene du tar - tenk gjennom hva som er best og skriv koden din etterpå - ikke fall i fellen kode først, metode etterpå.


```r
agg_equality <- equality %>%
  group_by(country_text_id, period) %>%
  summarise(avg_eq = mean(v2pepwrsoc, na.rm = TRUE)) %>% # regner ut gjennomsnittet for perioden
  mutate(period_num = as.numeric(period))
```

```
## `summarise()` has grouped output by 'country_text_id'. You can override using the `.groups` argument.
```

```r
table(agg_equality$period, agg_equality$period_num)
```

```
##    
##       1   2   3   4   5   6   7   8
##   1 157   0   0   0   0   0   0   0
##   2   0 158   0   0   0   0   0   0
##   3   0   0 158   0   0   0   0   0
##   4   0   0   0 157   0   0   0   0
##   5   0   0   0   0 157   0   0   0
##   6   0   0   0   0   0 158   0   0
##   7   0   0   0   0   0   0 177   0
##   8   0   0   0   0   0   0   0 175
```

```r
agg_equality
```

```
## # A tibble: 1,297 x 4
## # Groups:   country_text_id [179]
##    country_text_id period avg_eq period_num
##    <chr>            <dbl>  <dbl>      <dbl>
##  1 AFG                  1  1.02           1
##  2 AFG                  2  1.02           2
##  3 AFG                  3  1.01           3
##  4 AFG                  4  0.845          4
##  5 AFG                  5  0.845          5
##  6 AFG                  6  0.845          6
##  7 AFG                  7  0.587          7
##  8 AFG                  8 -0.133          8
##  9 AGO                  1 -2.52           1
## 10 AGO                  2 -2.52           2
## # ... with 1,287 more rows
```

Nå som data fra `equality` er i samme format som i `aid`, er vi klare til å kombinere informasjonen med `left_join`:


```r
# husk: ?left_join for å forstå funksjonen
aid2 <- left_join(aid, agg_equality,
                  by = c("country" = "country_text_id", "period" = "period_num")) # Spesifiserer nøkkelvariablene
# Sjekker missing:
table(is.na(aid2$avg_eq))
```

```
## 
## FALSE  TRUE 
##   325     6
```

```r
# 6 missing pga observasjonen som mangler

summary(aid2$avg_eq)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
## -2.2160 -0.6338  0.0530  0.1102  0.8920  2.3890       6
```
