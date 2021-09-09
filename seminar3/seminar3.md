Seminar 3
================

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.3     v dplyr   1.0.7
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   2.0.0     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
# Laster inn data
load("./aid.RData")

# Gjør de nødvendige omkodingene som dere gjorde i oppgaven
aid <- aid %>% 
  mutate(log_gdp_pr_capita = log(gdp_pr_capita),
         period_fac = as.factor(period),
         region = ifelse(fast_growing_east_asia == 1, "East Asia",
                         ifelse(sub_saharan_africa == 1, "Sub-Saharan Africa", "Other")),
         region = factor(region, levels = c("Other", "Sub-Saharan Africa", "East Asia")))
```

I dag skal vi fortsette med OLS og databehandling: 1. Kjøre OLS-modeller
1. Hvordan plotter vi resultater fra OLS? 3. Hvordan slår vi sammen
flere datasett?

Først: er det noen spørsmål til det vi gikk gjennom i går? Dersom du
synes manipulering av data er vanskelig så kan det hjelpe å ta en titt
på kapittel seks i **Lær deg R**. Dersom du er nysgjerrig på flere måter
å omkode variabler på så kan du kikke på kapittel 5 i [**R for Data
Science**](https://r4ds.had.co.nz/transform.html#add-new-variables-with-mutate).
Og ikke glem: internett er din venn når du skal lære R.

Dersom dere vil skrive en kvantitativ hjemmeoppgave er det **veldig
viktig** å sjekke om forutsetningene for OLS og/eller logistisk modell
er oppfylt. Dette går vi gjennom på [fordypningsseminar
1](https://github.com/martigso/stv4020aR21/blob/main/fordypningsseminar%201/Fordypningsseminar-1-Forutsetninger.md).

## Lineær regresjon (OLS) <a name="ols"></a>

### Syntaks

For å kjøre en lineær regresjon i R, bruker vi funksjonen `lm()`, som
har følgende syntaks:

``` r
lm(avhengig.variabel ~ uavhengig.variabel, data=mitt_datasett)
# på mac får du ~ med alt + k + space
```

La oss se på et eksempel med `aid` datasettet vi har brukt så langt:

``` r
m1 <- lm(gdp_growth ~ aid, data = aid) # lagrer m1 om objekt
summary(m1) # ser på resultatene med summary()
```

    ## 
    ## Call:
    ## lm(formula = gdp_growth ~ aid, data = aid)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -12.813  -2.181   0.144   2.153  15.443 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   1.5570     0.2730   5.704 2.64e-08 ***
    ## aid          -0.2993     0.1036  -2.889  0.00412 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.711 on 323 degrees of freedom
    ##   (6 observations deleted due to missingness)
    ## Multiple R-squared:  0.02519,    Adjusted R-squared:  0.02218 
    ## F-statistic: 8.348 on 1 and 323 DF,  p-value: 0.004122

``` r
class(m1) # Legg merke til at vi har et objekt av en ny klasse!
```

    ## [1] "lm"

``` r
str(m1) # Gir oss informasjon om hva objektet inneholder.
```

    ## List of 13
    ##  $ coefficients : Named num [1:2] 1.557 -0.299
    ##   ..- attr(*, "names")= chr [1:2] "(Intercept)" "aid"
    ##  $ residuals    : Named num [1:325] 0.149 -0.474 -2.665 -4.099 -2.652 ...
    ##   ..- attr(*, "format.stata")= chr "%10.0g"
    ##   ..- attr(*, "names")= chr [1:325] "1" "2" "3" "4" ...
    ##  $ effects      : Named num [1:325] -18.73 -10.72 -2.65 -4.09 -2.64 ...
    ##   ..- attr(*, "format.stata")= chr "%10.0g"
    ##   ..- attr(*, "names")= chr [1:325] "(Intercept)" "aid" "" "" ...
    ##  $ rank         : int 2
    ##  $ fitted.values: Named num [1:325] 1.55 1.55 1.55 1.55 1.55 ...
    ##   ..- attr(*, "format.stata")= chr "%10.0g"
    ##   ..- attr(*, "names")= chr [1:325] "1" "2" "3" "4" ...
    ##  $ assign       : int [1:2] 0 1
    ##  $ qr           :List of 5
    ##   ..$ qr   : num [1:325, 1:2] -18.0278 0.0555 0.0555 0.0555 0.0555 ...
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ : chr [1:325] "1" "2" "3" "4" ...
    ##   .. .. ..$ : chr [1:2] "(Intercept)" "aid"
    ##   .. ..- attr(*, "assign")= int [1:2] 0 1
    ##   ..$ qraux: num [1:2] 1.06 1.05
    ##   ..$ pivot: int [1:2] 1 2
    ##   ..$ tol  : num 1e-07
    ##   ..$ rank : int 2
    ##   ..- attr(*, "class")= chr "qr"
    ##  $ df.residual  : int 323
    ##  $ na.action    : 'omit' Named int [1:6] 79 80 81 296 297 298
    ##   ..- attr(*, "names")= chr [1:6] "79" "80" "81" "296" ...
    ##  $ xlevels      : Named list()
    ##  $ call         : language lm(formula = gdp_growth ~ aid, data = aid)
    ##  $ terms        :Classes 'terms', 'formula'  language gdp_growth ~ aid
    ##   .. ..- attr(*, "variables")= language list(gdp_growth, aid)
    ##   .. ..- attr(*, "factors")= int [1:2, 1] 0 1
    ##   .. .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. .. ..$ : chr [1:2] "gdp_growth" "aid"
    ##   .. .. .. ..$ : chr "aid"
    ##   .. ..- attr(*, "term.labels")= chr "aid"
    ##   .. ..- attr(*, "order")= int 1
    ##   .. ..- attr(*, "intercept")= int 1
    ##   .. ..- attr(*, "response")= int 1
    ##   .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
    ##   .. ..- attr(*, "predvars")= language list(gdp_growth, aid)
    ##   .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
    ##   .. .. ..- attr(*, "names")= chr [1:2] "gdp_growth" "aid"
    ##  $ model        :'data.frame':   325 obs. of  2 variables:
    ##   ..$ gdp_growth: num [1:325] 1.7 1.08 -1.12 -2.55 -1.1 ...
    ##   .. ..- attr(*, "format.stata")= chr "%10.0g"
    ##   ..$ aid       : num [1:325] 0.0182 0.0172 0.024 0.03 0.0157 ...
    ##   .. ..- attr(*, "format.stata")= chr "%10.0g"
    ##   ..- attr(*, "terms")=Classes 'terms', 'formula'  language gdp_growth ~ aid
    ##   .. .. ..- attr(*, "variables")= language list(gdp_growth, aid)
    ##   .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
    ##   .. .. .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. .. .. ..$ : chr [1:2] "gdp_growth" "aid"
    ##   .. .. .. .. ..$ : chr "aid"
    ##   .. .. ..- attr(*, "term.labels")= chr "aid"
    ##   .. .. ..- attr(*, "order")= int 1
    ##   .. .. ..- attr(*, "intercept")= int 1
    ##   .. .. ..- attr(*, "response")= int 1
    ##   .. .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
    ##   .. .. ..- attr(*, "predvars")= language list(gdp_growth, aid)
    ##   .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
    ##   .. .. .. ..- attr(*, "names")= chr [1:2] "gdp_growth" "aid"
    ##   ..- attr(*, "na.action")= 'omit' Named int [1:6] 79 80 81 296 297 298
    ##   .. ..- attr(*, "names")= chr [1:6] "79" "80" "81" "296" ...
    ##  - attr(*, "class")= chr "lm"

### Multivariat regresjon

Vi legger inn flere uavhengige variabler med `+`.

``` r
m2 <- lm(gdp_growth ~ aid + policy + region, data = aid)
summary(m2)
```

    ## 
    ## Call:
    ## lm(formula = gdp_growth ~ aid + policy + region, data = aid)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -12.0631  -1.6756  -0.0298   1.6239  12.9271 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               0.008186   0.332280   0.025   0.9804    
    ## aid                      -0.004915   0.138609  -0.035   0.9717    
    ## policy                    1.157168   0.179478   6.447 4.99e-10 ***
    ## regionSub-Saharan Africa -0.961814   0.529052  -1.818   0.0701 .  
    ## regionEast Asia           1.239202   0.712964   1.738   0.0833 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.204 on 279 degrees of freedom
    ##   (47 observations deleted due to missingness)
    ## Multiple R-squared:  0.2334, Adjusted R-squared:  0.2224 
    ## F-statistic: 21.24 on 4 and 279 DF,  p-value: 2.645e-15

``` r
# Her kombinerer vi summary() og opprettelse av modellobjekt på samme linje
```

### Samspill

Hypotesen til artikkelforfatterne var følgende: *bistand fører til
økonomisk vekst, men bare dersom de fører en god makroøkonomisk
politikk*. Dette kan vi sjekke ved hjelp av samspill, som undersøker
hvor vidt en effekt av en variabel er avhengig av en annen variabel. Vi
legger inn samspill ved å sette `*` (gangetegn) mellom to variabler. De
individuelle regresjonskoeffisientene til variablene vi spesifisere
samspill mellom blir automatisk lagt til.

``` r
m3 <- lm(gdp_growth ~ aid*policy + region, data = aid)
summary(m3)
```

    ## 
    ## Call:
    ## lm(formula = gdp_growth ~ aid * policy + region, data = aid)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -12.0096  -1.7193  -0.0145   1.6436  12.9254 
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                0.2264     0.3718   0.609 0.543033    
    ## aid                       -0.1270     0.1672  -0.760 0.448074    
    ## policy                     0.9362     0.2469   3.792 0.000183 ***
    ## regionSub-Saharan Africa  -1.0056     0.5295  -1.899 0.058552 .  
    ## regionEast Asia            1.5598     0.7535   2.070 0.039359 *  
    ## aid:policy                 0.1399     0.1074   1.302 0.194043    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.2 on 278 degrees of freedom
    ##   (47 observations deleted due to missingness)
    ## Multiple R-squared:  0.2381, Adjusted R-squared:  0.2244 
    ## F-statistic: 17.37 on 5 and 278 DF,  p-value: 5.806e-15

### Andregradsledd og andre omkodinger

Vi kan legge inn andregradsledd eller andre omkodinger av variabler i
regresjonsligningene våre. Annengradsledd er fine hvis vi antar at en
variabels effekt ikke er lineær, men snarere kurvformet. Logaritmiske
transformasjoner brukes gjerne for eksponentiell vekst eller for å
minske skjevhet. Omkoding til kategorisk variabel (faktor) er nyttig
hvis vi antar at variabelen inneholder et sett med distinkte kategorier.

Andregradsledd legger vi inn med `I(uavh.var^2)`, eller via funksjonen
`poly()`. Under har jeg lagt inn en `log()` omkoding, en `as.factor()`
omkoding og et andregradsledd. Merk at dere må legge inn
førstegradsleddet separat når dere legger inn andregradsledd. Dersom en
variabeltransformasjon krever mer enn en enkel funksjon, er det fint å
opprette en ny variabel i datasettet.

``` r
m4 <- lm(gdp_growth ~ log(gdp_growth) + institutional_quality + I(institutional_quality^2) + region + aid*policy +  as_factor(period), 
         data = aid, 
         na.action = "na.exclude")
```

    ## Warning in log(gdp_growth): NaNs produced

``` r
summary(m4)
```

    ## 
    ## Call:
    ## lm(formula = gdp_growth ~ log(gdp_growth) + institutional_quality + 
    ##     I(institutional_quality^2) + region + aid * policy + as_factor(period), 
    ##     data = aid, na.action = "na.exclude")
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.5588 -0.6457 -0.2681  0.2502  5.8270 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                 0.61118    1.38761   0.440    0.660    
    ## log(gdp_growth)             2.19769    0.11311  19.430   <2e-16 ***
    ## institutional_quality       0.04988    0.62256   0.080    0.936    
    ## I(institutional_quality^2)  0.01986    0.06677   0.297    0.766    
    ## regionSub-Saharan Africa    0.23459    0.28910   0.811    0.418    
    ## regionEast Asia             0.26763    0.33783   0.792    0.429    
    ## aid                         0.16008    0.10171   1.574    0.117    
    ## policy                      0.03525    0.12930   0.273    0.785    
    ## as_factor(period)3         -0.26928    0.26755  -1.006    0.316    
    ## as_factor(period)4         -0.31713    0.28787  -1.102    0.272    
    ## as_factor(period)5         -0.28454    0.35779  -0.795    0.428    
    ## as_factor(period)6         -0.42494    0.30570  -1.390    0.166    
    ## as_factor(period)7         -0.40445    0.31995  -1.264    0.208    
    ## aid:policy                 -0.03923    0.05401  -0.726    0.469    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.161 on 166 degrees of freedom
    ##   (151 observations deleted due to missingness)
    ## Multiple R-squared:  0.771,  Adjusted R-squared:  0.7531 
    ## F-statistic:    43 on 13 and 166 DF,  p-value: < 2.2e-16

En nyttig pakke for å lage fine tabeller med resultatet fra
regresjonsanalyser er `stargazer`.

``` r
#install.packages("stargazer")
library(stargazer)
stargazer(m2, m3,
          type = "text") 
```

    ## 
    ## ========================================================================
    ##                                        Dependent variable:              
    ##                          -----------------------------------------------
    ##                                            gdp_growth                   
    ##                                    (1)                     (2)          
    ## ------------------------------------------------------------------------
    ## aid                              -0.005                  -0.127         
    ##                                  (0.139)                 (0.167)        
    ##                                                                         
    ## policy                          1.157***                0.936***        
    ##                                  (0.179)                 (0.247)        
    ##                                                                         
    ## regionSub-Saharan Africa         -0.962*                 -1.006*        
    ##                                  (0.529)                 (0.529)        
    ##                                                                         
    ## regionEast Asia                  1.239*                  1.560**        
    ##                                  (0.713)                 (0.753)        
    ##                                                                         
    ## aid:policy                                                0.140         
    ##                                                          (0.107)        
    ##                                                                         
    ## Constant                          0.008                   0.226         
    ##                                  (0.332)                 (0.372)        
    ##                                                                         
    ## ------------------------------------------------------------------------
    ## Observations                       284                     284          
    ## R2                                0.233                   0.238         
    ## Adjusted R2                       0.222                   0.224         
    ## Residual Std. Error         3.204 (df = 279)        3.200 (df = 278)    
    ## F Statistic              21.237*** (df = 4; 279) 17.371*** (df = 5; 278)
    ## ========================================================================
    ## Note:                                        *p<0.1; **p<0.05; ***p<0.01

``` r
# Om du skriver i word så kan du bruke type="html", lagre i en mappe og åpne i word.
# obs. bruk .htm og ikke .html
stargazer(m2, m4,
          type = "html",
          out = "./bilder/regresjonstabell.htm") 

# Om du skriver i Latex så kan du bruker type = "latex" og kopiere inn output direkte, eller lagre i en mappe og hente inn via latex
stargazer(m2, m4,
          type = "latex") 

# Flere tips om tabeller finner dere i dokumentet Eksportere_tabeller_og_figurer. 
```

## Hvordan plotte resutlater fra OLS?

I dag skal vi plotte resultatene og gjøre regresjonsdiagnostikk på
modellen fra **Burnside and Dollar** - samme artikkel som vi har brukt
tidligere i uka og samme som dere repliserte i oppgaven i går. Først
laster vi inn pakker, data og kjører modellen.

``` r
library(tidyverse)

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
stargazer(m5, type = "text")
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

Så plotter vi effekten av institusjonell kvalitet på vekst i BNP (GDP).
Vi går ikke veldig nøye inn på dette nå, men les gjerne [denne guiden
til
regresjonsplot](https://github.com/liserodland/stv4020aR/blob/master/Materiell%20fra%20tidl%20semestre/docs/Regresjonsplot.md).
For å plotte en regresjonslinje så oppretter vi først et datasett der vi
holder alle uavhengige variabler, bortsett fra den vi vil plotte
effekten til, konstante. Her velger jeg å la `institutional_quality`
variere fra minimums- til maksimumsverdien og setter resten av
variablene til gjennomsnitt eller modusverdi. Neste steg er å predikere
verdier for det nye datasettet basert på modellen vår ved hjelp av
`predict()`. `predict()` tar datasettet vi har laget og gir oss blant
annet predikerte verdier og konfidensintervaller basert på modellen vår.
For å få datasettet vi skal bruke til plotting, så binder vi resultatet
av `predict` sammen med datasettet vi lagde. For at `predict()` skal gi
likt antall observasjoner som vi har i datasettet vårt så er det viktig
å bevare informasjon om de observasjonene som har missing. Dette gjør vi
med argumentet `na.action = "na.exclude` i `lm()`.

<!--fixme: bør vi bruke ggeffects i stedet for? Sparer mange kodelinjer, men gir kanskje mindre forståelse for hva som skjer? Dette var uansett noe mange synes var veldig vanskelig i fjor. -->

``` r
m6 <- lm(data = aid, 
         gdp_growth ~ aid + policy, 
         na.action = "na.exclude")



# Lager datasettet
snitt_data <- data.frame(policy = c(seq(min(aid$policy, na.rm = TRUE), 
                                        max(aid$policy, na.rm =TRUE), by = 0.5)),
                         aid = mean(aid$aid, na.rm = TRUE))

# Bruker predict
predict(m6, newdata = snitt_data, se = TRUE)
```

    ## $fit
    ##          1          2          3          4          5          6          7 
    ## -6.4146256 -5.7494886 -5.0843517 -4.4192148 -3.7540779 -3.0889409 -2.4238040 
    ##          8          9         10         11         12         13         14 
    ## -1.7586671 -1.0935302 -0.4283932  0.2367437  0.9018806  1.5670175  2.2321544 
    ##         15         16         17         18         19 
    ##  2.8972914  3.5624283  4.2275652  4.8927021  5.5578391 
    ## 
    ## $se.fit
    ##         1         2         3         4         5         6         7         8 
    ## 0.9321699 0.8533160 0.7748591 0.6969335 0.6197394 0.5435888 0.4689901 0.3968195 
    ##         9        10        11        12        13        14        15        16 
    ## 0.3286804 0.2676696 0.2198052 0.1950199 0.2019957 0.2379557 0.2923946 0.3569558 
    ##        17        18        19 
    ## 0.4270730 0.5004162 0.5757539 
    ## 
    ## $df
    ## [1] 281
    ## 
    ## $residual.scale
    ## [1] 3.231389

``` r
# Legger predikerte verdier inn i snitt_data
snitt_data <- cbind(snitt_data, predict(m6, newdata = snitt_data, se = TRUE, interval = "confidence"))
snitt_data
```

    ##          policy     aid    fit.fit    fit.lwr    fit.upr    se.fit  df
    ## 1  -4.503521919 1.75757 -6.4146256 -8.2495480 -4.5797031 0.9321699 281
    ## 2  -4.003521919 1.75757 -5.7494886 -7.4291917 -4.0697856 0.8533160 281
    ## 3  -3.503521919 1.75757 -5.0843517 -6.6096170 -3.5590864 0.7748591 281
    ## 4  -3.003521919 1.75757 -4.4192148 -5.7910880 -3.0473416 0.6969335 281
    ## 5  -2.503521919 1.75757 -3.7540779 -4.9739990 -2.5341567 0.6197394 281
    ## 6  -2.003521919 1.75757 -3.0889409 -4.1589640 -2.0189179 0.5435888 281
    ## 7  -1.503521919 1.75757 -2.4238040 -3.3469838 -1.5006242 0.4689901 281
    ## 8  -1.003521919 1.75757 -1.7586671 -2.5397833 -0.9775509 0.3968195 281
    ## 9  -0.503521919 1.75757 -1.0935302 -1.7405185 -0.4465418 0.3286804 281
    ## 10 -0.003521919 1.75757 -0.4283932 -0.9552854  0.0984989 0.2676696 281
    ## 11  0.496478081 1.75757  0.2367437 -0.1959302  0.6694176 0.2198052 281
    ## 12  0.996478081 1.75757  0.9018806  0.5179952  1.2857660 0.1950199 281
    ## 13  1.496478081 1.75757  1.5670175  1.1694006  1.9646344 0.2019957 281
    ## 14  1.996478081 1.75757  2.2321544  1.7637525  2.7005564 0.2379557 281
    ## 15  2.496478081 1.75757  2.8972914  2.3217295  3.4728533 0.2923946 281
    ## 16  2.996478081 1.75757  3.5624283  2.8597815  4.2650751 0.3569558 281
    ## 17  3.496478081 1.75757  4.2275652  3.3868967  5.0682337 0.4270730 281
    ## 18  3.996478081 1.75757  4.8927021  3.9076618  5.8777425 0.5004162 281
    ## 19  4.496478081 1.75757  5.5578391  4.4245009  6.6911772 0.5757539 281
    ##    residual.scale
    ## 1        3.231389
    ## 2        3.231389
    ## 3        3.231389
    ## 4        3.231389
    ## 5        3.231389
    ## 6        3.231389
    ## 7        3.231389
    ## 8        3.231389
    ## 9        3.231389
    ## 10       3.231389
    ## 11       3.231389
    ## 12       3.231389
    ## 13       3.231389
    ## 14       3.231389
    ## 15       3.231389
    ## 16       3.231389
    ## 17       3.231389
    ## 18       3.231389
    ## 19       3.231389

Variabelen som heter `fit.fit` er de predikerte verdiene. `fit.lwr` og
`fit.upr` er nedre og øvre grense for et 95 % konfidensintervall.
`se.fit` er standardfeilen.

Lager plot:

``` r
library(ggplot2)
ggplot(snitt_data, aes(x = policy, y = fit.fit)) + # Setter institusjonell kvalitet på x-aksen og predikert verdi på y-aksen
  geom_line() +                                                   # Sier at jeg vil ha et linjediagram
  scale_y_continuous(breaks = seq(-12, 12, 2)) +                  # Bestemmer verdier og mellomrom på y-aksen
  geom_ribbon(aes(ymin = fit.lwr, ymax = fit.upr, color = NULL), alpha = .2) + # Legger til konfidensintervall på plottet
  labs(x = "Kvalitet på institusjoner", y = "Forventet GDP vekst", color = "Policy", fill = "Policy") # Setter tittel på akser og plot
```

![](seminar3_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

Dette kan, og bør, også gjøres når det er samspill i modellen. Samspill
er vanskelig å tolke i en tabell og jeg synes derfor det er fint å
plotte disse. Når vi skal plotte samspill så lar vi begge variablene som
er en del av samspillsleddet variere, mens resten er konstante. Vi lar
den ene variabelen være `x`, mens vi bruker den andre til å fylle ut
argumentet `color`. I tilfellet med to kontinuerlige variabler må en
gjøre den ene om til en faktorvariabel slik jeg gjør med policy under.

``` r
# Lager plot data
snitt_data_sam <- data.frame(policy = c(rep(-1, 9), rep(0, 9), rep(1, 9)), 
                             aid = rep(0:8, 3))

# Predikerer verdier (løser likningen for modellen)
predict(m6, newdata = snitt_data_sam, se = TRUE)
```

    ## $fit
    ##           1           2           3           4           5           6 
    ## -1.40772464 -1.60473376 -1.80174289 -1.99875201 -2.19576114 -2.39277027 
    ##           7           8           9          10          11          12 
    ## -2.58977939 -2.78678852 -2.98379764 -0.07745079 -0.27445992 -0.47146904 
    ##          13          14          15          16          17          18 
    ## -0.66847817 -0.86548729 -1.06249642 -1.25950554 -1.45651467 -1.65352380 
    ##          19          20          21          22          23          24 
    ##  1.25282306  1.05581393  0.85880480  0.66179568  0.46478655  0.26777743 
    ##          25          26          27 
    ##  0.07076830 -0.12624082 -0.32324995 
    ## 
    ## $se.fit
    ##         1         2         3         4         5         6         7         8 
    ## 0.4501983 0.4094811 0.3956975 0.4115628 0.4539791 0.5164452 0.5926550 0.6779896 
    ##         9        10        11        12        13        14        15        16 
    ## 0.7694189 0.3289391 0.2796934 0.2686288 0.3000003 0.3629662 0.4442909 0.5356771 
    ##        17        18        19        20        21        22        23        24 
    ## 0.6327805 0.7333335 0.2567639 0.2024510 0.1998769 0.2506407 0.3310534 0.4245911 
    ##        25        26        27 
    ## 0.5242751 0.6271817 0.7319528 
    ## 
    ## $df
    ## [1] 281
    ## 
    ## $residual.scale
    ## [1] 3.231389

``` r
# Lagrer predikerte verdier i plot datasettet
snitt_data_sam <- cbind(snitt_data_sam, predict(m6, newdata = snitt_data_sam, se = TRUE, interval = "confidence"))
snitt_data_sam
```

    ##    policy aid     fit.fit    fit.lwr     fit.upr    se.fit  df residual.scale
    ## 1      -1   0 -1.40772464 -2.2939140 -0.52153527 0.4501983 281       3.231389
    ## 2      -1   1 -1.60473376 -2.4107735 -0.79869403 0.4094811 281       3.231389
    ## 3      -1   2 -1.80174289 -2.5806506 -1.02283522 0.3956975 281       3.231389
    ## 4      -1   3 -1.99875201 -2.8088896 -1.18861443 0.4115628 281       3.231389
    ## 5      -1   4 -2.19576114 -3.0893927 -1.30212962 0.4539791 281       3.231389
    ## 6      -1   5 -2.39277027 -3.4093628 -1.37617776 0.5164452 281       3.231389
    ## 7      -1   6 -2.58977939 -3.7563864 -1.42317235 0.5926550 281       3.231389
    ## 8      -1   7 -2.78678852 -4.1213717 -1.45220532 0.6779896 281       3.231389
    ## 9      -1   8 -2.98379764 -4.4983541 -1.46924118 0.7694189 281       3.231389
    ## 10      0   0 -0.07745079 -0.7249483  0.57004672 0.3289391 281       3.231389
    ## 11      0   1 -0.27445992 -0.8250201  0.27610026 0.2796934 281       3.231389
    ## 12      0   2 -0.47146904 -1.0002494  0.05731128 0.2686288 281       3.231389
    ## 13      0   3 -0.66847817 -1.2590113 -0.07794502 0.3000003 281       3.231389
    ## 14      0   4 -0.86548729 -1.5799653 -0.15100930 0.3629662 281       3.231389
    ## 15      0   5 -1.06249642 -1.9370574 -0.18793547 0.4442909 281       3.231389
    ## 16      0   6 -1.25950554 -2.3139549 -0.20505614 0.5356771 281       3.231389
    ## 17      0   7 -1.45651467 -2.7021064 -0.21092289 0.6327805 281       3.231389
    ## 18      0   8 -1.65352380 -3.0970483 -0.20999934 0.7333335 281       3.231389
    ## 19      1   0  1.25282306  0.7473983  1.75824785 0.2567639 281       3.231389
    ## 20      1   1  1.05581393  0.6573008  1.45432707 0.2024510 281       3.231389
    ## 21      1   2  0.85880480  0.4653587  1.25225095 0.1998769 281       3.231389
    ## 22      1   3  0.66179568  0.1684240  1.15516740 0.2506407 281       3.231389
    ## 23      1   4  0.46478655 -0.1868729  1.11644603 0.3310534 281       3.231389
    ## 24      1   5  0.26777743 -0.5680055  1.10356038 0.4245911 281       3.231389
    ## 25      1   6  0.07076830 -0.9612370  1.10277357 0.5242751 281       3.231389
    ## 26      1   7 -0.12624082 -1.3608117  1.10833004 0.6271817 281       3.231389
    ## 27      1   8 -0.32324995 -1.7640567  1.11755685 0.7319528 281       3.231389

``` r
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

![](seminar3_files/figure-gfm/unnamed-chunk-12-1.png)<!-- --> Vi skal
ikke bruke snitt\_data mer så jeg fjerner objektene fra environment:

``` r
rm(snitt_data, snitt_data_sam)
```

## Hvordan slår vi sammen flere datasett?

Når vi skal slå sammen ulike datasett må vi først tenke gjennom hvordan
vi kan få en felles nøkkel som lar oss knytte sammen informasjon om
observasjonene fra de to datasettene. Dette kan gjøres på flere nivåer.
Vi jobber videre med aid-datasettet.

``` r
aid
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

Ser dere noen variabler her vi kunne brukt som felles nøkkel?

Hvilken variabel vi bruker som nøkkel vil avhenge av variablene i det
andre datasettet. Er variablene på landnivå, årnivå, land-år-nivå,
region eller noe helt annet? Vi skal nå se på hvordan vi kan slå sammen
aid-datasettet med et datasett om konflikt.

Jeg har lastet ned versjon ti av Varieties of democracy datasettet fra
V-den sin [nettside](https://www.v-dem.net/en/data/data-version-10/). I
V-dem er det en variabel som heter `v2pepwrsoc`. Denne variabelen måler
hvor jevnt makt er fordelt mellom sosiale grupper. Jeg har lastet opp en
redusert versjon av V-dem datasettet på github. Det kan du lese inn
direkte fra [denne
lenken](https://raw.githubusercontent.com/liserodland/stv4020aR/master/H20-seminarer/Innf%C3%B8ringsseminarer/data/Vdem_10_redusert.csv).

``` r
equality <- read_csv("https://raw.githubusercontent.com/liserodland/stv4020aR/master/H20-seminarer/Innf%C3%B8ringsseminarer/data/Vdem_10_redusert.csv")
```

    ## Rows: 5163 Columns: 5

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (2): country_name, country_text_id
    ## dbl (3): country_id, year, v2pepwrsoc

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
summary(equality$v2pepwrsoc)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -2.9150 -0.6210  0.2460  0.3182  1.2780  3.2060

``` r
equality
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

``` r
# Vi ser at V-dem har en variabel som heter country_text_id og year
# Kanskje vi kan bruke disse?

# Bruker en logisk test og %in% for å sjekke om det finnes en match for alle land i aid-datasettet:
table(aid$country %in% equality$country_text_id)
```

    ## 
    ## FALSE  TRUE 
    ##     6   325

``` r
# Ikke alle matcher. 
```

Når ikke alle observasjonen har en match så kan dette kan enten løses
manuelt eller ved hjelp av andre datasett eller R-pakker.

``` r
# For å løse det manuelt så kan du bruke denne koden til å identifisere de som ikke matcher:
aid %>% 
  select(country) %>%  # Velger country variabelen i aid
  anti_join(equality, by = c("country" = "country_text_id")) %>% # Bevarer de verdiene i equality som ikke er aid. 
  unique()
```

    ## # A tibble: 1 x 1
    ##   country
    ##   <chr>  
    ## 1 ZAR

``` r
# En nyttig pakke dersom dere kommer over dette problemet kan være countrycode
```

Vi kommer ikke til å bruke tid i seminar på å rette opp i dette, men her
finner dere et eksempel på hvordan det kunne vært løst. Vi går derfor
videre vel vitende om at vi ikke klarte å matche alle observasjonen
(dette anbefaler jeg **ikke** å gjøre i hjemmeoppgaven). Det er fortsatt
en ting vi må gjøre før vi kan slå datasettene sammen. V-dem-datasettet
inneholder land-år-observasjoner, mens aid-datasettet inneholder
land-periode-observasjoner. Vi må derfor lage en periode-variabel i
equality-datasettet.

``` r
# Oppretter periode-variabel i V-dem datasettet, slik at jeg er klar til å merge. Verdiene til period-variabelen går fra 1-8, jeg vil gi de samme periodene (datasettet inneholder imidlertid bare data for periode 2-7). Her bruker jeg et en egenskap ved `as.numeric` på en faktor som ofte fører til feil i kode for å gjøre dette raskt:
table(aid$periodstart, aid$period)
```

    ##       
    ##         2  3  4  5  6  7
    ##   1970 56  0  0  0  0  0
    ##   1974  0 56  0  0  0  0
    ##   1978  0  0 56  0  0  0
    ##   1982  0  0  0 56  0  0
    ##   1986  0  0  0  0 54  0
    ##   1990  0  0  0  0  0 53

``` r
table(aid$periodend, aid$period)
```

    ##       
    ##         2  3  4  5  6  7
    ##   1973 56  0  0  0  0  0
    ##   1977  0 56  0  0  0  0
    ##   1981  0  0 56  0  0  0
    ##   1985  0  0  0 56  0  0
    ##   1989  0  0  0  0 54  0
    ##   1993  0  0  0  0  0 53

``` r
# Det kommer ikke tydelig frem her, men datasettet gikk opprinnelig fra 1966-1998
# Dersom jeg bruker 1966, 1970, 1974, 1978, 1982, 1986, 1990 og 1994 som kuttpunkt,
# bør jeg få de samme gruppene i V-dem-datasettet som i aid

periodcutpoints <-  unique(c(aid$periodstart)) # henter ut ovennevnt årtsall med unique()
# Her buker jeg funksjonen cut(), jeg kunne også brukt ifelse(), men cut() er raskere her.
equality$period <- cut(equality$year, periodcutpoints)
table(equality$year, equality$period)
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

``` r
# Tabellen viser at jeg må justere periodcutpoints for å få rett

periodcutpoints <- periodcutpoints - 1
table(periodcutpoints)
```

    ## periodcutpoints
    ## 1969 1973 1977 1981 1985 1989 
    ##    1    1    1    1    1    1

``` r
periodcutpoints <- c(1965, periodcutpoints, 1993, 1997) # legger til tre kuttpunkt for å få med periode 1, 7 og 8

# Forsøker på nytt:
equality$period <- cut(equality$year, periodcutpoints)
table(equality$year,equality$period)
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

``` r
equality$period <- as.numeric(as_factor(equality$period))

table(equality$year,equality$period)
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

``` r
# Ser fint ut
```

Da har vi forhåpentligvis variabler som kan fungere som nøkler i begge
datasettene. Neste steg er å endre på datastrukturen i datasettet
`equality`, slik at den blir lik som i `aid`. For å få til dette, må vi
endre observasjonene i `equality` til land-perioder. Dette kan vi gjøre
med `group_by` og `summarise()`. På dette stadiet, må vi tenke
datastruktur, og gjøre metodologiske valg om hvordan vi skal
operasjonalisere informasjonen om konflikter. Under viser jeg to
muligheter. I hjemmeoppgaven er dette et punkt der jeg vil anbefale at
du tenker grundig gjennom de metodologiske implikasjonene av valgene du
tar - tenk gjennom hva som er best og skriv koden din etterpå - ikke
fall i fellen kode først, metode etterpå.

``` r
agg_equality <- equality %>%
  group_by(country_text_id, period) %>%
  summarise(avg_eq = mean(v2pepwrsoc, na.rm = TRUE)) %>% # regner ut gjennomsnittet for perioden
  mutate(period_num = as.numeric(period))
```

    ## `summarise()` has grouped output by 'country_text_id'. You can override using the `.groups` argument.

``` r
table(agg_equality$period, agg_equality$period_num)
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

``` r
agg_equality
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

Nå som data fra `equality` er i samme format som i `aid`, er vi klare
til å kombinere informasjonen med `left_join`:

``` r
# husk: ?left_join for å forstå funksjonen
aid2 <- left_join(aid, agg_equality,
                  by = c("country" = "country_text_id", "period" = "period_num")) # Spesifiserer nøkkelvariablene
# Sjekker missing:
table(is.na(aid2$avg_eq))
```

    ## 
    ## FALSE  TRUE 
    ##   325     6

``` r
# 6 missing pga observasjonen som mangler


# Sjekker hvilke land som har missing
table(aid2$country[which(is.na(aid2$avg_eq))])
```

    ## 
    ## ZAR 
    ##   6

``` r
summary(aid2$avg_eq)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ## -2.2160 -0.6338  0.0530  0.1102  0.8920  2.3890       6
