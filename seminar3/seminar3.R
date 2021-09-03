#' ---
#' title: "Seminar 3"
#' output:
#'   github_document
#' #    keep_md: yes
#' #    self_contained: no
#' #    keep_html: no
#' editor_options: 
#'   chunk_output_type: console
#' ---
#' 
#' I dag skal vi fortsette med OLS og databehandling:
#' 1. Hvordan plotter vi resultater fra OLS?
#' 2. Hvordan bruker vi R til å sjekke om forutsetningene for OLS holder?
#' 3. Hvordan slår vi sammen flere datasett? 
#' 
#' Først: er det noen spørsmål til det vi gikk gjennom i går? Dersom du synes manipulering av data er vanskelig så kan det hjelpe å ta en titt på kapittel seks i **Lær deg R**. Dersom du er nysgjerrig på flere måter å omkode variabler på så kan du kikke på kapittel 5 i [**R for Data Science**](https://r4ds.had.co.nz/transform.html#add-new-variables-with-mutate). Og ikke glem: internett er din venn når du skal lære R. 
#' 
#' ## Hvordan plotte resutlater fra OLS? 
#' I dag skal vi plotte resultatene og gjøre regresjonsdiagnostikk på modellen fra **Burnside and Dollar** - samme artikkel som vi har brukt tidligere i uka og samme som dere repliserte i oppgaven i går. Først laster vi inn pakker, data og kjører modellen.  
#' 
## ----------------------------------------------------------------------------------------------------------------------
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

#' 
#' Så plotter vi effekten av institusjonell kvalitet på vekst i BNP (GDP). Vi går ikke veldig nøye inn på dette nå, men les gjerne [denne guiden til regresjonsplot](https://github.com/liserodland/stv4020aR/blob/master/Materiell%20fra%20tidl%20semestre/docs/Regresjonsplot.md). For å plotte en regresjonslinje så oppretter vi først et datasett der vi holder alle uavhengige variabler, bortsett fra den vi vil plotte effekten til, konstante. Her velger jeg å la `institutional_quality` variere fra minimums- til maksimumsverdien og setter resten av variablene til gjennomsnitt eller modusverdi. Neste steg er å predikere verdier for det nye datasettet basert på modellen vår ved hjelp av `predict()`. `predict()` tar datasettet vi har laget og gir oss blant annet predikerte verdier og konfidensintervaller basert på modellen vår. For å få datasettet vi skal bruke til plotting, så binder vi resultatet av `predict` sammen med datasettet vi lagde. For at `predict()` skal gi likt antall observasjoner som vi har i datasettet vårt så er det viktig å bevare informasjon om de observasjonene som har missing. Dette gjør vi med argumentet `na.action = "na.exclude` i `lm()`. 
#' 
## ----tidy=FALSE--------------------------------------------------------------------------------------------------------
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

# Legger predikerte verdier inn i snitt_data
snitt_data <- cbind(snitt_data, predict(m5, newdata = snitt_data, se = TRUE, interval = "confidence"))
snitt_data

#' Variabelen som heter `fit.fit` er de predikerte verdiene. `fit.lwr` og `fit.upr` er nedre og øvre grense for et 95 % konfidensintervall. `se.fit` er standardfeilen. 
#' 
#' 
#' Lager plot:
## ----------------------------------------------------------------------------------------------------------------------
library(ggplot2)
ggplot(snitt_data, aes(x = institutional_quality, y = fit.fit)) + # Setter institusjonell kvalitet på x-aksen og predikert verdi på y-aksen
  geom_line() +                                                   # Sier at jeg vil ha et linjediagram
  scale_y_continuous(breaks = seq(-12, 12, 2)) +                  # Bestemmer verdier og mellomrom på y-aksen
  geom_ribbon(aes(ymin = fit.lwr, ymax = fit.upr, color = NULL), alpha = .2) + # Legger til konfidensintervall på plottet
  labs(x = "Kvalitet på institusjoner", y = "Forventet GDP vekst", color = "Policy", fill = "Policy") # Setter tittel på akser og plot

#' 
#' Dette kan, og bør, også gjøres når det er samspill i modellen. Samspill er vanskelig å tolke i en tabell og jeg synes derfor det er fint å plotte disse. Når vi skal plotte samspill så lar vi begge variablene som er en del av samspillsleddet variere, mens resten er konstante. Vi lar den ene variabelen være `x`, mens vi bruker den andre til å fylle ut argumentet `color`. I tilfellet med to kontinuerlige variabler må en gjøre den ene om til en faktorvariabel slik jeg gjør med policy under. 
#' 
## ----tidy=FALSE--------------------------------------------------------------------------------------------------------
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

# Lagrer predikerte verdier i plot datasettet
snitt_data_sam <- cbind(snitt_data_sam, predict(m5, newdata = snitt_data_sam, se = TRUE, interval = "confidence"))
snitt_data_sam

# Plotter
ggplot(snitt_data_sam, aes(x = aid, y = fit.fit, 
                       group = factor(policy), 
                       color = factor(policy), 
                       fill = factor(policy))) +
  geom_line() +
  scale_y_continuous(breaks = seq(-12, 12, 2)) +
  geom_ribbon(aes(ymin = fit.lwr, ymax = fit.upr, color = NULL), alpha = .2) +
  labs(x = "Bistandsnivå", y = "Forventet GDP vekst", color = "Policy", fill = "Policy")

#' Vi skal ikke bruke snitt_data mer så jeg fjerner objektene fra environment:
#' 
## ----------------------------------------------------------------------------------------------------------------------
rm(snitt_data, snitt_data_sam)

#' 
#' 
#' ## Hvordan slår vi sammen flere datasett? 
#' Når vi skal slå sammen ulike datasett må vi først tenke gjennom hvordan vi kan få en felles nøkkel som lar oss knytte sammen informasjon om observasjonene fra de to datasettene. Dette kan gjøres på flere nivåer. Vi jobber videre med aid-datasettet. 
#' 
## ----------------------------------------------------------------------------------------------------------------------
aid

#' 
#' Ser dere noen variabler her vi kunne brukt som felles nøkkel?
#' 
#' Hvilken variabel vi bruker som nøkkel vil avhenge av variablene i det andre datasettet. Er variablene på landnivå, årnivå, land-år-nivå, region eller noe helt annet? Vi skal nå se på hvordan vi kan slå sammen aid-datasettet med et datasett om konflikt. 
#' 
#' Jeg har lastet ned versjon tid av Varieties of democracy datasettet fra V-den sin [nettside](https://www.v-dem.net/en/data/data-version-10/). I V-dem er det en variabel som heter `v2pepwrsoc`. Denne variabelen måler hvor jevnt makt er fordelt mellom sosiale grupper. Jeg har lastet opp en redusert versjon av V-dem datasettet på github. Det kan du lese inn direkte fra [denne lenken](https://raw.githubusercontent.com/liserodland/stv4020aR/master/H20-seminarer/Innf%C3%B8ringsseminarer/data/Vdem_10_redusert.csv).
#' 
## ----------------------------------------------------------------------------------------------------------------------
equality <- read_csv("https://raw.githubusercontent.com/liserodland/stv4020aR/master/H20-seminarer/Innf%C3%B8ringsseminarer/data/Vdem_10_redusert.csv")

summary(equality$v2pepwrsoc)

equality

# Vi ser at V-dem har en variabel som heter country_text_id og year
# Kanskje vi kan bruke disse?

# Bruker en logisk test og %in% for å sjekke om det finnes en match for alle land i aid-datasettet:
table(aid$country %in% equality$country_text_id)
# Ikke alle matcher. 

#' Når ikke alle observasjonen har en match så kan dette kan enten løses manuelt eller ved hjelp av andre datasett eller R-pakker. 
#' 
## ----------------------------------------------------------------------------------------------------------------------
# For å løse det manuelt så kan du bruke denne koden til å identifisere de som ikke matcher:
aid %>% 
  select(country) %>%  # Velger country variabelen i aid
  anti_join(equality, by = c("country" = "country_text_id")) %>% # Bevarer de verdiene i equality som ikke er aid. 
  unique()

# En nyttig pakke dersom dere kommer over dette problemet kan være countrycode

#' 
#' Vi kommer ikke til å bruke tid i seminar på å rette opp i dette, men her finner dere et eksempel på hvordan det kunne vært løst. Vi går derfor videre vel vitende om at vi ikke klarte å matche alle observasjonen (dette anbefaler jeg **ikke** å gjøre i hjemmeoppgaven). Det er fortsatt en ting vi må gjøre før vi kan slå datasettene sammen. V-dem-datasettet inneholder land-år-observasjoner, mens aid-datasettet inneholder land-periode-observasjoner. Vi må derfor lage en periode-variabel i equality-datasettet. 
#' 
## ----------------------------------------------------------------------------------------------------------------------
# Oppretter periode-variabel i V-dem datasettet, slik at jeg er klar til å merge. Verdiene til period-variabelen går fra 1-8, jeg vil gi de samme periodene (datasettet inneholder imidlertid bare data for periode 2-7). Her bruker jeg et en egenskap ved `as.numeric` på en faktor som ofte fører til feil i kode for å gjøre dette raskt:
table(aid$periodstart, aid$period)
table(aid$periodend, aid$period)
# Det kommer ikke tydelig frem her, men datasettet gikk opprinnelig fra 1966-1998
# Dersom jeg bruker 1966, 1970, 1974, 1978, 1982, 1986, 1990 og 1994 som kuttpunkt,
# bør jeg få de samme gruppene i V-dem-datasettet som i aid

periodcutpoints <-  unique(c(aid$periodstart)) # henter ut ovennevnt årtsall med unique()
# Her buker jeg funksjonen cut(), jeg kunne også brukt ifelse(), men cut() er raskere her.
equality$period <- cut(equality$year, periodcutpoints)
table(equality$year, equality$period)
# Tabellen viser at jeg må justere periodcutpoints for å få rett

periodcutpoints <- periodcutpoints - 1
table(periodcutpoints)

periodcutpoints <- c(1965, periodcutpoints, 1993, 1997) # legger til tre kuttpunkt for å få med periode 1, 7 og 8

# Forsøker på nytt:
equality$period <- cut(equality$year, periodcutpoints)
table(equality$year,equality$period)
equality$period <- as.numeric(as_factor(equality$period))

table(equality$year,equality$period)
# Ser fint ut

#' 
#' Da har vi forhåpentligvis variabler som kan fungere som nøkler i begge datasettene. Neste steg er å endre på datastrukturen i datasettet `equality`, slik at den blir lik som i `aid`. For å få til dette, må vi endre observasjonene i `equality` til land-perioder. Dette kan vi gjøre med `group_by` og `summarise()`. På dette stadiet, må vi tenke datastruktur, og gjøre metodologiske valg om hvordan vi skal operasjonalisere informasjonen om konflikter. Under viser jeg to muligheter. I hjemmeoppgaven er dette et punkt der jeg vil anbefale at du tenker grundig gjennom de metodologiske implikasjonene av valgene du tar - tenk gjennom hva som er best og skriv koden din etterpå - ikke fall i fellen kode først, metode etterpå.
#' 
## ----------------------------------------------------------------------------------------------------------------------
agg_equality <- equality %>%
  group_by(country_text_id, period) %>%
  summarise(avg_eq = mean(v2pepwrsoc, na.rm = TRUE)) %>% # regner ut gjennomsnittet for perioden
  mutate(period_num = as.numeric(period))

table(agg_equality$period, agg_equality$period_num)

agg_equality

#' 
#' Nå som data fra `equality` er i samme format som i `aid`, er vi klare til å kombinere informasjonen med `left_join`:
#' 
## ----------------------------------------------------------------------------------------------------------------------
# husk: ?left_join for å forstå funksjonen
aid2 <- left_join(aid, agg_equality,
                  by = c("country" = "country_text_id", "period" = "period_num")) # Spesifiserer nøkkelvariablene
# Sjekker missing:
table(is.na(aid2$avg_eq))
# 6 missing pga observasjonen som mangler

summary(aid2$avg_eq)

#' 
## ----generere_script, eval=FALSE, echo=FALSE---------------------------------------------------------------------------
## # knitr::purl("./seminar3/seminar3.Rmd", output = "./seminar3/seminar3.R", documentation = 2)

