library(tidyverse)

# Laster inn data
load("./aid.RData")

# Omkoder regionvariabelen:
aid <- aid %>% 
  mutate(region = ifelse(fast_growing_east_asia == 1, "East Asia",
                         ifelse(sub_saharan_africa == 1, "Sub-Saharan Africa", "Other")),
         region = factor(region, levels = c("Other", "Sub-Saharan Africa", "East Asia")))



## lm(avhengig.variabel ~ uavhengig.variabel, data=mitt_datasett)
## # på mac får du ~ med alt + k + space

m1 <- lm(gdp_growth ~ aid, data = aid) # lagrer m1 som objekt
summary(m1) # ser på resultatene med summary()
class(m1) # Legg merke til at vi har et objekt av en ny klasse!
str(m1) # Gir oss informasjon om hva objektet inneholder.

m2 <- lm(gdp_growth ~ aid + policy + region, data = aid)
summary(m2)

m3 <- lm(gdp_growth ~ aid * policy + region, data = aid)
summary(m3)

m4 <- lm(gdp_growth ~ log(gdp_pr_capita) + institutional_quality + I(institutional_quality^2) + region + aid * policy +  as_factor(period), 
         data = aid,
         na.action = "na.exclude")
summary(m4)

#install.packages("stargazer")
library(stargazer)
stargazer(m2, m3,
          type = "text") 


## # Om du skriver i word så kan du bruke type="html", lagre i en mappe og åpne i word.
## # obs. bruk .htm og ikke .html i filnavnet
## stargazer(m2, m4,
##           type = "html",
##           out = "./bilder/regresjonstabell.htm")
## 
## # Om du skriver i Latex så kan du bruker type = "latex" og kopiere inn output direkte, eller lagre i en mappe og hente inn via latex
## stargazer(m2, m4,
##           type = "latex")
## 
## # Flere tips om tabeller finner dere i dokumentet Eksportere_tabeller_og_figurer.

# Kjører en redusert modell
m6 <- lm(data = aid, 
         gdp_growth ~ aid + policy, 
         na.action = "na.exclude")



# Lager datasettet
snitt_data <- data.frame(policy = c(seq(min(aid$policy, na.rm = TRUE), 
                                        max(aid$policy, na.rm =TRUE), by = 0.5)),
                         aid = mean(aid$aid, na.rm = TRUE))

# Bruker predict
predict(m6, newdata = snitt_data, se = TRUE)

# Legger predikerte verdier inn i snitt_data
snitt_data <- cbind(snitt_data, predict(m6, newdata = snitt_data, se = TRUE, interval = "confidence"))
snitt_data

ggplot(snitt_data, aes(x = policy, y = fit.fit)) + # Setter institusjonell kvalitet på x-aksen og predikert verdi på y-aksen
  geom_line() +                                                   # Sier at jeg vil ha et linjediagram
  scale_y_continuous(breaks = seq(-12, 12, 2)) +                  # Bestemmer verdier og mellomrom på y-aksen
  geom_ribbon(aes(ymin = fit.lwr, ymax = fit.upr, color = NULL), alpha = .2) + # Legger til konfidensintervall på plottet
  labs(x = "Policy index", y = "Forventet GDP vekst") # Setter tittel på akser og plot

# Kjører en redusert modell med samspill
m7 <- lm(data = aid, 
         gdp_growth ~ aid * policy, 
         na.action = "na.exclude")

# Lager plot data
snitt_data_sam <- data.frame(policy = c(rep(-1, 9), rep(0, 9), rep(1, 9)), 
                             aid = rep(0:8, 3))

# Predikerer verdier (løser likningen for modellen)
predict(m7, newdata = snitt_data_sam, se = TRUE)

# Lagrer predikerte verdier i plot datasettet
snitt_data_sam <- cbind(snitt_data_sam, predict(m7, newdata = snitt_data_sam, se = TRUE, interval = "confidence"))
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

rm(snitt_data, snitt_data_sam)

aid

equality <- read_csv("https://raw.githubusercontent.com/liserodland/stv4020aR/master/H20-seminarer/Innf%C3%B8ringsseminarer/data/Vdem_10_redusert.csv")

summary(equality$v2pepwrsoc)

equality

# Vi ser at V-dem har en variabel som heter country_text_id og year
# Kanskje vi kan bruke disse?

# Bruker en logisk test og %in% for å sjekke om det finnes en match for alle land i aid-datasettet:
table(aid$country %in% equality$country_text_id)
# Ikke alle matcher. 

# For å løse det manuelt så kan du bruke denne koden til å identifisere de som ikke matcher:
aid %>% 
  select(country) %>%  # Velger country variabelen i aid
  anti_join(equality, by = c("country" = "country_text_id")) %>% # Bevarer de verdiene i equality som ikke er aid. 
  unique()

# En nyttig pakke dersom dere kommer over dette problemet kan være countrycode

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

agg_equality <- equality %>%
  group_by(country_text_id, period) %>%
  summarise(avg_eq = mean(v2pepwrsoc, na.rm = TRUE)) %>% # regner ut gjennomsnittet for perioden
  mutate(period_num = as.numeric(period))

table(agg_equality$period, agg_equality$period_num)

agg_equality

# husk: ?left_join for å forstå funksjonen
aid2 <- left_join(aid, agg_equality,
                  by = c("country" = "country_text_id", "period" = "period_num")) # Spesifiserer nøkkelvariablene
# Sjekker missing:
table(is.na(aid2$avg_eq))
# 6 missing pga observasjonen som mangler


# Sjekker hvilke land som har missing med base
table(aid2$country[which(is.na(aid2$avg_eq))])

# Sjekker hvilke land som har missing med tidyverse
aid2 %>% 
  filter(is.na(avg_eq)) %>% 
  select(country) 

# Henter ut informasjon om variabelen i det nye datasettet
summary(aid2$avg_eq)

## # knitr::purl("./seminar3/seminar3.Rmd", output = "./seminar3/seminar3.R", documentation = 0)
