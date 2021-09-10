## library(tidyverse) # read_funksjoner fra readr i tidyvsere
## datasett <- read_filtype("filnavn.filtype")
## read_csv("filnavn.csv") # for .csv, sjekk også read.table
## load("") # For filer i R-format.
## 
## library(haven) # filformat fra andre statistiske pakker (SAS, SPSS og STATA)
## # Fra haven-pakken - dette skal vi se på i senere seminar
## read_spss("filnavn.sav")  # for .sav-filer fra spss
## read_dta("filnavn.dta") # for .dta-filer fra stata
## 

library(tidyverse)
library(haven)
aid <-  read_dta("aid.dta")


str(aid) # Gir deg infor om variabelnavn, klasse m.m.
aid # Printer samme informasjon som `str()` gjør for en data.frame hvis det er en tibble (dvs. en spesiell type objekt fra tidyverse)
head(aid, 10) # første 10 observasjoner
tail(aid, 10) # siste 10 observasjoner
sample_n(aid, 10) # Velg 10 observasjoner tilfeldig

names(aid) # Printer variabelnavnene

table(complete.cases(aid)) # tester hvor mange observasjoner(rader) som ikke har noen missing på noen variabler
table(is.na(aid$gdp_growth))  # tester hvor mange observasjoner som har missing på variabelen gdp_growth


## table(!is.na(aid$gdp_growth)) # tester hvor mange observasjoner som _ikke_ har missing på variabelen elrdgdpg
## table(is.na(aid$gdp_growth) == FALSE) # akkurat samme resultat som over, med en annen metode

## data$ny_var <- funksjon(data$gammel_var)
## # Vi anvender en funksjon som omarbeider informasjonen i en gammel variabel i datasettet vårt, og legger den til datasettet vårt med et nytt navn

# oppretter alternativ policy-indeks variabel
aid$policy_index <- aid$inflation + aid$budget_balance + aid$economic_open # Eksempel i tråd med det som er beskrevet i "Lær deg R"

aid %>% # Spesifiserer at vi skal jobbe med datasettet aid - R vil da lete etter variabler vi referer til her, slik at vi slipper aid$var
  mutate(policy_index = economic_open + inflation + budget_balance) # lager variabelen policy ved å summere budsjettbalanse, inflasjon og en indeks for øk. åpenhet



aid <- aid %>% # samme kode som over, men nå overskriver jeg variabelen jeg lagde i stad - gjør dette etter at du har testet at koden fungerte
  mutate(policy_index = economic_open + inflation + budget_balance,
         policy_sent = policy - mean(policy, na.rm = TRUE)) %>% 
  rename(policy2 = policy_index)
# Her lager jeg to versjoner av policyindeksen - først en additiv indeks og en sentrert variant av denne.
# På siste rad endrer vi navn fra policy_index til policy2
# Dette er en ryddig måte å samle alle omkodinger på!


aid$period_fac <- as.factor(aid$period)
aid$country_num <- as.numeric(as.factor(aid$country)) # Denne fungerer bare hvis variabelen inneholder noe som kan leses som tall, legg merke til hva den gjør med faktor-variabler!
aid$gdp_growth_chr <- as.character(aid$gdp_growth)

## data$nyvar <- ifelse(test = my_data$my.variabel == "some logical condition",
##        yes  = "what to return if 'some condition' is TRUE",
##        no   = "what to return if 'some condition' is FALSE")

table(aid$periodstart) # Sjekker mulige verdier
aid <- aid %>%  # Jeg vil jobbe med aid datasettet og lagre endringene
  mutate(decade = ifelse(periodstart < 1980, "70s", 
                         ifelse(periodstart > 1980 & periodstart < 1990, "80s", "90s"))) 

# sjekker at det ser fint ut med en tabell der jeg også får opp missing-verdiene
# Når du omkoder en variabel er det spesielt viktig å sjekke missingverdier

table(aid$decade, aid$periodstart, useNA = "always")


# OBS! Her skriver vi over det opprinnelige objektet vårt. Når du skriver hjemmeoppgaven så 
# sjekk først at det blir riktig før du gjør det samme. 

aid <- aid %>% # Forteller at vi skal jobbe med aid-datasettet
       mutate(region = ifelse(sub_saharan_africa == 1, "Sub-Saharan Africa",
                               ifelse(central_america == 1, "Central America",
                                      ifelse(fast_growing_east_asia == 1, "East Asia", "Other"))))
# Her nøster jeg ifelse-funksjoner inne i hverandre, ved å skrive en ifelse() funksjon med det som skal gjøres med observasjoner som får FALSE på at de ligger i Afrika sør for Sahara, osv. La oss sjekke omkodingen med en tabell
table(aid$region, aid$sub_saharan_africa, useNA = "always") # ser at det er like mange land - kunne gjort det samme for resten av kategoriene

aid %>%
   group_by(region) %>% # grupperer observasjoner basert på verdi på region-variabelen. Alle observasjoner med lik verdi (uavh. av tidsperiode) blir gruppert sammen.
   summarise(neigh_growth = mean(gdp_growth, na.rm = T), # regner gjennomsnitt for økonomisk vekst innad i hver gruppe - for hele tidsperioden data dekker sett under ett
             n_region = n()) # Teller antall observasjoner i hvert gruppe


# Samme kode, men lagrer som et objekt - vi får et nytt datasett der vi har lagt til variablene
# OBS! Her skriver vi over det opprinnelige objektet vårt. Når du skriver hjemmeoppgaven så 
# sjekk først at det blir riktig før du gjør det samme. 
aid <- aid %>%
  group_by(region) %>%
  mutate(neigh_growth = mean(gdp_growth, na.rm = T), # Her bruker jeg mutate for å legge variabelen til
          n_region = n()) %>% 
  ungroup() # Vi bruker ungroup() for å fortelle R at vi nå vil bruke dataene på det opprinnelige nivået igjen (her: land)

# Sjekker resultatet
table(aid$neigh_growth, aid$region, useNA = "always")



min(aid$gdp_growth, na.rm = TRUE)  # minimumsverdi, na.rm = T spesifiserer at missing skal droppes i beregning.
max(aid$gdp_growth, na.rm = TRUE)  # maksimumsverdi
mean(aid$gdp_growth, na.rm = TRUE) # gjennomsnitt
median(aid$gdp_growth, na.rm = T)  # median
sd(aid$gdp_growth, na.rm = T)      # standardavvik
var(aid$gdp_growth, na.rm = T)     # varians

#install.packages("moments")
library(moments)
skewness(aid$gdp_growth, na.rm = T) # skjevhet - fra moments
kurtosis(aid$gdp_growth, na.rm = T) # kurtose - fra moments

summary(aid$gdp_growth) # forskjellig deskriptiv statistikk for en variabel
summary(aid)            # deskriptiv statistikk for alle variabler i datasettet

cor(aid$gdp_growth, aid$aid, use = "pairwise.complete.obs") # argumentet use bestemmer missing-håndtering
cor.test(aid$gdp_growth, aid$gdp_pr_capita) # Denne gir deg også signifikans og konfidensintervaller

str(aid)         # sjekker hvilke variabler som er numeriske, str(aid hvis du ikke har en tibble)

aid %>%
select(6:13) %>% # Her tar vi med variablene fra gdp_growth (nr 6) til aid (nr 13)
  cor(., use = "pairwise.complete.obs")  # korrelasjonsmatrise basert på numeriske variabler
# Sjekk hva use = argumentet styrer i hjelpefilen

table(aid$region)      # frekvenstabell
prop.table(table(aid$region)) # prosentfordeling basert på frekvenstabell

table(aid$gdp_growth > median(aid$gdp_growth, na.rm = TRUE))
table(aid$gdp_growth > median(aid$gdp_growth, na.rm = TRUE), aid$country)

## # Laster inn tidyverse
## library(tidyverse)
## 
## # Forslag til løsning for å se på forskjeller i korrelasjon mellom regioner
## aid_korr <- aid %>%
##   group_by(region) %>%
##   summarise(cor_aid_growth = cor(aid, gdp_growth, use = "pairwise"),
##             cor_aid_policy = cor(aid, policy, use = "pairwise"),
##             cor_policy_growth = cor(policy, gdp_growth, use = "pairwise"))
## aid_korr
## 
## # Lager nye grupperingsvariabler
## aid <- aid %>%
##   mutate(good_policy = ifelse(policy > 0, 1, 0),
##          good_policy2 = ifelse(policy2 > 0, 1, 0))
## 
## # Lager nytt datasett for å se på forskjell mellom de med negativ og positiv verdi
## aid_korr2 <- aid %>%
##   group_by(good_policy, good_policy2) %>%
##   summarise(cor_aid_policy = cor(aid, policy))
## 
## aid_korr2
## 
## 

## ggplot(data = my_data, aes(x = x-axis_var_name, y = y-axis_var_name, col = my.var3)) +
##   geom_point()

## # install.packages("ggplot2")
## library(ggplot2)
## ggplot(aid, aes(x = gdp_growth)) +
##   geom_histogram(bins = 50) # lager histogram

ggplot(aid, aes(x = gdp_growth)) + 
  geom_histogram(bins = 50) # lager histogram
ggsave("./bilder/seminar2_1.png")
knitr::include_graphics("./bilder/seminar2_1.png")

## ggplot(aid, aes(x = as.factor(region), y = aid)) +
##   geom_boxplot()

ggplot(aid, aes(x = as.factor(region), y = aid)) + 
  geom_boxplot() # lager histogram
ggsave("./bilder/seminar2_2.png")
knitr::include_graphics("./bilder/seminar2_2.png")

## ggplot(aid, aes(x = period, y = gdp_growth, col = country)) +
##   geom_line() +
##   theme(legend.key.size = unit(0.5,"line")) # Her justerer jeg størrelsen på legend for å få plass til alt

ggplot(aid, aes(x = period, y = gdp_growth, col = country)) + 
  geom_line() +
  theme(legend.title = element_blank(),
        legend.key.size = unit(0.5,"line"))
ggsave("./bilder/seminar2_3.png")
knitr::include_graphics("./bilder/seminar2_3.png")

## # Hvilke land finnes i Sub-Saharan Africa? Velger land kun herfra:
## aid %>%
##   filter(region == "Sub-Saharan Africa") %>%
##   ggplot() + geom_line(aes(x = period, y = gdp_growth, col = country))
## 
## # Fortsatt litt mye informasjon til å være enkelt å lese - La oss sammenligne 5 land med %in%


aid %>% 
  filter(region == "Sub-Saharan Africa") %>%
  ggplot() + geom_line(aes(x = period, y = gdp_growth, col = country))# lager histogram
ggsave("./bilder/seminar2_4.png")
knitr::include_graphics("./bilder/seminar2_4.png")

## 
## # Velger land med %in%, fint for mindre sammenligninger
## aid %>%
##   filter(country %in% c("KEN", "ETH", "GHA", "SOM", "TZA")) %>%
##   ggplot() + geom_line(aes(x = period, y = gdp_growth, col = country))
## 


aid %>% 
  filter(country %in% c("KEN", "ETH", "GHA", "SOM", "TZA")) %>%
  ggplot(., aes(x = period, y = gdp_growth, col = country)) + 
  geom_line()
ggsave("./bilder/seminar2_5.png")
knitr::include_graphics("./bilder/seminar2_5.png")

ggplot(aid, aes(x = aid, y = gdp_growth, col = policy)) + 
  geom_point()


## ggplot(aid, aes(x=aid, y=gdp_growth, col=policy, shape=as.factor(region))) +
##   geom_point() +
##   geom_smooth(method="lm") +  # merk: geom_smooth gir bivariat regresjon
##   ggtitle("Visualization of relationship between aid and growth to showcase ggplot") +
##   xlab("aid") +
##   ylab("growth") +
##   theme_minimal()

ggplot(aid, aes(x=aid, y=gdp_growth, col=policy, shape=as.factor(region))) +
  geom_point() +
  geom_smooth(method="lm") +  # merk: geom_smooth gir bivariat regresjon
  ggtitle("Visualization of relationship between aid and growth to showcase ggplot") +
  xlab("aid") +
  ylab("growth") +
  theme_minimal()
ggsave("./bilder/seminar2_6.png")
knitr::include_graphics("./bilder/seminar2_6.png")

## ggsave("testplot.png", width = 8, height = 5) # lagrer ditt siste ggplot i det formatet du vil på working directory

## ggplot(aid,aes(x = aid, y = gdp_growth, col = policy)) +
##   geom_point() +
##   facet_wrap(~ region)

## save(aid, file = "aid.Rdata")

## # knitr::purl("./seminar2/seminar2.Rmd", output = "./seminar2/seminar2.R", documentation = 0)
