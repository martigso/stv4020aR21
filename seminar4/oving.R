

### Forskningsspørsmål: Er det en positiv sammenheng mellom høy minstelønn og finansiell sikkerhet i EU-land?


### IMPORT ###

# Fyll ut det som mangler: 

install.___("eurostat") # Installer pakken "eurostat"

## Eurostat er det sentrale statistiske organet til EU.
## De har laget en egen R-pakke hvor vi kan hente data direkte fra deres database med kode
## For full oversikt over databasen til Eurostat, se: https://ec.europa.eu/eurostat/web/main/data/database 


___(eurostat) # Hent inn pakken vi akkurat installerte til R


# For å hente inn datasett til R gjennom Eurostat-pakken bruker vi funksjonen "get_eurostat".
# Pass på at du er koblet til internett når du kjører funksjonen.
# Kodene i fnutter er id til datasettene vi bruker (de er å finne i Eurostat sin database, se lenken over).

# Kall datasettene vi laster inn for unexp, pop og minwage

___ <- get_eurostat("ilc_mdes04") # Percentage of population with inability to face unexpected financial expenses
___ <- get_eurostat("demo_pjan") # Population on 1 January by age and sex
___ <- get_eurostat("earn_mw_cur") # Monthly minimum wages - bi-annual data


### TIDY ###

## 1. Skaffe oversikt over data

head(unexp) # Printer de seks første radene til datasettet unexp
glimpse(unexp) # Ser på variabeltyper, antall rader og antall kolonner til datasettet unexp

___(___) # Print de seks første radene til datasettet pop
___(___) # Se på variabeltyper, antall rader og antall kolonner til datasettet pop

___(___) # Print de seks første radene til datasettet minwage
___(___) # Se på variabeltyper, antall rader og antall kolonner til datasettet minwage

names(___) # Sett et av datasettene inn i denne funksjonen. Hva er det funksjonen forteller oss om datasettet?

nrow(___) # Sett inn et annet datasett her. Hva forteller denne funksjonen oss?


## 2. Se på missingverdier

unexp %>%
  ___.cases() # Print en tabell som viser oss alle enheter med missingverdi på minst en variabel

minwage %>%
  ___(values) %>% # Hent ut variabelen "values" fra datasettet
  is.na() %>%
  ___() # Tell opp antall verdier som er missing på variabelen "values"


## 3. Er variablene nyttige?

## time-variabelen

table(unexp$time) # Print verdiene til variabelen "time" i datasettet "unexp".
# Hva sier denne variabelen oss? Er den nyttig? 
# Er det viktig for oss at data gjelder 1. januar hvert år, eller bare at de gjelder for det året?
# Trenger vi alle årene her (se forskningsspørsmålet øverst i scriptet)?

unexp_reduced <- unexp %>% # Omkod variabelen "time" med funksjonen "ifelse" til å kun inneholde årstall, ikke datoer
  mutate(time = ifelse(time == "___", "2015",
                       ifelse(time == "___", "2016", 
                              ifelse(time == "2017-01-01", "___",
                                     ifelse(time == "2018-01-01", "___",
                                            ifelse(time == "___", "2019",
                                                   ifelse(time == "___", "___", 
                                                          time))))))) %>%
  filter(time %in% c("___", "___", "___", "___", "___", "___")) # Hent ut rader med årstallene 2015, 2016, 2017, 2018, 2019 og 2020

table(___$time) # Sjekk at omkodingen ble riktig. Husk at vi nå har laget et nytt datasett med et nytt navn.


___(___$___) # Sjekk om datasettet "pop" har samme måte å kode time-variabelen sin på som datasettet unexp


pop_reduced <- pop %>%
  ___(time = substr(time, 1, 4)) # Lag en ny variabel "time" i datasettet pop med bare årstall, ikke datoer
# Mer effektiv omkoding: Bruker funksjonen substr() for å hente ut de første fire tegnene i en character vector. 

pop_reduced <- pop_reduced %>%
  filter(___ %in% c("2015", "2016", "2017", "2018", "2019", "2020")) # Hent ut bare årstallene vi trenger


# minwage-datasettet er annerledes. Dette datasettet er biannual, som betyr at det går annethvert år.
table(minwage$time) 

# Siden de andre datasettene vi har er på år-basis, må dette datasettet også være på år-basis for at vi skal kunne sammenlikne.
# Vi henter ut rader med minstelønn per 1. januar


# Sett disse kodesnuttene i riktig rekkefølge:
# Tips: "time" variabelen er lagret som klasse "date" og må omkodes til character før man kan bruke filter

  
  mutate(time = substr(time, 1, 4)) %>%
  
  filter(time %in% c("2015", "2016", "2017", "2018", "2019", "2020")) 
  
  minwage_reduced <- minwage %>% 
  
  filter(!time %in% c("2020-07-01", "2019-07-01", "2018-07-01", "2017-07-01", "2016-07-01", "2015-07-01")) %>%
  
  mutate(time = as.character(time)) %>%
    




## values-variabelen

class(unexp_reduced$___) # Finn klassen til variabelen "values" i unexp-datasettet. Kommenter hva klassen er.
# ___

___(unexp_reduced$values, na.rm = TRUE) # Finn maksverdi til variabelen "values" i datasettet unexp
___(unexp_reduced$values, na.rm = TRUE) # Finn minsteverdi til variabelen "values" i datasettet unexp

# Hva forteller egentlig denne variabelen oss?

head(unexp_reduced)
table(unexp_reduced$unit)
# Print koden over og se på variabelen "unit". Den har verdi "PC". Det betyr at variabelen "values" er oppgitt i prosent.
# Altså viser "values" oss hvor mange prosent i hvert EU-land ("geo") som opplever finansiell usikkerhet 
# (fordelt på husholdningstype ("hhtyp") og inntektsgruppe ("incgrp")).

# Er "values" et nyttig navn på variabelen? 

unexp_reduced <- unexp_reduced %>%
  ___(unexp_percent = values) # Endre navnet på variabelen fra "values" til "unexp_percent"


# Sjekk om datasettene pop og minwage også inneholder en variabel kalt "values"

___(pop_reduced) # Hva slags verdi tror du "values"-variabelen i er oppgitt i? Kommenter under.
table(___$unit)
# 

___(minwage_reduced) # Hva slags verdi tror du "values"-variabelen i er oppgitt i? Kommenter under.
table(___$currency)
# 

# Endre navnet på disse variablene også.

pop_reduced <- pop_reduced %>%
  rename(n_people = ___) 

minwage_reduced <- ___ %>%
  rename(minwage_eur = values) 



## 4. Er observasjonene nyttige?

# Hva er enhetene i forskningsspørsmålet vårt? Hvilken variabel skiller mellom enhetene i datasettet unexp? Kommenter under.

glimpse(unexp_reduced)
# 

# Enhetene er gruppert etter to variabler: hhtyp og incgrp

# Hvilke verdier finnes i hhtyp-variabelen (husholdningstype)?

___(unexp_reduced$hhtyp)

# Hvilke verdier finnes i incgrp-variabelen (inntektsgruppe)?

___(unexp_reduced$incgrp)


# Hent ut fra incgrp-variabelen kun den raden som har verdi "TOTAL"

unexp_reduced <- unexp_reduced %>%
  filter(incgrp == ___)

# Hent ut fra hhtyp-variabelen kun de radene som har verdi "A1F", "A1M", "A2" og "TOTAL"

unexp_reduced <- unexp_reduced %>%
  ___(hhtyp %in% c("A1F", "A1M", "A2", "TOTAL"))


# Nå har variablene "incgrp" og "unit" samme verdi hele veien, så de forteller oss ikke så mye mer nyttig.
# Fjern variablene "incgrp" og "unit" fra datasettet ved å bare velge de andre variablene med select().

unexp_reduced <- unexp_reduced %>%
  select(hhtyp, geo, ___, ___)


# På samme måte rydder vi i pop-datasettet og minwage-datasettet. Fyll inn kodene under slik at det blir riktig.

pop_reduced <- pop_reduced %>%
  filter(age == "TOTAL") %>%
  filter(sex == "T") %>%
  ___(geo, time, n_people)


minwage_reduced <- minwage_reduced %>%
  ___(currency ___ "EUR") %>%
  select(-currency) # Minus med select() fjerner variabler


# 5. Deskriptiv statistikk

___(unexp_reduced$unexp_percent, na.rm = TRUE) # Finn gjennomsnittlig prosentandel som sier de ikke kunne klart en plutselig finansiell utgift

___(pop_reduced$n_people) # Logtransformer variabelen n_people (befolkningsantall)


# Finn gjennomsnittlig prosentandel som sier de ikke kunne klart en plutselig finansiell utgift gruppert på år og land
unexp_reduced %>%
  group_by(___, ___) %>% 
  summarise(unexp_pc_mean = mean(___))



#####################################
########### VISUALIZE ###############
#####################################

# Lag et boksplot med unexp_percent (prosentandel med finansiell usikkerhet) på x-aksen, 
# geo (land) på y-aksen og farget etter hhtyp (husholdningstype)
# Hva forteller dette plottet oss?

unexp_reduced %>%
  ___(aes(x = unexp_percent, ___ = geo, color = hhtyp)) + 
  geom_boxplot() 

# ___


# Filtrer ut bare landene FR (Frankrike), DE (Tyskland), ES (Spania), BE (Belgia), NO (Norge), SE (Sverige) og DK (Danmark), og plott på nytt.
# Omkod verdiene på hhtyp til noe som er mer lettfattelig å forstå

# Ordne kodelinjene under slik at de kommer i riktig rekkefølge:

unexp_reduced %>%
  
  geom_boxplot() 

  ggplot(aes(x = unexp_percent, y = geo, color = hhtyp)) + 
    
  filter(geo %in% c("FR", "DE", "ES", "BE", "NO", "SE", "DK")) %>%

  mutate(hhtyp = ifelse(hhtyp == "A1F", "Enslig kvinne",
                      ifelse(hhtyp == "A1M", "Enslig mann",
                             ifelse(hhtyp == "A2", "To voksne", hhtyp)))) %>%


# I hvilket land er det totalt størst prosentandel som rapporterer finansiell usikkerhet?
# ___

# I hvilket land er det totalt minst prosentandel som rapporterer finansiell usikkerhet?
# ___

# Hvilken husholdningstype har størst prosentandel som rapporterer finansiell usikkerhet?
# ___

# Hva er omtrentlig medianen til prosentandelen som rapporterer finansiell usikkerhet blant husholdninger med to voksne i Tyskland?
# ___


# Plot minstelønnen til de europeiske landene over tid med en linjediagram. Farge dottene etter land.
# Hva forteller dette plottet oss?

minwage_reduced %>%
  ggplot(aes(x = time, y = minwage_eur, 
             group = geo, ___ = geo)) + 
  ___() +
  labs(x = "", y = "Minstelønn")

# ____


## Slå sammen datasettene 

# Se på datasettene under. Hva er de felles nøklene for datasettene våre?

head(unexp_reduced)
head(pop_reduced)
head(minwage_reduced)
# ___ og ___


df <- unexp_reduced %>%
  filter(hhtyp == ___) %>% # Ta ut bare verdien "TOTAL" fra hhtyp-variabel, som gjør at vi kun får finansiell usikkerhet for alle husholdningstyper
  left_join(pop_reduced, by = c(___, ___)) %>%  # Slå sammen med pop_reduced datasettet
  left_join(minwage_reduced, by = c(___, ___)) # Slå sammen med minwage_reduced datasettet


# Bruk det sammenslåtte datasettet til å plotte minstelønn i de europeiske landene opp mot finansiell usikkerhet
# Farge punktene etter land
# Legg på en lineær regresjonslinje
# Hva forteller dette plottet oss?

df %>%
  ggplot(___(x = minwage_eur, y = unexp_percent)) + 
  geom_point(aes(color = ___)) + 
  geom_smooth(___ = "lm")
# Det er en negativ bivariat sammenheng mellom finansiell usikkerhet og minstelønn.
# Desto lavere minstelønn, desto høyere finansiell usikkerhet for europeiske land mellom 2015 og 2020



#####################################
############# MODEL #################
#####################################

# Lag en lineær modell der du setter unexp_percent som avhengig variabel og som uavhengige variabler
# setter du minwage_eur, n_people og time. Oppgi time som en kategorisk variabel (faste effekter / dummy-variabel). 

modell1 <- lm(___ ~ ___ + ___ + factor(___), 
              na.action = "na.exclude",
              data = ___)

___(modell1) # Oppgi et sammendrag av modellen

___(stargazer) # Last inn stargazer-pakken

stargazer(___, # Lag en fin tabell av modell1
          ___ = "text") # Print tabellen i console


## Plot effekten fra modell1

# Lag en vektor der du lar minwage_eur-variabelen variere mellom minsteverdi og størsteverdi
minwage_eur_vektor = c(seq(___(df$minwage_eur, na.rm = ___),
                           ___(df$minwage_eur, na.rm = ___), 
                           by = ___)) # La variabelen variere med et intervall på 10

n_people_vektor <- mean(___, na.rm = TRUE) # Lag en vektor der du setter n_people (befolkningsantall) til gjennomsnittet 

time_vektor <- ___ # Lag en vektor der du setter året til 2017


# Sett sammen alle vektorene til et datasett. Kall variablene minwage_eur, n_people og time. Hva slags fiktiv verden er dette?
snitt_data <- ___(minwage_eur = ___,
                  n_people = ___,
                  time = ___) 
# ___


# Bruk datasettet snitt_data til å predikere prosentandel med finansiell usikkerhet med modellen du lagde.
pred_verdier <- ___(modell1, 
                        newdata = ___, 
                        se = TRUE, interval = "confidence")


# Legg på vektorene som ble generert med predict() i datasettet snitt_data, slik at de blir nye kolonner.
snitt_data <- cbind(___, ___)


# Hva er predikert prosentandel med finansiell usikkerhet for et land i 2017 med 62.678.031 innbyggere og minstelønn på 412.69 euro?

snitt_data %>%
  filter(minwage_eur == ___) %>%
  select(fit.fit)
# ___


# Hva er predikert prosentandel med finansiell usikkerhet for et land i 2017 med 62.678.031 innbyggere og minstelønn på gjennomsnittet av minwage-vektoren?

snitt_data %>%
  ___(minwage_eur == ___(minwage_eur_vektor)) %>%
  select(fit.fit)
# ___


# Hva er nedre konfidensintervall til den predikert prosentandelen med finansiell usikkerhet for et land i 2017 med 62.678.031 innbyggere og 
# minstelønn på maksimal verdi av minwage-vektoren?

snitt_data %>%
  filter(minwage_eur == max(___)) %>%
  select(___)
# ___


# Plot effekten av minstelønn på finansiell usikkerhet.

# Ordne linjene under slik at de blir i riktig rekkefølge:

snitt_data %>%
  
  geom_line() +
  
  labs(x = "Minstelønn", y = "Forventet finansiell usikkerhet")

  ggplot(aes(x = minwage_eur, y = fit.fit)) + 
 
  geom_ribbon(aes(ymin = fit.lwr, 
                  ymax = fit.upr), 
              alpha = .2) + 

# Basert på det vi har gjort til nå, tenker du at dette er en god modell for å svare på forskningsspørsmålet (se øverst i scriptet)? 
# Er det noe du ville gjort annerledes?
# ___
