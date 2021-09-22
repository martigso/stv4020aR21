#######################################
### FORDYPNINGSSEMINAR 2: PANELDATA ###
#######################################

# Laster inn pakker ----
# Laster inn nødvendige pakker fra biblioteket
# Husk å kjør install.packages("pakkenavn") først om det er første gang du bruker pakken
library(haven) # For å kunne lese inn .dta-filer
library(tidyverse) # For å kunne bruke ggplot, dplyr og liknende
library(stargazer) # For å kunne lage pene tabeller

# Installerer pakken plm for bruk med paneldata
install.packages("plm") # Fjern # foran om ikke du har installert pakken før
library(plm)


# Laster inn data ----
data <- read_dta("neumayer_spess_2005.dta")

# Lagrer informasjon om enheter og tid i et plm-objekt ----
data.plm <- pdata.frame(data, index = c("country", "year"))

class(data.plm)

head(attr(data.plm, "index"))


# Er panelet balansert? ----
# Har vi data for alle land for alle tidsperiodene som er inkludert? 
is.pbalanced(data.plm)

# Lager et datasett uten missingverdier for å dobbeltsjekke: 
data.complete <- data.plm %>% 
  na.omit() %>% 
  mutate(year = droplevels(year), 
         country = droplevels(country)) # Her fjerner jeg faktornivåene (levels) til de årene og landene som ikke lengre er med i datasettet

# Sjekker om det nye datasettet som blir lagt til grunn for analysen er balansert
is.pbalanced(data.complete)

# Vi kan balansere et datasett på flere måter:
data.balanced.time <- make.pbalanced(data.complete, balance.type = "shared.times") # Beholder bare de enhetene vi har data for alle tidsperiode for
# Dette datasettet inneholder 0 observasjoner fordi vi ikke har data for alle tidsperioder for noen enheter

data.balanced.ind <- make.pbalanced(data.complete, balance.type = "shared.individuals") %>%  # Beholder bare de tidsperiodene vi har data for alle enhetene for
  mutate(year = droplevels(year),                               
         country = droplevels(country)) # Her fjerner jeg faktornivåene (levels) til de årene og landene som ikke lengre er med i datasettet

# Finner antall tidsperioder i opprinnelig datasett uten missing
length(unique(data.complete$country))
length(unique(data.balanced.ind$country))
# I det nye datasettet har vi data for 45 unike land sammenlignet med 120 land opprinelig

length(unique(data.complete$year))
length(unique(data.balanced.ind$year))
# Vi har fortsatt data for 32 år
# Dette gir mening fordi vi valgte å beholde de enhetene vi hadde observasjoner for alle år for. 


# Pooled OLS med PCSE ----

# I plm får vi en vanlig OLS-modell om vi velger model = "pooling". 
mod1ols <- plm(data = data.complete, 
              fdi_inflow ~ bits + ln_gdp_pr_cap + ln_population +
                economic_growth + inflation + resource_rent + 
                bilateral_trade_agreements + wto_member + polcon3,
              na.action = "na.exclude", model = "pooling")

# Beregner PCSE:
bkse <- round(sqrt(diag(vcovBK(mod1ols, cluster = "group"))), digits = 4)
# PCSE finnes i mange varianter. 
# I funksjonen vcovBK kan man bruke cluster = "group" for å justere for autokorrelasjon 
# eller cluster = "time" for å justere for panelspesifikk heteroskedastisitet og korrelasjon.
# Det finnes også andre alternativer som vcovDC som justerer for både group og time. 

# Printer resultatene i en tabell
stargazer(mod1ols, mod1ols, type = "text",
          column.labels = c("Med PCSE", "Med vanlige SE"),
          se = list(bkse))
# Med argumentet se = list(bkse) forteller jeg stargazer at jeg i den første kolonnen
# vil erstatte de opprinnelige standardfeilene med de panelkorrigerte standardfeilene
# jeg regnet ut over. 


# Restleddsutfordringer og paneldata ----

## Autokorrelasjon ----
# Ofte vil ikke forutsetningene om uavhengige restledd være oppfylt med tidsseriedata

# Kjører durbin watson test på modellobjektet
pdwtest(mod1ols)


# Kjører modellen med lagget avhengig variabel som uavhengig variabel 
mod1ols_lag <- plm(data = data.complete, 
              fdi_inflow ~ lag(fdi_inflow, 1) +  # Bytt ut "1" med ønsket antall lags dersom du vil ha mer enn 1
                bits + ln_gdp_pr_cap + ln_population +
                economic_growth + inflation + resource_rent + 
                bilateral_trade_agreements + wto_member + polcon3,
              na.action = "na.exclude", model = "pooling")


# Vi kan også differensiere
# Her lager jeg først en variabel som er lik Yt - Yt-1
data.complete$y.lag <- lag(data.complete$fdi_inflow, 1)
data.complete$y_diff <- data.complete$fdi_inflow - data.complete$y.lag

# Kjører modellen med differensiert avhengig variabel for å illustrere hvordan det kan gjøres
mod1ols_diff <- plm(data = data.complete, 
               y_diff ~ bits + ln_gdp_pr_cap + ln_population +
                economic_growth + inflation + resource_rent + 
                bilateral_trade_agreements + wto_member + polcon3,
              na.action = "na.exclude", model = "pooling")

summary(mod1ols_diff)

stargazer(mod1ols, mod1ols_lag, mod1ols_diff, 
          type = "text",
          column.labels = c("Pooles OLS", "Lagget AVAR som UVAR", "Differensiert AVAR som AVAR"))

# Vi bruke durbin watson testen til å sjekke om dette har hjulpet mot autokorrelasjon:
pdwtest(mod1ols_lag)
pdwtest(mod1ols_diff)


# Panel-spesifikk heteroskedastisitet ----

# Legger predikerte verdier og residualer inn i datasettet
data.complete <- data.complete %>% 
  mutate(resid = resid(mod1ols),
         fdi_inflow_pred = predict(mod1ols))

# Eye ball test av heteroskedastisitet
ggplot(data.complete %>% 
         filter(country %in% c("Norway", "Ethiopia", "Chile", "Estonia",
                               "Chad", "Switzerland", "Spain")) %>%      # Velger noen land som jeg plotter for å ikke få et helt uoversiktelig plot
         data.frame(),                                                   # Gjør om til en data.frame objekt for å plotte
       aes(x = fdi_inflow_pred, y = resid)) +         
  geom_point(aes(col = country)) +
  geom_smooth(method = "lm")

# Det finnes varianter av PCSE som kan justere restleddene for autokorrelasjon,
# samtidig korrelasjon og panel-spesifikk heteroskedastisitet.


# Faste effekter ----

# Med tversnittsfaste effekter (i dette tilfellet land)
plm.fe.ind <- plm(data = data.complete, 
              fdi_inflow ~ bits + ln_gdp_pr_cap + ln_population +
                economic_growth + inflation + resource_rent + 
                bilateral_trade_agreements + wto_member + polcon3,
              na.action = "na.exclude", model = "within", effect = "individual")


# Med tidsfaste effekter (i dette tilfellet år)
plm.fe.time <- plm(data = data.complete, 
              fdi_inflow ~ bits + ln_gdp_pr_cap + ln_population +
                economic_growth + inflation + resource_rent + 
                bilateral_trade_agreements + wto_member + polcon3,
              na.action = "na.exclude",model = "within", effect = "time")

# Med tversnitts- og tidsfaste effekter (i dette tilfellet år og land)
plm.fe.two <- plm(data = data.complete, 
              fdi_inflow ~ bits + ln_gdp_pr_cap + ln_population +
                economic_growth + inflation + resource_rent + 
                bilateral_trade_agreements + wto_member + polcon3,
              na.action = "na.exclude", model = "within", effect = "twoways")

# Viser resultatene i en tabell uten koeffisientene til de faste effektene

stargazer(plm.fe.ind, plm.fe.time, plm.fe.two, type = "text",
          column.labels = c("Tversnitts FE", "Tids FE", "Tversnitts og tids FE"),
          omit = c("country", "year"))

# Eneste forskjellen fra lm() er at du ikke får med konstantledd. 


# Henter ut de faste effektene: 
fixef(plm.fe.ind)[1:5] # Henter ut de fem første tversnittsfaste effektene
fixef(plm.fe.time)[1:5] # Henter ut de fem første tidsfaste effektene

# I modeller med både tversnitts- og tidsfaste effekter må du spesifiser effect = "time" 
# om du vil ha de tidsfaste effektene
fixef(plm.fe.two)[1:5] # Henter ut de fem første tversnittssfaste effektene
fixef(plm.fe.two, effect = "time")[1:5] # Henter ut de fem første tidsfaste effektene

# For å vurdere størrelse og signifikans på faste effekter
# Land/enhet
summary(fixef(plm.fe.ind))
# Tid:
summary(fixef(plm.fe.two, effect = "time"))

# OBS! Lite variasjon i UVA gir risiko for multikolinearitet ved bruk av faste effekter
# Da kan det være lurt å plotte enkeltvariabler.
# Her viser jeg hvordan du kan lage et plot per land for å se utvikling over tid:
ggplot(data.complete %>% 
         filter(country %in% c("Bagladesh", "Lesotho", "India", "Chile",
                               "China", "Lithuania", "Mozambique", "Togo", 
                               "Zambia", "Fiji", "Belize", "Peru", "Guyana"))) +
  geom_point(aes(x = as.numeric(as.character(year)), y = ln_population)) +
  facet_wrap(~country) +
  theme_bw() +
  xlab("Year") + ylab("ln population")
  
table(data.complete$year)


ggplot(data.complete%>% 
         filter(country %in% c("Bagladesh", "Lesotho", "India", "Chile",
                               "China", "Lithuania", "Mozambique", "Togo", 
                               "Zambia", "Fiji", "Belize", "Peru", "Guyana"))) + 
  geom_point(aes(x = as.numeric(as.character(year)), y = bits)) +
  facet_wrap(~country) +
  theme_bw() +
  xlab("Year") + ylab("Antall BITs")

# Om vi vil sjekke vif må vi kjøre en pooled ols med enhetsfaste effekter:
ols.fe.pooled <- mod1ols <- plm(data = data.complete, 
              fdi_inflow ~ bits + ln_gdp_pr_cap + ln_population +
                economic_growth + inflation + resource_rent + 
                bilateral_trade_agreements + wto_member + polcon3 + 
                country + year,
              na.action = "na.exclude", model = "pooling")

car::vif(ols.fe.pooled)

# Modeller med random effects ----

# Antar at enhetsspesifikke konstantledd er tilfeldig trukket fra en felles
# fordeling. Blir biased dersom noen av disse er korrelerte med minst en UVAR.  

plm.re.ind <- plm(data = data.complete, 
              fdi_inflow ~ bits + ln_gdp_pr_cap + ln_population +
                economic_growth + inflation + resource_rent + 
                bilateral_trade_agreements + wto_member + polcon3,
              na.action = "na.exclude", model = "random", effect = "individual")

plm.re.time <- plm(data = data.complete, 
              fdi_inflow ~ bits + ln_gdp_pr_cap + ln_population +
                economic_growth + inflation + resource_rent + 
                bilateral_trade_agreements + wto_member + polcon3,
              na.action = "na.exclude", model = "random", effect = "time")

plm.re.two <- plm(data = data.complete, 
              fdi_inflow ~ bits + ln_gdp_pr_cap + ln_population +
                economic_growth + inflation + resource_rent + 
                bilateral_trade_agreements + wto_member + polcon3,
              na.action = "na.exclude", model = "random", effect = "twoways")

stargazer::stargazer(plm.re.ind, plm.re.time, plm.re.two, type = "text",
                     column.labels = c("Tversnitts RE", "Tids RE", "Tversnitts og tids RE"))


# Tolke effekter -----

# Stort sett som OLS

## Plotte koeffisienter ----
# Laster inn pakken 
# husk å installere ved hjelp av install.packages("sjPlot") om ikke du har brukt den før
library(sjPlot)

# Plotter koeffisientene og 95 % KI fra modellen med enhetsfaste effekter 
plot_model(plm.fe.ind, sort.est = TRUE)


## Plotte faktisk verdi opp mot predikert verdi
# I dette eksempelet tar jeg utgangspunkt i både tids- og enhetseffekter
# Legger til predikerte verdier på avhengig variabel for random effects i datasett
data.complete$pred_fdi_inflow_re <- predict(plm.re.two)
# Legger til predikerte verdier for pooled OLS i datasett 
data.complete$pred_fdi_inflow_ols <- predict(mod1ols)
# Legger til predikerte verdier for fixed effects i datasett
data.complete$pred_fdi_inflow_fe <- predict(plm.fe.two)


# Jeg gjør om en dataframe fra wide til long for å kunne få ulik farge på de predikerte verdiene i plottet
plot_data <- data.complete %>%
  data.frame() %>% # Gjør om fra plm-objekt til data.frame
  select(country, year, fdi_inflow, pred_fdi_inflow_fe, pred_fdi_inflow_ols, pred_fdi_inflow_re) %>%  # Velger ut de aktuelle variablene
  pivot_longer(cols = contains("fdi_inflow"), names_to = "model", "values_to" = "FDI_inflow")  # omformer datasettet 
# Argumentet cols = sier hvilke kolonner(variabler) som skal slås sammen til en kolonne
# contains("fdi_inflow") betyr at jeg vil ha med alle variablene som inneholder fdi_inflow

head(plot_data, 10)

ggplot(plot_data %>% 
         filter(country %in% c("Bagladesh", "Lesotho", "India", "Chile",
                               "China", "Lithuania", "Mozambique", "Togo", 
                               "Zambia", "Fiji", "Belize", "Peru", "Guyana"))) +    # Tar med et utvalg av land
  geom_line(aes(x = as.numeric(as.character(year)), y = FDI_inflow, col = model)) + # Plotter linjen
  facet_wrap(~country) +                                                            # Lager et plot per land  
  theme_classic() +
  scale_color_discrete(labels = c("True value", "Predicted FE", "Predicted pooled OLS", "Prediced RE")) + # Setter inn labels på fargeoversikt
  xlab("Year") + ylab("FDI inflow") +
  theme(legend.position = "right", legend.title = element_blank())


# Hausman-testen ----
# Denne gir en indikasjon og tester om koeffisientene er like før RE og FE

# Gjennom PLM kan vi kjøre en hausman test
# Modellene med både tverrsnitts- og tidsfaste effekter
phtest(plm.fe.two, plm.re.two)

# Modellene med tverssnitssfaste effekter
phtest(plm.fe.ind, plm.re.ind)

# Modellene med tidsfaste effekter
phtest(plm.fe.time, plm.re.time)

