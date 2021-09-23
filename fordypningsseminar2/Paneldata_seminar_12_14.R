
###########################
### OLS forutsetninger: ###
###########################

# 1. Ingen utelatt variabelskjevhet
# 2. Lineær sammenheng mellom variablene
# 3. Ingen autokorrelasjon/Uavhengige observasjoner
# 4. Normalfordelte residualer
# 5. Homoskedastiske residualer
# 6. Ingen perfekt multikollinearitet
# 7. Ingen outliers eller innflytelsesrike observasjoner
# 8. Manglende opplysninger(missing values)


####################################


library(haven) # For å kunne lese inn .dta-filer
library(tidyverse) # For å kunne bruke ggplot, dplyr og liknende
library(stargazer) # For å kunne lage pene tabeller

# Laster inn data
data <- read_dta("neumayer_spess_2005.dta")

## I paneldata har vi observasjoner for flere enheter over tid.
## Typisk for land og for år.

glimpse(data)


#### Hvorfor bruke paneldata? ####

## 1. Flere observasjoner, større N, mer informasjon.
## 2. Mulighet for å kontrollere for årsakssammenheng (årsaksretning): OVB!

library(plotly)

time <- data %>%
  ggplot(aes(x = fdi_inflow, y = bits)) + # color = country
  geom_point(aes(frame = year)) + 
  theme(legend.position = "none")

ggplotly(time)


## Egenskaper med paneldata:

library(plm) # Vi bruker plm-pakken

data_plm <- pdata.frame(data, index = c("country", "year")) # Må opprette et plm-objekt

class(data_plm) # pdata.frame

glimpse(data_plm) # pseries-typer

head(attr(data_plm, "index"))

#### Utfordringer med paneldata ####

# Balanse: Ta ut missingverdier?
# Heteroskedastisitet: Panelkorrigerte standardfeil
# Samtidig korrelasjon: Panelkorrigerte standardfeil
# Autokorrelasjon: Lagget avhengig variabel, trendvariabel eller klyngekorrigert standardfeil


# Er dataene balansert?

is.pbalanced(data_plm)
## Like mange land-variabler som år-variabler?
## Tar ikke hensyn til missing

data.complete <- data_plm %>% 
  na.omit() %>% 
  mutate(year = droplevels(year), 
         country = droplevels(country))

# Er dataene balansert etter å ha tatt ut missing?
is.pbalanced(data.complete)

# Skal vi gjøre data balansert?
## Fordelen er at dette gir bedre prediksjonskraft på modellen (vi får ikke skjevhet).
## Ulempen er at vi ofte mangler god nok informasjon til å skape balanse.

data.balanced.time <- make.pbalanced(data.complete, 
                                     balance.type = "shared.times") 
# Beholder bare de enhetene (landene) der vi har data for alle tidsperiodene (årene)
# Dette datasettet inneholder 0 observasjoner fordi vi ikke har data for alle tidsperioder for noen enheter

glimpse(data.balanced.time)

# Beholder bare de tidsperiodene (årene) der vi har data for alle enhetene (landene)
data.balanced.ind <- make.pbalanced(data.complete, 
                                    balance.type = "shared.individuals") %>%  
  mutate(year = droplevels(year),                               
         country = droplevels(country)) 
# Fjerner faktornivåene (levels) til de årene og landene som ikke 
# lengre er med i datasettet for å unngå følgefeil

glimpse(data.balanced.ind)

### Mister ganske mange observasjoner:
# Finner antall tidsperioder i opprinnelig datasett uten missing
length(unique(data.complete$country))
length(unique(data.balanced.ind$country))
# I det nye datasettet har vi data for 45 unike land sammenlignet med 120 land opprinelig

length(unique(data.complete$year))
length(unique(data.balanced.ind$year))
# Vi har fortsatt data for 32 år
# Dette gir mening fordi vi valgte å beholde de enhetene vi hadde observasjoner for alle år for. 



#### Hva slags modell kan vi kjøre med paneldata? ####

# Pooled
# Fixed effects
# Random effects


#### Pooled oLS ####

# I plm får vi en vanlig OLS-modell om vi velger model = "pooling". 
mod1ols <- plm(data = data.complete, 
               fdi_inflow ~ bits + ln_gdp_pr_cap + ln_population +
                 economic_growth + inflation + resource_rent + 
                 bilateral_trade_agreements + wto_member + polcon3,
               na.action = "na.exclude", model = "pooling")

## Utfodring 1: Heteroskedastisitet: Ulik variasjon av residualer over forskjellig verdi av y

# Sjekker heteroskedastisitet:

## Henter ut predikerte verdier
ols.predict <- predict(mod1ols)

# Legger predikerte verdier og residualer inn i datasettet
data.complete <- data.complete %>% 
  mutate(resid = resid(mod1ols),
         resid_lag = lag(resid),
         fdi_inflow_pred = ols.predict)

# Eye ball test av heteroskedastisitet
ggplot(data.complete %>% 
         filter(country %in% c("Norway", "Ethiopia", "Chile", "Estonia",
                               "Chad", "Switzerland", "Spain")) %>% # Velger noen land som jeg plotter for å ikke få et helt uoversiktelig plot
         data.frame(),# Gjør om til en data.frame objekt for å plotte
       aes(x = fdi_inflow_pred, y = resid)) +         
  geom_point(aes(col = country)) +
  geom_smooth(method = "lm")


# Utfordring 2: Samtidig korrelasjon

### Løsning: Vi må ha panelkorrigerte standardfeil: PCSE
# vcovBK gir panelkorrigerte standardfeil

# Beregner PCSE:
bkse <- round(sqrt(diag(vcovBK(mod1ols, cluster = "group"))), 
              digits = 4)

# Printer resultatene i en tabell
stargazer(mod1ols, mod1ols, type = "text",
          column.labels = c("Med PCSE", "Med vanlige SE"),
          se = list(bkse))

## Retter opp i både heteroskedastisitet og samtidig korrelasjon


# Utfordring 3: Autokorrelasjon

pdwtest(mod1ols) # Sjekker autokorrelasjonen (nærhet av 0 = autokorrelasjon (mot nærhet mot 2))

# Løsninger: Lagget avhengig variabel eller differensiering

# Kjører modellen med lagget avhengig variabel som uavhengig variabel 
mod1ols_lag <- plm(data = data.complete, 
                   fdi_inflow ~ lag(fdi_inflow, 1) +  # Bytt ut "1" med ønsket antall lags dersom du vil ha mer enn 1
                     bits + ln_gdp_pr_cap + ln_population +
                     economic_growth + inflation + resource_rent + 
                     bilateral_trade_agreements + wto_member + polcon3,
                   na.action = "na.exclude", model = "pooling")

plm::pdwtest(mod1ols_lag) # p > 0.05 og DW = 2.29


# Vi kan også differensiere
# Her lager jeg først en variabel som er lik Yt - Yt-1
data.complete$y.lag <- lag(data.complete$fdi_inflow, 1)
data.complete$y_diff <- data.complete$fdi_inflow - data.complete$y.lag

# Kjører modellen med differensiert avhengig variabel for å illustrere hvordan det kan gjøres
mod1ols_diff <- plm(data = data.complete, 
                    fdi_inflow ~ y_diff + # Bytt ut "1" med ønsker antall lags dersom du vil ha mer enn 1
                      bits + ln_gdp_pr_cap + ln_population +
                      economic_growth + inflation + resource_rent + 
                      bilateral_trade_agreements + wto_member + polcon3,
                    na.action = "na.exclude", model = "pooling")

summary(mod1ols_diff)

plm::pdwtest(mod1ols_diff) # p < 0.05 og DW = 0.37
# autokorrelasjon

## Det finnes også robuste standardfeil mot autokorrelasjon i residual

stargazer(mod1ols, mod1ols_lag, mod1ols_diff, 
          type = "text",
          column.labels = c("Pooles OLS", "Lagget AVAR som UVAR", "Differensiert AVAR som UVAR"))


#### Fixed effects ####
# Kontrollerer for det som er fast over tid (f. eks. kultur)
## Kontrollerer for ting som er vanskelig å fange opp
## Uheldig hvis det som er fast over tid varierer mellom enheter
## Bias ved korte tidsserier ( < 20)

# Med tversnittsfaste effekter (i dette tilfellet land)
# Kontrollerer ut variasjon mellom land, undersøker bare variasjon innad i landet.
plm.fe.ind <- plm(data = data.complete, 
                  fdi_inflow ~ bits + ln_gdp_pr_cap + ln_population +
                    economic_growth + inflation + resource_rent + 
                    bilateral_trade_agreements + wto_member + polcon3,
                  na.action = "na.exclude", model = "within", effect = "individual")


# Med tidsfaste effekter (i dette tilfellet år)
# Kontrollerer ut variasjon mellom år, undersøker bare variasjon per år.
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

## Se på de faste effektene:
fixef(plm.fe.time)[1:5] # Henter ut de fem første tidsfaste effektene

fixef(plm.fe.ind)[1:5] # Henter ut de fem første tversnittsfaste effektene

fixef(plm.fe.two)[1:5] # Henter ut de fem første tids- og tversnittssfaste effektene

# Signfikians på de faste effektene:
summary(fixef(plm.fe.ind))
summary(fixef(plm.fe.two, effect = "time"))

# OBS: Krever mer variasjon i uavhengige variabler, ellers får vi multikolinearitet
# Hvordan undersøke om det blir multikolinearitet?

# Sjekker variasjon i befolkning:
ggplot(data.complete%>% 
         filter(country %in% c("Bagladesh", "Lesotho", "India", "Chile",
                               "China", "Lithuania", "Mozambique", "Togo", 
                               "Zambia", "Fiji", "Belize", "Peru", "Guyana"))) + # Velger ut utvalg av land
  geom_point(aes(x = as.numeric(as.character(year)), y = ln_population)) + # Bruker as.numeric(as.character(year)) fordi year er en faktor
  facet_wrap(~country) + 
  theme_bw() +
  xlab("Year") + ylab("ln population") # Legger til aksetitler

# Sjekker variasjon i bilaterale avtaler:
ggplot(data.complete%>% 
         filter(country %in% c("Bagladesh", "Lesotho", "India", "Chile",
                               "China", "Lithuania", "Mozambique", "Togo", 
                               "Zambia", "Fiji", "Belize", "Peru", "Guyana"))) +
  geom_point(aes(x = as.numeric(as.character(year)), y = bits)) +
  facet_wrap(~country) +
  theme_bw() +
  xlab("Year") + ylab("Antall BITs")

# Kan sjekke multikolineratitet med VIF, men må da ha pooled OLS

ols.fe.pooled <- mod1ols <- plm(data = data.complete, 
                                fdi_inflow ~ bits + ln_gdp_pr_cap + ln_population +
                                  economic_growth + inflation + resource_rent + 
                                  bilateral_trade_agreements + wto_member + polcon3 + 
                                  country + year,
                                na.action = "na.exclude", model = "pooling")

car::vif(ols.fe.pooled)

# Løsning ved høy vif: F. eks. ha regionvariabel eller periodevariabel.


#### Random effects ####
# Mer effesiente (bruker med variasjon, drar nytte av variasjon i skjæringspunkt)
# Antar at enhetsspesifikke konstantledd er trukket fra en tilfeldig fordeling
# Faste effekter bør ikke korrellere med uavhengige variabler
# Strengere antakelse: Forventer at enhetsspesifikke konstantledd er trukket fra en tilfeldig fordeling

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


#### Hvilken modell? ####

# Gjennom PLM kan vi kjøre en hausman test
# Får du omtrent de samme resultatene med og uten hausman test?
# Hvis du får omtrent samme resultater kan du bruke random effects.

# Modellene med både tverrsnitts- og tidsfaste effekter
phtest(plm.fe.two, plm.re.two)
## Hypotese: Modellene er ikke like
## p-verdi forteller oss at hypotesen er signifikant
## Vi bør bruke faste effekter

# Modellene med tverssnitssfaste effekter
phtest(plm.fe.ind, plm.re.ind)

# Modellene med tidsfaste effekter
phtest(plm.fe.time, plm.re.time)


## Mål 1: Dra nytte av så mye variasjon som mulig.
## Mål 2: Ikke fange opp spuriøse effekter 
# (variabler som er korrelert med uavhengig variabel og påvirker avhengig variabel)
## Derfor: Fixed effect når det er omitted variable bias. 


#### Hvordan kommunisere? ####

library(sjPlot)

# Plotter koeffisientene og 95 % KI fra modellen med enhetsfaste effekter 
# Lurt å standardisere koeffisientene først.
plot_model(plm.fe.ind, sort.est = TRUE)


#### Plotte ####

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
  pivot_longer(cols = contains("fdi_inflow"), 
               names_to = "model", 
               values_to = "FDI_inflow")  # omformer datasettet 
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
