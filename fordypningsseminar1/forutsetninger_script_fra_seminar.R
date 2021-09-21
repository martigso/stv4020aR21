

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



######################################
#### Gjør klar for å kjøre modell ####
######################################

library(tidyverse)
library(stargazer)

load("aid.RData") # Henter data

aid <- aid %>% 
  mutate(log_gdp_pr_capita = log(gdp_pr_capita), # Gjør nødvendige omkodinger
         period_fac = as.factor(period),
         region = ifelse(fast_growing_east_asia == 1, "East Asia",
                         ifelse(sub_saharan_africa == 1, "Sub-Saharan Africa", 
                                "Other")),
         region = factor(region, levels = c("Other", "Sub-Saharan Africa", "East Asia")))


### Titte på modellen vår: ###

modell <- lm(gdp_growth ~ log_gdp_pr_capita + ethnic_frac*assasinations + 
               institutional_quality + m2_gdp_lagged + region + policy*aid +
               period_fac, 
             data = aid, 
             na.action = "na.exclude")

aid_check <- aid %>%
  mutate(prediksjon = predict(modell),
         residualer = residuals(modell)) %>%
  select(country, period, gdp_growth, prediksjon, residualer)

aid_check %>%
  ggplot(aes(x = prediksjon, y = gdp_growth, color = residualer)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "pink") +
  scale_color_gradient(low = "blue", high = "green") +
  labs(x = "Predikert BNP-vekst", y = "Faktisk BNP-vekst",
       title = "Residualer i modellen") +
  theme_bw()


#############################################
#### 1. Ingen "utelatt variabelskjevhet" ####
#############################################

# Hvilke variabler å ta med bak tilden? Begrunn med teori og tidligere studier. 

modell <- lm(gdp_growth ~ log_gdp_pr_capita + ethnic_frac*assasinations + 
               institutional_quality + m2_gdp_lagged + region + policy*aid +
               period_fac, 
             data = aid, 
             na.action = "na.exclude")

stargazer(modell, type = "text")

## Konsekvens ved å bryte: 
## Koeffisientene blir feil, vi kan tro det er en sammenheng som ikke egentlig er der.


################################################
#### 2. Lineær sammenheng mellom variablene ####
################################################

# Er sammenhengen mellom avhengig og uavhengig variabel lineær?

# Sjekker linearitet med ggplot()

aid %>%
  ggplot(aes(y = gdp_growth, x = policy)) + # aid | I(policy^2)
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw()


# Sjekker linearitet med car()

# install.packages("car")
library(car)

model_usam <- lm(gdp_growth ~ log_gdp_pr_capita + ethnic_frac + assasinations +
                    institutional_quality + m2_gdp_lagged + region + policy + aid +
                    period_fac,
                  data = aid, na.action = "na.exclude")

stargazer(model_usam, 
          type = "text")


library(car)

ceresPlot(model_usam, "aid")
ceresPlot(model_usam, "policy")

## Konsekvens ved å bryte: 
## Modellen blir dårlig tilpasset data, lav R2 og høye residualer = større standardavvik og mindre signifikans.


#############################################################
#### 3. Ingen autokorrelasjon / Uavhengige observasjoner ####
#############################################################

# Sjeldent relevant i tverrsnittsundersøkelser med enkelt tilfeldig utvalg.
# Relevant når man har strukturerte data.
# Spesielt relevant i longitudinelle studier (tidsserier), da vi får autokorrelasjon.

## Durbin-Watson testen: 
# d = 2 indikerer ingen autokorrelasjon
# d < 1 (nær 0) tyder på positiv autokorrelasjon 
# d > 3 (nær 4) tyder på negativ autokorrelasjon

# For tidsserier og paneldata:
library(plm)

modell_B <- plm(gdp_growth ~ log_gdp_pr_capita + ethnic_frac * assasinations + 
                 institutional_quality + m2_gdp_lagged + region + policy * aid +
                 period_fac + factor(period) + factor(country),
                model = "pooling",
                data = aid,
                na.action = "na.exclude")

pdwtest(modell_B)

# For tidsserier:

modell_C <- lm(gdp_growth ~ log_gdp_pr_capita + ethnic_frac * assasinations + 
                  institutional_quality + m2_gdp_lagged + region + policy * aid +
                  period_fac,
                data = aid, 
                na.action = "na.omit") # Her blir det problemer om vi bevarer na med na.exclude.

car::durbinWatsonTest(modell_C)


## Hva slags data er aid-datasettet, tverrsnitt, tidsserie eller paneldata?

## Konsekvens ved å bryte: 
## Residualene blir feil, standardavvikene kan bli for lave og vi kan tro det er 
## signfikans der det egentlig ikke er det.


######################################
#### 4. Normalfordelte residualer ####
######################################

## Metode 1: 
stand_resid <- rstandard(modell)
# Henter standardiserte residualer (residualer delt på standardavviket).

ggplot() +
  geom_histogram(aes(x = stand_resid, y = ..density..)) + 
  stat_function(fun = dnorm, 
                color = "goldenrod")


## Metode 2:

# install.packages("moments")
library(moments)

skewness(stand_resid, na.rm = TRUE)
# Minusverdier indikerer skjevhet mot venstre.
# Plussverdier indikerer skjevhet mot høyre. 
# Moderat skjevhet = verdier over 0.5 eller under -0.5.
# Stor skjevhet = verdier over 1 eller under -1

kurtosis(stand_resid, na.rm = TRUE)
# Sjekker spesielt uteliggere (høye standardiserte residualer, dvs. over 1)
# En normalfordeling har kurtose på 3.
# Bør ikke være over 5.


## Metode 3:

# Plotter studentiserte residualer mot kvantiler fra den 
# kumulative normalfordelingen.
car::qqPlot(modell)


## Konsekvens ved å bryte: 
## Beregningen av standardfeilen kan bli misvisende, spesielt for små utvalg, slik at
## vi tror noe er signifikant når det egentlig ikke er det.


#######################################
#### 5. Homoskedastiske residualer ####
#######################################

# Variansen til residualene skal være konstant for ulike nivå av uavhengig variabel.

car::residualPlot(modell)

## Konsekvens ved å bryte:
## Kan være en indikasjon på at modellen utelater viktige ikke-lineære sammenhenger.
## Vi kan f. eks. prøve å inkludere flere variabler, samspill eller omkode avhengig variabel. 


#############################################
#### 6. Ingen perfekt multikollinearitet ####
#############################################

car::vif(modell)

## Konvekvens ved å bryte:
## Koeffisientene blir feil og vi fanger ikke opp sammenhenger.


################################################################
#### 7. Ingen outliers eller innflytelsesrike observasjoner ####
################################################################

car::influenceIndexPlot(modell,
                        id = list(n=3)) 
# Hvor mange obs du vil ha nummerert


# Se på enkelte observasjoner:
aid[c(87,86), ]

aid %>%
  slice(87, 86)


obj <- dfbetas(modell) # %>% head()
# Hver observasjons innflytelse på koeffisientene til alle 
# variablene i en modell


## Konsekvens ved å bryte:
## Enkelte observasjoner får høy "leverage" og trekker regresjonslinjen mot seg



###################################################
#### 8. Manglende opplysninger(missing values) ####
###################################################

# Kan være NA, men også f. eks. 888, 999, eller liknende. Sjekk kodeboken.

#data %>%
#  mutate(budget_balance = ifelse(budget_balance == 999, NA, 
#                                 budget_balance))

table(aid$country) # Ingen suspekte verdier

# Lager variabel som viser hvilke observasjoner som forsvinner i regresjon med 
# de sentrale variablene

aid$reg_miss <- aid %>%
  select(gdp_growth, aid, policy) %>%
  complete.cases()

table(aid$reg_miss)


### Hvordan danne seg et inntrykk av konsekvensene av missingverdier?

modell_redusert <- lm(gdp_growth ~ aid*policy + as.factor(period) + ethnic_frac*assasinations, 
                      data = aid )
summary(modell_redusert) # output viser at 48 observasjoner fjernes pga. missing


### 1. Korrelasjonsmatriser

aid <- aid %>%
  mutate(period2 = ifelse(period == 2, 1, 0))

aid$period2 <- ifelse(aid$period==2, 1, 0)
aid$period3 <- ifelse(aid$period==3, 1, 0)
aid$period4 <- ifelse(aid$period==4, 1, 0)
aid$period5 <- ifelse(aid$period==5, 1, 0)
aid$period6 <- ifelse(aid$period==6, 1, 0)
aid$period7 <- ifelse(aid$period==7, 1, 0)
aid$period8 <- ifelse(aid$period==8, 1, 0)

aid %>% 
  select(gdp_growth,aid,policy, ethnic_frac,assasinations,period2,period3,period4,period5,period6,period7) %>%
  cor(use = "pairwise.complete.obs")

aid %>% 
  select(gdp_growth,aid,policy, ethnic_frac,assasinations,period2,period3,period4,period5,period6,period7) %>%
  cor(use = "complete.obs")

# Alternativet "pairwise.complete.obs" fjerner bare missing for de enkelte bivariate korrelasjonene


### 2. Analyse av dummy-variabler

miss_mod <- glm(reg_miss ~ aid*policy + as.factor(period), 
                data = aid)

# Land med høyere bistand som prosent av BNP fjernes oftere enn land med lavere bistand.

summary(miss_mod) # ingen store forskjeller


#################################
### Logistisk forutsetninger: ###
#################################

# Forventninger som brytes ved logistisk regresjon:
## Homoskedastiske residualer
## Normalfordelte residualer

# Lager dikotom variabel
aid <- aid %>% 
  mutate(gdp_growth_d = ifelse(gdp_growth <= 0, 0, 1))


### Likelihood-ratio test 
# Variabler å inkludere.
# Vi sammenlikner modeller med hverandre (de må være nøstede)

# Lager et datasett med variablene vi trenger og uten NA
aid.complete <- aid %>% 
  select(gdp_growth_d, aid, policy, period) %>%  # Velger variablene vi skal bruker
  na.omit() # Beholder bare observasjoner uten NA 

# Lager en nullmodel
gm0 <- glm(gdp_growth_d ~ aid, data = aid.complete, 
           family = binomial(link = "logit"))

# Lager en modell med avhengige variabler
gm1 <- glm(gdp_growth_d ~ aid + policy + as.factor(period), data = aid.complete, 
           family = binomial(link = "logit"))

anova(gm0, gm1, test = "LRT")
# Mer avansert modell bør være signifikant bedre


### Hosmer-Lemeshow
# Sammenhengens form

# install.packages("ResourceSelection")
library(ResourceSelection)

hoslem.test(gm0$y, gm0$fitted.values)

hoslem.test(gm1$y, gm1$fitted.values)

# Bør ikke være signifikant



### Pseudo R
# Modellens tilpasning til data
# install.packages("pscl")
library(pscl)

pR2(gm0)

pR2(gm1)

# McFadden's mellom 0.2 og 0.4


