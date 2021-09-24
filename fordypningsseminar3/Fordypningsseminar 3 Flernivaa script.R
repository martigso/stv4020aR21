# Laster inn nødvendige pakker
# Husk å kjøre install.packages("pakkenavn") om det er første gang du
# bruker pakken
# install.packages("lme4")
library(lme4) # For å kjøre flernivåmodeller
library(tidyverse) # Bl.a. for å preppe data
library(stargazer) # For pene tabeller
# install.packages("sjlabelled")
library(sjlabelled) # For å hente ut informasjon om labels e.l.
# install.packages("lmtest")
library(lmtest) # For å kjøre likelihood ratio test
# install.packages("countrycode")
library(countrycode) # For å endre landkoder
library(sjPlot)

# Laster inn Rdata ---- 
load("ess.rda") # Her må du bytte ut mappestien "../data/" med stien til mappen du lagret data i

str(ess)

# Undersøker labels ----
get_label(ess$trust_eurparl)
summary(ess$trust_eurparl)
get_labels(ess$trust_eurparl)


# Lager nytt datasett uten labels ----
ess_nolabel <- remove_all_labels(ess)

# I tillegg fjerner jeg de observasjonene som har missing på noen 
# av variablene vi skal bruke senere sånn at modellene er nøstede
# mtp observasjoner
ess_nolabel <- ess_nolabel %>% 
  filter(!is.na(income_decile) &
         !is.na(trust_politicians))


# Beregner intraklasekorrelasjon (ICC) ----
# Kjører en nullmodell:
m0 <- lmer(data = ess_nolabel, 
           trust_politicians ~ (1|country))

# Henter ut info:
summary(m0)

# Deler nivå 2 varians (her intercept) på total varians (intercept + residual) 
# for å finne nivå 2s andel av variansen til AVAR:
1.294/(1.294 + 4.642)
# Bør kjøre flernivåanalyse ved ICC >= 0.05 
# Residual variansen skyldes av nivå 1 enhetene, her individer, avviker fra nivå 2
# gjennomsnittet, her land

# Alternativ for å lagre informasjon om varians:
# Vi lagrer først et element med de estimerte variansene
m0var <- VarCorr(m0)

# Så bruker vi print() og ber om å få varians
print(m0var, comp = "Variance")


# Flernivå med uavh. variabel på nivå 1, fixed effects og random intercept ---- 
# Fixed betyr at koeffisienten til variabelen er lik i alle nivå 2 enheter
# Random intercept betyr at vi lar konstantleddet variere mellom nivå 2 enhetene
m1 <- lmer(data = ess_nolabel, 
           trust_politicians ~ (1|country) + income_decile,
           na.action = "na.exclude")

summary(m1)

# Plotter resultatene
# 1. Esimtere modell (gjort)
# 2. Lage plot data
plot_data_m1 <- data.frame(income_decile = rep(1:10, 2),
                        country = c(rep("Sweden", 10), rep("Switzerland", 10)))

# 3. Henter ut predikerte verdier på avhengig variabel og lagrer i plotdata
plot_data_m1$pred <- predict(m1, plot_data_m1)                        

# 4. Plotter
ggplot(plot_data_m1) +
  geom_line(aes(y = pred, x = income_decile, col = country)) +
  theme_minimal() +
  xlab("Inntektsdesil") + 
  ylab("Predikert tillit til politikere") +
  scale_x_continuous(limits = c(1,10)) +
  scale_y_continuous(limits = c(0,10)) +
  ggtitle("Modell med random intercept") + 
  theme(legend.title = element_blank())


# Flernivå med uavh. variabel på nivå 1, random intercept og slopes ----
# Random slopes betyr at vi lar effekten av nivå 1 variabelen, her inntektsdesil, 
# variere mellom nivå 2 enheter
m2 <- lmer(data = ess_nolabel, 
           trust_politicians ~ (income_decile|country) + income_decile,
           na.action = "na.exclude")

summary(m2)
# Dersom varians er lav indikerer det at effekten av inntektsdesil er lik
# i alle land. Hvis det er tilfelle er kanskje fixed slope bedre egnet.  

stargazer(m0, m1, m2, type = "text",
          column.labels = c("Nullmodell", "Fixed slope", "Random slope"))

# Plotter effekter
# 1. Kjører modellen

# 2. Lager plotdata
plot_data_m2 <- data.frame(income_decile = rep(1:10, 2),
                        country = c(rep("Sweden", 10), rep("Switzerland", 10)))

# 3. Lagrer predikerte verdier på AVAR i datasettet
plot_data_m2$pred <- predict(m2, plot_data_m2)                        

# 4. Plotter
ggplot(plot_data_m2) +
  geom_line(aes(y = pred, x = income_decile, col = country)) +
  theme_minimal() +
  theme_minimal() +
  xlab("Inntektsdesil") + 
  ylab("Predikert tillit til politikere") +
  scale_x_continuous(limits = c(1,10)) +
  scale_y_continuous(limits = c(0,10)) +
  ggtitle("Modell med random intercept og random slopes") +
  theme(legend.title = element_blank())


# Slår sammen datasett for å få nivå 2 variabel ---- 
gini <- read_csv("https://raw.githubusercontent.com/martigso/stv4020aR21/main/fordypningsseminar3/OECD_gini.csv") 
str(gini)

# cntry i gini-dataene ligner på cntry i ess
str(ess)

# Lager ny landkodevariabel
gini$cntry2 <- countrycode(gini$cntry, "iso3c", "iso2c")

# Slår sammen datasettene
ess2 <- ess %>% 
  left_join(gini, by = c("cntry" = "cntry2"))

# Sjekker om alle land har fått en giniverdi ved hjelp av is.na
table(is.na(ess2$gini))

# Ingen har missing så dette ser fint ut


# Flernivå med uavh. var på nivå 1 med random effects, random intercept og fixed uavh. variabel på nivå 2 ----

# I denne modellen har vi random intercept, random slopes (income_decile)
# Men holdt effekten av gini konstant på tvers av land

m3 <- lmer(data = ess2, 
           trust_politicians ~  income_decile + (income_decile|country) + gini)



# Plotter først effekten av inntektsdesil (random slope)
plot_data_m3 <- data.frame(income_decile = rep(1:10, 2),
                           country = c(rep("Sweden", 10), rep("Switzerland", 10)),
                           gini = mean(ess2$gini))

plot_data_m3$pred <- predict(m3, plot_data_m3)                        

ggplot(plot_data_m3) +
  geom_line(aes(y = pred, x = income_decile, col = country)) +
  theme_minimal() +
  theme_minimal() +
  xlab("Inntektsdesil") + 
  ylab("Predikert tillit til politikere") +
  scale_x_continuous(limits = c(1,10)) +
  scale_y_continuous(limits = c(0,10)) +
  ggtitle("Random intercept, random slopes") +
  theme(legend.title = element_blank())


# Plotter så effekten av gini (fixed nivå 2)
plot_data_m3b <- data.frame(income_decile = mean(ess2$income_decile, na.rm = TRUE),
                           country = c(rep("Sweden", 10), rep("Switzerland", 10)),
                           gini = c(rep(seq(min(ess2$gini), 
                                            max(ess2$gini), 0.0075), 2)))
plot_data_m3b$pred <- predict(m3, plot_data_m3b)                        

ggplot(plot_data_m3b) +
  geom_line(aes(y = pred, x = gini, col = country)) +
  theme_minimal() +
  theme_minimal() +
  xlab("Gini") + 
  ylab("Predikert tillit til politikere") +
  scale_x_continuous(limits = c(0.2,0.35)) +
  scale_y_continuous(limits = c(0,10)) +
  ggtitle("Random intercept & slopes, fixed nivå 2 (gini)") +
  theme(legend.title = element_blank())
## OBS!! Dette er bare for å illustrere, men husk at vi bare har observasjoner
# av landfor ett år. Derfor har vi egentlig bare en observasjon av gini per land.


## Flernivå med uavh. var på mikronivå med random effects, kryssnivåsamspill, 
# og uavhengig variabel på makronivå:

# Her har vi random intercept (land), random slopes (income_feel) og kryssnivåsamspill
# mellom en variabel på nivå 1 (income_feel) og en variabel på nivå (2).
# Med kryssnivåsamspill så lar vi nivå 2 konteksten påvirke effekten av nivå 1 
# variabelen på avhengig variabel. 

m4 <- lmer(data = ess2, 
           trust_politicians ~ (income_feel|country) + income_feel*gini, 
           na.action = "na.exclude")

summary(m4)

# Plotter effekten av income_feel for ulike nivåer av gini
plot_data_m4 <- data.frame(income_feel = rep(1:4, 4),
                            country = c(rep("Sweden", 8), rep("Switzerland", 8)),
                            gini = c(rep(c(min(ess2$gini), 
                                           min(ess2$gini),
                                           min(ess2$gini),
                                           min(ess2$gini),
                                           max(ess2$gini),
                                           max(ess2$gini),
                                           max(ess2$gini),
                                           max(ess2$gini)), 2)))

plot_data_m4$pred <- predict(m4, plot_data_m4)                        

ggplot(plot_data_m4) +
  geom_line(aes(y = pred, x = income_feel, col = as.factor(gini))) +
  facet_wrap(~country)+ 
  theme_minimal() +
  theme_minimal() +
  xlab("Income feel") + 
  ylab("Predikert tillit til politikere") +
  scale_x_continuous(limits = c(1,4)) +
  scale_y_continuous(limits = c(3,6)) +
  ggtitle("Random intercept og slopes + kryssnivåsamspill") 
# OBS! I dette plottet har jeg "juksa" litt med y-aksen for å få frem forskjellene
# OBS! Husk at dette ikke er tidsserie så vi har egentlig bare en gini-verdi per 
# land. Derfor er det et lite egnet plot der vi gjør prediksjoner langt utover 
# datamaterialet vårt. I dette tilfellet har jeg gjort det først og fremst for å
# vise dere hvordan disse slår ut.


# Hvordan hente ut koeffisienter ----
# Fixed effects
fixef(m2)

# Random effects
ranef(m2)

# Koeffisienter
coef(m2)
# Random slopes i ranef() tilsvarer differensansen mellom konstantleddet vi 
# henter ut med fixef() og interceptene vi får med coef()

# Som eksempel: 
fixef(m2)[1] - coef(m2)$country$`(Intercept)`[1]

# Varians:
# Vi lagrer først et element med de estimerte variansene
m2var <- VarCorr(m2)

# Så bruker vi print() og ber om å få varians
print(m2var, comp = "Variance")

# Henter frem summary fra modellen vår med random intercept og random slop
summary(m2)
# Variansen til income_decile er tilnærmet lik null. 
# Vi kan også bruke en logisk test for å sjekke om variansen er minst 
# dobbelt så stor som standardavviket. Det er den ikke. 
0.001434 > 2*0.03787


# Modellevaluering ---
AIC(m0,m1,m2,m3)
BIC(m0,m1,m2,m3)


# Tar en LR-test
lrtest(m1, m2)
# Positiv og signifikant LR-test betyr at den fullstendige modellen er signifikant
# bedre tilpaset datamaterialet enn den reduserte. NB! må være nøstede modeller.

# Sentrerering ----
# I flernivåanalyse er hovedregelen at uavhengige variabler bør sentreres:
ess_nolabel$income_decile_sent <- ess_nolabel$income_decile - mean(ess_nolabel$income_decile, na.rm = TRUE)
summary(ess_nolabel$income_decile_sent)
# Gjennomsnittet er lik null 
# NB! I flernivåanalyse kan man sentrere på snitt for alle nivå 1-enhetene eller
# på grupperte gjennomsnitt for hver nivå 2 enhet. 


# Plotte koeffisienter ----- 

plot_model(m2, type = "re", order = TRUE)
