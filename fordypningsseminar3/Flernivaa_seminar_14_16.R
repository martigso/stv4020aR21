
library(lme4) 
library(tidyverse) 
library(stargazer) 
library(sjlabelled)
library(lmtest) 
library(lmerTest)
library(countrycode)

load("ess.rda")

glimpse(ess)

## Hva kan vi gjøre når data er labelled?

summary(ess$trust_eurparl)

get_label(ess$trust_eurparl)

get_labels(ess$trust_eurparl)

# Fjerner labels:
ess_nolabel <- remove_all_labels(ess)

# I tillegg fjerner jeg de observasjonene som har missing på noen 
# av variablene vi skal bruke senere sånn at modellene er nøstede mtp observasjoner
ess_nolabel <- ess_nolabel %>% 
  filter(!is.na(income_decile) & !is.na(trust_politicians))


#### Hva er flernivå? ####

ess %>% head()

library(ggraph)
library(igraph)

edge_list_1_2 <- ess %>%
  mutate(world = "World") %>%
  select(world, country) %>% 
  unique %>% 
  rename(from=world, to=country)

edge_list_2_3 <- ess %>% 
  group_by(country) %>%
  sample_n(20) %>%
  ungroup() %>%
  select(country, idno) %>% 
  unique %>% 
  rename(from=country, to=idno)

edge_list = rbind(edge_list_1_2, edge_list_2_3)

mygraph <- graph_from_data_frame(edge_list)
ggraph(mygraph, 
       layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal() +
  geom_node_point(color = "goldenrod") +
  # geom_node_text(aes(label=name, filter=leaf), 
  #                angle=90, 
  #                hjust=1,
  #                overlap = FALSE) +
  theme_void()

rm(edge_list_1_2, edge_list_2_3, edge_list, mygraph)


## Vi har strukturerte data: 
# Avhengige observasjoner som kan variere systematisk innenfor en gruppe.
# Standardfeilen undervurderes.

# Reduserer faktisk utvalgsstørrelse, og derfor overvurderer signifikans.

# Bør helst ha over 30 nivå 2-enheter og over 30 nivå 1-enheter innenfor hver nivå 2-enhet.


#### Er det nødvendig å bruke flernivåanalyse? ####

# Intraklassekorrelasjon: Andel varians som nivå 2-enheter svarer for.

## Ser på avhengigheten til nivå 1-enheter innenfor hver nivå 2-enhet.
## Er individer innenfor hvert land såpass like at vi bør bruke flernivå?

# Kjører modell med kun random intercept, 
# dvs. at konstantleddet varierer mellom nivå 2-enheter.
# Konstantleddet er gjennomsnittet av avhengig variabel for alle nivå 2-enheter.

m0 <- lmer(data = ess_nolabel, 
           trust_politicians ~ (1|country))

## Metode 1

summary(m0)
# Konstantledd: Gjennomsnittlig tillit til politikere i alle land
# country-varians: Her 1.294. Ulike land avviker noe fra gjennomsnittet på tillit. 
# individ-varians: Residual, her 4.642. Folk i ulike land har forskjellig tillit.


1.294/(1.294 + 4.642)

## Metode 2
# Vi lagrer først et element med de estimerte variansene
m0var <- VarCorr(m0)

# Så bruker vi print() og ber om å få varians
print(m0var, comp = "Variance")

# Land svarer for ca 22 prosent av variansen til tillit til politikere.

# ICC >= 0.05 tyder på at flernivå bør brukes.


#### Hva slags modellspesifikasjon bør vi bruke? ####


#### Alternativ 1: ####

## 1. Random intercept: Gjennomsnittlig tillit varierer mellom land.
## 2. Fixed effects: Inntektskoeffisienten antas å være den samme for alle innbyggere.
## 3. En uavhengig variabel på nivå 1.


m1 <- lmer(data = ess_nolabel, 
           trust_politicians ~ (1|country) + income_decile,
           na.action = "na.exclude")

summary(m1)

plot_data_m1 <- data.frame(income_decile = rep(1:10, 2),
                           country = c(rep("Sweden", 10), 
                                       rep("Switzerland", 10)))

plot_data_m1$pred <- predict(m1, plot_data_m1)                        

ggplot(plot_data_m1) +
  geom_line(aes(y = pred, x = income_decile, color = country)) +
  theme_minimal() +
  xlab("Inntektsdesil") + 
  ylab("Predikert tillit til politikere") +
  scale_x_continuous(limits = c(1,10)) +
  scale_y_continuous(limits = c(0,10)) +
  ggtitle("Modell med random intercept") + 
  theme(legend.title = element_blank())


#### Alternativ 2: ####

## 1. Random intercept: Gjennomsnittlig tillit varierer mellom land.
## 2. Random slopes: Effekten av inntekt varierer mellom land
## 3. En uavhengig variabel på nivå 1.

m2 <- lmer(data = ess_nolabel, 
           trust_politicians ~ (income_decile|country) + income_decile,
           na.action = "na.exclude")

summary(m2)

plot_data_m2 <- data.frame(income_decile = rep(1:10, 2),
                           country = c(rep("Sweden", 10), rep("Switzerland", 10)))

plot_data_m2$pred <- predict(m2, plot_data_m2)                        

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


## Sammenlikner modeller:
stargazer(m0, m1, m2, 
          type = "text")


#### Alternativ 3: ####

## 1. Random intercept: Gjennomsnittlig tillit varierer mellom land.
## 2. Random slopes: Effekten av inntekt varierer mellom land.
## 3. En uavhengig variabel på nivå 1 (som får variere)
## 4. En uavhengig variabel på nivå 2 (som holdes konstant)

## Da må vi først slå sammen datasett

gini <- read.csv("OECD_gini.csv") 

glimpse(gini)
glimpse(ess)

# cntry i gini-dataene ligner på cntry i ess

library(countrycode) 

# Lager ny landkodevariabel
gini$cntry2 <- countrycode(gini$cntry, 
                           "iso3c", "iso2c")

# Slår sammen datasettene
ess2 <- ess %>% 
  left_join(gini, by = c("cntry" = "cntry2"))

# Sjekker om alle land har fått en giniverdi ved hjelp av is.na
table(is.na(ess2$gini))
# Ingen har missing så dette ser fint ut

m3 <- lmer(data = ess2, 
           trust_politicians ~ income_decile + (income_decile|country) + gini)

plot_data_m3 <- data.frame(income_decile = rep(1:10, 2),
                           country = c(rep("Sweden", 10), rep("Switzerland", 10)),
                           gini = mean(ess2$gini))

plot_data_m3$pred <- predict(m3, plot_data_m3)                        

# Plotter effekten av inntekt
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

# Plotter effekten av gini
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
# av land for ett år. Derfor har vi egentlig bare en observasjon av gini per land.


#### Alternativ 4: ####

## 1. Random intercept: Gjennomsnittlig tillit varierer mellom land.
## 2. Random slopes: Effekten av inntekt varierer mellom land.
## 3. En uavhengig variabel på nivå 1 (som får variere)
## 4. En uavhengig variabel på nivå 2 (som holdes konstant)
## 5. Kryssnivåsamspill: Antar at effekten av en individvariabel avhenger av landvariabel.

m4 <- lmer(data = ess2, 
           trust_politicians ~ (income_feel|country) + income_feel*gini, 
           na.action = "na.exclude")

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


#### Hente ut modellinformasjon #####

# Modell 2:
## 1. Random intercept: Gjennomsnittlig tillit varierer mellom land.
## 2. Random slopes: Effekten av inntekt varierer mellom land
## 3. En uavhengig variabel på nivå 1.


fixef(m2) # Fixed intercept 
ranef(m2) # Random intercept og slopes
coef(m2) # Koeffisienter
# Random slopes i ranef() tilsvarer differensansen mellom interceptene vi får med coef()

# Vi lagrer først et element med de estimerte variansene
m2var <- VarCorr(m2)

# Så bruker vi print() og ber om å få varians
print(m2var, comp = "Variance")

summary(m2)


# Variansen til income_decile er tilnærmet lik null. 
# Variansen er ikke mer enn dobbelt så stor som standardavviket.
0.001434 > 2*0.03787


#### Modellvurdering #####

# Sammenlikner modeller for å se hvilken modell som er best tilpasset data.

AIC(m0,m1,m2,m3) # Akaike Information Criterion
# Lavere verdier er bedre tilpasning

BIC(m0,m1,m2,m3) # Bayesian Information Criterion
# Lavere verdier er bedre tilpasning

lrtest(m1, m2) # Likelihood-ratio
# Positiv + signifikant betyr at den mer komplekse modellen er best tilpasset data.




#### Kommunisere ####

## Sentrering:
# For hver nivå 1-enhet, trekk fra gjennomsnittet på uavhengige variabler (grand mean).
ess_nolabel$income_decile_sent <- ess_nolabel$income_decile - mean(ess_nolabel$income_decile, na.rm = TRUE)
summary(ess_nolabel$income_decile_sent)

# Forenkler tolkningen av flernivåanalyse


## Plotte:
library(sjPlot)

plot_model(m2, type = "re")
