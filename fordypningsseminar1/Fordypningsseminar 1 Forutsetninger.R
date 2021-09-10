
library(tidyverse)

load("aid.RData")

aid <- aid %>% 
  mutate(log_gdp_pr_capita = log(gdp_pr_capita),
         period_fac = as.factor(period),
         region = ifelse(fast_growing_east_asia == 1, "East Asia",
                         ifelse(sub_saharan_africa == 1, "Sub-Saharan Africa", "Other")),
         region = factor(region, levels = c("Other", "Sub-Saharan Africa", "East Asia")))

m5 <- lm(data = aid, 
         gdp_growth ~ log_gdp_pr_capita + ethnic_frac*assasinations + 
           institutional_quality + m2_gdp_lagged + region + policy*aid +
           period_fac, 
         na.action = "na.exclude")



library(ggplot2)

ggplot(aid) + 
  geom_point(aes(y = gdp_growth, x = policy)) +
  geom_smooth(aes(y = gdp_growth, x = policy), 
              se = FALSE) +
  theme_bw()


## # Kjører modellen uten samspill for å illustrere ceresplot()
## model5_usam <- lm(gdp_growth ~ log_gdp_pr_capita + ethnic_frac + assasinations +
##                institutional_quality + m2_gdp_lagged + region + policy + aid +
##                period_fac,
##              data = aid, na.action = "na.exclude")
## 
## stargazer(model5_usam, type = "text")
## 
## # installerer og laster inn pakken
## # install.packages("car")
## library(car)
## 
## ceresPlot(model5_usam, "aid")
## ceresPlot(model5_usam, "policy")

#install.packages("plm")
# library(plm)

# Kjører modellen på ny uten å bevare missingverdier
m5b <- lm(gdp_growth ~ log_gdp_pr_capita + ethnic_frac * assasinations + 
               institutional_quality + m2_gdp_lagged + region + policy * aid +
               period_fac,
             data = aid, na.action = "na.omit")
# Her blir det problemer om vi bevarer na med na.exclude. 

car::durbinWatsonTest(m5b)

ggplot() +
  geom_histogram(aes(x = rstandard(m5),
                     y = ..density..)) + 
  stat_function(fun = dnorm, 
                color = "goldenrod2") # Plotter inn en normalfordeling

car::qqPlot(m5)

#install.packages("moments")
library(moments)
kurtosis(rstandard(m5), na.rm = TRUE)
skewness(rstandard(m5), na.rm = TRUE)

car::residualPlot(m5)

car::vif(m5)


car::influenceIndexPlot(m5,
                   id = list(n=3))

# Bruker indeksering til å se nærmere på noen av observasjonene
aid[c(39,86), ]


table(aid$country) # ingen suspekte verdier

aid$reg_miss <- aid %>%
  select(gdp_growth, aid, policy) %>%
  complete.cases()

# Lager variabel som viser hvilke observasjoner som forsvinner i regresjon med de sentrale variablene
# gdp_growth, aid og policy - fin å bruke i plot for å få et inntrykk av hva slags informasjon du mister ved å legge til flere kontrollvariabler.
table(aid$reg_miss) # 47 observasjoner har missing på en eller flere av de tre variablene

# Kjører en modell med litt færre variabler
m1 <- lm(gdp_growth ~ aid*policy + as.factor(period) + ethnic_frac*assasinations, data = aid )
summary(m1) # output viser at 48 observasjoner fjernes pga. missing

# Siden as.factor(period) lager en dummvariabel for alle perioder unntatt periode 1, må vi gjøre dette for å inkludere denne variabelen i korrelasjonsmatrisen (inkluder gjerne også periode 1 i matrise):

aid$period2 <- ifelse(aid$period==2, 1, 0)
aid$period3 <- ifelse(aid$period==3, 1, 0)
aid$period4 <- ifelse(aid$period==4, 1, 0)
aid$period5 <- ifelse(aid$period==5, 1, 0)
aid$period6 <- ifelse(aid$period==6, 1, 0)
aid$period7 <- ifelse(aid$period==7, 1, 0)
aid$period8 <- ifelse(aid$period==8, 1, 0)

aid %>% 
  select(gdp_growth,aid,policy, ethnic_frac,assasinations,period2,period3,period4,period5,period6,period7) %>%
  cor(, use = "pairwise.complete.obs")

# Alternativet "pairwise.complete.obs" fjerner bare missing for de enkelte bivariate korrelasjonene
aid %>% 
  select(gdp_growth,aid,policy, ethnic_frac,assasinations,period2,period3,period4,period5,period6,period7) %>%
  cor(, use = "complete.obs")

# Alternativet "complete.obs" fjerner alle observasjoner som har missing på en av variablene som inngår, mao. det samme som regresjonsanalysen.

miss_mod <- glm(reg_miss ~ aid*policy + as.factor(period), data = aid)
summary(miss_mod) # ingen store forskjeller

# I denne modellen ønsker du ikke signifikante uavhengige variabler

rm(m1, m5, m5b, miss_mod, model5_usam)


# Lager dikotom variabel
aid <- aid %>% 
  mutate(gdp_growth_d = ifelse(gdp_growth <= 0, 0, 1))

# Lager et datasett med variablene vi trenger og uten NA
aid.complete <- aid %>% 
  select(gdp_growth_d, aid, policy, period) %>%  # Velger variablene vi skal bruker
  na.omit()                                      # Beholder bare observasjoner uten NA 

# Lager en nullmodel
gm0 <- glm(gdp_growth_d ~ aid, data = aid.complete, 
           family = binomial(link = "logit"))

# Lager en modell med avhengige variabler
gm1 <- glm(gdp_growth_d ~ aid + policy + as.factor(period), data = aid.complete, 
           family = binomial(link = "logit"))
anova(gm0, gm1, test = "LRT") # Sjekk de forskjellige alternativene til test med ?anova.glm


#install.packages("ResourceSelection")
library(ResourceSelection)

# Sjekker nullmodellen
hoslem.test(gm0$y, gm0$fitted.values)

# Sjekker den alternative modellen vår
hoslem.test(gm1$y, gm1$fitted.values)

# install.packages("pscl")
library(pscl)
pR2(gm0)
pR2(gm1)

