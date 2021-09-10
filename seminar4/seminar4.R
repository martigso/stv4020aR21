library(tidyverse)
aid <- read_csv("https://raw.githubusercontent.com/liserodland/stv4020aR/master/H20-seminarer/Innf%C3%B8ringsseminarer/data/aid.csv")

# Lager ny dikotom variabel
aid <- aid %>% 
  mutate(gdp_growth_d = ifelse(gdp_growth <= 0, 0, 1))

# Sjekker missing på ny variabel ved hjelp av en tabell
table(is.na(aid$gdp_growth_d), 
      is.na(aid$gdp_growth))


# Kjører en binomisk logistisk modell ved hjelp av glm
m1 <- glm(gdp_growth_d ~ aid + policy + as.factor(period), data = aid, 
          family = "binomial",
          na.action = "na.exclude")
summary(m1)

library(stargazer)

# Viser resultatene i en tabell
stargazer(m1, type = "text")

# Enklere formel for å regne ut sannsynligheten når alle uavhengige variabler har verdien 0:
exp(m1$coefficients["(Intercept)"])/
  (1 + exp(m1$coefficients["(Intercept)"]))


# Her henter jeg ut koeffisientene fra modellobjektet ved hjelp av indeksering
# og setter inn verdien på uavhengig variabel inn i likningen: 
exp(m1$coefficients["(Intercept)"] +
      m1$coefficients["as.factor(period)4"]*1 + 
      m1$coefficients["aid"]*(1) + 
      m1$coefficients["policy"]*median(aid$policy, na.rm = TRUE))/
  (1 + exp(m1$coefficients["(Intercept)"] + 
      m1$coefficients["as.factor(period)4"]*1 + 
      m1$coefficients["aid"]*(1) + 
      m1$coefficients["policy"]*median(aid$policy, na.rm = TRUE)))

# Trinn 2: Vi lager et datasett med plotdata der vi lar aid variere
plotdata <- data.frame(aid = seq(min(aid$aid, na.rm = TRUE), 
                                    max(aid$aid, na.rm = TRUE), 1),
                       policy = mean(aid$policy, na.rm = TRUE),
                       period = "4")

# Trinn 3: Bruker først predict til å predikere logits
preds <- predict(m1, 
                 se.fit = TRUE,
                 newdata = plotdata,
                 type = "link") # Dette gir oss logits


# Trinn 4: Lagrer predikert verdi og standardfeil i plotdata
plotdata$fit <- preds$fit
plotdata$se <- preds$se.fit

# Eksempel på plot uten konfidensintervall:
ggplot(plotdata) + 
  geom_line(aes(x = aid, y = fit))

# Trinn 5: Regner ut 95 % konfidensintervaller
plotdata <- plotdata %>% 
  mutate(ki.lav = fit - 1.96*se,
         ki.hoy = fit + 1.96*se)

# Trinn 6: Legger KI til i plottet
ggplot(plotdata) + 
  geom_line(aes(x = aid, y = fit)) + 
  geom_line(aes(x = aid, y = ki.lav), linetype = "dotted") + 
  geom_line(aes(x = aid, y = ki.hoy), linetype = "dotted") +
  theme_minimal()

# Snarvei for å plotte sannsynlighet:
# Brukt i Lær deg R, men kan gi konfidensintervaller som går utenfor referanseområdet
# Her gjenbruker vi trinn 1 og 2 fra tidligere og går rett på trinn 3
# Trinn 3: Bruker predict med type = response for å få sannsynligheter
preds <- predict(m1, 
                 se.fit = TRUE,
                 newdata = plotdata,
                 type = "response") # Velger respons for å få sannsynlighet


# Trinn 4: Lagrer informasjon om predikerte verdier og standardfeil fra predict i plotdata
plotdata$fit.prob <- preds$fit
plotdata$se.prob <- preds$se.fit

# Trinn 5: Lager 95 % konfidensintervaller
plotdata <- plotdata %>% 
  mutate(ki.lav.prob = fit.prob - (1.96*se.prob),
         ki.hoy.prob = fit.prob + (1.96*se.prob))


# Trinn 6: Plotter med konfidensintervaller
ggplot(plotdata) + 
  geom_line(aes(x = aid, y = fit.prob)) + 
  geom_line(aes(x = aid, y = ki.lav.prob), linetype = "dotted") + 
  geom_line(aes(x = aid, y = ki.hoy.prob), linetype = "dotted") +
  theme_minimal() +
  scale_y_continuous(limits = c(0:1))


summary(plotdata$ki.lav.prob)
summary(plotdata$ki.hoy.prob)

# Her gjenbruker vi trinn 1 - 4 fra når vi predikerte logits og går rett på trinn 5
# Trinn 5: regner om sannsynligheter fra logits-prediksjonene og lagrer i plotdata
plotdata$ki.lav.prob2  <- exp(plotdata$fit - 1.96*plotdata$se)/(1 + exp(plotdata$fit - 1.96*plotdata$se))
plotdata$ki.hoy.prob2 <- exp(plotdata$fit + 1.96*plotdata$se)/(1 + exp(plotdata$fit + 1.96*plotdata$se))
plotdata$fit.prob2 <- exp(plotdata$fit)/(1+ exp(plotdata$fit))

# Trinn 6: plotter
ggplot(plotdata) + 
  geom_line(aes(x = aid, y = fit.prob2)) + 
  geom_line(aes(x = aid, y = ki.lav.prob2), linetype = "dotted") + 
  geom_line(aes(x = aid, y = ki.hoy.prob2), linetype = "dotted") +
  theme_minimal() +
  scale_y_continuous(limits = c(0:1))



summary(plotdata$ki.lav.prob2)
summary(plotdata$ki.hoy.prob2)

# Nå ligger alle verdiene til konfidensintervallene innenfor mulighetsområdet 0-1. 

# NB! Legg merke til at den predikerte verdien fortsatt er den samme. Her bruker
# jeg table() og en logisk test til å teste dette og alle er like. 
table(plotdata$fit.prob == plotdata$fit.prob2)


# Henter ut residualer og lagrer dem i datasettet
aid$resid <- residuals(m1)
# Henter ut predikerte sannsynligheter og lagrer dem i datasettet
aid$predict <- predict(m1, type = "response")

summary(aid$predict)


kuttpunkt <- mean(aid$gdp_growth_d, na.rm = TRUE)
kuttpunkt
# I nullmodellen predikerer vi 62 prosent riktig om vi gjetter at alle land har vekst

# Lager en variabel der de med predikert sannsynlighet høyere enn kuttpunktet får verdien 1
aid$growth.pred <- as.numeric(aid$predict > kuttpunkt)

# Bruker en logisk test til å sjekke om predikert verdi er lik faktisk verdi
aid$riktig <- aid$growth.pred == aid$gdp_growth_d
mean(aid$riktig, na.rm = TRUE)

## mean(aid[aid$gdp_growth_d == 1, ]$riktig, na.rm = TRUE)
## mean(aid[aid$gdp_growth_d == 0, ]$riktig, na.rm = TRUE)

krysstabell <- table(aid$growth.pred, aid$gdp_growth_d)
krysstabell
prop.table(krysstabell, margin = 2)


#install.packages("plotROC")
library(plotROC)

# Basic ROC:
basicplot <- ggplot(aid, aes(d = gdp_growth_d, m = predict)) + geom_roc(labelround = 2) 

# Pyntet ROC med AUC.
basicplot + 
    style_roc(ylab = "Sensivity (True positive fraction)") +
  theme(axis.text = element_text(colour = "blue")) +
  annotate("text", x = .75, y = .25, 
           label = paste("AUC =", round(calc_auc(basicplot)$AUC, 2))) +
  scale_x_continuous("1 - Specificity", breaks = seq(0, 1, by = .1))


## # knitr::purl("./seminar4/seminar4.Rmd", output = "./seminar4/seminar4.R", documentation = 0)
