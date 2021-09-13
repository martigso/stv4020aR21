##############################################
##### LØSNINGSFORSLAG OPPGAVER SEMINAR 1 #####
##############################################

# Laster først inn data
library(tidyverse)
titanic <- read.csv("https://raw.githubusercontent.com/martigso/stv4020aR/master/Gruppe%203/data/titanic.csv")

## OPPGAVE 1
names(titanic)

## OPPGAVE 2
glimpse(titanic)
# Variablenes klasse står til høyre for variabelnavnene (chr = character, dbl/int = numerisk klasse)
# Vi kan også bruke class() på hver enkel variabel: 
class(titanic$PassengerId)

## OPPGAVE 3
# Datasettet har 891 observasjoner. Det kan vi lese under beskrivelse i Environment
# Alternativt så kan du bruke denne koden for å finne antall rader (ikke brukt i seminar):
nrow(titanic)

## OPPGAVE 4
table(titanic$Sex, useNA = "always")
# Det var flest menn. Vi tar med argumentet "useNA = "always"" så vi også for informasjon om eventuelle missingverdier 

## OPPGAVE 5
# Base R:
mean(titanic[titanic$Sex == "male", ]$Age, na.rm = TRUE) # Finner gjennomsnittsaldere til menn ved hjelp av indeksering

## OPPGAVE 6
# Lager en logisk test med større eller lik (>=)
max(titanic[titanic$Sex == "male", ]$Age, na.rm = TRUE) <= max(titanic[titanic$Sex == "female", ]$Age, na.rm = TRUE)

## OPPGAVE 7
# Dette kan vi gjøre på to måter
# Med tidyverse
over70 <- titanic %>% 
  filter(Age > 70)

# Med indeksering
over70_2 <- titanic[titanic$Age > 70, ]  # Her fikk vi også med alle de vi ikke vet alderen på
over70_2 <- titanic[titanic$Age > 70 & !is.na(titanic$Age), ] # Derfor må vi si at vi ønsker de observasjonene som er over 70 OG (&) som ikke har missing

## OPPGAVE 8
summary(titanic$Age)

## OPPGAVE 9
titanic_q <- titanic %>% # Sier at vi vil lage et nytt objekt titanic_q som tar utgangspunkt i datasettet titatnic
  filter(Embarked == "Q")  # Her er det viktig å huske hermetegn fordi Embarked er en character variabel

## OPPGAVE 10
titanic$age_above_mean <- ifelse(titanic$Age > mean(titanic$Age, na.rm = TRUE), 1, 0)

## OPPGAVE 12
titanic$age_squared <- titanic$Age^2

## OPPGAVE 13
m1 <- lm(Survived ~ Pclass + Sex + Age, 
         data = titanic, na.action = "na.exclude")
summary(m1)
