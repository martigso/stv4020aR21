#knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)

# Her ser dere et eksempel på R-kode
# Jeg bruker # for å skrive inn en kommentar som ikke skal evalueres av R

# Her lager vi vektoren Thea, Ole, Mari
# Vi bruker ctrl/cmd + enter for å kjøre koden
c("Thea", "Ole", "Mari")

# Her lagrer vi vektoren Thea, Ole, Mari som et objekt vi kaller navn:  
navn <- c("Thea", "Ole", "Mari")


alder <- c(23, 20, 25)
alder

bachelor <- c("UIO", "UIB", "UIS")
bachelor

data <- data.frame(navn, alder, bachelor) # Her slår vi de tre vektorene sammen for å lage et datasett
data


"Hello world!"

1 + 1  # addisjon
2 - 3  # subtraksjon
4/2    # divisjon
2 * 2  # multiplikasjon
2^3    # potens
exp(2) # eksponentiering
log(2) # logaritme (default er naturlig logaritme)
2 * (4-2)/(4-2) # Parentesregler fungerer som i vanlig algebra: den innerste parentesen regnes ut først

1 == 2                                # tester om 1 er lik 2
2 == 2                                # tester om 2 er lik 2
"Statsvitenskap" == "statsvitenskap"  # Logiske tester kan også brukes på tekst
"statsvitenskap" == "statsvitenskap"  # R er imidlertid sensitivt til store og små bokstaver
1 <= 2                                # Tester om 1 er mindre enn eller lik 2
1 >= 2                                # Tester om 1 er større enn eller lik 2
1 != 2                                # Tester om 1 er ulik 2
1 == 2 | 1 == 1                       # Tester om en av de to påstandene 1 er lik 2 eller 1 er lik 1 er sanne
1 == 2 & 1 == 1                       # Tester om begge de to påstandene 1 er lik 2 og 1 er lik 1 er sanne

## install.packages("pakkenavn") # Laster ned filene pakken består av fra nett til PC - må bare gjøres en gang
## library(pakkenavn)            # Tilgjengeliggjør pakken i R-sesjonen, må gjøres hver gang du vil bruke pakken i en ny sesjon

## # install.packages("tidyverse") # Fjern hashtag på starten av denne og neste linje!
## library(tidyverse)

library(dplyr)

# Endimensjonal vektor:
navn[1]

# For todimensjonale vektorer så gjelder dette generelt:
# data[rad, kolonne] henter ut en gitt rad og en gitt kolonne 
# data[rad, ] henter ut en alle kolonner for en gitt rad
# data[, kolonne] henter ut alle rader for en gitt kolonne


# Vi kan bruke base R (som beskrevet i Lær deg R)
data[navn == "Thea", ]      # vi ønsker alle kolonner/variabler for observasjone/radene med verdien "Thea" 
data[data$navn == "Thea", ] # vi ønsker alle kolonner/variabler for observasjone/radene med verdien "Thea"
data[1, ]                   # Thea er den første raden (observasjonen) i datasettet

# Vi kan også bruke tidyverse-pakken dplyr:
data %>%                    
  filter(navn == "Thea") 



# Ved hjelp av base R
data$alder                  
data[, "alder"]
data[, 2]                    # Alder er kolonne nr to fra venstre

# Ved hjelp av dplyr 
data %>% 
  select(alder)



# Ved hjelp av base R
data[navn == "Thea", "alder"]

data[data$navn == "Thea", "alder"]

data$alder[data$navn == "Thea"]

# Ved hjelp av dplyr
data %>% 
  filter(navn == "Thea") %>% 
  select(alder)


"Atomic vector" <- c("numeric", "integer", "character", "factor", "logical")
"List" <- c("blanding", "", "", "", "")
tabell <- cbind(`Atomic vector`, List)


class(data$navn)
class(data$alder)
class(data$bachelor)

glimpse(data)


# Her lager vi en ny variabel alder_ch der vi ber R lagre alder som character
data$alder_ch <- as.character(data$alder)

# Slik ser datasettet ut
glimpse(data)


## aFunction(x = "R-objekt", arg = "alternativ for figurens oppførsel")
## ## Merk: dette er ikke en faktisk funksjon i R. Funksjoner kan også ha andre syntakser.

# Viser de første tre radene i datasettet
head(data)

# Finner minimumsverdi (den laveste alderen)
min(data$alder, na.rm = TRUE) # na.rm = TRUE sier at missing skal droppes i beregningen

# Finner maksimumsveriden (den høyeste alderen)
max(data$alder, na.rm = TRUE)

# Finner gjennomsnittalder
mean(data$alder, na.rm = TRUE)

# Finner medianalderen
median(data$alder, na.rm = TRUE)

# Finner standardavviket
sd(data$alder, na.rm = TRUE)

# Finner varians
var(data$alder, na.rm = TRUE)

# Finner kvantilverdiene
quantile(data$alder, na.rm = TRUE)

# Finner forskjellig deskriptiv statistikk for en variabel
summary(data$alder)

# Finner forskjellig deskriptiv statistikk for alle variabler i datasettet
summary(data)

# Finner skjevhet ved hjelp av moments-pakken
# install.packages("moments")
library(moments)
skewness(data$alder, na.rm = TRUE) 

# Finner kurtose ved hjelp av moments-pakken
kurtosis(data$alder, na.rm = TRUE)

# Får en tabell med de ulike studiestedene
table(data$bachelor, useNA = "always") # useNA = "always" betyr at vi også vil ha med antall "missing" 


## getwd()
## list.files()

## setwd("C:/Users/Navn/R/der/du/vil/jobbe/fra")   # For windows
## setwd("~/R/der/du/vil/jobbe/fra")               # For mac/linux
## # Merk at R bruker / for å skille mellom mappenivåer, mens filutforsker i Windows bruker \
## # Kopierer du mappebanen fra filutforsker må du derfor huske å snu skråstrekene i R

## library(tidyverse) # read_funksjoner fra readr i tidyvsere
## datasett <- read_filtype("filnavn.filtype") # Laster inn og lagrer datasettet som et objekt
## read_csv("filnavn.csv") # for .csv, sjekk også read.table
## load("") # For filer i R-format.
## 
## library(haven) # Fra haven-pakken - dette skal vi se på i senere seminar
## read_spss("filnavn.sav")  # for .sav-filer fra spss
## read_dta("filnavn.dta") # for .dta-filer fra stata

## # knitr::purl("./seminar1/Seminar1.Rmd", output = "./seminar1/seminar1.R", documentation = 2)
