---
title: "Installasjonsguide"
author: "Lise Rødland"
date: "22.2.2021"
output: 
  pdf_document: 
      keep_md: yes
  github_document: default
    
---
#### Hvordan innstallere og oppdatere R og Rstudio? 

Du må installere R og Rstudio før første seminar. Dersom du allerede har installert R og Rstudio så er det fint om du oppdaterer til siste versjon før seminar. Om alle har samme versjon så vil seminarene gå mye smidigere. 

Har du R og Rstudio installert? 

* Ja - følg instruksjonene under "Hvordan oppdatere R og Rstudio".
* Nei - følg instruksjonene under "Hvordan installere R og Rstudio". 

##### Hvordan installere R og Rstudio

*Windows*:

1. Åpne en nettleser og gå til [www.r-project.org](https://www.r-project.org). 
2. Klikk på "download R" lenken. Den er i første avsnitt under "Getting started". 
3. Velg CRAN location. Her burde du gå for Norway. Klikk på lenken.  
4. Klikk på "Download R for Windows" på toppen av siden. 
5. Klikk på "install R for the first time". 
6. Klikk på "Download R for Windows" og lagre filen på maskinen din. Når nedlastningen er ferdig så åpner du .exe-filen og følger installasjonsveiledningen. 
7. Nå kan du laste ned Rstudio. 

*Mac*: 

1. Åpne en nettleser og gå til [www.r-project.org](https://www.r-project.org). 
2. Klikk på "download R" lenken. Den finner du i første avsnitt under "Getting started". 
3. Velg CRAN location. Her burde du gå for Norway. Klikk på lenken. 
4. Klikk på "Download R for (Mac) OS X" på toppen av siden. Du må lese det som står på siden du kommer til nå nøye dersom du har Mac. 
5. Velg den R-versjonen som passer den operativsystemversjonen du har. Om du har en eldre MacBook så er det viktig at du klikker på riktig lenke. Er du usikker på hvilken macOS-versjon du har så kan du klikke på eplet i venstre hjørne og velge "about this mac". Der skal det stå macOS etterfulgt av navnet på versjonen.
6. Klikk på lenken som matcher din versjon og lagre filen på maskinen. Når nedlastningen er ferdig så åpner du filen og følger installasjonsveiledningen. 
7. Nå kan du laste ned Rstudio. 

##### Installere Rstudio

1. Gå til [www.rstudio.com](https://www.rstudio.com). 
2. Trykk på "Download".
3. Trykk "Download" knappen under gratisversjonen av Rstudio Desktop. 
4. Finn den versjonen som er anbefalt for operativsystemet ditt. Klikk på lenken i kolonnen "Download" for å laste ned installasjonsfilen. 
5. Kjør filen og følg installasjonsveiledning.

Nå er du klar for første R-seminar!

#### Hvordan oppdatere R og Rstudio
Seminarene går smidigere om alle bruker samme versjon av R. Dersom du har R og Rstudio installert fra før så kan du følge denne oppskriften for å sjekke hvilken versjon du har og eventuelt oppdatere dersom nødvendig.   

###### Verifiser R versjon

Åpne Rstudio. Øverst i konsoll så vil du se "session info". Den første linjen forteller deg hvilken R versjon du bruker. Alternativt så kan du kjøre koden `R.version.string` for å printe R versjonen. 

Har du R versjon 4.1.0) installert?

* Nei - følg instruksjonene for å “Oppdatere R og Rstudio”
* Ja - supert! Du er klar for første R-seminar!

###### Oppdatere R og RStudio

*Windows*

For å oppdatere R på en Windows maskin så kan du prøve å bruke pakken `installr`. 

1. Installer og last inn `installr`: skriv inn og kjør kodene `install.packages("installr")` og `library(installr)`

2. Skriv `updateR()` og kjør koden. Dette vil starte oppdateringsveilederen til R. 

3. For å oppdatere Rstudio så åpner du Rstudio, velger Help > Check for Updates. Dette vil sende deg til Rstudios nettside hvor du kan laste ned den nyeste versjonen. 

Nå er du klar for første R-seminar!

*Mac*

På Mac så laster du selv ned og installerer den nyeste versjonen av R. Når du restarter Rstudio så vil den  oppdaterte versjonen av R automatisk bli tatt i bruk. *NB! Her er det snakk om den nyeste versjonen av R som er kompatibel med din Mac.* 

1. Følg installasjonsveiledningen for MAC.

2. For å oppdatere Rstudio så åpner du Rstudio, velger Help > Check for Updates. Dette vil sende deg til Rstudios nettside hvor du kan laste ned den nyeste versjonen. 

Nå er du klar for første R-seminar!
