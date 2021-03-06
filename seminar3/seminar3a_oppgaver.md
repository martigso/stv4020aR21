Oppgaver seminar 2
================

1.  Last ned de aid-fire datasettene i [datamappen på
    github](https://github.com/liserodland/stv4020aR/tree/master/H20-seminarer/Innf%C3%B8ringsseminarer/data).
    Dette skal vi gjøre for å lære å laste inn ulike typer data. Lagre
    datasettene i ditt working directory eller i prosjektmappen. Last
    inn de ulike dataformatene. Tips: bruk `rm(objektnavn)` mellom hver
    innlastning for å slette det lagrede objektet.

2.  Kjør modell (5) på s. 856 i [Burnside and
    Dollar (2000)](https://www.jstor.org/stable/117311) uten å bearbeide
    data noe mer. `gdp_growth` er avhengig variabel. De uavhengige
    variablene bør være mulige å kjenne igjen. Se gjerne på
    oppsummeringen fra [seminar
    2](https://github.com/martigso/stv4020aR21/blob/main/seminar2/seminar2.md)
    for å repetere de ulike variablene i datasettet. Husk å lagre
    modellen som et objekt, og ikke glem samspillsleddene.

3.  Som du ser blir ikke resultatene de samme. For å replisere
    artikkelen må du gjøre noen flere justeringer:

-   Logtransformer variabelen gdp\_pr\_capita (Tips: husk å lagre den
    nye variabelen i datasettet ved hjelp av `aid$`)
-   Kontroller for variabelen periode (Tips: i datasettet er variabelen
    numerisk. Blir det riktig eller burde det vært en faktor?)
-   Sett område-dummiene (`sub_saharan_africa` og
    `fast_growing_east_asia`) til riktig målenivå (Tips: se hva vi
    gjorde når vi lagde region-variabelen i seminar 2, men legg merke
    til at her inngår Central America i “Other”)

4.  Kjør modellen på ny, denne gangen med faste effekter for periode.
    Husk å lagre modellen som et objekt.

5.  Sammenlign resultatene fra de to modellene i en tabell. Last den inn
    i word, latex eller ditt foretrukne tekstredigeringsprogram.

**Gratulerer! Du har nå replisert en forskningsartikkel :) **

6.  Lag et plot som viser sammenhengen mellom gdp\_pr\_capita (BNP per
    innbygger) og den logtransformerte varianten av variabelen. Hvordan
    påvirker logtransformasjone betydningen av en enhets økning i
    gdp\_pr\_capita? Tips: bruk `geom_point()` om du bruker funksjonen
    `ggplot()`.
