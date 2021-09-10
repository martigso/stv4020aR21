Oppgaver seminar 4
================

## Introduksjon

Datasettet «beer» består av 336 observasjoner og 10 variabler.
Observasjonene er av amerikanske stater i tidsperioden 1982-1988. Det er
ikke missing-data.

## Variablene

| Variabel |                   Beskrivelse                    |
|----------|:------------------------------------------------:|
| state    |                  state ID code                   |
| year     |                       year                       |
| mrall    |     traffic fatality rate (deaths per 10000)     |
| beertax  |         tax on case of beer (percentage)         |
| mlda     |            minimum legal drinking age            |
| jaild    |   mandatory jail sentence after drunk-driving?   |
| comserd  | mandatory community service after drunk-driving? |
| vmiles   |             average miles per driver             |
| unrate   |                unemployment rate                 |
| perinc   |            per capita personal income            |

## Oppgaver

1.  Importer datasettet beer.csv eller beer.Rdata fra [data-mappen på
    github](https://github.com/liserodland/stv4020aR/tree/master/H20-seminarer/Innf%C3%B8ringsseminarer/data)
    som et objekt i R-Studio. Du skal bruke dette datasettet i alle
    oppgavene.

2.  Hvilken klasse har variablene i datasettet? Vis hvordan du finner ut
    av dette med kode.

3.  Lag et spredningsplot (scatter-plot) med `beertax` på x-aksen, og
    `mrall` på y-aksen.

4.  Lag et nytt datasett basert på `beer`, bestående av variablene
    `year`, `mrall`, `beertax`, `vmiles`, `unrate` og `perinc.` Lag en
    korrelasjonsmatrise med utgangspunkt i det nye datasettet.

5.  Opprett et nytt datasett med alle observasjoner fra år 1982 i det
    opprinnelige datasettet, og et datasett med alle observasjoner fra
    år 1988 i det opprinnelige datasettet. Hva er gjennomsnittlig skatt
    på øl og gjennomsnittlig dødsrate per 10000 innbygger i de to
    datasettene? Oppgi gjennomsnittene i en kommentar.

6.  Kjør en lineær regresjon med `mrall` som avhengig variabel og
    `beertax`, `vmiles`, `unrate` og `perinc` som uavhengige variabler.
    Lagre modellen som et objekt. Tolk koeffesienten til `beertax`.

7.  Opprett en ny variabel i datasettet ditt, `state_fac`, ved å omkode
    variabelen `state` til en factor. Lag deretter et boxplot med
    `state_fac` på x-aksen og `mrall` på y-aksen. Lag deretter det samme
    plottet som i oppgave 3, men legg til argumentet
    `facet_wrap(~state_fac)`. Hva leser du fra dette plottet?

8.  Kjør en lineær regresjon med `mrall` som avhengig variabel og
    `beertax`, `vmiles`, `unrate`, `perinc` og `state_fac` som
    uavhengige variabler. Lagre modellen som et objekt. Tolk
    koeffesienten til `beertax`. (P.S.: ved å legge til variabelen
    `state_fac` spesifiserer vi en modell med det som kalles fixed
    effects).

9.  Lag en ny variabel `comserd_d` som tar verdien:

-   1 dersom `comserd` har verdien “yes”

-   0 dersom `comserd` har verdien “no”

Sjekk ved hjelp av en tabell at det ble riktig.

10. Kjør en binomisk logistisk regresjon med `comserd_d` som avhengig
    variabel og `unrate`, `perinc`, `mrall` og `mlda` som uavhengige
    variabler. Hva kan si om effekten av antall trafikkdødsfall?

11. Hva er gjennomsnitt og median til variabelen beertax? Vis hvordan du
    kommer frem til det via kode og oppgi verdiene i en kommentar.
