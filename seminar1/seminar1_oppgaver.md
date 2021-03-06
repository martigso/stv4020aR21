Oppgaver seminar 1
================

### Oppgaver fra dagens tema:

Opprett et nytt script og kjør følgende kode:
`titanic <- read.csv("https://raw.githubusercontent.com/martigso/stv4020aR/master/Gruppe%203/data/titanic.csv")`.
Du laster da inn et datasett som inneholder informasjon om passasjerer
på Titanic, til objektet `titanic`.

1.  Hvilke variabler finnes i datasettet?
2.  Hvilken klasse er variablene?
3.  Hvor mange observasjoner er det i datasettet?
4.  Var det flest menn eller kvinner på Titanic?
5.  Hva er gjenomsnittsalderen for menn som var på Titanic? (Tips:
    kombiner mean() med det vi har lært om indeksering)
6.  Lag en logisk test for om den eldeste mannen på Titanic var yngre
    enn den eldste kvinnen.
7.  Hent ut passasjerene som er eldre enn 70 år. Lagre observasjonene
    som et objekt. Hvor mange var det?
8.  Hvor mange missing (NA’s) er det på aldersvariabelen?
9.  Lag et datasett som bare inneholder observasjoner av de som gikk om
    bord i Queenstown (de som har verdien “Q” på variabelen Embarked).
    (Tips: kombiner &lt;- med det vi har lært om indeksering)

### Oppgaver om morgendagens tema:

10. Lag en ny variabel som har verdien 1 dersom passasjeren er eldre enn
    gjennomsnittet og 0 ellers. Lagre den i titanic datasettet.
11. Lag en ny variabel som er Age opphøyd i annen.
12. Kjør en OLS-regresjon med Survived som avhengig variabel og Pclass,
    Sex og Age som uavhengige variabler.
