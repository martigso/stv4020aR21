Oppgaver seminar 3
================

## Introduksjon

Dagens oppgaver er designet for å trene forståelsen din av regresjon, og
bruker mye plotting til dette formålet.

## Instruksjoner:

Last inn `aid`-datasettet.

I oppgavene under får du oppgitt sett av variabler. I hver regresjon,
bruker du `aid$gdp_growth` som avhengig variabel, og variablene du får
oppgitt som uavhengige variabler.

-   For oppgave 1 lag et spredningsplot der du visualiserer alle
    variablene som nevnes, samt `aid$gdp_growth` som du setter på
    y-aksen. For oppgave 2 og 3, lag en korrelasjonsmatrise mellom
    variablene i oppgaven samt `aid$gdp_growth`.
-   For hver oppgave, kjør minst to forskjellige
    regresjonsspesifikasjoner, der den ene er lineær, mens den andre
    inneholder samspill og/eller høyeregradspolynomer/matematiske
    transformasjoner eller andre omkodinger av variabler. Du står også
    fritt til å forsøke forskjellige omkodinger.

BONUS:

-   For hver regresjon, plot resultatene med konfidensintervaller.

Tips: I tillegg til argumentet `col =` i `ggplot()` , kan du bruke
`shape =`, `alpha =`, m.m. Du kan også bruke `facet_wrap()` med en eller
flere variabler (velg variabler med et begrenset antall verdier - dersom
du vil bruke denne strategien når du bare har kontinuerlige variabler,
bruk `cut()` til å lage en ny variabel som deler en kontinuerlig var. i
kategorier). Du kan også se på oppsummeringen fra [seminar
2](https://github.com/martigso/stv4020aR21/blob/main/seminar2/seminar2.md)
for tips om `ggplot()`.

## Oppgaver:

1.  Variabler: `aid`, `policy`, `period`.
2.  Variabler: `aid`, `economic_open`, `budget_balance`, `inflation` -
    Merk at de tre siste variablene ble brukt til å opprette `policy`-
    variablene.
3.  Variabler: `aid`, `period`, `policy`, `institutional_quality`,
    `assasinations`. Bruk argumentet `use = "complete.obs"` i
    korrelasjonsmatrisen - dette kaster ut de samme observasjonene som
    kastes ut av regresjonsanalysen din pga. missing.
