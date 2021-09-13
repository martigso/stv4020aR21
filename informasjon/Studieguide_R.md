Studieguide til R for STV4020A H19
================
Erlend Langørgen
12 august 2019

## Introduksjon

Denne guiden er særlig rettet mot dem som skal skrive en statistisk
hjemmeoppgave, men den kan også være til hjelp for andre som ønsker å
bli flinkere i R. Dersom du skal skrive hjemmeoppgave med R, må du regne
med å bruke litt tid på å lære R ut over seminarene. Du vil få
læringsressurser skreddersydd til STV4020A i seminarene, men det finnes
også utallige læringsressurser på nett. Du kommer noen ganger til å
trenge slike læringsressurser for å løse utfordringer som vi ikke dekker
i seminarene. Vi kommer til å trene på å finne og bruke
tilleggsressurser i seminarene. Det kan likevel være vanskelig å vite
hvordan det er lurt å gå frem for å lære seg tilstrekkelig R til
hjemmeoppgaven på en mest mulig effektiv måte.

Heldigvis har Google og mer spesialiserte søk hos
[stackoverflow](https://stackoverflow.com/questions/tagged/r),
[R-seek](https://rseek.org/), [Quick-R](https://www.statmethods.net/)
stort sett alltid svaret på det du lurer på i R, men dersom du ikke har
tilstrekkelig basisforståelse av R kan det være vanskeligå anvende
svarene du finner på nett. Det kan også være vanskelig å vite hva du
skal spørre om. Derfor er det lurt å tilegne seg et solid fundament av
basisferdigheter ganske raskt. For å tilegne deg disse basisferdighetene
anbefaler vi at du i tillegg til seminarene gjennomføre studieplanen
skissert under basert på den gratis E-boken [R for Data
Science](https://r4ds.had.co.nz/). Dersom du står fast anbefales
forøvrig også den nybegynner-vennlige [facebookgruppen for R-brukere på
statsvitenskap](https://www.facebook.com/groups/427792970608618/)

Det finnes mange andre gode introduksjonsbøker til R, så hvorfor akkurat
*R for Data Science*? I R finnes det stort sett mange forskjellige koder
som gir deg samme resultat, enten det er snakk om dataforberedelser,
statistiske analyser, grafikk eller andre ting. Dette skyldes at R er
open source, ulike programmere skriver funksjoner som de deler med andre
R-brukere gjennom **pakker**. Ulike programmerere skriver ulike
funksjoner for å løse samme problem. Dette kan være forvirrende for
ferske R-brukere som googler etter en løsning på problemet sitt på
internett. Så lenge koden din fungerer, er alt greit. Det er imidlertid
lurt å lære seg et sett av basisferdigheter - metoder for å løse
problemer som du ofte kommer til å støte på i forbindelse med
hjemmeoppgaven. Ved å investere tid i basisferdigheter, sparer du mye
tid på frustrerende googling. Du vil også få en grunnforståelse av R som
gjør det lettere for deg å forstå svarene du finner på nett, samt og
stille de riktige spørsmålene. *R for Data Science* dekker alle
basisferdigheter godt, og er konsistent i bruken av funksjoner fra
[tidyverse](https://www.tidyverse.org/). Pakkene i dette universet
kommer til å bli brukt i seminarene, fungerer godt sammen, brukes stadig
mer i svarene du finner på google, og gjør det lettere å få gode
R-vaner. Resten av denne guiden handler stort sett om hvordan du kan
bruke *R for Data Science* effektivt.

**Lær deg R** av Silje Synnøve Lyder Hermansen er et mer skreddersydd
alternativ for R-brukere på statsvitenskap ved UiO. Denne boken er også
god, men vær oppmerksom på at den bruker base-R i stedet for tidyverse.
I første omgang er det lurt å fokusere på å lære seg en fremgangsmåte
for å løse problemer på godt. Velg derfor enten base og **Lær deg R**
eller tidyverse. Etter hvert som dere mestrer en av fremgangsmåtene, er
det mye god læring i å beherske både `base` og `tidyverse` - da blir det
lettere å lese andres kode.

## Hvordan bruke R for Data Science

**Først:** Bruk **R for Data Science** for å lære deg R, ikke som en
statistikktekst eller mal for analyse. Dette gjelder spesielt for deg
som ikke gjør en replikasjon, fordi boken har et hypotesegenerende
perspektiv (se seksjon 1.3.4 og 22.1).

**Kapittel 1-12** gir deg en solid grunnforståelse av R, og disse
kapitlene (eller tilsvarende materiale fra boken til Silje/andre kilder)
bør jobbes gjennom av alle som skal skrive hjemmeoppgave med R, gjerne
tidlig i semesteret. **Kapittel 13-16** gir en solid innføring i
grunnferdigheter som vil være essensielle for noen hjemmeoppgaver, men
som ikke er relevanter for andre - disse kapitlene kan du lese etter
behov/lyst. I **Kapittel 17-21 og kapittel 25** lærer du grunnleggende
programmeringsferdigheter. Disse ferdighetene er ikke nødvendig for å
lykkes med R til hjemmeoppgaven, men er vel verdt å investere tid i
dersom du ønsker å komme deg til neste nivå i R. Dersom du får problemer
med at en funksjon ikke fungerer slik du hadde forventet på en variabel,
ta en titt på **kap. 20**. **Kapittel 22-24** handler om
hypotesegenerende modellering - gode kapitler, men ikke mest relevant
for hjemmeoppgaver/R-ferdigheter. Les hvis du har lyst. **Kapittel 27,
29 og 30** introduserer R Markdown, som det er mulig å skrive
hjemmeoppgaven i.
[Markdown](https://bookdown.org/yihui/rmarkdown/how-to-read-this-book.html)
er en mindre syntaks-tung variant av
[Latex](https://www.latex-project.org/), begge fungerer ypperlig til å
skrive kvantitativ oppgave med R (men tar litt tid å sette seg inn i).
Du kan også bruke R Markdown dersom du skriver i word og sliter med å
lage fine figurer/tabeller, da R Markdown og R studio gjør det lett å
eksportere dette til word-dokumenter. **Kapittel 28** lærer deg hvordan
du kan lage finere figurer til hjemmeoppgaven med ggplot2, og er vel
verdt en titt.

Du trenger imidlertid ikke lese alle kapitlene like nøye. Under følger
mine forklaringer av hva du vil lære i hvert kapittel i den mest
essensielle delen, **kapittel 1-12**, samt hva du lærer i **kapittel
13-16**. Dersom du står fast med R, vil det å jobbe seg gjennom disse
kapitelen garanteret være til hjelp dersom du ikke har gjort det
allerede. Dette er fordi at du uten tilstrekkelige basisferdigheter kan
ha vanskelig for å komme i gang/forstå hva må du gjøre/forstå hva
problemet ditt er.

-   **Kapittel 1:** Dette kan du skumme, men det er lurt å lese (svært
    kort).

-   **Kapittel 2:** Dette kan du skumme, men det er lurt å lese (svært
    kort).

-   **Kapittel 3:** Dette kapittelet lar deg lage mange figurer, for å
    venne deg til å kjøre R-kode. Dersom du føler at du ikke aner hva
    som foregår, fokuser på å kjøre kode/tenke raskt på spørsmål, eller
    kom tilbake til kapittelet etter kapittel 6. Dersom du ikke henger
    med, er det en god indikasjon på at du bør ta en ekstra titt på
    script fra introduksjonsforelesning og seminar 1. Dersom du klarer å
    henge med, lærer du å lage forskjellige typer plot - dette er vel
    verdt å bruke tid på.

-   **Kapittel 4:** Dette er en veldig kort intro til hvordan R virker,
    repetisjon av ting fra introduksjonsforelesningen. Jobb raskt
    gjennom.

-   **Kapittel 5:** Dette kapittelet er helt sentralt. Du bør bruke god
    tid på å fordøye alt som står der, da du garantert vil få bruk for
    det til hjemmeoppgaven.

-   **Kapittel 6:** Dette er en veldig kort intro til hvordan
    Rstudio/script virker. Delvis repetisjon av ting fra
    introduksjonsforelesningen. Jobb raskt gjennom.

-   **Kapittel 7:** Sammen med kapittel 3, gir kapittel 7 en innføring
    av visualisering av deskreptiv statistikk med **ggplot2**, vel verdt
    å bruke litt tid på. Les gjerne kapitlene i sammenheng.

-   **Kapittel 8:** Gir deg gode arbeidsvaner i R (se særlig seksjon
    8.4). Ta en titt på dette senest ved oppstart av hjemmeoppgaven,
    gjerne før. Kort kapittel.

-   **Kapittel 9:** Dette kan du skumme, men det er lurt å lese (svært
    kort).

-   **Kapittel 10:** Kort kapitel. Sammen med seminarene, kapittel 11 og
    12, lærer du det viktigste om alt som har med datasett å gjøre her.
    Se særlig 10.3.2. og 10.4

-   **Kapittel 11:** Hvordan laste inn data i R - høyaktuelt for
    hjemmeoppgaven. Dersom du klarer å laste inn data du trenger, kan du
    hoppe over 11.3 og 11.4. Dersom du har datasett fra andre
    statistikkprogram, kan du vurdere å hoppe rett til 11.6. For de
    fleste vil, det trolig holde med `read_csv()` og `write_csv()`.

-   **Kapittel 12:** Les 12.1, 12.2 og særlig 12.5 (missing data) nøye.
    Du kan skumme gjennom resten av seksjonene med mindre du finner ut
    at du trenger funksjoner fra disse seksjonene (for de fleste vil
    ikke dette være nødvendig).

-   **Kapittel 13:** Dersom du skal kombinere variabler/informasjon fra
    flere datasett bør du lese dette kapittelet først. Det forklarer alt
    du trenger å vite.

-   **Kapittel 14:** Dersom du vil lage en numerisk variabel basert på
    en tekstvariabel, eller gjøre andre operasjoner på tekst, bør du
    lese dette kapittelet.

-   **Kapittel 15:** Dersom du opplever feilmeldinger som sier noe om
    klassen til en variabel eller om `factor` bør du ta en titt på dette
    kapittelet. Du vil nesten helt sikkert få bruk for dette kapittelet
    på ett eller annet tidspunkt, så det kan være vel verdt å gjøre for
    alle. Særlig relevant for dem som ikke forstår hvorfor egen kode
    ikke fungerer.

-   **Kapittel 16:** Les dette kapittelet dersom du skal jobbe med
    datoer/tidspunkt.

For å sikre deg at du har forstått kapitlene du jobber med er det fint å
løse oppgavene. [Her](https://jrnold.github.io/r4ds-exercise-solutions/)
finner dere løsningsforslag. Jeg har ikke sett gjennom alt, så ta
kontakt med meg eller en annen seminarleder dersom det er noe som ikke
fungerer/du lurer på.

I seminarene kommer vi til å dekke noen momenter fra **R or Data
Science**, men vi kommer også til å bruke mye tid på ferdigheter du ikke
finner i denne boken. Her er likevel en røff oversikt over sammenhengen
mellom undervisning og Kapitler i **R for Data Science**, slik at du kan
ta en ekstra titt i **R for Data Science** før eller etter seminar for
ekstra læringseffekt (denne oversikten kan komme til å endres):

Introduksjonsforelesning: Kap. 1-4, litt kapittel 10-12 og litt kapittel
20 (siste del om variabeltyper) Seminar 1: Kap. 5 - dplyr, kap. 11
import av data og Kap. 3 + 7 - ggplot()  
Seminar 2: Kap. 13 (join) og kap. 8 - organisering Seminar 3: Kap. 12
(den delen som handler om missing) og en smakebit på kap. 16 (datoer)
Seminar 4: Smakebit på kap. 14-15 (tekst og faktor) Seminar 5: Noen
momenter fra kap. 27-29 (lage fin output)
