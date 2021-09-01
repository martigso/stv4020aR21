# stv4020aR
Repo for R-undervisning i STV4020A høsten 2021

## **Linker**
- [Last ned R](http://cran.uib.no/)
- [Last ned Rstudio](https://www.rstudio.com/products/rstudio/download/#download)
- [R cheat sheet](https://s3.amazonaws.com/quandl-static-content/Documents/Quandl+-+R+Cheat+Sheet.pdf)
- [Stilguide for R](https://google.github.io/styleguide/Rguide.xml)
- [Mappestruktureringsforslag](https://nicercode.github.io/blog/2013-04-05-projects/)
- [Bruke prosjekter i R](https://support.rstudio.com/hc/en-us/articles/200526207-Using-Projects)
- [Guide til ggplot2](http://docs.ggplot2.org/current/)
- [Data fra Fivethirtyeight](https://github.com/fivethirtyeight/data)
- [LaTex (Windows)](https://www.latex-tutorial.com/installation/)
- [LaTex (Mac)](https://www.tug.org/mactex/)


## **Praktisk info**

Velkommen til R-seminar i STV4020A!

I uke 37 setter vi endelig i gang med R-seminarer! R er et programmeringsspråk som blir brukt mye til statistiske beregninger og dataanalyse. Dette semesteret skal dere lære noen grunnleggende ferdigheter i R. Vi skal blant annet lære hvordan vi kan lage fine plot og kjøre dataanalyser.  

Vi er så heldige at innføringsseminarene i uke 37 vil gå fysisk (med unntak av seminargruppe 1), men R-prøven 20. september blir digital. Vi kommer ofte til å ha seminarundervisning i rom uten PC-er så alle må ha med egen laptop med R og Rstudio installert på seminar. Fordypningsseminarene i uke 38 går digitalt og er for de som ønsker å lære mer om bruk av R. Det blir ingen prøve etter fordypnignsseminarene, men forhåpentligvis har dere lært mye dere kan bruke i hjemme- og/eller masteroppgaven. Tre fordypningsseminarer kjøres på samme dag på ulike tidspunkt, og de har samme tema.

Dere har sikkert mange spørsmål om seminaret så her skal jeg prøve å svare på noen av dem. Men først, for at seminarene skal gå så smidig som mulig, er det viktig at du:

- møter opp på seminar.
- jobber med å løse oppgaver mellom seminarene.

For noen kan R virke skummelt ved første øyekast, men erfaringsmessig så går dette veldig fint så lenge du legger ned litt innsats underveis :)

*Hva må jeg gjøre før første seminar?*

Før første seminar må du:

- Lese [installasjonsguiden](https://github.com/martigso/stv4020aR21/blob/main/Installasjonsguide.md) og denne [introduksjonen til R](https://github.com/liserodland/stv4020aR/blob/master/H20-seminarer/Innf%C3%B8ringsseminarer/docs/installasjonsguide_R.md).
- Laste ned og installere R og Rstudio (se installasjonsguide over).
- Dersom du har R og Rstudio installert fra før så sjekk hvilken versjon du har og om du bør oppdatere (se installasjonsguide over). 
- Bli gjerne med i facebookgruppen [R for statsvitenskap ved UiO](https://www.facebook.com/groups/427792970608618) og [discord-serveren "R-stv"](https://discord.gg/CAP9TbdWFa) (dette gjelder spesielt de som skal bruke R i hjemmeoppgaven).

Om du har problemer med installasjonen kan du [melde deg opp til installasjonshjelp på dato mandag 6.9 eller onsdag 8.9 her](https://nettskjema.no/a/212858#/page/1). Vi har lite tid på seminar og kommer derfor ikke til å kunne hjelpe med installasjonsproblemer der. Dersom du har mac og har problemer med installasjonen så prøv å oppdatere macen først.

*Hva gjør jeg om jeg blir syk?*

Blir du syk eller får covid-19-symptomer skal du ikke møte på fysisk seminar, men om du er i form til det så kan du følge det digitale innføringsseminaret. For å følge det digitale seminaret (seminargruppe 1) må du sende en e-post til lise.rodland@stv.uio.no senest klokken 12 samme dag som seminaret går for å få Zoom-lenke. Det digitale innføringsseminaret går mandag, tirsdag, torsdag og fredag kl. 14.15-16. Tilbudet om å delta på den digitale seminargruppen er forbeholdt de som er meldt opp i andre seminargrupper, men som ikke kan delta på eget seminar pga. sykdom.  

*Når er R-prøven?*

R-seminarene blir etterfulgt av en digital prøve 20. september som du må bestå for å kunne gå opp til eksamen i STV4020A. Dersom du ikke består på første forsøk så får du et nytt forsøk 29. september. Dersom du er syk på prøven må du søke om utsatt obligatorisk aktivitet [her](https://www.uio.no/studier/eksamen/obligatoriske-aktiviteter/sv-fraver-fra-obligatorisk-aktivitet.html). 


## **Hvorfor lære R?**

Matematikk kan ved første øyekast virke litt fremmed for en statsviter, men statistikk står i kjernen av statsvitenskap. Begrepet "Statistik" ble introdusert av Gottfried Achenwall i 1749 for å omfatte analyse om staten, og bygger på det latinske "statisticum", som betyr "om staten". Den gangen var statistikk begrenset til å samle informasjon i et tabellformat, og kanskje regne ut noen summer eller gjennomsnitt. Slik var det lenge, og statistiske operasjoner før i tiden kunne være en tung prosess med hyllevis med ruteark og langsomme utregninger. Heldigvis er vi ikke der lenger. Inntoget av datamaskiner har endret alt.

Historien om datamaskiner er et eget kapittel, men utviklingen har gått fort, fra hullkort til i dag. For å ta i bruk datamaskinenes enorme regnekraft bruker vi programmeringsspråk, og R er et av de mest brukte programmeringsspråkene for statistiske formål. Fordi språket er open source - altså at hvem som helst kan bidra til å lage funksjoner - kan vi dra på en hel verden sin erfaring rundt programmering og bruk av statistiske modeller.


### **Hvordan jobbe med R?**

Når vi jobber med data i R, går vi typisk gjennom noen vante steg, som vist i figuren under:

![Hvordan jobbe med data i R](https://d33wubrfki0l68.cloudfront.net/571b056757d68e6df81a3e3853f54d3c76ad6efc/32d37/diagrams/data-science.png)
https://r4ds.had.co.nz/introduction.html 

  1. Lese inn data til R, enten det er fra en excel-fil, en STATA-fil eller annet (import)
  2. Rydde opp i dataene, f. eks. ta ut missing og slå sammen datasett (tidy)
  3. Ordne dataene slik at de passer til ditt forskningsspørsmål, f. eks. omkode variabler eller finne gjennomsnitt (transform)
  4. Plotte data for å få en visuell oversikt over dataene, f. eks. stolpediagram for å sammenlikne to variabler eller linjediagram for å se utvikling over tid (visualize)
  5. Bruke modeller for å finne svar på forskningsspørsmål, f. eks. OLS eller logistisk modell (model)
  6. Kommunisere resultatene, f. eks. i en rapport med tilhørende tabeller og figurer (communicate)


### **Hvordan finne datasett?**

Velger dere en kvantitativ framgangsmåte i hjemmeoppgaven, vil dere sannsynligvis gå fram som beskrevet over. Da er en første utfordring å finne datasett. Her er noen lenker dere kan ta utgangspunkt i for å finne aktuelle datasett til hjemmeoppgaven deres. Listen er ikke uttømmende, og det finnes nok mye bra som ikke er listet opp her også. Dere må selv sette dere ned med kodebok og vurdere om dataene er relevante og av god nok kvalitet for din problemstilling. Vi begynner med instituttet og UB sine ressuser: 

- Dere finner mange datasett sortert på tema på [ISV sin side "Statistikk, datasett og ressurser på nett"](https://www.sv.uio.no/isv/tjenester/kunnskap/datasett/)
- [UB sin fagside med lenker til noen datasett (se nederst)](https://www.ub.uio.no/fag/samfunn-politikk/statsvit/)

Noen eksempler (mulig disse også er lenket til via ISV sin side): 

- [The Nonviolent and Violent Campaigns and Outcomes (NAVCO) Data Project](https://www.du.edu/korbel/sie/research/chenow_navco_data.html)
- [Varities of democracy](https://www.v-dem.net/en/)
- [Data on armed conflict fra PRIO](https://www.prio.org/Data/Armed-Conflict/)
- [European social survey](https://www.europeansocialsurvey.org/) 
- [Quality of government](https://www.gu.se/en/quality-government/qog-data) 
- [Afrobarometro](https://www.afrobarometer.org/)
- [Latinobarometro](https://www.latinobarometro.org/lat.jsp)
- [Political party database project (PPDB)](https://www.politicalpartydb.org/)
- [Parliaments and government database (PARLGOV)](http://www.parlgov.org/)

For replikasjonsdata: 

- Søk etter tema eller artikkel på [Harvard dataverse](https://dataverse.harvard.edu/)
- Sjekk også tidsskriftet artikkelen er publisert i
- Sjekk fotnoter i artikkelen om det står noe om hvor data er gjort tilgjenglig
- Ikke alle legger ut repliksjonsdata dessverre 

