### Forskjell mellom ICC og fixed effects?
Det er to forskjellige ting. ICC forteller oss hvor mye av variansen I avhengig variabel som nivå 2-enhetene står for (her er land nivå2-enhter). Fixed effect er en måte å spesifisere modellen, der vi antar at effekten av en variabel er den samme for alle enhetene på samme nivå.

### Hva er forskjell på iso3c og iso2c?
Det er to forskjellige standarder for å kode forkortelser på land. Den ene bruker typisk to bokstaver (så Norge blir f. eks. NO) mens den andre går for tre bokstaver (Norge blir NOR). Vi må ha samme standard for å kode land hvis vi skal bruke variabelen som koblingsnøkkel, noe vi trenger når vi skal slå sammen datasett. Derfor gjør vi en omkoding med pakken "countrycode".

### Hva betyr rep(), hvorfor bruker vi det i plot_data?
c(rep("Sweden", 8)) er tilsvarende å skrive c("Sweden", Sweden", Sweden", Sweden", Sweden", Sweden", Sweden", Sweden"). Det er to grunner til at vi repeterer slike verdier. (1) Vi må sette flere verdier for å få alle verdikombinasjoner som er relevant å predikere på, og (2) for å få like lange kolonner slik at de kan slås sammen til en dataframe.

### Hva forteller egentlig plot_data oss (dvs. dataframen vi lager når vi bruker predict() ) ?
Vi (1) Lager et oppsett, et scenario (eller fiktiv verden), og (2) bruker modellen for å predikere avhengig variabel på dette scenarioet. Output blir prediksjoner gitt at du holder visse variabler konstant og lar andre variere.
