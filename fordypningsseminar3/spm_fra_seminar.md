### Forskjell mellom ICC og fixed effects?
Det er to forskjellige ting. ICC forteller oss hvor mye av variansen I avhengig variabel som nivå 2-enhetene står for (her er land nivå2-enhter). Fixed effect er en måte å spesifisere modellen, der vi antar at effekten av en variabel er den samme for alle enhetene på samme nivå.

### Hva er forskjell på iso3c og iso2c?
Det er to forskjellige standarder for å kode forkortelser på land. Den ene bruker typisk to bokstaver (så Norge blir f. eks. NO) mens den andre går for tre bokstaver (Norge blir NOR). Vi må ha samme standard for å kode land hvis vi skal bruke variabelen som koblingsnøkkel, noe vi trenger når vi skal slå sammen datasett. Derfor gjør vi en omkoding med pakken "countrycode".
