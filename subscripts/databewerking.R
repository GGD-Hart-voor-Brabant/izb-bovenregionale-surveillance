


########################
# Inlezen en opschonen #
########################

# Initialisatie van max_maand indien er een kwartaalrapport moet worden gemaakt ipv een jaarrapport. Wordt gebruikt om de cases, situations (en
# principal contextual settings) en telefoon data te filteren. In de grafieken wordt alle data getoond, in de tabellen alleen de aantallen van het
# specifieke kwartaal (+ hetzelfde kwartaal een jaar eerder).
# type_rapport wordt gedefinieerd in RegionaleSurveillance.Rmd en moet worden aangepast door de gebruiker.

# als type_rapport = 1 (Q1, neem de max qua jaar en gooi alles vanaf 1 april maxJaar eruit)
# als type_rapport = 2 (Q2, neem de max qua jaar en gooi alles vanaf 1 juli maxJaar eruit)
# als type_rapport = 3 (Q3, neem de max qua jaar en gooi alles vanaf 1 okt maxJaar eruit)
# 4 = Q4, 5 = jaarrapport. 

if (type_rapport %in% c(1, 2, 3, 4)) {
  max_maand <- type_rapport * 3 + 1
} else if (type_rapport %in% c(5)){
  max_maand <- 13
}




#########
# Inwonersaantallen
#Sheetnaam argument verwijderd; er is maar 1 sheet in dit bestand & de naam van
#die sheet is afhankelijk van de taalinstellingen van de PC van diegene 
#die het subscript Inwonersaantallen draait
inwoners <- lees_bestand(bestand = pad_naar_inwonersaantallen) %>% 
  
  # Zet om naar long format
  pivot_longer(cols = starts_with("20"), # Eerste twee cijfers van een jaartal. Gaat er vanuit dat er geen data voor 2000 en na 2099 wordt ingeladen.
               names_to = "Jaar", 
               values_to = "Inwoners") %>% 
  
  # Inwonersaantallen per 1 jan 2020 worden gebruikt voor cases ingevoerd in 2021. Zet daarnaast kolom Jaar om van tekst naar numeric, tel er 1 bij op, en maak koppelcode aan om te kunnen koppelen naar de aantallen uit cases
  mutate(Jaar_rapport = as.numeric(Jaar) + 1,
         Jaar = as.numeric(Jaar),
         koppelcode = paste0(GGD, "_", Jaar_rapport)) 


#########
# Osirisdata

landelijk <- lees_bestand(bestand = pad_naar_Osiris_bestand, sheet = naam_sheet_Osiris_data) %>%
  
  rename(Infectieziekte = Ziekte.RIVM,
         Aantal = Landelijke.aantallen.in.rapportageperiode) # Hernoem kolommen

# RIVM en HPZone omschrijven dezelfde ziekten, maar de spelling kan anders zijn. Dit is een probleem als je de twee wilt koppelen, daarvoor moet 
# de spelling overeenkomen. Pas daarom in de lijst van het RIVM de namen aan naar hoe ze in HPZone staan geschreven

# Lees het bestand in waarin de link tussen RIVM en HPZone is omschreven. 
Link_RIVM_HPZone <- lees_bestand(pad_naar_link_RIVM_HPZone, sheet = naam_sheet_link_RIVM_HPZone)

# Voor elke regel in het RIVM bestand, kijk of die ziekte voorkomt in het link-tussen-RIVM-en-HPZone bestand. Zo ja, overschrijf de RIVM naam dan 
# met de naam van HPZone
for (i in 1:length(landelijk$Infectieziekte)){
  if (landelijk$Infectieziekte[i] %in% Link_RIVM_HPZone$Ziekte.RIVM){
    landelijk$Infectieziekte[i] <- Link_RIVM_HPZone$Meldingsplichtige.ziekte.HPZone[Link_RIVM_HPZone$Ziekte.RIVM == landelijk$Infectieziekte[i]]
  }
}


#########
# Cases

cases <- lees_bestand(pad_naar_cases_bestand) %>%
  
  # # Vervang spaties in kolomnamen met punten, om aanroepen makkelijker te maken
  # rename_all(list(~make.names(.))) %>% 
  
  # Laat corona achterwege
  filter(!Meldingsplichtige.ziekte %in% c("COVID-19", "Coronavirus", "Wuhan Novel Coronavirus")) %>% 
  
  # Hercodeer meerdere variabelen.
  # "Invasieve pneumokokkenziekte  (bij kinderen)" mag samengevoegd worden met de gewone pneumokokken. Spelling in HP is met 1 spatie teveel voor de (bij kinderen).
  mutate(Meldingsplichtige.ziekte = recode(Meldingsplichtige.ziekte, "Invasieve pneumokokkenziekte  (bij kinderen)" = "Invasieve pneumokokkenziekte"),
         
         # Zet alle Hepatitis B en C in eerst instantie naar niet gespecificeerd. In de volgende mutate wordt, waar van toepassing, omgezet naar
         # acuut en chronisch
         Meldingsplichtige.ziekte = recode(Meldingsplichtige.ziekte, "Hepatitis B" = "Hepatitis B, niet gespecificeerd"),
         Meldingsplichtige.ziekte = recode(Meldingsplichtige.ziekte, "Hepatitis C" = "Hepatitis C, niet gespecificeerd"),
         
         # Voeg de HPZone losse dierlijke influenza virussen samen naar 1 categorie
         Meldingsplichtige.ziekte = recode(Meldingsplichtige.ziekte, "Humane infectie met aviair Influenzavirus B" = "Humane infectie met dierlijk influenzavirus"),
         Meldingsplichtige.ziekte = recode(Meldingsplichtige.ziekte, "Nieuwe Influenza A (H1N1)" = "Humane infectie met dierlijk influenzavirus")
         ) %>% 
  
  # Scabies bevat diakrieten (in dit geval e met trema). Diakrieten gebruiken in dit R script geeft problemen, als je het bestand verplaatst of voor het eerst opent, dan kunnen de diakrieten
  # verspringen naar een vraagteken of naar "C+". Vervang diakrieten daarom met hun niet-diakriet tegenhanger.
  mutate(Summary.Diagnosis = gsub(pattern = "Scabi.s", replacement = "Scabies", x = Summary.Diagnosis)) %>% 
  
  # Zet Ziekte naar Scabies (staat anders op NA), hercodeer Hepatitis, voeg Monkeypox en CPE toe
  mutate(Meldingsplichtige.ziekte = case_when(Summary.Diagnosis %in% c("Scabies", "ScabiC+s crustosa", "ScabiC+s, unspecified") ~ "Scabies",
                            
                            Summary.Diagnosis %in% c("Acute hepatitis B") ~ "Hepatitis B, acuut",
                            Summary.Diagnosis %in% c("Chronic hepatitis B") ~ "Hepatitis B, chronisch",
                            Summary.Diagnosis %in% c("Chronic hepatitis B (with D)") ~ "Hepatitis B, chronisch",
                            Summary.Diagnosis %in% c("Hepatitis B, unspecified") ~ "Hepatitis B, niet gespecificeerd",
                            
                            Summary.Diagnosis %in% c("Acute hepatitis C") ~ "Hepatitis C, acuut",
                            Summary.Diagnosis %in% c("Chronic hepatitis C") ~ "Hepatitis C, chronisch",
                            Summary.Diagnosis %in% c("Hepatitis C, unspecified") ~ "Hepatitis C, niet gespecificeerd",
                            
                            Summary.Diagnosis %in% c("Monkeypox") ~ "Monkeypox",
                            
                            ABR %in% c("CPE") ~ "CPE",
                            
                            Infection %in% c("Chikungunya") ~ "Chikungunya",
                            
                            Infection %in% c("Dengue Fever") ~ "Dengue Fever",
                            
                        # Laat van alle andere rijen in Ziekte de oude waarde staan    
                         TRUE ~ Meldingsplichtige.ziekte)) %>% 
  
  # Verwijder de regels waarbij Meldingsplichtige ziekte op dit punt nog leeg is.
  filter(!is.na(Meldingsplichtige.ziekte)) %>% 
  
  # Bewaar alle meldingsplichtige ziekte, indien status is definitief of gefiatteerd. 
  # Behalve bij Rabies en Virale hemorragische koorts, daar mogen Niet meldingsplichtig ook mee 
  # Scabies ook meenemen, staat onder summary diagnosis
  
  # Neem alleen een deel van de cases mee
  filter(Confidence %in% c("Confirmed", "Possible", "Probable"),
         is.na(Datum.gewist), # Datum gewist moet leeg zijn
        Status.van.de.melding %in% c("Definitief", "Gefiatteerd") | 
          (Meldingsplichtige.ziekte %in% c("Rabies (Hondsdolheid)", "Virale hemorragische koorts", "Scabies") & !Status.van.de.melding %in% c("Gewist", "Overgedragen"))
          ) %>% # Status van de melding moet definitief of gefiatteerd zijn, behalve bij Rabies, virale koorts en Scabies
  
  # Maak variabele GGD aan. Zet het formaat dat hieruit ontstaat om naar de gewenste spellingen voor de aparte GGDen. BZO is al zoals gewenst.
  mutate(GGD = sub("/.*", "", Case.Identifier),
         GGD = case_when(GGD %in% "WBT" ~ "WB",
                         GGD %in% "HVB" ~ "HvB",
                         GGD %in% "ZEE" ~ "Zee",
                         GGD %in% "NLG" ~ "LN",
                         GGD %in% "ZLG" ~ "ZL",
                         # Laat van alle andere rijen de oude waarde staan    
                         TRUE ~ GGD)) %>% 
  
  # Maak kolommen met maand en jaar aan
  mutate(Time.entered = dmy(Time.entered), 
         Maand = month(Time.entered),
         Jaar = year(Time.entered),
         Kwartaal = quarter(Time.entered)) %>% 
  
  # Maak variabele aan die aangeeft of een case bewaard moet blijven irt type rapport dat wordt gemaakt
  mutate(bewaar = case_when(Jaar < max(Jaar, na.rm = T) ~ 1,
                          Jaar == max(Jaar, na.rm = T) & Maand < max_maand ~ 1,
                          Jaar == max(Jaar, na.rm = T) & Maand >= max_maand ~ 0,
                          )) %>% 
  
  # Filter daadwerkelijk de data
  filter(bewaar %in% 1) %>% 
  # Gooi tijdelijke kolom weg
  select(-bewaar)


# Geef een foutmelding als het gekozen kwartaal om een rapport op uit te draaien niet voorkomt in de data.
if (type_rapport != 5 & nrow(cases[cases$Jaar == max(cases$Jaar, na.rm = TRUE) & cases$Kwartaal == type_rapport,]) == 0){
  stop("Het kwartaal opgegeven bij 'type_rapport' komt niet voor in het bestand met de situations. Geef een bestand op waarin er wel data uit dat kwartaal aanwezig is.")
}

### Template op basis van ziekten die voorkomen in cases. Template is nodig om waarden van 0 te krijgen bij maanden/jaren waarin een ziekte niet voorkomt.
alle_meldingsplichtige_ziekten <- unique(Link_RIVM_HPZone$Meldingsplichtige.ziekte.HPZone)

# Template om per GGD alle jaren en maanden. Dit om de waarde naar 0 te krijgen voor maanden waarin geen ziekten gemeld zijn. Als je dit niet doet,
# wordt er in de grafiek een waarde geintrapoleerd: een maand krijgt dan de tussenliggende waarde van 2 maanden waar de ziekte wel gemeld is.
template_GGD_Jaar_Maand_Ziekte <- crossing(GGD = c(unique(cases$GGD)),
                                    Jaar = unique(cases$Jaar), 
                                    Maand = c("01", "02", "03", "04", "05", "06", "06", "07", "08", "09", "10", "11", "12"), # Leading 0 is nodig om maanden uit de template te filteren die verder in de toekomst liggen dan waarvan meldingen in de data zitten. Anders stort de lijn in je grafiek aan het einde naar 0. 
                                    Infectieziekte = alle_meldingsplichtige_ziekten) %>% 
  mutate(jaarMaand = paste0(Jaar, "-",  Maand)) %>%
  filter(jaarMaand <= substr(max(cases$Time.entered, na.rm = T), 1, 7)) %>%
  mutate(Maand = as.numeric(Maand))

# Sla het meest recente jaar op in een aparte variabele, om later flexibel te kunnen gebruiken in de tabellen (geen hardcoding van het jaar in 
# de kolomnamen)
Laatste_rapportagejaar <- as.numeric(substr(max(cases$Jaar, na.rm = T), 1, 4))


#########
# Cases per GGD per maand per ziekte

ziekten_per_GGD_per_maandJaar <- cases %>% 
  
  # Groepeer naar jaar, GGD en soort ziekte
  group_by(Jaar, Maand, GGD, Meldingsplichtige.ziekte) %>% 
  
  # Tel op hoeveel cases er zijn per jaar, GGD en ziekte
  summarize(Aantal = n()) %>%
  
  # right_join aan template, om alle meldingsplichtige ziekten voor elke maand in het overzicht te hebben
  right_join(template_GGD_Jaar_Maand_Ziekte, by = c("Meldingsplichtige.ziekte" = "Infectieziekte", "Jaar" = "Jaar", "GGD" = "GGD", "Maand" = "Maand")) %>% 

  # Zet NA naar 0 (ziekte kwam in die maand niet voor)
  replace_na(list("Aantal" = 0))


###############
# Maak afsplitsing om aantallen voor de overkoepelende regio bij te kunnen voegen

ziekten_incl_regio_per_maandJaar <- ziekten_per_GGD_per_maandJaar %>% 
  
  # Tel aantallen op voor de regio
  group_by(Jaar, Maand, jaarMaand, Meldingsplichtige.ziekte) %>% 
  
  summarize(Aantal = sum(Aantal, na.rm = TRUE)) %>% 
  
  # Maak kolom aan met "GGD code"
  mutate(GGD = "Regio") %>% 
  
  # Zet kolommen in goede volgorde voor rbind
  select(Jaar, GGD, Meldingsplichtige.ziekte, Aantal) %>% 
  
  # Voeg samen met data per maand per GGD
  rbind(ziekten_per_GGD_per_maandJaar) %>% 
  
  # Maak koppelcode aan
  mutate(koppelcode = paste0(GGD, "_", Jaar)) %>% 
  
  # Koppel de inwonersaantallen aan dit bestand 
  left_join(inwoners, by = "koppelcode") %>% 
  
  # Bereken incidentie en rond deze af zonder decimalen
  mutate(Incidentie = Aantal / Inwoners * 1000000) %>% 
  
  # Verwijder bepaalde kolommen
  select(Jaar.x, GGD.x, Meldingsplichtige.ziekte, Aantal, Incidentie) %>% 
  
  # Hernoem kolommen
  rename(GGD = GGD.x, Jaar = Jaar.x) %>% 
  
  # Voeg kwartaal toe, en combi van jaar en kwartaal. 
  mutate(Kwartaal = case_when(
                        Maand %in% c(1, 2, 3) ~ "Q1",
                        Maand %in% c(4, 5, 6) ~ "Q2",
                        Maand %in% c(7, 8, 9) ~ "Q3",
                        Maand %in% c(10, 11, 12) ~ "Q4"),
         Jaar_kwartaal = paste0(as.character(Jaar), "_", Kwartaal)) %>% 

  # Als het om een jaarrapport gaat, overschrijf Jaar_kwartaal dan (dit om 1 variabele te hebben waar
  # voor de tabel op gegroepeerd kan worden)

  mutate(Jaar_kwartaal = case_when(
                            type_rapport == 5 ~ as.character(Jaar),
                            type_rapport %in% c(1, 2, 3, 4) ~ Jaar_kwartaal))


##################################
# Cases in formaat voor de tabel #
##################################

#########
# Bereken de landelijke cijfers

# Zet landelijk om naar long format
landelijk_voor_tabel <- landelijk %>% 
  
  # Hernoem kolommen
  rename("Meldingsplichtige.ziekte" = "Infectieziekte") %>% 
  
  # Voeg GGD en jaar toe, om dezelfde kolommen te hebben als bij de aparte GGDen + regio. Als kolommen niet overeenkomen, lukt koppelen niet.
  mutate(GGD = "Nederland", 
         Jaar = Laatste_rapportagejaar) %>% 
  
  # Maak koppelcode aan
  mutate(koppelcode = paste0(GGD, "_", Jaar)) %>% 
  
  # Koppel de inwonersaantallen aan dit bestand 
  left_join(inwoners, by = c("koppelcode")) %>% 
  
  # Bereken incidentie en rond deze af zonder decimalen
  mutate(Incidentie = Aantal / Inwoners * 1000000) %>% 
  
  # Voeg Jaar_kwartaal toe, anders werkt rbind met regionale data later niet meer
  mutate(Jaar_kwartaal = case_when(
                            type_rapport == 5 ~ as.character(Jaar.x),
                            type_rapport %in% c(1, 2, 3, 4) ~ paste0(as.character(Jaar.x), "_Q", type_rapport)
  )) %>% 
  
  # Behoud bepaalde kolommen
  select(GGD.x, Jaar.x, Jaar_kwartaal, Meldingsplichtige.ziekte, Aantal, Incidentie) %>% 
  
  # Hernoem kolommen
  rename(GGD = GGD.x, Jaar = Jaar.x) 
  

##########  
### Zet de data uit de cases import om naar jaarcijfers en giet in het juiste formaat.   
  
ziekten_per_GGD_per_jaar_voor_tabel_alle_jaren <- ziekten_incl_regio_per_maandJaar %>% 

  group_by(GGD, Jaar, Jaar_kwartaal, Meldingsplichtige.ziekte) %>% 
  
  # Tel op hoeveel cases er zijn per jaar, GGD en ziekte
  summarize(Aantal = sum(Aantal, na.rm = TRUE),
            Incidentie = sum(Incidentie, na.rm = TRUE)) %>% 
  
  # Voeg Nederlandse cijfers toe
  rbind(landelijk_voor_tabel) %>% 

  # Doe eerst een pivot longer, om daarna naar het volledig brede formaat uit de tabel te kunnen gaan
  pivot_longer(cols = c(Aantal, Incidentie)) %>% 
  pivot_wider(names_from = c(GGD, name),
              values_from = value) %>% 
  
  # Pas volgorde van de kolommen aan
  select(Jaar, Jaar_kwartaal, Meldingsplichtige.ziekte, 
       BZO_Aantal, HvB_Aantal, LN_Aantal, WB_Aantal, Zee_Aantal, ZL_Aantal, Regio_Aantal, Nederland_Aantal,
       BZO_Incidentie, HvB_Incidentie, LN_Incidentie, WB_Incidentie, Zee_Incidentie, ZL_Incidentie, Regio_Incidentie, Nederland_Incidentie) 


# Binnen het volgende blok code na onderstaande regel wordt een rij aan de tabel toegevoegd met de totalen, mbv de summarise functie.
# Ik weet niet hoe ik de summarise functie parameters mee kan laten nemen van de sum functie, dus daarom deze workaround.
sum_without_na <- function(a){sum(a, na.rm = T)} 

## De tabel bevat gegevens voor alle GGDen + regio  + Nederland van het meest recente jaar en regio van het vorige jaar.

ziekten_per_GGD_per_jaar_voor_tabel <- ziekten_per_GGD_per_jaar_voor_tabel_alle_jaren %>% 
  
  # Ungroup, anders mag je bepaalde berekeningen niet doen
  ungroup() %>%
  
  # Filter naar meest recente jaar 
  filter(Jaar == Laatste_rapportagejaar) %>% # Laatste_rapportagejaar wordt bovenaan script opgegeven
  
  # Filter naar gekozen kwartaal
  {if (type_rapport %in% c(1, 2, 3, 4)) filter(., Jaar_kwartaal == paste0(as.character(Jaar), "_Q", type_rapport)) else .} %>% 
  
  # Voeg van regio het een-na-laatste jaar toe
  cbind(ziekten_per_GGD_per_jaar_voor_tabel_alle_jaren %>% 
          filter(Jaar == Laatste_rapportagejaar - 1) %>% 
          {if (type_rapport %in% c(1, 2, 3, 4)) filter(., Jaar_kwartaal == paste0(as.character(Jaar), "_Q", type_rapport)) else .} %>% 
          pull(Regio_Incidentie)) %>% 
  
  # Hernoem laatste kolom, op basis van kolomnummer, omdat deze kolom toegevoegd werd dmv de
  # cbind, en R daarom zelf een naam toewijst. 
  rename("Regio_Incidentie_VorigJaar" = ncol(.)) %>% 
  
  #Pieter: Scabies & Rabies & TBC wegfilteren voordat eindtotaal wordt berekend
  dplyr::filter(!Meldingsplichtige.ziekte %in% c("Scabies","Rabies (Hondsdolheid)","Tuberculose")) %>%
  
  # Voeg Eindtotaal toe
  add_row(Meldingsplichtige.ziekte = "Eindtotaal", summarise(., across(where(is.numeric), sum_without_na))) %>% 
  
  # Rond getallen af op 0 decimalen
  mutate(across(where(is.numeric), round2, 0)) %>% 
  

  # Haal jaarkolom weg
  select(-Jaar, -Jaar_kwartaal)


# Bereken de start en eind indices op basis van hoeveel unieke GGDen er in de dataset voorkomen
eind1 <- 2 + length(unique(cases$GGD))
start2 <- 2 + eind1
eind2 <- start2 + length(unique(cases$GGD)) + 2

# Zet 0 naar missing (= NA) bij de Aantal kolommen
ziekten_per_GGD_per_jaar_voor_tabel[2:eind1][ziekten_per_GGD_per_jaar_voor_tabel[2:eind1] == 0] <- NA

#Pieter: Op verzoek Elke; NL incidentie moet wel de 0 laten zien.
index_nl_incidentie <- which(names(ziekten_per_GGD_per_jaar_voor_tabel) == "Nederland_Incidentie")
kolommen_naar_NA <- c(start2:eind2)[c(start2:eind2) != index_nl_incidentie]

ziekten_per_GGD_per_jaar_voor_tabel[kolommen_naar_NA][ziekten_per_GGD_per_jaar_voor_tabel[kolommen_naar_NA] == 0] <- NA

# Bereken hoeveel kolommen de tekst "Absolute aantallen" en "Incidentie per 1 miljoen inwoners" moeten omspannen in de bovenste rij van de tabel
aantal_kolom_absoluteAantallen_cases <- length(unique(cases$GGD)) + 2 # Alle GGDen + Regio en NL
aantal_kolom_incidentie_cases <- length(unique(cases$GGD)) + 3 # Alle GGDen + Regio, NL en Regio vorig jaar

# De headernaam voor de tabel is iets anders in het jaarrapport ("Regio 20xx") dan in het kwartaalrapport ("Regio 20xx Q3")
# Maak hier de juiste naam aan
header_kolomnaam <- case_when(
  type_rapport == 5 ~ paste0(Regionaam, " ", Laatste_rapportagejaar - 1),
  type_rapport %in% c(1, 2, 3, 4) ~ paste0(Regionaam, " ", Laatste_rapportagejaar - 1, " Q", as.character(type_rapport)))

### Maak tabel
tabel_meldplichtZiekten <- ziekten_per_GGD_per_jaar_voor_tabel %>% 
  
  # #NL incidentie kolom naar character zodat er 0en te zien zijn
  # mutate(Nederland_Incidentie = as.character(Nederland_Incidentie)) %>%
  # # #De rest; 0 naar NA
  #  mutate_if(is.numeric,~na_if(.,0)) %>%
  
  flextable() %>% 
  
  # Font style en font size
  font(fontname = "Calibri", part = "all") %>% 
  fontsize(size = 11, part = "all") %>% 
  
  # Maak kolomlabels dikgedrukt
  bold(bold = TRUE, part = "header") %>% 
  
  # Pas namen van header aan.
  set_header_labels(Meldingsplichtige.ziekte = "Ziekte" ,
                    BZO_Aantal = "BZO",
                    HvB_Aantal = "HvB",
                    LN_Aantal = "LN",
                    WB_Aantal = "WB", 
                    Zee_Aantal = "Zee", 
                    ZL_Aantal = "ZL", 
                    Regio_Aantal = paste0(Regionaam, " totaal"), 
                    Nederland_Aantal = "NL totaal",
                    BZO_Incidentie = "BZO", 
                    HvB_Incidentie = "HvB", 
                    LN_Incidentie = "LN", 
                    WB_Incidentie = "WB", 
                    Zee_Incidentie = "Zee", 
                    ZL_Incidentie = "ZL", 
                    Regio_Incidentie = Regionaam, 
                    Nederland_Incidentie = "NL totaal" ,
                    Regio_Incidentie_VorigJaar = header_kolomnaam) %>% 
  
  # Pas breedte van kolommen aan
  width(j = 1, width = 4, unit = "cm") %>% # Kolom "Ziekte"
  width(j = c(2, 3, 10, 11), width = 1.1, unit = "cm") %>% # BZO en HvB
  width(j = c(4:7, 12:15), width = 1, unit = "cm") %>% # Kolommen met GGD namen
  width(j = c(8, 16, 18), width = 1.6, unit = "cm") %>% # Regio totaal
  width(j = c(9, 17), width = 1.4, unit = "cm") %>% # "NL Totaal"
  
  
  # Voeg extra header toe
  add_header_row(values = c("", "Absolute aantallen", "Incidentie per 1 miljoen inwoners"), colwidths = c(1, aantal_kolom_absoluteAantallen_cases, aantal_kolom_incidentie_cases), top = TRUE) %>% 
  
  # Centreer de header namen. Zet daarna alleen kolom 1 terug naar align = left
  align(align = "center", part = "header") %>%
  align(align = "left", part = "header", j = 1) %>%

  # Maak kolom Regio totaal en Regio (inc) lichtgrijs
  bg(j = which(colnames(ziekten_per_GGD_per_jaar_voor_tabel) %in% c("Regio_Aantal", "Regio_Incidentie")), # Flexibele opzoeking wat de kolomnummers zijn van de gespecificeerde kolommen. Je kunt ook c(8, 16) invoeren.
     bg = "#ECECEC", part = "all") %>% 
  
  # Maak kolom NL totaal en NL totaal (inc) donkergrijs
  bg(j = which(colnames(ziekten_per_GGD_per_jaar_voor_tabel) %in% c("Nederland_Aantal", "Nederland_Incidentie")), 
     bg = "#D4D4D4", part = "all") %>% 
  
  # Maak Regio vorig jaar lichtblauw
  bg(j = which(colnames(ziekten_per_GGD_per_jaar_voor_tabel) %in% c("Regio_Incidentie_VorigJaar")), 
     bg = "#dbe5f1", part = "all") %>% 
  
  # Toon cell borders
  border_outer(part = "all", border = fp_border(width = 1, color = "#000000")) %>% 
  border_inner(part = "all", border = fp_border(width = 1, color = "#000000")) %>% 
  #Eindtotaal dikgedrukt
  bold(i = nrow(ziekten_per_GGD_per_jaar_voor_tabel))




############## 
# Situations #
##############

situations <- lees_bestand(pad_naar_situations_bestand) %>%
  
  # Input data heeft 2x de kolom "Infectious Agent", waardoor readr er een getal achter. Dit getal is het kolomnummer, en kan dus mogelijk verschillen als de
  # export ietsjes anders wordt gedaan (andere kolomselectie). Vind de eerste kolom met een zoekfunctie en zet haal daar het getal af.
  rename("Infectious.Agent" = which(startsWith(colnames(.), "Infectious.Agent"))[1]) %>% 
  #filter(!Infectious.Agent %in% c("Coronavirus", "COVID-19")) %>%
  # Alleen gevallen waarbij waarde in kolom "Artikel 26" gelijk is aan "Yes"
  filter(Artikel.26 %in% c("Yes")) %>% 
  # Maak kolommen met maand en jaar aan
  mutate(Date.entered = dmy(Date.entered),
         Maand = month(Date.entered),
         Jaar = year(Date.entered),
         Kwartaal = quarter(Date.entered)) %>% 
  
  # Maak variabele aan die aangeeft of een case bewaard moet blijven irt type rapport dat wordt gemaakt. max_maand wordt bovenaan het script aangemaakt obv type_rapport
  mutate(bewaar = case_when(Jaar < max(Jaar, na.rm = T) ~ 1,
                            Jaar == max(Jaar, na.rm = T) & Maand < max_maand ~ 1,
                            Jaar == max(Jaar, na.rm = T) & Maand >= max_maand ~ 0,
  )) %>% 
  
  # Filter daadwerkelijk de data
  filter(bewaar %in% 1) %>% 
  
  # Gooi tijdelijke kolom weg
  select(-bewaar) %>% 
  
  # Maak variabele GGD aan
  mutate(GGD = sub("/.*", "", Name),
         GGD = case_when(GGD %in% "WBT" ~ "WB",
                         GGD %in% "HVB" ~ "HvB",
                         GGD %in% "ZEE" ~ "Zee",
                         GGD %in% "NLG" ~ "LN",
                         GGD %in% "ZLG" ~ "ZL",
                         # Laat van alle andere rijen de oude waarde staan    
                         TRUE ~ GGD)) %>%
  
  # Zet ontbrekende waarden bij scenario om naar "(leeg)" ipv missing
  replace_na(list("Scenario" = "(leeg)",
                  "Principal.Contextual.Setting" = "(leeg)")) 

# Geef een foutmelding als het gekozen kwartaal om een rapport op uit te draaien niet voorkomt in de data.
if (type_rapport != 5 & nrow(situations[situations$Jaar == max(situations$Jaar, na.rm = TRUE) & situations$Kwartaal == type_rapport,]) == 0){
  stop("Het kwartaal opgegeven bij 'type_rapport' komt niet voor in het bestand met de situations. Geef een bestand op waarin er wel data uit dat kwartaal aanwezig is.")
}

# Voor de situations wordt de data zowel weergegeven op basis van kolom "Scenario", als ook uitsplitsing via "Infectious Agent". Maak eerst een lijst
# van welke combinaties van Scenario en Infectious Agent voorkomen. Gebruik deze lijst voor de template. Splits dan later de boel weer af.
combi_Scenario_InfAgent <- unique(paste0(situations$Scenario," - ", situations$Infectious.Agent))


# Template om per GGD alle jaren en maanden. Dit om de waarde naar 0 te krijgen voor maanden waarin geen ziekten gemeld zijn. Als je dit niet doet,
# wordt er in de grafiek een waarde geintrapoleerd: een maand krijgt dan de tussenliggende waarde van 2 maanden waar de ziekte wel gemeld is.
template_GGD_Jaar_Maand_Scenario_InfAgent <- crossing(GGD = c(unique(situations$GGD)),
                                           Jaar = unique(situations$Jaar), 
                                           Maand = c("01", "02", "03", "04", "05", "06", "06", "07", "08", "09", "10", "11", "12"), # Leading 0 is nodig om maanden uit de template te filteren die verder in de toekomst liggen dan waarvan meldingen in de data zitten. Anders stort de lijn in je grafiek aan het einde naar 0. 
                                           Scenario_InfAgent = combi_Scenario_InfAgent) %>% 
  mutate(jaarMaand = paste0(Jaar, "-",  Maand)) %>%
  filter(jaarMaand <= substr(max(situations$Date.entered), 1, 7)) %>%
  mutate(Maand = as.numeric(Maand))




#########
# Situations per GGD per maand en jaar per ziekte

scenario_Agent_per_GGD_per_maandJaar <- situations %>% 
  
  # Maak koppelvariabele aan om te kunnen koppelen aan de template
  mutate(Scenario_InfAgent = paste0(situations$Scenario," - ", situations$Infectious.Agent)) %>% 
  
  # Groepeer naar jaar, maand, GGD en scenario
  group_by(Jaar, Maand, GGD, Scenario_InfAgent) %>% 
  
  # Tel op hoeveel situations er zijn per jaar, maand, GGD en scenario
  summarize(Aantal = n(), .groups = 'drop') %>% 
  
  # right_join aan template, om ervoor te zorgen dat maanden zonder aantal een waarde van 0 krijgen ipv interpolatie
  right_join(template_GGD_Jaar_Maand_Scenario_InfAgent, by = c("Scenario_InfAgent", "Jaar", "GGD", "Maand")) %>% 
  
  # Zet NA naar 0 (scenario kwam in die maand niet voor)
  replace_na(list("Aantal" = 0)) 


###############
# Maak afsplitsing om aantallen voor de regio bij te kunnen voegen

scenario_Agent_incl_regio_per_maandJaar <- scenario_Agent_per_GGD_per_maandJaar %>% 
  
  # Tel aantallen op voor de regio
  group_by(Jaar, Maand, jaarMaand, Scenario_InfAgent) %>% 
  
  summarize(Aantal = sum(Aantal, na.rm = TRUE)) %>% 
  
  # Maak kolom aan met "GGD code"
  mutate(GGD = "Regio") %>% 
  
  # Zet kolommen in goede volgorde voor rbind
  select(Jaar, GGD, Scenario_InfAgent, Aantal) %>% 
  
  # Voeg samen met data per maand per GGD
  rbind(scenario_Agent_per_GGD_per_maandJaar) %>% 
  
  # Maak koppelcode aan
  mutate(koppelcode = paste0(GGD, "_", Jaar)) %>% 
  
  # Koppel de inwonersaantallen aan dit bestand 
  left_join(inwoners, by = "koppelcode") %>% 
  
  # Bereken incidentie en rond deze af zonder decimalen
  mutate(Incidentie = Aantal / Inwoners * 1000000) %>% 
  
  # Verwijder bepaalde kolommen
  select(Jaar.x, GGD.x, Scenario_InfAgent, Aantal, Incidentie) %>% 
  
  # Hernoem kolommen
  rename(GGD = GGD.x, Jaar = Jaar.x) %>% 
  
  # Splits Scenario en Agent weer van elkaar af
  separate(col = Scenario_InfAgent, into = c("Scenario", "Infectious.Agent"), sep = " - ") 

###############
# Aggregeer van Scenario - Infectious Agent naar alleen Scenario. Nodig voor de grafieken, wordt geen tabel van gemaakt.
scenario_incl_regio_per_maandJaar <- scenario_Agent_incl_regio_per_maandJaar %>% 
  filter(!Infectious.Agent %in% c("Coronavirus", "COVID-19")) %>%
  
  group_by(GGD, Jaar, Maand, jaarMaand, Scenario) %>% 
  
  summarize(Aantal = sum(Aantal, na.rm = TRUE),
            Incidentie = sum(Incidentie, na.rm = TRUE))
  

##########  
### Zet de data betreffende situations om naar jaarcijfers en giet in het juiste formaat. 

scenario_per_GGD_per_jaar_voor_tabel_alle_jaren <- scenario_Agent_incl_regio_per_maandJaar %>% 
  
  # Voeg kwartaal toe, en combi van jaar en kwartaal. 
  mutate(Kwartaal = case_when(
    Maand %in% c(1, 2, 3) ~ "Q1",
    Maand %in% c(4, 5, 6) ~ "Q2",
    Maand %in% c(7, 8, 9) ~ "Q3",
    Maand %in% c(10, 11, 12) ~ "Q4"),
    Jaar_kwartaal = paste0(as.character(Jaar), "_", Kwartaal)) %>% 
  
  # Als het om een jaarrapport gaat, overschrijf Jaar_kwartaal dan (dit om 1 variabele te hebben waar
  # voor de tabel op gegroepeerd kan worden)
  
  mutate(Jaar_kwartaal = case_when(
    type_rapport == 5 ~ as.character(Jaar),
    type_rapport %in% c(1, 2, 3, 4) ~ Jaar_kwartaal)) %>% 
  
  group_by(GGD, Jaar, Jaar_kwartaal, Scenario) %>% 
  
  # Tel op hoeveel cases er zijn per jaar, GGD en ziekte
  summarize(Aantal = sum(Aantal, na.rm = TRUE),
            Incidentie = sum(Incidentie, na.rm = TRUE)) %>% 
  
  # Doe eerst een pivot longer, om daarna naar het volledig brede formaat uit de tabel te kunnen gaan
  pivot_longer(cols = c(Aantal, Incidentie)) %>% 
  pivot_wider(names_from = c(GGD, name),
              values_from = value) %>% 
  
  # Pas volgorde van de kolommen aan
  select(Jaar, Jaar_kwartaal, Scenario, 
         BZO_Aantal, HvB_Aantal, LN_Aantal, WB_Aantal, Zee_Aantal, ZL_Aantal, Regio_Aantal,
         BZO_Incidentie, HvB_Incidentie, LN_Incidentie, WB_Incidentie, Zee_Incidentie, ZL_Incidentie, Regio_Incidentie) 



## Geef foutmelding als de minimale datum van situations hoger ligt dan Laatst_rapportagejaar (laatste wordt bepaald op basis van cases data)
if(min(situations$Jaar, na.rm = TRUE) > Laatste_rapportagejaar){
  stop("De data in het situations bestand bevat enkel data van na het meest recente jaar uit het cases bestand. Gebruik een bestand dat over dezelfde tijdsperiode gaat.")
}


## De tabel bevat gegevens voor alle GGDen + regio van het meest recente jaar en regio van het vorige jaar.

scenario_per_GGD_per_jaar_voor_tabel <- scenario_per_GGD_per_jaar_voor_tabel_alle_jaren %>% 
  
  # Ungroup, anders mag je bepaalde berekeningen niet doen
  ungroup() %>%
  
  # Filter naar meest recente jaar 
  filter(Jaar == Laatste_rapportagejaar) %>% # Laatste_rapportagejaar wordt bovenaan script opgegeven
  
  # Filter naar gekozen kwartaal
  {if (type_rapport %in% c(1, 2, 3, 4)) filter(., Jaar_kwartaal == paste0(as.character(Jaar), "_Q", type_rapport)) else .} %>% 
  
  # Voeg van de regio het een-na-laatste jaar toe
  cbind(scenario_per_GGD_per_jaar_voor_tabel_alle_jaren %>% 
          filter(Jaar == Laatste_rapportagejaar - 1) %>% 
          {if (type_rapport %in% c(1, 2, 3, 4)) filter(., Jaar_kwartaal == paste0(as.character(Jaar), "_Q", type_rapport)) else .} %>% 
          pull(Regio_Incidentie)) %>% 
  
  # Voeg Eindtotaal toe
  add_row(Scenario = "Eindtotaal", summarise(., across(where(is.numeric), sum))) %>% 
  
  # Rond getallen af op 0 decimalen
  mutate(across(where(is.numeric), round2, 0)) %>%
  
  # Hernoem laatste kolom, op basis van kolomnummer, omdat deze kolom toegevoegd werd dmv de
  # cbind, en R daarom zelf een naam toewijst. 
  rename("Regio_Incidentie_VorigJaar" = ncol(.)) %>% 
  
  # Haal jaarkolom weg
  select(-Jaar, -Jaar_kwartaal) 


# Bereken de start en eind indices op basis van hoeveel unieke GGDen er in de dataset voorkomen
eind1 <- 2 + length(unique(situations$GGD)) - 1
start2 <- eind1 + 2
eind2 <- start2 + length(unique(situations$GGD)) - 1

# Zet 0 naar missing (= NA) bij de Aantal kolommen
scenario_per_GGD_per_jaar_voor_tabel[2:eind1][scenario_per_GGD_per_jaar_voor_tabel[2:eind1] == 0] <- NA
scenario_per_GGD_per_jaar_voor_tabel[start2:eind2][scenario_per_GGD_per_jaar_voor_tabel[start2:eind2] == 0] <- NA

# Bereken hoeveel kolommen de tekst "Absolute aantallen" en "Incidentie per 1 miljoen inwoners" moeten omspannen in de bovenste rij van de tabel
aantal_kolom_absoluteAantallen_situations <- length(unique(situations$GGD)) + 1 # Alle GGDen + Regio
aantal_kolom_incidentie_situations <- length(unique(situations$GGD)) + 2 # Alle GGDen + Regio, en Regio vorig jaar


### Maak tabel
tabel_scenario <- scenario_per_GGD_per_jaar_voor_tabel %>% 

  flextable() %>% 
  
  # Font style en font size
  font(fontname = "Calibri", part = "all") %>% 
  fontsize(size = 11, part = "all") %>% 
  
  # Maak kolomlabels dikgedrukt
  bold(bold = TRUE, part = "header") %>% 

  # Pas namen van header aan.
  set_header_labels(Scenario = "Scenario" ,
                    BZO_Aantal = "BZO",
                    HvB_Aantal = "HvB",
                    LN_Aantal = "LN",
                    WB_Aantal = "WB", 
                    Zee_Aantal = "Zee", 
                    ZL_Aantal = "ZL", 
                    Regio_Aantal = paste0(Regionaam, " totaal"), 
                    BZO_Incidentie = "BZO", 
                    HvB_Incidentie = "HvB", 
                    LN_Incidentie = "LN", 
                    WB_Incidentie = "WB", 
                    Zee_Incidentie = "Zee", 
                    ZL_Incidentie = "ZL", 
                    Regio_Incidentie = Regionaam, 
                    Regio_Incidentie_VorigJaar = header_kolomnaam) %>% # header_kolomnaam wordt aangemaakt bij de omzetting van cases naar hun tabel

  # Pas breedte van kolommen aan
  width(j = 1, width = 4.5, unit = "cm") %>% 
  width(j = c(2, 3, 9, 10), width = 1.1, unit = "cm") %>% # BZO en HvB
  width(j = c(4:7, 11:14), width = 1, unit = "cm") %>% # Overige GGDen
  width(j = c(8, 15, 16), width = 1.6, unit = "cm") %>% # Regio totaal
  
  # Voeg extra header toe
  add_header_row(values = c("", "Absolute aantallen", "Incidentie per 1 miljoen inwoners"), colwidths = c(1, aantal_kolom_absoluteAantallen_situations, aantal_kolom_incidentie_situations), top = TRUE) %>% 
  
  # Centreer de header namen. Zet daarna alleen kolom 1 terug naar align = left
  align(align = "center", part = "header") %>%
  align(align = "left", part = "header", j = 1) %>%
  
  # Maak kolom regio totaal en regio (inc) lichtgrijs
  bg(j = which(colnames(scenario_per_GGD_per_jaar_voor_tabel) %in% c("Regio_Aantal", "Regio_Incidentie")), # Flexibele opzoeking wat de kolomnummers zijn van de gespecificeerde kolommen. Je kunt ook c(8, 16) invoeren.
     bg = "#ECECEC", part = "all") %>% 
  
  # Maak regio vorig jaar lichtblauw
  bg(j = which(colnames(scenario_per_GGD_per_jaar_voor_tabel) %in% c("Regio_Incidentie_VorigJaar")), 
     bg = "#dbe5f1", part = "all") %>% 
  
  # Toon cell borders
  border_outer(part = "all", border = fp_border(width = 1, color = "#000000")) %>% 
  border_inner(part = "all", border = fp_border(width = 1, color = "#000000")) %>%
  #Eindtotaal dikgedrukt
  bold(i = nrow(scenario_per_GGD_per_jaar_voor_tabel))
  
  
# Principal Contextual Setting #
################################

# Template om per GGD alle jaren en maanden. Dit om de waarde naar 0 te krijgen voor maanden waarin geen ziekten gemeld zijn. Als je dit niet doet,
# wordt er in de grafiek een waarde geintrapoleerd: een maand krijgt dan de tussenliggende waarde van 2 maanden waar de ziekte wel gemeld is.
template_GGD_Jaar_Maand_PrinContext <- crossing(GGD = c(unique(situations$GGD)),
                                           Jaar = unique(situations$Jaar), 
                                           Maand = c("01", "02", "03", "04", "05", "06", "06", "07", "08", "09", "10", "11", "12"), # Leading 0 is nodig om maanden uit de template te filteren die verder in de toekomst liggen dan waarvan meldingen in de data zitten. Anders stort de lijn in je grafiek aan het einde naar 0. 
                                           PrincipalContext = unique(situations$Principal.Contextual.Setting)) %>% 
  mutate(jaarMaand = paste0(Jaar, "-",  Maand)) %>%
  filter(jaarMaand <= substr(max(situations$Date.entered), 1, 7)) %>%
  mutate(Maand = as.numeric(Maand))

#########
# Cases per GGD per maand per ziekte

PrincipalContext_per_GGD_per_maandJaar <- situations %>% 
  
  # Groepeer naar jaar, GGD en soort ziekte
  group_by(Jaar, Maand, GGD, Principal.Contextual.Setting) %>% 
  
  # Tel op hoeveel cases er zijn per jaar, GGD en ziekte
  summarize(Aantal = n(), .groups = 'drop') %>% 
  
  # right_join aan template, om alle meldingsplichtige ziekten voor elke maand in het overzicht te hebben
  right_join(template_GGD_Jaar_Maand_PrinContext, by = c("Principal.Contextual.Setting" = "PrincipalContext", "Jaar" = "Jaar", "GGD" = "GGD", "Maand" = "Maand")) %>% 
  
  # Zet NA naar 0 (ziekte kwam in die maand niet voor)
  replace_na(list("Aantal" = 0))


###############
# Maak afsplitsing om aantallen voor regio bij te kunnen voegen

PrincipalContext_incl_regio_per_maandJaar <- PrincipalContext_per_GGD_per_maandJaar %>% 
  
  # Tel aantallen op voor de regio
  group_by(Jaar, Maand, jaarMaand, Principal.Contextual.Setting) %>% 
  
  summarize(Aantal = sum(Aantal, na.rm = TRUE)) %>% 
  
  # Maak kolom aan met "GGD code"
  mutate(GGD = "Regio") %>% 
  
  # Zet kolommen in goede volgorde voor rbind
  select(Jaar, GGD, Principal.Contextual.Setting, Aantal) %>% 
  
  # Voeg samen met data per maand per GGD
  rbind(PrincipalContext_per_GGD_per_maandJaar) %>% 
  
  # Maak koppelcode aan
  mutate(koppelcode = paste0(GGD, "_", Jaar)) %>% 
  
  # Koppel de inwonersaantallen aan dit bestand 
  left_join(inwoners, by = "koppelcode") %>% 
  
  # Bereken incidentie en rond deze af zonder decimalen
  mutate(Incidentie = Aantal / Inwoners * 1000000) %>% 
  
  # Behoud bepaalde kolommen
  select(Jaar.x, GGD.x, Principal.Contextual.Setting, Aantal, Incidentie) %>% 
  
  # Voeg Jaar_kwartaal toe
  
  #Pieter: Volgens mij gaat dit niet goed. Nu wordt een kwartaal Q{n} toegevoegd
  #aan alle data obv het type rapport. Hierdoor valt latere filtering verkeerd.
  #Als type_rapport = 2, worden ook observaties uit Q1 met Q2 gelabeld.
  
  # mutate(Jaar_kwartaal = case_when(
  #                             type_rapport == 5 ~ as.character(Jaar.x),
  #                             type_rapport %in% c(1, 2, 3, 4) ~ paste0(as.character(Jaar.x), "_Q", type_rapport)
  #                           )) %>%
  
#Aangepast:
  mutate(Jaar_kwartaal = case_when(
                              type_rapport == 5 ~ as.character(Jaar.x),
                              type_rapport %in% c(1, 2, 3, 4) ~ paste0(as.character(Jaar.x), "_Q", quarter(ym(jaarMaand)))
                            )) %>%

  # Behoud bepaalde kolommen
  select(GGD.x, Jaar.x, Jaar_kwartaal, Principal.Contextual.Setting, Aantal, Incidentie) %>% 
  
  # Hernoem kolommen
  rename(GGD = GGD.x, Jaar = Jaar.x)
  
  


##########  
### Zet de data betreffende Principal Contextual Settings om naar jaarcijfers en giet in het juiste formaat. 

PrincipalContext_per_GGD_per_jaar_voor_tabel_alle_jaren <- PrincipalContext_incl_regio_per_maandJaar %>% 
  
  group_by(GGD, Jaar, Jaar_kwartaal, Principal.Contextual.Setting) %>% 
  
  # Tel op hoeveel cases er zijn per jaar, GGD en ziekte
  summarize(Aantal = sum(Aantal, na.rm = TRUE),
            Incidentie = sum(Incidentie, na.rm = TRUE)) %>% 
  
  # Doe eerst een pivot longer, om daarna naar het volledig brede formaat uit de tabel te kunnen gaan
  pivot_longer(cols = c(Aantal, Incidentie)) %>% 
  pivot_wider(names_from = c(GGD, name),
              values_from = value) %>% 
  
  # Pas volgorde van de kolommen aan
  select(Jaar, Jaar_kwartaal, Principal.Contextual.Setting, 
         BZO_Aantal, HvB_Aantal, LN_Aantal, WB_Aantal, Zee_Aantal, ZL_Aantal, Regio_Aantal,
         BZO_Incidentie, HvB_Incidentie, LN_Incidentie, WB_Incidentie, Zee_Incidentie, ZL_Incidentie, Regio_Incidentie) 


## De tabel bevat gegevens voor alle GGDen + regio van het meest recente jaar en regio van het vorige jaar.

PrincipalContext_per_GGD_per_jaar_voor_tabel <- PrincipalContext_per_GGD_per_jaar_voor_tabel_alle_jaren %>% 
  
  # Ungroup, anders mag je bepaalde berekeningen niet doen
  ungroup() %>%
  
  # Filter naar 1 jaar 
  filter(Jaar == Laatste_rapportagejaar) %>% 
  
  # Filter naar gekozen kwartaal
  {if (type_rapport %in% c(1, 2, 3, 4)) filter(., Jaar_kwartaal == paste0(as.character(Jaar), "_Q", type_rapport)) else .} %>% 
  
  # Voeg van de regio het een-na-laatste jaar toe
  cbind(PrincipalContext_per_GGD_per_jaar_voor_tabel_alle_jaren %>% 
          filter(Jaar == Laatste_rapportagejaar - 1) %>% 
          {if (type_rapport %in% c(1, 2, 3, 4)) filter(., Jaar_kwartaal == paste0(as.character(Jaar), "_Q", type_rapport)) else .} %>% 
          pull(Regio_Incidentie)) %>% 
  
  
  
  # Voeg Eindtotaal toe
  add_row(Principal.Contextual.Setting = "Eindtotaal", summarise(., across(where(is.numeric), sum))) %>% 
  
  # Rond getallen af op 0 decimalen
  mutate(across(where(is.numeric), round2, 0)) %>% 
  
  # Hernoem kolommen. Laatste kolom niet op basis van naam gedaan, maar op kolomnummer, omdat deze kolom toegevoegd werd dmv de
  # cbind, en R daarom zelf een naam toewijst. Ik hardcode deze naam liever niet.
  rename("Regio_Incidentie_VorigJaar" = ncol(.)) %>% 
  
  # Haal jaarkolom weg
  select(-Jaar, -Jaar_kwartaal) 


# Bereken de start en eind indices op basis van hoeveel unieke GGDen er in de dataset voorkomen
eind1 <- 2 + length(unique(situations$GGD)) - 1
start2 <- eind1 + 2
eind2 <- start2 + length(unique(situations$GGD)) - 1

# Zet 0 naar missing (= NA) bij de Aantal kolommen
PrincipalContext_per_GGD_per_jaar_voor_tabel[2:eind1][PrincipalContext_per_GGD_per_jaar_voor_tabel[2:eind1] == 0] <- NA
PrincipalContext_per_GGD_per_jaar_voor_tabel[start2:eind2][PrincipalContext_per_GGD_per_jaar_voor_tabel[start2:eind2] == 0] <- NA


### Maak tabel
tabel_PrincipalContext <- PrincipalContext_per_GGD_per_jaar_voor_tabel %>%
  flextable() %>% 
  
  # Font style en font size
  font(fontname = "Calibri", part = "all") %>% 
  fontsize(size = 11, part = "all") %>% 
  
  # Maak kolomlabels dikgedrukt
  bold(bold = TRUE, part = "header") %>% 
  
  # Pas namen van header aan.
  set_header_labels(Principal.Contextual.Setting = "Principal Contextual Setting" ,
                    BZO_Aantal = "BZO",
                    HvB_Aantal = "HvB",
                    LN_Aantal = "LN",
                    WB_Aantal = "WB", 
                    Zee_Aantal = "Zee", 
                    ZL_Aantal = "ZL", 
                    Regio_Aantal = paste0(Regionaam, " totaal"), 
                    BZO_Incidentie = "BZO", 
                    HvB_Incidentie = "HvB", 
                    LN_Incidentie = "LN", 
                    WB_Incidentie = "WB", 
                    Zee_Incidentie = "Zee", 
                    ZL_Incidentie = "ZL", 
                    Regio_Incidentie = Regionaam, 
                    Regio_Incidentie_VorigJaar = header_kolomnaam) %>% # header_kolomnaam wordt aangemaakt bij de omzetting van cases naar hun tabel
  
  # Pas breedte van kolommen aan
  width(j = 1, width = 4.5, unit = "cm") %>% 
  width(j = c(2, 3, 9, 10), width = 1.1, unit = "cm") %>% # BZO en HvB
  width(j = c(4:7, 11:14), width = 1, unit = "cm") %>% # Overige GGDen
  width(j = c(8, 15, 16), width = 1.6, unit = "cm") %>% # Regio totaal
  
  # Voeg extra header toe
  add_header_row(values = c("", "Absolute aantallen", "Incidentie per 1 miljoen inwoners"), colwidths = c(1, aantal_kolom_absoluteAantallen_situations, aantal_kolom_incidentie_situations), top = TRUE) %>% 
  
  
  # Centreer de header namen. Zet daarna alleen kolom 1 terug naar align = left
  align(align = "center", part = "header") %>%
  align(align = "left", part = "header", j = 1) %>%
  
  # Maak kolom regio totaal en regio (inc) lichtgrijs
  bg(j = which(colnames(PrincipalContext_per_GGD_per_jaar_voor_tabel) %in% c("Regio_Aantal", "Regio_Incidentie")), # Flexibele opzoeking wat de kolomnummers zijn van de gespecificeerde kolommen. Je kunt ook c(8, 16) invoeren.
     bg = "#ECECEC", part = "all") %>% 
  
  # Maak regio vorig jaar lichtblauw
  bg(j = which(colnames(PrincipalContext_per_GGD_per_jaar_voor_tabel) %in% c("Regio_Incidentie_VorigJaar")), 
     bg = "#dbe5f1", part = "all") %>% 
  
  # Toon cell borders
  border_outer(part = "all", border = fp_border(width = 1, color = "#000000")) %>% 
  border_inner(part = "all", border = fp_border(width = 1, color = "#000000")) %>%
  #Eindtotaal dikgedrukt
  bold(i = nrow(PrincipalContext_per_GGD_per_jaar_voor_tabel))


################
# Telefoontjes #
################

# Per GGD, jaar, maand
# broad topic = infectieziekten


telefoon <- lees_bestand(pad_naar_telefoonbestand) %>%
  
  # Filter: Broad.Topic alleen "Infectieziekten", geen missende waarden bij Received.on, geen covid-19.
  filter(Broad.Topic %in% "Infectieziekten",
         !is.na(Received.on), # Received.on mag niet leeg zijn
         
         !Specific.Topic %in% c("COVID-19", "Wuhan Novel Coronavirus")) %>%  # Specific Topic moet niet gelijk zijn aan "COVID-19"
        
  
  mutate(Received.on = as.Date(Received.on, format = "%d/%m/%Y"),
         Jaar = year(Received.on),
         Maand = month(Received.on),
         Kwartaal = quarter(Received.on),
         GGD = case_when(
           GGD %in% "Brabant-Zuidoost" ~ "BZO",
           GGD %in% "Hart voor Brabant" ~ "HvB",
           GGD %in% "West-Brabant" ~ "WB",
           GGD %in% "Zeeland" ~ "Zee",
           GGD %in% "Noord Limburg" ~ "LN",
           GGD %in% "Zuid Limburg" ~ "ZL")) %>% 
  
  # Maak variabele aan die aangeeft of een entry bewaard moet blijven irt type rapport dat wordt gemaakt
  mutate(bewaar = case_when(Jaar < max(Jaar, na.rm = T) ~ 1,
                            Jaar == max(Jaar, na.rm = T) & Maand < max_maand ~ 1,
                            Jaar == max(Jaar, na.rm = T) & Maand >= max_maand ~ 0,
  )) %>% 
  
  # Filter daadwerkelijk de data
  filter(bewaar %in% 1) %>% 
  
  # Gooi tijdelijke kolom weg
  select(-bewaar)

# Geef een foutmelding als het gekozen kwartaal om een rapport op uit te draaien niet voorkomt in de data.
if (type_rapport != 5 & nrow(telefoon[telefoon$Jaar == max(telefoon$Jaar, na.rm = TRUE) & telefoon$Kwartaal == type_rapport,]) == 0){
  stop("Het kwartaal opgegeven bij 'type_rapport' komt niet voor in het bestand met de telefoontjes. Geef een bestand op waarin er wel data uit dat kwartaal aanwezig is.")
}

## Geef foutmelding als de minimale datum van enquiries hoger ligt dan Laatst_rapportagejaar (laatste wordt bepaald op basis van cases data)
if(min(telefoon$Jaar, na.rm = TRUE) > Laatste_rapportagejaar){
  stop("De data in het enquiries bestand bevat enkel data van na het meest recente jaar uit het cases bestand. Gebruik een bestand dat over dezelfde tijdsperiode gaat.")
}


# Template om voor elke maand elke ziekte een datapunt te hebben
template_telefoon_GGD_Jaar_Maand_Ziekte <- crossing(GGD = c(unique(telefoon$GGD)),
                                           Jaar = unique(telefoon$Jaar), 
                                           Maand = c("01", "02", "03", "04", "05", "06", "06", "07", "08", "09", "10", "11", "12"), # Leading 0 is nodig om maanden uit de template te filteren die verder in de toekomst liggen dan waarvan meldingen in de data zitten. Anders stort de lijn in je grafiek aan het einde naar 0. 
                                           Specific.Topic = unique(telefoon$Specific.Topic)) %>% 
  mutate(jaarMaand = paste0(Jaar, "-",  Maand)) %>%
  filter(jaarMaand <= substr(max(telefoon$Received.on), 1, 7)) %>%
  mutate(Maand = as.numeric(Maand))


# Groepeer, bereken aantallen, incidenties
telefoon_per_GGD_per_onderwerp_per_maand <- telefoon %>% 
  
  # Groupeer
  group_by(Jaar, Maand, GGD, Specific.Topic) %>% 
  
  # Tel op
  summarize(Aantal = n()) %>% 
  
  # right_join aan template, om alle meldingsplichtige ziekten voor elke maand in het overzicht te hebben
  right_join(template_telefoon_GGD_Jaar_Maand_Ziekte, by = c("Specific.Topic", "Jaar", "Maand", "GGD")) %>% 

  # Zet NA naar 0 (ziekte kwam in die maand niet voor)
  replace_na(list("Aantal" = 0)) %>% 
  
  # Maak variabele aan om als x-as in te voeren in de grafiek  
  mutate(jaarMaand = paste0(Jaar, "-", Maand))

  
###############
# Maak afsplitsing om aantallen voor de regio bij te kunnen voegen

telefoon_incl_regio_per_maandJaar <- telefoon_per_GGD_per_onderwerp_per_maand %>% 
  
  # Groepeer
  group_by(Jaar, Maand, jaarMaand, Specific.Topic) %>% 
  
  # Tel op
  summarize(Aantal = sum(Aantal, na.rm = TRUE)) %>% 
  
  # Maak kolom aan met "GGD code"
  mutate(GGD = "Regio") %>% 
  
  # Selecteer benodigde kolommen, in de juiste volgorde, voor rbind
  select(Jaar, GGD, Specific.Topic, Aantal) %>% 
  
  # Voeg samen met de data per maand per GGD 
  rbind(telefoon_per_GGD_per_onderwerp_per_maand) %>% 
  
  # Maak koppelcode aan
  mutate(koppelcode = paste0(GGD, "_", Jaar)) %>% 
  
  # Koppel de inwonersaantallen aan dit bestand 
  left_join(inwoners, by = "koppelcode") %>% 
  
  # Bereken incidentie en rond deze af zonder decimalen
  mutate(Incidentie = Aantal / Inwoners * 1000000) %>% 
  
  # Verwijder bepaalde kolommen
  select(Jaar.x, GGD.x, Specific.Topic, Aantal, Incidentie) %>% 
  
  # Hernoem kolommen
  rename(GGD = GGD.x, Jaar = Jaar.x) %>% 
  
  # Voeg kwartaal toe, en combi van jaar en kwartaal. 
  mutate(Kwartaal = case_when(
    Maand %in% c(1, 2, 3) ~ "Q1",
    Maand %in% c(4, 5, 6) ~ "Q2",
    Maand %in% c(7, 8, 9) ~ "Q3",
    Maand %in% c(10, 11, 12) ~ "Q4"),
    Jaar_kwartaal = paste0(as.character(Jaar), "_", Kwartaal)) %>% 
  
  # Als het om een jaarrapport gaat, overschrijf Jaar_kwartaal dan (dit om 1 variabele te hebben waar
  # voor de tabel op gegroepeerd kan worden)
  
  mutate(Jaar_kwartaal = case_when(
    type_rapport == 5 ~ as.character(Jaar),
    type_rapport %in% c(1, 2, 3, 4) ~ Jaar_kwartaal))




### Top 5 aantallen aan belletjes per GGD
top5 <- telefoon_incl_regio_per_maandJaar %>% 
  
  #spatie tussen besmettingsaccident voor een (hopelijk) logische linebreak
  mutate(Specific.Topic = ifelse(Specific.Topic == "Besmettingsaccident/mensenbeet",
                             "Besmettingsaccident / mensenbeet", Specific.Topic)) %>%
  
  # Filter naar meest recente jaar en haal regio eruit
  filter(Jaar == Laatste_rapportagejaar & !GGD %in% "Regio") %>% 
  
  # Filter naar gekozen kwartaal
  {if (type_rapport %in% c(1, 2, 3, 4)) filter(., Jaar_kwartaal == paste0(as.character(Jaar), "_Q", type_rapport)) else .} %>%
  
  # Groepeer
  group_by(Specific.Topic, GGD, Jaar_kwartaal) %>% 
  
  # Tel de maanden op naar kwartaal of jaar
  summarize(Aantal = sum(Aantal), .groups = "drop") %>% 
  
  # Groepeer
  group_by(GGD, Jaar_kwartaal) %>% 
  
  # Sorteer aflopend op basis van aantal
  arrange(GGD, desc(Aantal)) %>% 
  
  # Bewaar de eerste 5 regels (Houdt geen rekening met gedeelde plaats bij 5e en 6e plaats)
  slice_head(n = 5)  %>% 

  # Voeg kolom toe die rank aangeeft
  mutate(top = row_number()) %>%

  # Zet data in wide format
  pivot_wider(names_from = GGD, values_from = c(Specific.Topic, Aantal)) %>%

  # Verwijder Jaar_kwartaal kolom (eerst ungroep nodig)
  ungroup() %>%
  select(-Jaar_kwartaal)


###
# Intermezzo: maak vector aan die later in variabele top5 de kolommen na pivot_wider ordent, zonder handcoding van hoeveel GGDen er zijn

volgorde <- 1

for (i in 2:(1+length(unique(telefoon$GGD)))){
  volgorde <- c(volgorde, i, i + length(unique(telefoon$GGD)))
}


top5_tabel <- top5 %>%

  # De pivot_wider laat eerst alle Specifics.Topics zien, dan de aantallen die erbij horen. Ik wil de topics en aantallen per GGD bij elkaar hebben. Code die
  # onafhankelijk is van het aantal GGDen die in de data zitten:
  select(all_of(volgorde)) %>%

  # Maak flextable van dataframe
  flextable() %>%
  # Proberen tekstdoorloop goed te krijgen
  fontsize(size = 8) %>%
  # Kolom 1 met rangnummers vetgedrukt
  bold(j = 1) %>%
  set_table_properties(layout = "autofit") %>%

  # Verwijder de default header (met de kolomnamen), voeg nieuwe header toe
  delete_part(part = "header") %>% 
  add_header_row(values = c("#", c(sort(unique(telefoon$GGD)))),
                 colwidths = c(1, rep(2, length(unique(telefoon$GGD)))), top = TRUE) %>% 
  
  # Toon cell borders
  border_outer(part = "all", border = fp_border(width = 1, color = "#000000")) %>% 
  border_inner(part = "all", border = fp_border(width = 1, color = "#000000"))
  

  # fontsize() %>%
  # flextable::width()




####################
####################
####################

# Hier gebleven. Ik wil de tabel splitsen naar max 3 GGDen per regel. Maar hoe?

####################
####################
####################

# 
# # Poging
# # Dynamisch creeeren van tabellen. Ik wil max 3 GGDen in de tabel laten zien.
# 
# for (i in unique(top5$GGD)){
#   assign(paste0("top5_", i), top5 %>% 
#            ungroup() %>% 
#            filter(GGD %in% i) %>% 
#            select(Specific.Topic, Aantal) %>% 
#            flextable() %>% 
#            set_header_labels(Specificic.Topic = "Specific Topic",
#                              Aantal = "Aantal") %>% 
#            width(j = 1, width = 7, unit = "cm"))
#   
# }
# 
# for (i in 1:(length(unique(telefoon$GGD))/3)){
#   
# 
# }