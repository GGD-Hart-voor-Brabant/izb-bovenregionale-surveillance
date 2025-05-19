
# Dit script maakt doet 2 queries naar tabellen van CBS Statline. De eerste query
# vraagt op welke gemeenten bij welke GGD horen. De tweede query vraagt van deze
# gemeenten de inwonersaantallen op, voegt aan de data toe bij welke GGD een
# gemeente hoort en telt daarna per GGD de inwonersaantallen op. Tel zelf de 
# aantallen per GGD op om de aantallen voor ZeeBraLim te krijgen.



# Nodige aanpassingen bij het opzoeken van inwonersaantallen van een nieuw jaar:
#
# Stap 1.
# 	Zoek de juiste tabel uit CBS Statline op. Voorbeeld van 2022: (Gebieden in Nederland 2022)
#   https://opendata.cbs.nl/statline/#/CBS/nl/dataset/85067NED/table?ts=1678109891661
# 	Noteer de code van de tabel. In bovenstaand voorbeeld is de code "85067NED
#   Vul deze code in op als eerste argument in de cbs_get_data() functie => cbs_get_data('85067NED',
#
# Stap 2.
#	Pas het jaar aan in de query naar de CBS tabel met inwonersaantallen, op het punt:
# Perioden = c("2021JJ00"). Als je dit om wilt zetten naar bijvoorbeeld 2022, maak er dan c("2022JJ00") van.





rm(list=ls())

library(cbsodataR)
library(dplyr)
library(stringr)


######################
# Gemeenten naar GGD #
######################

## CBS tabellencode gebiedsindeling (Gebieden in Nederland 2022)
# https://opendata.cbs.nl/statline/#/CBS/nl/dataset/85067NED/table?ts=1678109891661
# -> Code is 85067NED

# 2022: 85067NED
# 2021: 84929NED
# 2020: 84721NED
# 2019: 84378NED


# # Gebruik de meta data om de codes te achterhalen van de data die je wilt hebben
# meta <- cbs_download_meta('84929NED')

gemeenten_per_GGD <- cbs_get_data('85385NED',
                          select = c("Code_1",
                                     "Naam_2",
                                     "Code_14", 
                                     "Naam_15")) %>% 
  mutate(Code_1 = str_trim(Code_1, side = "both"),
    Naam_15 = str_trim(Naam_15, side = "both")) %>% 
  filter(Naam_15 %in% c("GGD Brabant-Zuidoost",
                        "GGD Hart voor Brabant",
                        "GGD West-Brabant",
                        "GGD Limburg-Noord",
                        "GGD Zuid-Limburg",
                        "GGD Zeeland")) 
    
#####################
# Inwonersaantallen # 
#####################


## CBS tabellencode inwonersaantallen
# https://opendata.cbs.nl/statline/#/CBS/nl/dataset/37230ned/table
# -> Code is 37230NED

# # Gebruik de meta data om de codes te achterhalen van de data die je wilt hebben
meta <- cbs_download_meta('37230NED')


# Haal data op van Statline
inwonersaantallen <- cbs_get_data("37230NED", 
                                        RegioS = has_substring(gemeenten_per_GGD$Code_1),
                                        Perioden = c("2023MM01"),  
                                        select = c("RegioS", 
                                                   "Perioden",
                                                   "BevolkingAanHetBeginVanDePeriode_1"
                                        )) %>%
  cbs_add_label_columns() %>% 
  left_join(gemeenten_per_GGD[, c("Code_1", "Naam_15")], by = c("RegioS" = "Code_1")) %>% 
  group_by(Naam_15) %>% 
  summarize(Totaal = sum(BevolkingAanHetBeginVanDePeriode_1, na.rm = T))

# Inwonersaantal heel ZeeBraLim
sum(inwonersaantallen$Totaal)


# Inwonersaantal heel Nederland
cbs_get_data("37230NED", 
   RegioS = has_substring("NL01"),
   Perioden = c("2023MM01"),  
   select = c("RegioS", 
              "Perioden",
              "BevolkingAanHetBeginVanDePeriode_1"
   ))
