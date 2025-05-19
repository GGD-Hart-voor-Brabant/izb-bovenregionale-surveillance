
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
# 	Noteer de code van de tabel (staat in de weblink). In bovenstaand voorbeeld is de code "85067NED
#   Vul deze code in op als eerste argument in de cbs_get_data() functie => cbs_get_data('85067NED',
#
# Stap 2.
#	Pas het jaar aan in de query naar de CBS tabel met inwonersaantallen, op het punt:
# Perioden = c("2021JJ00"). Als je dit om wilt zetten naar bijvoorbeeld 2022, maak er dan c("2022JJ00") van.
# Mocht je gebruik maken van voorlopige cijfers (bij de kwartaalrapportages), gebruik dan niet het formaat 2022JJ00 (jaartal-JJ-00), maar
# het formaat 2023MM01 (jaartal-MM-01).


library(cbsodataR)
library(dplyr)
library(stringr)


######################
# Gemeenten naar GGD #
######################

## CBS tabellencode gebiedsindeling (Gebieden in Nederland 2022)
# https://opendata.cbs.nl/statline/#/CBS/nl/dataset/85067NED/table?ts=1678109891661
# -> Code is 85067NED


#Navragen Elke; Moet de data van voorgaande jaren ingedeeld worden obv de toenmalige GGD indeling/
#Of; willen we de inwonersaantallen van bijvoorbeeld 2014; tellen o.b.v. de huidige indeling


# # Gebruik de meta data om de codes te achterhalen van de data die je wilt hebben
# meta <- cbs_download_meta('85385NED')

gemeenten_per_GGD <- cbs_get_data('85385NED',
                          select = c("Code_1",
                                     "Naam_2",
                                     "Code_14", 
                                     "Naam_15")) %>% 
  mutate(Code_1 = str_trim(Code_1, side = "both"),
    Naam_15 = str_trim(Naam_15, side = "both")) %>% 
  filter(Naam_15 %in% GGD_namen_cbs) 
    
#####################
# Inwonersaantallen # 
#####################


## CBS tabellencode inwonersaantallen
# https://opendata.cbs.nl/statline/#/CBS/nl/dataset/37230ned/table
# -> Code is 37230NED

# # Gebruik de meta data om de codes te achterhalen van de data die je wilt hebben
# meta <- cbs_download_meta('37230NED')

# Perioden = c("2021JJ00"). Als je dit om wilt zetten naar bijvoorbeeld 2022, maak er dan c("2022JJ00") van.

#Pieter: Vector maken met alle relevante perioden:

alle_jaren <- paste0(2014:year(today()),"MM01")


# Haal data op van Statline
inwonersaantallen <- cbs_get_data("37230NED", 
                                        RegioS = has_substring(gemeenten_per_GGD$Code_1),
                                        Perioden = alle_jaren,  
                                        select = c("RegioS", 
                                                   "Perioden",
                                                   "BevolkingAanHetBeginVanDePeriode_1"
                                        )) %>%
  cbs_add_label_columns() %>% 
  left_join(gemeenten_per_GGD[, c("Code_1", "Naam_15")], by = c("RegioS" = "Code_1")) %>% 
  group_by(Naam_15,Perioden) %>% 
  summarize(Totaal = sum(BevolkingAanHetBeginVanDePeriode_1, na.rm = T)) %>%
  ungroup()

# 2023: 85385NED
# 2022: 85067NED
# 2021: 84929NED
# 2020: 84721NED
# 2019: 84378NED



# Inwonersaantal heel ZeeBraLim
inwoners_regio_totaal <- inwonersaantallen %>%
  group_by(Perioden) %>% 
  summarise(Totaal = sum(Totaal)) %>% 
  mutate(GGD = "Regio")%>%
  #Volgorde kolommen aanpassen met select
  select(GGD,Perioden,Totaal)


# Inwonersaantal heel Nederland
inwoners_heel_nl <- cbs_get_data("37230NED", 
   RegioS = has_substring("NL01"),
   Perioden = alle_jaren,  
   select = c("RegioS", 
              "Perioden",
              "BevolkingAanHetBeginVanDePeriode_1"
   ))



names(inwonersaantallen) <- c("GGD","Jaar","Aantal")
names(inwoners_regio_totaal) <- c("GGD","Jaar","Aantal")
names(inwoners_heel_nl) <- c("GGD","Jaar","Aantal")

inwoners_heel_nl$GGD <- "Nederland"
#NL en GGDen samenvoegen
inwonersaantallen_breed <- rbind(inwonersaantallen,  inwoners_regio_totaal,inwoners_heel_nl) %>%
  mutate(Jaar = substr(Jaar,1,4)) %>%
  #Naar breed; kolom_per jaar
  pivot_wider(values_from = Aantal, names_from = Jaar)

#Checken of er namen aangepast moeten worden of niet; daarna aanpassen
if(!is.null(GGD_namen_aangepast)){
  #Dit kan vast handiger
  #Had in eerste instantie de hele vector vervangen:
  #inwonersaantallen_breed$GGD[1:length(GGD_namen_cbs)] <- GGD_namen_aangepast
  
  #Dit kan echter de overhoop gooien (waardoor GGDen dus de verkeerde naam kregen)
  
  for(i in 1:length(GGD_namen_cbs)){
  inwonersaantallen_breed$GGD[inwonersaantallen_breed$GGD == GGD_namen_cbs[i]] <-
  GGD_namen_aangepast[i]  
  }
}

writexl::write_xlsx(inwonersaantallen_breed,"input/Inwonersaantallen_06_07_Pieter.xlsx")
