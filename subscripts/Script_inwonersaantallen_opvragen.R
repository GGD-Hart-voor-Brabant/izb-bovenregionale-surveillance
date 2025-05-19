
# Dit script doet 2 queries naar tabellen van CBS Statline. De eerste query
# vraagt op welke gemeenten bij welke GGD horen. De tweede query vraagt van deze
# gemeenten de inwonersaantallen op, voegt aan de data toe bij welke GGD een
# gemeente hoort en telt daarna per GGD de inwonersaantallen op.

# Deze queries worden per jaar uitgevoerd in een loop.


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
library(lubridate)
library(tidyr)
# 2024: 85755NED
# 2023: 85385NED
# 2022: 85067NED
# 2021: 84929NED
# 2020: 84721NED
# 2019: 84378NED
# 2018: 83859NED
# 2017: 83553NED
# 2016: 83287NED



gemeenten_per_GGD_per_jaar <- cbind("tabelcode" = c("83287NED","83553NED","83859NED", "84378NED","84721NED","84929NED","85067NED","85385NED", "85755NED"),
                                    "jaar" = c(2016:2024))
jaren <- 2016:2024

inwoners_per_jaar <- lapply(1:nrow(gemeenten_per_GGD_per_jaar), function(x){
  
  tabelcode =  gemeenten_per_GGD_per_jaar[x,1]
  jaar = gemeenten_per_GGD_per_jaar[x,2]
  
  #Gemeentelijke indeling per GGD voor dat jaar ophalen
  gemeenten_per_jaar <- cbs_get_data(tabelcode,
                                         select = c("Code_1","Naam_2","Code_14","Naam_15")) %>%
    mutate(Code_1 = str_trim(Code_1, side = "both"),
           Naam_15 = str_trim(Naam_15, side = "both")) %>%
    filter(Naam_15 %in% GGD_namen_cbs)
  
  jaarcode = paste0(jaar,"MM01")
  
  inwonersaantallen <- cbs_get_data("37230NED", 
                                    RegioS = has_substring(gemeenten_per_jaar$Code_1),
                                    Perioden = jaarcode,  
                                    select = c("RegioS", 
                                               "Perioden",
                                               "BevolkingAanHetBeginVanDePeriode_1")) %>%
    left_join(gemeenten_per_jaar, by = c("RegioS" = "Code_1")) %>%
    rename("GGD" = "Naam_15") %>%
    group_by(GGD) %>%
    summarise(Aantal = sum(BevolkingAanHetBeginVanDePeriode_1)) %>%
    mutate(Jaar = jaar)
  
  print(paste("inwoners per regio in", jaar, "opgehaald"))
  
  return(inwonersaantallen)
})

inwonersaantallen <- do.call(rbind,inwoners_per_jaar) %>%
  select(GGD,Jaar,Aantal)


inwoners_regio_totaal <- inwonersaantallen %>%
  group_by(Jaar) %>% 
  summarise(Aantal = sum(Aantal)) %>% 
  mutate(GGD = "Regio")%>%
  #Volgorde kolommen aanpassen met select
  select(GGD,Jaar,Aantal)
  

# Inwonersaantal heel Nederland
inwoners_heel_nl <- cbs_get_data("37230NED", 
                                 RegioS = has_substring("NL01"),
                                 Perioden = paste0(jaren,"MM01"),  
                                 select = c("RegioS", 
                                            "Perioden",
                                            "BevolkingAanHetBeginVanDePeriode_1")) %>%
  mutate(GGD = "Nederland",
         Jaar = substr(Perioden,1,4),
         Aantal = BevolkingAanHetBeginVanDePeriode_1) %>%
  filter(Jaar %in% unique(inwonersaantallen$Jaar)) %>%
  select(GGD,Jaar,Aantal)

print("Landelijke inwoners opgehaald")

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
  
  #Dit kan echter de volgorde overhoop gooien (waardoor GGDen dus de verkeerde naam kregen)
  
  for(i in 1:length(GGD_namen_cbs)){
  inwonersaantallen_breed$GGD[inwonersaantallen_breed$GGD == GGD_namen_cbs[i]] <-
  GGD_namen_aangepast[i]  
  }
}

writexl::write_xlsx(inwonersaantallen_breed,"input/Inwonersaantallen.xlsx")
