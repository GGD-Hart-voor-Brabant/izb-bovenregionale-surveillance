


plot_incidentie <- function(data, kolom, ziekte = "totaal", exclusie = NULL, kolom_uitsplitsing = NULL, uitsplitsing = NULL){
  
  ####
  #### Blok code dat checkt of totaal van ziekten of specifieke getoond moet worden, of bepaalde ziekten
  #### moeten worden uitgesloten
  #### Daarnaast wordt gecheckt of wat is ingevuld als ziekte/exclusie/uitsplitsing ook daadwerkelijk voorkomt 
  #### in de data. Zo ja, dan wordt data gefilterd. Zo nee, dan wordt er een melding in de console getoond
  #### en wordt het script onderbroken.
  ####
  
  # Als er een specifieke ziekte moet worden uitgesloten, verwijder deze dan uit de data
  if (!is.null(exclusie)){
    
    stop_Flag <- 0
    
    # Voor alles wat is ingevuld onder exclusie, check of opgegeven ziekte voorkomt in de dataframe
    for (i in exclusie) { 
      if( !i %in% data[[kolom]]) {
        
        stop_Flag = 1
        
        stop(paste0(i, ' komt niet voor in de data. Heb je een ziekte verkeerd gespeld?')  )
      }
    }
    
    if (stop_Flag == 0){
      
      data <- data[!data[[kolom]] %in% exclusie,] # Uitroepteken om "niet in" data te bereiken
      
    }
  }
  
  # Als er een specifieke ziekte getoond moet worden, filter de data dan naar de opgegeven ziekte:
  if (!"totaal" %in% ziekte){
    
    stop_Flag <- 0
    
    # Voor alles wat is ingevuld onder ziekte, check of opgegeven ziekte voorkomt in de dataframe
    for (i in ziekte) { 
      if( !i %in% data[[kolom]]) {
        
        stop_Flag = 1
        
         stop(paste0(i, ' komt niet voor in de data, kan data niet filteren.')  )
      }
    }
    
    if (stop_Flag == 0){
      
      data <- data[data[[kolom]] %in% ziekte,]
      
    }
  }
  
  
  
  
  ####
  #### Blok code dat checkt of er iets is ingevuld bij uitsplitsingen, en of wat er is ingevuld
  #### ook voorkomt in de data. Zo ja, dan wordt data gefilterd.
  #### 
  
  # In het geval dat er iets is ingevuld bij uitsplitsingen
  if (!is.null(kolom_uitsplitsing)){
    
    # Check eerst of de ingevulde naam wel voorkomt in de data. Zo niet, dan kan er niet gefilterd worden.
    stop_Flag <- 0
    
    for (i in uitsplitsing) {
      # Als wat is ingevuld bij uitsplitsing niet voorkomt in de data
      if( !i %in% data[[kolom_uitsplitsing]]) {
        
        stop_Flag = 1
        
        stop(paste0(i, ' komt niet voor in de data, kan data niet filteren.')  )
      }
    }
    
    if (stop_Flag == 0){
      
      data <- data[data[[kolom]] %in% ziekte & data[[kolom_uitsplitsing]] %in% uitsplitsing,]
      
    }
  }
  

  
  ####
  #### Code voor het maken van de grafiek
  ####
  
  # Functie om jaar extra op de x-as te krijgen, op aparte regel. Komt terug binnen scale_x_date.
  firstmonth <- function(x) {
    if_else(lubridate::month(x) == 1,
            format.Date(x, "%b\n%Y"),
            format.Date(x, "%b"))
  }
  
  # Zet jaarMaand om naar een datum, zodat bij de plot op de x-as alleen het jaartal kan worden getoond
  data <- mutate(data, jaarMaand = as.Date(paste0(jaarMaand, "-01"), format = "%Y-%m-%d")) %>% 
    
    ### Als bij ziekte of bij uitsplitsing meer dan 1 waarde wordt ingevuld, dan moet de data opgeteld worden. R doet dit niet automatisch, die plot
    ### beide punten en verbindt ze. Groepeer daarom altijd naar GGD en jaarMaand om 1 datapunt per maand te garanderen
    group_by(GGD, jaarMaand) %>% 
    
    summarize(Aantal = sum(Aantal, na.rm = TRUE),
              Incidentie = sum(Incidentie, na.rm = TRUE)) %>% 
    
    # Hernoem de waarde van de overkoepelende regio in de GGD kolom, zodat deze naam wordt weergegeven in de grafiek subplots en de legenda
    mutate(GGD = case_when(GGD %in% "Regio" ~ Regionaam,
           # Laat van alle andere rijen de oude waarde staan    
           TRUE ~ GGD))
  
  
  ### Splits de data van de overkoepelende regio af
  Regio <- data[data$GGD %in% Regionaam,]
  
  
  grafiek <- ggplot() +
    # Plot alle GGDen behalve de ZeeBraLim cijfers
    geom_line(data = data[!data$GGD %in% Regionaam,], 
              aes(x = jaarMaand, y = Incidentie, group = GGD, color = GGD),
              size = 1.5) + 
    # Zet maanden en jaartallen op de x-as
    scale_x_date(date_breaks = "3 months", labels = firstmonth, limits = c(min(data$jaarMaand), max = NA), expand = c(0,0)) +
    # Bepaal zelf de kleuren per GGD
    scale_color_manual(values=c("BZO" = "#e41a1c", 
                                "HvB" = "#377eb8", 
                                "LN" = "#4daf4a", 
                                "WB" = "#984ea3",
                                "Zee" = "#ff7f00", 
                                "ZL" = "#ffff33",
                                "ZeeBraLim" = "#000000")) + 
    # Geef elke GGD zijn eigen subplot
    facet_wrap(~GGD, ncol = 2) +
    # Voeg ZeeBraLim aan elke plot toe
    annotate(geom = 'line', x = Regio$jaarMaand, y = Regio$Incidentie, group = 1, color = "black", size = 0.75) +
    # Haal de tekst op de x-as weg
    xlab("") + 
    # Ander thema voor layout
    # theme_bw() #+ 
    theme(panel.background = element_rect(fill = "white", colour = NA), 
          panel.border = element_rect(fill = NA, colour = "grey20"), 
          panel.grid = element_line(colour = "grey92"), 
          panel.grid.minor = element_line(size = rel(0.5)), 
          # panel.margin = unit(3, "lines"),
          strip.background = element_rect(fill = "grey85", colour = "grey20"), 
          legend.key = element_rect(fill = "white", colour = NA)
          #axis.text.x = element_text(angle = 45, hjust = 1)
          )
    # Verberg de legenda
    # theme(legend.position = "none")

  
  return(grafiek)
  
}

