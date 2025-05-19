


lees_bestand <- function(bestand, sheet = NULL){
  
  # Als Excel bestand en sheet naam is gespecificeerd
  if(endsWith(bestand, "xlsx") & !is.null(sheet)){
    
    data <- read_excel(bestand, sheet = sheet, guess_max = 21474836)
  
    # Als Excel bestand, sheet naam is niet gespecificeerd  
  } else if(endsWith(bestand, "xlsx")){
    
    data <- read_excel(bestand, guess_max = 21474836)
    
    # Als csv bestand
  } else if(endsWith(bestand, "csv")){
    
    data <- readr::read_csv(bestand, guess_max = 21474836, show_col_types = FALSE)
    
    # Als csv is ingelezen, maar er is maar 1 kolom, dan staat de delimiter waarschijnlijk niet goed. Gebruik read_csv2.
    if(ncol(data) == 1){
      data <- readr::read_csv2(bestand, guess_max = 21474836, show_col_types = FALSE)

    }
  }
  
  # # Vervang spaties in kolomnamen met punten, om aanroepen makkelijker te maken 
  data <- data %>%
    select_all(~gsub("\\s+", ".", .))
    # rename_all(list(~make.names(.)))
  
  return(data)
  
}

