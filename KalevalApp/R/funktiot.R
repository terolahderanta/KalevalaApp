# Tietoa kalevala.txt tiedostosta:
# This eBook is for the use of anyone anywhere at no cost and with
# almost no restrictions whatsoever.  You may copy it, give it away or
# re-use it under the terms of the Project Gutenberg License included
# with this eBook or online at www.gutenberg.org


# Lataa tarvittavat paketit datan käsittelemiseksi
lataa_paketit <- function(){
  library(tidyverse)
  library(tidytext)
  library(purrr)
}

# Lataa Kalevala-teksti ja palauta tibble, jossa yksi havainto on yksi tekstirivi 
lataa_data <- function(tiedostosijainti = "data/kalevala.txt"){
  # Lataa teksti
  teksti <- tibble(
    rivi = read_file(tiedostosijainti) |>
      str_split("\r\n") |>
      unlist() #|>
      # Poista välimerkit
      #str_replace_all("[:punct:]", replacement = "") |>
      # Vain pienet kirjaimet
      #str_to_lower()
  ) |>
    # Poista tyhjät rivit
    filter(rivi != "") |> 
    
    # Jätä vain oleelliset rivit liittyen Kalevalaan
    slice(-(1:46)) |> 
    slice(-(22847:n())) |> 
    
    #  Poista ylimääräiset välilyönnit rivin alusta ja lopusta
    mutate(rivi = str_trim(rivi)) |> 
    
    # Lisää ID-muuttuja
    mutate(id = 1:n(), .before = 1)
  
  # Etsi runojen alut
  runo_alut <- teksti |> 
    filter(str_detect(rivi, "runo$")) |> 
    pull(id) |> 
    append(nrow(teksti))
  
  # Lisää runon numero
  teksti <- teksti |> 
    mutate(runo_nro = rep(0:50, times = c(runo_alut[1],diff(runo_alut))), .after = 2) |> 
    
    # Poista runojen otsikot
    filter(!str_detect(rivi, "runo$")) |> 
    
    # Päivitä ID
    mutate(id = 1:n()) |> 
    
    # Lisää runojen aiheille nimet
    mutate(runo_aihe = case_when(
      runo_nro %in% 1:2 ~ "Luominen ja Väinämöisen syntymä",
      runo_nro %in% 3:5 ~ "Aino-runo",
      runo_nro %in% 6:10 ~ "Väinämöisen ja Ilmarisen matkat Pohjolaan",
      runo_nro %in% 11:15 ~ "Lemminkäinen I",
      runo_nro %in% 16:19 ~ "Väinämöisen veneenveisto ja Pohjolasta kosinta I",
      runo_nro %in% 20:25 ~ "Häärunot",
      runo_nro %in% 26:30 ~ "Lemminkäinen II",
      runo_nro %in% 31:36 ~ "Kullervo",
      runo_nro %in% 37:38 ~ "Kultaneito ja Pohjolasta kosinta II",
      runo_nro %in% 39:41 ~ "Matkalla sammon ryöstöön",
      runo_nro %in% 42:43 ~ "Taistelu sammosta",
      runo_nro %in% 44 ~ "Kanteleen soitto",
      runo_nro %in% 45:49 ~ "Kalevalan ja Pohjolan välinen taistelu",
      runo_nro %in% 50 ~ "Väinämöisen lähtö",
      TRUE ~ "Ei nimeä"), .after = 3) |> 
    
    # Lisää runon nimelle ID
    #mutate(runo_aihe_id = runo_aihe |> 
    #         factor(levels = unique(runo_aihe)) |> 
    #         as.integer(), .after = 3) |> 
    
    # Lisää runokohtainen id
    #group_by(runo_nro) |> 
    #mutate(rivi_id_runossa = as.integer(id - min(id) + 1), .after = 2) |> 
    #ungroup() |> 
    
    # Vaihda taulukon sarakkeiden nimet
    rename(
      "Rivin numero" = id,
      "Teksti" = rivi,
      "Runon numero" = runo_nro,
      "Runon aihe" = runo_aihe
    )
  
  return(teksti)
}

# Palauta rivit, joissa sana mainitaan
etsi_sana <- function(sana, teksti){
  rivi_id <- teksti |> 
    mutate(Teksti = Teksti |> 
           # Poista välimerkit
           str_replace_all("[:punct:]", replacement = "") |>
           # Vain pienet kirjaimet
           str_to_lower()) |> 
    filter(str_detect(Teksti, sana)) |> 
    pull("Rivin numero")
    
  teksti |> filter(`Rivin numero` %in% rivi_id)
}
