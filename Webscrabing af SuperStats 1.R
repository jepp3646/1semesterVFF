# Webscrabing af superstats

# Liste over alle pakker
packages <- c(
  "tidyverse", "palmerpenguins", "ggthemes", "ggridges", "nycflights13",
  "Lahman", "janitor", "vroom", "arrow", "readxl", "writexl", "googlesheets4",
  "RSQLite", "babynames", "stringi", "hms", "tidymodels", "fable", "car", "MASS",
  "lmtest", "tseries", "ggfortify", "polite", "tufte", "rvest", "xml2", "dplyr","stringr"
)

library(tidyverse)
library(rvest)
library(httr)
library(dplyr)
library(stringr)

html <- ("https://superstats.dk/hold/sason?id=11&vis=hjemme&aar=2025%2F2026")

#Vi finder ud af hvilken table der er den rigtige på superstats
st1 <- html_table(tables[1], fill = TRUE)
st2 <- html_table(tables[2], fill = TRUE)
st3 <- html_table(tables[3], fill = TRUE)

view(st3)


#Så laver en variable der gemmer sæsonerne (Kun sæsoner med det nye stadium)
seasons <- c(
  "2025/2026",
  "2024/2025",
  "2023/2024",
  "2022/2023",
  "2021/2022",
  "2016/2017",
  "2015/2016",
  "2013/2014",
  "2007/2008",
  "2006/2007",
  "2005/2006",
  "2004/2005",
  "2003/2004",
  "2002/2003",
  "2001/2002",
  "2000/2001",
  "1999/2000",
  "1998/1999",
  "1996/1997",
  "1995/1996",
  "1993/1994"
)

#Vi laver et tomt nyt dataframe
dataloop <- data.frame()  


#Så laver vi et loop som tager dataen fra alle årene

for (s in seasons) {
  
  url <- paste0(
    "https://superstats.dk/hold/sason?id=11&vis=hjemme&aar=",
    URLencode(s, reserved = TRUE)
  )
  
  webpage <- read_html(url)
  tables <- html_nodes(webpage, "table")
  
  st3_season <- html_table(tables[[3]], fill = TRUE, convert = FALSE)
  
  st3_season$Season <- s

  dataloop <- bind_rows(dataloop, st3_season)
}


#Lav date kolonnen om til en dato og insæt / i stedet for .

dataloop <- dataloop %>%
  mutate(
    Dato = as.Date(Dato, format = "%d.%m.%Y")
  )

#Få R til at læse 4.500 som 4500 og lav det om til 4500

dataloop <- dataloop %>%
  mutate(
    Tilskuere = as.character(Tilskuere),                #Laver kolonnen om til tekststreng
    Tilskuere = str_trim(Tilskuere),
    Tilskuere = na_if(Tilskuere, "-"),
    Tilskuere = na_if(Tilskuere, ""),
    Tilskuere = str_replace_all(Tilskuere, "\\.", ""),  #Fjerner tusindtals punktummer
    Tilskuere = str_replace_all(Tilskuere, ",", ""),    #Fjerner alle kommaer
    Tilskuere = as.numeric(Tilskuere),                  #Laver det tilbage til tal
    Modstander = str_replace(Kamp, "VFF - ", ""),       #Laver en ny kolonne med mostanderholdet
    Dato = as.Date(Dato, format = "%d.%m.%Y")
  )

# Vi laver en kolonne, hvor hvert hold 

#Indel i kategorier baseret på median

modstander_niveau <- dataloop %>%
  group_by(Modstander) %>%
  summarise(
    mean_tilskuere = mean(Tilskuere, na.rm = TRUE),
    .groups = "drop"
  )

# Beregner kvartiler, så holdende kan indeles i kategorier

qs <- quantile(modstander_niveau$mean_tilskuere,
               probs = c(0.25, 0.5, 0.75),
               na.rm = TRUE)

# Sætter holdene i kategorier udfra opdelingen af tilskuere 

modstander_niveau <- modstander_niveau %>%
  mutate(
    kategori = case_when(
      mean_tilskuere > qs[3] ~ "A",   # højeste gennemsnit
      mean_tilskuere > qs[2] ~ "B",
      mean_tilskuere > qs[1] ~ "C",
      TRUE                  ~ "D"     # laveste gennemsnit
    )
  )

#Indsæt mostander kategori i dataloop dataframe

dataloop <- dataloop %>%
  left_join(modstander_niveau, by = "Modstander")

#Lav en ny kolonne til gennemsnits tilskuer pr. år

dataloop <- dataloop %>%
  group_by(Season) %>%          # gruppér efter sæson
      mutate(
        mean_tilskuere_sæson = mean(Tilskuere, na.rm = TRUE)
      ) %>%
      ungroup()

# Hvor mange mål har VFF scoret kampen før Hjemmekampe

dataloop <- dataloop %>%
  mutate(
    # Split "2 - 2" til "2" og "2"
    mål_split = str_split_fixed(Res, " - ", 2),

    VFF_mål = as.numeric(mål_split[,1]),
    
    Modstander_mål = as.numeric(mål_split[,2])
  ) %>%
  select(-mål_split)   # fjern hjælpekolonnen


dataloop <- dataloop %>%
  arrange(Dato) %>%              # sortér efter dato
  mutate(
    VFF_mål_kampen_foer = lag(VFF_mål)
  )

#Tjek kolonne navne i dataloop
names(dataloop)


# Se billet salg fra vffkort01 tabel
view(vffkort01)

names(vffkort01)

#Vi skal have joinet vffkort01, hvilket vi gør på sæson og runde, da der ikke er en dato i vffkort01

#Først sikrer vi os, at de har samme format, altså season er text og runde er int

dataloop  <- dataloop  %>% mutate(Season = as.character(Season),
                                  Rnd    = as.integer(Rnd))

vffkort01 <- vffkort01 %>% mutate(sæson = as.character(sæson),
                                  runde = as.integer(runde))

#Vi får en fejl, når vi forsøger at joine, så for at finde evt dubletter tæller vi season og rnd, og ser om der er nogen gentager sig (n>1)
dataloop %>%
  count(Season, Rnd) %>%
  filter(n > 1)

#Her kan vi se, at i vffkort01 er der er dubletter 

vffkort01 %>%
  count(sæson, runde) %>%
  filter(n > 1)

#For at fjerne dem kan vi distinct funktionen til at fjerne dubletter og beholde den første.

vffkort01 <- vffkort01 %>%
  distinct(sæson, runde, .keep_all = TRUE)


#Nu kan vi joine de to tabelleber via left join og kalder den dataloop_join
dataloop_join <- dataloop %>%
  left_join(
    vffkort01,
    by = c("Season" = "sæson",
           "Rnd"    = "runde")
  )


#Nu kan vi joine vores vejr tabel: vff_hjemmekampe_med_vejr

view(vff_hjemmekampe_med_vejr)

#Vi tjekker vil 
head(vff_hjemmekampe_med_vejr$dato_index)

#Før vi kan det skal vi sørge for, at date_index i vff_hjemmekampe_med_vejr er en date (og ikke en tekst) ligesom i dataloop_join

vff_hjemmekampe_med_vejr$dato_index <- as.Date(vff_hjemmekampe_med_vejr$dato_index, 
                                               format = "%d/%m/%Y")

#Nu kan vi joine vores vff_hjemmekampe_med_vejr med hovedtabellen data_loop_join
joinedTable <- left_join(
  dataloop_join,
  vff_hjemmekampe_med_vejr,
  by = c("Dato" = "dato_index")
)

view(joinedTable)




