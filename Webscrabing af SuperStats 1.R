
# WEB SCRAPING AF SUPERSTATS OG JOIN AF ANDRE TABELLER


# Vi angiver URL til Superstats (bruges kun som udgangspunkt)
html <- "https://superstats.dk/hold/sason?id=11&vis=hjemme&aar=2025%2F2026"

# Vector med alle sæsoner vi vil hente data for
seasons <- c(
  "2025/2026","2024/2025","2023/2024","2022/2023","2021/2022",
  "2016/2017","2015/2016","2013/2014","2007/2008","2006/2007",
  "2005/2006","2004/2005","2003/2004","2002/2003","2001/2002",
  "2000/2001","1999/2000","1998/1999","1996/1997","1995/1996","1993/1994"
)

# Tomt dataframe som data fra alle sæsoner løbende samles i
st_webscrabe <- data.frame()

# Loop der gennemgår hver sæson én ad gangen
for (s in seasons) {
  
  # Bygger korrekt URL for den pågældende sæson
  url <- paste0(
    "https://superstats.dk/hold/sason?id=11&vis=hjemme&aar=",
    URLencode(s, reserved = TRUE)
  )
  
  # Henter HTML-indholdet fra siden
  webpage <- read_html(url)
  
  # Finder alle tabeller på siden
  tables <- html_nodes(webpage, "table")
  
  # Tabel nr. 3 er den med kampdata
  st3_season <- html_table(tables[[3]], fill = TRUE, convert = FALSE)
  
  # Tilføjer sæson som variabel
  st3_season$Season <- s
  
  # Lægger sæsonens data oven i det samlede dataframe
  st_webscrabe <- bind_rows(st_webscrabe, st3_season)
}

# -----------------------------
# DATARENS
# -----------------------------

st_webscrabe <- st_webscrabe %>%
  mutate(
    # Tilskuere omdannes fra tekst til numerisk værdi
    Tilskuere = as.character(Tilskuere),
    Tilskuere = str_trim(Tilskuere),
    Tilskuere = na_if(Tilskuere, "-"),
    Tilskuere = na_if(Tilskuere, ""),
    Tilskuere = str_replace_all(Tilskuere, "\\.", ""), # fjerner tusindtals-punktum
    Tilskuere = str_replace_all(Tilskuere, ",", ""),
    Tilskuere = as.numeric(Tilskuere),
    
    # Udtrækker modstanderhold fra kamp-teksten ("VFF - FCK" → "FCK")
    Modstander = str_replace(Kamp, "VFF - ", ""),
    
    # Konverterer dato fra tekst til Date-klasse
    Dato = as.Date(Dato, format = "%d.%m.%Y")
  )

# -----------------------------
# MODSTANDER-KATEGORIER
# -----------------------------

# Beregner gennemsnitligt tilskuertal pr. modstander
modstander_niveau <- st_webscrabe %>%
  group_by(Modstander) %>%
  summarise(mean_tilskuere = mean(Tilskuere, na.rm = TRUE), .groups = "drop")

# Finder kvartiler for gennemsnitligt tilskuertal
qs <- quantile(modstander_niveau$mean_tilskuere,
               probs = c(0.25, 0.5, 0.75),
               na.rm = TRUE)

# Inddeler modstandere i A–D kategorier
modstander_niveau <- modstander_niveau %>%
  mutate(
    kategori = case_when(
      mean_tilskuere > qs[3] ~ "A",
      mean_tilskuere > qs[2] ~ "B",
      mean_tilskuere > qs[1] ~ "C",
      TRUE                  ~ "D"
    )
  )

# Lægger modstanderkategori ind i hoveddatasættet
st_webscrabe <- st_webscrabe %>%
  left_join(modstander_niveau, by = "Modstander")


# Beregner gennemsnitligt tilskuertal pr. sæson
st_webscrabe <- st_webscrabe %>%
  group_by(Season) %>%
  mutate(mean_tilskuere_sæson = mean(Tilskuere, na.rm = TRUE)) %>%
  ungroup()

# -----------------------------
# LAV KOLONNE MED VFF'S MÅL I SIDSTE KAMP
# -----------------------------

# Splitter resultat "2 - 1" i VFF-mål og modstander-mål, så vi kan 
st_webscrabe <- st_webscrabe %>%
  mutate(
    mål_split = str_split_fixed(Res, " - ", 2),
    VFF_mål = as.numeric(mål_split[,1]),
    Modstander_mål = as.numeric(mål_split[,2])
  ) %>%
  select(-mål_split)

# Finder hvor mange mål VFF scorede i kampen før
st_webscrabe <- st_webscrabe %>%
  arrange(Dato) %>%
  mutate(VFF_mål_kampen_foer = lag(VFF_mål))

# -----------------------------
# JOIN ST_WEBSCRABE MED BILLET-DATA
# -----------------------------

# Sikrer ens datatyper før join
st_webscrabe <- st_webscrabe %>%
  mutate(Season = as.character(Season),
         Rnd = as.integer(Rnd))

vffkort01 <- vffkort01 %>%
  mutate(sæson = as.character(sæson),
         runde = as.integer(runde)) %>%
  distinct(sæson, runde, .keep_all = TRUE)

# Joiner billetdata på sæson + runde
st_webscrabe_join <- st_webscrabe %>%
  left_join(
    vffkort01,
    by = c("Season" = "sæson",
           "Rnd"    = "runde")
  )

# -----------------------------
# JOIN MED VEJRDATA
# -----------------------------

# Konverterer dato til Date-klasse
vff_hjemmekampe_med_vejr$dato_index <- as.Date(
  vff_hjemmekampe_med_vejr$dato_index,
  format = "%d/%m/%Y"
)

# Endeligt datasæt: webscraping + billetter + vejr
st_webscrabe_joined <- st_webscrabe_join %>%
  left_join(
    vff_hjemmekampe_med_vejr,
    by = c("Dato" = "dato_index")
  )

# Vis resultat
view(st_webscrabe_joined)


