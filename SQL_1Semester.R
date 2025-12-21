# -----------------------------
# Forbind til SQLite Database
# -----------------------------

# Liste over alle pakker (dokumentation: hvad projektet bruger)
packages <- c(
  "tidyverse", "palmerpenguins", "ggthemes", "ggridges", "nycflights13",
  "Lahman", "janitor", "vroom", "arrow", "readxl", "writexl", "googlesheets4",
  "RSQLite", "babynames", "stringi", "hms", "tidymodels", "fable", "car", "MASS",
  "lmtest", "tseries", "ggfortify", "polite", "tufte", "rvest", "xml2",
  "dplyr", "stringr", "DBI", "lubridate"
)

# Indlæs pakker vi bruger i dette script
library(tidyverse)
library(rvest)
library(httr)
library(dplyr)
library(stringr)
library(DBI)
library(RSQLite)
library(janitor)    # Bruges til at rense kolonnenavne (snake_case) og generel oprydning
library(lubridate)  # Bruges til at arbejde med datoer/tid


# Opret forbindelse til SQLite databasen (filen ligger i dit projekt-folder)
con <- dbConnect(
  SQLite(),
  dbname = "VffDataBaseNew.db"
)

# Sørger for at forbindelsen altid lukkes, selv hvis scriptet fejler undervejs
on.exit(DBI::dbDisconnect(con), add = TRUE)


# -----------------------------
# Load data ind i R
# -----------------------------

# Load vores tabeller ind (fra .rds-filer)
vffkort01 <- readRDS("vffkort01.rds")
fcidk <- readRDS("fcidk.rds")
vff_hjemmekampe_med_vejr <- readRDS("vff_hjemmekampe_med_vejr.rds")

# Skriv (eller overskriv) tabellerne i databasen, så DB altid er opdateret
dbWriteTable(con, "st_webscrabe", st_webscrabe, overwrite = TRUE)
dbWriteTable(con, "vffkort01", vffkort01, overwrite = TRUE)
dbWriteTable(con, "vff_hjemmekampe_med_vejr", vff_hjemmekampe_med_vejr, overwrite = TRUE)
dbWriteTable(con, "fcidk", fcidk, overwrite = TRUE)

# Kig på vejr-tabellen i R (for at validere data ser fornuftige ud)
View(vff_hjemmekampe_med_vejr)


# -----------------------------
# Join Tabeller (kontrol-joins)
# -----------------------------

# Join st_webscrabe og DMI tabel (vff_hjemmekampe_med_vejr)
# OBS: st_webscrabe.Dato er numeric (dage siden 1970-01-01),
# mens vff_hjemmekampe_med_vejr.dato_index er tekst "dd/mm/yyyy".
# Derfor konverterer vi Dato -> dd/mm/yyyy i SQL før join.

joined_st_webscrabe_dmi <- DBI::dbGetQuery(con, "
SELECT *
FROM st_webscrabe s
INNER JOIN vff_hjemmekampe_med_vejr d
  ON strftime('%d/%m/%Y', date('1970-01-01', printf('+%d days', s.Dato))) = d.dato_index
ORDER BY s.Dato DESC;
")

# Tjek joinet i R
View(joined_st_webscrabe_dmi)

# (Debug / sanity checks) – viser et par datoer i hver tabel
DBI::dbGetQuery(con, "SELECT Dato FROM st_webscrabe ORDER BY Dato DESC LIMIT 10;")
DBI::dbGetQuery(con, "SELECT dato_index FROM vff_hjemmekampe_med_vejr ORDER BY dato_index DESC LIMIT 10;")

# Størrelse og kolonnenavne på joinet
nrow(joined_st_webscrabe_dmi)
ncol(joined_st_webscrabe_dmi)
names(joined_st_webscrabe_dmi)

# Da der laves udtræk fra SQL, så bliver "Dato" værdien retuneret som numeric,
# og det skal vi lave om til Date værdi igen i R
joined_st_webscrabe_dmi$Dato <- as.Date(joined_st_webscrabe_dmi$Dato, origin = "1970-01-01")

# Se hvilken datatype Dato har
class(joined_st_webscrabe_dmi$Dato)


# Join st_webscrabe og fcidk (giver modstanders gennemsnitlige tilskuere)
joined_st_webscrabe_fcidk <- dbGetQuery(con, "
  SELECT
      s.*,
      f.tilskuere AS modstander_gns_tilskuere
  FROM st_webscrabe AS s
  LEFT JOIN fcidk AS f
      ON s.Modstander = f.kort
")

View(joined_st_webscrabe_fcidk)

# Se hvilke tabeller der er i databasen (kontrol)
dbListTables(con)


# Join st_webscrabe med vffkort01 (sæson/runde join)
joined_st_webscrabe_vfkort01 <- dbGetQuery(con, "
SELECT
  st_webscrabe.*,
  vffkort01.*
FROM st_webscrabe
LEFT JOIN vffkort01
  ON st_webscrabe.Season = vffkort01.sæson
 AND st_webscrabe.Rnd    = vffkort01.runde
")

View(joined_st_webscrabe_vfkort01)


# -----------------------------
# Byg samlet model-datasæt i databasen
# -----------------------------

# Nu vil vi lave en stor tabel, hvor vi joiner st_webscrabe med vffkort01, fcidk og vores DMI tabel
# Vi dropper den først, så vi altid starter fra en "ren" tabel
DBI::dbExecute(con, "DROP TABLE IF EXISTS model_dataset;")

DBI::dbExecute(con, "
CREATE TABLE model_dataset AS
SELECT
  s.*,
  v.*,
  f.*,
  d.*
FROM st_webscrabe AS s
LEFT JOIN vffkort01 AS v
  ON s.Season = v.`sæson`
 AND s.Rnd    = v.`runde`
LEFT JOIN fcidk AS f
  ON s.Modstander = f.kort
LEFT JOIN vff_hjemmekampe_med_vejr AS d
  ON strftime('%d/%m/%Y', date('1970-01-01', printf('+%d days', s.Dato))) = d.dato_index;
")

# Kontrol: findes tabellen nu?
DBI::dbListTables(con)

# Læs tabellen ind i R og kig på den
model_dataset <- DBI::dbReadTable(con, "model_dataset")
View(model_dataset)
names(model_dataset)

# Kontrol: hvilke felter findes i vejr-tabellen (hvis du vil tjekke at de er med i join)
DBI::dbListFields(con, "vff_hjemmekampe_med_vejr")


# -----------------------------
# Next Step:
# Download joinet datasæt, formatér behørigt, og gør data klar til preprocessing
# -----------------------------

# Eksporter model_dataset til R (hvis du kører scriptet i bidder, er det rart at læse den igen)
model_dataset <- DBI::dbReadTable(con, "model_dataset")

# Vi bruger glimpse fra dplyr pakken, da den giver et glimrende overblik over rækker og kolonner
dplyr::glimpse(model_dataset)


# Vi rydder op i dato, kolonnenavne, fjern junk + dubletter
# - clean_names() gør fx "VFF_mål" -> "vff_mal"
# - Vi laver dato om til Date
# - Vi gør udvalgte tekstfelter til factor (giver mening i modelling)
df <- model_dataset %>%
  janitor::clean_names() %>%
  distinct() %>%  # undgå dubletter efter joins
  mutate(
    dato = as.Date(dato, origin = "1970-01-01"),
    season = factor(season),
    modstander = factor(modstander),
    kategori = factor(kategori)
  )

# Fjern “join-rester”: kolonner der næsten kun er NA
# (det sker ofte når en join kun matcher for nogle rækker)
na_frac <- sapply(df, function(x) mean(is.na(x)))
df <- df[, na_frac < 0.95]   # behold kun kolonner med <95% NA

# Vi sørger for at res er ensartet og udtrækker mål (hvis res findes)
# Eksempel: "3 - 1" eller "3-1" -> "3-1" og så to nye numeric kolonner
if ("res" %in% names(df)) {
  df <- df %>%
    mutate(
      res = str_replace_all(res, "\\s+", ""),  # "3 - 1" -> "3-1"
      vff_maal_from_res = as.integer(str_extract(res, "^[0-9]+")),
      modstander_maal_from_res = as.integer(str_extract(res, "(?<=-)[0-9]+"))
    )
}

# Fjern ubrugelig x7-kolonne (tom/junk fra webscrape)
# Vi gør det robust: slet den kun hvis den findes
if ("x7" %in% names(df)) {
  df <- df %>% select(-x7)
}

# Overblik efter oprydning
dplyr::glimpse(df)


# -----------------------------
# Gem datasættet (så du kan uploade det her)
# -----------------------------

# Gem som RDS (bevarer datatyper som Date/factor)
saveRDS(df, "df_model_ready.rds")

# Gem som CSV (nem at dele, men Date/factor bliver til tekst)
readr::write_csv(df, "df_model_ready.csv")

