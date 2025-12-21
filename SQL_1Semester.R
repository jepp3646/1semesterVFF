
# -----------------------------
# Forbind til SQLite Database
# -----------------------------

# Liste over alle pakker
packages <- c(
  "tidyverse", "palmerpenguins", "ggthemes", "ggridges", "nycflights13",
  "Lahman", "janitor", "vroom", "arrow", "readxl", "writexl", "googlesheets4",
  "RSQLite", "babynames", "stringi", "hms", "tidymodels", "fable", "car", "MASS",
  "lmtest", "tseries", "ggfortify", "polite", "tufte", "rvest", "xml2", "dplyr","stringr","DBI"
)

library(tidyverse)
library(rvest)
library(httr)
library(dplyr)
library(stringr)
library(DBI)
library(RSQLite)



con <- dbConnect(
  SQLite(),
  dbname = "VffDataBaseNew.db"
)

on.exit(DBI::dbDisconnect(con), add = TRUE)

# Load vores tabeller ind:

vffkort01 <- readRDS("vffkort01.rds")
fcidk <- readRDS("fcidk.rds")
vff_hjemmekampe_med_vejr <- readRDS("vff_hjemmekampe_med_vejr.rds")


dbWriteTable(con, "st_webscrabe", st_webscrabe, overwrite = TRUE)
dbWriteTable(con, "vffkort01", vffkort01, overwrite = TRUE)
dbWriteTable(con, "vff_hjemmekampe_med_vejr", vff_hjemmekampe_med_vejr, overwrite = TRUE)
dbWriteTable(con, "fcidk", fcidk, overwrite = TRUE)

# -----------------------------
# Join Tabeller
# -----------------------------

#Join st_webscrabe og dmi tabel (vff_hjemmekampe_med_vejr) 

joined_st_webscrabe_dmi <- dbGetQuery(con, "
  SELECT *
  FROM st_webscrabe
  INNER JOIN vff_hjemmekampe_med_vejr
    ON st_webscrabe.Dato = vff_hjemmekampe_med_vejr.dato_index
  ORDER BY st_webscrabe.Dato DESC
")

view(joined_st_webscrabe_dmi)

#Da der laves udtræk fra SQL, så bliver "Dato" værdien retuneret som numeric, og det skal vi lave om til Date værdi igen

joined_st_webscrabe_dmi$Dato <- as.Date(joined_st_webscrabe_dmi$Dato, origin = "1970-01-01")

#Se hvilken værdi Dato har
class(joined_st_webscrabe_dmi$Dato)

#Join st_webscrabe og fcidk 

joined_st_webscrabe_fcidk <- dbGetQuery(con, "
  SELECT
      s.*,
      f.tilskuere AS modstander_gns_tilskuere
  FROM st_webscrabe AS s
  LEFT JOIN fcidk AS f
      ON s.Modstander = f.kort
")

view(joined_st_webscrabe_fcidk)

dbListTables(con)
names(vffkort01)
names(st_webscrabe)

#Join st_webscrabe med vfkort01 

joined_st_webscrabe_vfkort01 <- dbGetQuery(con, "
SELECT
st_webscrabe.*,
vffkort01.*
  FROM st_webscrabe
LEFT JOIN vffkort01
ON st_webscrabe.Season = vffkort01.sæson
AND st_webscrabe.Rnd    = vffkort01.runde
")

view(joined_st_webscrabe_vfkort01)

#Nu vil vi lave en stor tabel

DBI::dbExecute(con, "
CREATE TABLE model_dataset AS
SELECT
  s.*,
  v.tilskuere     AS vffkort01_tilskuere,
  v.år            AS vffkort01_år,
  v.hold          AS vffkort01_hold,
  v.d10_tilskuere AS vffkort01_d10_tilskuere,
  v.d7_tilskuere  AS vffkort01_d7_tilskuere,
  v.d3_tilskuere  AS vffkort01_d3_tilskuere,
  f.tilskuere     AS modstander_gns_tilskuere,
  d.temp_kamp,
  d.regn_dag,
  d.vind_1h
FROM st_webscrabe AS s
LEFT JOIN vffkort01 AS v
  ON s.Season = v.`sæson`
 AND s.Rnd    = v.`runde`
LEFT JOIN fcidk AS f
  ON s.Modstander = f.kort
LEFT JOIN vff_hjemmekampe_med_vejr AS d
  ON s.Dato = d.dato_index;
")

DBI::dbListTables(con)
DBI::dbListFields(con, "vff_hjemmekampe_med_vejr")

model_dataset <- DBI::dbReadTable(con, "model_dataset")
View(model_dataset)


### Next Step Download joinet datasæt, formatér behørigt, og gør data klar til preprocessing





DBI::dbDisconnect(con)




