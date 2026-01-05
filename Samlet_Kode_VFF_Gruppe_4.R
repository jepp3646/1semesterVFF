# -----------------------------
# Forbind til SQLite Database
# -----------------------------

# Liste over alle pakker (dokumentation: hvad projektet bruger)
packages <- c(
  "tidyverse", "palmerpenguins", "ggthemes", "ggridges", "nycflights13",
  "Lahman", "janitor", "vroom", "arrow", "readxl", "writexl", "googlesheets4",
  "RSQLite", "babynames", "stringi", "hms", "tidymodels", "fable", "car", "MASS",
  "lmtest", "tseries", "ggfortify", "polite", "tufte", "rvest", "xml2",
  "dplyr", "stringr", "DBI", "lubridate","leaps"
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
library(leaps)


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
st_webscrabe <- readRDS("st_webscrabe.rds")
vffkort01 <- readRDS("vffkort01.rds")
fcidk <- readRDS("fcidk.rds")
vff_hjemmekampe_med_vejr <- readRDS("vff_hjemmekampe_med_vejr.rds")

# Skriv (eller overskriv) tabellerne i databasen, så DB altid er opdateret
dbWriteTable(con, "st_webscrabe", st_webscrabe, overwrite = TRUE)
dbWriteTable(con, "vffkort01", vffkort01, overwrite = TRUE)
dbWriteTable(con, "vff_hjemmekampe_med_vejr", vff_hjemmekampe_med_vejr, overwrite = TRUE)
dbWriteTable(con, "fcidk", fcidk, overwrite = TRUE)


# -----------------------------
# Join Tabeller 
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


# Vi gemmer datasættetet 
# saveRDS(df, "df_model_ready.rds")


df <- df %>%
  mutate(
    # Hvilken ugedag spilles kampen?
    weekday = wday(dato, label = TRUE),
    
    # Er det weekend?
    is_weekend = weekday %in% c("Sat", "Sun"),
    
    # Måneden (sæson-effekt)
    month = month(dato),
    
    # Mål-forskel (kampens sportslige betydning)
    maal_diff = vff_mal - modstander_mal
  )

#Nu laver vi fire forskellige splits, da vi skal lave modeller for hhv. 3d,7d,10d og 2 måneder

#2 Måneder før - Dataframe: Vi laver et dataframe, hvor udvalgte variabler er med. De er udvalgt udfra hvoad der giver mening, at vi ved 2 måneder før. 
df_2m <- df %>%
  select(
    tilskuere,                 # TARGET (det vi forudsiger)
    season,
    modstander,
    kategori,
    mean_tilskuere,
    mean_tilskuere_saeson,
    weekday,
    is_holiday,
  ) %>%
  drop_na() #Bruger kun rækker, hvor der ikke er missing value (NA)


#10 dage før - Dataframe: 
df_10d <- df %>%
  select(
    tilskuere,
    season,
    rnd,
    modstander,
    kategori,
    mean_tilskuere,
    mean_tilskuere_saeson,
    weekday,
    is_holiday,
    kamp_tid,
    kickoff_hour,
    d10_tilskuere
  ) %>%
  drop_na()

#7 dage før - Dataframe:
df_7d <- df %>%
  select(
    tilskuere,
    season,
    rnd,
    modstander,
    kategori,
    mean_tilskuere,
    mean_tilskuere_saeson,
    weekday,
    is_holiday,
    kamp_tid,
    kickoff_hour,
    regn_dag_kat,
    temp_kamp_kat,
    vind_1h_kat,
    d10_tilskuere,
    d7_tilskuere
  ) %>%
  drop_na()

#3 dage før - Dataframe:
df_3d <- df %>%
  select(
    tilskuere,
    season,
    rnd,
    modstander,
    kategori,
    mean_tilskuere,
    mean_tilskuere_saeson,
    weekday,
    is_holiday,
    kamp_tid,
    kickoff_hour,
    temp_kamp,
    regn_dag,
    vind_1h,
    d10_tilskuere,
    d7_tilskuere,
    d3_tilskuere
  ) %>%
  drop_na()


# ==================
# Modellering
# ==================

# Vi Laver et fælles "setup" til 2m, 10d, 7d og 3d, så vi kan genanvende objektekterne

# --------------------
# 2m fælles før setup
# --------------------

# X og y (bruges af ALLE modeller)
x_2m <- model.matrix(tilskuere ~ ., df_2m)[, -1]
y_2m <- df_2m$tilskuere

# 80/20 split
set.seed(4)
n <- nrow(x_2m)
train_2m <- sample(1:n, size = floor(0.8 * n))
test_2m  <- setdiff(1:n, train_2m)

y_test_2m <- y_2m[test_2m]

# --------------------
# 10d fælles før setup
# ---------------------

# X og y (bruges af ALLE modeller)
x_10d <- model.matrix(tilskuere ~ ., df_10d)[, -1]
y_10d <- df_10d$tilskuere

# 80/20 split
set.seed(4)
n <- nrow(x_10d)
train_10d <- sample(1:n, size = floor(0.8 * n))
test_10d  <- setdiff(1:n, train_10d)

y_test_10d <- y_10d[test_10d]

# -------------------
# 7d fælles før setup
# -------------------

# X og y (bruges af ALLE modeller)
x_7d <- model.matrix(tilskuere ~ ., df_7d)[, -1]
y_7d <- df_7d$tilskuere

# 80/20 split
set.seed(4)
n <- nrow(x_7d)
train_7d <- sample(1:n, size = floor(0.8 * n))
test_7d  <- setdiff(1:n, train_7d)

y_test_7d <- y_7d[test_7d]


# -------------------
# 3d fælles før setup
# -------------------


# X og y (bruges af ALLE modeller)
x_3d <- model.matrix(tilskuere ~ ., df_3d)[, -1]
y_3d <- df_3d$tilskuere

# 80/20 split
set.seed(4)
n <- nrow(x_3d)
train_3d <- sample(1:n, size = floor(0.8 * n))
test_3d  <- setdiff(1:n, train_3d)

y_test_3d <- y_3d[test_3d]


# -------------------------------------------------------------
# Modellering Lineær Regression (STOR MODEL) - 2 måneder før
# -------------------------------------------------------------

# STOR model: vi bruger alle variabler der findes i df_2m (undtagen tilskuere).
# Denne burde vi kunne sammenligne med Ridge/Lasso/subset senere.
lm_2m_big <- lm(tilskuere ~ ., data = df_2m[train_2m, ])

# summary viser koefficienter, p-værdier og forkaringsgrad R^2.
summary(lm_2m_big)

# -------------------------------------------------------------
# Modellering Lineær Regression (STOR MODEL) - 10 dage før
# -------------------------------------------------------------

# Samme metode som før, bare med datasættet for 10 dage før kamp.


# STOR model: bruger alle variabler som vi har i df_10d.
lm_10d_big <- lm(tilskuere ~ ., data = df_10d[train_10d, ])
summary(lm_10d_big)


# -------------------------------------------------------------
# Modellering Lineær Regression (STOR MODEL) - 7 dage før
# -------------------------------------------------------------

# Vi gør det samme for 7 dage før.

# STOR model: alle variabler i df_7d.
lm_7d_big <- lm(tilskuere ~ ., data = df_7d[train_7d, ])
summary(lm_7d_big)

# -------------------------------------------------------------
# Modellering Lineær Regression (STOR MODEL) - 3 dage før
# -------------------------------------------------------------

# Til sidst gør vi det samme for 3 dage før kamp.

# STOR model: alle variabler i df_3d.
lm_3d_big <- lm(tilskuere ~ ., data = df_3d[train_3d, ])
summary(lm_3d_big)


# -------------------------------------------------
# Test-RMSE for stor lineær regression (2 måneder før)
# -------------------------------------------------

# Vi kan få fejl hvis test indeholder modstandere, som ikke findes i træningsdata.
# Derfor laver vi et nyt testdatasæt og filtrerer "ukendte" modstandere fra.

# Lav testdata
test_df_2m <- df_2m[test_2m, ]

# Fjern modstandere som ikke findes i træning
test_df_2m <- test_df_2m %>%
  dplyr::filter(modstander %in% df_2m$modstander[train_2m])

# Forudsig på det filtrerede test-sæt
pred_lm_2m <- predict(lm_2m_big, newdata = test_df_2m)

# Beregn RMSE
rmse_lm_test_2m <- sqrt(mean((test_df_2m$tilskuere - pred_lm_2m)^2))

rmse_lm_test_2m

# -------------------------------------------------
# Test-RMSE for stor lineær regression (10 dage før)
# -------------------------------------------------

# Samme problem kan opstå her: test kan have nye factor-levels (modstander).
# Derfor filtrerer vi test, så den kun indeholder modstandere fra træning.

# Lav testdata
test_df_10d <- df_10d[test_10d, ]

# Fjern modstandere som ikke findes i træning
test_df_10d <- test_df_10d %>%
  dplyr::filter(modstander %in% df_10d$modstander[train_10d])

# Forudsig på det filtrerede test-sæt
pred_lm_10d <- predict(lm_10d_big, newdata = test_df_10d)

# Beregn RMSE
rmse_lm_test_10d <- sqrt(mean((test_df_10d$tilskuere - pred_lm_10d)^2))

rmse_lm_test_10d

# -------------------------------------------------
# Test-RMSE for stor lineær regression (7 dage før)
# -------------------------------------------------

# Igen: Hvis test indeholder en modstander som ikke var i træningsdata,
# kan predict() ikke danne de rigtige dummy-variable og giver fejl.
# Derfor filtrerer vi test-sættet.

# Lav testdata
test_df_7d <- df_7d[test_7d, ]

# Fjern modstandere som ikke findes i træning
test_df_7d <- test_df_7d %>%
  dplyr::filter(modstander %in% df_7d$modstander[train_7d])

# Forudsig på det filtrerede test-sæt
pred_lm_7d <- predict(lm_7d_big, newdata = test_df_7d)

# Beregn RMSE
rmse_lm_test_7d <- sqrt(mean((test_df_7d$tilskuere - pred_lm_7d)^2))

rmse_lm_test_7d



# -------------------------------------------------
# Test-RMSE for stor lineær regression (3 dage før)
# -------------------------------------------------

#Det giver en fejl, da AB (holdet) ikke er med i testdataen og kun i trænings,
#Så Vi skal lige manuelt fjerne dem / lave et nyt test datasæt, som er ens med det andet uden AB dog

# Lav testdata
test_df_3d <- df_3d[test_3d, ]

# Fjern modstandere som ikke findes i træning (AB)
test_df_3d <- test_df_3d %>%
  dplyr::filter(modstander %in% df_3d$modstander[train_3d])

# Forudsig antal tilskuere på testdatasættet
pred_lm_3d <- predict(lm_3d_big, newdata = test_df_3d)

# Beregn RMSE
rmse_lm_test_3d <- sqrt(mean((test_df_3d$tilskuere - pred_lm_3d)^2))

rmse_lm_test_3d

# Vi laver en opsummering af RSME for alle fire splits

rmse_lm_overview <- data.frame(
  Tid = c("2 måneder", "10 dage", "7 dage", "3 dage"),
  RMSE_test_lm = c(rmse_lm_test_2m, rmse_lm_test_10d, rmse_lm_test_7d, rmse_lm_test_3d)
)

rmse_lm_overview


# ==================================
# Modellering Lasso 
# ==================================
library(glmnet)

# Vi bruger samme lambda-grid til alle Lasso-modeller
grid <- 10^seq(10, -2, length.out = 100)

# -----------------------------------
# Modellering Lasso - 2 måneder før
# -----------------------------------

# Vi estimerer en Lasso-model på træningsdata.
# alpha = 1 betyder Lasso (L1-regularisering),
# som både reducerer overfitting og kan sætte koefficienter lig 0.
lasso_2m <- glmnet(
  x_2m[train_2m, ],
  y_2m[train_2m],
  alpha  = 1,
  lambda = grid,
  thresh = 1e-12
)

# Cross-validation bruges til at finde den bedste lambda-værdi.
# Seed sikrer, at fold-opdelingen i CV er den samme hver gang.
set.seed(4)
lasso_cv_2m <- cv.glmnet(
  x_2m[train_2m, ],
  y_2m[train_2m],
  alpha  = 1,
  lambda = grid,
  nfolds = 5,
  thresh = 1e-12
)

# Den lambda-værdi som giver lavest CV-fejl
bestlam_lasso_2m <- lasso_cv_2m$lambda.min

# RMSE fra cross-validation (træningsdata)
rmse_lasso_cv_2m <- sqrt(
  lasso_cv_2m$cvm[lasso_cv_2m$lambda == bestlam_lasso_2m]
)

# Forudsigelser på testdatasættet
pred_lasso_2m <- as.numeric(
  predict(lasso_2m, s = bestlam_lasso_2m, newx = x_2m[test_2m, ])
)

# Test-RMSE: gennemsnitlig fejl i antal tilskuere
rmse_lasso_test_2m <- sqrt(mean((pred_lasso_2m - y_test_2m)^2))

bestlam_lasso_2m
rmse_lasso_cv_2m
rmse_lasso_test_2m

# De variabler som Lasso faktisk bruger (koefficient ≠ 0)
coef_lasso_2m <- coef(lasso_cv_2m, s = "lambda.min")
coef_lasso_2m[coef_lasso_2m[,1] != 0, , drop = FALSE]


# -----------------------------------
# Modellering Lasso - 10 dage før
# -----------------------------------

# Lasso-model på træningsdata
lasso_10d <- glmnet(
  x_10d[train_10d, ],
  y_10d[train_10d],
  alpha  = 1,
  lambda = grid,
  thresh = 1e-12
)

# Cross-validation til valg af lambda
set.seed(4)
lasso_cv_10d <- cv.glmnet(
  x_10d[train_10d, ],
  y_10d[train_10d],
  alpha  = 1,
  lambda = grid,
  nfolds = 5,
  thresh = 1e-12
)

bestlam_lasso_10d <- lasso_cv_10d$lambda.min

# CV-RMSE
rmse_lasso_cv_10d <- sqrt(
  lasso_cv_10d$cvm[lasso_cv_10d$lambda == bestlam_lasso_10d]
)

# Test-RMSE
pred_lasso_10d <- as.numeric(
  predict(lasso_10d, s = bestlam_lasso_10d, newx = x_10d[test_10d, ])
)

rmse_lasso_test_10d <- sqrt(mean((pred_lasso_10d - y_test_10d)^2))

bestlam_lasso_10d
rmse_lasso_cv_10d
rmse_lasso_test_10d

coef_lasso_10d <- coef(lasso_cv_10d, s = "lambda.min")
coef_lasso_10d[coef_lasso_10d[,1] != 0, , drop = FALSE]

# -----------------------------------
# Modellering Lasso - 7 dage før
# -----------------------------------

lasso_7d <- glmnet(
  x_7d[train_7d, ],
  y_7d[train_7d],
  alpha  = 1,
  lambda = grid,
  thresh = 1e-12
)

set.seed(4)
lasso_cv_7d <- cv.glmnet(
  x_7d[train_7d, ],
  y_7d[train_7d],
  alpha  = 1,
  lambda = grid,
  nfolds = 5,
  thresh = 1e-12
)

bestlam_lasso_7d <- lasso_cv_7d$lambda.min

rmse_lasso_cv_7d <- sqrt(
  lasso_cv_7d$cvm[lasso_cv_7d$lambda == bestlam_lasso_7d]
)

pred_lasso_7d <- as.numeric(
  predict(lasso_7d, s = bestlam_lasso_7d, newx = x_7d[test_7d, ])
)

rmse_lasso_test_7d <- sqrt(mean((pred_lasso_7d - y_test_7d)^2))

bestlam_lasso_7d
rmse_lasso_cv_7d
rmse_lasso_test_7d

coef_lasso_7d <- coef(lasso_cv_7d, s = "lambda.min")
coef_lasso_7d[coef_lasso_7d[,1] != 0, , drop = FALSE]

# -----------------------------------
# Modellering Lasso - 3 dage før
# -----------------------------------

lasso_3d <- glmnet(
  x_3d[train_3d, ],
  y_3d[train_3d],
  alpha  = 1,
  lambda = grid,
  thresh = 1e-12
)

set.seed(4)
lasso_cv_3d <- cv.glmnet(
  x_3d[train_3d, ],
  y_3d[train_3d],
  alpha  = 1,
  lambda = grid,
  nfolds = 5,
  thresh = 1e-12
)

bestlam_lasso_3d <- lasso_cv_3d$lambda.min

rmse_lasso_cv_3d <- sqrt(
  lasso_cv_3d$cvm[lasso_cv_3d$lambda == bestlam_lasso_3d]
)

pred_lasso_3d <- as.numeric(
  predict(lasso_3d, s = bestlam_lasso_3d, newx = x_3d[test_3d, ])
)

rmse_lasso_test_3d <- sqrt(mean((pred_lasso_3d - y_test_3d)^2))

bestlam_lasso_3d
rmse_lasso_cv_3d
rmse_lasso_test_3d

coef_lasso_3d <- coef(lasso_cv_3d, s = "lambda.min")
coef_lasso_3d[coef_lasso_3d[,1] != 0, , drop = FALSE]


# Vi laver en opsummering af RSME for alle fire splits

rmse_lasso_overview <- data.frame(
  Tid = c("2 måneder", "10 dage", "7 dage", "3 dage"),
  RMSE_test_lasso = c(rmse_lasso_test_2m, rmse_lasso_test_10d, rmse_lasso_test_7d, rmse_lasso_test_3d)
)

# Her kan de forskellige RSME ses for 2m, 10d, 7d, 3d
rmse_lasso_overview



# ==================
# Ridge regression 
# ==================


library(glmnet)
set.seed(4)

# Vi laver et grid af mulige lambda-værdier (fra meget stor til lille)
grid <- 10^seq(10, -2, length.out = 100)

# ---------------------------------
# Modellering Ridge - 2 Måneder Før
# ---------------------------------

# Vi træner en Ridge-model på træningsdata.
ridge_2m <- glmnet(
  x_2m[train_2m, ],
  y_2m[train_2m],
  alpha  = 0,
  lambda = grid,
  thresh = 1e-12
)

# Cross-validation: finder den lambda der giver mindst fejl på træningsdata.
set.seed(4)
ridge_cv_2m <- cv.glmnet(
  x_2m[train_2m, ],
  y_2m[train_2m],
  alpha  = 0,
  lambda = grid,
  nfolds = 5,
  thresh = 1e-12
)

# Den bedste lambda fra CV (den som giver lavest CV-fejl)
bestlam_ridge_2m <- ridge_cv_2m$lambda.min

# CV-RMSE (træningsdata via cross-validation)
rmse_ridge_cv_2m <- sqrt(
  ridge_cv_2m$cvm[ridge_cv_2m$lambda == bestlam_ridge_2m]
)

# Forudsigelser på testdata med den bedste lambda
pred_ridge_2m <- as.numeric(
  predict(ridge_2m, s = bestlam_ridge_2m, newx = x_2m[test_2m, ])
)

# Test-RMSE: gennemsnitlig fejl i antal tilskuere på testdatasættet
rmse_ridge_test_2m <- sqrt(mean((pred_ridge_2m - y_test_2m)^2))

bestlam_ridge_2m
rmse_ridge_cv_2m
rmse_ridge_test_2m

# ==================================================
# Ridge - 10 dage før
# ==================================================

ridge_10d <- glmnet(
  x_10d[train_10d, ],
  y_10d[train_10d],
  alpha  = 0,
  lambda = grid,
  thresh = 1e-12
)

set.seed(4)
ridge_cv_10d <- cv.glmnet(
  x_10d[train_10d, ],
  y_10d[train_10d],
  alpha  = 0,
  lambda = grid,
  nfolds = 5,
  thresh = 1e-12
)

bestlam_ridge_10d <- ridge_cv_10d$lambda.min

rmse_ridge_cv_10d <- sqrt(
  ridge_cv_10d$cvm[ridge_cv_10d$lambda == bestlam_ridge_10d]
)

pred_ridge_10d <- as.numeric(
  predict(ridge_10d, s = bestlam_ridge_10d, newx = x_10d[test_10d, ])
)

rmse_ridge_test_10d <- sqrt(mean((pred_ridge_10d - y_test_10d)^2))

bestlam_ridge_10d
rmse_ridge_cv_10d
rmse_ridge_test_10d


# ==================================================
# Ridge - 7 dage før
# ==================================================

ridge_7d <- glmnet(
  x_7d[train_7d, ],
  y_7d[train_7d],
  alpha  = 0,
  lambda = grid,
  thresh = 1e-12
)

set.seed(4)
ridge_cv_7d <- cv.glmnet(
  x_7d[train_7d, ],
  y_7d[train_7d],
  alpha  = 0,
  lambda = grid,
  nfolds = 5,
  thresh = 1e-12
)

bestlam_ridge_7d <- ridge_cv_7d$lambda.min

rmse_ridge_cv_7d <- sqrt(
  ridge_cv_7d$cvm[ridge_cv_7d$lambda == bestlam_ridge_7d]
)

pred_ridge_7d <- as.numeric(
  predict(ridge_7d, s = bestlam_ridge_7d, newx = x_7d[test_7d, ])
)

rmse_ridge_test_7d <- sqrt(mean((pred_ridge_7d - y_test_7d)^2))

bestlam_ridge_7d
rmse_ridge_cv_7d
rmse_ridge_test_7d


# ==================================================
# Ridge - 3 dage før
# ==================================================

ridge_3d <- glmnet(
  x_3d[train_3d, ],
  y_3d[train_3d],
  alpha  = 0,
  lambda = grid,
  thresh = 1e-12
)

set.seed(4)
ridge_cv_3d <- cv.glmnet(
  x_3d[train_3d, ],
  y_3d[train_3d],
  alpha  = 0,
  lambda = grid,
  nfolds = 5,
  thresh = 1e-12
)

bestlam_ridge_3d <- ridge_cv_3d$lambda.min

rmse_ridge_cv_3d <- sqrt(
  ridge_cv_3d$cvm[ridge_cv_3d$lambda == bestlam_ridge_3d]
)

pred_ridge_3d <- as.numeric(
  predict(ridge_3d, s = bestlam_ridge_3d, newx = x_3d[test_3d, ])
)

rmse_ridge_test_3d <- sqrt(mean((pred_ridge_3d - y_test_3d)^2))

bestlam_ridge_3d
rmse_ridge_cv_3d
rmse_ridge_test_3d


# ----------------------------------------------------
# Samlet oversigt over Ridge (Test-RMSE)
# ----------------------------------------------------

rmse_ridge_overview <- data.frame(
  Tid = c("2 måneder", "10 dage", "7 dage", "3 dage"),
  RMSE_test_ridge = c(
    rmse_ridge_test_2m,
    rmse_ridge_test_10d,
    rmse_ridge_test_7d,
    rmse_ridge_test_3d
  )
)

rmse_ridge_overview

# =========================
# Best subset selection 
# =========================

# regsubsets() har ikke en predict()-funktion indbygget, så vi laver en selv.
# Ideen er:
# 1) Lav samme model-matrix (dummy-variabler osv.) som ved træning
# 2) Hent koefficienter for den modelstørrelse vi vil bruge (id)
# 3) Gang X-matrix med beta-vektoren -> så får vi prediction
predict.regsubsets <- function(object, newdata, id, ...) {
  form  <- as.formula(object$call[[2]])
  mat   <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  
  # FIX: Hvis et fold/test-sæt ikke indeholder alle factor-levels,
  # kan model.matrix mangle nogle dummy-kolonner. Så crasher mat[, xvars].
  # Derfor tilføjer vi manglende kolonner som 0, så dimensionerne passer.
  missing_vars <- setdiff(xvars, colnames(mat))
  if (length(missing_vars) > 0) {
    missing_mat <- matrix(0, nrow = nrow(mat), ncol = length(missing_vars))
    colnames(missing_mat) <- missing_vars
    mat <- cbind(mat, missing_mat)
  }
  
  # Til sidst tager vi kun de kolonner modellen forventer (i samme rækkefølge)
  mat <- mat[, xvars, drop = FALSE]
  
  # Prediction = X * beta
  mat %*% coefi
}

# ------------------------------------------------
# Best Subset: 2 måneder før (bruger 80/20 fra setup)
# ------------------------------------------------

# Vi tager træning og test fra de fælles setup-indeks (train_2m / test_2m)
vff_train <- df_2m[train_2m, ]
vff_test  <- df_2m[test_2m, ]

# Cross-validation setup:
# k = antal folds, n = antal observationer i træningsdata
k <- 10
n <- nrow(vff_train)

set.seed(4)  # styrer fold-inddelingen (så vi får samme CV hver gang)
folds <- sample(rep(1:k, length = n))

# Matrix til at gemme CV-fejl (MSE) for hver fold og hver modelstørrelse
# Antal modelstørrelser = antal variabler - 1 (fordi tilskuere er target)
cv.errors <- matrix(
  NA, k, dim(vff_train)[2] - 1,
  dimnames = list(NULL, paste(1:(dim(vff_train)[2] - 1)))
)

# Vi gennemløber alle folds:
# - træner på k-1 folds
# - validerer på fold j
for (j in 1:k) {
  
  # Forward selection: modellen bygger variabler på én ad gangen
  best.fit <- regsubsets(
    tilskuere ~ .,
    data   = vff_train[folds != j, ],
    nvmax  = dim(vff_train)[2] - 1,
    method = "forward"
  )
  
  # For hver mulig modelstørrelse (1..p) udregner vi MSE i fold j
  for (i in 1:(dim(vff_train)[2] - 1)) {
    pred <- predict(best.fit, vff_train[folds == j, ], id = i)
    cv.errors[j, i] <- mean((vff_train$tilskuere[folds == j] - pred)^2)
  }
}

# Gennemsnitlig CV-MSE for hver modelstørrelse
mean.cv.errors <- apply(cv.errors, 2, mean)

# Vælg modelstørrelsen med lavest CV-MSE
best_size_2m <- which.min(mean.cv.errors)

# Fit endelig model på ALLE træningsdata (så koefficienter estimeres på mest data)
reg.best <- regsubsets(
  tilskuere ~ .,
  data   = vff_train,
  nvmax  = dim(vff_train)[2] - 1,
  method = "forward"
)

# Prediction på test-sættet med den valgte modelstørrelse
pred_best_subset <- predict(reg.best, vff_test, id = best_size_2m)

# Test-fejl (RMSE)
mse_best_subset <- mean((vff_test$tilskuere - pred_best_subset)^2)
rmse_bestsubset_test_2m <- sqrt(mse_best_subset)

# CV-RMSE (kvadratrod af den mindste CV-MSE)
rmse_bestsubset_cv_2m <- sqrt(min(mean.cv.errors))

# Output
best_size_2m
rmse_bestsubset_cv_2m
rmse_bestsubset_test_2m
coef(reg.best, best_size_2m)


# ------------------------------------------------
# Best Subset: 10 dage før (bruger 80/20 fra setup)
# ------------------------------------------------

vff_train <- df_10d[train_10d, ]
vff_test  <- df_10d[test_10d, ]

k <- 10
n <- nrow(vff_train)

set.seed(4)
folds <- sample(rep(1:k, length = n))

cv.errors <- matrix(
  NA, k, dim(vff_train)[2] - 1,
  dimnames = list(NULL, paste(1:(dim(vff_train)[2] - 1)))
)

for (j in 1:k) {
  
  best.fit <- regsubsets(
    tilskuere ~ .,
    data   = vff_train[folds != j, ],
    nvmax  = dim(vff_train)[2] - 1,
    method = "forward"
  )
  
  for (i in 1:(dim(vff_train)[2] - 1)) {
    pred <- predict(best.fit, vff_train[folds == j, ], id = i)
    cv.errors[j, i] <- mean((vff_train$tilskuere[folds == j] - pred)^2)
  }
}

mean.cv.errors <- apply(cv.errors, 2, mean)
best_size_10d <- which.min(mean.cv.errors)

reg.best <- regsubsets(
  tilskuere ~ .,
  data   = vff_train,
  nvmax  = dim(vff_train)[2] - 1,
  method = "forward"
)

pred_best_subset <- predict(reg.best, vff_test, id = best_size_10d)

mse_best_subset <- mean((vff_test$tilskuere - pred_best_subset)^2)
rmse_bestsubset_test_10d <- sqrt(mse_best_subset)
rmse_bestsubset_cv_10d <- sqrt(min(mean.cv.errors))

best_size_10d
rmse_bestsubset_cv_10d
rmse_bestsubset_test_10d
coef(reg.best, best_size_10d)


# ------------------------------------------------
# Best Subset: 7 dage før (bruger 80/20 fra setup)
# ------------------------------------------------

vff_train <- df_7d[train_7d, ]
vff_test  <- df_7d[test_7d, ]

k <- 10
n <- nrow(vff_train)

set.seed(4)
folds <- sample(rep(1:k, length = n))

cv.errors <- matrix(
  NA, k, dim(vff_train)[2] - 1,
  dimnames = list(NULL, paste(1:(dim(vff_train)[2] - 1)))
)

for (j in 1:k) {
  
  best.fit <- regsubsets(
    tilskuere ~ .,
    data   = vff_train[folds != j, ],
    nvmax  = dim(vff_train)[2] - 1,
    method = "forward"
  )
  
  for (i in 1:(dim(vff_train)[2] - 1)) {
    pred <- predict(best.fit, vff_train[folds == j, ], id = i)
    cv.errors[j, i] <- mean((vff_train$tilskuere[folds == j] - pred)^2)
  }
}

mean.cv.errors <- apply(cv.errors, 2, mean)
best_size_7d <- which.min(mean.cv.errors)

reg.best <- regsubsets(
  tilskuere ~ .,
  data   = vff_train,
  nvmax  = dim(vff_train)[2] - 1,
  method = "forward"
)

pred_best_subset <- predict(reg.best, vff_test, id = best_size_7d)

mse_best_subset <- mean((vff_test$tilskuere - pred_best_subset)^2)
rmse_bestsubset_test_7d <- sqrt(mse_best_subset)
rmse_bestsubset_cv_7d <- sqrt(min(mean.cv.errors))

best_size_7d
rmse_bestsubset_cv_7d
rmse_bestsubset_test_7d
coef(reg.best, best_size_7d)


# ------------------------------------------------
# Best Subset: 3 dage før (bruger 80/20 fra setup)
# ------------------------------------------------

vff_train <- df_3d[train_3d, ]
vff_test  <- df_3d[test_3d, ]

k <- 10
n <- nrow(vff_train)

set.seed(4)
folds <- sample(rep(1:k, length = n))

cv.errors <- matrix(
  NA, k, dim(vff_train)[2] - 1,
  dimnames = list(NULL, paste(1:(dim(vff_train)[2] - 1)))
)

for (j in 1:k) {
  
  best.fit <- regsubsets(
    tilskuere ~ .,
    data   = vff_train[folds != j, ],
    nvmax  = dim(vff_train)[2] - 1,
    method = "forward"
  )
  
  for (i in 1:(dim(vff_train)[2] - 1)) {
    pred <- predict(best.fit, vff_train[folds == j, ], id = i)
    cv.errors[j, i] <- mean((vff_train$tilskuere[folds == j] - pred)^2)
  }
}

mean.cv.errors <- apply(cv.errors, 2, mean)
best_size_3d <- which.min(mean.cv.errors)

reg.best <- regsubsets(
  tilskuere ~ .,
  data   = vff_train,
  nvmax  = dim(vff_train)[2] - 1,
  method = "forward"
)

pred_best_subset <- predict(reg.best, vff_test, id = best_size_3d)

mse_best_subset <- mean((vff_test$tilskuere - pred_best_subset)^2)
rmse_bestsubset_test_3d <- sqrt(mse_best_subset)
rmse_bestsubset_cv_3d <- sqrt(min(mean.cv.errors))

best_size_3d
rmse_bestsubset_cv_3d
rmse_bestsubset_test_3d
coef(reg.best, best_size_3d)



#==================================
# Sammenligning af RMSE Test og CV 
#==================================

# Vi bruger RSME i stedet for MSE for at give et lettere fortolkende tal. 
# Herefter kan vi sammenligne dem med hinanden for at finde den model med bedst prædiktionsevne. 

#RSME TEST 2 måneder
rmse_bestsubset_test_2m
rmse_ridge_test_2m
rmse_lasso_test_2m

#RSME Træning 2 måneder
rmse_ridge_cv_2m
rmse_bestsubset_cv_2m
rmse_lasso_cv_2m


#RSME TEST 10 dage
rmse_bestsubset_test_10d
rmse_ridge_test_10d
rmse_lasso_test_10d

#RSME Træning 10 dage
rmse_ridge_cv_10d
rmse_bestsubset_cv_10d
rmse_lasso_cv_10d



#RSME TEST 7 dage
rmse_bestsubset_test_7d
rmse_ridge_test_7d
rmse_lasso_test_7d

#RSME Træning 7 dage
rmse_ridge_cv_7d
rmse_bestsubset_cv_7d
rmse_lasso_cv_7d


#RSME TEST 3 dage
rmse_bestsubset_test_3d
rmse_ridge_test_3d
rmse_lasso_test_3d

#RSME Træning 3 dage
rmse_ridge_cv_3d
rmse_bestsubset_cv_3d
rmse_lasso_cv_3d



# -------------------------------------------------------------
# Vi laver et GGplot af RSME for modellerne (Kun for 3dage før)
# -------------------------------------------------------------

library(ggplot2)

rmse_data_3d <- data.frame(
  Model = c("Best Subset", "Lasso", "Ridge","LM"),
  RMSE = c(
    rmse_bestsubset_test_3d,
    rmse_lasso_test_3d,
    rmse_ridge_test_3d,
    rmse_lm_test_3d
  )
)

ggplot(rmse_data_3d, aes(x = Model, y = RMSE, fill = Model)) +
  geom_col(color = "black") +
  theme_minimal() +
  labs(
    title = "Test-RMSE for modeller (3 dage før kamp)",
    x = "Model",
    y = "RMSE (tilskuere)"
  ) +
  theme(legend.position = "none")


# Nu laver vi et samlet plot med de forskellige RMSE ift tid før kamp

library(ggplot2)

rmse_over_time <- data.frame(
  Tid = factor(
    c("2 måneder", "10 dage", "7 dage", "3 dage"),
    levels = c("2 måneder", "10 dage", "7 dage", "3 dage")
  ),
  LM = c(
    rmse_lm_test_2m,
    rmse_lm_test_10d,
    rmse_lm_test_7d,
    rmse_lm_test_3d
  ),
  BestSubset = c(
    rmse_bestsubset_test_2m,
    rmse_bestsubset_test_10d,
    rmse_bestsubset_test_7d,
    rmse_bestsubset_test_3d
  ),
  Lasso = c(
    rmse_lasso_test_2m,
    rmse_lasso_test_10d,
    rmse_lasso_test_7d,
    rmse_lasso_test_3d
  ),
  Ridge = c(
    rmse_ridge_test_2m,
    rmse_ridge_test_10d,
    rmse_ridge_test_7d,
    rmse_ridge_test_3d
  )
)

# Gør data "long" (uden ekstra pakker)
rmse_over_time_long <- data.frame(
  Tid   = rep(rmse_over_time$Tid, times = 4),
  Model = rep(c("LM","Best Subset", "Lasso", "Ridge"), each = nrow(rmse_over_time)),
  RMSE  = c(rmse_over_time$LM,rmse_over_time$BestSubset, rmse_over_time$Lasso, rmse_over_time$Ridge)
)

ggplot(rmse_over_time_long, aes(x = Tid, y = RMSE, group = Model, color = Model)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Test-RMSE som funktion af tid før kamp",
    x = "Tid før kamp",
    y = "RMSE (tilskuere)"
  )


## ============================================================
# Prædiktion på nye data 2 måneder før – Best/Worst case
# ============================================================

# Vi bruger ridge, da den havde lavet test RSME 2 måneder

# Her laver vi to "scenarier”: 1) Best-Case ift tilskuere 2) Worst Case ift tilskuere
# Og så bruger vi vores risge model til at lave en prediction.

# Når vi laver nye data manuelt, kan R ellers finde på at lave factor-levels forkert.
# Derfor tvinger vi værdien til at bruge de samme levels som i df_2m.
as_level <- function(value, ref_factor) {
  factor(value, levels = levels(ref_factor))
}

# ------------------------------------------------------------
# Vi vælger nogle “standard” valg til scenarierne
# ------------------------------------------------------------

# Vi tager den seneste sæson i datasættet (tænkt som “seneste forår” eller nyeste sæson)
season_latest <- levels(df_2m$season)[length(levels(df_2m$season))]

# Vi vælger en modstander som “best” og “worst”.
# Her bruger vi bare rækkefølgen i levels, men man kan selv justere det til noget mere realistisk.
modstander_best  <- levels(df_2m$modstander)[1]
modstander_worst <- levels(df_2m$modstander)[10]

# Vi vælger en standard kategori (igen: kan justeres hvis I har bestemte kategorier)
kategori_default <- levels(df_2m$kategori)[1]

# ------------------------------------------------------------
# Best-case (2 måneder før)
# ------------------------------------------------------------
# Her prøver vi at “skrue op” for de ting der typisk giver flere tilskuere:
# - høje historiske gennemsnit (90%-percentilen)
# - weekendkamp (lørdag)
# - ikke helligdag (kan også sættes TRUE hvis det giver mening i jeres data)
new_best_2m <- data.frame(
  season = as_level(season_latest, df_2m$season),
  modstander = as_level(modstander_best, df_2m$modstander),
  kategori = as_level(kategori_default, df_2m$kategori),
  mean_tilskuere = quantile(df_2m$mean_tilskuere, 0.90, na.rm = TRUE),
  mean_tilskuere_saeson = quantile(df_2m$mean_tilskuere_saeson, 0.90, na.rm = TRUE),
  weekday = as_level("Sat", df_2m$weekday),
  is_holiday = FALSE
)

# ------------------------------------------------------------
# Worst-case (2 måneder før)
# ------------------------------------------------------------
# Her gør vi det modsatte:
# - lave historiske gennemsnit 
# - hverdag (mandag)
new_worst_2m <- data.frame(
  season = as_level(season_latest, df_2m$season),
  modstander = as_level(modstander_worst, df_2m$modstander),
  kategori = as_level(kategori_default, df_2m$kategori),
  mean_tilskuere = quantile(df_2m$mean_tilskuere, 0.10, na.rm = TRUE),
  mean_tilskuere_saeson = quantile(df_2m$mean_tilskuere_saeson, 0.10, na.rm = TRUE),
  weekday = as_level("Mon", df_2m$weekday),
  is_holiday = FALSE
)

# ------------------------------------------------------------
# Predict med Ridge (glmnet)
# ------------------------------------------------------------
# glmnet kræver at nye data laves om til en model.matrix (uden intercept).
# Det er samme “format” som vi trænede modellen på.
x_new_best_2m  <- model.matrix(~ ., new_best_2m)[, -1]
x_new_worst_2m <- model.matrix(~ ., new_worst_2m)[, -1]

# Vi bruger ridge_2m modellen og den bedste lambda (fra CV) til at lave prediction
pred_best_2m  <- as.numeric(predict(ridge_2m, s = bestlam_ridge_2m, newx = x_new_best_2m))
pred_worst_2m <- as.numeric(predict(ridge_2m, s = bestlam_ridge_2m, newx = x_new_worst_2m))

# ------------------------------------------------------------
# Vi samler resultaterne i en lille tabel så det er nemt at vise
# ------------------------------------------------------------
scenario_preds_2m <- data.frame(
  scenario = c("Best-case (2m)", "Worst-case (2m)"),
  predicted_tilskuere = c(pred_best_2m, pred_worst_2m)
)

print(scenario_preds_2m)



# ---------------------------------------
# Output-mapper (opret hvis de ikke findes)
# ---------------------------------------
# dir.create("figures", showWarnings = FALSE)
#dir.create("tables", showWarnings = FALSE)

# -----------------------------
# Plot 1: Test-RMSE (3 dage før)
# -----------------------------
#p_rmse_3d <- ggplot(rmse_data_3d, aes(x = Model, y = RMSE, fill = Model)) +
  #geom_col(color = "black") +
  #theme_minimal() +
  #labs(
    #title = "Test-RMSE for modeller (3 dage før kamp)",
    #x = "Model",
    #y = "RMSE (tilskuere)"
  #) +
  #theme(legend.position = "none")

#ggsave("figures/rmse_3d.png", plot = p_rmse_3d, width = 7, height = 4, dpi = 300)

# ------------------------------------------
# Plot 2: Test-RMSE som funktion af tid før kamp
# ------------------------------------------
#p_rmse_time <- ggplot(rmse_over_time_long, aes(x = Tid, y = RMSE, group = Model, color = Model)) +
 # geom_line(linewidth = 1) +
  #geom_point(size = 2) +
  #theme_minimal() +
  #labs(
   # title = "Test-RMSE som funktion af tid før kamp",
  #  x = "Tid før kamp",
   # y = "RMSE (tilskuere)"
  #)

#ggsave("figures/rmse_over_time.png", plot = p_rmse_time, width = 7, height = 4, dpi = 300)

# -----------------------------
# Tabeller (CSV)
# -----------------------------
#rmse_test_table <- data.frame(
 # Tid = c("2 måneder", "10 dage", "7 dage", "3 dage"),
  #LM = c(rmse_lm_test_2m, rmse_lm_test_10d, rmse_lm_test_7d, rmse_lm_test_3d),
  #Ridge = c(rmse_ridge_test_2m, rmse_ridge_test_10d, rmse_ridge_test_7d, rmse_ridge_test_3d),
  #Lasso = c(rmse_lasso_test_2m, rmse_lasso_test_10d, rmse_lasso_test_7d, rmse_lasso_test_3d),
  #BestSubset = c(rmse_bestsubset_test_2m, rmse_bestsubset_test_10d, rmse_bestsubset_test_7d, rmse_bestsubset_test_3d)
#)
#write.csv(rmse_test_table, "tables/rmse_test_table.csv", row.names = FALSE)

#3rmse_cv_table <- data.frame(
  #Tid = c("2 måneder", "10 dage", "7 dage", "3 dage"),
  #Ridge = c(rmse_ridge_cv_2m, rmse_ridge_cv_10d, rmse_ridge_cv_7d, rmse_ridge_cv_3d),
  #Lasso = c(rmse_lasso_cv_2m, rmse_lasso_cv_10d, rmse_lasso_cv_7d, rmse_lasso_cv_3d),
  #BestSubset = c(rmse_bestsubset_cv_2m, rmse_bestsubset_cv_10d, rmse_bestsubset_cv_7d, rmse_bestsubset_cv_3d)
#)
#write.csv(rmse_cv_table, "tables/rmse_cv_table.csv", row.names = FALSE)

#write.csv(scenario_preds_2m, "tables/scenario_preds_2m.csv", row.names = FALSE)
