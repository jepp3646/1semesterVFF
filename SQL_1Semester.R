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
saveRDS(df, "df_model_ready.rds")


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
    is_weekend,
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
    is_weekend,
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
    is_weekend,
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
    is_weekend,
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

# ---------------------------------------------
# Modellering Lineær Regression - 2 måneder før
# ---------------------------------------------

# Definerer x variabler og y variabel
y_2m <- df_2m$tilskuere
x_2m <- model.matrix(tilskuere ~ ., df_2m)[, -1]

# Nu laver vi et træningsdataset og et testdataset
set.seed(123)

train_idx_2m <- sample(1:nrow(x_2m), nrow(x_2m) * 0.8)
test_idx_2m  <- setdiff(1:nrow(x_2m), train_idx_2m)

x_train_2m <- x_2m[train_idx_2m, ]
x_test_2m  <- x_2m[test_idx_2m, ]

y_train_2m <- y_2m[train_idx_2m]
y_test_2m  <- y_2m[test_idx_2m]

#Nu kan vi bruge lineær regressions model til at finde y

lm_2m_simple <- lm(
  tilskuere ~ mean_tilskuere + mean_tilskuere_saeson +
    weekday + is_weekend + is_holiday +
    kategori + modstander,
  data = df_2m[train_idx_2m, ]
)

summary(lm_2m_simple)


# ------------------------------------------
# Modellering Lineær Regression - 10 dage før
# ------------------------------------------

# Definerer x variabler og y variabel
y_10d <- df_10d$tilskuere
x_10d <- model.matrix(tilskuere ~ ., df_10d)[, -1]

# Nu laver vi et træningsdataset og et testdataset
set.seed(123)

train_idx_10d <- sample(1:nrow(x_10d), nrow(x_10d) * 0.8)
test_idx_10d  <- setdiff(1:nrow(x_10d), train_idx_10d)

x_train_10d <- x_10d[train_idx_10d, ]
x_test_10d  <- x_10d[test_idx_10d, ]

y_train_10d <- y_10d[train_idx_10d]
y_test_10d  <- y_10d[test_idx_10d]

#Nu kan vi bruge lineær regressions model til at finde y
lm_10d_simple <- lm(
  tilskuere ~ mean_tilskuere + mean_tilskuere_saeson +
    d10_tilskuere + weekday + is_weekend + is_holiday + kickoff_hour +
    kategori + modstander,
  data = df_10d[train_idx_10d, ]
)

summary(lm_10d_simple)

names(df)
view(df)


# ------------------------------------------
# Modellering Lineær Regression - 7 dage før
# ------------------------------------------

# Definerer x variabler og y variabel
y <- df_7d$tilskuere
x <- model.matrix(tilskuere ~ ., df_7d)[, -1]

#Nu laver vi et træningsdataset og et testdataset

set.seed(123)

train_idx <- sample(1:nrow(x), nrow(x) * 0.8)
test_idx  <- setdiff(1:nrow(x), train_idx)

x_train <- x[train_idx, ]
x_test  <- x[test_idx, ]

y_train <- y[train_idx]
y_test  <- y[test_idx]

#Nu kan vi bruge lineær regressions model til at finde y

lm_7d_simple <- lm(
  tilskuere ~ mean_tilskuere + mean_tilskuere_saeson +
    d7_tilskuere + weekday + is_weekend + is_holiday + kickoff_hour +
    kategori + modstander,
  data = df_7d[train_idx, ]
)

summary(lm_7d_simple)

"kickoff_hour" %in% names(df_7d)

# ------------------------------------------
# Modellering Lineær Regression - 3 dage før
# ------------------------------------------

# Definerer x variabler og y variabel
y_3d <- df_3d$tilskuere
x_3d <- model.matrix(tilskuere ~ ., df_3d)[, -1]

# Nu laver vi et træningsdataset og et testdataset
set.seed(123)

train_idx_3d <- sample(1:nrow(x_3d), nrow(x_3d) * 0.8)
test_idx_3d  <- setdiff(1:nrow(x_3d), train_idx_3d)

x_train_3d <- x_3d[train_idx_3d, ]
x_test_3d  <- x_3d[test_idx_3d, ]

y_train_3d <- y_3d[train_idx_3d]
y_test_3d  <- y_3d[test_idx_3d]

#Nu kan vi bruge lineær regressions model til at finde y

lm_3d_simple <- lm(
  tilskuere ~ mean_tilskuere + mean_tilskuere_saeson +
    d3_tilskuere + weekday + is_weekend + is_holiday + kickoff_hour +
    kategori + modstander,
  data = df_3d[train_idx_3d, ]
)

summary(lm_3d_simple)

# ---------------------------------
# Modellering Lasso - 2 Måneder Før
# ---------------------------------
library(glmnet)
set.seed(1)

# samme grid som sidste års eksamen
grid <- 10^seq(10, -2, length.out = 100)

# X og y
x_2m <- model.matrix(tilskuere ~ ., df_2m)[, -1]
y_2m <- df_2m$tilskuere

# train/test split (2/3 - 1/3)
train_2m <- sample(1:nrow(x_2m), nrow(x_2m) * 2/3)
test_2m  <- (-train_2m)
y_test_2m <- y_2m[test_2m]

# fit lasso på træning (alle lambdas i grid)
lasso_2m <- glmnet(
  x_2m[train_2m, ],
  y_2m[train_2m],
  alpha = 1,
  lambda = grid,
  thresh = 1e-12
)

# cross-validation for at vælge bedste lambda
lasso_cv_2m <- cv.glmnet(
  x_2m[train_2m, ],
  y_2m[train_2m],
  alpha = 1,
  lambda = grid,
  nfolds = 5,
  thresh = 1e-12
)

bestlam_lasso_2m <- lasso_cv_2m$lambda.min

# CV-RMSE (samme stil som sidste år)
rmse_lasso_cv_2m <- sqrt(
  lasso_cv_2m$cvm[lasso_cv_2m$lambda == bestlam_lasso_2m]
)

# test-RMSE
pred_lasso_2m <- predict(lasso_2m, s = bestlam_lasso_2m, newx = x_2m[test_2m, ])
rmse_lasso_test_2m <- sqrt(mean((pred_lasso_2m - y_test_2m)^2))

# print resultater
bestlam_lasso_2m
rmse_lasso_cv_2m
rmse_lasso_test_2m

coef_lasso_2m <- coef(lasso_cv_2m, s = "lambda.min")
coef_lasso_2m[coef_lasso_2m[,1] != 0, , drop = FALSE]
#Resultaterne viser, at modstander og historiske tilskuertal er de vigtigste faktorer 2 måneder før kamp.

# -------------------------------
# Modellering Lasso - 10 dage før
# -------------------------------
library(glmnet)
set.seed(1)

# Lambda-grid (samme som de andre modeller)
grid <- 10^seq(10, -2, length.out = 100)

# 1) X og y
x_10d <- model.matrix(tilskuere ~ ., df_10d)[, -1]
y_10d <- df_10d$tilskuere

# 2) Train / test split (2/3 - 1/3)
train_10d <- sample(1:nrow(x_10d), nrow(x_10d) * 2/3)
test_10d  <- (-train_10d)
y_test_10d <- y_10d[test_10d]

# 3) Fit Lasso (alpha = 1)
lasso_10d <- glmnet(
  x_10d[train_10d, ],
  y_10d[train_10d],
  alpha  = 1,
  lambda = grid,
  thresh = 1e-12
)

# 4) Cross-validation
lasso_cv_10d <- cv.glmnet(
  x_10d[train_10d, ],
  y_10d[train_10d],
  alpha  = 1,
  lambda = grid,
  nfolds = 5,
  thresh = 1e-12
)

bestlam_lasso_10d <- lasso_cv_10d$lambda.min

# 5) CV-RMSE
rmse_lasso_cv_10d <- sqrt(
  lasso_cv_10d$cvm[lasso_cv_10d$lambda == bestlam_lasso_10d]
)

# 6) Test-RMSE
pred_lasso_10d <- predict(
  lasso_10d,
  s    = bestlam_lasso_10d,
  newx = x_10d[test_10d, ]
)

rmse_lasso_test_10d <- sqrt(mean((pred_lasso_10d - y_test_10d)^2))

# 7) Print resultater
bestlam_lasso_10d
rmse_lasso_cv_10d
rmse_lasso_test_10d

# (Valgfrit) Hvilke variabler vælger Lasso?
coef_lasso_10d <- coef(lasso_cv_10d, s = "lambda.min")
coef_lasso_10d[coef_lasso_10d[,1] != 0, , drop = FALSE]
#10 dage før kamp spiller både modstander og kortsigtede kampvariabler en større rolle for billetsalget.

# -------------------------------------------------------
# Modellering Lasso - 7 dage før
# -------------------------------------------------------

library(glmnet)
set.seed(1)

# Lambda-grid
grid <- 10^seq(10, -2, length.out = 100)

# 1) X og y
x_7d <- model.matrix(tilskuere ~ ., df_7d)[, -1]
y_7d <- df_7d$tilskuere

# 2) Train / test split (2/3 - 1/3)
train_7d <- sample(1:nrow(x_7d), nrow(x_7d) * 2/3)
test_7d  <- (-train_7d)
y_test_7d <- y_7d[test_7d]

# 3) Fit Lasso (alpha = 1)
lasso_7d <- glmnet(
  x_7d[train_7d, ],
  y_7d[train_7d],
  alpha  = 1,
  lambda = grid,
  thresh = 1e-12
)

# 4) Cross-validation
lasso_cv_7d <- cv.glmnet(
  x_7d[train_7d, ],
  y_7d[train_7d],
  alpha  = 1,
  lambda = grid,
  nfolds = 5,
  thresh = 1e-12
)

bestlam_lasso_7d <- lasso_cv_7d$lambda.min

# 5) CV-RMSE
rmse_lasso_cv_7d <- sqrt(
  lasso_cv_7d$cvm[lasso_cv_7d$lambda == bestlam_lasso_7d]
)

# 6) Test-RMSE
pred_lasso_7d <- predict(
  lasso_7d,
  s    = bestlam_lasso_7d,
  newx = x_7d[test_7d, ]
)

rmse_lasso_test_7d <- sqrt(mean((pred_lasso_7d - y_test_7d)^2))

# 7) Print resultater
bestlam_lasso_7d
rmse_lasso_cv_7d
rmse_lasso_test_7d

# (Valgfrit) Hvilke variabler vælger Lasso?
coef_lasso_7d <- coef(lasso_cv_7d, s = "lambda.min")
coef_lasso_7d[coef_lasso_7d[,1] != 0, , drop = FALSE]
#7 dage før kamp får flere kortsigtede kampvariabler betydning for billetsalget."

# -------------------------------------------------------
# Modellering Lasso - 3 dage før
# -------------------------------------------------------

library(glmnet)
set.seed(1)

# Lambda-grid (samme som de andre modeller)
grid <- 10^seq(10, -2, length.out = 100)

# 1) X og y
x_3d <- model.matrix(tilskuere ~ ., df_3d)[, -1]
y_3d <- df_3d$tilskuere

# 2) Train / test split (2/3 - 1/3)
train_3d <- sample(1:nrow(x_3d), nrow(x_3d) * 2/3)
test_3d  <- (-train_3d)
y_test_3d <- y_3d[test_3d]

# 3) Fit Lasso (alpha = 1)
lasso_3d <- glmnet(
  x_3d[train_3d, ],
  y_3d[train_3d],
  alpha  = 1,
  lambda = grid,
  thresh = 1e-12
)

# 4) Cross-validation
lasso_cv_3d <- cv.glmnet(
  x_3d[train_3d, ],
  y_3d[train_3d],
  alpha  = 1,
  lambda = grid,
  nfolds = 5,
  thresh = 1e-12
)

bestlam_lasso_3d <- lasso_cv_3d$lambda.min

# 5) CV-RMSE
rmse_lasso_cv_3d <- sqrt(
  lasso_cv_3d$cvm[lasso_cv_3d$lambda == bestlam_lasso_3d]
)

# 6) Test-RMSE
pred_lasso_3d <- predict(
  lasso_3d,
  s    = bestlam_lasso_3d,
  newx = x_3d[test_3d, ]
)

rmse_lasso_test_3d <- sqrt(mean((pred_lasso_3d - y_test_3d)^2))

# 7) Print resultater
bestlam_lasso_3d
rmse_lasso_cv_3d
rmse_lasso_test_3d

# (Valgfrit) Hvilke variabler vælger Lasso?
coef_lasso_3d <- coef(lasso_cv_3d, s = "lambda.min")
coef_lasso_3d[coef_lasso_3d[,1] != 0, , drop = FALSE]
#3 dage før kamp dominerer kortsigtede faktorer som nyligt billetsalg, ugedag og vejr.