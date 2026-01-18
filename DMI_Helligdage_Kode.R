library(rvest)      # Læser HTML + finder tabeller og scrape dem fra web
library(tidyverse)  # Stor pakke til mange ting
library(lubridate)  # Nem dato/tid håndtering
library(purrr)      # map/map_dfr til loops og samle rækker
library(httr2)      # API-kald helligdage/DMI osv

# DEL 1: Scrape én sæson (bare for at se hvordan strukturen er)
# ============================================================

url_1 <- "https://superstats.dk/hold/sason?id=11&vis=program&aar=2024%2F2025" # Vores link til at skrape
page_1 <- read_html(url_1)  # Henter HTML-koden fra siden

alle_tabeller_1 <- html_table(page_1, fill = TRUE)
# html_table() finder alle <table> i HTML og laver dem til data.frames
# fill=TRUE er vigtigt fordi nogle tabeller har huller i celler så fylder den op.

kampe_raw_1 <- alle_tabeller_1[[2]]
# På den side er kamp-programmet ligger i tabel 2
View(kampe_raw_1)

# DEL 2 Loop over flere sæsoner (genbrug kode med en funktion)
# ============================================================

sæsoner <- c("2024/2025","2023/2024","2022/2023", "2021/2022", "2016/2017",
             "2015/2016","2013/2014","2007/2008","2006/2007","2005/2006",
             "2004/2005","2003/2004") # Manuelt difinere sæsoner hvor VFF spillede i Superligaer og gemme i objekt

hent_sæson <- function(s){
 
  sæson_link <- gsub("/", "%2F", s) # gsub ændrer "%2F" som URL bruger til "/" som vi har i objekt sæsoner, med andre ord "2024%2F2025 -> 2024/2025"
  
  url_2 <- paste0("https://superstats.dk/hold/sason?id=11&vis=program&aar=", sæson_link)
  
  cat("Henter kamperne for:", s, url_2, "\n")  # progress print (ser pænt ud og giver info hvad der er hentet
  
  page_2 <- read_html(url_2)
  alle_tabeller <- html_table(page_2, fill = TRUE)
  
  kampe_raw <- alle_tabeller[[2]]  # tabel 2 er program-tabellen på "hold/sason"siden
  
  # Nogle tabeller kan indeholde tomme kolonnenavne.
  # Det er irriterende i dplyr, så vi fjerner dem her:
  kol_ok <- !is.na(names(kampe_raw)) & names(kampe_raw) != ""
  kampe_raw <- kampe_raw[, kol_ok]
  
  # Nu kommer en vigtig del: "Dato" ligger som fx 6.10 (eller noget lignende) vi vil lave en rigtig Date.
  kampe <- kampe_raw |>
    mutate(
      # "Dato" tolkes som tal: fx 6.10 bliver til 6.1 (numeric), derfor dette trick:
      Dato_num = as.numeric(Dato), #gør det til tal altså 6.1
      dag      = floor(Dato_num),  # heltalsdelen altså fra 6.10 vil det være 6 = dag
      måned    = round((Dato_num - dag) * 100),   # 6.1-6 er 0.1, 0.1 * 100 = 10
      
      # sæsonen "2024/2025" har to år, vi finder start og slut
      år_start = as.integer(str_sub(s, 1, 4)), # Den tager første 4 tegn i strengen altså 2024
      år_slut  = as.integer(str_sub(s, 6, 9)), # Den tager tegn 6 til 9 altså 2025
      
      # Logik er hvis måned >= 7, så er det i sæsonens start-år (sommer/efterår)
      # ellers er det i sæsonens slut-år (vinter/forår)
      år = if_else(måned >= 7, år_start, år_slut),
      
      match_date = make_date(år, måned, dag),
      sæson = s
    )
  
  return(kampe)
}

# map_dfr = kør funktionen "hent_sæson" for hver sæson defineret i objekt "sæsoner" og bind rækkerne sammen til ét data frame
alle_kampe <- map_dfr(sæsoner, hent_sæson)
View(alle_kampe)


# DEL 3: Scrape kickoff-tid fra stadion-siden (en anden tabel) én sæson (bare for at se hvordan strukturen er)
# ============================================================
sæson_3 <- ("2024/2025")
season_link <- gsub("/", "%2F", sæson_3)
url_3 <- paste0("https://superstats.dk/stadion?aar=", season_link, "&id=11")
page_3 <- read_html(url_3)  

alle_tabeller_3 <- html_table(page_3, fill = TRUE)

#Tabel 4 er den vi skal bruge
kampe_raw_3 <- alle_tabeller_3[[4]]

#Sammenligning af tabeller

View(kampe_raw_3)
View(kampe_raw_1)

#DEL 4 Loop over flere sæsoner (genbrug kode med en funktion)
# ============================================================

hent_sæson_stadion <- function(s) {
  season_link <- gsub("/", "%2F", s)
  url <- paste0("https://superstats.dk/stadion?aar=", season_link, "&id=11")
 
  cat("Henter:", url, "\n") #Pænt print over hvad der er hentet "\n" = ny linje
  
  page <- read_html(url)
  alle_tabeller <- html_table(page, fill = TRUE)
  
  kampe_raw <- alle_tabeller[[4]]
  # Her er kickoff-tiderne i tabel 4 på stadion-siden har testet
  
  #Mere robust for manglende data så funktionen kan køre hvis de opstår
  kol_ok <- !is.na(names(kampe_raw)) & names(kampe_raw) != ""
  kampe_raw <- kampe_raw[, kol_ok]
  
  kampe <- kampe_raw |>
    filter(!is.na(Dato), Dato != "") |> # VI beholder kun rækker hvor kolonnen "Dato" har en værdi
                                        # "!is.na(Dato)" fjerner rækker hvor Dato er NA
                                        # Dato !="" fjerner rækker hvor Dato er tom tekst
    mutate(
      # mutate tilføjer kolonner uden at fjerne de gamle
      # dato-strengen er: "06/10/2024 kl. 14:00"
      # dmy_hm læser teksten som dato + tids-format altså: dag-måned-år time:minut 
      # vi fjerner "kl. " så lubridate kan parse det
      kickoff    = dmy_hm(gsub("kl\\. ", " ", Dato), tz = "Europe/Copenhagen"), # "\\." betyder punktum så "kl."
                                                                               # Uden "tz" kan tiden ende som UTC så bliver 18:00 til 20:00
      
      # match_date bruges som join-nøgle (dato uden tid)
      match_date = as.Date(kickoff),
      
      # aftalt dato_index til at joine med Jeppes data
      dato_index = format(match_date, "%d/%m/%Y"),
      sæson      = s
    )
  
  kampe
}

alle_kampe_tid <- map_dfr(sæsoner, hent_sæson_stadion)
View(alle_kampe_tid)

# DEL 5: Join kickoff ind i kamp-programmet
# ============================================================

# Vi joiner på sæson + Kamp + match_date for at matche præcist
# left_join = behold alle kampene fra programmet, selv hvis kickoff mangler.
alle_kampe_med_tid <- alle_kampe |>
  left_join(
    alle_kampe_tid |> select(sæson, Kamp, match_date, kickoff, dato_index),
    by = c("sæson", "Kamp", "match_date")
  )

#Hurtig Tjek af datasæt
View(alle_kampe_med_tid)

#Tjek om der er NA'er
alle_kampe_med_tid |>
  summarise(across(everything(), ~ sum(is.na(.)))) #opsummering pr alle kolonner i datasæt, sum(is.na(.) anonym funktion)

#NA i kickoff skyldes at der blev hentet data for kickoff kun for hjemmekampe, OK andre ikke relevante
alle_kampe_med_tid |>
  filter(is.na(kickoff)) |>
  select(sæson, Kamp, match_date)

# DEL 6: Filtrer til VFF hjemmekampe
# ============================================================

kampe_med_hold <- alle_kampe_med_tid |>
  separate(Kamp, into = c("hjemme", "ude"), sep = " - ", remove = FALSE) |> #Vores Kamp er en streng "VFF - BIF"
  mutate(
    hjemme = str_trim(hjemme),
    ude   = str_trim(ude)
  )

# Tjek om hjemme/ude ser fornuftigt ud
view(kampe_med_hold)
kampe_med_hold |> count(hjemme, sort = TRUE) |> head(10)

vff_hjemmekampe <- kampe_med_hold |>
  filter(hjemme == "VFF")

# Tjek antal hjemmekampe pr sæson
view(vff_hjemmekampe)
vff_hjemmekampe |> count(sæson) |> print(n = Inf) #Inf betyder at den skal vise alle rækker


# DEL 7: Feature 1 Helligdag (date.nager.at)
# ============================================================
# Forudsætnigner for Nager API er at den giver helligdage pr år
# Så vi finder hvilke år vi har i vores kampe datasæt
# Henter helligdage en gang per år
# Joiner dem by dato med kampe

# Del 7.1 Hente data kun for en dag for at forstå strukturen
 
dato_7 <- as.Date("2024-12-25") #eksempel på dato som er hellidag (1 Jule dag)

år_7 <- format(dato_7 ,"%Y") #Henter år fra vores dato i det nødvendig format (API giver helligdage pr år)

url_7 <- paste0("https://date.nager.at/api/v3/publicholidays","/", år_7, "/DK")

resp_7 <- request(url_7) |> req_perform()

data_7 <- resp_body_json(resp_7, simplifyVector = TRUE)



#Vi laver en tabel med alle helligedage i år med  dato i dato format, og danske navne på holiday
helligdage_7 <- tibble(
  dato = as.Date(data_7$date),
  navn = data_7$localName
)
helligdage_7


# Tjek om dato_7 er helligdag
dato_7 %in% helligdage_7$dato


#DEL 7.2 Loop over alle kampe
# ============================================================

glimpse(vff_hjemmekampe)

år <- sort(unique(vff_hjemmekampe$år)) #Vi får en pæn vektor med alle unike år hvor VFF spillede hjemmekamp

hent_helligdage <- function(y) {
  url <- paste0("https://date.nager.at/api/v3/PublicHolidays/", y, "/DK")
  cat("Henter helligdage:", y, "\n")
  
  resp <- request(url) |> req_perform()
  data <- resp_body_json(resp, simplifyVector = TRUE)
  
  tibble(
    match_date   = as.Date(data$date),
    holiday_name = data$localName,
    is_holiday   = TRUE
  )
}

helligdage <- map_dfr(år, hent_helligdage)

# Tjek på helligdage
view(helligdage)

# Join helligdage med vff_hjemmekampe + udfyld NA
vff_hjemmekampe <- vff_hjemmekampe |>
  left_join(helligdage, by = "match_date") |>
  mutate(
    is_holiday   = if_else(is.na(is_holiday), FALSE, TRUE),
    holiday_name = if_else(is.na(holiday_name), "Ingen", holiday_name)
  )

# Tjek hvor mange kampe er på helligdage?
table(vff_hjemmekampe$is_holiday)

# Mini analyse--------------------------

vff_hjemmekampe |>
  group_by(is_holiday) |>
  summarise(
    kampe = n(),                                    # tæller hvor mange kampe er i defineret grupper
    mean_tilskuere = mean(Tilskuere, na.rm = TRUE), # beregner gennemsnit af tislkuere inden hver grupper og ignorer NA
    .groups = "drop"                                # Efter opsummering beholde gruppering indeni outputtet så den er fjernet når vi færdig
  )


# DEL 8: Feature 2  Weekend vs hverdag
# ============================================================

vff_hjemmekampe <- vff_hjemmekampe |>
  mutate(
    weekday_num = wday(match_date, week_start = 1), # funtkion "wday()" fra lubridate finder ugedag ud fra en Date altså 1=mandag, 7=søndag
    dagstype    = if_else(weekday_num >= 6, "Weekend", "Hverdag")
  )

# Tjek fordeling
table(vff_hjemmekampe$dagstype)

# Mini analyse--------------------------
vff_hjemmekampe |>
  group_by(dagstype) |>
  summarise(
    kampe = n(),
    mean_tilskuere = mean(Tilskuere, na.rm = TRUE),
    .groups = "drop"
  )


# DEL 9: Feature 3 — Eftermiddag vs aften (kickoff)
# ============================================================

vff_hjemmekampe <- vff_hjemmekampe |>
  mutate(
    kickoff_hour = hour(kickoff), # hour() funktion fra lubridate som trækker timen ud af tidspunkt og gemmes i kickoff_hour
    kamp_tid = case_when( # ifelse kæde oppefra
      is.na(kickoff_hour) ~ NA_character_,
      kickoff_hour < 17   ~ "Eftermiddag",
      TRUE                ~ "Aften"
    )
  )

# Tjek fordeling?
table(vff_hjemmekampe$kamp_tid)


# Mini analyse--------------------------
vff_hjemmekampe |>
  group_by(kamp_tid) |>
  summarise(
    kampe = n(),
    mean_tilskuere = mean(Tilskuere, na.rm = TRUE),
    .groups = "drop"
  )

# DEL 10: DMI Hente rå observationer for en kamp
# ============================================================
#Forudsætninger:
#DMI kræver: stationid, tidsvindue i UTC ISO- format og API key

#DMI Key
#Det er min DMI key
dmi_api_key <- Sys.getenv("DMI_API_KEY")

#  1) Definere Kamp kickoff

kickoff_10 <- ymd_hm("2024-10-27 16:00", tz = "Europe/Copenhagen") # fra lubridate laver en dato og klokken, tz sikrer dansk tid

# 2) Vælg station (tæt på Viborg)
station_id_10 <- "06060"

# 3) Vælg tidsvindue (1 time før kickoff)
start_10 <- kickoff_10 - hours(1)
end_10 <- kickoff_10

# 4) Konvertér til UTC ISO-format (DMI forventer UTC)
start_iso_10 <- format(with_tz(start_10, "UTC"), "%Y-%m-%dT%H:%M:%SZ")
end_iso_10 <- format(with_tz(end_10,   "UTC"), "%Y-%m-%dT%H:%M:%SZ")

cat("Kickoff (DK):", format(kickoff_10, "%Y-%m-%d %H:%M %Z"), "\n")
cat("Interval UTC:", start_iso_10, "->", end_iso_10, "\n")

# 5) Byg request (robust med req_url_query)
req_10 <- request("https://dmigw.govcloud.dk/v2/metObs/collections/observation/items") |>
  req_url_query(
    stationId = station_id_10,
    datetime  = paste0(start_iso_10, "/", end_iso_10),
    `api-key` = dmi_api_key,
    limit     = 10000
  )

# 6) Kald API + parse JSON
resp_10 <- req_10 |> req_perform()
data_10  <- resp_body_json(resp_10, simplifyVector = TRUE)

# Tjek af til hvilke data vi har adgang
names(data_10)
data_10$features
data_10$features$properties

# 7) Flad "features$properties" ud til en tibble med transmute som virker som mutate men den beholder kun angivnde kolonner alle de andre fjernes
obs_10 <- as_tibble(data_10$features$properties) |>
  transmute(
    observed_utc = ymd_hms(observed, tz = "UTC"),
    observed_dk  = with_tz(observed_utc, "Europe/Copenhagen"),
    parameterId,
    value = as.numeric(value)
  )
obs_10


# Del 10.2: Lav simple features (én kamp) for at forstå strukturen
# ---------------------------------------

# Regn i 1 time før kamp:
# precip_past10min = nedbør “sidste 10 min” (mm), vi summerer for at få mm i vinduet
regn_1h_mm_10 <- obs_10 |>
  filter(parameterId == "precip_past10min") |>
  summarise(v = sum(value, na.rm = TRUE)) |>
  pull(v)

# Temperatur omkring kamp (gennemsnit i vinduet)
temp_1h_c_10 <- obs_10 |>
  filter(parameterId == "temp_dry") |>
  summarise(v = mean(value, na.rm = TRUE)) |>
  pull(v)

# Skydække (gennemsnit i vinduet)
skydaekke_1h_pct_10 <- obs_10 |>
  filter(parameterId == "cloud_cover") |>
  summarise(v = mean(value, na.rm = TRUE)) |>
  pull(v)

# Vind (hvis tilgængelig i din station/interval)
vind_1h_ms_10 <- obs_10 |>
  filter(parameterId == "wind_speed_past1h") |>
  summarise(v = mean(value, na.rm = TRUE)) |>
  pull(v)

# Saml som én pæn “feature-række”
features_1kamp_10 <- tibble(
  kickoff_dk = kickoff_10,
  station_id = station_id_10,
  regn_1h_mm = regn_1h_mm_10,
  temp_1h_c  = temp_1h_c_10,
  skydaekke_1h_pct = skydaekke_1h_pct_10,
  vind_1h_ms = vind_1h_ms_10
)

features_1kamp_10

# Regn hele dag før kamp (fra kl 6 til kickoff)
#Her er vi nødt til at definere ny observation som gælder hele dag
#------------------------------------------------------------------------------
#Definer dagen

match_date_10 <- as.Date(kickoff_10) 
start_dag_10 <- as.POSIXct(paste0(match_date_10, " 06:00:00"), tz = "Europe/Copenhagen")
end_dag_10 <- kickoff_10


start_iso_dag_10 <- format(with_tz(start_dag_10, "UTC"), "%Y-%m-%dT%H:%M:%SZ")
end_iso_dag_10   <- format(with_tz(end_dag_10,   "UTC"), "%Y-%m-%dT%H:%M:%SZ")


# Byg request (robust med req_url_query)
req_dag_10 <- request("https://dmigw.govcloud.dk/v2/metObs/collections/observation/items") |>
  req_url_query(
    stationId = station_id_10,
    datetime  = paste0(start_iso_dag_10, "/", end_iso_dag_10),
    `api-key` = dmi_api_key,
    limit     = 10000
  )

# Kald API + parse JSON
resp_dag_10 <- req_dag_10 |> req_perform()
data_dag_10  <- resp_body_json(resp_dag_10, simplifyVector = TRUE)

# Tjek af til hvilke data vi har adgang
data_dag_10$features$properties

# Lav det til en tibble
obs_day_10 <- as_tibble(data_dag_10$features$properties) |> 
  transmute(
    observed_utc =ymd_hms(observed, tz ="UTC"),
    observed_dk = with_tz(observed_utc, "Europe/Copenhagen"),
    parameterId,
    value =as.numeric(value)
  ) 

# Feature regn hele dag
regn_dag_mm_10 <- obs_day_10 |> 
  filter(parameterId == "precip_past10min") |> 
  summarise(v = sum(value, na.rm = TRUE)) |> 
  pull(v)

# TJek hvordan regn udviklede sig gennem dagen

names(obs_day_10)

regn_dag_udv <- obs_day_10 |>
  filter(parameterId == "precip_past10min") |>
  arrange(observed_dk)

regn_dag_udv |> 
  select(observed_dk, value) |> 
  print(n = Inf)

# Del 10.3: Loop af DMI for alle kampe

#VI skal sikre at vores kickoff er i dansk tidszone



# 1) Funktion for at hente rå observationer for ét tidsvindue
# ------------------------------------------------------------
hent_obs <- function(station_id, start_iso, end_iso, api_key = dmi_api_key) {

  req <- request("https://dmigw.govcloud.dk/v2/metObs/collections/observation/items") |>
    req_url_query(
      stationId = station_id,
      datetime  = paste0(start_iso, "/", end_iso),
      `api-key` = api_key,
      limit     = 10000
    ) |>
    req_timeout(15) #Hvis DMI ikke svare i 15s så afbrydes kaldet
  
  resp <- tryCatch(req_perform(req), error = function(e) NULL) # prøver at køre requesten returnerer NULL i stedet for at crashe hele scriptet
  
  #Hvis request fejlede NULL eller HTTP status er 400+ printer besked og returnerer en tom tibble så resten kan køre videre
  if (is.null(resp) || resp_status(resp) >= 400) {
    cat("DMI-kald fejlede")
    return(tibble(parameterId = character(), value = numeric(), observed = character()))
  }
  
  #parser JSON-svaret fra DMI til et R objekt
  dat <- resp_body_json(resp, simplifyVector = TRUE)
  
  #Igen hvis der ingen data er i intervallet printer besked og returnerer tom tibble så funktionen kan køre videre
  if (is.null(dat$features) || length(dat$features) == 0) {
    cat("Ingen observationer i intervallet")
    return(tibble(parameterId = character(), value = numeric(), observed = character()))
  }
  
  #Laver observationer om til en tibble og vælger kun 3 relevante kolonner
  as_tibble(dat$features$properties) |>
    transmute(
      parameterId = as.character(parameterId),
      value       = as.numeric(value),
      observed    = as.character(observed)
    )
}

# 2) Hent features for én kamp (én kickoff)
# ------------------------------------------------------------
dmi_features_for_kamp <- function(kickoff_time,
                                  kampnavn   = NA_character_,
                                  station_id = "06060",
                                  morgen_tid = 6L,
                                  api_key    = dmi_api_key) {
  
  #Hvis der mangler enten kickoff eller API key så springer vi over og returner en tibble med alle features som NA_real_
  if (is.na(kickoff_time) || api_key == "") {
    cat("Springer kamp over (mangler kickoff eller API-key)")
    return(tibble(
      regn_1h_for = NA_real_, regn_dag = NA_real_,
      temp_kamp = NA_real_, vind_1h = NA_real_,
      vindstod_1h = NA_real_, skydaekke_kamp = NA_real_
    ))
  }
  #Vi sikre at kickoff_time er i DK-tid
  kickoff_time <- force_tz(kickoff_time, "Europe/Copenhagen") 
  match_date   <- as.Date(kickoff_time)
  
  cat("Henter vejr for kamp:", format(kickoff_time, "%Y-%m-%d kl. %H:%M"), "-", kampnavn, "\n")
  
  
  # Vindue A: 1 time før kamp 
  start_1h <- kickoff_time - hours(1)
  end_1h   <- kickoff_time
  
  start_1h_iso <- format(with_tz(start_1h, "UTC"), "%Y-%m-%dT%H:%M:%SZ")
  end_1h_iso   <- format(with_tz(end_1h,   "UTC"), "%Y-%m-%dT%H:%M:%SZ")
  
  # Vindue B: kl 06 til kickoff 
  start_dag <- as.POSIXct(
    paste0(match_date, " ", sprintf("%02d:00:00", morgen_tid)), # "sprintf("%o2d" sikrer "06" i stedet for "6"
    tz = "Europe/Copenhagen"
  )
  end_dag <- kickoff_time
  
  start_dag_iso <- format(with_tz(start_dag, "UTC"), "%Y-%m-%dT%H:%M:%SZ")
  end_dag_iso   <- format(with_tz(end_dag,   "UTC"), "%Y-%m-%dT%H:%M:%SZ")
  
  # Hent rå observationer
  obs_1h  <- hent_obs(station_id, start_1h_iso,  end_1h_iso,  api_key)
  obs_dag <- hent_obs(station_id, start_dag_iso, end_dag_iso, api_key)
  
  # Lav features
  tibble(
    # Regn 1 time før
    regn_1h_for = obs_1h |>
      filter(parameterId == "precip_past10min") |>
      summarise(v = sum(value, na.rm = TRUE)) |>
      pull(v),
    # Regn hele dag
    regn_dag = obs_dag |>
      filter(parameterId == "precip_past10min") |>
      summarise(v = sum(value, na.rm = TRUE)) |>
      pull(v),
    
    # Temperatur
    temp_kamp = obs_1h |>
      filter(parameterId == "temp_dry") |>
      summarise(v = mean(value, na.rm = TRUE)) |>
      pull(v),
    
    # Vind
    vind_1h = obs_1h |>
      filter(parameterId == "wind_speed_past1h") |>
      summarise(v = mean(value, na.rm = TRUE)) |>
      pull(v),
    #Vindstød
    vindstod_1h = obs_1h |>
      filter(parameterId == "wind_gust_always_past1h") |>
      summarise(v = mean(value, na.rm = TRUE)) |>
      pull(v),
    
    # Skydække
    skydaekke_kamp = obs_1h |>
      filter(parameterId == "cloud_cover") |>
      summarise(v = mean(value, na.rm = TRUE)) |>
      pull(v)
  )
  
}

# 4) LOOP OVER ALLE KAMPE (map + unnest)
# ============================================================

vff_hjemmekampe_med_vejr <- vff_hjemmekampe |>
  filter(!is.na(kickoff)) |> # Vi fjerner kampe uden kickoff, dem skal vi ikke brug altså vi kan ikke hente data uden tidspunkt
  mutate(
    dmi = map2(      # Her vi laver ny kolonne dmi og så ved map2 (purr) vi kører række for række og bruger to kolonner ad gangen (kickoff og kamp)
      kickoff, Kamp, # til at kalde funktionen dmi_features_for_kamp() så funktionen kalder DMI, beregner regn,temperatur osv. og returnerer en lille tabel med 1 række
      ~ dmi_features_for_kamp(.x, kampnavn = .y) # så resultatet er stadig en række per kamp men nu med en ny kolonne dmi som er liste med features fra dmi
    )
  ) |>
  unnest(dmi) #unnest(dmi) tager list kolonnen dmi og gøres dens indhold til almindelige kolonner altså regn_1h for, temp_kamp laves om til kolonner i tabbellen

View(vff_hjemmekampe_med_vejr)

# 4) Vejret kategorier
# ============================================================
vff_hjemmekampe_med_vejr <- vff_hjemmekampe_med_vejr |>
  mutate(
    # Regn 1h før kamp
    regn_1h_kat = case_when(
      is.na(regn_1h_for) ~ NA_character_,
      regn_1h_for == 0   ~ "Ingen regn før kamp",
      regn_1h_for <= 5   ~ "Let regn før kamp",
      TRUE               ~ "Kraftig regn før kamp"
    ),
    
    # Regn på kampdagen (fra kl. 06)
    regn_dag_kat = case_when(
      is.na(regn_dag) ~ NA_character_,
      regn_dag == 0   ~ "Tør dag",
      regn_dag <= 5   ~ "Lidt regn i løbet af dagen",
      TRUE            ~ "Våd dag"
    ),
    
    # Temperatur ved kamp
    temp_kamp_kat = case_when(
      is.na(temp_kamp) ~ NA_character_,
      temp_kamp < 10   ~ "Koldt",
      temp_kamp < 18   ~ "Mildt",
      TRUE             ~ "Varmt"
    ),
    
    # Vind 1h før kamp
    vind_1h_kat = case_when(
      is.na(vind_1h) ~ NA_character_,
      vind_1h < 4    ~ "Svag vind",
      vind_1h < 9    ~ "Mellem vind",
      TRUE           ~ "Kraftig vind"
    ),
    
    # Vindstød 1h før kamp
    vindstød_1h_kat = case_when(
      is.na(vindstod_1h) ~ NA_character_,
      vindstod_1h < 8    ~ "Let vindstød",
      vindstod_1h < 15   ~ "Mellem vindstød",
      TRUE               ~ "Kraftige vindstød"
    ),
    
    # Skydaekke ved kamp
    skydække_kamp_kat = case_when(
      is.na(skydaekke_kamp) ~ NA_character_,
      skydaekke_kamp < 30   ~ "Solrigt",
      skydaekke_kamp < 70   ~ "Let overskyet",
      TRUE                 ~ "Overskyet"
    )
  )
  

#Tjek

view(vff_hjemmekampe_med_vejr)

# Mini analyser med gennemsnitet for at undersøge eventuelt påvirkning på tilskuertal-------------------------

#Regn føre kamp og tilskuertal
vff_hjemmekampe_med_vejr |>
  group_by(regn_1h_kat) |>
  summarise(
    kampe = n(),
    mean_tilskuere = mean(Tilskuere, na.rm = TRUE),
    .groups = "drop"
  )

#Regn på dagen og tislkuertal
vff_hjemmekampe_med_vejr |>
  group_by(regn_dag_kat) |>
  summarise(
    kampe = n(),
    mean_tilskuere = mean(Tilskuere, na.rm = TRUE),
    .groups = "drop"
  )

#Temperatur ved kamp og tilskuere
vff_hjemmekampe_med_vejr |>
  group_by(temp_kamp_kat) |>
  summarise(
    kampe = n(),
    mean_tilskuere = mean(Tilskuere, na.rm = TRUE),
    .groups = "drop"
  )

#Vind ved kamp
vff_hjemmekampe_med_vejr |>
  group_by(vind_1h_kat) |>
  summarise(
    kampe = n(),
    mean_tilskuere = mean(Tilskuere, na.rm = TRUE),
    .groups = "drop"
  )

#Skydække ved kamp
vff_hjemmekampe_med_vejr |>
  group_by(skydække_kamp_kat) |>
  summarise(
    kampe = n(),
    mean_tilskuere = mean(Tilskuere, na.rm = TRUE),
    .groups = "drop"
  )
#EVENTUEL NY FEATURE kombineret som fortæller om vejret er enten god eller ej
vff_hjemmekampe_med_vejr <- vff_hjemmekampe_med_vejr |>
  mutate(
    stadionvejr = case_when(
      regn_1h_for == 0 &
        temp_kamp >= 10 &
        vind_1h < 6 ~ "Godt stadionvejr",
      TRUE        ~ "Ikke så godt vejr"
    )
  )

#Tjek a gennemsnit
vff_hjemmekampe_med_vejr |>
  group_by(stadionvejr) |>
  summarise(
    kampe = n(),
    mean_tilskuere = mean(Tilskuere, na.rm = TRUE),
    .groups = "drop"
  )


