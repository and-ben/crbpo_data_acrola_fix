# la séquence du code est importante
# il faut lancer dans l'ordre
# car pour corriger DS, il faut corriger HS et / ou HEURE
# pour corriger HS, il faut corriger HEURE


# Paramétrage de l'environnement de travail ####
rm(list = ls()) # nettoie l'environnement de travail

# liste les paquets utiles
packages = c("tidyverse",
             "nplyr")

# fonction pour installer et/ou charger les paquets utiles
package.check = lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)


# charge les données
## ici données de deux extractions jointes
## avant jointure : 
## - date passée en format date avec dmy(DATE)
## - une entrée dans MA "VOL" remplacée par NA
## - puis transformation colonne MA en numérique
crbpo = read_delim("/media/ben/SSD_BEN/ACROLA/dataset/baguage/data/raw/Extrait20240902-20241011.txt",
                   guess_max = 1048576)
crbpo_raw = crbpo # backup

# création colonne année
# création colonne mois
# sélection des données
crbpo = crbpo %>%
  mutate(yr = year(DATE),
         mois = month(DATE)) %>%
  filter(mois >= 6 & mois <= 10 & yr > 2007)


# Thème de session ####
# si THEME SESSION est vide et THEME est ACROLA, 
# remplir ACROLA dans THEME SESSION,
# sinon remplir avec la valeur de THEME SESSION
## fix_* : trace des changements TRUE = données modifiées / FALSE = non modifiées
crbpo = crbpo %>%
  mutate(fix_theme_session = case_when(is.na(`THEME SESSION`) & THEME == "ACROLA" ~ TRUE,
                                     .default = FALSE),
         `THEME SESSION` = case_when(is.na(`THEME SESSION`) & THEME == "ACROLA" ~ "ACROLA",
                                   .default = `THEME SESSION`))

# NOTE
# testé imputation simple, i.e., information disponible pour la même session
# avec base clé session = c(DATE, BAGUEUR, LAT, LON) :
# informations complètes, pas d'imputation en sortie


# Localisation ####
## Erreurs de coordonnées ####
# remplace les coordonnées erronnées :
## relocalise BEUVRY (centroïde)
## relocalise CRECHES-SUR-SAONE (centroïde)
# fix_* : trace des changements TRUE/FALSE
crbpo = crbpo %>%
  mutate(LAT = case_when(yr < 2024 & cId_Localisation == 91518 ~ 50.53372,
                         yr < 2024 & cId_Localisation == 285793 ~ 46.24636,
                         .default = LAT),
         LON = case_when(yr < 2024 & cId_Localisation == 91518 ~ 2.686479,
                         yr < 2024 & cId_Localisation == 285793 ~ 4.786796,
                         .default = LON),
         fix_LAT = case_when(yr < 2024 & cId_Localisation == 91518 ~ TRUE,
                            yr < 2024 & cId_Localisation == 285793 ~ TRUE,
                            .default = FALSE),
         fix_LON = case_when(yr < 2024 & cId_Localisation == 91518 ~ TRUE,
                             yr < 2024 & cId_Localisation == 285793 ~ TRUE,
                             .default = FALSE))


## LOCALITE ####
# remplace le contenu des cellules contenant deux localités
# par les communes définies dans la base INSEE v_communes_2024.csv
# https://www.insee.fr/fr/information/7766585
# uniquement COM et pas COMD
## fix_* :  trace des changements TRUE / FALSE
crbpo = crbpo %>%
  mutate(fix_LOCALITE = case_when(LOCALITE == "CARENTAN\r\nSAINT-COME-DU-MONT" ~ TRUE,
                                  LOCALITE == "VAUVILLE\r\nBEAUMONT-HAGUE" ~ TRUE,
                                  LOCALITE == "CARENTAN\r\nBREVANDS" ~ TRUE,
                                  LOCALITE == "SAINTENY\r\nSAINT-GEORGES-DE-BOHON" ~ TRUE,
                                  .default = FALSE),
         LOCALITE = case_when(LOCALITE == "CARENTAN\r\nSAINT-COME-DU-MONT" ~ "CARENTAN-LES-MARAIS",
                              LOCALITE == "VAUVILLE\r\nBEAUMONT-HAGUE" ~ "HAGUE",
                              LOCALITE == "CARENTAN\r\nBREVANDS" ~ "CARENTAN-LES-MARAIS",
                              LOCALITE == "SAINTENY\r\nSAINT-GEORGES-DE-BOHON" ~ "TERRE-ET-MARAIS",
                              .default = LOCALITE))

## LIEUDIT ####
# harmonisation des noms
## fix_* : trace des changements TRUE / FALSE
crbpo = crbpo %>%
  mutate(fix_LIEUDIT = case_when(LIEUDIT == "PARENT" ~ TRUE,
                                 LIEUDIT == "PK20" ~ TRUE,
                                 LIEUDIT == "ETANG DU LEHAN" ~ TRUE,
                                 LIEUDIT == "RCFS MIGRON" ~ TRUE,
                                 LIEUDIT == "PK21" ~ TRUE,
                                 LIEUDIT == "Terre d'oiseaux" ~ TRUE, 
                                 LIEUDIT == "CEN_1" ~ TRUE,
                                 .default = FALSE),
         LIEUDIT = case_when(LIEUDIT == "PARENT" ~ "PARENTS",
                             LIEUDIT == "PK20" ~ "PK 20",
                             LIEUDIT == "ETANG DU LEHAN" ~ "Marais du Léhan",
                             LIEUDIT == "RCFS MIGRON" ~ "Réserve du Migron",
                             LIEUDIT == "PK21" ~ "PK 21",
                             LIEUDIT == "Terre d'oiseaux" ~ "Terres d'oiseaux", 
                             LIEUDIT == "CEN_1" ~ "CD_2",
                             .default = LIEUDIT))


# Effort de capture ####
# données hors thème ACROLA
crbpo_wo_acrola = crbpo %>%
  filter(`THEME SESSION` != "ACROLA" | is.na(`THEME SESSION`))

# données en thème ACROLA
# génère une clé de session (car cId_Session pas fiable)
# créé la colonne FS_copied vide et formatée pour réception des données
# créé la colonne HS_copied vide et formatée pour réception des données
# créé la colonne DS_copied vide et formatée pour réception des données
# remplace NF code (-)999 par NA (anciens codes ??)
# remplace NF code 33 par 3 car faute de frappe suspectée
# remplace FS code 3 par 36 car faute de frappe suspectée
# imbrique les données par clés de session
# dans les données imbriquées (i.e., colonne data) :
## si une valeur unique de FS par session, copie la valeur pour toute la session, sinon NA
## si une valeur unique de HS par session, copie la valeur pour toute la session, sinon NA
## si une valeur unique de DS par session, copie la valeur pour toute la session, sinon NA
# paramètre un calcul par ligne
# estime le nombre d'unités de capture par session, i.e., nombre de valeurs NF différentes
# désimbrique les données
## fix_* :  trace des changements TRUE / FALSE
crbpo_acrola = crbpo %>%
  filter(`THEME SESSION` == "ACROLA") %>%
  mutate(session_key = paste(DATE, BAGUEUR, LAT, LON),
         FS_copied = NA,
         HS_copied = hms::as_hms(NA),
         DS_copied = hms::as_hms(NA),
         fix_NF = case_when(NF == 999 | NF == -999 ~ TRUE,
                            NF == 33 ~ TRUE,
                            .default = FALSE),
         NF = case_when(NF == 999 | NF == -999 ~ NA,
                        NF == 33 ~ 3,
                        .default = NF),
         fix_FS = case_when(FS == 3 ~ TRUE,
                            .default = FALSE),
         FS = case_when(FS == 3 ~ 36,
                        .default = FS)) %>%
  nest(data = -session_key) %>%
  nest_mutate(data,
              FS_copied = ifelse(length(na.omit(unique(FS))) == 1,
                                 unique(na.omit(FS)), NA),
              HS_copied = ifelse(length(na.omit(unique(HS))) == 1,
                                 unique(na.omit(HS)), NA),
              DS_copied = ifelse(length(na.omit(unique(DS))) == 1,
                                 unique(na.omit(DS)), NA)) %>%
  rowwise() %>%
  mutate(nb_unit = length(unique(na.omit(data$NF)))) %>%
  unnest(cols = c(data)) 

# nombre de valeurs FS imputables
crbpo_acrola %>%
  filter(!is.na(FS_copied) & is.na(FS)) %>%
  nrow()

# nombre de valeurs HS imputables
crbpo_acrola %>%
  filter(!is.na(HS_copied) & is.na(HS)) %>%
  nrow()

# nombre de valeurs DS imputables
crbpo_acrola %>%
  filter(!is.na(DS_copied) & is.na(DS)) %>%
  nrow()

# FS == 612 ??
# FS == 34 ??

# remplace FS session 457058, car erreur de saisie suspectée
# impute les valeurs de FS quand valeurs manquantes
# impute les valeurs de HS quand valeurs manquantes
# impute les valeurs de DS quand valeurs manquantes
# recalcul FS si FS = 36 et plusieurs NF renseignés (bug si intégré dans case_when)
# formate HS
# formate DS
## fix_* :  trace des changements TRUE / FALSE
crbpo_acrola = crbpo_acrola %>%
  mutate(fix_FS = case_when(cId_Session == 457058 ~ TRUE,
                            is.na(FS) ~ TRUE,
                            .default = fix_FS),
         FS = case_when(cId_Session == 457058 ~ nb_unit * 36,
                        is.na(FS) ~ FS_copied,
                        .default = FS),
         fix_HS = ifelse(is.na(HS), TRUE, FALSE),
         HS = ifelse(is.na(HS), HS_copied, HS),
         fix_DS = ifelse(is.na(DS), TRUE, FALSE),
         DS = ifelse(is.na(DS), DS_copied, DS)) %>%
  mutate(fix_FS = if_else(FS == 36 & nb_unit > 1, TRUE, fix_FS),
         FS = if_else(FS == 36 & nb_unit > 1, nb_unit * 36, FS),
         HS = hms::as_hms(HS),
         DS = hms::as_hms(DS))


## HEURE (heure de capture) ####
# cas si ESPECE est ACROLA
# si HEURE est minuit, remplace par midi car erreur de format suspectée
# si HEURE < 5h, remplace par HEURE x 60 car erreur de format suspectée (ex : 00:06:00 renseigné au lieu de 06:00:00)
# si HEURE > 15h, remplace par HEURE -12h, car erreur de format suspectée
## fix_* :  trace des changements TRUE / FALSE
crbpo_acrola = crbpo_acrola %>%
  mutate(fix_HEURE = case_when(ESPECE == "ACROLA" & 
                                 HEURE == hms::as_hms("00:00:00") ~ TRUE,
                               ESPECE == "ACROLA" & 
                                 HEURE < hms::as_hms("05:00:00") ~ TRUE,
                               ESPECE == "ACROLA" & 
                                 HEURE > hms::as_hms("15:00:00") ~ TRUE,
                               .default = FALSE),
         HEURE = case_when(ESPECE == "ACROLA" & 
                             HEURE == hms::as_hms("00:00:00") ~ hms::as_hms(HEURE + 12 * 60 * 60),
                           ESPECE == "ACROLA" & 
                             HEURE < hms::as_hms("05:00:00") ~ hms::as_hms(HEURE * 60),
                           ESPECE == "ACROLA" & 
                             HEURE > hms::as_hms("15:00:00") ~ hms::as_hms(HEURE - 12 * 60 * 60),
                           .default = HEURE))

## HS (heure de début de session) ####
# cas si ESPECE est ACROLA
# si HS < 4h, remplace par HEURE x 60 car erreur de format suspectée (ex : 00:03:00 renseigné au lieu de 03:00:00)
# si HS > 10h, remplace par HEURE -12h, car erreur de format suspectée
## fix_* :  trace des changements TRUE / FALSE
crbpo_acrola = crbpo_acrola %>%
  mutate(fix_HS = case_when(ESPECE == "ACROLA" & 
                              HS < hms::as_hms("04:00:00") ~ TRUE,
                            ESPECE == "ACROLA" & 
                              HS > hms::as_hms("10:00:00") ~ TRUE,
                            .default = fix_HS),
         HS = case_when(ESPECE == "ACROLA" & 
                             HS < hms::as_hms("04:00:00") ~ hms::as_hms(HS * 60),
                        ESPECE == "ACROLA" & 
                          HS > hms::as_hms("10:00:00") ~ hms::as_hms(HS - 12 * 60 * 60),
                        .default = HS))


## DS ####
# imbrique les données par clé de session
# paramétrise un calcul par ligne
# création et calcul de la colonne h_max (heure de capture max)
# création et calcul de la colonne h_min (heure de capture min)
# remplace les valeurs -Inf (généré quand pas de data, voir messages d'avis) par NA
# désimbrique les données
# cas si ESPECE est ACROLA
# si DS < 30 min, remplace par DS x 60, car erreur de format suspectée
# si DS > 8h30, on considère que l'heure de fin de session a été renseigné et non la durée, donc recalcul DS
# si DS non renseigné, estime DS sur la base de heure de capture max et heure de début de session
# si DS et HS non renseignés, estime DS sur la base des heures de capture min et max
crbpo_acrola = crbpo_acrola %>%
  nest(data = -session_key) %>%
  rowwise() %>%
  mutate(h_max = hms::as_hms(max(data$HEURE, na.rm = T)),
         h_min = hms::as_hms(min(data$HEURE, na.rm = T)),
         h_max = if_else(h_max == -Inf, NA, h_max),
         h_min = if_else(h_min == -Inf, NA, h_min)) %>%
  unnest(cols = c(data)) %>%
  mutate(fix_DS = case_when(ESPECE == "ACROLA" & 
                              DS < hms::as_hms("00:30:00") ~ TRUE,
                            ESPECE == "ACROLA" & 
                              DS > hms::as_hms("08:30:00") ~ TRUE,
                            ESPECE == "ACROLA" &
                              is.na(DS) ~ TRUE,
                            ESPECE == "ACROLA" &
                              is.na(DS) & is.na(HS) ~ TRUE,
                            .default = fix_DS),
         DS = case_when(ESPECE == "ACROLA" & 
                          DS < hms::as_hms("00:30:00") ~ hms::as_hms(DS * 60),
                        ESPECE == "ACROLA" & 
                          DS > hms::as_hms("08:30:00") ~ hms::as_hms(DS - HS),
                        ESPECE == "ACROLA" &
                          is.na(DS) ~ h_max - HS,
                        ESPECE == "ACROLA" &
                          is.na(DS) & is.na(HS) ~ h_max - h_min,
                        .default = DS))


# Age ####
# recoller toutes les données ensemble
crbpo = bind_rows(crbpo_wo_acrola, crbpo_acrola)

# remplace l'AGE en cas de problème détecté
# NOTE :
## si le code commence par DATE, imputation avec info la plus pertinente
## si le code commence par yr, plusieurs âges différents pour la même année donc remplace par NA toutes les occurences
## si le code commence par ESPECE, règle générale de conversion vers jeune / adulte / NA
crbpo = crbpo %>%
  mutate(fix_AGE = case_when(DATE == "2009-06-20" & BAGUE == "....6068531" ~ TRUE,
                             DATE == "2010-07-06" & BAGUE == "....6051019" ~ TRUE,
                             yr < 2024 & BAGUE == "....6337853" ~ TRUE,
                             DATE == "2011-08-12" & BAGUE == "...12129585" ~ TRUE,
                             DATE == "2011-08-17" & BAGUE == "....6471335" ~ TRUE,
                             DATE == "2012-08-16" & BAGUE == "....6471335" ~ TRUE,
                             DATE == "2011-08-23" & BAGUE == "....6550785" ~ TRUE,
                             DATE == "2014-07-24" & BAGUE == "....6546902" ~ TRUE,
                             DATE == "2014-08-20" & BAGUE == "....7477385" ~ TRUE,
                             yr < 2024 & BAGUE == "....7857682" ~ TRUE,
                             yr < 2024 & BAGUE == "....7540607" ~ TRUE,
                             yr < 2024 & BAGUE == "....7508724" ~ TRUE,
                             yr < 2024 & BAGUE == "....7949411" ~ TRUE,
                             DATE == "2017-09-15" & BAGUE == "....3886190" ~ TRUE,
                             DATE == "2017-09-04" & BAGUE == "....3886187" ~ TRUE,
                             DATE == "2017-09-01" & BAGUE == "....3886183" ~ TRUE,
                             DATE == "2017-08-20" & BAGUE == "....3886144" ~ TRUE,
                             DATE == "2018-08-03" & BAGUE == "....3886205" ~ TRUE,
                             DATE == "2021-07-09" & BAGUE == "....8298172" ~ TRUE,
                             DATE == "2019-08-02" & BAGUE == "....9026002" ~ TRUE,
                             DATE == "2021-08-10" & BAGUE == "....9694936" ~ TRUE,
                             DATE == "2022-08-24" & BAGUE == "....8532184" ~ TRUE,
                             DATE == "2022-08-16" & BAGUE == "....9122090" ~ TRUE,
                             yr < 2024 & BAGUE == "....9071835" ~ TRUE,
                             yr < 2024 & BAGUE == "....6546938" ~ TRUE,
                             DATE == "2012-08-27" & BAGUE == "....7017227" ~ TRUE,
                             DATE == "2012-08-09" & BAGUE == "....6490895" ~ TRUE,
                             yr < 2024 & BAGUE == "....7508782" ~ TRUE,
                             DATE == "2014-08-20" & BAGUE == "....6950950" ~ TRUE,
                             DATE == "2017-08-20" & BAGUE == "....3886142" ~ TRUE,
                             yr < 2024 & BAGUE == "....8486043" ~ TRUE,
                             ESPECE == "ACROLA" & AGE == "+1?" ~ TRUE,
                             ESPECE == "ACROLA" & AGE == "+1A" ~ TRUE,
                             ESPECE == "ACROLA" & AGE == "+2A" ~ TRUE,
                             ESPECE == "ACROLA" & AGE == "1A" ~ TRUE,
                             ESPECE == "ACROLA" & AGE == "1A?" ~ TRUE,
                             ESPECE == "ACROLA" & AGE == "2A" ~ TRUE,
                             ESPECE == "ACROLA" & AGE == "2A?" ~ TRUE,
                             ESPECE == "ACROLA" & AGE == "???" ~ TRUE,
                             ESPECE == "ACROLA" & AGE == "PUL" ~ TRUE,
                             ESPECE == "ACROLA" & AGE == "VOL" ~ TRUE,
                             .default = FALSE),
         AGE = case_when(DATE == "2009-06-20" & BAGUE == "....6068531" ~ "adulte",
                         DATE == "2010-07-06" & BAGUE == "....6051019" ~ "adulte",
                         yr < 2024 & BAGUE == "....6337853" ~ NA,
                         DATE == "2011-08-12" & BAGUE == "...12129585" ~ "jeune",
                         DATE == "2011-08-17" & BAGUE == "....6471335" ~ "jeune",
                         DATE == "2012-08-16" & BAGUE == "....6471335" ~ "adulte",
                         DATE == "2011-08-23" & BAGUE == "....6550785" ~ "jeune",
                         DATE == "2014-07-24" & BAGUE == "....6546902" ~ "adulte",
                         DATE == "2014-08-20" & BAGUE == "....7477385" ~ "jeune",
                         yr < 2024 & BAGUE == "....7857682" ~ NA,
                         yr < 2024 & BAGUE == "....7540607" ~ NA,
                         yr < 2024 & BAGUE == "....7508724" ~ NA,
                         yr < 2024 & BAGUE == "....7949411" ~ NA,
                         DATE == "2017-09-15" & BAGUE == "....3886190" ~ "jeune",
                         DATE == "2017-09-04" & BAGUE == "....3886187" ~ "adulte",
                         DATE == "2017-09-01" & BAGUE == "....3886183" ~ "jeune",
                         DATE == "2017-08-20" & BAGUE == "....3886144" ~ "jeune",
                         DATE == "2018-08-03" & BAGUE == "....3886205" ~ "jeune",
                         DATE == "2021-07-09" & BAGUE == "....8298172" ~ "adulte",
                         DATE == "2019-08-02" & BAGUE == "....9026002" ~ "jeune",
                         DATE == "2021-08-10" & BAGUE == "....9694936" ~ "adulte",
                         DATE == "2022-08-24" & BAGUE == "....8532184" ~ "jeune",
                         DATE == "2022-08-16" & BAGUE == "....9122090" ~ "jeune",
                         yr < 2024 & BAGUE == "....9071835" ~ NA,
                         yr < 2024 & BAGUE == "....6546938" ~ NA,
                         DATE == "2012-08-27" & BAGUE == "....7017227" ~ "jeune",
                         DATE == "2012-08-09" & BAGUE == "....6490895" ~ "jeune",
                         yr < 2024 & BAGUE == "....7508782" ~ NA,
                         DATE == "2014-08-20" & BAGUE == "....6950950" ~ "jeune",
                         DATE == "2017-08-20" & BAGUE == "....3886142" ~ "jeune",
                         yr < 2024 & BAGUE == "....8486043" ~ NA,
                         ESPECE == "ACROLA" & AGE == "+1?" ~ NA,
                         ESPECE == "ACROLA" & AGE == "+1A" ~ "adulte",
                         ESPECE == "ACROLA" & AGE == "+2A" ~ "adulte",
                         ESPECE == "ACROLA" & AGE == "1A" ~ "jeune",
                         ESPECE == "ACROLA" & AGE == "1A?" ~ NA,
                         ESPECE == "ACROLA" & AGE == "2A" ~ "adulte",
                         ESPECE == "ACROLA" & AGE == "2A?" ~ "adulte",
                         ESPECE == "ACROLA" & AGE == "???" ~ NA,
                         ESPECE == "ACROLA" & AGE == "PUL" ~ "jeune",
                         ESPECE == "ACROLA" & AGE == "VOL" ~ NA,
                         .default = AGE))


# Exports ####
write_csv(crbpo, "/media/ben/SSD_BEN/ACROLA/dataset/baguage/data/processed/crbpo_formatted.csv")

crbpo_modif = crbpo %>%
  filter(fix_AGE == T |
           fix_DS == T |
           fix_FS == T |
           fix_HEURE == T |
           fix_HS == T |
           fix_LAT == T |
           fix_LIEUDIT == T |
           fix_LOCALITE == T |
           fix_LON == T |
           fix_NF == T |
           fix_theme_session == T)


# END
# NOT RUN
###############################################################################
###############################################################################
# valeurs biométriques aberrantes 
# routine DS > 7 et DS > 2 ok
# vérif des HS ? et HEURE pour corrections DS ok
# vérif heure de baguage avant début de session
## HS = 08:40:00 & DS = 04:00:00 ?? cid_session = 1574274
## HS = 06:45:00 & DS = 05:15:00 ou DS = 04:15:00, HS <=> DS ?
# vérif même num bague mais sp diff
check = crbpo %>%
  filter(`THEME SESSION` == "ACROLA") %>%
  select(BAGUE, ESPECE) %>%
  group_by(BAGUE) %>%
  nest() %>%
  rowwise() %>%
  mutate(nb_sp = length(unique(data$ESPECE))) %>%
  filter(nb_sp > 1) %>%
  unnest(data)
# pb ....9108217


# croiser MA avec adip si pour corrections MA
# tous les +2A? +2 +2? etc.. == adultes ok
# vol vérifier si controle intr-journalier pas recopié ok

# attention si on change plusieurs fois les colonnes fix, il faut remplacer .default par colonne fix et pas FALSE
# erreur de format 00:00:00 à "multiplier" par 60 ou alors c'est midi ? le plus probable = x60

# erreurs ou valeurs remarquables ?
## bagues : ....6473210 / ....7017226


# FS == 612 ??
# FS == 34 ??

##################################
ds_count = crbpo_acrola %>%
  filter(ESPECE == "ACROLA") %>%
  group_by(DS) %>%
  summarise(n = n())

hs_count = crbpo_acrola %>%
  filter(ESPECE == "ACROLA") %>%
  group_by(HS) %>%
  summarise(n = n())
# HS 00:05:00 / 00:06:00/00:08:00

heure_count = crbpo_acrola %>%
  filter(ESPECE == "ACROLA") %>%
  group_by(HEURE) %>%
  summarise(n = n())

check = crbpo_acrola %>%
  filter(ESPECE == "ACROLA") %>%
  filter(DS > hms::as_hms("08:30:00")) %>%
  group_by(session_key) %>%
  nest()

hs_count = crbpo_acrola %>%
  filter(ESPECE == "ACROLA") %>%
  group_by(HS) %>%
  summarise(n = n())

check = crbpo_acrola %>%
  filter(ESPECE == "ACROLA") %>%
  filter(HS < hms::as_hms("04:00:00")) %>%
  group_by(session_key) %>%
  nest()

check = crbpo_acrola %>%
  filter(ESPECE == "ACROLA") %>%
  filter(HS > hms::as_hms("10:00:00")) %>%
  group_by(session_key) %>%
  nest()


crbpo %>%
  filter(ESPECE == "ACROLA") %>%
  count(AGE)

check = crbpo %>%
  filter(ESPECE == "ACROLA") %>%
  select(BAGUE, AGE, yr) %>%
  group_by(BAGUE) %>%
  nest() %>%
  rowwise() %>%
  mutate(n_age = length(unique(data$AGE))) %>%
  filter(n_age > 1)

crbpo %>%
  filter(BAGUE == check$BAGUE[126])

crbpo %>%
  count(fix_theme_session)
crbpo %>%
  count(fix_LAT)
crbpo %>%
  count(fix_LON)
crbpo %>%
  count(fix_LOCALITE)
crbpo %>%
  count(fix_LIEUDIT)
crbpo %>%
  count(fix_NF)
crbpo %>%
  count(fix_FS)
crbpo %>%
  count(fix_HS)
crbpo %>%
  count(fix_DS)
crbpo %>%
  count(fix_HEURE)
crbpo %>%
  count(fix_AGE)
