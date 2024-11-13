# fix_dataset_crbpo_acrola

## Cadre
PNA ACROLA 2022-2031

## Objectif
Mise en forme des extractions issues de la base de données de baguage et déplacements d’oiseaux de France (effectuées avant 2024) avant exploitation du jeu de données

## Résumé des modifications
### Thème
Modification appliquées sur l'ensemble du jeu de données : 
- thème de la session de capture (THEME SESSION) complété par imputation

### Localisations
- correction des erreurs de saisies pour les coordonnées géographiques (LAT, LON) de communes françaises, par géocodage (paquet 'tidygeocoder')
- correction des communes (LOCALITE) quand plusieurs communes renseignées, base INSEE
- normalisation des LIEUDIT

### Effort de capture
Modifications appliquées uniquement sur les données en thème ACROLA (après modifications ci-dessus) :
- imputation simple des données (si l'information est présente au moins une fois pour la même session de capture, elle est propagée pour toute les données de la même session) pour le nombre d'unités de capture (FS), l'heure de début de session (HS), la durée de session (DS) et le numéro de l'unité de capture (NF)
- l'heure de capture (HEURE), HS et DS sont modifiées quand une erreur de format a été suspectée
- DS est estimée si l'information est manquante, avec HS et / ou HEURE

### Age des individus
Modifications appliquées uniquement sur les données dont l'espèce est ACROLA :
- imputation simple en comparant le numéro de bague et les années de capture
- les cas contradictoires sont remplacés par NA
- catégorisation en classes d'âges (jeune versus adulte)

### à suivre...
