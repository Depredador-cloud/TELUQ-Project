# Charger les bibliothèques nécessaires
library(tidyverse)
library(lubridate)
library(randomForest)
library(caret)
library(ggplot2)
library(readr)

# Chemin du fichier DWE
path_dwe <- "/home/christian/ProjetsessionTELUQ/Projet Finale TELUQ/DATABASE Project/Electricity/Electricity_DWE.csv"

# Vérifier si le fichier existe
if (!file.exists(path_dwe)) {
  stop("Le fichier n'existe pas à l'emplacement spécifié.")
}

# Charger les données DWE
data_dwe <- read_csv(path_dwe)

# Afficher les premières lignes du jeu de données
head(data_dwe)

# Vérifier les noms des colonnes
colnames(data_dwe)

# Utiliser la colonne de date et heure correcte
datetime_col <- "unix_ts"  # Assurez-vous que c'est le bon nom de colonne pour les dates et heures dans votre fichier

# Assurez-vous que la colonne de date et heure existe
if (!datetime_col %in% colnames(data_dwe)) {
  stop(paste("La colonne", datetime_col, "n'existe pas dans le fichier CSV."))
}

# Vérifiez si la colonne est numérique
if (!is.numeric(data_dwe[[datetime_col]])) {
  stop(paste("La colonne", datetime_col, "n'est pas numérique."))
}

# Convertir la colonne de date et heure en objet Date-Heure
data_dwe[[datetime_col]] <- as.POSIXct(data_dwe[[datetime_col]], origin = "1970-01-01")

# Vérifier les valeurs manquantes
sum(is.na(data_dwe))

# Nettoyage des données : Gérer les valeurs manquantes si nécessaire
data_dwe <- na.omit(data_dwe)

# Analyse exploratoire des données : Statistiques descriptives
summary(data_dwe)

# Visualiser la consommation DWE au fil du temps
ggplot(data_dwe, aes(x = !!sym(datetime_col), y = P)) +  # Remplacez "P" par le nom de la colonne de consommation si différent
  geom_line(color = "blue") +
  labs(title = "Consommation DWE au Fil du Temps", x = "Temps", y = "Consommation DWE")

# Extraction de caractéristiques : Extraire des caractéristiques utiles à partir du timestamp
data_dwe <- data_dwe %>%
  mutate(
    annee = year(!!sym(datetime_col)),
    mois = month(!!sym(datetime_col)),
    jour = day(!!sym(datetime_col)),
    heure = hour(!!sym(datetime_col))
  )

# Afficher les premières lignes après l'extraction des caractéristiques
head(data_dwe)
