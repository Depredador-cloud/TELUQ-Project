# Charger les bibliothèques nécessaires
library(tidyverse)
library(lubridate)
library(randomForest)
library(caret)
library(ggplot2)
library(readr)

# Chemin du fichier HPE
path_hpe <- "/home/christian/ProjetsessionTELUQ/Projet Finale TELUQ/DATABASE Project/Electricity/Electricity_HPE.csv"

# Vérifier si le fichier existe
if (!file.exists(path_hpe)) {
  stop("Le fichier n'existe pas à l'emplacement spécifié.")
}

# Charger les données HPE
data_hpe <- read_csv(path_hpe)

# Afficher les premières lignes du jeu de données
head(data_hpe)

# Vérifier les noms des colonnes
colnames(data_hpe)

# Utiliser la colonne de date et heure correcte
datetime_col <- "unix_ts"  # Assurez-vous que c'est le bon nom de colonne pour les dates et heures dans votre fichier

# Assurez-vous que la colonne de date et heure existe
if (!datetime_col %in% colnames(data_hpe)) {
  stop(paste("La colonne", datetime_col, "n'existe pas dans le fichier CSV."))
}

# Vérifiez si la colonne est numérique
if (!is.numeric(data_hpe[[datetime_col]])) {
  stop(paste("La colonne", datetime_col, "n'est pas numérique."))
}

# Convertir la colonne de date et heure en objet Date-Heure
data_hpe[[datetime_col]] <- as.POSIXct(data_hpe[[datetime_col]], origin = "1970-01-01")

# Vérifier les valeurs manquantes
sum(is.na(data_hpe))

# Nettoyage des données : Gérer les valeurs manquantes si nécessaire
data_hpe <- na.omit(data_hpe)

# Analyse exploratoire des données : Statistiques descriptives
summary(data_hpe)

# Visualiser la consommation HPE au fil du temps
ggplot(data_hpe, aes(x = !!sym(datetime_col), y = P)) +  # Remplacez "P" par le nom de la colonne de consommation si différent
  geom_line(color = "blue") +
  labs(title = "Consommation HPE au Fil du Temps", x = "Temps", y = "Consommation HPE")

# Extraction de caractéristiques : Extraire des caractéristiques utiles à partir du timestamp
data_hpe <- data_hpe %>%
  mutate(
    annee = year(!!sym(datetime_col)),
    mois = month(!!sym(datetime_col)),
    jour = day(!!sym(datetime_col)),
    heure = hour(!!sym(datetime_col))
  )

# Afficher les premières lignes après l'extraction des caractéristiques
head(data_hpe)
