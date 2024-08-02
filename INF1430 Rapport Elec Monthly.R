# Charger les bibliothèques nécessaires
library(tidyverse)
library(lubridate)
library(randomForest)
library(caret)
library(ggplot2)
library(readr)

# Chemin du fichier Monthly
path_monthly <- "/home/christian/ProjetsessionTELUQ/Projet Finale TELUQ/DATABASE Project/Electricity/Electricity_Monthly.csv"

# Vérifier si le fichier existe
if (!file.exists(path_monthly)) {
  stop("Le fichier n'existe pas à l'emplacement spécifié.")
}

# Charger les données Monthly
data_monthly <- read_csv(path_monthly)

# Afficher les premières lignes du jeu de données
head(data_monthly)

# Vérifier les noms des colonnes
colnames(data_monthly)

# Utiliser la colonne de date et heure correcte
datetime_col <- "Month Year"  # Assurez-vous que c'est le bon nom de colonne pour les dates et heures dans votre fichier

# Assurez-vous que la colonne de date et heure existe
if (!datetime_col %in% colnames(data_monthly)) {
  stop(paste("La colonne", datetime_col, "n'existe pas dans le fichier CSV."))
}

# Convertir la colonne de date et heure en objet Date
data_monthly[[datetime_col]] <- parse_date(data_monthly[[datetime_col]], format = "%b %Y")

# Vérifier les valeurs manquantes
sum(is.na(data_monthly))

# Nettoyage des données : Gérer les valeurs manquantes si nécessaire
data_monthly <- na.omit(data_monthly)

# Analyse exploratoire des données : Statistiques descriptives
summary(data_monthly)

# Visualiser la consommation Monthly au fil du temps
ggplot(data_monthly, aes(x = !!sym(datetime_col), y = `Net Consumption (kWh)`)) +
  geom_line(color = "blue") +
  labs(title = "Consommation Monthly au Fil du Temps", x = "Temps", y = "Consommation Monthly")

# Extraction de caractéristiques : Extraire des caractéristiques utiles à partir du timestamp
data_monthly <- data_monthly %>%
  mutate(
    annee = year(!!sym(datetime_col)),
    mois = month(!!sym(datetime_col)),
    jour = day(!!sym(datetime_col)),
    heure = hour(!!sym(datetime_col))
  )

# Afficher les premières lignes après l'extraction des caractéristiques
head(data_monthly)

