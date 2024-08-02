# Charger les bibliothèques nécessaires
library(tidyverse)
library(lubridate)
library(randomForest)
library(caret)
library(ggplot2)
library(readr)

# Chemin du fichier S
path_s <- "/home/christian/ProjetsessionTELUQ/Projet Finale TELUQ/DATABASE Project/Electricity/Electricity_S.csv"

# Vérifier si le fichier existe
if (!file.exists(path_s)) {
  stop("Le fichier n'existe pas à l'emplacement spécifié.")
}

# Charger les données S
data_s <- read_csv(path_s)

# Afficher les premières lignes du jeu de données
head(data_s)

# Vérifier les noms des colonnes
colnames(data_s)

# Utiliser la colonne de date et heure correcte
datetime_col <- "UNIX_TS"  # Assurez-vous que c'est le bon nom de colonne pour les dates et heures dans votre fichier

# Assurez-vous que la colonne de date et heure existe
if (!datetime_col %in% colnames(data_s)) {
  stop(paste("La colonne", datetime_col, "n'existe pas dans le fichier CSV."))
}

# Convertir la colonne de date et heure en objet Date-Heure
data_s[[datetime_col]] <- as.POSIXct(data_s[[datetime_col]], origin = "1970-01-01", tz = "UTC")

# Vérifier les valeurs manquantes
sum(is.na(data_s))

# Nettoyage des données : Gérer les valeurs manquantes si nécessaire
data_s <- na.omit(data_s)

# Analyse exploratoire des données : Statistiques descriptives
summary(data_s)

# Visualiser la consommation WHE au fil du temps (remplacez "WHE" par une colonne appropriée si nécessaire)
ggplot(data_s, aes(x = !!sym(datetime_col), y = WHE)) +  # Remplacez "WHE" par le nom de la colonne de consommation si différent
  geom_line(color = "blue") +
  labs(title = "Consommation WHE au Fil du Temps", x = "Temps", y = "Consommation WHE")

# Extraction de caractéristiques : Extraire des caractéristiques utiles à partir du timestamp
data_s <- data_s %>%
  mutate(
    annee = year(!!sym(datetime_col)),
    mois = month(!!sym(datetime_col)),
    jour = day(!!sym(datetime_col)),
    heure = hour(!!sym(datetime_col))
  )

# Afficher les premières lignes après l'extraction des caractéristiques
head(data_s)
