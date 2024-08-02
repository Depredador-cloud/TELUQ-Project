# Charger les bibliothèques nécessaires
library(tidyverse)
library(lubridate)
library(randomForest)
library(caret)
library(ggplot2)
library(readr)

# Chemin du fichier I
path_i <- "/home/christian/ProjetsessionTELUQ/Projet Finale TELUQ/DATABASE Project/Electricity/Electricity_I.csv"

# Vérifier si le fichier existe
if (!file.exists(path_i)) {
  stop("Le fichier n'existe pas à l'emplacement spécifié.")
}

# Charger les données I
data_i <- read_csv(path_i)

# Afficher les premières lignes du jeu de données
head(data_i)

# Vérifier les noms des colonnes
colnames(data_i)

# Utiliser la colonne de date et heure correcte
datetime_col <- "UNIX_TS"  # Assurez-vous que c'est le bon nom de colonne pour les dates et heures dans votre fichier

# Assurez-vous que la colonne de date et heure existe
if (!datetime_col %in% colnames(data_i)) {
  stop(paste("La colonne", datetime_col, "n'existe pas dans le fichier CSV."))
}

# Vérifiez si la colonne est numérique
if (!is.numeric(data_i[[datetime_col]])) {
  stop(paste("La colonne", datetime_col, "n'est pas numérique."))
}

# Convertir la colonne de date et heure en objet Date-Heure
data_i[[datetime_col]] <- as.POSIXct(data_i[[datetime_col]], origin = "1970-01-01")

# Vérifier les valeurs manquantes
sum(is.na(data_i))

# Nettoyage des données : Gérer les valeurs manquantes si nécessaire
data_i <- na.omit(data_i)

# Analyse exploratoire des données : Statistiques descriptives
summary(data_i)

# Visualiser la consommation I au fil du temps
ggplot(data_i, aes(x = !!sym(datetime_col), y = WHE)) +  # Remplacez "WHE" par le nom de la colonne de consommation si différent
  geom_line(color = "blue") +
  labs(title = "Consommation I au Fil du Temps", x = "Temps", y = "Consommation I")