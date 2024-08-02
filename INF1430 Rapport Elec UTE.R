# Charger les bibliothèques nécessaires
library(tidyverse)
library(lubridate)
library(randomForest)
library(caret)
library(ggplot2)
library(readr)

# Chemin du fichier UTE
path_ute <- "/home/christian/ProjetsessionTELUQ/Projet Finale TELUQ/DATABASE Project/Electricity/Electricity_UTE.csv"

# Vérifier si le fichier existe
if (!file.exists(path_ute)) {
  stop("Le fichier n'existe pas à l'emplacement spécifié.")
}

# Charger les données UTE
data_ute <- read_csv(path_ute)

# Afficher les premières lignes du jeu de données
head(data_ute)

# Vérifier les noms des colonnes
colnames(data_ute)

# Utiliser la colonne de date et heure correcte
datetime_col <- "unix_ts"  # Assurez-vous que c'est le bon nom de colonne pour les dates et heures dans votre fichier

# Assurez-vous que la colonne de date et heure existe
if (!datetime_col %in% colnames(data_ute)) {
  stop(paste("La colonne", datetime_col, "n'existe pas dans le fichier CSV."))
}

# Convertir la colonne de date et heure en objet Date-Heure
data_ute[[datetime_col]] <- as.POSIXct(data_ute[[datetime_col]], origin = "1970-01-01", tz = "UTC")

# Vérifier les valeurs manquantes
sum(is.na(data_ute))

# Nettoyage des données : Gérer les valeurs manquantes si nécessaire
data_ute <- na.omit(data_ute)

# Analyse exploratoire des données : Statistiques descriptives
summary(data_ute)

# Vérifier les colonnes disponibles
print(colnames(data_ute))

# Assurez-vous que la colonne de consommation "P" existe
consumption_col <- "P"  # Remplacez "P" par le nom de la colonne de consommation si différent

if (!consumption_col %in% colnames(data_ute)) {
  stop(paste("La colonne", consumption_col, "n'existe pas dans le fichier CSV."))
}

# Visualiser la consommation P au fil du temps
ggplot(data_ute, aes(x = !!sym(datetime_col), y = !!sym(consumption_col))) +
  geom_line(color = "blue") +
  labs(title = "Consommation P au Fil du Temps", x = "Temps", y = "Consommation P")

# Extraction de caractéristiques : Extraire des caractéristiques utiles à partir du timestamp
data_ute <- data_ute %>%
  mutate(
    annee = year(!!sym(datetime_col)),
    mois = month(!!sym(datetime_col)),
    jour = day(!!sym(datetime_col)),
    heure = hour(!!sym(datetime_col))
  )

# Afficher les premières lignes après l'extraction des caractéristiques
head(data_ute)
