# Charger les bibliothèques nécessaires
library(tidyverse)
library(lubridate)
library(randomForest)
library(caret)
library(ggplot2)
library(readr)

# Chemin du fichier RSE
path_rse <- "/home/christian/ProjetsessionTELUQ/Projet Finale TELUQ/DATABASE Project/Electricity/Electricity_RSE.csv"

# Vérifier si le fichier existe
if (!file.exists(path_rse)) {
  stop("Le fichier n'existe pas à l'emplacement spécifié.")
}

# Charger les données RSE
data_rse <- read_csv(path_rse)

# Afficher les premières lignes du jeu de données
head(data_rse)

# Vérifier les noms des colonnes
colnames(data_rse)

# Utiliser la colonne de date et heure correcte
datetime_col <- "unix_ts"  # Assurez-vous que c'est le bon nom de colonne pour les dates et heures dans votre fichier

# Assurez-vous que la colonne de date et heure existe
if (!datetime_col %in% colnames(data_rse)) {
  stop(paste("La colonne", datetime_col, "n'existe pas dans le fichier CSV."))
}

# Convertir la colonne de date et heure en objet Date-Heure
data_rse[[datetime_col]] <- as.POSIXct(data_rse[[datetime_col]], origin = "1970-01-01", tz = "UTC")

# Vérifier les valeurs manquantes
sum(is.na(data_rse))

# Nettoyage des données : Gérer les valeurs manquantes si nécessaire
data_rse <- na.omit(data_rse)

# Analyse exploratoire des données : Statistiques descriptives
summary(data_rse)

# Visualiser la consommation P au fil du temps (remplacez "P" par la colonne souhaitée)
ggplot(data_rse, aes(x = !!sym(datetime_col), y = P)) +  # Remplacez "P" par le nom de la colonne de consommation si différent
  geom_line(color = "blue") +
  labs(title = "Consommation au Fil du Temps", x = "Temps", y = "Consommation")

# Extraction de caractéristiques : Extraire des caractéristiques utiles à partir du timestamp
data_rse <- data_rse %>%
  mutate(
    annee = year(!!sym(datetime_col)),
    mois = month(!!sym(datetime_col)),
    jour = day(!!sym(datetime_col)),
    heure = hour(!!sym(datetime_col))
  )

# Afficher les premières lignes après l'extraction des caractéristiques
head(data_rse)
