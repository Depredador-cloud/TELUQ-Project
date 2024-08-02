# Charger les bibliothèques nécessaires
library(tidyverse)
library(lubridate)
library(randomForest)
library(caret)
library(ggplot2)
library(readr)

# Chemin du fichier EBE
path_ebe <- "/home/christian/ProjetsessionTELUQ/Projet Finale TELUQ/DATABASE Project/Electricity/Electricity_EBE.csv"

# Vérifier si le fichier existe
if (!file.exists(path_ebe)) {
  stop("Le fichier n'existe pas à l'emplacement spécifié.")
}

# Charger les données EBE
data_ebe <- read_csv(path_ebe)

# Afficher les premières lignes du jeu de données
head(data_ebe)

# Vérifier les noms des colonnes
colnames(data_ebe)

# Utiliser la colonne de date et heure correcte
datetime_col <- "unix_ts"  # Assurez-vous que c'est le bon nom de colonne pour les dates et heures dans votre fichier

# Assurez-vous que la colonne de date et heure existe
if (!datetime_col %in% colnames(data_ebe)) {
  stop(paste("La colonne", datetime_col, "n'existe pas dans le fichier CSV."))
}

# Vérifiez si la colonne est numérique
if (!is.numeric(data_ebe[[datetime_col]])) {
  stop(paste("La colonne", datetime_col, "n'est pas numérique."))
}

# Convertir la colonne de date et heure en objet Date-Heure
data_ebe[[datetime_col]] <- as.POSIXct(data_ebe[[datetime_col]], origin = "1970-01-01")

# Vérifier les valeurs manquantes
sum(is.na(data_ebe))

# Nettoyage des données : Gérer les valeurs manquantes si nécessaire
data_ebe <- na.omit(data_ebe)

# Analyse exploratoire des données : Statistiques descriptives
summary(data_ebe)

# Visualiser la consommation EBE au fil du temps
ggplot(data_ebe, aes(x = !!sym(datetime_col), y = P)) +  # Remplacez "P" par le nom de la colonne de consommation si différent
  geom_line(color = "blue") +
  labs(title = "Consommation EBE au Fil du Temps", x = "Temps", y = "Consommation EBE")

# Extraction de caractéristiques : Extraire des caractéristiques utiles à partir du timestamp
data_ebe <- data_ebe %>%
  mutate(
    annee = year(!!sym(datetime_col)),
    mois = month(!!sym(datetime_col)),
    jour = day(!!sym(datetime_col)),
    heure = hour(!!sym(datetime_col))
  )

# Afficher les premières lignes après l'extraction des caractéristiques
head(data_ebe)
