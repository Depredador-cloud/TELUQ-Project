# Charger les bibliothèques nécessaires
library(tidyverse)
library(lubridate)
library(randomForest)
library(caret)
library(ggplot2)
library(readr)

# Chemin du fichier Q
path_q <- "/home/christian/ProjetsessionTELUQ/Projet Finale TELUQ/DATABASE Project/Electricity/Electricity_Q.csv"

# Vérifier si le fichier existe
if (!file.exists(path_q)) {
  stop("Le fichier n'existe pas à l'emplacement spécifié.")
}

# Charger les données Q
data_q <- read_csv(path_q)

# Afficher les premières lignes du jeu de données
head(data_q)

# Vérifier les noms des colonnes
colnames(data_q)

# Utiliser la colonne de date et heure correcte
datetime_col <- "UNIX_TS"  # Assurez-vous que c'est le bon nom de colonne pour les dates et heures dans votre fichier

# Assurez-vous que la colonne de date et heure existe
if (!datetime_col %in% colnames(data_q)) {
  stop(paste("La colonne", datetime_col, "n'existe pas dans le fichier CSV."))
}

# Convertir la colonne de date et heure en objet Date-Heure
data_q[[datetime_col]] <- as.POSIXct(data_q[[datetime_col]], origin = "1970-01-01", tz = "UTC")

# Vérifier les valeurs manquantes
sum(is.na(data_q))

# Nettoyage des données : Gérer les valeurs manquantes si nécessaire
data_q <- na.omit(data_q)

# Analyse exploratoire des données : Statistiques descriptives
summary(data_q)

# Visualiser la consommation Q au fil du temps
ggplot(data_q, aes(x = !!sym(datetime_col), y = WHE)) +  # Remplacez "WHE" par le nom de la colonne de consommation si différent
  geom_line(color = "blue") +
  labs(title = "Consommation Q au Fil du Temps", x = "Temps", y = "Consommation Q")

# Extraction de caractéristiques : Extraire des caractéristiques utiles à partir du timestamp
data_q <- data_q %>%
  mutate(
    annee = year(!!sym(datetime_col)),
    mois = month(!!sym(datetime_col)),
    jour = day(!!sym(datetime_col)),
    heure = hour(!!sym(datetime_col))
  )

# Afficher les premières lignes après l'extraction des caractéristiques
head(data_q)
