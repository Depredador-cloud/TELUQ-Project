# Charger les bibliothèques nécessaires
library(tidyverse)
library(lubridate)
library(randomForest)
library(caret)
library(ggplot2)
library(readr)

# Chemin du fichier HTE
path_hte <- "/home/christian/ProjetsessionTELUQ/Projet Finale TELUQ/DATABASE Project/Electricity/Electricity_HTE.csv"

# Vérifier si le fichier existe
if (!file.exists(path_hte)) {
  stop("Le fichier n'existe pas à l'emplacement spécifié.")
}

# Charger les données HTE
data_hte <- read_csv(path_hte)

# Afficher les premières lignes du jeu de données
head(data_hte)

# Vérifier les noms des colonnes
colnames(data_hte)

# Utiliser la colonne de date et heure correcte
datetime_col <- "unix_ts"  # Assurez-vous que c'est le bon nom de colonne pour les dates et heures dans votre fichier

# Assurez-vous que la colonne de date et heure existe
if (!datetime_col %in% colnames(data_hte)) {
  stop(paste("La colonne", datetime_col, "n'existe pas dans le fichier CSV."))
}

# Vérifiez si la colonne est numérique
if (!is.numeric(data_hte[[datetime_col]])) {
  stop(paste("La colonne", datetime_col, "n'est pas numérique."))
}

# Convertir la colonne de date et heure en objet Date-Heure
data_hte[[datetime_col]] <- as.POSIXct(data_hte[[datetime_col]], origin = "1970-01-01")

# Vérifier les valeurs manquantes
sum(is.na(data_hte))

# Nettoyage des données : Gérer les valeurs manquantes si nécessaire
data_hte <- na.omit(data_hte)

# Analyse exploratoire des données : Statistiques descriptives
summary(data_hte)

# Visualiser la consommation HTE au fil du temps
ggplot(data_hte, aes(x = !!sym(datetime_col), y = P)) +  # Remplacez "P" par le nom de la colonne de consommation si différent
  geom_line(color = "blue") +
  labs(title = "Consommation HTE au Fil du Temps", x = "Temps", y = "Consommation HTE")

# Extraction de caractéristiques : Extraire des caractéristiques utiles à partir du timestamp
data_hte <- data_hte %>%
  mutate(
    annee = year(!!sym(datetime_col)),
    mois = month(!!sym(datetime_col)),
    jour = day(!!sym(datetime_col)),
    heure = hour(!!sym(datetime_col))
  )

# Afficher les premières lignes après l'extraction des caractéristiques
head(data_hte)
