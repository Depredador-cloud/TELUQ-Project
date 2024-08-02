# Charger les bibliothèques nécessaires
library(tidyverse)
library(lubridate)
library(randomForest)
library(caret)
library(ggplot2)
library(readr)

# Chemin du fichier FRE
path_fre <- "/home/christian/ProjetsessionTELUQ/Projet Finale TELUQ/DATABASE Project/Electricity/Electricity_FRE.csv"

# Vérifier si le fichier existe
if (!file.exists(path_fre)) {
  stop("Le fichier n'existe pas à l'emplacement spécifié.")
}

# Charger les données FRE
data_fre <- read_csv(path_fre)

# Afficher les premières lignes du jeu de données
head(data_fre)

# Vérifier les noms des colonnes
colnames(data_fre)

# Utiliser la colonne de date et heure correcte
datetime_col <- "unix_ts"  # Assurez-vous que c'est le bon nom de colonne pour les dates et heures dans votre fichier

# Assurez-vous que la colonne de date et heure existe
if (!datetime_col %in% colnames(data_fre)) {
  stop(paste("La colonne", datetime_col, "n'existe pas dans le fichier CSV."))
}

# Vérifiez si la colonne est numérique
if (!is.numeric(data_fre[[datetime_col]])) {
  stop(paste("La colonne", datetime_col, "n'est pas numérique."))
}

# Convertir la colonne de date et heure en objet Date-Heure
data_fre[[datetime_col]] <- as.POSIXct(data_fre[[datetime_col]], origin = "1970-01-01")

# Vérifier les valeurs manquantes
sum(is.na(data_fre))

# Nettoyage des données : Gérer les valeurs manquantes si nécessaire
data_fre <- na.omit(data_fre)

# Analyse exploratoire des données : Statistiques descriptives
summary(data_fre)

# Visualiser la consommation FRE au fil du temps
ggplot(data_fre, aes(x = !!sym(datetime_col), y = P)) +  # Remplacez "P" par le nom de la colonne de consommation si différent
  geom_line(color = "blue") +
  labs(title = "Consommation FRE au Fil du Temps", x = "Temps", y = "Consommation FRE")

# Extraction de caractéristiques : Extraire des caractéristiques utiles à partir du timestamp
data_fre <- data_fre %>%
  mutate(
    annee = year(!!sym(datetime_col)),
    mois = month(!!sym(datetime_col)),
    jour = day(!!sym(datetime_col)),
    heure = hour(!!sym(datetime_col))
  )

# Afficher les premières lignes après l'extraction des caractéristiques
head(data_fre)
