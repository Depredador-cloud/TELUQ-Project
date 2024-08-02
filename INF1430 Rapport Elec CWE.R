# Charger les bibliothèques nécessaires
library(tidyverse)
library(lubridate)
library(randomForest)
library(caret)
library(ggplot2)
library(readr)

# Chemin du fichier CWE
path_cwe <- "/home/christian/ProjetsessionTELUQ/Projet Finale TELUQ/DATABASE Project/Electricity/Electricity_CWE.csv"

# Vérifier si le fichier existe
if (!file.exists(path_cwe)) {
  stop("Le fichier n'existe pas à l'emplacement spécifié.")
}

# Charger les données CWE
data_cwe <- read_csv(path_cwe)

# Afficher les premières lignes du jeu de données
head(data_cwe)

# Vérifier les noms des colonnes
colnames(data_cwe)

# Utiliser la colonne de date et heure correcte
datetime_col <- "unix_ts"

# Assurez-vous que la colonne de date et heure existe
if (!datetime_col %in% colnames(data_cwe)) {
  stop(paste("La colonne", datetime_col, "n'existe pas dans le fichier CSV."))
}

# Vérifiez si la colonne est numérique
if (!is.numeric(data_cwe[[datetime_col]])) {
  stop(paste("La colonne", datetime_col, "n'est pas numérique."))
}

# Convertir la colonne de date et heure en objet Date-Heure
data_cwe[[datetime_col]] <- as.POSIXct(data_cwe[[datetime_col]], origin = "1970-01-01")

# Vérifier les valeurs manquantes
sum(is.na(data_cwe))

# Nettoyage des données : Gérer les valeurs manquantes si nécessaire
data_cwe <- na.omit(data_cwe)

# Analyse exploratoire des données : Statistiques descriptives
summary(data_cwe)

# Visualiser la consommation CWE au fil du temps
ggplot(data_cwe, aes(x = !!sym(datetime_col), y = P)) +
  geom_line(color = "blue") +
  labs(title = "Consommation CWE au Fil du Temps", x = "Temps", y = "Consommation CWE")

# Extraction de caractéristiques : Extraire des caractéristiques utiles à partir du timestamp
data_cwe <- data_cwe %>%
  mutate(
    annee = year(!!sym(datetime_col)),
    mois = month(!!sym(datetime_col)),
    jour = day(!!sym(datetime_col)),
