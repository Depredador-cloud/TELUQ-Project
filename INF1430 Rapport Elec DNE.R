# Charger les bibliothèques nécessaires
library(tidyverse)
library(lubridate)
library(randomForest)
library(caret)
library(ggplot2)

# Chemin du fichier DNE
path_dne <- "/home/christian/ProjetsessionTELUQ/Projet Finale TELUQ/DATABASE Project/Electricity/Electricity_DNE.csv"

# Vérifier si le fichier existe
if (!file.exists(path_dne)) {
  stop("Le fichier n'existe pas à l'emplacement spécifié.")
}

# Charger les données DNE
data_dne <- read.csv(path_dne)

# Afficher les premières lignes du jeu de données
head(data_dne)

# Vérifier les noms des colonnes
colnames(data_dne)

# Utiliser la colonne de date et heure correcte
datetime_col <- "unix_ts"

# Assurez-vous que la colonne de date et heure existe
if (!datetime_col %in% colnames(data_dne)) {
  stop(paste("La colonne", datetime_col, "n'existe pas dans le fichier CSV."))
}

# Convertir la colonne de date et heure en objet Date-Heure
data_dne[[datetime_col]] <- as.POSIXct(data_dne[[datetime_col]], origin = "1970-01-01")

# Vérifier les valeurs manquantes
sum(is.na(data_dne))

# Nettoyage des données : Gérer les valeurs manquantes si nécessaire
data_dne <- na.omit(data_dne)

# Analyse exploratoire des données : Statistiques descriptives
summary(data_dne)

# Visualiser la consommation DNE au fil du temps
ggplot(data_dne, aes(x = !!sym(datetime_col), y = P)) +
  geom_line(color = "blue") +
  labs(title = "Consommation DNE au Fil du Temps", x = "Temps", y = "Consommation DNE")

# Extraction de caractéristiques : Extraire des caractéristiques utiles à partir du timestamp
data_dne <- data_dne %>%
  mutate(
    annee = year(!!sym(datetime_col)),
    mois = month(!!sym(datetime_col)),
    jour = day(!!sym(datetime_col)),
    heure = hour(!!sym(datetime_col))
  )

# Séparation des données en ensembles d'entraînement et de test
set.seed(123)
index_train <- createDataPartition(data_dne$P, p = 0.8, list = FALSE)
data_train <- data_dne[index_train, ]
data_test <- data_dne[-index_train, ]

# Modèle de Régression Linéaire
mod_lm <- lm(P ~ annee + mois + jour + heure, data = data_train)
summary(mod_lm)

# Prédictions et évaluation pour la Régression Linéaire
pred_lm <- predict(mod_lm, data_test)
rmse_lm <- sqrt(mean((pred_lm - data_test$P)^2))
mae_lm <- mean(abs(pred_lm - data_test$P))

cat("RMSE Régression Linéaire :", rmse_lm, "\
