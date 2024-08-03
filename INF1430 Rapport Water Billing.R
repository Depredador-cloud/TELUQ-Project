# Charger les bibliothèques nécessaires
library(tidyverse)
library(lubridate)
library(ggplot2)
library(randomForest)
library(caret)
library(corrplot)

# Définir le chemin correct vers le fichier CSV
csv_path <- "/home/christian/ProjetsessionTELUQ/Projet Finale TELUQ/DATABASE Project/Water/Water_Billing.csv"

# Charger les données CSV
water_data <- read_csv(csv_path)

# Afficher les noms des colonnes pour vérifier les éventuelles divergences
print(colnames(water_data))

# Afficher un aperçu des données
print(head(water_data))

# Préparer les données
# Ajouter une colonne Date fictive pour permettre la manipulation des dates
# Étant donné que les données sont annuelles, nous allons créer une date fictive pour chaque année
water_data <- water_data %>%
  mutate(Date = as.Date(paste0(Year, "-01-01")),
         Day = 1,
         Month = 1)

# Sélectionner uniquement les colonnes nécessaires pour l'analyse
water_data_clean <- water_data %>%
  select(Date, `Flat Water`, `Sewer Pacel`, `Sewer Use`, `Garbage Disposal Fee`, `Total Fees`, Day, Month, Year)

# Diviser les données en ensembles d'entraînement et de test
set.seed(123)
train_indices <- sample(seq_len(nrow(water_data_clean)), size = 0.7 * nrow(water_data_clean))
train_data <- water_data_clean[train_indices, ]
test_data <- water_data_clean[-train_indices, ]

# Modèle de régression linéaire
lm_model <- lm(`Total Fees` ~ Day + Month + Year, data = train_data)
lm_predictions <- predict(lm_model, test_data)
lm_rmse <- sqrt(mean((lm_predictions - test_data$`Total Fees`)^2))

# Modèle de forêt aléatoire
rf_model <- randomForest(`Total Fees` ~ Day + Month + Year, data = train_data)
rf_predictions <- predict(rf_model, test_data)
rf_rmse <- sqrt(mean((rf_predictions - test_data$`Total Fees`)^2))

# Analyse de corrélation
cor_matrix <- cor(water_data_clean %>% select(-Date))

# Trouver les périodes de consommation extrême
extreme_consumption <- water_data_clean %>%
  filter(`Total Fees` == max(`Total Fees`) | `Total Fees` == min(`Total Fees`) |
           `Flat Water` == max(`Flat Water`) | `Flat Water` == min(`Flat Water`) |
           `Sewer Pacel` == max(`Sewer Pacel`) | `Sewer Pacel` == min(`Sewer Pacel`) |
           `Sewer Use` == max(`Sewer Use`) | `Sewer Use` == min(`Sewer Use`) |
           `Garbage Disposal Fee` == max(`Garbage Disposal Fee`) | `Garbage Disposal Fee` == min(`Garbage Disposal Fee`))

# Visualiser les périodes de consommation extrême
extreme_plot <- ggplot(extreme_consumption) +
  geom_line(aes(x = Date, y = `Total Fees`, color = "Total Fees"), linewidth = 1) +
  geom_line(aes(x = Date, y = `Flat Water`, color = "Flat Water"), linewidth = 1) +
  geom_line(aes(x = Date, y = `Sewer Pacel`, color = "Sewer Pacel"), linewidth = 1) +
  geom_line(aes(x = Date, y = `Sewer Use`, color = "Sewer Use"), linewidth = 1) +
  geom_line(aes(x = Date, y = `Garbage Disposal Fee`, color = "Garbage Disposal Fee"), linewidth = 1) +
  labs(title = 'Périodes de consommation extrême', x = 'Date', y = 'Consommation') +
  scale_color_manual(name = "Metrics", values = c("Total Fees" = "blue", "Flat Water" = "green", "Sewer Pacel" = "red", "Sewer Use" = "purple", "Garbage Disposal Fee" = "orange")) +
  theme_minimal()

# Enregistrer le graphique
ggsave('extreme_consumption_periods.png', plot = extreme_plot, device = 'png')

# Afficher le graphique
print(extreme_plot)

# Visualiser les résultats de la régression linéaire
lm_plot <- ggplot() +
  geom_point(aes(x = test_data$`Total Fees`, y = lm_predictions), color = 'blue') +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', color = 'red') +
  labs(title = 'Consommation réelle vs prédite (Régression linéaire)', x = 'Consommation réelle', y = 'Consommation prédite') +
  theme_minimal()

# Enregistrer les graphiques
ggsave('lm_actual_vs_predicted.png', plot = lm_plot, device = 'png')

# Afficher les graphiques
print(lm_plot)

# Imprimer les valeurs RMSE
cat('Régression linéaire RMSE:', lm_rmse, '\n')
cat('Forêt aléatoire RMSE:', rf_rmse, '\n')
