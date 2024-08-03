# Charger les bibliothèques nécessaires
library(tidyverse)
library(lubridate)
library(ggplot2)
library(caret)

# Définir le chemin correct vers le fichier CSV
csv_path <- "/home/christian/ProjetsessionTELUQ/Projet Finale TELUQ/DATABASE Project/Water/Water_HTW.csv"

# Charger les données CSV
if (file.exists(csv_path)) {
  water_data <- read_csv(csv_path)
} else {
  stop("Le fichier CSV n'existe pas au chemin spécifié.")
}

# Vérifier les noms des colonnes pour s'assurer qu'elles existent dans le jeu de données
print(colnames(water_data))

# Afficher un aperçu des données
print(head(water_data))

# Préparer les données
# Ajouter une colonne Date à partir du timestamp unix_ts
water_data <- water_data %>%
  mutate(Date = as_datetime(unix_ts, origin = "1970-01-01"),
         Day = day(Date),
         Month = month(Date),
         Year = year(Date))

# Sélectionner uniquement les colonnes nécessaires pour l'analyse
water_data_clean <- water_data %>%
  select(Date, counter, avg_rate, Day, Month, Year)

# Diviser les données en ensembles d'entraînement et de test
set.seed(123)
train_indices <- sample(seq_len(nrow(water_data_clean)), size = 0.7 * nrow(water_data_clean))
train_data <- water_data_clean[train_indices, ]
test_data <- water_data_clean[-train_indices, ]

# Modèle de régression linéaire
lm_model <- lm(avg_rate ~ Day + Month + Year, data = train_data)
lm_predictions <- predict(lm_model, test_data)
lm_rmse <- sqrt(mean((lm_predictions - test_data$avg_rate)^2))

# Afficher les RMSE
cat('Régression linéaire RMSE:', lm_rmse, '\n')

# Visualiser les résultats de la régression linéaire
lm_plot <- ggplot(test_data, aes(x = avg_rate, y = lm_predictions)) +
  geom_point(color = 'blue') +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', color = 'red') +
  labs(title = 'Consommation réelle vs prédite (Régression linéaire)', 
       x = 'Consommation réelle (avg_rate)', 
       y = 'Consommation prédite') +
  theme_minimal()

# Enregistrer les graphiques
ggsave('lm_actual_vs_predicted.png', plot = lm_plot, device = 'png')

# Afficher les graphiques
print(lm_plot)

# Trouver les périodes de consommation extrême
extreme_consumption <- water_data_clean %>%
  filter(avg_rate == max(avg_rate) | avg_rate == min(avg_rate) |
           counter == max(counter) | counter == min(counter))

# Visualiser les périodes de consommation extrême
extreme_plot <- ggplot(extreme_consumption) +
  geom_line(aes(x = Date, y = avg_rate, color = "avg_rate"), linewidth = 1) +
  geom_line(aes(x = Date, y = counter, color = "counter"), linewidth = 1) +
  labs(title = 'Périodes de consommation extrême', x = 'Date', y = 'Consommation') +
  scale_color_manual(name = "Metrics", values = c("avg_rate" = "blue", "counter" = "green")) +
  theme_minimal()

# Enregistrer le graphique
ggsave('extreme_consumption_periods.png', plot = extreme_plot, device = 'png')

# Afficher le graphique
print(extreme_plot)
