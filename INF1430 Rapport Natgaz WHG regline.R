# Charger les bibliothèques nécessaires
library(tidyverse)
library(lubridate)
library(corrplot)

# Définir le chemin correct vers le fichier CSV
csv_path <- "/home/christian/ProjetsessionTELUQ/Projet Finale TELUQ/DATABASE Project/Gaz/NaturalGas_WHG.csv"

# Charger les données CSV
natural_gas_data <- read_csv(csv_path)

# Afficher les noms des colonnes pour vérifier les éventuelles divergences
print(colnames(natural_gas_data))

# Préparer les données
# Ajouter une colonne Date à partir du timestamp unix_ts
natural_gas_data <- natural_gas_data %>%
  mutate(Date = as_datetime(unix_ts, origin = "1970-01-01"),
         Day = day(Date),
         Month = month(Date),
         Year = year(Date))

# Sélectionner uniquement les colonnes nécessaires
natural_gas_data_clean <- natural_gas_data %>%
  select(Date, counter, avg_rate, inst_rate, Day, Month, Year)

# Transposer les données pour obtenir une structure plus adaptée
natural_gas_data_long <- natural_gas_data_clean %>%
  pivot_longer(cols = c(counter, avg_rate, inst_rate), names_to = "Metric", values_to = "Consumption") %>%
  arrange(Date, Day)

# Gérer les valeurs manquantes
# Supprimer les lignes contenant des valeurs NA
natural_gas_data_long <- natural_gas_data_long %>%
  drop_na()

# Diviser les données en ensembles d'entraînement et de test
set.seed(123)
train_indices <- sample(seq_len(nrow(natural_gas_data_long)), size = 0.7 * nrow(natural_gas_data_long))
train_data <- natural_gas_data_long[train_indices, ]
test_data <- natural_gas_data_long[-train_indices, ]

# Modèle de régression linéaire
lm_model <- lm(Consumption ~ Day + Month + Year, data = train_data)
lm_predictions <- predict(lm_model, test_data)
lm_rmse <- sqrt(mean((lm_predictions - test_data$Consumption)^2))

# Visualiser les résultats
# Graphique : Consommation réelle vs prédite (Régression linéaire)
lm_plot <- ggplot() +
  geom_point(aes(x = test_data$Consumption, y = lm_predictions), color = 'blue') +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', color = 'red') +
  labs(title = 'Consommation réelle vs prédite (Régression linéaire)', x = 'Consommation réelle', y = 'Consommation prédite') +
  theme_minimal()

# Enregistrer le graphique
ggsave('lm_actual_vs_predicted.png', plot = lm_plot, device = 'png')

# Imprimer les valeurs RMSE
cat('Régression linéaire RMSE:', lm_rmse, '\n')

# Afficher le graphique
print(lm_plot)
