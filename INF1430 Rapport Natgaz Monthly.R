# Charger les bibliothèques nécessaires
library(tidyverse)
library(lubridate)
library(randomForest)
library(caret)
library(corrplot)

# Définir le chemin correct vers le fichier CSV
csv_path <- "/home/christian/ProjetsessionTELUQ/Projet Finale TELUQ/DATABASE Project/Gaz/NaturalGas_Monthly.csv"

# Charger les données CSV
natural_gas_data <- read_csv(csv_path)

# Afficher les noms des colonnes pour vérifier les éventuelles divergences
print(colnames(natural_gas_data))

# Préparer les données
# Séparer la colonne "Month Year" en deux colonnes "Month" et "Year"
natural_gas_data <- natural_gas_data %>%
  separate(`Month Year`, into = c("Month", "Year"), sep = " ") %>%
  mutate(Date = parse_date(paste(Year, Month, "01", sep = "-"), format = "%Y-%b-%d")) %>%
  select(Date, Net_Consumption_GJ = `Net Consumption (GJ)`, Avg_Outside_Temp_C = `Avg Outside Temp (℃)`)

# Gérer les valeurs manquantes
# Supprimer les lignes contenant des valeurs NA
natural_gas_data <- natural_gas_data %>%
  drop_na()

# Diviser les données en ensembles d'entraînement et de test
set.seed(123)
train_indices <- sample(seq_len(nrow(natural_gas_data)), size = 0.7 * nrow(natural_gas_data))
train_data <- natural_gas_data[train_indices, ]
test_data <- natural_gas_data[-train_indices, ]

# Modèle de régression linéaire
lm_model <- lm(Net_Consumption_GJ ~ Date + Avg_Outside_Temp_C, data = train_data)
lm_predictions <- predict(lm_model, test_data)
lm_rmse <- sqrt(mean((lm_predictions - test_data$Net_Consumption_GJ)^2))

# Modèle de forêt aléatoire
rf_model <- randomForest(Net_Consumption_GJ ~ Date + Avg_Outside_Temp_C, data = train_data)
rf_predictions <- predict(rf_model, test_data)
rf_rmse <- sqrt(mean((rf_predictions - test_data$Net_Consumption_GJ)^2))

# Analyse de corrélation
cor_matrix <- cor(natural_gas_data %>% select(Net_Consumption_GJ, Avg_Outside_Temp_C))

# Identifier les périodes de consommation extrême
extreme_consumption <- natural_gas_data %>%
  filter(Net_Consumption_GJ == max(Net_Consumption_GJ) | Net_Consumption_GJ == min(Net_Consumption_GJ))

# Visualiser les résultats
# Graphique 1 : Consommation réelle vs prédite (Régression linéaire)
lm_plot <- ggplot() +
  geom_point(aes(x = test_data$Net_Consumption_GJ, y = lm_predictions), color = 'blue') +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', color = 'red') +
  labs(title = 'Consommation réelle vs prédite (Régression linéaire)', x = 'Consommation réelle', y = 'Consommation prédite') +
  theme_minimal()

# Graphique 2 : Matrice de corrélation
# Utiliser corrplot directement sans ggsave car corrplot génère des graphiques de base
corrplot(cor_matrix, method = 'circle')
ggsave('correlation_matrix.png')

# Graphique 3 : Périodes de consommation extrême
extreme_plot <- ggplot(extreme_consumption) +
  geom_line(aes(x = Date, y = Net_Consumption_GJ), color = 'purple') +
  labs(title = 'Périodes de consommation extrême', x = 'Date', y = 'Consommation (GJ)') +
  theme_minimal()

# Enregistrer les graphiques
ggsave('lm_actual_vs_predicted.png', plot = lm_plot, device = 'png')
ggsave('extreme_consumption_periods.png', plot = extreme_plot, device = 'png')

# Imprimer les valeurs RMSE
cat('Régression linéaire RMSE:', lm_rmse, '\n')
cat('Forêt aléatoire RMSE:', rf_rmse, '\n')

# Afficher les graphiques
print(lm_plot)
print(extreme_plot)
