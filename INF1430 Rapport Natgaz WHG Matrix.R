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

# Calculer la matrice de corrélation
cor_matrix <- cor(natural_gas_data_clean %>% select(-Date))

# Visualiser la matrice de corrélation
corrplot(cor_matrix, method = 'circle', type = 'upper')

# Sauvegarder la matrice de corrélation
ggsave('correlation_matrix.png')
