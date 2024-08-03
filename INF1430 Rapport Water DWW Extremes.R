# Charger les bibliothèques nécessaires
library(tidyverse)
library(lubridate)
library(ggplot2)

# Définir le chemin correct vers le fichier CSV
csv_path <- "/home/christian/ProjetsessionTELUQ/Projet Finale TELUQ/DATABASE Project/Water/Water_DWW.csv"

# Charger les données CSV
water_data <- read_csv(csv_path)

# Afficher les noms des colonnes pour vérifier les éventuelles divergences
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
ggsave('/home/christian/ProjetsessionTELUQ/Projet Finale TELUQ/extreme_consumption_periods.png', plot = extreme_plot, device = 'png')

# Afficher le graphique
print(extreme_plot)
