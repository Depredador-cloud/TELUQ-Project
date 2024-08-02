# Charger les bibliothèques nécessaires
library(tidyverse)
library(lubridate)
library(ggplot2)

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

# Trouver les périodes de consommation extrême
extreme_consumption <- natural_gas_data_clean %>%
  filter(counter == max(counter) | counter == min(counter) |
           avg_rate == max(avg_rate) | avg_rate == min(avg_rate) |
           inst_rate == max(inst_rate) | inst_rate == min(inst_rate))

# Visualiser les périodes de consommation extrême
extreme_plot <- ggplot(extreme_consumption) +
  geom_line(aes(x = Date, y = counter, color = "Counter"), size = 1) +
  geom_line(aes(x = Date, y = avg_rate, color = "Avg Rate"), size = 1) +
  geom_line(aes(x = Date, y = inst_rate, color = "Inst Rate"), size = 1) +
  labs(title = 'Périodes de consommation extrême', x = 'Date', y = 'Consommation') +
  scale_color_manual(name = "Metrics", values = c("Counter" = "blue", "Avg Rate" = "green", "Inst Rate" = "red")) +
  theme_minimal()

# Enregistrer le graphique
ggsave('extreme_consumption_periods.png', plot = extreme_plot, device = 'png')

# Afficher le graphique
print(extreme_plot)
