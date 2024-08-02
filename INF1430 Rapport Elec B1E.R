# Charger les bibliothèques nécessaires
library(tidyverse)
library(lubridate)
library(ggplot2)

# Chemins des fichiers
path_electricity_b1e <- "/home/christian/ProjetsessionTELUQ/Projet Finale TELUQ/DATABASE Project/Electricity/Electricity_B1E.csv"

# Vérifier si le fichier existe
if (!file.exists(path_electricity_b1e)) {
  stop("Le fichier n'existe pas à l'emplacement spécifié : ", path_electricity_b1e)
}

# Charger les données
electricity_b1e <- read.csv(path_electricity_b1e)

# Convertir le timestamp en date
electricity_b1e$unix_ts <- as.POSIXct(electricity_b1e$unix_ts, origin = "1970-01-01")

# Agrégation des données à des intervalles horaires
data_minute <- electricity_b1e %>%
  mutate(minute = floor_date(unix_ts, "minute")) %>%
  group_by(minute) %>%
  summarise(consumption = mean(P, na.rm = TRUE))

# Créer le graphique
electricity_plot <- ggplot(data_minute, aes(x = minute, y = consumption)) +
  geom_line(color = "blue") +
  labs(title = "Consommation d'Électricité au Fil du Temps",
       x = "Date",
       y = "Consommation d'Électricité (kW)") +
  theme_minimal()

# Afficher le graphique
print(electricity_plot)

