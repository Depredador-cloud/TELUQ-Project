# Charger les bibliothèques nécessaires
library(tidyverse)
library(lubridate)
library(ggplot2)

# Chemins des fichiers
path_electricity_b2e <- "/home/christian/ProjetsessionTELUQ/Projet Finale TELUQ/DATABASE Project/Electricity/Electricity_B2E.csv"

# Vérifier si le fichier existe
if (!file.exists(path_electricity_b2e)) {
  stop("Le fichier n'existe pas à l'emplacement spécifié.")
}

# Charger les données
electricity_b2e <- read.csv(path_electricity_b2e)

# Convertir le timestamp en date
electricity_b2e$unix_ts <- as.POSIXct(electricity_b2e$unix_ts, origin = "1970-01-01")

# Agrégation des données à des intervalles horaires
data_minute_b2e <- electricity_b2e %>%
  mutate(minute = floor_date(unix_ts, "minute")) %>%
  group_by(minute) %>%
  summarise(consumption_b2e = mean(P, na.rm = TRUE))

# Créer le graphique
electricity_plot <- ggplot(data_minute_b2e, aes(x = minute)) +
  geom_line(aes(y = consumption_b2e, color = "B2E")) +
  labs(title = "Consommation d'Électricité au Fil du Temps (B2E)",
       x = "Date",
       y = "Consommation d'Électricité (kW)",
       color = "Type") +
  theme_minimal()

# Afficher le graphique
print(electricity_plot)
