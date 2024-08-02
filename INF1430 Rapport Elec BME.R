# Charger les bibliothèques nécessaires
library(tidyverse)
library(lubridate)
library(ggplot2)

# Chemin du fichier
path_electricity_bme <- "/home/christian/ProjetsessionTELUQ/Projet Finale TELUQ/DATABASE Project/Electricity/Electricity_BME.csv"

# Vérifier si le fichier existe
if (!file.exists(path_electricity_bme)) {
  stop("Le fichier Electricity_BME.csv n'existe pas à l'emplacement spécifié.")
} else {
  print("Le fichier Electricity_BME.csv a été trouvé.")
}

# Charger les données
electricity_bme <- read.csv(path_electricity_bme)

# Vérifier les noms des colonnes
print(colnames(electricity_bme))

# Convertir le timestamp en date pour BME
electricity_bme$unix_ts <- as.POSIXct(electricity_bme$unix_ts, origin = "1970-01-01")

# Agrégation des données à des intervalles horaires pour BME
data_minute_bme <- electricity_bme %>%
  mutate(minute = floor_date(unix_ts, "minute")) %>%
  group_by(minute) %>%
  summarise(consumption_bme = mean(P, na.rm = TRUE))

# Créer le graphique
electricity_plot <- ggplot(data_minute_bme, aes(x = minute, y = consumption_bme)) +
  geom_line(color = "blue") +
  labs(title = "Consommation d'Électricité au Fil du Temps (BME)",
       x = "Date",
       y = "Consommation d'Électricité (kW)") +
  theme_minimal()

# Afficher le graphique
print(electricity_plot)
