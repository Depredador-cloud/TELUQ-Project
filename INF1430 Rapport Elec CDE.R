# Charger les bibliothèques nécessaires
library(tidyverse)
library(lubridate)
library(ggplot2)

# Chemin du fichier
path_electricity_cde <- "/home/christian/ProjetsessionTELUQ/Projet Finale TELUQ/DATABASE Project/Electricity/Electricity_CDE.csv"

# Vérifier si le fichier existe
if (!file.exists(path_electricity_cde)) {
  stop("Le fichier Electricity_CDE.csv n'existe pas à l'emplacement spécifié.")
} else {
  print("Le fichier Electricity_CDE.csv a été trouvé.")
}

# Charger les données
electricity_cde <- read.csv(path_electricity_cde)

# Vérifier les noms des colonnes
print(colnames(electricity_cde))

# Convertir le timestamp en date pour CDE
electricity_cde$unix_ts <- as.POSIXct(electricity_cde$unix_ts, origin = "1970-01-01")

# Agrégation des données à des intervalles horaires pour CDE
data_minute_cde <- electricity_cde %>%
  mutate(minute = floor_date(unix_ts, "minute")) %>%
  group_by(minute) %>%
  summarise(consumption_cde = mean(P, na.rm = TRUE))

# Créer le graphique
electricity_plot <- ggplot(data_minute_cde, aes(x = minute, y = consumption_cde)) +
  geom_line(color = "blue") +
  labs(title = "Consommation d'Électricité au Fil du Temps (CDE)",
       x = "Date",
       y = "Consommation d'Électricité (kW)") +
  theme_minimal()

# Afficher le graphique
print(electricity_plot)
