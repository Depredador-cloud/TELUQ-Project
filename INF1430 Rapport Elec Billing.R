# Charger les bibliothèques nécessaires
library(tidyverse)
library(lubridate)
library(ggplot2)

# Chemins des fichiers
path_electricity_billing <- "/home/christian/ProjetsessionTELUQ/Projet Finale TELUQ/DATABASE Project/Electricity/Electricity_Billing.csv"
path_ampds2_data <- "/home/christian/ProjetsessionTELUQ/Projet Finale TELUQ/Data Core AMPds2.RData"

# Vérifier si les fichiers existent
if (!file.exists(path_electricity_billing)) {
  stop("Le fichier Electricity_Billing.csv n'existe pas à l'emplacement spécifié.")
} else {
  print("Le fichier Electricity_Billing.csv a été trouvé.")
}

if (!file.exists(path_ampds2_data)) {
  stop("Le fichier Data Core AMPds2.RData n'existe pas à l'emplacement spécifié.")
} else {
  print("Le fichier Data Core AMPds2.RData a été trouvé.")
}

# Charger les données CSV
electricity_billing <- read.csv(path_electricity_billing)

# Charger les données RData
load(path_ampds2_data)

# Afficher les premières lignes des données chargées pour vérifier le contenu
head(electricity_billing)

# Convertir les dates en objets date
electricity_billing$Invoice.Date <- as.Date(electricity_billing$Invoice.Date)
electricity_billing$From.Date <- as.Date(electricity_billing$From.Date)
electricity_billing$To.Date <- as.Date(electricity_billing$To.Date)

# Agrégation des données par mois
data_monthly_billing <- electricity_billing %>%
  mutate(month = floor_date(From.Date, "month")) %>%
  group_by(month) %>%
  summarise(monthly_usage = sum(kWh.Usage, na.rm = TRUE))

# Créer le graphique
electricity_plot <- ggplot(data_monthly_billing, aes(x = month, y = monthly_usage)) +
  geom_line(color = "blue") +
  labs(title = "Consommation Mensuelle d'Électricité",
       x = "Date",
       y = "Consommation d'Électricité (kWh)") +
  theme_minimal()

# Afficher le graphique
print(electricity_plot)
