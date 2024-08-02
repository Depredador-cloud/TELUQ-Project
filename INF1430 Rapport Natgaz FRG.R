# Load necessary libraries
library(tidyverse)
library(lubridate)
library(ggplot2)

# Define the correct path to the CSV file
csv_path <- "/home/christian/ProjetsessionTELUQ/Projet Finale TELUQ/DATABASE Project/Gaz/NaturalGas_FRG.csv"

# Load the CSV data
natural_gas_frg <- read_csv(csv_path)

# Print column names to check for any discrepancies
print(colnames(natural_gas_frg))

# Prepare the data
# Convert unix timestamp to date
natural_gas_frg <- natural_gas_frg %>%
  mutate(Date = as_datetime(unix_ts)) %>%
  arrange(Date) %>%
  drop_na()

# Check the structure of the data
str(natural_gas_frg)

# Create a plot to illustrate the natural gas consumption over time
consumption_plot <- ggplot(natural_gas_frg, aes(x = Date, y = avg_rate)) +
  geom_line(color = 'blue') +
  labs(title = 'Natural Gas Consumption Over Time',
       x = 'Date',
       y = 'Average Rate') +
  theme_minimal()

# Display the plot
print(consumption_plot)

# Save the plot
ggsave('natural_gas_consumption_frg.png', plot = consumption_plot, device = 'png')
