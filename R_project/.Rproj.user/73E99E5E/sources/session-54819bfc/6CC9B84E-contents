# Installer et charger le package ggplot2
library(ggplot2)

# Importer les données dans un dataframe (df)
df <- read.csv("results.csv")

# Convertir la première colonne en date
df$Date <- as.Date(df[, 1])

# Créer un graphique avec ggplot2
ggplot(df, aes(x = Date)) +
  geom_line(aes(y = home_score, color = "Home Score"), size = 1) +
  geom_line(aes(y = away_score, color = "Away Score"), size = 1) +
  labs(title = "Évolution des scores au fil du temps",
       x = "Date",
       y = "Score") +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_color_manual(values = c("Home Score" = "blue", "Away Score" = "red"))

