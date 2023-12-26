# Chargement des bibliothèques nécessaires
library(ggplot2)
library(dplyr)
library(gridExtra)

# Lecture des données
goalscorers <- read.csv("goalscorers.csv")
results <- read.csv("results.csv")

# Fusion des données de buteurs et de résultats
merged_data <- merge(goalscorers, results, by = c("date", "home_team", "away_team"))

# Ajout d'une colonne de différence de score
merged_data$score_diff <- with(merged_data, home_score - away_score)

# Identification des matchs avec buts CSC et penalties
merged_data$match_with_own_goal <- with(merged_data, own_goal == TRUE)
merged_data$match_with_penalty <- with(merged_data, penalty == TRUE)

# Agrégation pour obtenir le score final des matchs avec et sans CSC/Penalties
own_goal_impact <- merged_data %>%
  group_by(date, home_team, away_team, match_with_own_goal) %>%
  summarise(final_score_diff = max(score_diff)) %>%
  ungroup()

penalty_impact <- merged_data %>%
  group_by(date, home_team, away_team, match_with_penalty) %>%
  summarise(final_score_diff = max(score_diff)) %>%
  ungroup()

# Création des graphiques
p1 <- ggplot(own_goal_impact, aes(x = final_score_diff, fill = match_with_own_goal)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Impact des Buts CSC sur le Score Final",
       x = "Différence de Score Final",
       y = "Nombre de Matchs") +
  scale_fill_discrete(name = "But CSC") +
  theme_minimal()

p2 <- ggplot(penalty_impact, aes(x = final_score_diff, fill = match_with_penalty)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Impact des Penalties sur le Score Final",
       x = "Différence de Score Final",
       y = "Nombre de Matchs") +
  scale_fill_discrete(name = "Penalty") +
  theme_minimal()

# Affichage des graphiques
grid.arrange(p1, p2, ncol = 2)

