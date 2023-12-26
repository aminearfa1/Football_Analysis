library(tidyverse)
library(dplyr)

# Charger les données
goalscorers <- read.csv("goalscorers.csv")
results <- read.csv("results.csv")

# Fusionner les données de buteurs et de résultats
merged_data <- merge(goalscorers, results, by = c("date", "home_team", "away_team"))

# Calculer les meilleurs buteurs
top_scorers <- goalscorers %>% 
  group_by(scorer) %>% 
  summarise(total_goals = n()) %>% 
  arrange(desc(total_goals)) %>% 
  top_n(10, total_goals)

top_home_scorers <- merged_data %>% 
  filter(team == home_team) %>% 
  group_by(scorer) %>% 
  summarise(home_goals = n()) %>% 
  arrange(desc(home_goals)) %>% 
  top_n(10, home_goals)

top_away_scorers <- merged_data %>% 
  filter(team == away_team) %>% 
  group_by(scorer) %>% 
  summarise(away_goals = n()) %>% 
  arrange(desc(away_goals)) %>% 
  top_n(10, away_goals)

# Fonction pour créer un diagramme circulaire
create_pie_chart <- function(data, count_column, title) {
  ggplot(data, aes(x = "", y = !!sym(count_column), fill = scorer)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) +
    theme_void() +
    labs(title = title, fill = "Scorer") +
    geom_text(aes(label = !!sym(count_column)), position = position_stack(vjust = 0.5)) +
    theme(plot.title = element_text(hjust = 0.5))
}

# Créer les diagrammes
plot1 <- create_pie_chart(top_scorers, "total_goals", "Top 10 Goal Scorers")
plot2 <- create_pie_chart(top_home_scorers, "home_goals", "Top 10 Home Goal Scorers")
plot3 <- create_pie_chart(top_away_scorers, "away_goals", "Top 10 Away Goal Scorers")

# Afficher les diagrammes
print(plot1)
print(plot2)
print(plot3)
