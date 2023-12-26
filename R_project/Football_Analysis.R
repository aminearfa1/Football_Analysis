library(tidyverse)
library(dplyr)
library(gridExtra)
library(lubridate)
library(ggthemes)
library(viridis)
library(patchwork)




######################Analyse des Modèles de Marquage de Buts#########################


# Charger les données
goalscorers <- read.csv("goalscorers.csv")
results <- read.csv("results.csv")

# Fusionner les données de buteurs et de résultats
merged_data <- merge(goalscorers, results, by = c("date", "home_team", "away_team"))

# Calculer les meilleurs buteurs dans chaque catégorie
top_scorers <- goalscorers %>% 
  group_by(scorer) %>% 
  summarise(goals = n()) %>% 
  arrange(desc(goals)) %>% 
  slice_max(order_by = goals, n = 10) %>%
  mutate(percentage = goals / sum(goals) * 100)

top_home_scorers <- merged_data %>% 
  filter(team == home_team) %>% 
  group_by(scorer) %>% 
  summarise(goals = n()) %>% 
  arrange(desc(goals)) %>% 
  slice_max(order_by = goals, n = 10) %>%
  mutate(percentage = goals / sum(goals) * 100)

top_away_scorers <- merged_data %>% 
  filter(team == away_team) %>% 
  group_by(scorer) %>% 
  summarise(goals = n()) %>% 
  arrange(desc(goals)) %>% 
  slice_max(order_by = goals, n = 10) %>%
  mutate(percentage = goals / sum(goals) * 100)

create_pie_chart <- function(data, title) {
  # Filtrer pour enlever les NA avant de calculer les pourcentages
  data <- data %>% filter(!is.na(scorer))
  
  ggplot(data, aes(x = "", y = goals, fill = reorder(scorer, -goals))) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) +
    theme_void() +
    labs(title = title, fill = "Scorer") +
    geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5), size = 3) # Ajuster la taille ici
}

# Ensuite, vous pouvez créer vos diagrammes et les afficher comme avant
plot1 <- create_pie_chart(top_scorers, "Top 10 Goal Scorers")
plot2 <- create_pie_chart(top_home_scorers, "Top 10 Home Goal Scorers")
plot3 <- create_pie_chart(top_away_scorers, "Top 10 Away Goal Scorers")


# Définir la matrice de mise en page pour la disposition en triangle
layout_matrix <- matrix(c(NA, 1, NA, 
                          2, NA, 3), 
                        nrow = 2, byrow = TRUE)

# Afficher les diagrammes en forme de triangle
grid.arrange(plot1, plot2, plot3, layout_matrix = layout_matrix)


##############################Timing des Buts#################################



# Séparer les buts en fonction de la mi-temps
first_half_goals <- goalscorers %>% filter(minute <= 45)
second_half_goals <- goalscorers %>% filter(minute > 45 & minute <= 90)

# Fonction pour créer un histogramme avec une palette de couleurs viridis
create_histogram <- function(data, title, color) {
  ggplot(data, aes(x = minute)) +
    geom_histogram(binwidth = 1, fill = color, color = "white") +
    labs(title = title, x = "Minute du but", y = "Nombre de buts") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12))
}

# Créer un histogramme pour les buts de la première mi-temps
first_half_plot <- create_histogram(first_half_goals, "Distribution des buts en première mi-temps", viridis::viridis(1))

# Créer un histogramme pour les buts de la deuxième mi-temps
second_half_plot <- create_histogram(second_half_goals, "Distribution des buts en deuxième mi-temps", viridis::plasma(1))

# Afficher les deux graphiques côte à côte
grid.arrange(first_half_plot, second_half_plot, ncol = 2)




####################Impact des Buts contre son Camp et des Penalties####################
# Counting own goals and penalty goals
goal_counts <- goalscorers %>%
  summarise(OwnGoals = sum(own_goal, na.rm = TRUE),
            PenaltyGoals = sum(penalty, na.rm = TRUE)) %>%
  gather(key = "GoalType", value = "Count")

# Plotting the bar chart for Own Goals vs Penalty Goals
plot1 <- ggplot(goal_counts, aes(x = GoalType, y = Count, fill = GoalType)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Count of Own Goals vs Penalty Goals", x = "", y = "Count")

# Converting date to Year and summarising data for trend analysis
goalscorers$Year <- format(as.Date(goalscorers$date), "%Y")
annual_data <- goalscorers %>%
  group_by(Year) %>%
  summarise(OwnGoals = sum(own_goal, na.rm = TRUE),
            PenaltyGoals = sum(penalty, na.rm = TRUE))

# Melting data for plotting in ggplot
annual_data_long <- gather(annual_data, key = "GoalType", value = "Count", -Year)

# Plotting the line graph for the trend of Own Goals and Penalty Goals Over Time
plot2 <- ggplot(annual_data_long, aes(x = as.numeric(Year), y = Count, color = GoalType)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Trend of Own Goals and Penalty Goals Over Time", x = "Year", y = "Count")


################################



# Fusionner les données
combined_data <- merge(goalscorers, results, by = c("date", "home_team", "away_team"))

# Calculer la différence de score pour chaque match
combined_data$score_difference <- combined_data$home_score - combined_data$away_score

# Créer un indicateur pour les matchs avec buts contre son camp et penalties
combined_data$own_goal_match <- combined_data$own_goal
combined_data$penalty_match <- combined_data$penalty

# Sélectionner les données nécessaires
own_goal_data <- combined_data %>%
  select(date, home_team, away_team, own_goal_match, score_difference) %>%
  filter(!is.na(own_goal_match)) %>%  # Exclure les lignes avec des NA dans own_goal_match
  distinct()

# Sélectionner les données nécessaires
penalty_data <- combined_data %>%
  select(date, home_team, away_team, penalty_match, score_difference) %>%
  filter(!is.na(penalty_match)) %>%  # Exclure les lignes avec des NA dans penalty_match
  distinct()

# Histogramme pour l'impact des buts contre son camp
plot3 <- ggplot(own_goal_data, aes(x = score_difference, fill = own_goal_match)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.6) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(title = "Distribution de la Différence de Score avec/sans Buts Contre Son Camp",
       x = "Différence de Score", y = "Nombre de Matchs")

# Histogramme pour l'impact des penalties
plot4 <- ggplot(penalty_data, aes(x = score_difference, fill = penalty_match)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.6) +
  scale_fill_manual(values = c("green", "orange")) +
  labs(title = "Distribution de la Différence de Score avec/sans Penalties",
       x = "Différence de Score", y = "Nombre de Matchs")

# Afficher les deux diagrammes côte à côte
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2)