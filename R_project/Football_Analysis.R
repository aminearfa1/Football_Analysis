library(tidyverse)
library(dplyr)
library(gridExtra)
library(lubridate)
library(ggthemes)
library(viridis)
library(patchwork)
library(ggplot2)
library(corrplot)
library(fmsb)



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

######################Analyse des performances des équipes dans les tournois majeurs#########################

file_path <- "results.csv"
results <- read.csv(file_path, header = TRUE, sep = ",", stringsAsFactors = FALSE)
head(results)

major_competitions <- c("FIFA World Cup", "UEFA Euro", "African Cup of Nations", "Copa América")
# Créez une liste pour stocker les graphiques
plots <- list()

# Pour chaque compétition majeure
for (comp in major_competitions) {
  # Filtrez les données pour la compétition actuelle
  data_comp <- results %>%
    filter(tournament == comp) %>%
    group_by(country = home_team) %>%
    summarise(count = n()) %>%
    top_n(5, count) %>%  
    ungroup() %>%
    arrange(desc(count))
  
  # Créez un graphique à barres empilées
  p <- ggplot(data_comp, aes(x = reorder(country, -count), y = count, fill = country)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
          axis.title.y = element_blank()) +
    labs(title = comp, x = NULL, y = "Nombre de participations") +
    scale_fill_brewer(palette = "Set3")  # Utilisez une palette de couleurs claires
  
  # Ajoutez le graphique à la liste
  plots[[comp]] <- p
}
do.call(grid.arrange, c(plots, ncol = 1))

################################


# Fonction pour calculer et visualiser les taux de victoire à domicile et à l'extérieur
create_win_rate_plot <- function(data, competition) {
  comp_data <- data %>%
    filter(tournament == competition) %>%
    mutate(home_win = as.numeric(home_score > away_score),
           away_win = as.numeric(away_score > home_score)) %>%
    summarise(home_win_rate = mean(home_win, na.rm = TRUE),
              away_win_rate = mean(away_win, na.rm = TRUE)) %>%
    pivot_longer(cols = c(home_win_rate, away_win_rate), names_to = "location", values_to = "win_rate") %>%
    mutate(location = factor(location, levels = c("home_win_rate", "away_win_rate"), labels = c("Home Win Rate", "Away Win Rate")))
  
  ggplot(comp_data, aes(x = location, y = win_rate, fill = location)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
    geom_text(aes(label = scales::percent(win_rate)),
              position = position_dodge(width = 0.8), vjust = -0.5, size = 3.5) +
    scale_fill_manual(values = c("Home Win Rate" = "blue", "Away Win Rate" = "red"),
                      labels = c("Home Goals", "Away Goals")) +
    ylim(0, 1) +
    labs(title = paste("Win Rates for", competition), y = "Win Rate", x = "", fill = "Goal Type") +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# On applique la fonction à chaque compétition on cree un graphique
plots <- lapply(major_competitions, function(comp) create_win_rate_plot(results, comp))

# Affichage des graphiques côte à côte
do.call(grid.arrange, c(plots, ncol = 2))


################################


# Calcul de l'efficacité offensive et défensive
team_efficiency <- results %>%
  # Calcul des buts marqués et concédés à domicile
  mutate(home_goals_scored = home_score,
         home_goals_conceded = away_score) %>%
  group_by(home_team) %>%
  summarise(avg_home_goals_scored = mean(home_goals_scored, na.rm = TRUE),
            avg_home_goals_conceded = mean(home_goals_conceded, na.rm = TRUE)) %>%
  # Calcul des buts marqués et concédés à l'extérieur
  inner_join(
    results %>%
      mutate(away_goals_scored = away_score,
             away_goals_conceded = home_score) %>%
      group_by(away_team) %>%
      summarise(avg_away_goals_scored = mean(away_goals_scored, na.rm = TRUE),
                avg_away_goals_conceded = mean(away_goals_conceded, na.rm = TRUE)),
    by = c("home_team" = "away_team")
  ) %>%
  
  rename(team = home_team) %>%
  select(team, everything())


# On définit une liste des pays pour chaque tournoi majeur
top_5_countries_fifawolrdcup <- c("Brazil", "Germany", "Argentina", "Italy", "France")
top_5_countries_copaamerica <- c("Argentina", "Brazil", "Chile", "Bolivia", "Uruguay")
top_5_countries_uefa <- c("England", "France", "Germany", "Italy", "Netherlands")
top_5_countries_african <- c("Egypte", "Cameroon", "Ghana", "Nigeria", "Algeria", "Ivory Coast")

# Préparation des données pour chaque tournoi avec une colonne supplémentaire pour le tournoi
team_efficiency_worldcup <- team_efficiency %>%
  filter(team %in% top_5_countries_fifawolrdcup) %>%
  mutate(tournament = "FIFA World Cup")

team_efficiency_copaamerica <- team_efficiency %>%
  filter(team %in% top_5_countries_copaamerica) %>%
  mutate(tournament = "Copa America")

team_efficiency_uefa <- team_efficiency %>%
  filter(team %in% top_5_countries_uefa) %>%
  mutate(tournament = "UEFA Euro")

team_efficiency_african <- team_efficiency %>%
  filter(team %in% top_5_countries_african) %>%
  mutate(tournament = "African Cup of Nations")

# Cration d' un graphique pour chaque tournoi
plot_worldcup <- ggplot(team_efficiency_worldcup, aes(x = avg_home_goals_scored + avg_away_goals_scored, y = avg_home_goals_conceded + avg_away_goals_conceded, label = team, color = team)) +
  geom_point(size = 5) + 
  labs(title = "FIFA World Cup")+ xlab("Moyenne des Buts Marqués (Domicile + Extérieur)")+ ylab(" Buts Concédés ")

plot_copaamerica <- ggplot(team_efficiency_copaamerica, aes(x = avg_home_goals_scored + avg_away_goals_scored, y = avg_home_goals_conceded + avg_away_goals_conceded, label = team, color = team)) +
  geom_point(size = 5) + 
  labs(title = "Copa America")+ xlab("Moyenne des Buts Marqués (Domicile + Extérieur)")+ ylab("Moyennes des ")

plot_uefa <- ggplot(team_efficiency_uefa, aes(x = avg_home_goals_scored + avg_away_goals_scored, y = avg_home_goals_conceded + avg_away_goals_conceded, label = team, color = team)) +
  geom_point(size = 5) + 
  labs(title = "UEFA Euro")+ xlab("Moyenne des Buts Marqués (Domicile + Extérieur)")+ ylab(" Buts   Concédés ")

plot_african <- ggplot(team_efficiency_african, aes(x = avg_home_goals_scored + avg_away_goals_scored, y = avg_home_goals_conceded + avg_away_goals_conceded, label = team, color = team)) +
  geom_point(size = 5) + 
  labs(title = "African Cup of Nations")+ xlab("Moyenne des Buts Marqués (Domicile + Extérieur)")+ ylab("Moyenne des")


grid.arrange(plot_worldcup, plot_copaamerica, plot_uefa, plot_african, ncol = 1)
# Création d' un dataframe avec les valeurs moyennes des buts marqués et concédés pour chaque équipe
team_stats <- data.frame(
  Team = team_efficiency$team,
  Avg_Goals_Scored = team_efficiency$avg_home_goals_scored + team_efficiency$avg_away_goals_scored,
  Avg_Goals_Conceded = team_efficiency$avg_home_goals_conceded + team_efficiency$avg_away_goals_conceded
)

team_stats <- data.frame(
  Team = team_efficiency$team,
  Avg_Goals_Scored = team_efficiency$avg_home_goals_scored + team_efficiency$avg_away_goals_scored,
  Avg_Goals_Conceded = team_efficiency$avg_home_goals_conceded + team_efficiency$avg_away_goals_conceded
)

# Filtrer le dataframe en fonction des listes de pays pour chaque tournoi
team_stats_fifawolrdcup <- team_stats[team_stats$Team %in% top_5_countries_fifawolrdcup, ]
team_stats_copaamerica <- team_stats[team_stats$Team %in% top_5_countries_copaamerica, ]
team_stats_uefa <- team_stats[team_stats$Team %in% top_5_countries_uefa, ]
team_stats_african <- team_stats[team_stats$Team %in% top_5_countries_african, ]

# Afficher les valeurs pour chaque tournoi
print("FIFA World Cup")
print(team_stats_fifawolrdcup)

print("Copa America")
print(team_stats_copaamerica)

print("UEFA Euro")
print(team_stats_uefa)

print("African Cup of Nations")
print(team_stats_african)
