library(tidyverse)
library('fastDummies')


run_regression <- function(dataframe, name) {
    model <- lm(game_total_yards ~ ., data = dataframe)
    plot(fitted(model), resid(model), col = "green", abline(0, 0),
    xlab = "Total Yards", ylab = "Residuals",
    main = sprintf("%s Model Residuals", name))  #plot residuals
    print(name)
    print(summary(model)) # summarize model
}

log_regression <- function(dataframe, name) {
    model <- lm(log_game_total_yards ~ ., data = dataframe)
    plot(fitted(model), resid(model), col = "green", abline(0, 0),
    xlab = "Total Yards", ylab = "Residuals",
    main = sprintf("%s Model Residuals", name))  #plot residuals
    print(name)
    print(summary(model)) # summarize model
}

make_interaction_terms <- function(dataframe) {
    # READ DATAFRAME AND CREATE INTERACTION TERMS BETWEEN VARIABLES, RETURN NEW DATAFRAME # nolint
    temp <- data.frame(dataframe)
    temp$pass_snaps <- temp$game_off_snaps * temp$game_passing_att * temp$Position_QB # nolint
    temp$rushing_snaps <- temp$game_off_snaps * temp$game_rushing_att * temp$Position_RB # nolint
    temp$receiving_snaps <- temp$game_off_snaps * temp$game_receiving_targets * temp$Position_WR # nolint
    temp$started_pass_snaps <- temp$pass_snaps * temp$Game.Started
    temp$started_rushing_snaps <- temp$rushing_snaps * temp$Game.Started
    temp$started_receiving_snaps <- temp$receiving_snaps * temp$Game.Started
    return(temp)


}

make_quadtratic_terms <- function(dataframe) {
    temp <- data.frame(dataframe)
    temp$quad_team_season_game_num <- temp$team_season_game_num**2
    temp$quad_Age <- temp$Age**2
    temp$quad_season_off_snaps <- temp$season_off_snaps**2
    temp$quad_game_off_snaps <- temp$game_off_snaps**2
    temp$quad_Career_Games_Played_sum <- temp$Career_Games_Played_sum**2
    temp$quad_game_receiving_targets <- temp$game_receiving_targets**2
    temp$quad_game_rushing_att <- temp$game_rushing_att**2
    temp$quad_game_passing_att <- temp$game_passing_att**2
    temp$quad_Career_Passing_TD_sum <- temp$Career_Passing_TD_sum**2
    temp$quad_career_total_yds <- temp$career_total_yds**2
    return(temp)


}


original <- read.csv("nfl_stats.csv", header = TRUE)
pos <- c("WR", "TE", "QB", "FB", "RB", "RB-WR", "QB/TE", "HB")
filtered <- filter(original, original$Position %in% pos)
df <- dummy_cols(filtered, remove_first_dummy = TRUE,
remove_selected_columns = TRUE)


scaled_df <- data.frame(scale(df))
interaction_df <- make_interaction_terms(df)
quadtratic_df <- make_quadtratic_terms(df)
interaction_quad_df <- make_quadtratic_terms(interaction_df)
base_model <- run_regression(df, "Standard Linear Regression")
scaled_model <- run_regression(scaled_df, "Scaled Model - Linear Regression")
interaction_model <- run_regression(interaction_df, "Linear Regression with Interaction Terms")
quadtratic_model <- run_regression(quadtratic_df, "Linear Regression with Quadratic Terms")
interaction_quadratic_model <- run_regression(interaction_quad_df, "Linear Regression with Interaction and Quadratic Terms")


log_df <- filter(df, game_total_yards >= 0)
log_df$log_game_total_yards <- log1p(log_df$game_total_yards)
log_df <- subset(log_df, select = -game_total_yards)
log_level <- log_regression(log_df, "Log Level Model - Linear Regression")

position_hist <- ggplot(filtered, aes(x = game_total_yards)) +
geom_histogram(aes(color = Position, fill = Position), bins = 100)

print(position_hist)

hist(log_df$log_game_total_yards,col= "pink")