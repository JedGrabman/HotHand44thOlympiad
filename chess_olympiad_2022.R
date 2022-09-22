library(rvest)
library(dplyr)

tables_from_team_index = function(team_index){
  team_url = paste0("https://results.fide.com/olymp.php?ev_code=36&team=", team_index)
  team_html = read_html(team_url)
  team_tables = get_team_tables(team_html)
  return(team_tables)
}

get_team_tables = function(html_page){
  return (html_page %>% html_elements("body") %>% html_elements("table"))
}

team_pages = lapply(c(1:188), function(x) tables_from_team_index(x))
team_pages = team_pages[-106] # No pakistan

get_player_ratings = function(team_ratings){
  team_table = team_ratings %>% html_table()
  clean_table = team_table[2:nrow(team_table),2:3]
  ratings = as.numeric(unlist(clean_table[,2]))
  player_names = unlist(clean_table[,1])
  names(ratings) = player_names
  return(ratings)
}

player_ratings = unlist(lapply(team_pages, function(x) get_player_ratings(x[[1]])))


get_game_results = function(results_html){
  results_table = results_html %>% html_table(header=FALSE)
  
  colnames(results_table) = results_table[2,]
  results_table = results_table[,1:6]
  results_table$Res[results_table$Res == "½"] = 0.5
  
  start_rows = which(results_table[,1] == "Rd.") + 1
  num_players = length(start_rows)
  end_rows = c(start_rows[2:num_players] - 3, nrow(results_table))
  player_names = results_table[start_rows - 2, 1]$Rd.
  player_results = vector("list", num_players)
  
  for (i in c(1:num_players)){
    player_results[[i]] = results_table[start_rows[i]:end_rows[i],]
    player_results[[i]]$Rd. = as.numeric(player_results[[i]]$Rd.)
    player_results[[i]]$Res = as.numeric(player_results[[i]]$Res)
    player_results[[i]]$Rtg = as.numeric(player_results[[i]]$Rtg)
  }
  names(player_results) = player_names
  return(player_results)
}


player_results = unlist(lapply(team_pages, function(x) get_game_results(x[[2]])), recursive = FALSE)

affected_opponents = player_results[["Panthoum Madol Garang"]]$Name
correct_rating =  player_ratings["Panthoum Madol Garang"]

for (opponent in affected_opponents){
  player_results[opponent][[1]][player_results[opponent][[1]]$Name == "Panthoum Madol Garang",]$Rtg = correct_rating
  
}
# player, rating, first half score, expected first half, 
# second half score, expected second half

get_performance_rating = function(score, opponent_ratings){
  if(score == 0){
    score = 0.25
  }
  games = length(opponent_ratings)
  if(score == games){
    score = games - 0.25
  }
  low_rating = 0
  high_rating = 4000
  while (high_rating - low_rating > 1){
    mid_rating = (low_rating + high_rating)/2
    mid_score = sum(1 / (1 + 10**((opponent_ratings - mid_rating)/400)))
    if (mid_score < score){
      low_rating = mid_rating
    }
    else{
      high_rating = mid_rating
    }
  }
  return(round(mid_rating))
}

num_players = length(player_ratings)
results_df = data.frame(player = character(), 
                        rating = double(),
                        first_half_score = double(),
                        first_half_expected = double(),
                        first_half_rating = double(),
                        second_half_score = double(), 
                        second_half_expected = double(),
                        second_half_rating = double())
player_counter = 1
for (i in c(1:num_players)){
  player_name = names(player_ratings)[i]
  player_rating = player_ratings[i]
  if(player_rating > 0){
    tournament_results = player_results[[player_name]]
    if(!is.null(tournament_results)){
      tournament_results = tournament_results[tournament_results$Rtg > 0 & !is.na(tournament_results$Res),]
      first_half = tournament_results[tournament_results$Rd. <=6,]
      second_half = tournament_results[tournament_results$Rd. > 6,]
      if (nrow(first_half) > 3 & nrow(second_half) > 3){
        first_half_score = sum(first_half$Res)
        first_half_expected = sum(1 / (1 + 10**((first_half$Rtg - player_rating)/400)))
        second_half_score = sum(second_half$Res)
        second_half_expected = sum(1 / (1 + 10**((second_half$Rtg - player_rating)/400)))
        results_df[player_counter,]$player = player_name
        results_df[player_counter,]$rating = player_rating
        results_df[player_counter,]$first_half_score = first_half_score
        results_df[player_counter,]$first_half_expected = first_half_expected
        results_df[player_counter,]$first_half_rating = get_performance_rating(first_half_score, first_half$Rtg)
        results_df[player_counter,]$second_half_score = second_half_score
        results_df[player_counter,]$second_half_expected = second_half_expected
        results_df[player_counter,]$second_half_rating = get_performance_rating(second_half_score, second_half$Rtg)
        player_counter = player_counter + 1
      }
    }
  }
}

results_df$first_half_difference = with(results_df, first_half_score - first_half_expected)
results_df$second_half_difference = with(results_df, second_half_score - second_half_expected)
results_df$first_half_rating_difference = results_df$first_half_rating - results_df$rating
results_df$second_half_rating_difference = results_df$second_half_rating - results_df$rating

plot(results_df$first_half_difference, results_df$second_half_difference, xlim = c(-4, 4), ylim = c(-4, 4), 
     xlab = "First Half Relative Performance", ylab = "Second Half Relative Performance", main = "Score Over and Underperformance by Half in the 44th Chess Olympiad")
lm_1 = with(results_df, lm(second_half_difference ~ first_half_difference))
abline(a = lm_1$coefficients[1], b = lm_1$coefficients[2])
summary(lm_1)

plot(results_df$first_half_rating_difference, results_df$second_half_rating_difference, xlim = c(-800, 800), ylim = c(-800, 800),
     xlab = "First Half Relative Performance", ylab = "Second Half Relative Performance", main = "Rating Over and Underperformance by Half in the 44th Chess Olympiad")

lm_2 = with(results_df, lm(first_half_rating_difference ~ second_half_rating_difference))
abline(a = lm_2$coefficients[1], b = lm_2$coefficients[2])
summary(lm_2)

analyzed_players = nrow(results_df)

results_df$first_half_category = "Average"
results_df$first_half_category[order(results_df$first_half_rating_difference)[1:round(analyzed_players/4)]] = "Underperformed"
results_df$first_half_category[order(results_df$first_half_rating_difference)[round(3*analyzed_players/4):analyzed_players]] = "Overperformed"
results_df$second_half_category = "Average"
results_df$second_half_category[order(results_df$second_half_rating_difference)[1:round(analyzed_players/4)]] = "Underperformed"
results_df$second_half_category[order(results_df$second_half_rating_difference)[round(3*analyzed_players/4):analyzed_players]] = "Overperformed"
with(results_df, table(first_half_category, second_half_category, xlab = "test"))
performance_table = with(results_df, table(first_half_category, second_half_category))
performance_table[c(3,1,2),c(3,1,2)]

first_half_overperformers = results_df$player[results_df$first_half_category == "Overperformed"]
first_half_average = results_df$player[results_df$first_half_category == "Average"]
first_half_underperformers = results_df$player[results_df$first_half_category == "Underperformed"]

op_vs_up_df = data.frame(player = character(), 
                                   score = double(),
                                   score_expected = double(),
                                   games = double())

group = first_half_underperformers
player_counter = 1
for (player in first_half_overperformers){
  player_rating = player_ratings[player]
  player_table = player_results[player][[1]]
  player_table_second_half = player_table[player_table$Rd. > 6,]
  vs_group = player_table_second_half[player_table_second_half$Name %in% group,]
  expected_score = sum(1 / (1 + 10**((vs_group$Rtg - player_rating)/400)))
  actual_score = sum(vs_group$Res)
  score_difference = actual_score - expected_score
  vs_games = nrow(vs_group)
  op_vs_up_df[player_counter,]$player = player
  op_vs_up_df[player_counter,]$score = actual_score
  op_vs_up_df[player_counter,]$score_expected = expected_score
  op_vs_up_df[player_counter,]$games = vs_games
  player_counter = player_counter + 1
}
sum(op_vs_up_df$games)
sum(op_vs_up_df$score_expected)
sum(op_vs_up_df$score)
