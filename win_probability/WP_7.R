library(tidyverse)
library(devtools)
library(lubridate)

library(nnet)
library(caret)
library(scales)

#source("functions_etc/scrape_StatsBomb.R")
source("app/app_functions/functions_and_constants_time.R")
source("app/app_functions/functions_and_constants_visual.R")

#~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))
#                             training the model
#~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))

#i'm going to fit a multinomial regression model to obtain probability
#distributions of future goals given time bin, score differential, and ball location

#i have gathered the data i'm going to use in WP_adding_data3.R
fg_list <- list(
  fg1 = read.csv("win_probability/wp_data/futureGoals_16_1.csv"),
  fg2 = read.csv("win_probability/wp_data/futureGoals_37_2.csv"),
  fg3 = read.csv("win_probability/wp_data/futureGoals_43_3.csv"),
  fg4 = read.csv("win_probability/wp_data/futureGoals_11_4.csv"),
  fg5 = read.csv("win_probability/wp_data/futureGoals_49_5.csv"),
  fg6 = read.csv("win_probability/wp_data/futureGoals_2_6.csv"),
  fg8 = read.csv("win_probability/wp_data/futureGoals_72_8.csv"))
#fg7 is the UEFA Euro 2020 dataset, so i'll leave it out of training
fg7 <- read.csv("win_probability/wp_data/futureGoals_55_7.csv")

#taking out dummy rows
fg_list <- lapply(fg_list, function(x) x[-1,])
fg7 <- fg7[-1,]

#binding dataframes into one
d <- rbind(fg_list[[1]], fg_list[[2]]) %>% rbind(fg_list[[3]]) %>% 
  rbind(fg_list[[4]]) %>% rbind(fg_list[[5]]) %>% 
  rbind(fg_list[[6]]) %>% rbind(fg_list[[7]])

#nullify old sets to free up memory
fg_list <- NULL

#clean dataset a little
d1 <- d %>% 
  mutate(id = paste0(competition_id, "_", season_id, "_", match_id),
         cur_score_diff = cur_home_score - cur_away_score) %>%
  select(-all_of(c("X","competition_id","season_id","match_id", 
                   "cur_home_score", "cur_away_score"))) %>%
  relocate(id)

#scale data to between 0 and 1
d1_scaled <- as_tibble(rescale(as.matrix(d1[,-1]))) %>%
  mutate(id = d1$id) %>%
  relocate(id)

#different datasets for home and away, to build two different 
#models for future goals
d1s.home <- d1_scaled %>% select(-future_goals_away)
d1.home <- d1 %>% select(-future_goals_away)

d1s.away <- d1_scaled %>% select(-future_goals_home)
d1.away <- d1 %>% select(-future_goals_home)

#how many unique matches in set?
length(unique(d1.home$id))
#1045

#hold out n matches for testing
test_n <- 20
set.seed(22); test_ids <- sample(unique(d1.home$id), test_n, replace = F)

#use same matches to train both models?  
#for now i am...

# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
#   Home Team Model 
#--------------------------------------------------------------------

#split training and testing sets
#scaled versions
d1s.h.te <- d1s.home %>% filter(id %in% test_ids)
d1s.h.tr <- d1s.home %>% filter(!(id %in% test_ids))

#non-scaled versions for reference
d1.h.tr <- d1.home %>% filter(!(id %in% test_ids))
d1.h.te <- d1.home %>% filter(id %in% test_ids)

#fitting multinomial regression model from nnet package
mod2.h <- multinom(future_goals_home ~ time_bin + cur_score_diff + start_loc_x + start_loc_y + end_loc_x + end_loc_y, d1s.h.tr)
saveRDS(mod2.h, "app/app_functions/goals_model_w_scaling_home.rds")

#--------------------------------------------------------------------

# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
#   Away Team Model 
#--------------------------------------------------------------------

#split training and testing sets
#scaled versions
d1s.a.tr <- d1s.away %>% filter(!(id %in% test_ids))
d1s.a.te <- d1s.away %>% filter(id %in% test_ids)

#non-scaled versions for reference
d1.a.tr <- d1.away %>% filter(!(id %in% test_ids))
d1.a.te <- d1.away %>% filter(id %in% test_ids)

#fitting multinomial regression model from nnet package
mod2.a <- multinom(future_goals_away ~ time_bin + cur_score_diff + start_loc_x + start_loc_y + end_loc_x + end_loc_y, d1s.a.tr)
saveRDS(mod2.a, "app/app_functions/goals_model_w_scaling_away.rds")

#--------------------------------------------------------------------


#~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))
#                       predicting UEFA 2020 matches
#~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))

mod2.a <- readRDS("app/app_functions/goals_model_w_scaling_away.rds")
mod2.h <- readRDS("app/app_functions/goals_model_w_scaling_home.rds")

#all the UEFA 2020 events
evc <- read.csv("app/app_data/events_corr_3.csv")
evp <- read.csv("win_probability/wp_data/events_pred_set.csv")

evp_scaled <- rescale(as.matrix(evp %>% select(time_bin:end_loc_y)))
evp_scaled <- as_tibble(evp_scaled) %>%
  mutate(match_id = evp$match_id) %>%
  relocate(match_id)

#some obs are in evc but not evp.  which obs from evc are going to be left out of prediction?
evc_event_ids <- sort(unique(evc$id))
length(evc_event_ids) #[1] 183481

evp_event_ids <- sort(unique(evp$id))
length(evp_event_ids) #[1] 182107

#1374 obs in evc will need to be flagged as not included in model

match_ids <- unique(evp_scaled$match_id)

goal_probabilities <- data.frame(
  
  time_bin = 0, 
  goals = "0", 
  P_goals = 0, 
  match_id = 0, 
  team = "None"
)

outer_wp_df <- data.frame(
  
  match_id = 0,
  reps = 0,
  wins = 0,
  draws = 0,
  losses = 0, 
  time_bin = 999
)

#matchID = 3788744
for(matchID in match_ids){
  
  #filter model input for selected match
  this.nd <- evp_scaled %>% filter(match_id == matchID)
  this.fg <- evc %>% filter(match_id == matchID)
  #some time bins may be missing.  should i fill in?
  
  #get goal prediction matrices for match
  this.gpm.h <- predict(mod2.h, this.nd[,-1], type = "probs")
  this.gpm.a <- predict(mod2.a, this.nd[,-1], type = "probs")
  
  #renaming columns back to number of goals
  colnames(this.gpm.h) <- as.character(0:13)
  colnames(this.gpm.a) <- as.character(0:9)
  
  #now, get win/draw/lose probabilities for match
  
  reps <- 10000
  
  #df to store wp results
  inner_wp_df <- data.frame(
    reps = 0,
    wins = 0,
    draws = 0,
    losses = 0, 
    time_bin = 999
  )
  
  
  #win, lose, or draw predictions
  for(row in 1:dim(this.gpm.a)[1]){
    rolls.home <- sample(0:13, reps, prob = this.gpm.h[row,], replace = T)
    rolls.away <- sample(0:9, reps, prob = this.gpm.a[row,], replace = T)
    
    #cur_tb <- 
    #past goals scored as of selected time bin
    #!!! failed to filter fg_uefa2020 down to selected match id !!!
    this.fg <- evc %>% filter(match_id == matchID)
    past_hg <- this.fg$cur_home_score[this.tb + 1]
    past_ag <- this.fg$cur_away_score[this.tb + 1]
    
    wld <- data.frame(
      pred_goals_home = rolls.home,
      pred_goals_away = rolls.away
    )
    
    wld <- wld %>% mutate(
      pred_final_home = pred_goals_home + past_hg,
      pred_final_away = pred_goals_away + past_ag) %>%
      mutate(
        win = ifelse(pred_final_home > pred_final_away, 1, 0),
        draw = ifelse(pred_final_home == pred_final_away, 1, 0),
        loss = ifelse(pred_final_home < pred_final_away, 1, 0)
      )
    
    #summarize the predictions for time bin into row of data
    wp_row <- wld %>% summarize(
      reps = n(),
      wins = sum(win),
      draws = sum(draw),
      losses = sum(loss)) %>%
      mutate(
        time_bin = this.tb
      )
    
    #add row onto df
    inner_wp_df <- rbind(inner_wp_df, wp_row)
  }
  
  #here are the predictions for this match
  inner_wp_df <- inner_wp_df[-1,]
  inner_wp_df <- inner_wp_df %>% mutate(match_id = matchID) %>% relocate(match_id)
  outer_wp_df <- rbind(outer_wp_df, inner_wp_df)
  
}

outer_wp_df <- outer_wp_df %>% mutate(
  p_win = wins/reps,
  p_draw = draws/reps,
  p_loss = losses/reps
)

#all min-by-min goal probabilities for uefa 2020
write.csv(goal_probabilities, "app/app_data/goal_probabilities_scaled.csv")

#all min-by-min win/draw/lose probabilities for uefa 2020
write.csv(outer_wp_df, "app/app_data/win_probabilities_scaled.csv")


