
library(tidyverse)
library(devtools)
library(caret)
library(lubridate)

source("functions_etc/bombViz.R")
#importing colors and themes for plots
#github.com/JanLMoffett/datavizExtras
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/colorConstants.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/extraThemes.R")


tm <- read.csv("data/events_timeMarkers.csv")
pi <- read.csv("data/events_playerInfo.csv")
m <- read.csv("data/matches.csv")

#Q's:
#how do i know what game an event is from?    match_id

#paring down vars
nzv <- nearZeroVar(tm, names = T, saveMetrics = T)
zv <- row.names(nzv[nzv$zeroVar == T,])

tm <- tm %>% select(-all_of(zv))

nzv <- nearZeroVar(pi, names = T, saveMetrics = T)
zv <- row.names(nzv[nzv$zeroVar == T,])

pi <- pi %>% select(-all_of(zv))

names(tm)
#[1] "X.1"                  "X"                    "id"                  
#[4] "index"                "period"               "timestamp"           
#[7] "minute"               "second"               "possession"          
#[10] "type.id"              "type.name"            "possession_team.id"  
#[13] "possession_team.name" "play_pattern.id"      "play_pattern.name"   
#[16] "team.id"              "team.name"            "player.id"           
#[19] "player.name"          "position.id"          "position.name"       
#[22] "match_id"             "location.x"           "location.y"          
#[25] "milliseconds"         "ElapsedTime"          "StartOfPossession"   
#[28] "TimeInPoss"           "TimeToPossEnd"        "OpposingTeam"        
#[31] "OpposingTeam.id"      "location_x"           "location_y"  

names(pi)
#[1] "X.1"                           "X"                            
#[3] "id"                            "index"                        
#[5] "period"                        "timestamp"                    
#[7] "minute"                        "second"                       
#[9] "possession"                    "type.id"                      
#[11] "type.name"                     "possession_team.id"           
#[13] "possession_team.name"          "play_pattern.id"              
#[15] "play_pattern.name"             "team.id"                      
#[17] "team.name"                     "tactics.formation"            
#[19] "player.id"                     "player.name"                  
#[21] "position.id"                   "position.name"                
#[23] "substitution.outcome.id"       "substitution.outcome.name"    
#[25] "substitution.replacement.id"   "substitution.replacement.name"
#[27] "match_id"                      "milliseconds"                 
#[29] "ElapsedTime"                   "StartOfPossession"            
#[31] "TimeInPoss"                    "TimeToPossEnd"                
#[33] "OpposingTeam"                  "OpposingTeam.id"

setdiff(names(pi), names(tm))
setdiff(names(tm), names(pi))
#[1] "tactics.formation"             "substitution.outcome.id"      
#[3] "substitution.outcome.name"     "substitution.replacement.id"  
#[5] "substitution.replacement.name"
setdiff(names(tm), names(pi))
#[1] "location.x" "location.y" "location_x" "location_y"

#what has locations?
unique(tm$type.name[which(!is.na(tm$location.x))])
#Referee Ball-Drop

setdiff(tm$location.x, tm$location_x)
setdiff(tm$location.y, tm$location_y)
setdiff(tm$location_x, tm$location.x)
setdiff(tm$location_y, tm$location.y)
#these vars are the same

tm <- tm %>% rename(ballDropLoc_x = location.x,
                    ballDropLoc_y = location.y) %>%
  mutate(location_x = NULL,
         location_y = NULL)

#a plot of all the ball drop locations on the field
plot_pitch(tm, lineColor = jmbn["mint"]) + bombTurf + 
  geom_point(aes(x = ballDropLoc_x, y = ballDropLoc_y), color = jmbn["rose"])
#by match

matchIDs <- m %>% arrange(match_date, kick_off)

plot_pitch(tm %>% filter(match_id == 3788741), lineColor = jmbn["mint"]) + bombTurf + 
  geom_point(aes(x = ballDropLoc_x, y = ballDropLoc_y, color = factor(match_id)))

sort(head(m$kick_off))
head(tm$timestamp)

#transforming timestamp var so i can plot it on x axis
tm <- tm %>% mutate(timestamp_seconds = seconds(hms(timestamp))) %>% 
  mutate(timestamp_seconds = str_remove(timestamp_seconds, "S")) %>%
  mutate(timestamp_seconds = as.numeric(timestamp_seconds))

#rudimentary matchtime_plot
ggplot(tm) + bombTurf + 
  geom_point(aes(x = timestamp_seconds, y = factor(type.name)), 
             shape = "|", size = 4, color = jmbn["periwinkle"]) + 
  facet_wrap(vars(period)) + 
  labs(title = "Distribution of Timestamps by Period")


#i'm trying to figure out how time is structured in the game
tm %>% group_by(period) %>%
  summarize(
    min.ts = min(timestamp_seconds, na.rm = T),
    max.ts = max(timestamp_seconds, na.rm = T)) %>%
  mutate(
    max.tm = max.ts/60
  )
#  period min.ts max.ts max.tm
#1      1      0  3073.   51.2 
#2      2      0  3188.   53.1 
#3      3      0  1219.   20.3 
#4      4      0  1167.   19.4 
#5      5      0   513.   8.54

#first and second half are supposed to be 45 min long, but can be up to ~55 min
#the third and fourth periods are 15-20 min 
#the fifth period is up to 10

#timestamps revert to zero at the start of each new period

#how many games have more than 2 periods?
tm %>% group_by(match_id) %>%
  summarize(periods = n_distinct(period))

ggplot(tm) + bombTurf + 
  coord_cartesian(xlim = c(0,3300), ylim = c(0,2000))

