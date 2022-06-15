
library(StatsBombR)
library(tidyverse)


#with code from StatsBomb tutorial:
#----
#https://statsbomb.com/articles/soccer/statsbomb-announce-the-release-of-free-statsbomb-360-data-euro-2020-available-now/
Comp <- FreeCompetitions()

#the Matches dataset
Matches <- FreeMatches(Comp)
Matches <- Matches %>% filter(competition.competition_name=="UEFA Euro")

matches <- Matches %>% select(-all_of(c("home_team.managers", "away_team.managers")))

#write.csv(matches, "big_data/dbb_matches.csv")



#nested variable, home_team.managers from Matches
managers <- Matches %>% select(home_team.managers, away_team.managers)

str(matches)
unique(matches$home_team.home_team_name)

matches_simple <- matches %>% 
  select(match_id,
         away_team.away_team_name,
         away_team.away_team_group,
         home_team.home_team_name,
         home_team.home_team_group,
         competition_stage.name,
         match_week,
         match_date,
         kick_off,
         stadium.name,
         stadium.country.name
         ) %>%
  arrange(match_date, kick_off)

#write.csv(matches_simple, "data/matches_simple.csv")
#write.csv(matches, "data/matches.csv")


data360 <- StatsBombFree360Events(MatchesDF = Matches, Parallel = T)

events <- StatsBombFreeEvents(MatchesDF = Matches, Parallel = T)
events <- allclean(events)
events <- get.opposingteam(events)

data360 = data360 %>% rename(id = event_uuid)

#events = events %>% left_join(data360, by = c("id" = "id"))
#events = events %>% rename(match_id = match_id.x) %>% select(-match_id.y)

#write.csv(data360, "big_data/dbb_data360.csv")
#write.csv(events, "big_data/dbb_events.csv")

#what's in the dataset?

look_at_df <- function(aDataframe){
  
  for(i in seq_along(aDataframe)){
    print(paste0(names(aDataframe)[i], " : ", class(aDataframe[[i]])))
  }
  
  print("~ Lists:")
  for(i in seq_along(aDataframe)){
    if(class(aDataframe[[i]]) == "list"){
      print(paste0(names(aDataframe)[i]))
    }
  }
  
}

look_at_df(data360)
#unnest vars:

#visible_area
data360$visible_area[1]
#this is annoying bc the inner vectors are unnamed
#they alternate x1, y1, x2, y2, ...
data360_visibleArea <- data360 %>% select(id, visible_area) %>%
  unnest(cols = visible_area)

coord_type <- rep(c("x","y"), dim(data360_visibleArea)[1]/2)
data360_visibleArea$coord_type <- coord_type


tx <- data360_visibleArea %>% filter(coord_type == "x")
ty <- data360_visibleArea %>% filter(coord_type == "y")

tx <- tx %>% mutate(coord_order = 1:dim(tx)[1])
ty <- ty %>% mutate(coord_order = 1:dim(ty)[1])

data360_visibleArea <- full_join(tx, ty, by = c("id", "coord_order")) %>%
  select(id, visible_area.x, visible_area.y) %>%
  rename(visible_area_x = visible_area.x, 
         visible_area_y = visible_area.y)

data360_visibleArea <- data360_visibleArea %>% 
  mutate(one = 1) %>%
  group_by(id) %>%
  mutate(coord_seq = cumsum(one)) %>%
  ungroup() %>% mutate(one = NULL)

va1 <- data360_visibleArea %>% filter(coord_seq == 1) %>%
  rename(va_x_1 = visible_area_x,
         va_y_1 = visible_area_y)
va2 <- data360_visibleArea %>% filter(coord_seq == 2) %>%
  rename(va_x_2 = visible_area_x,
         va_y_2 = visible_area_y)
va3 <- data360_visibleArea %>% filter(coord_seq == 3) %>%
  rename(va_x_3 = visible_area_x,
         va_y_3 = visible_area_y)
va4 <- data360_visibleArea %>% filter(coord_seq == 4) %>%
  rename(va_x_4 = visible_area_x,
         va_y_4 = visible_area_y)
va5 <- data360_visibleArea %>% filter(coord_seq == 5) %>%
  rename(va_x_5 = visible_area_x,
         va_y_5 = visible_area_y)
va6 <- data360_visibleArea %>% filter(coord_seq == 6) %>%
  rename(va_x_6 = visible_area_x,
         va_y_6 = visible_area_y)
va7 <- data360_visibleArea %>% filter(coord_seq == 7) %>%
  rename(va_x_7 = visible_area_x,
         va_y_7 = visible_area_y)
va8 <- data360_visibleArea %>% filter(coord_seq == 8) %>%
  rename(va_x_8 = visible_area_x,
         va_y_8 = visible_area_y)

#this is how many values I should end up with:
length(unique(data360_visibleArea$id))
#172081

data360_visibleArea <- full_join(va1, va2, by = "id") %>%
  full_join(va3, by = "id") %>% full_join(va4, by = "id") %>%
  full_join(va5, by = "id") %>% full_join(va6, by = "id") %>%
  full_join(va7, by = "id") %>% full_join(va8, by = "id") %>%
  select(-starts_with("coord_seq"))

#write.csv(data360_visibleArea, "big_data/dbb_data360_visibleArea.csv")

#freeze_frame
class(data360$freeze_frame[[1]])
#these are data frames

data360_ff <- data360 %>% select(id, freeze_frame) %>%
  unnest(cols = freeze_frame)

head(data360_ff %>% unnest(cols = location))
data360_ff <- data360_ff %>% unnest(cols = location)

coord_type <- rep(c("x","y"), dim(data360_ff)[1]/2)
data360_ff$coord_type <- coord_type

data360_ff.x <- data360_ff %>% filter(coord_type == "x") %>%
  rename(location_x = location)
data360_ff.y <- data360_ff %>% filter(coord_type == "y") %>%
  rename(location_y = location)

data360_ff <- left_join(data360_ff.x, data360_ff.y %>% select(id, location_y), by = "id")






clook_at_df(events)
#unnest vars:

#"related_events"
#"location"
#"tactics.lineup"
#"pass.end_location"
#"carry.end_location"
#"shot.end_location"
#"shot.freeze_frame"
#"goalkeeper.end_location"


#taking out nested variables
events.noNest <- events %>% 
  select(-all_of(listVars))

#write.csv(events.noNest, "data/events_noNestedVars.csv")

#unnest each of the nested vars and make them joinable by event id

events.location <- events %>% 
  select(id, location) %>%
  unnest(cols = location) %>%
  group_by(id) %>%
  mutate(location_x = first(location),
         location_y = last(location)) %>%
  summarize(
    id = first(id),
    location_x = first(location_x),
    location_y = first(location_y)
  ) %>% ungroup()

events.related_events <- events %>% 
  select(id, type.id, type.name, related_events) %>% 
  unnest(cols = related_events)
#write.csv(events.related_events, "data/unnested_relatedEvents.csv")

events.lineup <- events %>% filter(type.name == "Starting XI") %>%
  select(id, match_id, team.id, team.name, tactics.lineup) %>%
  rename(lineup = tactics.lineup) %>%
  unnest(cols = lineup) %>%
  select(-id)
  
#write.csv(events.lineup, "data/unnested_startingLineups.csv")
names(events.noNest)
events.passEndLoc <- events %>% select(id, pass.end_loc)
events %>% pull(pass.end_location)


#what are the different types of events?
unique(events$type.name)

#filtering down to passes only and reducing variables (statsbomb tutorial)
events.passes = events %>%
  group_by(team.name) %>%
  filter(type.name=="Pass") %>%
  select(id, match_id, team.name, OpposingTeam, player.name, type.name, minute, second, location.x, location.y, pass.end_location.x, pass.end_location.y, pass.type.name, pass.cross, freeze_frame)

events.passes = events.passes %>% unnest(freeze_frame) %>%
  mutate(ff_location.x = (map(location, 1)), 
         ff_location.y = (map(location, 2))) %>%
  select(-location) %>%
  mutate(ff_location.x = as.numeric(ifelse(ff_location.x == "NULL", NA, ff_location.x)), 
         ff_location.y = as.numeric(ifelse(ff_location.y == "NULL", NA, ff_location.y)))



#this is me:
ggplot(ffs %>% filter(match_id == 3788741)) +
  geom_point(aes(x = location.x, y = location.y)) + 
  geom_point(aes(x = ff_location.x, y = ff_location.y), color = "blue")

length(unique(ffs$id))
thisEvent <- unique(ffs$id)[651]



t <- ffs %>% filter(id == thisEvent)

ggplot(t) +
  geom_point(aes(x = location.x, y = location.y), color = "purple", shape = 22) +
  geom_point(aes(x = pass.end_location.x, y = pass.end_location.y), color = "red", shape = 15) +
  geom_segment(aes(x = location.x, xend = pass.end_location.x, y = location.y, yend = pass.end_location.y), color = "purple") +
  geom_point(aes(x = ff_location.x, y = ff_location.y), color = "blue")


#----


#what's in the data?

#Comp
str(Comp) #this is to look up id for UEFA Euro 2020

#str function produces huge output
for(i in seq_along(ffs)){
  print(paste0(names(ffs)[i], " : ", class(ffs[[i]])))
}


#data360----
names(data360)
#str function produces huge output
for(i in seq_along(data360)){
  print(paste0(names(data360)[i], " : ", class(data360[[i]])))
}

#visible area and freeze frame are big lists
visible_area <- data360$visible_area
#list of numeric vectors with about 12 entries for each observation

freeze_frame <- data360$freeze_frame
freeze_frame[[]]

#id
data360[[1]]

#visible_area
data360[[2]]

#end data360----