
#library(StatsBombR)
library(shiny)
library(tidyverse)
library(lubridate)

#source("app_functions/scrape_StatsBomb.R")
source("app_functions/bombViz.R")
source("app_functions/timestamp_to_seconds.R")
source("app_functions/get_match_players2.R")
source("app_functions/get_match_info_table.R")

#position abbreviations and display coordinates
posAb <- read.csv("app_functions/positionDisplay.csv")


# ||=||=||@||=||=||=||=||=||@||=||=||=||=||=||@||=||=||=||=||=||@||=||=||=
#                               Get Data
# ||=||=||@||=||=||=||=||=||@||=||=||=||=||=||@||=||=||=||=||=||@||=||=||=

#since there is only one competition used in hackathon data, pulling data from
#api isn't reactive, just once at the beginning.  ideally, dashboard will be
#usable with any available StatsBomb360 competition dataset

#Matches
#m <- scrape_matches(compName = "UEFA Euro")
m <- read.csv("app_data/dbb_matches.csv")

#Events (with nested vars)
#ev_og <- scrape_events(m)

#Unnested starting lineups
#lu <- get_startingXI(ev_og)
lu <- read.csv("app_data/dbb_events_startingXI.csv")

#related events
#rel_ev <- get_related_events(ev_og)

#events without nested vars
#ev <- get_stripped_events(ev_og)
ev <- read.csv("app_data/dbb_events.csv")

#ev_og <- NULL

#list of unique match_id values 
matchIDs <- unique(m$match_id)
#all data used in app will be filtered by input match_id value 
#each time a new match id is selected

#event types are standing in for statviews, which will be groups of event types tbd
eventTypes <- unique(ev$type.name)

#transforming timestamp var so i can plot it on x axis
ev <- ev %>% mutate(timestamp_seconds = timestamp_to_seconds(timestamp))


ui <- fluidPage(
  
  tags$head(
    
    #app styling
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Dosis&display=swap');
      body {
        background-color: #ccebff;
        color: #223fb3;
      }
      h2 {
        font-family: 'Dosis', sans-serif;
      }
      .shiny-html-output {
        background-color: #a3daff;
        color: #223fb3;
      }
      .shiny-input-container {
        color: #223fb3;
        margin: 10px;
        
      }"))
  ),
  
  
  
  titlePanel("StatsBomb 360"),
  
  #first main row - match selection input
  fluidRow(
    
    
    #match selection input
    selectInput("match_select", "Select Match:", matchIDs, selectize = F)
    
    
  ),
  
  #second main row - plots and other content
  fluidRow(
    #first column, timeline plots
    column(9,
           plotOutput("wp_plot", height = 300, width = 900),
           uiOutput("timeline_slider"),
           plotOutput("timeline_plot", height = 100, width = 900,
                      brush = brushOpts(id = "timeline_brush",
                                        fill = shUEFA["orange"],
                                        stroke = shUEFA["orange"],
                                        opacity = 0.25,
                                        direction = "x")),
           plotOutput("possession_plot", height = 100, width = 900)
           
           
           
           
           ),
    #second column - text and tables, pitch plot
    column(3,
           
           tableOutput("match_info_table")
           
           
           
           )
    
  ),
  
  fluidRow(
    column(9,
           plotOutput("roster_timeline_plot", height = 500)
           ),
    
    column(3,
           plotOutput("roster_plot", height = 500)
           )
    
    
  )
)

    
    
  
  



server <- function(input, output){
  
  #update data according to match id input
  this_match_data <- reactive({
    
    
    this.m <- m %>% filter(match_id == input$match_select)
    this.ev <- ev %>% filter(match_id == input$match_select)
    
    #df of players in match
    mp <- get_match_players(input$match_select, m, ev, lu)
    #period and time information
    pers <- get_period_summary(this.ev)
    #table of match info to display
    mit <- get_match_info_table(this.m)
    
    #do all necessary data transformations
    
    return(
      list(
        "mit" = mit,
        "mp" = mp,
        "pers" = pers,
        "this_m" = this.m,
        "this_ev" = this.ev
      )
    )
    
    
  })
  
  output$timeline_slider <- renderUI({
    this.data <- this_match_data()
    pers <- this.data[["pers"]]
    
    sliderInput("slider_time", "Select time:", min = 0, max = max(pers$cum_total_seconds), value = 0, width = "900px")
    
  })
  
  
  output$match_info_table <- renderTable({
    
    this.data <- this_match_data()
    this.data[["mit"]]
    
    
  })
  
  output$possession_plot <- renderPlot({
    
    this.data <- this_match_data()
    mp <- this.data[["mp"]]
    pers <- this.data[["pers"]]
    this.m <- this.data[["this_m"]]
    this.ev <- this.data[["this_ev"]]
    
    awayName <- this.m %>% pull(away_team.away_team_name)
    homeName <- this.m %>% pull(home_team.home_team_name)
    
    #transform timestamps into cumulative seconds
    this.ev <- get_cumulative_match_seconds(this.ev)
    
    #possessions df
    poss <- this.ev %>% group_by(possession) %>%
      summarize(
        possession_team = first(possession_team.name),
        start_of_poss = first(cum_match_seconds)) %>% 
      mutate(end_of_poss = lead(start_of_poss, n = 1))
    #need to debug so that last possession end isn't NA
    
    #to make a manual color scale, need vector same length as num of levels,
    #and to rename colors to match levels
    clrs <- shUEFA[c("orangeLt","orangeDk")]
    names(clrs) <- c(homeName,awayName)
    
    #plot of possessions throughout the match
    ggplot(poss) + shUEFA_theme_icy + 
      geom_rect(aes(xmin = start_of_poss, xmax = end_of_poss,
                    ymin = 1, ymax = 2, 
                    color = possession_team, fill = possession_team)) + 
      scale_color_manual(values = clrs) + 
      scale_fill_manual(values = clrs)
    
  })
  
  
  
  #1-D time slider tailored to time and periods of match
  output$timeline_plot <- renderPlot({
    
    this.data <- this_match_data()
    mp <- this.data[["mp"]]
    pers <- this.data[["pers"]]
    this.m <- this.data[["this_m"]]
    this.ev <- this.data[["this_ev"]]
    
    
    
    
    ggplot() + shUEFA_theme_icy +
      
      coord_cartesian(xlim = c(0, max(pers$cum_total_seconds)), ylim = c(-20,100)) + 
      #big rectangle
      geom_rect(aes(xmin = 0, xmax = max(pers$cum_total_seconds), ymin = 0, ymax = 100), 
                fill = icyUEFA["ice2"], color = icyUEFA["ice4"], size = 1) +
      #period divider
      geom_segment(aes(x = pers$cum_total_seconds, xend = pers$cum_total_seconds, 
                       y = rep.int(-15, dim(pers)[1]), yend = rep.int(100, dim(pers)[1])), 
                   color = shUEFA["blueLt"], size = 1) + 
      #time line
      geom_segment(aes(x = 0, xend = max(pers$cum_total_seconds), 
                       y = 50, yend = 50), color = icyUEFA["ice5"], size = 1.5) + 
      #time line ends
      geom_segment(aes(x = c(0, max(pers$cum_total_seconds)), 
                       xend = c(0, max(pers$cum_total_seconds)), 
                       y = c(40, 40), yend = c(60,60)), color = icyUEFA["ice5"], size = 1.5) + 
      #time labels
      annotate("text", y = rep.int(-10, dim(pers)[1]), x = pers$cum_total_seconds-50, 
               label = pers$max_ts, color = shUEFA["blueLt"], hjust = 1) 
      
      
  })
  
  output$roster_timeline_plot <- renderPlot({
    
    this.data <- this_match_data()
    mp <- this.data[["mp"]]
    pers <- this.data[["pers"]]
    this.m <- this.data[["this_m"]]
    this.ev <- this.data[["this_ev"]]
    
    #get match-specific information
    #team names
    awayName <- this.m %>% pull(away_team.away_team_name)
    homeName <- this.m %>% pull(home_team.home_team_name)
    
    
    #add a var to sep home and away
    mp <- mp %>% mutate(home_or_away = ifelse(team.name == awayName, "away", "home"))
    
    #transform timestamps into cumulative seconds
    this.ev <- get_cumulative_match_seconds(this.ev)
    mp <- get_cumulative_ts_on_off(mp, pers)
    
    #assign a y value to each player in match
    mp <- mp %>% arrange(home_or_away, position.id, ts_on_cum_seconds) %>%
      mutate(timeline_y = seq(dim(mp)[1], 1, -1)) %>%
      mutate(timeline_y = ifelse(home_or_away == "away", timeline_y +1, timeline_y))
    #join position abbreviations
    mp <- mp %>% left_join(posAb %>% select(position_abbr, position.id), by = "position.id")
    
    #number of players on each team
    nA <- dim(mp %>% filter(home_or_away == "away"))[1]
    nH <- dim(mp %>% filter(home_or_away == "home"))[1]
    
    #match timeline_y values to events based on player id
    this.ev <- this.ev %>% left_join(mp %>% select(player.id, timeline_y), by = "player.id")
    #make NA values for duration into zeros
    this.ev <- this.ev %>% mutate(duration = ifelse(is.na(duration), 0, duration))
    
    base_tl <- ggplot(mp) + shUEFA_theme_icy + 
      #coord_cartesian(xlim = c(-3000, max(pers$cum_total_seconds))) +
      
      #dotted lines for each player
      annotate("segment", x = rep.int(0, dim(mp)[1]), xend = rep.int(max(pers$cum_total_seconds), dim(mp)[1]), y = mp$timeline_y, yend = mp$timeline_y, 
               color = icyUEFA["ice2"], linetype = 2) + 
      
      #rectangles around each team
      annotate("rect", xmin = 0, xmax = max(pers$cum_total_seconds), ymin = 0, ymax = nH+1, fill = NA, color = icyUEFA["ice2"]) + 
      annotate("rect", xmin = 0, xmax = max(pers$cum_total_seconds), ymin = nH+1, ymax = nH+nA+2, fill = NA, color = icyUEFA["ice2"]) + 
      
      #on-field timelines for each player
      geom_segment(aes(x = ts_on_cum_seconds, xend = ts_off_cum_seconds, y = timeline_y, yend = timeline_y), color = shUEFA["orangeLt"])
      
      
    
    base_tl
    
    
  })
  
  output$roster_plot <- renderPlot({
    
    
    this.data <- this_match_data()
    mp <- this.data[["mp"]]
    pers <- this.data[["pers"]]
    this.m <- this.data[["this_m"]]
    this.ev <- this.data[["this_ev"]]
    
    #get match-specific information
    #team names
    awayName <- this.m %>% pull(away_team.away_team_name)
    homeName <- this.m %>% pull(home_team.home_team_name)
    
    
    #add a var to sep home and away
    mp <- mp %>% mutate(home_or_away = ifelse(team.name == awayName, "away", "home"))
    
    #transform timestamps into cumulative seconds
    this.ev <- get_cumulative_match_seconds(this.ev)
    mp <- get_cumulative_ts_on_off(mp, pers)
    
    #assign a y value to each player in match
    mp <- mp %>% arrange(home_or_away, position.id, ts_on_cum_seconds) %>%
      mutate(timeline_y = seq(dim(mp)[1], 1, -1)) %>%
      mutate(timeline_y = ifelse(home_or_away == "away", timeline_y +1, timeline_y))
    #join position abbreviations
    mp <- mp %>% left_join(posAb %>% select(position_abbr, position.id), by = "position.id")
    
    #number of players on each team
    nA <- dim(mp %>% filter(home_or_away == "away"))[1]
    nH <- dim(mp %>% filter(home_or_away == "home"))[1]
    
    #match timeline_y values to events based on player id
    this.ev <- this.ev %>% left_join(mp %>% select(player.id, timeline_y), by = "player.id")
    #make NA values for duration into zeros
    this.ev <- this.ev %>% mutate(duration = ifelse(is.na(duration), 0, duration))
    
    base_tl <- ggplot(mp) + shUEFA_theme_icy +
      
      #player names and positions
      annotate("text", x = rep.int(350, dim(mp)[1]), y = mp$timeline_y, label = mp$player.name, hjust = 1, color = shUEFA["blueLt"]) + 
      annotate("text", x = rep.int(100, dim(mp)[1]), y = mp$timeline_y, label = mp$position_abbr, hjust = 0.5, color = shUEFA["blueLt"])
    
    
    base_tl
    
    
    
  })
  
  output$wp_plot <- renderPlot({
    
    
  })
  
}


shinyApp(ui, server)









