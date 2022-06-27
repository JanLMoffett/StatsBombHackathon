

xid <- 3788753

mp <- get_match_players(xid, m, ev, lu)

cur_m <- m %>% filter(match_id == xid)
cur_ev <- ev %>% filter(match_id == xid)

#get time summary for match
pers <- get_period_summary(ev %>% filter(match_id == xid))
pers <- pers %>% mutate(cum_seconds = cumsum(total_seconds))

mp <- get_cumulative_ts_on_off(mp, pers)

dlist <- list("mp" = mp, "cur_m" = cur_m, "cur_ev" = cur_ev)

x.mp <- dlist[["mp"]]

dlist[["cur_m"]]$away_team.away_team_name
