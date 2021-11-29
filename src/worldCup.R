################################################################################
############       Scraping Ball-by-Ball data for The Hundred       ############
############                 James Brook 2021-07-24                 ############
################################################################################

# source("src/x_util.R")
eval(parse("src/x_util.R", encoding = "UTF-8"))

############
# Setup 
############

options(dplyr.summarise.inform = FALSE)

T20WorldCup <- importSeries("ICC Men's T20 World Cup")

allBatting <- summaryT20(T20WorldCup, "batting")
allBowling <- summaryT20(T20WorldCup, "bowling")
allInnings <- summaryT20(T20WorldCup, "innings")

game <- T20WorldCup %>%
  group_by(matchID, match, innings, team, ball) %>%
  summarise(runs = sum(runs)) %>%
  group_by(matchID, match,innings, team) %>%
  mutate(cumruns = cumsum(runs),
         innings = case_when(
           innings == 1 ~ "First Innings",
           TRUE ~ "Second Innings"
         ),
         ball2 = floor(ball) + ((ball-floor(ball))*10/6)) %>%
  group_by(matchID, match) %>%
  mutate(matchDesc = paste0(matchID, " - ", first(team), " v ", last(team)))

game$matchDesc <- factor(game$matchDesc, levels = rev(unique(game$matchDesc)))

ggplot(game, aes(x=ball2, y=cumruns, group=innings, color=team, linetype=innings)) +
  geom_line(size=1) +
  theme_kantar() +
  teamColour(matchNames = TRUE) + #scale_color_manual(breaks=teamNames, values=teamColours) +
  theme(
    # legend.position = "bottom",
    panel.grid.major.y = element_line(colour="grey95"),
    panel.grid.major.x = element_line(colour="grey95"),
  ) +
  scale_x_continuous(limits=c(0, 20)) +
  scale_y_continuous(limits=c(0, ceiling(max(game$cumruns)/50)*50)) +
  annotate("segment", x=-Inf, xend=Inf, y=0, yend=0, size=0.5, colour="grey75") +
  annotate("segment", x=0, xend=0, y=-Inf, yend=Inf, size=0.5, colour="grey75") +
  labs(
    title = "Worm Charts",
    subtitle = "All T20 World Cup Games",
    x = "Overs",
    y = "Runs"
  ) +
  facet_wrap(~matchDesc, scales="free_x")

game2 <- T20WorldCup %>%
  mutate(over = floor(ball) + 1) %>%
  group_by(matchID, match, innings, team, over) %>%
  summarise(runs = sum(runs)) %>%
  group_by(matchID, match) %>%
  mutate(crr = runs / over,
         matchDesc = paste0(matchID, " - ", first(team), " v ", last(team)))

game2$matchDesc <- factor(game2$matchDesc, levels = rev(unique(game2$matchDesc)))

ggplot(game2, aes(x = as.factor(over), y = runs, group=paste0(innings, " ", team), fill=team)) +
  geom_col(width = 0.5, position=position_dodge2(preserve="single", padding = 0.2)) +
  theme_kantar() +
  teamFill(matchNames = TRUE) + #  scale_fill_manual(breaks=teamNames, values=teamColours) +
  theme(
    panel.grid.major.y = element_line(colour="grey95")
  ) +
  scale_y_continuous(limits=c(0, ceiling(max(game2$runs)/5)*5)) +
  annotate("segment", x=-Inf, xend=Inf, y=0, yend=0, size=0.5, colour="grey75") +
  labs(
    title = "Manhattan Charts",
    subtitle = "All T20 World Cup Games",
    x = "Over",
    y = "Runs"
  ) +
  facet_wrap(~matchDesc)


ggplot(game2, aes(x = over, y = crr, colour=team)) +
  geom_line() +
  theme_kantar() +
  teamColour(matchNames = TRUE) + 
  theme(
    panel.grid.major.y = element_line(colour="grey95")
  ) +
  scale_y_continuous(limits=c(0, ceiling(max(game2$crr)/5)*5)) +
  labs(
    title = "Rute Rate Charts",
    subtitle = "All T20 World Cup Games",
    x = "Over",
    y = "Runs"
  ) +
  facet_wrap(~matchDesc)