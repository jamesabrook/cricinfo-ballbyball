################################################################################
############       Scraping Ball-by-Ball data for The Hundred       ############
############                 James Brook 2021-07-24                 ############
################################################################################

source("src/x_util.R")

#Due to the dot ball symbol we should switch to use this instead
eval(parse("src/scrapeIPL.R", encoding = "UTF-8"))


############
# Setup 
############

options(dplyr.summarise.inform = FALSE)

cl <- makeCluster(detectCores() - 2)
registerDoParallel(cl)

#Where to save output data
outLoc <- paste0("data/")

#ESPN cricinfo results page for the desired tournament
url <- "https://www.espncricinfo.com/series/ipl-2019-1165643/match-results"

#Identifier
id <- gsub("https://www.espncricinfo.com/series/", "", gsub("/match-results", "", url))

#Read the raw html data
raw <- try(xml2::read_html(url), silent = TRUE)
if ("try-error" %in% class(raw)) 
  stop("Error in URL")


#Extract tables
t <- raw %>% 
  rvest::html_nodes("a") %>% 
  html_attr("href") %>%
  data.frame(href = .) %>%
  filter(grepl("full-scorecard", href) & grepl(id, href) & !duplicated(href))

clusterEvalQ(cl, library("data.table"))
clusterEvalQ(cl, library("dplyr"))
clusterEvalQ(cl, library("fst"))
clusterEvalQ(cl, library("rvest"))
clusterEvalQ(cl, library("stringr"))

matchInfo <- rbindlist(parLapply(cl, t$href, getMatchInfo), use.names=TRUE, fill=TRUE) %>%
  arrange(date, matchID) %>%
  mutate(superOver = ifelse(grepl("eliminator", result), TRUE, FALSE))

stopImplicitCluster()

saveMatchInfo(matchInfo, overwrite=TRUE)

p <- as.integer(1008)
driver <- rsDriver(browser=c("chrome"), port = p, geckover = NULL, chromever = "94.0.4606.41")
remDr <- driver[["client"]]

test <- mapply(scrapeIPL, matchInfo$url, seq(1:nrow(matchInfo)), superOver=matchInfo$superOver)


matchInfo <- matchInfo %>%
  rowwise() %>%
  mutate(overwrite = ifelse(is.null(checkData(matchID)), FALSE, TRUE))

# match1 <- scrapeIPL(matchInfo$url[1], 1, matchInfo$superOver[1], cookiesDeclined = TRUE, notNowClicked = TRUE, overwrite=TRUE)
# testna2 <- match14[[1]] %>% filter(is.na(Bowler))

# saveRDS(test, paste0(outLoc, "All Balls (Men).RDS"))


index <- NULL
for (x in c(seq(1, length(test), 3))) {
  if(!is.na(test[x])) {
    index <- c(index, x)
  }
}


allBalls <- rbindlist(test[index])  


check <- matchInfo %>%
  filter(superOver) %>%
  select(matchID)

check <- allBalls %>% semi_join(check, by="matchID") %>% group_by(matchID, innings) %>% summarise(b = n())



test2 <- lapply(test[index], summaryT20, "batting")
test3 <- lapply(test[index], summaryT20, "bowling")
test4 <- lapply(test[index], summaryT20, "innings")

saveRDS(test2, paste0(outLoc, "Batting.RDS"))
saveRDS(test3, paste0(outLoc, "Bowling.RDS"))
saveRDS(test4, paste0(outLoc, "Innings.RDS"))

allBatting <- summaryT20(allBalls, "batting")
allBowling <- summaryT20(allBalls, "bowling")
allInnings <- summaryT20(allBalls, "innings")


# allBatting2 <- allBatting %>%
#   group_by(Batsman) %>%
#   select(-c(batsman, HSText)) %>%
#   mutate(innings = 1) %>%
#   summarise_all(sum) %>%
#   mutate(SR = round(Runs/Balls*100, 2),
#          HS = case_when(
#            HSout == 0 ~ paste0(as.integer(HS / innings), "*"),
#            TRUE ~ paste0(as.integer(HS / innings))),
#          Ave = round(Runs / (innings - NotOut), 2)) %>%
#   select(-c(match, HSout))
# 
# allBowling2 <- allBowling %>%
#   group_by(Bowler) %>%
#   select(-bowler) %>%
#   mutate(innings = 1) %>%
#   summarise_all(sum) %>%
#   mutate(Economy = round(Runs/Overs, 2)) %>%
#   select(-match)


game <- allBalls %>%
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
    # axis.line = element_line(colour="grey75")
    # axis.line.y = element_line(colour="grey75"),
  ) +
  scale_x_continuous(limits=c(0, 20)) +
  scale_y_continuous(limits=c(0, ceiling(max(game$cumruns)/50)*50)) +
  # xlim(c(0, 20)) +
  # ylim(c(0, ceiling(max(game$cumruns)/50)*50)) +
  annotate("segment", x=-Inf, xend=Inf, y=0, yend=0, size=0.5, colour="grey75") +
  annotate("segment", x=0, xend=0, y=-Inf, yend=Inf, size=0.5, colour="grey75") +
  labs(
    title = "Worm Charts",
    subtitle = "All T20 World Cup Games (so far)",
    x = "Overs",
    y = "Runs"
  ) +
  facet_wrap(~matchDesc, scales="free_x")
  # theme(
  #   legend.position = "none"
  # )

game2 <- allBalls %>%
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
  scale_fill_manual(breaks=teamNames, values=teamColours) +
  theme(
    # legend.position = "bottom",
    panel.grid.major.y = element_line(colour="grey95")
    # panel.grid.major.x = element_line(colour="grey95"),
    # axis.line = element_line(colour="grey75")
    # axis.line.y = element_line(colour="grey75"),
  ) +
  scale_y_continuous(limits=c(0, ceiling(max(game2$runs)/5)*5)) +
  annotate("segment", x=-Inf, xend=Inf, y=0, yend=0, size=0.5, colour="grey75") +
  # scale_x_discrete(seq(0:20)) + 
  # xlim(c(0,20)) +
  labs(
    title = "Manhattan Charts",
    subtitle = "All T20 World Cup Games (so far)",
    x = "Over",
    y = "Runs"
  ) +
  facet_wrap(~matchDesc)


ggplot(game2, aes(x = over, y = crr, colour=team)) +
  geom_line() +
  theme_kantar() +
  scale_colour_manual(breaks=teamNames, values=teamColours) +
  theme(
    # legend.position = "bottom",
    panel.grid.major.y = element_line(colour="grey95")
    # panel.grid.major.x = element_line(colour="grey95"),
    # axis.line = element_line(colour="grey75")
    # axis.line.y = element_line(colour="grey75"),
  ) +
  scale_y_continuous(limits=c(0, ceiling(max(game2$crr)/5)*5)) +
  # scale_x_discrete(seq(0:20)) + 
  # xlim(c(0,20)) +
  labs(
    title = "Rute Rate Charts",
    subtitle = "All T20 World Cup Games (so far)",
    x = "Over",
    y = "Runs"
  ) +
  facet_wrap(~matchDesc)


for (x in matchInfo2$matchID) {
  check <- file.exists(paste0("data/ball-by-ball/", x, ".fst"))
  
  if (!check) {
    print(paste0(x, " is missing!"))
  }
}