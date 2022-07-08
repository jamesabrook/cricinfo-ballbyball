################################################################################
############       Scraping Ball-by-Ball data for The Hundred       ############
############                 James Brook 2021-07-24                 ############
################################################################################

# source("src/x_util.R")
eval(parse("src/x_util.R", encoding = "UTF-8"))
eval(parse("src/scrapeTheAshes.R", encoding = "UTF-8"))

############
# Setup 
############

options(dplyr.summarise.inform = FALSE)

cl <- makeCluster(detectCores() - 2)
registerDoParallel(cl)

#Where to save output data
outLoc <- paste0("data/")

#ESPN cricinfo results page for the desired tournament
url <- "https://www.espncricinfo.com/series/england-in-australia-2021-22-1263452/match-results"


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
  filter(grepl("full-scorecard", href) & grepl(id, href) &!duplicated(href) & grepl("test", href))


clusterEvalQ(cl, library("data.table"))
clusterEvalQ(cl, library("dplyr"))
clusterEvalQ(cl, library("fst"))
clusterEvalQ(cl, library("rvest"))
clusterEvalQ(cl, library("stringr"))

matchInfo <- rbindlist(parLapply(cl, t$href, getMatchInfo), use.names=TRUE, fill=TRUE) %>%
  # arrange(date, matchID) %>%
  mutate(superOver = FALSE)

stopImplicitCluster()


p <- as.integer(4009)
driver <- rsDriver(browser=c("chrome"), port = p, geckover = NULL, chromever = "103.0.5060.24")
remDr <- driver[["client"]]

ashes <- mapply(scrapeTheAshes, matchInfo$url, seq(1:nrow(matchInfo))) #,
                #cookiesDeclined=TRUE, notNowClicked=TRUE)


matchInfo <- matchInfo %>%
  rowwise() %>%
  mutate(overwrite = ifelse(is.null(checkDataT20(matchID)), FALSE, TRUE))


saveMatchInfo(matchInfo, overwrite=TRUE)

matchInfo2 <- read.fst("data/matchInfo.fst")

match <- importMatch(1263464)

ashes <- importSeries("The Ashes")

battingInnings <- summaryT20(ashes, "batting")
battingTotal <- battingInnings %>%
  group_by(batsman, Batsman) %>%
  arrange(desc(Runs)) %>%
  summarise(Runs = sum(Runs),
            Balls = sum(Balls),
            `4s` = sum(`4s`),
            `6s` = sum(`6s`),
            `100` = sum(`100`),
            `50` = sum(`50`),
            `0` = sum(`0`),
            NotOut = sum(NotOut),
            HSText = first(HSText)) %>%
  mutate(SR = Runs / Balls * 100) %>%
  arrange(desc(Runs))

manhattanChart(ashes)


#Plot batsman innings
data <- ashes %>%
  group_by(Batsman, ball) %>%
  summarise(runs = sum(batting_runs)) %>%
  group_by(Batsman) %>%
  mutate(ball2 = seq(1:n()),
         runs = cumsum(runs))


ggplot(data = data %>% filter(max(runs) >= 100), aes(x=ball2, y=runs, colour=Batsman)) +
  geom_line(size=1) +
  theme_kantar() +
  theme(
    panel.grid.major = element_line(colour = "grey95"),
    legend.position = "bottom"
  ) +
  labs(title = "Cumulative Runs Scored",
       subtitle = "England tour of Australia 2010/11",
       caption = "Minimum 100 runs scored",
       x = "Balls Faced",
       y = "Runs Scored") +
  ylim(c(0, ceiling(max(data$runs)/25)*25))
