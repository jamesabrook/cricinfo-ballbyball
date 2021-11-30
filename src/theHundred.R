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

cl <- makeCluster(detectCores() - 2)
registerDoParallel(cl)

#Where to save output data
outLoc <- paste0("data/")

#ESPN cricinfo results page for the desired tournament
# url <- "https://www.espncricinfo.com/series/the-hundred-men-s-competition-2021-1252040/match-results"
url <- "https://www.espncricinfo.com/series/the-hundred-women-s-competition-2021-1252659/match-results"

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

#A couple of games that were abandoned before a ball was bowled have some missing info, let's fix...
matchInfo[is.na(ballsPerOver), ballsPerOver:=5]

stopImplicitCluster()


p <- as.integer(4006)
driver <- rsDriver(browser=c("chrome"), port = p, geckover = NULL, chromever = "96.0.4664.45")
remDr <- driver[["client"]]

hundred <- mapply(scrapeTheHundred, matchInfo$url, seq(1:nrow(matchInfo)), superOver=matchInfo$superOver, overwrite=matchInfo$overwrite)

matchInfo <- matchInfo %>%
  rowwise() %>%
  mutate(overwrite = ifelse(is.null(checkDataHundred(matchID)), FALSE, TRUE))

saveMatchInfo(matchInfo, overwrite=TRUE)

theHundredWomens <- importSeries("The Hundred Women's Competition")

battingInnings <- summaryTheHundred(theHundredWomens, "batting")
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

manhattanChart(theHundredWomens)


