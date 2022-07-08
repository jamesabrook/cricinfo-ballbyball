################################################################################
############        Scraping Ball-by-Ball data for The Ashes        ############
############                 James Brook 2022-07-08                 ############
################################################################################


# eval(parse("src/x_util.R", encoding = "UTF-8"))
# eval(parse("src/scrapeTheAshes.R", encoding = "UTF-8"))

############
# Packages 
############

library(data.table)
library(dplyr)
library(doParallel)
library(foreach)
library(ggplot2)
library(jsonlite)
library(kwutilr)
library(pbapply)
library(rvest)
library(stringr)


############
# Functions 
############

getMatchInfo <- function(x) {
  url <- paste0("https://www.espncricinfo.com", x)
  
  #Read the raw html data
  raw <- try(xml2::read_html(url), silent = TRUE)
  if ("try-error" %in% class(raw)) 
    stop("Error in URL")
  
  #Match description
  desc <- raw %>%
    rvest::html_nodes("div.ds-text-tight-m") %>%
    rvest::html_text() %>%
    utils::head(1) %>%
    base::data.frame(desc = .) %>%
    dplyr::mutate(url     = url,
                  seriesID = substr(url, regexpr("-\\d*/", url) + 1, regexpr("-\\d*/", url) + attributes(regexpr("-\\d*/", url))$match.length - 2),
                  matchID = substr(url, nchar(url) - 21, nchar(url)-15),
                  round   = strsplit(desc, ", ")[[1]][1],
                  groupID = strsplit(desc, ", ")[[1]][2],
                  days    = strsplit(desc, ", ")[[1]][3]) %>%
    dplyr::select(-desc)
  
  #Teams
  teams <- raw %>%
    rvest::html_nodes("a.ds-text-ui-typo") %>%
    rvest::html_text() %>%
    base::data.frame(teams=.) %>%
    dplyr::mutate(homeTeam = "Australia", # paste0(strsplit(teams, " ")[[1]], collapse=" "),
                  awayTeam = "England") %>% #paste0(strsplit(teams, " ")[[2]], collapse=" ")) %>%
    dplyr::select(homeTeam, awayTeam) %>%
    dplyr::slice(1)
  
  #Result
  result <- raw %>%
    rvest::html_nodes("p.ds-text-tight-m") %>%
    rvest::html_text() %>%
    base::data.frame(result = .) %>%
    slice(nrow(.)) %>%
    # dplyr::filter(grepl(teams$homeTeam, result) | grepl(teams$awayTeam, result)) %>%
    as.character(.)
  
  #Further details
  tables <- rvest::html_table(raw, fill=TRUE) 
  
  if(grepl("innings", result)) {
    moreTable <- 7
  } else {
    moreTable <- 9
  }
  
  moreTable <- length(tables) - 3
  
  more <- try(setDT(tables[[moreTable]]), silent=TRUE) 
  setnames(more, names(more), c("Detail", "Value"))
  more <- more[!Detail %like% "debut"] %>% 
    mutate(row = seq(1:n())) %>%
    filter(row > 1) %>%
    select(-row)
  
  
  links <- raw %>%
    rvest::html_nodes("a.ds-decoration-ui-stroke") %>%
    rvest::html_text() %>%
    base::data.frame(Value = .) %>%
    dplyr::mutate(Value = gsub("-", "", Value))
  
  
  
  links <- links %>%
    rowwise() %>%
    mutate(Detail = ifelse(grepl(Value, paste0(more$Value, collapse = " ,")), more$Detail[grep(Value, paste0(more$Value))], "")) %>%
    filter(Detail != "") %>%
    group_by(Value, Detail) %>%
    summarise() %>%
    semi_join(more, by="Detail")
  
  more <- more %>%
    anti_join(links, by="Detail") %>%
    rbind(links) %>%
    group_by(Detail) %>%
    mutate(rows = n(),
           Detail = ifelse(rows > 1, paste0(Detail, " ", seq(1:n())), Detail)) %>%
    select(-rows) %>%
    ungroup() 
  
  if (!grepl("Series 1", paste0(more$Detail, collapse = " "))) {
    more <- more %>%
      mutate(Detail = ifelse(Detail == "Series", "Series 1", Detail))
    
    more <- rbind(more, data.frame(Detail = "Series 2", Value = ""))
  }
  
  #Ammend detail as they are transposed to column names
  more <- more %>%
    mutate(Detail = str_to_title(Detail),
           Detail = gsub(" ", "", paste0(str_to_lower(substr(Detail, 1, 1)), substr(Detail, 2, nchar(Detail)))))
  
  
  # more <- test[[1]][[3]]
  more$Detail <- factor(more$Detail, levels=unique(more$Detail))
  
  more <- more %>% 
    slice(2:nrow(.)) %>%
    dcast(.~Detail, value.var="Value") %>%
    select(-`.`)
  
  details <- cbind(desc, teams, result, more)
  
  return(details)
}


#Saves match information into a master record data/matchInfo.fst
#Can overwrite a matchID if needed or ignore duplicates and append new matches
saveMatchInfo <- function(matchInfo, outLoc="data/", filename="matchInfo.fst", overwrite=FALSE) {
  file <- paste0(outLoc, filename)
  if (file.exists(file)) {
    base <- fst::read.fst(file) 
    
    if (overwrite) {
      base <- base %>%
        dplyr::anti_join(matchInfo, by=c("seriesID", "matchID"))
      
      matchInfo <- rbindlist(list(base, matchInfo), use.names=TRUE, fill=TRUE) %>%
        dplyr::arrange(seriesID, matchID)
      
      fst::write.fst(matchInfo, file)
    } else {
      matchInfo <- matchInfo %>%
        dplyr::anti_join(base, by=c("seriesID", "matchID")) 
      
      matchInfo <- rbindlist(list(base, matchInfo), use.names=TRUE, fill=TRUE) %>%
        dplyr::arrange(date, matchID)
      
      fst::write.fst(matchInfo, file)
    }
    
  } else {
    fst::write.fst(matchInfo, file)
  }
}

#Returns the kth cumulatively ranked element of a vector x
#Used to get the 2nd highest end_row for a batsman to work out partnerships
cumulativeRank <- function(x, k){
  n <- length(x)
  y <- rep(NA, n)
  for (i in (k+1):n){
    y[i] = sort(x[1:i])[i - k]     
  }
  return(y)
}

scrapeInnings <- function(inn, seriesID, matchID) {
  url <- paste0("https://hs-consumer-api.espncricinfo.com/v1/pages/match/comments?lang=en&seriesId=", seriesID, "&matchId=", matchID, "&inningNumber=", inn, "&commentType=ALL&sortDirection=DESC")
  json <- try(xml2::read_html(url), silent = TRUE) %>% html_text()
  json <- fromJSON(json)
  
  data <- json[["comments"]] %>% 
    mutate(checkPostText = sum(!is.na(commentPostTextItems))) %>%
    rowwise() %>%
    mutate(commentTextItems = ifelse(is.null(commentTextItems), NA, commentTextItems[, 2]),
           commentPostTextItems = ifelse(checkPostText == 0, NA, list(unlist(commentPostTextItems)[[2]]))) %>%
    ungroup() %>%
    mutate(checkDismissalText = sum(!is.na(dismissalType)),
           dismissalTextShort = ifelse(checkDismissalText == 0, "", dismissalText[["short"]]),
           dismissalTextLong = ifelse(checkDismissalText == 0, "", dismissalText[["long"]]),
           dismissalTextCommentary = ifelse(checkDismissalText == 0, "", dismissalText[["commentary"]]),
           battingTeam = over[["team"]][["longName"]],
           battingTeamShort = over[["team"]][["abbreviation"]]) %>%
    select(id, inningNumber, oversUnique, oversActual, overNumber, 
           ballNumber, totalRuns, batsmanRuns, isFour, isSix, isWicket,
           dismissalType, dismissalTextShort, dismissalTextLong, dismissalTextCommentary, 
           byes, legbyes, wides, noballs, timestamp, batsmanPlayerId, 
           bowlerPlayerId, totalInningRuns, title, commentTextItems, commentPostTextItems,
           battingTeam, battingTeamShort)
  
  next_over <- json$nextInningOver
  
  while(!is.null(next_over)) {
    url2 <- paste0(url, "&fromInningOver=", next_over)
    json <- try(xml2::read_html(url2), silent = TRUE) %>% html_text()
    json <- fromJSON(json)
    
    data2 <- json[["comments"]] %>% 
      mutate(checkPostText = sum(!is.na(commentPostTextItems))) %>%
      rowwise() %>%
      mutate(commentTextItems = ifelse(is.null(commentTextItems), "", commentTextItems[, 2]),
             commentPostTextItems = ifelse(checkPostText == 0, "", list(unlist(commentPostTextItems)[[2]]))) %>%
      ungroup() %>%
      mutate(checkDismissalText = sum(!is.na(dismissalType)),
             dismissalTextShort = ifelse(checkDismissalText == 0, "", dismissalText[["short"]]),
             dismissalTextLong = ifelse(checkDismissalText == 0, "", dismissalText[["long"]]),
             dismissalTextCommentary = ifelse(checkDismissalText == 0, "", dismissalText[["commentary"]]),
             battingTeam = over[["team"]][["longName"]],
             battingTeamShort = over[["team"]][["abbreviation"]]) %>%
      select(id, inningNumber, oversUnique, oversActual, overNumber, 
             ballNumber, totalRuns, batsmanRuns, isFour, isSix, isWicket,
             dismissalType, dismissalTextShort, dismissalTextLong, dismissalTextCommentary, 
             byes, legbyes, wides, noballs, timestamp, batsmanPlayerId, 
             bowlerPlayerId, totalInningRuns, title, commentTextItems, commentPostTextItems,
             battingTeam, battingTeamShort)
    
    data <- rbind(data, data2)
    
    next_over <- json$nextInningOver
    
    rm(url2, json, data2)
  }
  return(list(data))
}

parseData <- function(match, seriesID, matchID, index, batting_summary, bowling_summary) {
  match <- match %>%
    group_by(inningNumber) %>%
    mutate(battingTeam = max(battingTeam, na.rm=T),
           battingTeamShort = max(battingTeamShort, na.rm=T)) %>%
    ungroup() %>%
    mutate(bowlingTeam = ifelse(battingTeam == min(battingTeam), max(battingTeam), min(battingTeam)),
           bowlingTeamShort = ifelse(battingTeamShort == min(battingTeamShort), max(battingTeamShort), min(battingTeamShort)),
           bowler = gsub("([A-z \\'\\-]+) to.*", "\\1", title),
           batsman = gsub("([A-z \\-]+),.*", "\\1", sub("[A-z \\'\\-]+ to ([A-z ,]+)", "\\1", title)),
           legalBall = ifelse(wides > 0 | noballs > 0, FALSE, TRUE),
           ballFaced = ifelse(wides > 0 , FALSE, TRUE),
           extras = byes + legbyes + wides + noballs,
           battingRuns = totalRuns - extras,
           bowlingRuns = totalRuns - legbyes - byes,
           bowlingWicket = ifelse(isWicket == TRUE & dismissalType != 4, TRUE, FALSE)) %>%
    arrange(inningNumber, overNumber, ballNumber) %>%
    group_by(inningNumber) %>%
    mutate(row = seq(1:n())) %>%
    ungroup()
  
  w <- match %>%
    filter(isWicket) %>%
    group_by(inningNumber) %>%
    mutate(wicketNumber = cumsum(isWicket)) %>%
    ungroup() %>%
    select(inningNumber, batsman, isWicket, totalInningRuns, wicketNumber, dismissalType, dismissalTextShort, dismissalTextLong, dismissalTextCommentary, row)
  
  ballsFaced <- match %>%
    group_by(inningNumber, batsman) %>%
    summarise(Runs = sum(battingRuns),
              Balls = sum(ballFaced),
              `4s` = sum(isFour),
              `6s` = sum(isSix))
  
  ballsBowled <- match %>%
    group_by(inningNumber, bowler) %>%
    summarise(Runs = sum(bowlingRuns),
              Balls = sum(legalBall),
              Overs = floor(Balls/6) + (Balls %% 6 / 10),
              `0s` = sum(bowlingRuns == 0),
              `4s` = sum(isFour),
              `6s` = sum(isSix))
  
  
  batting_summary <- batting_summary %>%
    group_by(inningNumber) %>%
    mutate(battingOrder = seq(1:n()))
  
  bowling_summary <- bowling_summary %>%
    group_by(inningNumber) %>%
    mutate(bowlingOrder = seq(1:n()))
  
  
  batting_summary2 <- merge(ballsFaced, batting_summary, by=c("inningNumber", "Runs", "Balls", "4s", "6s"), all.y=T) %>%
    mutate(Batsman = gsub("\\(c\\)|†", "", Batsman),
           Batsman = trimws(gsub("  ", " ", Batsman)),
           Batsman = ifelse(grepl("[A-z]", substr(Batsman, nchar(Batsman), nchar(Batsman))), Batsman, substr(Batsman, 1, nchar(Batsman) - 1))) %>%
    select(inningNumber, batsman, Batsman, battingOrder) %>%
    group_by(inningNumber, batsman) %>%
    mutate(rows = n(),
           batsman = ifelse(is.na(batsman) & battingOrder <= 11, substr(Batsman, regexpr(" ", Batsman) + 1, nchar(Batsman)), batsman)) %>%
    rowwise() %>%
    filter(rows == 1 | grepl(batsman, Batsman))
  
  bowling_summary2 <- merge(ballsBowled, bowling_summary, by=c("inningNumber", "Runs", "Overs", "0s", "4s", "6s")) %>%
    mutate(Bowler = gsub("\\(c\\)|†", "", Bowler),
           Bowler = trimws(gsub("  ", " ", Bowler)),
           Bowler = ifelse(grepl("[A-z]", substr(Bowler, nchar(Bowler), nchar(Bowler))), Bowler, substr(Bowler, 1, nchar(Bowler) - 1))) %>%
    select(inningNumber, bowler, Bowler, bowlingOrder) %>%
    group_by(inningNumber, bowler) %>%
    mutate(rows = n()) %>%
    rowwise() %>%
    filter(rows == 1 | grepl(bowler, Bowler))
  
  
  w2 <- merge(w, batting_summary2, by="inningNumber")  %>%
    rowwise() %>%
    mutate(d = grepl(Batsman, dismissalTextCommentary)) %>%
    filter(d) %>%
    group_by(inningNumber) %>%
    mutate(wicketOrder = seq(1:n())) %>%
    select(inningNumber, endRow=row, dismissed = batsman.y, wicketOrder) 
  
  partnerships <- match %>%
    group_by(inningNumber, batsman) %>%
    summarise(startRow = min(row),
              firstFaced = min(row),
              lastFaced = max(row)) %>%
    left_join(w2, by=c("inningNumber", "batsman"="dismissed")) 
  
  partnerships <- batting_summary2 %>%
    left_join(partnerships, by=c("inningNumber", "batsman")) %>%
    group_by(inningNumber) %>%
    mutate(endRow = ifelse(is.na(endRow), max(lastFaced, na.rm=T), endRow)) %>%
    arrange(inningNumber, battingOrder) %>%
    mutate(startRow = ifelse(is.na(lag(startRow, 2)), 1, cumulativeRank(endRow, 2) + 1),
           prev_endRow = lag(endRow), 
           prev_endRow2 = lag(endRow, 2),
           cum_endRow_max = lag(cummax(endRow))) %>%
    group_by(inningNumber) %>%
    mutate(prev_startRow = lag(startRow),
           startRow = ifelse(startRow == prev_startRow & startRow != 1, prev_startRow + 1, startRow)) %>%
    select(inningNumber, batsman, startRow, firstFaced, lastFaced, endRow, Batsman, battingOrder) %>%
    arrange(inningNumber, desc(endRow)) %>%
    mutate(endRow = ifelse(seq(1:n()) == 2, lag(endRow), endRow)) %>%
    arrange(inningNumber, battingOrder)
  
  partnerships <- partnerships %>%
    left_join(partnerships, by=c("inningNumber")) %>%
    filter(batsman.x != batsman.y & endRow.x >= startRow.y & startRow.x <= startRow.y) %>%
    group_by(inningNumber, batsman.x, batsman.y) %>%
    mutate(startRow = max(startRow.x, startRow.y, na.rm=T),
           endRow = min(endRow.x, endRow.y, na.rm=T)) %>%
    select(inningNumber, batsman1 = batsman.x, batsman2 = batsman.y, startRow, endRow) %>%
    group_by(inningNumber) %>%
    arrange(inningNumber, startRow) %>%
    filter(seq(1:n()) != 2)
  
  match <- match %>%
    left_join(partnerships, by=c("inningNumber")) %>%
    filter(row >= startRow & row <= endRow) %>%
    mutate(nonStriker = ifelse(batsman == batsman1, batsman2, batsman1)) %>%
    left_join(w2, by=c("inningNumber", "row"="endRow")) %>%
    left_join(batting_summary2, by=c("inningNumber", "batsman")) %>%
    left_join(bowling_summary2, by=c("inningNumber", "bowler"))
  
  match <- match %>%
    mutate(seriesID = seriesID, 
           matchID = matchID,
           match = index) %>%
    select(seriesID,
           matchID, 
           match,
           inningNumber,
           id,
           timestamp,
           battingTeamShort,
           battingTeam,
           bowlingTeamShort,
           bowlingTeam,
           oversUnique,
           oversActual,
           overNumber, 
           ballNumber,
           legalBall,
           ballFaced,
           title,
           commentTextItems,
           # commentPostTextItems,
           bowler,
           Bowler,
           bowlerPlayerId,
           batsman,
           Batsman,
           batsmanPlayerId,
           nonStriker,
           totalInningRuns,
           totalRuns,
           battingRuns,
           bowlingRuns,
           byes,
           legbyes,
           wides,
           noballs,
           extras,
           isFour,
           isSix,
           isWicket,
           dismissalType,
           dismissalTextShort,
           dismissalTextLong,
           dismissalTextCommentary,
           wicketOrder,
           dismissed,
           bowlingWicket,
           battingOrder, 
           bowlingOrder)
  
}

scrapeTheAshes <- function(url, seriesID, matchID, index, overwrite=FALSE) {
  #output file(s)
  file1 <- paste0(outLoc, "/", seriesID,  "/ball-by-ball/", matchID, ".fst")
  file2 <- paste0(outLoc, "/", seriesID,  "/batting-summaries/", matchID, ".fst")
  file3 <- paste0(outLoc, "/", seriesID,  "/bowling-summaries/", matchID, ".fst")
  
  #Create output location
  dir.create(paste0(outLoc), showWarnings = F)
  dir.create(paste0(outLoc, "/", seriesID), showWarnings = F)
  dir.create(paste0(outLoc, "/", seriesID,  "/ball-by-ball/"), showWarnings = F)    #Doesn't crash if the directory already exists
  dir.create(paste0(outLoc, "/", seriesID,  "/batting-summaries/"), showWarnings = F)    #Doesn't crash if the directory already exists
  dir.create(paste0(outLoc, "/", seriesID,  "/bowling-summaries/"), showWarnings = F)    #Doesn't crash if the directory already exists
  
  #If we need to overwrite the data or scrape if it doesn't exist already
  if(overwrite | !file.exists(file1)) {
    print(paste0("Index: ", index))
    
    #Read the raw html data
    raw <- try(xml2::read_html(url), silent = TRUE)
    if ("try-error" %in% class(raw)) 
      stop("Error in URL")
    
    #Teams
    # teams <- raw %>%
    #   rvest::html_nodes("span.ds-text-tight-l") %>%
    #   rvest::html_text() 
    
    #Result
    result <- raw %>%
      rvest::html_nodes("p.ds-text-tight-m") %>%
      rvest::html_text() %>%
      base::data.frame(result = .) %>%
      slice(nrow(.)) %>%
      # dplyr::filter(grepl(teams$homeTeam, result) | grepl(teams$awayTeam, result)) %>%
      as.character(.)
    
    #Extract tables
    tables <- rvest::html_table(raw, fill=TRUE)
    
    #Layout of tables:
    # - Detailed 1st innings batting scorecard
    # - Detailed 1st innings bowling scorecard
    # - Detailed 2nd innings batting scorecard
    # - Detailed 2nd innings bowling scorecard
    # - Match details (ground, toss etc.)
    # - Sidebar batting scorecard (last innings?)
    # - Sidebar bowling scorecard (last innings?)
    # - Sidebar wickets (last innings?)
    # - Table
    #
    
    if(grepl("innings", result)) {
      batting_summary <- try(setDT(rbind(setDT(tables[[1]][, 1:8])[, innings:=1], 
                                         setDT(tables[[3]][, 1:8])[, innings:=2], 
                                         setDT(tables[[5]][, 1:8])[, innings:=3])),
                             silent=TRUE)
      
      
      bowling_summary <- try(setDT(rbind(setDT(tables[[2]])[, innings:=1], 
                                         setDT(tables[[4]])[, innings:=2], 
                                         setDT(tables[[6]])[, innings:=3])),
                             silent=TRUE)
      
      matchInfo <- try(setDT(tables[[7]]), silent=TRUE) 
      setnames(matchInfo, names(matchInfo), c("Detail", "Value"))
      
      innlist <- c(1, 2, 3)
    } else if(length(tables) < 12) {
      
      moreTable <- length(tables) - 3
      
      batting_summary <- try(setDT(rbind(setDT(tables[[1]][, 1:8])[, innings:=1], 
                                         setDT(tables[[3]][, 1:8])[, innings:=2], 
                                         setDT(tables[[5]][, 1:8])[, innings:=3])),
                             silent=TRUE)
      
      
      bowling_summary <- try(setDT(rbind(setDT(tables[[2]])[, innings:=1], 
                                         setDT(tables[[4]])[, innings:=2], 
                                         setDT(tables[[6]])[, innings:=3])),
                             silent=TRUE)
      
      matchInfo <- try(setDT(tables[[moreTable]]), silent=TRUE) 
      setnames(matchInfo, names(matchInfo), c("Detail", "Value"))
      
      innlist <- c(1, 2, 3)
    } else {
      batting_summary <- try(setDT(rbind(setDT(tables[[1]][, 1:8])[, innings:=1], 
                                         setDT(tables[[3]][, 1:8])[, innings:=2], 
                                         setDT(tables[[5]][, 1:8])[, innings:=3], 
                                         setDT(tables[[7]][, 1:8])[, innings:=4])),
                             silent=TRUE)
      
      
      bowling_summary <- try(setDT(rbind(setDT(tables[[2]])[, innings:=1], 
                                         setDT(tables[[4]])[, innings:=2], 
                                         setDT(tables[[6]])[, innings:=3], 
                                         setDT(tables[[8]])[, innings:=4])),
                             silent=TRUE)
      
      matchInfo <- try(setDT(tables[[9]]), silent=TRUE) 
      setnames(matchInfo, names(matchInfo), c("Detail", "Value"))
      innlist <- c(1, 2, 3, 4)
    }
    
    if (sum(class(batting_summary) != "try-error") == length(class(batting_summary))) {
      #Set the names so that we don't have any duplicates
      setnames(batting_summary, names(batting_summary), c("Batsman", "Dismissal", "Runs", "Balls", "Minutes", "4s", "6s", "SR", "inningNumber"))
      setnames(bowling_summary, names(bowling_summary), c("Bowler", "Overs", "Maidens", "Runs", "Wickets", "Economy", "0s", "4s", "6s", "WDs", "NBs", "inningNumber"))
      
      #Retain batsman rows and Extras/Total lines
      #Removes others
      batting_summary <- batting_summary[Batsman != "" & !Batsman %like% "Fall of" & !Batsman %like% "Did not bat"]
      
      #Filter out empty rows
      bowling_summary <- bowling_summary[WDs != ""]
      
      #This is a flag for which team is batting first
      # batFirst <- matchInfo %>%
      #   slice(2) %>%
      #   mutate(team = case_when(
      #     grepl(teams[1], Value) & grepl("field", Value) ~ 0,
      #     grepl(teams[1], Value) & !grepl("field", Value) ~ 1,
      #     grepl(teams[2], Value) & grepl("field", Value) ~ 1,
      #     grepl(teams[2], Value) & !grepl("field", Value) ~ 0,
      #     TRUE ~ 2)) 
      
      match <- rbindlist(mapply(scrapeInnings, innlist, seriesID, matchID))
      
      match <- setDT(parseData(match, seriesID, matchID, index, batting_summary, bowling_summary))
      
      write.fst(match, file1)
      write.fst(batting_summary, file2)
      write.fst(bowling_summary, file3)
    } else {
      match <- NA
    }
  } # else {
  #   match <- read.fst(file1, as.data.table = TRUE)
  #   batting_summary <- read.fst(file2, as.data.table = TRUE)
  #   bowling_summary <- read.fst(file3, as.data.table = TRUE)
  # } 
  
  # return(list(match, batting_summary, bowling_summary))
}

#Returns the ball-by-ball data for a given matchID
importMatch <- function(matchID, seriesID) {
  return(read.fst(paste0("data/", seriesID, "/ball-by-ball/", matchID, ".fst")))
}

#Combine all balls from a particular series
importSeries <- function(seriesID) {
  matchIDs <- read.fst("data/matchInfo.fst") %>%
    dplyr::filter(seriesID == seriesID) %>%
    dplyr::filter(!grepl("abandoned|No result", result)) %>%
    dplyr::select(seriesID, matchID)
  
  tryCatch({
    balls <- lapply(matchIDs$matchID, importMatch, matchIDs$seriesID)
    # balls <- lapply(paste0("data/ball-by-ball/", matchIDs$matchID, ".fst"), read.fst)
    return(data.table::rbindlist(balls))
  },
  error = function(e) { 
    message("Error - scrape data")
  })
}

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
  arrange(seriesID, matchID)

stopImplicitCluster()

saveMatchInfo(matchInfo, overwrite=TRUE)

mapply(scrapeTheAshes, url=matchInfo$url, matchInfo$seriesID, matchInfo$matchID, seq(1:nrow(matchInfo)))

ashes <- importSeries(seriesID=matchInfo$seriesID[1])

check <- ashes %>%
  group_by(seriesID, matchID, inningNumber, batsman) %>%
  summarise(runs = sum(battingRuns),
            balls = sum(ballFaced),
            mins = max(timestamp) - min(timestamp))

