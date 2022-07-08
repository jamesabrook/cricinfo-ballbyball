#Test new approach
library(jsonlite)

scrapeInnings <- function(inn, seriesID, matchID) {
  url <- paste0("https://hs-consumer-api.espncricinfo.com/v1/pages/match/comments?lang=en&seriesId=", seriesID, "&matchId=", matchID, "&inningNumber=", inn, "&commentType=ALL&sortDirection=DESC")
  json <- try(xml2::read_html(url), silent = TRUE) %>% html_text()
  json <- fromJSON(json)
  
  data <- json[["comments"]] %>% 
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

scrapeTheAshes <- function(url, seriesID, matchID, index, overwrite=FALSE) {
  #output file(s)
  file1 <- paste0(outLoc, "/", seriesID,  "/ball-by-ball/", matchID, ".fst")
  file2 <- paste0(outLoc, "/", seriesID,  "/batting-summaries/", matchID, ".fst")
  file3 <- paste0(outLoc, "/", seriesID,  "/bowling-summaries/", matchID, ".fst")
  
  #Create output location
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
    teams <- raw %>%
      rvest::html_nodes("span.ds-text-tight-l") %>%
      rvest::html_text() 
    
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
  
    if (class(batting_summary) != "try-error") {
      #Set the names so that we don't have any duplicates
      setnames(batting_summary, names(batting_summary), c("Batsman", "Dismissal", "Runs", "Balls", "Minutes", "4s", "6s", "SR", "inningNumber"))
      setnames(bowling_summary, names(bowling_summary), c("Bowler", "Overs", "Maidens", "Runs", "Wickets", "Economy", "0s", "4s", "6s", "WDs", "NBs", "inningNumber"))
      
      #Retain batsman rows and Extras/Total lines
      #Removes others
      batting_summary <- batting_summary[Batsman != "" & !Batsman %like% "Fall of" & !Batsman %like% "Did not bat"]
      
      #Filter out empty rows
      bowling_summary <- bowling_summary[Bowler != ""]
      
      #This is a flag for which team is batting first
      batFirst <- matchInfo %>%
        slice(2) %>%
        mutate(team = case_when(
          grepl(teams[1], Value) & grepl("field", Value) ~ 0,
          grepl(teams[1], Value) & !grepl("field", Value) ~ 1,
          grepl(teams[2], Value) & grepl("field", Value) ~ 1,
          grepl(teams[2], Value) & !grepl("field", Value) ~ 0,
          TRUE ~ 2)) 
      
      match <- rbindlist(mapply(scrapeInnings, innlist, seriesID, matchID))
    }
  }
  
  return(match)
}

ashes <- scrapeTheAshes(url=matchInfo$url[1], seriesID=matchInfo$seriesID[1], matchID=matchInfo$matchID[1], index=1, overwrite=FALSE)

# ashes <- mapply(scrapeTheAshes, matchInfo$url, matchInfo$seriesID, matchInfo$matchID, seq(1:nrow(matchInfo)))
ashes2 <- match %>%
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

w <- ashes2 %>%
  filter(isWicket) %>%
  group_by(inningNumber) %>%
  mutate(wicketNumber = cumsum(isWicket)) %>%
  ungroup() %>%
  select(inningNumber, batsman, isWicket, totalInningRuns, wicketNumber, dismissalType, dismissalTextShort, dismissalTextLong, dismissalTextCommentary, row)

ballsFaced <- ashes2 %>%
  group_by(inningNumber, batsman) %>%
  summarise(Runs = sum(battingRuns),
            Balls = sum(ballFaced),
            `4s` = sum(isFour),
            `6s` = sum(isSix))

ballsBowled <- ashes2 %>%
  group_by(inningNumber, bowler) %>%
  summarise(Runs = sum(bowlingRuns),
            Balls = sum(legalBall),
            Overs = floor(Balls/6) + (Balls %% 6 / 10),
            `0s` = sum(bowlingRuns == 0),
            `4s` = sum(isFour),
            `6s` = sum(isSix))
            

batting_summary <- batting_summary %>%
  rename(inningNumber = Innings) %>%
  group_by(inningNumber) %>%
  mutate(battingOrder = seq(1:n()))

bowling_summary <- bowling_summary %>%
  rename(inningNumber = Innings) %>%
  group_by(inningNumber) %>%
  mutate(bowlingOrder = seq(1:n()))


batting_summary2 <- merge(ballsFaced, batting_summary, by=c("inningNumber", "Runs", "Balls", "4s", "6s")) %>%
  mutate(Batsman = gsub("\\(c\\)|†", "", Batsman),
         Batsman = trimws(gsub("  ", " ", Batsman)),
         Batsman = ifelse(grepl("[A-z]", substr(Batsman, nchar(Batsman), nchar(Batsman))), Batsman, substr(Batsman, 1, nchar(Batsman) - 1))) %>%
  select(inningNumber, batsman, Batsman, battingOrder) %>%
  group_by(inningNumber, batsman) %>%
  mutate(rows = n()) %>%
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
  select(inningNumber, end_row=row, dismissed = batsman.y, wicketOrder) 
