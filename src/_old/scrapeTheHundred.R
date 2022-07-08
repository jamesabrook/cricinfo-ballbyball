


parseData <- function(match, matchID, index, teams, batFirst, batting_summary, bowling_summary) {
  match <- match %>%
    mutate(team = case_when(
      innings %in% c(1, 4) ~ teams[2 - batFirst$team],
      TRUE ~ teams[1 + batFirst$team]),
    ) %>%
    group_by(ball, fullDesc) %>%
    mutate(
      #Inserts a new line character into the full description.  Doesn't seem to work?
      fullDesc = gsub(shortDesc, paste0(shortDesc, "\n"), fullDesc),
      #Extract the bowler's name from the short description
      bowler = gsub("([A-z \\'\\-]+) to.*", "\\1", shortDesc),
      #Also try and extract a second copy of this name with initial if it exists
      bowler3 = case_when(
        ball == "0.1" ~ gsub(paste0(".*?([A-Z] ", bowler, ") .*"), "\\1", all),
        grepl("\\W1", ball) ~ gsub(paste0(".*([A-Z] ", bowler, ") .*"), "\\1", all),
        TRUE ~ ""),
      #Extract the batsman's name from the short description
      batsman = gsub("([A-z \\-]+),.*", "\\1", sub("[A-z \\'\\-]+ to ([A-z ,]+)", "\\1", shortDesc)),
      #Ensure the ball is numeric
      ball = as.numeric(ball),
      #If there is a wicket, this overrides the event column
      #This checks the description to see if any runs were scored (run outs etc)
      runsCheck = case_when(
        grepl("no run", shortDesc) ~ 0,
        grepl("1", shortDesc) ~ 1,
        grepl("2", shortDesc) ~ 2,
        grepl("3", shortDesc) ~ 3,
        grepl("FOUR", shortDesc) ~ 4,
        grepl("5", shortDesc) ~ 5,
        grepl("SIX", shortDesc) ~ 6,
        #This appears not to work
        # grepl("[0-6]", shortDesc) ~ as.integer(grep("[0-6]", shortDesc)),
        TRUE ~ 0),
      batting_runsCheck = case_when(
        grepl("no run|leg bye|no ball|wide", shortDesc) ~ 0,
        grepl("1", shortDesc) ~ 1,
        grepl("2", shortDesc) ~ 2,
        grepl("3", shortDesc) ~ 3,
        grepl("FOUR", shortDesc) ~ 4,
        grepl("5", shortDesc) ~ 5,
        grepl("SIX", shortDesc) ~ 6,
        #This appears not to work
        # grepl("[0-6]", shortDesc) ~ as.integer(grep("[0-6]", shortDesc)),
        TRUE ~ 0),
      #Was the ball legal - used to sort in the appropriate order
      legalBall = case_when(
        grepl("wide|no ball", shortRuns) ~ 0,
        TRUE ~ 1
      ),
      #Was the ball faced - used to sort in the appropriate order
      ballFaced = case_when(
        grepl("wide", shortRuns) ~ 0,
        TRUE ~ 1
      ),
      #Extract the number of runs scored
      runs = case_when(
        event %in% c(".") ~ 0,
        event %in% c("W") ~ as.double(runsCheck), #Here we use the runsCheck column to ensure we capture all runs scored
        grepl("1", event) ~ 1,
        grepl("2", event) ~ 2,
        grepl("3", event) ~ 3,
        grepl("4", event) ~ 4,
        grepl("5", event) ~ 5,
        grepl("6", event) ~ 6,
        grepl("7", event) ~ 7,
        grepl("8", event) ~ 8,
        TRUE ~ -9),
      #How many runs should be awarded to the batsman?
      batting_runs = case_when(
        event == "W" ~ batting_runsCheck, 
        grepl("lb|w|\\.|n\\-l", event) ~ 0, #lb and wides can't have any runs for the batsman
        grepl("nb", event) ~ runs - 2, #Remove a run for a no ball
        grepl("b", event) ~ 0, #Byes can't be awared to the batsman
        grepl("1", event) ~ 1,
        grepl("2", event) ~ 2,
        grepl("3", event) ~ 3,
        grepl("4", event) ~ 4,
        grepl("5", event) ~ 5,
        grepl("6", event) ~ 6,
        TRUE ~ -9),
      #How many runs should be counted against the bowler?
      bowling_runs = case_when(
        event == "W" ~ runsCheck,
        grepl("lb|\\.", event) ~ 0, #Leg byes aren't awarded against the bowler
        grepl("nb", event) ~ runs,
        grepl("n\\-b|n\\-l", event) ~ 2, #No-ball + (leg) bye counts as 2 against the bowler
        grepl("b", event) ~ 0, #Byes don't count against the bowler
        grepl("1", event) ~ 1,
        grepl("2", event) ~ 2,
        grepl("3", event) ~ 3,
        grepl("4", event) ~ 4,
        grepl("5", event) ~ 5,
        grepl("6", event) ~ 6,
        grepl("7", event) ~ 7,
        grepl("8", event) ~ 8,
        TRUE ~ -9),
      #Did a wicket fall on this ball?
      wickets = case_when(
        grepl("W", event) ~ 1,
        grepl("OUT", shortDesc) ~ 1,
        TRUE ~ 0),
      #Does the wicket count for the bowler? (TODO: Other non bowler dismissals)
      bowling_wickets = case_when(
        grepl("W", event) & !grepl("run out", wicketDesc) ~ 1,
        grepl("OUT", shortDesc) & !grepl("run out", wicketDesc) ~ 1,
        TRUE ~ 0),
      extras = case_when(
        grepl("nb", event) ~ 2, #No balls count as 2
        # grepl("b|lb|n-b|n-l|w", event) ~ as.integer(grep("[1-6]", event)), #byes, leg byes, no balls + (leg) byes & wides
        grepl("1[b|lb|n\\-b|n\\-l|w]", event) ~ 1,
        grepl("2[b|lb|n\\-b|n\\-l|w]", event) ~ 2,
        grepl("3[b|lb|n\\-b|n\\-l|w]", event) ~ 3,
        grepl("4[b|lb|n\\-b|n\\-l|w]", event) ~ 4,
        grepl("5[b|lb|n\\-b|n\\-l|w]", event) ~ 5,
        grepl("6[b|lb|n\\-b|n\\-l|w]", event) ~ 6,
        grepl(".|[1-6]|W", event) ~ 0, #No extras
        TRUE ~ -9)
    ) %>%
    arrange(innings, ball, legalBall) %>%
    group_by(innings) %>%
    mutate(row = seq(1:n())) 
  
  #Wickets to fall in the match
  w <- match %>%
    filter(event == "W" | grepl("OUT", shortDesc)) %>%
    select(innings, row, batsman, wickets, wicketDesc)
  
  #Check to see if a name is used more than once (*cough* YADAV *cough*)
  w2 <- w %>%
    filter(!grepl("run out", wicketDesc)) %>%
    group_by(innings, batsman) %>%
    mutate(rows = n(),
           batsman2 = ifelse(rows > 1, paste0(batsman, seq(1:n())), batsman)) %>%
    filter(rows > 1) %>%
    select(innings, batsman, r=row, batsman2)
  
  #Stick this back onto the match data and relabel the batsman
  match <- match %>%
    left_join(w2, by=c("innings", "batsman")) %>%
    group_by(innings, batsman) %>%
    mutate(keep = ifelse(!is.na(batsman2) & ((row <= min(r) & r == min(r)) | (row > min(r) & r == max(r))), 1, 0),
           batsman = ifelse(!is.na(batsman2) & keep == 1, batsman2, batsman)) %>%
    filter(is.na(batsman2) | keep == 1) %>%
    select(-c(batsman2, keep, r))
  
  #Wickets to fall in the match
  w <- match %>%
    filter(event == "W" | grepl("OUT", shortDesc)) %>%
    select(innings, row, batsman, wickets, wicketDesc)
  
  w2 <- w %>%
    rowwise() %>%
    mutate(dismissed = ifelse(grepl(batsman, wicketDesc), 1, 0)) %>%
    group_by(innings, batsman) %>%
    summarise(dismissed = max(dismissed))
  
  
  #We can use the summary to check what the wickets name is!
  ballsFaced <- match %>%
    group_by(innings, batsman) %>%
    summarise(Innings = first(innings),
              Runs = sum(batting_runs),
              # Balls = sum(ballFaced),
              Balls = sum(legalBall + ifelse(grepl("no ball", shortRuns), 1, 0)),
              `4s` = sum(batting_runs==4 & shortDesc %like% "FOUR"), #Catch in case the four wasn't a boundary
              `6s` = sum(batting_runs==6 & shortDesc %like% "SIX")) %>%
    left_join(w %>% select(innings, batsman, wicketDesc), by=c("innings", "batsman")) %>%
    left_join(w2 %>% select(innings, batsman, dismissed), by=c("innings", "batsman")) %>%
    group_by(innings, batsman) %>%
    mutate(rows = n(),
           rownum = seq(1:n())) %>%
    rowwise() %>%
    filter(rows == 1 | (dismissed == 0 && rownum == 1) | grepl(batsman, wicketDesc) | is.na(wicketDesc))
  
  ballsBowled <- match %>%
    group_by(innings, bowler) %>%
    summarise(Innings = first(innings),
              Runs = sum(bowling_runs),
              Balls = sum(legalBall),
              # Overs = floor(Balls/6) + (Balls %% 6 / 10),
              Dots = sum(bowling_runs == 0),
              `4s` = sum(batting_runs == 4 & shortDesc %like% "FOUR"), #Catch in case the four wasn't a boundary
              `6s` = sum(batting_runs == 6 & shortDesc %like% "SIX"))
  
  #Does anyone have too many balls bowled?
  #This is probably not correct atm!
  ballsBowledTest <- ballsBowled %>% filter(Balls > 20)
  ballsBowledTest2 <- nrow(ballsBowled) == nrow(bowling_summary)
  if (nrow(ballsBowledTest) > 0 ) {
    bowlers <- unique(ballsBowledTest$bowler)
    matchTest <- match %>%
      filter(bowler %in% bowlers & bowler2 != "") %>%
      rowwise() %>%
      mutate(ambiguous = !grepl(paste0("[A-Z] ", bowler), bowler2)) %>%
      ungroup() %>%
      mutate(#If it's still ambiguous we know that there is either a change of bowler
        #when we are in consecutive overs, or no change for non-consecutive overs?
        bowler4 = case_when(
          ambiguous == TRUE & ball - lag(ball) == 1 ~ lag(bowler2, 2),
          ambiguous == TRUE ~ lag(bowler2, 1),
          TRUE ~ bowler2)
      ) %>%
      group_by(innings, ball, bowler4) %>%
      summarise()

    match <- match %>%
      left_join(matchTest, by=c("innings", "ball")) %>%
      mutate(bowler = ifelse(!is.na(bowler4), bowler4, bowler))
  } else if (!ballsBowledTest2) {
    bowling_summary2 <- merge(ballsBowled, bowling_summary, by=c("Innings", "Runs", "Balls", "Dots", "4s", "6s"))
    
    bowlers <- unique(match$bowler) %>%
      data.frame(bowler = .) %>%
      anti_join(bowling_summary2, by="bowler")
    
    matchTest <- match %>%
      filter(bowler %in% bowlers & bowler2 != "") %>%
      rowwise() %>%
      mutate(ambiguous = !grepl(paste0("[A-Z] ", bowler), bowler2)) %>%
      ungroup() %>%
      mutate(#If it's still ambiguous we know that there is either a change of bowler
        #when we are in consecutive overs, or no change for non-consecutive overs?
        bowler4 = case_when(
          ambiguous == TRUE & ball - lag(ball) == 1 ~ lag(bowler2, 2),
          ambiguous == TRUE ~ lag(bowler2, 1),
          TRUE ~ bowler2)
      ) %>%
      group_by(innings, ball, bowler4) %>%
      summarise()
    
    match <- match %>%
      left_join(matchTest, by=c("innings", "ball")) %>%
      mutate(bowler = ifelse(!is.na(bowler4), bowler4, bowler))
    
  }
  
  ballsBowled <- match %>%
    group_by(innings, bowler) %>%
    summarise(Innings = first(innings),
              Runs = sum(bowling_runs),
              Balls = sum(legalBall),
              # Overs = floor(Balls/6) + (Balls %% 6 / 10),
              Dots = sum(bowling_runs == 0),
              `4s` = sum(batting_runs == 4 & shortDesc %like% "FOUR"), #Catch in case the four wasn't a boundary
              `6s` = sum(batting_runs == 6 & shortDesc %like% "SIX"))
  
  batting_summary <- batting_summary %>%
    group_by(Innings) %>%
    mutate(battingOrder = seq(1:n()))
  
  bowling_summary <- bowling_summary %>%
    group_by(Innings) %>%
    mutate(bowlingOrder = seq(1:n()))
  
  batting_summary2 <- merge(ballsFaced, batting_summary, by=c("Innings", "Runs", "Balls", "4s", "6s")) %>%
    mutate(Batsman = gsub("\\(c\\)|†", "", Batsman),
           Batsman = trimws(gsub("  ", " ", Batsman)),
           Batsman = ifelse(grepl("[A-z]", substr(Batsman, nchar(Batsman), nchar(Batsman))), Batsman, substr(Batsman, 1, nchar(Batsman) - 1))) %>%
    select(innings, batsman, Batsman, battingOrder, wicketDesc) %>%
    group_by(innings, batsman) %>%
    mutate(rows = n()) %>%
    rowwise() %>%
    filter(grepl(batsman, Batsman)) %>%
    select(-wicketDesc)
  
  if (nrow(batting_summary2) != nrow(ballsFaced)) {
    batting_summary2 <- merge(ballsFaced, batting_summary, by=c("Innings", "Runs", "Balls", "4s", "6s")) %>%
      mutate(Batsman = gsub("\\(c\\)|†", "", Batsman),
             Batsman = trimws(gsub("  ", " ", Batsman)),
             Batsman = ifelse(grepl("[A-z]", substr(Batsman, nchar(Batsman), nchar(Batsman))), Batsman, substr(Batsman, 1, nchar(Batsman) - 1))) %>%
      select(innings, batsman, Batsman, battingOrder, wicketDesc) %>%
      group_by(innings, batsman) %>%
      mutate(rows = n()) %>%
      rowwise() %>%
      filter(rows == 1 | grepl(batsman, Batsman) | grepl(Batsman, wicketDesc)) %>%
      select(-wicketDesc)
  }
  
  bowling_summary2 <- merge(ballsBowled, bowling_summary, by=c("Innings", "Runs", "Balls", "Dots", "4s", "6s")) %>%
    mutate(Bowler = gsub("\\(c\\)|†", "", Bowler),
           Bowler = trimws(gsub("  ", " ", Bowler)),
           Bowler = ifelse(grepl("[A-z]", substr(Bowler, nchar(Bowler), nchar(Bowler))), Bowler, substr(Bowler, 1, nchar(Bowler) - 1))) %>%
    select(innings, bowler, Bowler, bowlingOrder) %>%
    group_by(innings, bowler) %>%
    mutate(rows = n()) %>%
    rowwise() %>%
    mutate(bowler2 = gsub("^([A-Z] )", "", bowler)) %>%
    filter(rows == 1 | grepl(bowler, Bowler) | grepl(bowler2, Bowler))
  
  w2 <- merge(w, batting_summary2, by="innings")  %>%
    rowwise() %>%
    mutate(d = grepl(Batsman, wicketDesc)) %>%
    filter(d) %>%
    group_by(innings, batsman.y) %>%
    slice(1) %>%
    arrange(innings, row) %>%
    group_by(innings) %>%
    mutate(wicketOrder = seq(1:n())) %>%
    select(innings, end_row=row, dismissed = batsman.y, wicketOrder)
  
  partnerships <- match %>%
    group_by(innings, batsman) %>%
    summarise(start_row = min(row),
              first_faced = min(row),
              last_faced = max(row)) %>%
    left_join(w2, by=c("innings", "batsman"="dismissed")) %>%
    left_join(batting_summary2, by=c("innings", "batsman")) %>%
    group_by(innings) %>%
    mutate(end_row = ifelse(is.na(end_row), max(last_faced), end_row)) %>%
    arrange(innings, battingOrder) %>%
    mutate(start_row = ifelse(is.na(lag(start_row, 2)), 1, cumulativeRank(end_row, 2) + 1),
           prev_end_row = lag(end_row), 
           prev_end_row2 = lag(end_row, 2),
           cum_end_row_max = lag(cummax(end_row))) %>%
    group_by(innings) %>%
    mutate(prev_start_row = lag(start_row),
           start_row = ifelse(start_row == prev_start_row & start_row != 1, prev_start_row + 1, start_row)) %>%
    select(innings, batsman, start_row, first_faced, last_faced, end_row, Batsman, battingOrder) %>%
    arrange(innings, desc(end_row)) %>%
    mutate(end_row = ifelse(seq(1:n()) == 2, lag(end_row), end_row)) %>%
    arrange(innings, battingOrder)
  
  partnerships2 <- partnerships %>%
    left_join(partnerships, by=c("innings")) %>%
    filter(batsman.x != batsman.y & end_row.x >= start_row.y & start_row.x <= start_row.y) %>%
    group_by(innings, batsman.x, batsman.y) %>%
    mutate(start_row = max(start_row.x, start_row.y, na.rm=T),
           end_row = min(end_row.x, end_row.y, na.rm=T)) %>%
    select(innings, batsman1 = batsman.x, batsman2 = batsman.y, start_row, end_row) %>%
    group_by(innings) %>%
    arrange(innings, start_row) %>%
    filter(seq(1:n()) != 2)
  
  match <- match %>%
    left_join(partnerships2, by=c("innings")) %>%
    filter(row >= start_row & row <= end_row | innings > 2) %>%
    mutate(non_striker = ifelse(batsman == batsman1, batsman2, batsman1)) %>%
    left_join(w2, by=c("innings", "row"="end_row")) %>%
    left_join(batting_summary2, by=c("innings", "batsman")) %>%
    left_join(bowling_summary2, by=c("innings", "bowler"))
  
  
  match <- match %>%
    mutate(matchID = matchID,
           match = index,
           set = floor((ball - 1)/5) + 1) %>%
    select(matchID, 
           match,
           innings,
           team,
           set, 
           ball, 
           legalBall,
           event, 
           shortRuns,
           shortDesc,
           fullDesc,
           bowler,
           Bowler,
           batsman,
           Batsman,
           non_striker,
           runs,
           batting_runs,
           bowling_runs,
           extras,
           wickets,
           # wicket,
           wicketDesc,
           wicketOrder,
           dismissed,
           bowling_wickets,
           battingOrder, 
           bowlingOrder)
}



scrapeTheHundred <- function(x, index, superOver=FALSE, cookiesDeclined=FALSE, notNowClicked=FALSE, overwrite=FALSE) {
  
  
  #matchID for the game in question
  # url <- matchInfo$url[19]
  url <- x
  matchID <- gsub("-", "", substr(url, nchar(url) - 21, nchar(url)-15))
  
  #output file(s)
  file1 <- paste0(outLoc, "/ball-by-ball/", matchID, ".fst")
  file2 <- paste0(outLoc, "/batting-summaries/", matchID, ".fst")
  file3 <- paste0(outLoc, "/bowling-summaries/", matchID, ".fst")
  
  #Create output location
  dir.create(paste0(outLoc, "/ball-by-ball/"), showWarnings = F)    #Doesn't crash if the directory already exists
  dir.create(paste0(outLoc, "/batting-summaries/"), showWarnings = F)    #Doesn't crash if the directory already exists
  dir.create(paste0(outLoc, "/bowling-summaries/"), showWarnings = F)    #Doesn't crash if the directory already exists
  
  #If we need to overwrite the data or scrape if it doesn't exist already
  if(overwrite | !file.exists(file1)) {
    print(paste0("Index: ", index))
      
    #Read the raw html data
    raw <- try(xml2::read_html(url), silent = TRUE)
    if ("try-error" %in% class(raw)) 
      stop("Error in URL")
    
    #Teams
    # teams <- raw %>%
    #   rvest::html_nodes("a.name-link") %>%
    #   rvest::html_text()
    
    teams <- raw %>%
      rvest::html_nodes("h5.header-title") %>%
      rvest::html_text() %>%
      as.data.frame() %>%
      dplyr::slice(1:2) %>%
      dplyr::mutate(team = gsub("([A-z \\-\\'\\(\\)]*) INNINGS .*", "\\1", `.`))
    
    teams <- c(teams$team[1], teams$team[2])
      
    
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
    #This assumes that both innings had at least one ball bowled.
    batting_summary <- try(setDT(rbind(setDT(tables[[1]][, c(1:4, 6:8)])[, innings:=1], 
                                       setDT(tables[[3]][, c(1:4, 6:8)])[, innings:=2])),
                           silent=TRUE)
    
    
    bowling_summary <- try(setDT(rbind(setDT(tables[[2]][, c(1:8)])[, innings:=1], 
                                       setDT(tables[[4]][, c(1:8)])[, innings:=2])),
                           silent=TRUE)
    
    matchInfo <- try(setDT(tables[[5]]), silent=TRUE) 
    
    if (class(batting_summary) != "try-error") {
      #Set the names so that we don't have any duplicates
      setnames(batting_summary, names(batting_summary), c("Batsman", "Dismissal", "Runs", "Balls", "4s", "6s", "SR", "Innings"))
      setnames(bowling_summary, names(bowling_summary), c("Bowler", "Balls", "Dots", "Runs", "Wickets", "RPB", "4s", "6s", "Innings"))
      setnames(matchInfo, names(matchInfo), c("Detail", "Value"))
      
      #Retain batsman rows and Extras/Total lines
      #Removes others
      batting_summary <- batting_summary[Batsman != "" & !Batsman %like% "Fall of" & !Batsman %like% "Did not bat"]
      
      #Filter out empty rows
      bowling_summary <- bowling_summary[Bowler != ""]
      
      #This is a flag for which team is batting first
      batFirst <- matchInfo %>%
        slice(2) %>%
        mutate(team = case_when(
          grepl(gsub("\\(", "\\\\(", gsub("\\)", "\\\\)", teams[1])), Value) &  grepl("field", Value) ~ 0,
          grepl(gsub("\\(", "\\\\(", gsub("\\)", "\\\\)", teams[1])), Value) & !grepl("field", Value) ~ 1,
          grepl(gsub("\\(", "\\\\(", gsub("\\)", "\\\\)", teams[2])), Value) &  grepl("field", Value) ~ 1,
          grepl(gsub("\\(", "\\\\(", gsub("\\)", "\\\\)", teams[2])), Value) & !grepl("field", Value) ~ 0,
          TRUE ~ 2)) 
      
      #Switch over to the commentary url
      url <- gsub("full-scorecard", "ball-by-ball-commentary", url)
      
      #Navigate to the commentary page and 'accepts' cookies
      remDr$navigate(url) #Entering our URL gets the browser to navigate to the page
      Sys.sleep(5)
      
      #Deal with cookies
      while (cookiesDeclined != TRUE & index == 1) {
        cookiesDeclined <- dealWithCookies(cookiesDeclined)
      }
      
      #Deal with live update pop-up
      while (notNowClicked != TRUE & index == 1) {
        notNowClicked <- declineLiveUpdates()
      }
      
      #Scrape each innings in turn and bind into one match dataframe
      #Check if there is a super over
      if (superOver == TRUE) {
        innList <- c(1, 2, 3)
      } else {
        innList <- c(1, 2)
      }
      match <- rbindlist(mapply(scrapeInnings, innList))
    
      #Parse extra details
      match <- parseData(match, matchID, index, teams, batFirst, batting_summary, bowling_summary)
      
      write.fst(match, file1)
      write.fst(batting_summary, file2)
      write.fst(bowling_summary, file3)
    } else {
      match <- NA
    }
  } else {
    match <- read.fst(file1, as.data.table = TRUE)
    batting_summary <- read.fst(file2, as.data.table = TRUE)
    bowling_summary <- read.fst(file3, as.data.table = TRUE)
  } 
  
  return(list(match, batting_summary, bowling_summary))
}
