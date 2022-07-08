#For Debug
# x <- t$href[5]

# x <- matchInfo$url[5]
# index <- 5

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

scrapeInnings <- function(inn) {
  #Locates the innings selection dropdown
  inningsDropdown <- remDr$findElement(using = 'xpath',
                                       '/html/body/div[1]/section/section/div[5]/div[1]/div/div[3]/div[1]/div[2]/div[1]/div[1]/div/div[2]/div/div/div/span')

  #Scroll to top of page - mostly so that I can check the innings selection
  remDr$executeScript(paste("scroll(0,",0,");"))

  # Sys.sleep(1)
  #Open dropdown
  inningsDropdown$clickElement()

  Sys.sleep(1)
  message(inn)
  #Find the element referring to the innings we want
  #We need to use the xpath for this as it has no other identifying tags
  inningsSelection <- remDr$findElement(using = 'xpath',
                                        paste0('/html/body/div[4]/div/div/div/div/div/ul/li[', inn, ']'))
                                        # paste0('//*[@id="main-container"]/div[1]/div/div/div[2]/div[2]/div[1]/div[1]/div/div/div/ul/li[', inn, ']'))
  
  inningsSelection$clickElement()
  
  #remDr$screenshot(display = TRUE) #This will take a screenshot and display it in the RStudio viewer

  #We need to scroll to make sure every ball is visible on the page
  firstBall <- FALSE #Have we found the first ball of the innings
  pass <- 0 #Counts our passes through the loop
  earliest <- NULL #Earliest ball found so far
  while(firstBall == FALSE) {
    #Update our previously found earliest ball
    prev <- earliest

    #Send a keypress of the end key to jump to the bottom of the page
    #This causes the page to add in more commentary of earlier balls
    body <- remDr$findElement("css", "body")
    body$sendKeysToElement(list(key = "end"))

    Sys.sleep(3)

    #Read the page source and extract the ball information
    #Filtering to ball 0.1 means we keep only the first ball if found
    ball <- xml2::read_html(remDr$getPageSource()[[1]]) %>%
      rvest::html_nodes("span.match-comment-over") %>%
      rvest::html_text() %>%
      dplyr::tibble(ball = .) %>%
      dplyr::filter(ball == "0.1")

    if(nrow(ball) > 0) {
      firstBall = TRUE
      break #Exit the loop
    }

    #I found that sometimes the page would freeze up on my and stop adding earlier balls
    #This is some logic to detect when the earliest ball found hasn't changed
    #If this is the case then we refresh the page and reselect the innings commentary

    #What is the earliest ball found
    earliest <- xml2::read_html(remDr$getPageSource()[[1]]) %>%
      rvest::html_nodes("span.ds-mb-1") %>%
      rvest::html_text() %>%
      dplyr::tibble(ball = .) %>%
      slice(nrow(.))


    #If we are on the first pass we need prev not to be NULL
    #Otherwise the if statement later fails
    if (pass == 0) {
      prev <- earliest
    }

    #On subsequent passes check if the earliest ball hasn't changed
    if (pass > 10) {
      print(pass)
      print(earliest)
      print(prev)
      if(earliest == prev) {
        message("Refreshing...")
        match <- NULL
        #If the page stops adding new deliveries then refresh and reselect the innings
        remDr$refresh()
        Sys.sleep(5)
        inningsDropdown <- remDr$findElement(using = 'xpath',
                                             '/html/body/div[1]/section/section/div[5]/div[1]/div/div[3]/div[1]/div[2]/div[1]/div[1]/div/div[2]/div/div/div/span')

        inningsDropdown$clickElement()

        Sys.sleep(2)
        inningsSelection <- remDr$findElement(using = 'xpath',
                                              paste0('/html/body/div[4]/div/div/div/div/div/ul/li[', inn, ']'))


        inningsSelection$clickElement()
      }
    }

    pass <- pass + 1
  }

  #Continue once we have all deliveries on the page

  #Read the page source and extract all ball  info
  pageSource <- xml2::read_html(remDr$getPageSource()[[1]])
  balls <- pageSource %>%
    rvest::html_nodes("span.ds-mb-1") %>%
    rvest::html_text() %>%
    base::data.frame(ball = .)

  #We can also extract the events that occurred on each ball
  #This only captures the 'main' event (i.e. wickets take priority over runs)
  events <- pageSource %>%
    rvest::html_nodes(".ds-p-1") %>%
    rvest::html_text() %>%
    base::data.frame(event = .)

  b <- paste0(balls$ball, collapse="|")
  
  #We can extract the short descrption for each delivery
  shortDesc <- pageSource %>%
    rvest::html_nodes("div.ds-py-2") %>%
    rvest::html_text() %>%
    base::data.frame(shortDesc = .) %>%
    dplyr::mutate(ball = substr(shortDesc, 1, regexpr("\\.", shortDesc)+1)) %>%
    dplyr::semi_join(balls, by="ball") %>%
    dplyr::select(-ball)

  # shortRuns <- pageSource %>%
  #   rvest::html_nodes(".comment-short-run") %>%
  #   rvest::html_text() %>%
  #   base::data.frame(shortRuns = .)

  #Finally a full text description of the outcome of the delivery
  # fullDesc <- pageSource %>%
  #   rvest::html_nodes(".match-comment-wrapper") %>%
  #   rvest::html_text() %>%
  #   base::data.frame(fullDesc = .)

  #Also extract wicket descriptions so that we can accurately determine which batsman was out
  #In run-outs we don't know if it was the striker or non-striker
  wicketDesc <- pageSource %>%
    rvest::html_nodes(".ds-mt-1") %>%
    rvest::html_text() %>%
    base::data.frame(wicketDesc = .)

  if(nrow(wicketDesc) > 0) {
    wicketDesc <- wicketDesc %>%
      mutate(wicket = seq(1:n()),
             innings = inn)

    #Our innings data is the combination of these four dataframes
    innings <- cbind(balls, events, shortDesc) %>% #, shortRuns, fullDesc) %>%
      dplyr::mutate(ball = as.numeric(ball)) %>%
      dplyr::arrange(ball) %>%
      dplyr::mutate(innings = inn,
                    w = ifelse(event == "W", 1, 0),
                    wicket = ifelse(event == "W", sum(w) - cumsum(w) + 1, 0)) %>%
      dplyr::select(-w) %>%
      dplyr::left_join(wicketDesc, by=c("innings", "wicket")) %>%
      rowwise() %>%
      dplyr::mutate(shortDesc = gsub(paste0("^", ball), "", shortDesc),
                    shortDesc = gsub(paste0("^", event), "", shortDesc))
  } else {
    #Our innings data is the combination of these four dataframes
    innings <- cbind(balls, events, shortDesc) %>% #, shortRuns, fullDesc) %>%
      dplyr::mutate(ball = as.numeric(ball)) %>%
      dplyr::arrange(ball) %>%
      dplyr::mutate(innings = inn,
                    wicket= 0,
                    wicketDesc = NA) %>%
      rowwise() %>%
      dplyr::mutate(shortDesc = gsub(paste0("^", ball), "", shortDesc),
                    shortDesc = gsub(paste0("^", event), "", shortDesc))
  }

  return(list(innings))
}

scrapeInnings2 <- function(inn) {
  url <- paste0("https://hs-consumer-api.espncricinfo.com/v1/pages/match/comments?lang=en&seriesId=1263452&matchId=1263466&inningNumber=", x, "&commentType=ALL&sortDirection=DESC")
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
           dismissalTextCommentary = ifelse(checkDismissalText == 0, "", dismissalText[["commentary"]])) %>%
    select(id, inningNumber, oversUnique, oversActual, overNumber, 
           ballNumber, totalRuns, batsmanRuns, isFour, isSix, isWicket,
           dismissalType, dismissalTextShort, dismissalTextLong, dismissalTextCommentary, 
           byes, legbyes, wides, noballs, timestamp, batsmanPlayerId, 
           bowlerPlayerId, totalInningRuns, title)
  
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
             dismissalTextCommentary = ifelse(checkDismissalText == 0, "", dismissalText[["commentary"]])) %>%
      select(id, inningNumber, oversUnique, oversActual, overNumber, 
             ballNumber, totalRuns, batsmanRuns, isFour, isSix, isWicket,
             dismissalType, dismissalTextShort, dismissalTextLong, dismissalTextCommentary, 
             byes, legbyes, wides, noballs, timestamp, batsmanPlayerId, 
             bowlerPlayerId, totalInningRuns, title)
    
    data <- rbind(data, data2)
    
    next_over <- json$nextInningOver
    
    rm(url2, json, data2)
  }
  return(data)
}

parseData <- function(match, matchID, index, teams, batFirst, batting_summary, bowling_summary) {
  match <- match %>%
    mutate(team = case_when(
      innings == 1 ~ teams[2 - batFirst$team],
      TRUE ~ teams[1 + batFirst$team])
    ) %>%
    group_by(ball, fullDesc) %>%
    mutate(
      #Inserts a new line character into the full description.  Doesn't seem to work?
      fullDesc = gsub(shortDesc, paste0(shortDesc, "\n "), fullDesc),
      #Extract the bowler's name from the short description
      bowler = gsub("([A-z \\'\\-]+) to.*", "\\1", shortDesc),
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
        event %in% c("•") ~ 0,
        event %in% c("W") ~ runsCheck, #Here we use the runsCheck column to ensure we capture all runs scored
        grepl("1", event) ~ 1,
        grepl("2", event) ~ 2,
        grepl("3", event) ~ 3,
        grepl("4", event) ~ 4,
        grepl("5", event) ~ 5,
        grepl("6", event) ~ 6,
        grepl("7", event) ~ 7,
        TRUE ~ -9),
      #How many runs should be awarded to the batsman?
      batting_runs = case_when(
        event == "W" ~ batting_runsCheck, 
        grepl("lb|w|•", event) ~ 0, #lb and wides can't have any runs for the batsman
        grepl("nb", event) ~ runs - 1, #Remove a run for a no ball
        grepl("b", event) ~ 0, #Byes can't be awarded to the batsman
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
        grepl("lb|•", event) ~ 0, #Leg byes aren't awarded against the bowler
        grepl("nb", event) ~ runs,
        grepl("n\\-b|n\\-l", event) ~ 1, #No-ball + (leg) bye counts as 1 against the bowler
        grepl("b", event) ~ 0, #Byes don't count against the bowler
        grepl("1", event) ~ 1,
        grepl("2", event) ~ 2,
        grepl("3", event) ~ 3,
        grepl("4", event) ~ 4,
        grepl("5", event) ~ 5,
        grepl("6", event) ~ 6,
        grepl("7", event) ~ 7,
        TRUE ~ -9),
      #Did a wicket fall on this ball?
      wickets = case_when(
        grepl("W", event) ~ 1,
        TRUE ~ 0),
      #Does the wicket count for the bowler? (TODO: Other non bowler dismissals)
      bowling_wickets = case_when(
        grepl("W", event) & !grepl("run out", wicketDesc) ~ 1,
        TRUE ~ 0),
      extras = case_when(
        grepl("nb", event) ~ 1, #No balls count as 1
        # grepl("b|lb|n-b|n-l|w", event) ~ as.integer(grep("[1-6]", event)), #byes, leg byes, no balls + (leg) byes & wides
        grepl("1[b|lb|n\\-b|n\\-l|w]", event) ~ 1,
        grepl("2[b|lb|n\\-b|n\\-l|w]", event) ~ 2,
        grepl("3[b|lb|n\\-b|n\\-l|w]", event) ~ 3,
        grepl("4[b|lb|n\\-b|n\\-l|w]", event) ~ 4,
        grepl("5[b|lb|n\\-b|n\\-l|w]", event) ~ 5,
        grepl("6[b|lb|n\\-b|n\\-l|w]", event) ~ 6,
        grepl("•|[1-6]|W", event) ~ 0, #No extras
        TRUE ~ -9)
    ) %>%
    arrange(innings, ball, legalBall) %>%
    group_by(innings) %>%
    mutate(row = seq(1:n())) 
  
  
  
  #Wickets to fall in the match
  w <- match %>%
    filter(event == "W") %>%
    select(innings, row, batsman, wickets, wicketDesc)
  
  #We can use the summary to check what the wickets name is!
  ballsFaced <- match %>%
    group_by(innings, batsman) %>%
    summarise(Innings = first(innings),
              Runs = sum(batting_runs),
              Balls = sum(ballFaced),
              `4s` = sum(batting_runs==4 & shortDesc %like% "FOUR"),
              `6s` = sum(batting_runs==6 & shortDesc %like% "SIX"))
  
  ballsBowled <- match %>%
    group_by(innings, bowler) %>%
    summarise(Innings = first(innings),
              Runs = sum(bowling_runs),
              Balls = sum(legalBall),
              Overs = floor(Balls/6) + (Balls %% 6 / 10),
              `0s` = sum(bowling_runs == 0),
              `4s` = sum(batting_runs == 4 & shortDesc %like% "FOUR"),
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
    select(innings, batsman, Batsman, battingOrder) %>%
    group_by(innings, batsman) %>%
    mutate(rows = n()) %>%
    rowwise() %>%
    filter(rows == 1 | grepl(batsman, Batsman))
  
  bowling_summary2 <- merge(ballsBowled, bowling_summary, by=c("Innings", "Runs", "Overs", "0s", "4s", "6s")) %>%
    mutate(Bowler = gsub("\\(c\\)|†", "", Bowler),
           Bowler = trimws(gsub("  ", " ", Bowler)),
           Bowler = ifelse(grepl("[A-z]", substr(Bowler, nchar(Bowler), nchar(Bowler))), Bowler, substr(Bowler, 1, nchar(Bowler) - 1))) %>%
    select(innings, bowler, Bowler, bowlingOrder) %>%
    group_by(innings, bowler) %>%
    mutate(rows = n()) %>%
    rowwise() %>%
    filter(rows == 1 | grepl(bowler, Bowler))
  
  
  
  w2 <- merge(w, batting_summary2, by="innings")  %>%
    rowwise() %>%
    mutate(d = grepl(Batsman, wicketDesc)) %>%
    filter(d) %>%
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
    filter(row >= start_row & row <= end_row) %>%
    mutate(non_striker = ifelse(batsman == batsman1, batsman2, batsman1)) %>%
    left_join(w2, by=c("innings", "row"="end_row")) %>%
    left_join(batting_summary2, by=c("innings", "batsman")) %>%
    left_join(bowling_summary2, by=c("innings", "bowler"))
  
  
  match <- match %>%
    mutate(matchID = matchID,
           match = index,
           over = floor(ball) + 1) %>%
    select(matchID, 
           match,
           innings,
           team,
           over, 
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

scrapeTheAshes <- function(x, index, cookiesDeclined=FALSE, notNowClicked=FALSE, overwrite=FALSE) {
  
  #matchID for the game in question
  # url <- paste0("https://www.espncricinfo.com", x)
  url <- x
  matchID <- substr(url, nchar(url) - 21, nchar(url)-15)
  
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
      setnames(batting_summary, names(batting_summary), c("Batsman", "Dismissal", "Runs", "Balls", "Minutes", "4s", "6s", "SR", "Innings"))
      setnames(bowling_summary, names(bowling_summary), c("Bowler", "Overs", "Maidens", "Runs", "Wickets", "Economy", "0s", "4s", "6s", "WDs", "NBs", "Innings"))
      
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
      match <- rbindlist(mapply(scrapeInnings, innlist))
      
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




































































































































































































































































































































