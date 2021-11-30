#--------------------------------------------------------------#
####     Utilities for scraping data from ESPN cricinfo     ####
####                 James Brook 2021-10-27                 ####
#--------------------------------------------------------------#

#--------------------------------------------------------------#
####     Required Packages                                  ####
#--------------------------------------------------------------#

library(data.table)
library(dplyr)
library(doParallel)
library(foreach)
library(ggplot2)
library(kwutilr)
library(pbapply)
library(RSelenium)
library(rvest)
library(stringr)

#--------------------------------------------------------------#
####     Useful Functions                                   ####
#--------------------------------------------------------------#

#Returns the appropriate element of teamColours based on the name passed
cricketColours <- function(...) {
  cols <- c(...)
  if (is.null(cols))
    return (teamColours) #Return the entire colour set if nothing passed
  if (is.na(teamColours[cols]))
    return ("#C6C6C6")
  
  return (teamColours[cols])
  
}

#Sets the colour of a series in ggplot
#Options to reverse the order or match the series name
teamColour <- function(palette = "T20 World Cup 2021", reverse = FALSE, matchNames = FALSE, ...) {
  if (matchNames) {
    scale_color_manual(values = teamPalettes[[palette]])
  } else {
    if (reverse) {
      scale_color_manual(values = rev(unname(teamPalettes[[palette]])))
    } else {
      scale_color_manual(values = unname(teamPalettes[[palette]]))
    }
  }
}

#Sets the fill of a series in ggplot
#Options to reverse the order or match the series name
teamFill <- function(palette = "T20 World Cup 2021", reverse = FALSE, matchNames = FALSE, ...) {
  if (matchNames) {
    scale_fill_manual(values = teamPalettes[[palette]])
  } else {
    if (reverse) {
      scale_fill_manual(values = rev(unname(teamPalettes[[palette]])))
    } else {
      scale_fill_manual(values = unname(teamPalettes[[palette]]))
    }
  }
}

#Retrieves information about the specified match
#x is the end of a url to the summary page
getMatchInfo <- function(x) {
  url <- paste0("https://www.espncricinfo.com", x)
  
  #Read the raw html data
  raw <- try(xml2::read_html(url), silent = TRUE)
  if ("try-error" %in% class(raw)) 
    stop("Error in URL")
  
  #Match description
  desc <- raw %>%
    rvest::html_nodes("div.description") %>%
    rvest::html_text() %>%
    utils::head(1) %>%
    base::data.frame(desc = .) %>%
    dplyr::mutate(url     = url,
                  matchID = gsub("-", "", substr(url, nchar(url) - 21, nchar(url)-15)),
                  # matchID = substr(gsub(id, "", url), gregexpr("-\\d", gsub(id, "", url))[[1]] + 1, gregexpr("\\d/", gsub(id, "", url))[[1]]),
                  round   = strsplit(desc, ", ")[[1]][1],
                  # groupID = strsplit(desc, ", ")[[1]][2], Only difference from T20 World Cup 2021
                  ground  = strsplit(desc, ", ")[[1]][2],
                  date    = as.Date(strsplit(desc, ", ")[[1]][3], format="%b %d %Y"),
                  event   = strsplit(desc, ", ")[[1]][4]) %>%
    dplyr::select(-desc)
  
  #Teams
  teams <- raw %>%
    rvest::html_nodes("a.name-link") %>%
    rvest::html_text() %>%
    base::data.frame(teams=.) %>%
    dplyr::mutate(homeTeam = paste0(strsplit(teams, " ")[[1]], collapse=" "),
                  awayTeam = paste0(strsplit(teams, " ")[[2]], collapse=" ")) %>%
    dplyr::select(homeTeam, awayTeam) %>%
    dplyr::slice(1)
  
  #Status (determines if a result happened)
  status <- raw %>%
    rvest::html_nodes("div.status") %>% 
    rvest::html_text() %>%
    base::data.frame(result = .)%>%
    slice(nrow(.)) %>%
    as.character(.)
  
  #Further details
  tables <- rvest::html_table(raw, fill=TRUE) 
  
  #If there is a result then the more details are stored in table 5
  if (status == "result") {
    n <- 5 
  } else { #Otherwise it's up to the 4th table depending on how many there are
    n <- min(4, length(tables) - 1)
  }
  
  #Result
  result <- raw %>%
    rvest::html_nodes("div.status-text") %>%
    rvest::html_text() %>%
    base::data.frame(result = .) %>%
    slice(nrow(.)) %>%
    as.character(.)
  
  #Read in more details such as the umpires etc.
  more <- try(setDT(tables[[n]]), silent=TRUE) 
  setnames(more, names(more), c("Detail", "Value"))
  more <- more[!Detail %like% "debut"]

  #The umpires often appear as Alex WharfNickCook
  #Use the fact that names are often links to split the umpires text into the two respective umpires
  links <- raw %>%
    rvest::html_nodes("a.d-block") %>%
    rvest::html_text() %>%
    base::data.frame(Value = .)

  #If we can find the link text in any of the more details then keep this to split apart later
  links <- links %>%
    rowwise() %>%
    mutate(Detail = ifelse(grepl(Value, paste0(more$Value, collapse = " ")), more$Detail[grep(Value, paste0(more$Value))], "")) %>%
    filter(Detail != "")

  #Remove any info that we can split using the links
  more <- more %>%
    anti_join(links, by="Detail") %>%
    rbind(links) %>%
    group_by(Detail) %>%
    mutate(rows = n(),
           Detail = ifelse(rows > 1, paste0(Detail, " ", seq(1:n())), Detail)) %>%
    select(-rows) %>%
    ungroup()

  #Some matches are part of multiple series
  #If it isn't then we need to relabel as Series 1 to ensure compatability with these games
  if (!grepl("Series 1", paste0(more$Detail, collapse = " "))) {
    more <- more %>%
      mutate(Detail = ifelse(Detail == "Series", "Series 1", Detail))

    more <- rbind(more, data.frame(Detail = "Series 2", Value = ""))
  }

  #Amend detail as they are transposed to column names
  #Shifting them to camelCase
  more <- more %>%
    mutate(Detail = str_to_title(Detail),
           Detail = gsub(" ", "", paste0(str_to_lower(substr(Detail, 1, 1)), substr(Detail, 2, nchar(Detail)))))

  
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
        dplyr::anti_join(matchInfo, by="matchID")
      
      matchInfo <- rbindlist(list(base, matchInfo), use.names=TRUE, fill=TRUE) %>%
        dplyr::arrange(date, matchID)
      
      fst::write.fst(matchInfo, file)
    } else {
      matchInfo <- matchInfo %>%
        dplyr::anti_join(base, by="matchID") 
      
      matchInfo <- rbindlist(list(base, matchInfo), use.names=TRUE, fill=TRUE) %>%
        dplyr::arrange(date, matchID)
      
      fst::write.fst(matchInfo, file)
    }
    
  } else {
    fst::write.fst(matchInfo, file)
  }
}

#Declines cookies when the popup asks for it
#Otherwise the popup intefers
dealWithCookies <- function(cookiesDeclined = FALSE) {
  #Deal with cookies
  
  #Locate the cookies button
  cookieButton <- try(remDr$findElement(using = 'xpath', '//*[@id="onetrust-close-btn-container"]/button'),
                      silent = TRUE)
  
  if(class(cookieButton) != "try-error") {
    cookieButton$clickElement()
    cookiesDeclined <- TRUE
  }
  
  Sys.sleep(2)
  
  return(cookiesDeclined)
}

#Declines live updates when the popup asks for it
#Otherwise the popup intefers
declineLiveUpdates <- function(notNowClicked = FALSE) {
  # CricInfo now has an annoying popup asking if we want live updates. 
  # This is a flag of if we have dismissed it or not
  # Wait until the button appears to dismiss it and then click it
  tryCatch(expr = {
    notNowUpdates <- remDr$findElement(using = 'xpath', '//*[@id="wzrk-cancel"]')
    if(class(notNowUpdates) != "try-error") {
      notNowUpdates$clickElement()
      notNowClicked <- TRUE
    }
    Sys.sleep(2)
  },
  error = function(e) {
    message("Waiting for live update popup...")
    Sys.sleep(2)
  })
  
  return(notNowClicked)
}

#Extracts information for each ball of an innings 
#Needs to scroll to the end of the page repeatedly until all balls are visible
scrapeInnings <- function(inn, sleep=2) {
  #Print a message to specify which innings is being scraped
  message(paste0("Scraping innings: ", inn))
  
  #Locates the innings selection dropdown
  inningsDropdown <- remDr$findElement(using = 'class',
                                       'comment-inning-dropdown')
  
  #Scroll to top of page - mostly so that I can check the innings selection
  remDr$executeScript(paste("scroll(0,",0,");"))
  
  #Open dropdown
  inningsDropdown$clickElement()
  
  #Find the element referring to the innings we want
  #We need to use the xpath for this as it has no other identifying tags
  Sys.sleep(sleep) #Wait a little bit to ensure the menu is open
  inningsSelection <- remDr$findElement(using = 'xpath', 
                                        paste0('//*[@id="main-container"]/div[1]/div/div/div[2]/div[2]/div[1]/div[1]/div/div/div/ul/li[', inn, ']'))
  
  #Selects the appropriate innings 
  inningsSelection$clickElement()
  
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
    
    Sys.sleep(sleep)
    
    #Read the page source and extract the ball information
    #Filtering to ball 0.1 means we keep only the first ball if found
    ball <- xml2::read_html(remDr$getPageSource()[[1]]) %>%
      rvest::html_nodes("span.match-comment-over") %>%
      rvest::html_text() %>%
      dplyr::tibble(ball = .) %>%
      dplyr::filter(ball == "0.1" | ball == "1")
    
    if(nrow(ball) > 0) {
      firstBall = TRUE
      break #Exit the loop
    }
    
    #I found that sometimes the page would freeze up on me and stop adding earlier balls
    #This is some logic to detect when the earliest ball found hasn't changed
    #If this is the case then we refresh the page and re-select the innings commentary
    
    #What is the earliest ball found
    earliest <- xml2::read_html(remDr$getPageSource()[[1]]) %>%
      rvest::html_nodes("span.match-comment-over") %>%
      rvest::html_text() %>%
      dplyr::tibble(ball = .) %>%
      slice(nrow(.))
    
    #If we are on the first pass we need prev not to be NULL
    #Otherwise the if statement later fails
    if (pass == 0) {
      prev <- earliest
    }
    
    #On subsequent passes check if the earliest ball hasn't changed
    if (pass > 10 & earliest == prev) {
      message("Refreshing...")
      
      #Remove any already scraped data
      match <- NULL
      #If the page stops adding new deliveries then refresh and re-select the innings
      remDr$refresh()
      Sys.sleep(5)
      inningsDropdown <- remDr$findElement(using = 'xpath',
                                           '//*[@id="main-container"]/div[1]/div/div/div[2]/div[2]/div[1]/div[1]/div')
      inningsDropdown$clickElement()
      
      Sys.sleep(1)
      inningsSelection <- remDr$findElement(using = 'xpath', 
                                            paste0('//*[@id="main-container"]/div[1]/div/div/div[2]/div[2]/div[1]/div[1]/div/div/div/ul/li[', inn, ']'))
      inningsSelection$clickElement()
      
      pass <- 0
    }
    
    pass <- pass + 1
  }
  
  #Continue once we have all deliveries on the page
  
  #Read the page source and extract all ball info
  pageSource <- xml2::read_html(remDr$getPageSource()[[1]])
  balls <- pageSource %>%
    rvest::html_nodes("span.match-comment-over") %>%
    rvest::html_text() %>%
     base::data.frame(ball = .) 
  
  #We can also extract the events that occurred on each ball
  #This only captures the 'main' event (i.e. wickets take priority over runs)
  events <- pageSource %>%
    rvest::html_nodes(".match-comment-run-container") %>%
    rvest::html_text() %>%
     base::data.frame(event = .) %>%
    dplyr::mutate(event = ifelse(event == "•", ".", event)) #Fix the special character
  
  #We can extract the short description for each delivery
  shortDesc <- pageSource %>%
    rvest::html_nodes(".match-comment-short-text") %>%
    rvest::html_text() %>%
     base::data.frame(shortDesc = .) 
  
  #This extracts just the text version of the runs scored
  shortRuns <- pageSource %>%
    rvest::html_nodes(".comment-short-run") %>%
    rvest::html_text() %>%
     base::data.frame(shortRuns = .)
  
  #Finally a full text description of the outcome of the delivery
  fullDesc <- pageSource %>%
    rvest::html_nodes(".match-comment-wrapper") %>%
    rvest::html_text() %>%
     base::data.frame(fullDesc = .)
  
  #Test
  #Grab the whole text from the match-comment div to see if it can help when
  #two bowlers have the same name (*cough*ASHWIN*cough*)
  all <- pageSource %>%
    rvest::html_nodes(".match-comment") %>%
    rvest::html_text() %>%
     base::data.frame(all = .)
  
  #This grabs the bowler info from the summary
  bowlerInfo <- pageSource %>%
    rvest::html_nodes(".match-comment-over-end") %>%
    rvest::html_text() %>%
     base::data.frame(bowlerInfo = .) %>%
    dplyr::mutate(balls = as.numeric(gsub("[A-z \\-\\']* (\\d*) balls.*", "\\1", bowlerInfo))) %>%
    dplyr::arrange(balls) %>%
    dplyr::mutate(diff = balls - lag(balls)) %>%
    dplyr::arrange(desc(balls))
  
  bowlerInfo2 <- pageSource %>% 
    rvest::html_nodes(".comment-over-end-player") %>% 
    rvest::html_text() %>% 
     base::data.frame(bowlerInfo2 = .) %>% filter(grepl("\\d/\\d", bowlerInfo2))
  
  bowlerInfo <- cbind(bowlerInfo, bowlerInfo2) %>% 
    dplyr::mutate(bowler2 = gsub("([A-z \\-\\']*) .*", "\\1", bowlerInfo2),
           diff = ifelse(is.na(diff), balls, diff)) %>% 
    dplyr::rowwise() %>% 
    dplyr::slice(rep(1:n(), each = diff)) %>%
    dplyr::ungroup() %>%
    dplyr::select(bowler2) %>%
    dplyr::mutate(ball = n() - seq(1:n()) + 1)
    
  
  #Also extract wicket descriptions so that we can accurately determine which batsman was out
  #In run-outs we don't know if it was the striker or non-striker
  wicketDesc <- pageSource %>%
    rvest::html_nodes(".match-comment-wicket-no-icon") %>%
    rvest::html_text() %>%
     base::data.frame(wicketDesc = .) 
  
  if(nrow(wicketDesc) > 0) { 
    wicketDesc <- wicketDesc %>%
      dplyr::mutate(wicket = seq(1:n()),
                    innings = inn)
    
    #Our innings data is the combination of these seven dataframes
    innings <- cbind(balls, events, shortDesc, shortRuns, fullDesc, all) %>%
      dplyr::mutate(ball = as.numeric(ball)) %>%
      dplyr::arrange(ball) %>%
      dplyr::left_join(bowlerInfo, by="ball") %>%
      rowwise() %>%
      dplyr::mutate(innings = inn,
                    w = ifelse(event == "W" | grepl("OUT", shortDesc), 1, 0)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(wicket = ifelse(event == "W" | grepl("OUT", shortDesc), sum(w) - cumsum(w) + 1, 0)) %>%
      dplyr::select(-w) %>%
      dplyr::left_join(wicketDesc, by=c("innings", "wicket"))
  } else {
    #Our innings data is the combination of these seven dataframes
    innings <- cbind(balls, events, shortDesc, shortRuns, fullDesc, all) %>%
      dplyr::mutate(ball = as.numeric(ball)) %>%
      dplyr::arrange(ball) %>%
      dplyr::left_join(bowlerInfo, by="ball") %>%
      dplyr::mutate(innings = inn,
                    wicket= 0,
                    wicketDesc = NA) 
  }
  
  #Currently T20 specific, splitting a super over into two clear innings
  if (inn == 3) {
    innings <- innings %>%
      dplyr::group_by(ball) %>%
      dplyr::mutate(innings = innings + (2 - seq(1:n()))) %>%
      dplyr::arrange(innings, ball)
  }
  
  return(list(innings))
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

#Extracts useful information from the scraped data
parseData <- function(match, matchID, index, teams, batFirst, batting_summary, bowling_summary) {
  match <- match %>%
    dplyr::mutate(team = case_when(
      innings %in% c(1, 4) ~ teams[2 - batFirst$team],
      TRUE ~ teams[1 + batFirst$team]),
    ) %>%
    dplyr::group_by(ball, fullDesc) %>%
    dplyr::mutate(
      #Inserts a new line character into the full description.  Doesn't seem to work?
      fullDesc = gsub(shortDesc, paste0(shortDesc, "\n "), fullDesc),
      #Extract the bowler's name from the short description
      bowler = gsub("([A-z \\'\\-]+) to.*", "\\1", shortDesc),
      #Also try and extract a second copy of this name with initial if it exists
      bowler2 = case_when(
        ball == "0.1" ~ gsub(paste0(".*?([A-Z] ", bowler, ") .*"), "\\1", all),
        grepl("\\W1", ball) ~ gsub(paste0(".*([A-Z] ", bowler, ") .*"), "\\1", all),
        TRUE ~ ""),
      #Extract the batsman's name from the short description
      batsman = gsub("([A-z \\'\\-]+),.*", "\\1", sub("[A-z \\'\\-]+ to ([A-z ,]+)", "\\1", shortDesc)),
      #Ensure the ball is numeric
      ball = as.numeric(ball),
      #If there is a wicket, this overrides the event column
      #This checks the description to see if any runs were scored (run outs etc)
      runsCheck = as.integer(case_when(
        grepl("no run", shortRuns) ~ 0,
        grepl("[1-6]", shortRuns) ~ as.double(gsub("^(\\d) [A-z]+", "\\1", shortRuns)),
        grepl("FOUR", shortRuns) ~ 4,
        grepl("SIX", shortRuns) ~ 6,
        TRUE ~ 0)),
      #As above, but for runs to be attributed to the batsman. 
      #Note that extras don't count for the batsman
      batting_runsCheck = as.integer(case_when(
        grepl("no run|leg bye|bye|wide", shortDesc) ~ 0,
        grepl("no ball", shortDesc) ~ as.double(gsub("^(\\d) [A-z]+", "\\1", shortRuns)) - 1,
        grepl("[1-6]", shortRuns) ~ as.double(gsub("^(\\d) [A-z]+", "\\1", shortRuns)),
        grepl("FOUR", shortRuns) ~ 4,
        grepl("SIX", shortRuns) ~ 6,
        TRUE ~ 0)),
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
      runs = as.integer(case_when(
        event %in% c(".") ~ 0,
        event %in% c("W") ~ runsCheck, #Here we use the runsCheck column to ensure we capture all runs scored
        grepl("[1-8]", event) ~ as.double(gsub("^(\\d)[ A-z|lb]+", "\\1", event)),
        TRUE ~ -9)),
      #How many runs should be awarded to the batsman?
      batting_runs = case_when(
        event == "W" ~ batting_runsCheck, 
        grepl("lb|w|\\.|n\\-l|n\\-b", event) ~ 0, #lb and wides can't have any runs for the batsman
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
        grepl("lb|\\.", event) ~ 0, #Leg byes aren't awarded against the bowler
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
        grepl("W", event) | grepl("OUT", shortDesc) ~ 1,
        TRUE ~ 0),
      #Does the wicket count for the bowler? (TODO: Other non bowler dismissals)
      bowling_wickets = case_when(
        grepl("W", event) & !grepl("run out", wicketDesc) ~ 1,
        grepl("OUT", shortDesc) & !grepl("run out", wicketDesc) ~ 1,
        TRUE ~ 0),
      #What extras were conceded?
      extras = as.integer(case_when(
        grepl("nb", event) ~ 1, #No balls count as 1
        grepl("\\d[b|lb|n\\-b|n\\-l|w]", event) ~ as.double(gsub("[b|lb|n\\-b|n\\-l|w]", "", event)), #byes, leg byes, no balls + (leg) byes & wides
        grepl(".|[1-6]|W", event) ~ 0, #No extras
        TRUE ~ -9))
    ) %>%
    dplyr::arrange(innings, ball, legalBall) %>%
    dplyr::group_by(innings) %>%
    dplyr::mutate(row = seq(1:n())) 
  
  #Wickets to fall in the match
  w <- match %>%
    dplyr::filter(event == "W" | grepl("OUT", shortDesc)) %>%
    dplyr::select(innings, row, batsman, wickets, wicketDesc)
  
  #Check to see if a name is used more than once (*cough* YADAV *cough*)
  w2 <- w %>%
    dplyr::filter(!grepl("run out", wicketDesc)) %>%
    dplyr::group_by(innings, batsman) %>%
    dplyr::mutate(rows = n(),
           batsman2 = ifelse(rows > 1, paste0(batsman, seq(1:n())), batsman)) %>%
    dplyr::filter(rows > 1) %>%
    dplyr::select(innings, batsman, r=row, batsman2)
  
  #Stick this back onto the match data and relabel the batsman
  match <- match %>%
    dplyr::left_join(w2, by=c("innings", "batsman")) %>%
    dplyr::group_by(innings, batsman) %>%
    dplyr::mutate(keep = ifelse(!is.na(batsman2) & ((row <= min(r) & r == min(r)) | (row > min(r) & r == max(r))), 1, 0),
           batsman = ifelse(!is.na(batsman2) & keep == 1, batsman2, batsman)) %>%
    dplyr::filter(is.na(batsman2) | keep == 1) %>%
    dplyr::select(-c(batsman2, keep, r))
  
  #Wickets to fall in the match
  w <- match %>%
    dplyr::filter(event == "W" | grepl("OUT", shortDesc)) %>%
    dplyr::select(innings, row, batsman, wickets, wicketDesc)
  
  w2 <- w %>%
    dplyr::rowwise() %>%
    dplyr::mutate(dismissed = ifelse(grepl(batsman, wicketDesc), 1, 0)) %>%
    dplyr::group_by(innings, batsman) %>%
    dplyr::summarise(dismissed = max(dismissed))
  
  #We can use the summary to check what the wickets name is!
  ballsFaced <- match %>%
    dplyr::group_by(innings, batsman) %>%
    dplyr::summarise(Innings = first(innings),
              Runs = sum(batting_runs),
              # Balls = sum(ballFaced),
              Balls = sum(legalBall + ifelse(grepl("no ball", shortRuns), 1, 0)),
              `4s` = sum(batting_runs==4 & shortDesc %like% "FOUR"), #Catch in case the four wasn't a boundary
              `6s` = sum(batting_runs==6 & shortDesc %like% "SIX")) %>%
    dplyr::left_join(w %>% select(innings, batsman, wicketDesc), by=c("innings", "batsman")) %>%
    dplyr::left_join(w2 %>% select(innings, batsman, dismissed), by=c("innings", "batsman")) %>%
    dplyr::group_by(innings, batsman) %>%
    dplyr::mutate(rows = n(),
           rownum = seq(1:n())) %>%
    dplyr::rowwise() %>%
    dplyr::filter(rows == 1 | (dismissed == 0 && rownum == 1) | grepl(batsman, wicketDesc) | is.na(wicketDesc))
  
  ballsBowled <- match %>%
    dplyr::group_by(innings, bowler) %>%
    dplyr::summarise(Innings = first(innings),
              Runs = sum(bowling_runs),
              Balls = sum(legalBall),
              Overs = floor(Balls/6) + (Balls %% 6 / 10),
              `0s` = sum(bowling_runs == 0),
              `4s` = sum(batting_runs == 4 & shortDesc %like% "FOUR"), #Catch in case the four wasn't a boundary
              `6s` = sum(batting_runs == 6 & shortDesc %like% "SIX"))
  
  #Does anyone have too many balls bowled?
  ballsBowledTest <- ballsBowled %>% filter(Overs > 4)
  if (nrow(ballsBowledTest) > 0) {
    bowlers <- unique(ballsBowledTest$bowler)
    matchTest <- match %>% 
      dplyr::filter(bowler %in% bowlers & bowler2 != "") %>%
      dplyr::rowwise() %>%
      dplyr::mutate(ambiguous = !grepl(paste0("[A-Z] ", bowler), bowler2)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(#If it's still ambiguous we know that there is either a change of bowler 
        #when we are in consecutive overs, or no change for non-consecutive overs?
        bowler3 = case_when(
          ambiguous == TRUE & ball - lag(ball) == 1 ~ lag(bowler2, 2),
          ambiguous == TRUE ~ lag(bowler2, 1),
          TRUE ~ bowler2),
        over = floor(ball)
      ) %>%
      dplyr::select(innings, over, bowler3)
    
    match <- match %>%
      dplyr::mutate(over = floor(ball)) %>%
      dplyr::left_join(matchTest, by=c("innings", "over")) %>%
      dplyr::mutate(bowler = ifelse(!is.na(bowler3), bowler3, bowler)) %>%
      dplyr::select(-over)
  }
  
  ballsBowled <- match %>%
    dplyr::group_by(innings, bowler) %>%
    dplyr::summarise(Innings = first(innings),
              Runs = sum(bowling_runs),
              Balls = sum(legalBall),
              Overs = floor(Balls/6) + (Balls %% 6 / 10),
              `0s` = sum(bowling_runs == 0),
              `4s` = sum(batting_runs == 4 & shortDesc %like% "FOUR"), #Catch in case the four wasn't a boundary
              `6s` = sum(batting_runs == 6 & shortDesc %like% "SIX"))
  
  batting_summary <- batting_summary %>%
    dplyr::group_by(Innings) %>%
    dplyr::mutate(battingOrder = seq(1:n()))
  
  bowling_summary <- bowling_summary %>%
    dplyr::group_by(Innings) %>%
    dplyr::mutate(bowlingOrder = seq(1:n()))
  
  batting_summary2 <- merge(ballsFaced, batting_summary, by=c("Innings", "Runs", "Balls", "4s", "6s")) %>%
    dplyr::mutate(Batsman = gsub("\\(c\\)|†", "", Batsman),
           Batsman = trimws(gsub("  ", " ", Batsman)),
           Batsman = ifelse(grepl("[A-z]", substr(Batsman, nchar(Batsman), nchar(Batsman))), Batsman, substr(Batsman, 1, nchar(Batsman) - 1))) %>%
    dplyr::select(innings, batsman, Batsman, battingOrder, wicketDesc) %>%
    dplyr::group_by(innings, batsman) %>%
    dplyr::mutate(rows = n()) %>%
    dplyr::rowwise() %>%
    dplyr::filter(grepl(batsman, Batsman)) %>%
    dplyr::select(-wicketDesc)
  
  if (nrow(batting_summary2) != nrow(ballsFaced)) {
    batting_summary2 <- merge(ballsFaced, batting_summary, by=c("Innings", "Runs", "Balls", "4s", "6s")) %>%
      dplyr::mutate(Batsman = gsub("\\(c\\)|†", "", Batsman),
             Batsman = trimws(gsub("  ", " ", Batsman)),
             Batsman = ifelse(grepl("[A-z]", substr(Batsman, nchar(Batsman), nchar(Batsman))), Batsman, substr(Batsman, 1, nchar(Batsman) - 1))) %>%
      dplyr::select(innings, batsman, Batsman, battingOrder, wicketDesc) %>%
      dplyr::group_by(innings, batsman) %>%
      dplyr::mutate(rows = n()) %>%
      dplyr::rowwise() %>%
      dplyr::filter(rows == 1 | grepl(batsman, Batsman) | grepl(Batsman, wicketDesc)) %>%
      dplyr::select(-wicketDesc)
  }
  
  bowling_summary2 <- merge(ballsBowled, bowling_summary, by=c("Innings", "Runs", "Overs", "0s", "4s", "6s")) %>%
    dplyr::mutate(Bowler = gsub("\\(c\\)|†", "", Bowler),
           Bowler = trimws(gsub("  ", " ", Bowler)),
           Bowler = ifelse(grepl("[A-z]", substr(Bowler, nchar(Bowler), nchar(Bowler))), Bowler, substr(Bowler, 1, nchar(Bowler) - 1))) %>%
    dplyr::select(innings, bowler, Bowler, bowlingOrder) %>%
    dplyr::group_by(innings, bowler) %>%
    dplyr::mutate(rows = n()) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(bowler2 = gsub("^([A-Z] )", "", bowler)) %>%
    dplyr::filter(rows == 1 | grepl(bowler, Bowler) | grepl(bowler2, Bowler))
  
  w2 <- merge(w, batting_summary2, by="innings")  %>%
    dplyr::rowwise() %>%
    dplyr::mutate(d = grepl(Batsman, wicketDesc)) %>%
    dplyr::filter(d) %>%
    dplyr::group_by(innings, batsman.y) %>%
    dplyr::slice(1) %>%
    dplyr::arrange(innings, row) %>%
    dplyr::group_by(innings) %>%
    dplyr::mutate(wicketOrder = seq(1:n())) %>%
    dplyr::select(innings, end_row=row, dismissed = batsman.y, wicketOrder)
  
  partnerships <- match %>%
    dplyr::group_by(innings, batsman) %>%
    dplyr::summarise(start_row = min(row),
              first_faced = min(row),
              last_faced = max(row)) %>%
    dplyr::left_join(w2, by=c("innings", "batsman"="dismissed")) %>%
    dplyr::left_join(batting_summary2, by=c("innings", "batsman")) %>%
    dplyr::group_by(innings) %>%
    dplyr::mutate(end_row = ifelse(is.na(end_row), max(last_faced), end_row)) %>%
    dplyr::arrange(innings, battingOrder) %>%
    dplyr::mutate(start_row = ifelse(is.na(lag(start_row, 2)), 1, cumulativeRank(end_row, 2) + 1),
           prev_end_row = lag(end_row), 
           prev_end_row2 = lag(end_row, 2),
           cum_end_row_max = lag(cummax(end_row))) %>%
    dplyr::group_by(innings) %>%
    dplyr::mutate(prev_start_row = lag(start_row),
           start_row = ifelse(start_row == prev_start_row & start_row != 1, prev_start_row + 1, start_row)) %>%
    dplyr::select(innings, batsman, start_row, first_faced, last_faced, end_row, Batsman, battingOrder) %>%
    dplyr::arrange(innings, desc(end_row)) %>%
    dplyr::mutate(end_row = ifelse(seq(1:n()) == 2, lag(end_row), end_row)) %>%
    dplyr::arrange(innings, battingOrder)
  
  partnerships2 <- partnerships %>%
    dplyr::left_join(partnerships, by=c("innings")) %>%
    dplyr::filter(batsman.x != batsman.y & end_row.x >= start_row.y & start_row.x <= start_row.y) %>%
    dplyr::group_by(innings, batsman.x, batsman.y) %>%
    dplyr::mutate(start_row = max(start_row.x, start_row.y, na.rm=T),
           end_row = min(end_row.x, end_row.y, na.rm=T)) %>%
    dplyr::select(innings, batsman1 = batsman.x, batsman2 = batsman.y, start_row, end_row) %>%
    dplyr::group_by(innings) %>%
    dplyr::arrange(innings, start_row) %>%
    dplyr::filter(seq(1:n()) != 2)
  
  match <- match %>%
    dplyr::left_join(partnerships2, by=c("innings")) %>%
    dplyr::filter(row >= start_row & row <= end_row | innings > 2) %>%
    dplyr::mutate(non_striker = ifelse(batsman == batsman1, batsman2, batsman1)) %>%
    dplyr::left_join(w2, by=c("innings", "row"="end_row")) %>%
    dplyr::left_join(batting_summary2, by=c("innings", "batsman")) %>%
    dplyr::left_join(bowling_summary2, by=c("innings", "bowler"))
  
  
  match <- match %>%
    dplyr::mutate(matchID = matchID,
           match = index,
           over = floor(ball) + 1) %>%
    dplyr::select(matchID, 
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
           wicketDesc,
           wicketOrder,
           dismissed,
           bowling_wickets,
           battingOrder, 
           bowlingOrder)
}

#Returns a summary of the match from the ball by ball data
summaryT20 <- function(x, type=c("batting", "bowling", "innings")) {
  
  tryCatch({
    #Summary code here
    if (type == "batting") {
      #Batting summary by innings (runs, balls, 4s, 6s, strike rate, not-out)
      summary <- x %>%
        group_by(matchID, innings, batsman, Batsman) %>%
        summarise(Runs = sum(batting_runs),
                  Balls = sum(legalBall + ifelse(grepl("no ball", shortRuns), 1, 0)),
                  `4s` = sum(batting_runs==4 & shortDesc %like% "FOUR"),
                  `6s` = sum(batting_runs==6 & shortDesc %like% "SIX"),
                  SR = round(Runs/Balls*100, 2)) %>%
        mutate(`100` = sum(Runs >= 100),
               `50` = sum(Runs >= 50 & Runs < 100),
               `0` = sum(Runs == 0)) %>%
        group_by(batsman) %>%
        mutate(HS = max(Runs))
      
      wickets <- x %>%
        group_by(matchID, innings, dismissed) %>%
        summarise() %>%
        mutate(out = 1)
      
      summary <- summary %>%
        left_join(wickets, by=c("matchID", "innings", "batsman"="dismissed")) %>%
        mutate(out = ifelse(is.na(out), 0, 1),
               HSout = ifelse(Runs==HS, out, 0),
               NotOut = 1 - out,
               HSText = ifelse(HSout == 0, paste0(HS, "*"), paste0(HS))) %>%
        select(-out)
      
      #Now we have the stats for each batsman we want to order them correctly
      summary <- x %>%
        group_by(matchID, innings, batsman, Batsman) %>%
        summarise(battingOrder = min(battingOrder)) %>%
        arrange(innings, battingOrder) %>%
        select(matchID, innings, batsman, Batsman) %>%
        left_join(summary, by=c("matchID", "innings", "batsman", "Batsman"))
    } else if (type == "bowling") {
      #Bowling summary by innings (balls, dots, runs, wickets, economy, 4s,6s, wides, no balls)
      summary <- x %>%
        group_by(matchID, innings, bowler, Bowler) %>%
        summarise(Balls = sum(legalBall),
                  Overs = floor(Balls/6) + (Balls %% 6 / 10),
                  Dots = sum(bowling_runs == 0),
                  Runs = sum(bowling_runs),
                  Wickets = sum(bowling_wickets),
                  RPB = round(Runs / Balls, 2),
                  `4s` = sum(batting_runs == 4 & shortDesc %like% "FOUR"),
                  `6s` = sum(batting_runs == 6 & shortDesc %like% "SIX"),
                  Wides = sum(grepl("w", event)),
                  NoBalls = sum(grepl("nb", event))) %>%
        select(-Balls)
      
      
      #Now we have the stats for each bowler we want to order them correctly
      summary <- x %>%
        group_by(matchID, innings) %>%
        mutate(row = seq(1:n())) %>%
        group_by(matchID, innings, bowler, Bowler) %>%
        summarise(start_row = min(row)) %>%
        arrange(innings, start_row) %>%
        select(matchID, innings, bowler, Bowler) %>%
        left_join(summary, by=c("matchID", "innings", "bowler", "Bowler"))
    } else {
      #Overall innings scores (runs/wickets)
      summary <- x %>%
        group_by(matchID, innings) %>%
        summarise(runs = sum(runs),
                  wickets = sum(wickets),
                  balls = max(ball))
    }
  }, 
  error = function(e) {
    summary <- NA
    message("Error in input!")
  })
  return(summary)
}

#Returns a summary of the match from the ball by ball data
#Some changes from the T20 copy due to using balls, not overs
summaryTheHundred <- function(x, type=c("batting", "bowling", "innings")) {
  tryCatch({
    #Summary code here
    if (type == "batting") {
      #Batting summary by innings (runs, balls, 4s, 6s, strike rate, not-out)
      summary <- x %>%
        group_by(matchID, innings, batsman, Batsman) %>%
        summarise(Runs = sum(batting_runs),
                  Balls = sum(legalBall + ifelse(grepl("no ball", shortRuns), 1, 0)),
                  `4s` = sum(batting_runs==4 & shortDesc %like% "FOUR"),
                  `6s` = sum(batting_runs==6 & shortDesc %like% "SIX"),
                  SR = round(Runs/Balls*100, 2)) %>%
        mutate(`100` = sum(Runs >= 100),
               `50` = sum(Runs >= 50 & Runs < 100),
               `0` = sum(Runs == 0)) %>%
        group_by(batsman) %>%
        mutate(HS = max(Runs))
      
      wickets <- x %>%
        group_by(matchID, innings, dismissed) %>%
        summarise() %>%
        mutate(out = 1)
      
      summary <- summary %>%
        left_join(wickets, by=c("matchID", "innings", "batsman"="dismissed")) %>%
        mutate(out = ifelse(is.na(out), 0, 1),
               HSout = ifelse(Runs==HS, out, 0),
               NotOut = 1 - out,
               HSText = ifelse(HSout == 0, paste0(HS, "*"), paste0(HS))) %>%
        select(-out)
      
      #Now we have the stats for each batsman we want to order them correctly
      summary <- x %>%
        group_by(matchID, innings, batsman, Batsman) %>%
        summarise(battingOrder = min(battingOrder)) %>%
        arrange(innings, battingOrder) %>%
        select(matchID, innings, batsman, Batsman) %>%
        left_join(summary, by=c("matchID", "innings", "batsman", "Batsman"))
    } else if (type == "bowling") {
      #Bowling summary by innings (balls, dots, runs, wickets, economy, 4s,6s, wides, no balls)
      summary <- x %>%
        group_by(matchID, innings, bowler, Bowler) %>%
        summarise(Balls = sum(legalBall),
                  Dots = sum(bowling_runs == 0),
                  Runs = sum(bowling_runs),
                  Wickets = sum(bowling_wickets),
                  RPB = round(Runs / Balls, 2),
                  `4s` = sum(batting_runs == 4 & shortDesc %like% "FOUR"),
                  `6s` = sum(batting_runs == 6 & shortDesc %like% "SIX"),
                  Wides = sum(grepl("w", event)),
                  NoBalls = sum(grepl("nb", event)))
      
      
      #Now we have the stats for each bowler we want to order them correctly
      summary <- x %>%
        group_by(matchID, innings) %>%
        mutate(row = seq(1:n())) %>%
        group_by(matchID, innings, bowler, Bowler) %>%
        summarise(start_row = min(row)) %>%
        arrange(innings, start_row) %>%
        select(matchID, innings, bowler, Bowler) %>%
        left_join(summary, by=c("matchID", "innings", "bowler", "Bowler"))
    } else {
      #Overall innings scores (runs/wickets)
      summary <- x %>%
        group_by(matchID, innings) %>%
        summarise(runs = sum(runs),
                  wickets = sum(wickets),
                  balls = max(ball))
    }
  }, 
  error = function(e) {
    summary <- NA
    message("Error in input!")
  })
  return(summary)
}

#Checks ball by ball data against the known batting/bowling card
checkDataT20 <- function(matchID) {
  try({
    match <- read.fst(paste0("data/ball-by-ball/", matchID, ".fst"), as.data.table=TRUE)
    batting_summary <- read.fst(paste0("data/batting-summaries/", matchID, ".fst"), as.data.table=TRUE) %>%
      mutate(Batsman = gsub("\\(c\\)|†|Â|â€", "", Batsman),
             Batsman = trimws(gsub("  ", " ", Batsman)),
             Batsman = ifelse(grepl("[A-z]", substr(Batsman, nchar(Batsman), nchar(Batsman))), Batsman, substr(Batsman, 1, nchar(Batsman) - 1)),
             Batsman = ifelse(grepl("[A-z]", substr(Batsman, nchar(Batsman), nchar(Batsman))), Batsman, substr(Batsman, 1, nchar(Batsman) - 1)))
    bowling_summary <- read.fst(paste0("data/bowling-summaries/", matchID, ".fst"), as.data.table=TRUE) %>%
      mutate(Bowler = gsub("\\(c\\)|†|Â|â€", "", Bowler),
             Bowler = trimws(gsub("  ", " ", Bowler)),
             Bowler = ifelse(grepl("[A-z]", substr(Bowler, nchar(Bowler), nchar(Bowler))), Bowler, substr(Bowler, 1, nchar(Bowler) - 1)),
             Bowler = ifelse(grepl("[A-z]", substr(Bowler, nchar(Bowler), nchar(Bowler))), Bowler, substr(Bowler, 1, nchar(Bowler) - 1)))
    
    #Check stats for batsman we successfully matched against the data
    batting_test <- merge(summaryT20(match, type="batting"), batting_summary, by="Batsman", sort=FALSE) %>%
      mutate(check_runs = ifelse(as.numeric(Runs.x) == as.numeric(Runs.y), 1, 0),
             check_balls = ifelse(as.numeric(Balls.x) == as.numeric(Balls.y), 1, 0),
             check_4s = ifelse(as.numeric(`4s.x`) == as.numeric(`4s.y`), 1, 0),
             check_6s = ifelse(as.numeric(`6s.x`) == as.numeric(`6s.y`), 1, 0),
             check = min(across(starts_with("check_"))))
    
    #Check all batsman appear too
    batting_test2 <- nrow(batting_summary[Balls > 0]) == nrow(batting_test)
    batting_test2 <- batting_test2 | nrow(batting_summary[Balls != ""]) == nrow(batting_test)
    
    #Check stats for bowlers we successfully matched against the data
    bowling_test <- merge(summaryT20(match, type="bowling"), bowling_summary, by="Bowler", sort=FALSE) %>%
      mutate(check_runs = ifelse(as.numeric(Runs.x) == as.numeric(Runs.y), 1, 0),
             check_overs = ifelse(as.numeric(Overs.x) == as.numeric(Overs.y), 1, 0),
             check_wickets = ifelse(as.numeric(Wickets.x) == as.numeric(Wickets.y), 1, 0),
             check_4s = ifelse(as.numeric(`4s.x`) == as.numeric(`4s.y`), 1, 0),
             check_6s = ifelse(as.numeric(`6s.x`) == as.numeric(`6s.y`), 1, 0),
             check = min(across(starts_with("check_"))))
    
    #Check all bowlers appear too
    bowling_test2 <- nrow(bowling_summary) == nrow(bowling_test)
    
    if(min(batting_test$check) == 0 & min(bowling_test$check)) {
      message(paste0("Error in ", matchID, ". Check batting & bowling stats."))
      return(matchID)
    } else if(batting_test2 == FALSE & bowling_test2 == FALSE) {
      message(paste0("Error in ", matchID, ". Check batting & bowling stats."))
    } else if(min(batting_test$check) == 0 | batting_test2 == FALSE) {
      message(paste0("Error in ", matchID, ". Check batting stats."))
      return(matchID)
    } else if(min(bowling_test$check) == 0 | bowling_test2 == FALSE) {
      message(paste0("Error in ", matchID, ". Check bowling stats."))
      return(matchID)
    } else {
      return(NULL)
    }
  }, silent=TRUE)
}

#Checks ball by ball data against the known batting/bowling card
checkDataHundred <- function(matchID) {
  tryCatch({
    match <- read.fst(paste0("data/ball-by-ball/", matchID, ".fst"), as.data.table=TRUE)
    batting_summary <- read.fst(paste0("data/batting-summaries/", matchID, ".fst"), as.data.table=TRUE) %>%
      mutate(Batsman = gsub("\\(c\\)|†|Â|â€", "", Batsman),
             Batsman = trimws(gsub("  ", " ", Batsman)),
             Batsman = ifelse(grepl("[A-z]", substr(Batsman, nchar(Batsman), nchar(Batsman))), Batsman, substr(Batsman, 1, nchar(Batsman) - 1)),
             Batsman = ifelse(grepl("[A-z]", substr(Batsman, nchar(Batsman), nchar(Batsman))), Batsman, substr(Batsman, 1, nchar(Batsman) - 1)))
    bowling_summary <- read.fst(paste0("data/bowling-summaries/", matchID, ".fst"), as.data.table=TRUE) %>%
      mutate(Bowler = gsub("\\(c\\)|†|Â|â€", "", Bowler),
             Bowler = trimws(gsub("  ", " ", Bowler)),
             Bowler = ifelse(grepl("[A-z]", substr(Bowler, nchar(Bowler), nchar(Bowler))), Bowler, substr(Bowler, 1, nchar(Bowler) - 1)),
             Bowler = ifelse(grepl("[A-z]", substr(Bowler, nchar(Bowler), nchar(Bowler))), Bowler, substr(Bowler, 1, nchar(Bowler) - 1)))
    
    #Check stats for batsman we successfully matched against the data
    batting_test <- merge(summaryTheHundred(match, type="batting"), batting_summary, by="Batsman", sort=FALSE) %>%
      mutate(check_runs = ifelse(as.numeric(Runs.x) == as.numeric(Runs.y), 1, 0),
             check_balls = ifelse(as.numeric(Balls.x) == as.numeric(Balls.y), 1, 0),
             check_4s = ifelse(as.numeric(`4s.x`) == as.numeric(`4s.y`), 1, 0),
             check_6s = ifelse(as.numeric(`6s.x`) == as.numeric(`6s.y`), 1, 0),
             check = min(across(starts_with("check_"))))
    
    #Check all batsman appear too
    batting_test2 <- nrow(batting_summary[Balls > 0]) == nrow(batting_test)
    batting_test2 <- batting_test2 | nrow(batting_summary[Balls != ""]) == nrow(batting_test)
    
    #Check stats for bowlers we successfully matched against the data
    bowling_test <- merge(summaryTheHundred(match, type="bowling"), bowling_summary, by="Bowler", sort=FALSE) %>%
      mutate(check_runs = ifelse(as.numeric(Runs.x) == as.numeric(Runs.y), 1, 0),
             # check_overs = ifelse(as.numeric(Overs.x) == as.numeric(Overs.y), 1, 0),
             check_balls = ifelse(as.numeric(Balls.x) == as.numeric(Balls.y), 1, 0),
             check_wickets = ifelse(as.numeric(Wickets.x) == as.numeric(Wickets.y), 1, 0),
             check_4s = ifelse(as.numeric(`4s.x`) == as.numeric(`4s.y`), 1, 0),
             check_6s = ifelse(as.numeric(`6s.x`) == as.numeric(`6s.y`), 1, 0),
             check = min(across(starts_with("check_"))))
    
    #Check all bowlers appear too
    bowling_test2 <- nrow(bowling_summary) == nrow(bowling_test)
    
    if(min(batting_test$check) == 0 & min(bowling_test$check)) {
      message(paste0("Error in ", matchID, ". Check batting & bowling stats."))
      return(matchID)
    } else if(batting_test2 == FALSE & bowling_test2 == FALSE) {
      message(paste0("Error in ", matchID, ". Check batting & bowling stats."))
    } else if(min(batting_test$check) == 0 | batting_test2 == FALSE) {
      message(paste0("Error in ", matchID, ". Check batting stats."))
      return(matchID)
    } else if(min(bowling_test$check) == 0 | bowling_test2 == FALSE) {
      message(paste0("Error in ", matchID, ". Check bowling stats."))
      return(matchID)
    } else {
      return(NULL)
    }
  },
  error = function(e) { return(NULL) }, silent=TRUE)
}

#Combine all balls from a particular series
importSeries <- function(series) {
  matchIDs <- read.fst("data/matchInfo.fst") %>%
    dplyr::filter(series1 == series | series2 == series) %>%
    dplyr::filter(!grepl("abandoned|No result", result)) %>%
    dplyr::select(matchID)
  
  tryCatch({
    balls <- lapply(paste0("data/ball-by-ball/", matchIDs$matchID, ".fst"), read.fst)
    return(data.table::rbindlist(balls))
  },
  error = function(e) { 
    message("Error - scrape data")
  })
}

#Needs fixing to be able to run either the Hundred or a T20/ODI
#Currently set to run the charts for the Hundred
manhattanChart <- function(match) {
  
  title = "Manhattan Chart"
  
  numMatches <- length(unique(match$matchID))
  if(numMatches == 1) {
    match <- match %>%
      dplyr::mutate(set = floor((ball - 1)/5) + 1) %>%
      dplyr::group_by(matchID, innings, team, set) %>%
      dplyr::summarise(runs = sum(runs)) %>%
      dplyr::group_by(matchID) %>%
      dplyr::mutate(crr = runs / set,
                    matchDesc = paste0(first(team), " v ", last(team)))
    
    subtitle <- unique(match$matchDesc)
    
    match$matchDesc <- factor(match$matchDesc, levels = rev(unique(match$matchDesc)))
    
    manhattan <- ggplot(match, aes(x = as.factor(set), y = runs, group=paste0(innings, " ", team), fill=team)) +
      geom_col(width = 0.5, position=position_dodge2(preserve="single", padding = 0.2)) +
      theme_kantar() +
      teamFill(matchNames = TRUE) + 
      theme(
        panel.grid.major.y = element_line(colour="grey95")
      ) +
      scale_y_continuous(limits=c(0, ceiling(max(match$runs)/5)*5)) +
      annotate("segment", x=-Inf, xend=Inf, y=0, yend=0, size=0.5, colour="grey75") +
      labs(
        title = title,
        subtitle = subtitle,
        x = "Over",
        y = "Runs"
      ) 
  } else {
    match <- match %>%
      dplyr::mutate(set = floor((ball - 1)/5) + 1) %>%
      dplyr::group_by(matchID, innings, team, set) %>%
      dplyr::summarise(runs = sum(runs)) %>%
      dplyr::group_by(matchID) %>%
      dplyr::mutate(crr = runs / set,
                    matchDesc = paste0(matchID, " - ", first(team), " v ", last(team)))
    
    subtitle <- "Selected Games"
    
    match$matchDesc <- factor(match$matchDesc, levels = rev(unique(match$matchDesc)))
    
    manhattan <- ggplot(match, aes(x = as.factor(set), y = runs, group=paste0(innings, " ", team), fill=team)) +
      geom_col(width = 0.5, position=position_dodge2(preserve="single", padding = 0.2)) +
      theme_kantar() +
      teamFill(matchNames = TRUE) + 
      theme(
        panel.grid.major.y = element_line(colour="grey95")
      ) +
      scale_y_continuous(limits=c(0, ceiling(max(match$runs)/5)*5)) +
      annotate("segment", x=-Inf, xend=Inf, y=0, yend=0, size=0.5, colour="grey75") +
      labs(
        title = title,
        subtitle = subtitle,
        x = "Over",
        y = "Runs"
      ) +
      facet_wrap(~matchDesc)
  }
  return(manhattan)
}

matchSummary <- function(matchID) {
  balls <- read.fst(paste0("data/ball-by-ball/", matchID, ".fst"))
  
  batting <- summaryT20(balls, type="batting")
  bowling <- summaryT20(balls, type="bowling")
  innings <- summaryT20(balls, type="innings")
  
  manhattan <- manhattanChart(balls)
  return(manhattan)
}

# s <- matchSummary(1273756)
# s

#--------------------------------------------------------------#
####     Config Options                                     ####
#--------------------------------------------------------------#

#What colours to use for various teams
teamColours <- c(
  #Teams from the ICC T20 World Cup 2021
  'Afghanistan'      = "#c6c6c6",
  'Australia'        = "#bfc83e",
  'Bangladesh'       = "#006a4e",
  'England'          = "#94bfac",
  'India'            = "#00008b",
  'Ireland'          = "#169b62",
  'Namibia'          = "#001489",
  'Netherlands'      = "#ff9B00",
  'New Zealand'      = "#2b2b2b",
  'Oman'             = "#db161b",
  'Pakistan'         = "#006600",
  'Papua New Guinea' = "#c8102e",
  'Scotland'         = "#0065bf",
  'South Africa'     = "#007a4d",
  'Sri Lanka'        = "#00008b",
  'West Indies'      = "#7b0041"

  #Add more teams below here (IPL Next)
)

#Groupings of colours based on team names
#Used to chart specific events
teamPalettes <- list(
  #ICC T20 World Cup 2021
  'T20 World Cup 2021' = cricketColours("Afghanistan", "Australia",
                                        "Bangladesh",  "England",
                                        "India",       "Ireland",
                                        "Namibia",     "Netherlands",
                                        "New Zealand", "Oman",
                                        "Pakistan",    "Papua New Guinea",
                                        "Scotland",    "South Africa",
                                        "Sri Lanka",   "West Indies")

  #IPL 2021
)
























