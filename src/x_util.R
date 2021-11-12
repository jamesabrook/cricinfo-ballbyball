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

#Saves match information into a master record data/matchInfo.fst
#Can overwrite a matchID if needed or ignore duplicates and append new matches
saveMatchInfo <- function(matchInfo, outLoc="data/", filename="matchInfo.fst", overwrite=FALSE) {
  file <- paste0(outLoc, filename)
  if (file.exists(file)) {
    base <- read.fst(file) 
    
    if (overwrite) {
      base <- base %>%
        anti_join(matchInfo, by="matchID")
      
      matchInfo <- rbindlist(list(base, matchInfo), use.names=TRUE, fill=TRUE) %>%
        arrange(date, matchID)
      
      write.fst(matchInfo, file)
    } else {
      matchInfo <- matchInfo %>%
        anti_join(base, by="matchID") 
      
      matchInfo <- rbindlist(list(base, matchInfo), use.names=TRUE, fill=TRUE) %>%
        arrange(date, matchID)
      
      write.fst(matchInfo, file)
    }
    
  } else {
    write.fst(matchInfo, file)
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

#Returns a summary of the match from the ball by ball data
summaryT20 <- function(x, type=c("batting", "bowling", "innings")) {
  if (!is.na(x)) {
    if (type == "batting") {
      #Batting summary by innings (runs, balls, 4s, 6s, strike rate, not-out)
      summary <- x %>%
        group_by(match, innings, batsman, Batsman) %>%
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
        group_by(match, innings, dismissed) %>%
        summarise() %>%
        mutate(out = 1)
      
      summary <- summary %>%
        left_join(wickets, by=c("match", "innings", "batsman"="dismissed")) %>%
        mutate(out = ifelse(is.na(out), 0, 1),
               HSout = ifelse(Runs==HS, out, 0),
               NotOut = 1 - out,
               HSText = ifelse(HSout == 0, paste0(HS, "*"), paste0(HS))) %>%
        select(-out)
      
      #Now we have the stats for each batsman we want to order them correctly
      summary <- x %>%
        group_by(match, innings, batsman, Batsman) %>%
        summarise(battingOrder = min(battingOrder)) %>%
        arrange(innings, battingOrder) %>%
        select(match, innings, batsman, Batsman) %>%
        left_join(summary, by=c("match", "innings", "batsman", "Batsman"))
    } else if (type == "bowling") {
      #Bowling summary by innings (balls, dots, runs, wickets, economy, 4s,6s, wides, no balls)
      summary <- x %>%
        group_by(match, innings, bowler, Bowler) %>%
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
        group_by(match, innings) %>%
        mutate(row = seq(1:n())) %>%
        group_by(match, innings, bowler, Bowler) %>%
        summarise(start_row = min(row)) %>%
        arrange(innings, start_row) %>%
        select(match, innings, bowler, Bowler) %>%
        left_join(summary, by=c("match", "innings", "bowler", "Bowler"))
    } else {
      #Overall innings scores (runs/wickets)
      summary <- x %>%
        group_by(match, innings) %>%
        summarise(runs = sum(runs),
                  wickets = sum(wickets),
                  balls = max(ball))
    }
  } else {
    summary <- NA
  }
  return(summary)
}

#Checks ball by ball data against the known batting/bowling card
checkData <- function(matchID) {
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

# check <- lapply(matchInfo2$matchID, checkData)
# check <- check[-which(sapply(check, is.null))]

# check <- checkData(1216493)

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
























