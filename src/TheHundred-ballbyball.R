################################################################################
############       Scraping Ball-by-Ball data for The Hundred       ############
############                 James Brook 2021-07-24                 ############
################################################################################

library(rvest)
library(stringr)
library(RSelenium)
library(dplyr)
library(data.table)

############
# Setup 
############
p <- as.integer(5005)
driver <- rsDriver(browser=c("chrome"), port = p, geckover = NULL)
remDr <- driver[["client"]]
# remDr$open()


url <- "https://www.espncricinfo.com/series/the-hundred-men-s-competition-2021-1252040/match-results"

#Read the raw html data
raw <- try(xml2::read_html(url), silent = TRUE)
if ("try-error" %in% class(raw)) 
  stop("Error in URL")

#Extract tables
t <- raw %>% 
  rvest::html_nodes("a") %>% 
  html_attr("href") %>%
  data_frame(.) %>%
  filter(grepl("full-scorecard", .) & grepl("the-hundred-men", .) & !duplicated(.))

scrapeTheHundred <- function(x, index) {
  cookiesAccepted <- FALSE
  notNowClicked <- FALSE
  url <- paste0("https://www.espncricinfo.com", x)
  
  #Read the raw html data
  raw <- try(xml2::read_html(url), silent = TRUE)
  if ("try-error" %in% class(raw)) 
    stop("Error in URL")
  
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
  summary <- try(setDT(rbind(setDT(tables[[1]][, c(1:4, 6:8)])[, innings:=1], 
                         setDT(tables[[3]][, c(1:4, 6:8)])[, innings:=2])),
                 silent=TRUE)
  
  if (class(summary) != "try-error") {
    #Set the names so that we don't have any duplicates
    setnames(summary, names(summary), c("Batsman", "Dismissal", "Runs", "Balls", "4s", "6s", "SR", "Innings"))
    
    #Retain batsman rows and Extras/Total lines
    #Removes others
    summary <- summary %>%
        filter(Batsman != "" & !Batsman %like% "Fall of" & !Batsman %like% "Did not bat")
    
    url <- gsub("full-scorecard", "ball-by-ball-commentary", url)
    
    
    #Navigates to the commentary page and 'accetps' cookies
    remDr$navigate(url) #Entering our URL gets the browser to navigate to the page
    Sys.sleep(5)
    #Deal with cookies
    while (cookiesAccepted != TRUE & index == 1) {
      cookieButton <- try(remDr$findElement(using = 'xpath', '//*[@id="onetrust-accept-btn-handler"]'),
                          silent = TRUE)
      
      if(class(cookieButton) != "try-error") {
        cookieButton$clickElement()
        cookiesAccepted <- TRUE
      }
      
      Sys.sleep(2)
    }
    
    # CricInfo now has an annoying popup asking if we want live updates. 
    # This is a flag of if we have dismissed it or not
    #Wait until the button appears to dismiss it and then click it
    while (notNowClicked != TRUE & index == 1) {
      notNowUpdates <- try(remDr$findElement(using = 'xpath', '//*[@id="wzrk-cancel"]'),
                           silent = TRUE)
      if(class(notNowUpdates) != "try-error") {
        notNowUpdates$clickElement()
        notNowClicked <- TRUE
      }
      Sys.sleep(5)
    }
    
    
    ############
    # Scrape
    ############
    match <- NULL
    
    #Locates the innings selection dropdown
    inningsDropdown <- remDr$findElement(using = 'class',
                                         'comment-inning-dropdown')
    
    #Now loop through each innings, selecting the appropriate dropdown option each time
    for (inn in 1:2) {
      #Scroll to top of page - mostly so that I can check the innings selection
      remDr$executeScript(paste("scroll(0,",0,");"))
      
      # Sys.sleep(1)
      #Open dropdown
      inningsDropdown$clickElement()
      
      #Find the element referring to the innings we want
      #We need to use the xpath for this as it has no other identifying tags
      inningsSelection <- remDr$findElement(using = 'xpath', 
                                            paste0('//*[@id="main-container"]/div[1]/div/div/div[2]/div[2]/div[1]/div[1]/div/div/ul/li[', inn, ']'))
      
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
        
        Sys.sleep(1)
        
        #Read the page source and extract the ball information
        #Filtering to ball 0.1 means we keep only the first ball if found
        ball <- xml2::read_html(remDr$getPageSource()[[1]]) %>%
          rvest::html_nodes("span.match-comment-over") %>%
          rvest::html_text() %>%
          dplyr::tibble(ball = .) %>%
          dplyr::filter(ball == "1")
        
        if(nrow(ball) > 0) {
          firstBall = TRUE
          break #Exit the loop
        }
        
        #I found that sometimes the page would freeze up on my and stop adding earlier balls
        #This is some logic to detect when the earliest ball found hasn't changed
        #If this is the case then we refresh the page and reselect the innings commentary
        
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
        if (pass > 0) {
          if(earliest == prev) {
            message("Refreshing...")
            #If the page stops adding new deliveries then refresh and reselect the innings
            remDr$refresh()
            inningsDropdown <- remDr$findElement(using = 'xpath', 
                                                 '//*[@id="main-container"]/div[1]/div/div/div[2]/div[2]/div[1]/div[1]')
            inningsDropdown$clickElement()
            
            inningsSelection <- remDr$findElement(using = 'class', 
                                                  'ci-dd__selected-option')
            inningsSelection$clickElement()
          }
        }
        
        pass <- pass + 1
      }
      
      #Continue once we have all deliveries on the page
      
      #Read the page source and extract all ball  info
      pageSource <- xml2::read_html(remDr$getPageSource()[[1]])
      balls <- pageSource %>%
        rvest::html_nodes("span.match-comment-over") %>%
        rvest::html_text() %>%
        dplyr::data_frame(ball = .) #%>%
      dplyr::select(ball)
      
      #We can also extract the events that occured on each ball
      events <- pageSource %>%
        rvest::html_nodes(".match-comment-run-container") %>%
        rvest::html_text() %>%
        dplyr::data_frame(event = .)
      
      #Finally extract the bowler/batsman for each delivery
      players <- pageSource %>%
        rvest::html_nodes(".match-comment-wrapper") %>%
        rvest::html_text() %>%
        dplyr::data_frame(text = .) %>%
        dplyr::mutate(bowler = sub("([A-z ]+) to.*", "\\1", text),
                      batsman = sub("([A-z ]+),.*", "\\1", sub("[A-z ]+ to ([A-z ,]+)", "\\1", text))) %>%
        dplyr::select(bowler, batsman)
      
      #Our innings data is the combination of these three dataframes
      innings <- cbind(balls, events, players) %>%
        dplyr::arrange(ball) %>%
        dplyr::mutate(innings = inn)
      
      #Bind to the match data
      match <- rbind(match, innings)
    }
    
    #Once we have all the deliveries in a match we can start to make them more friendly
    #Convert the event column into runs, batting_runs (attributed to the batsman) and wickets
    #There may be more here later
    match <- match %>%
      mutate(ball = as.numeric(ball)) %>%
      arrange(innings, ball) %>%
      mutate(runs = case_when(
        event %in% c("•", "W") ~ 0,
        event %in% c("1", "1b", "1lb", "1nb", "1w") ~ 1,
        event %in% c("2", "2b", "2lb", "2nb", "2n-l") ~ 2,
        event %in% c("3", "3b", "3lb", "3nb", "3n-l") ~ 3,
        event %in% c("4", "4b", "4lb", "4nb", "4n-l") ~ 4,
        event %in% c("5", "5b", "5lb", "5nb", "5n-l") ~ 5,
        event %in% c("6", "6b", "6lb", "6nb", "6n-l") ~ 6,
        event %in% c("7", "7b", "7lb", "7nb", "7n-l") ~ 7,
        event %in% c("8", "8b", "8lb", "8nb", "8n-l") ~ 8,
        TRUE ~ -9
      ),
      batting_runs = case_when(
        event %in% c("•", "W", 
                     "1b", "1lb", "1nb", "1w",
                     "2b", "2lb", "2nb", "2n-l", "2w",
                     "3b", "3lb", "3w", "3n-l",
                     "4b", "4lb", "4w", "4n-l",
                     "5b", "5lb", "5w", "5n-l",
                     "6b", "6lb", "6w", "6n-l") ~ 0,
        event %in% c("1", "3nb") ~ 1,
        event %in% c("2", "4nb") ~ 2,
        event %in% c("3", "5nb") ~ 3,
        event %in% c("4", "6nb") ~ 4,
        event %in% c("5", "7nb") ~ 5,
        event %in% c("6", "8nb") ~ 6,
        TRUE ~ -9
      ),
      bowling_runs = case_when(
        event %in% c("•", "W", 
                     "1b", "1lb",
                     "2b", "2lb", 
                     "3b", "3lb",
                     "4b", "4lb",
                     "5b", "5lb",
                     "6b", "6lb") ~ 0,
        event %in% c("1", "1nb", "1w") ~ 1,
        event %in% c("2", "2w", "2nb", "2n-l", 
                     "3n-l", "4n-l", "5n-l", "6n-l") ~ 2,
        event %in% c("3", "3w", "3nb") ~ 3,
        event %in% c("4", "4w", "4nb") ~ 4,
        event %in% c("5", "5w", "5nb") ~ 5,
        event %in% c("6", "6w", "6nb") ~ 6,
        event %in% c("7", "7w", "7nb") ~ 7,
        event %in% c("8", "8w", "8nb") ~ 7,
        TRUE ~ -9
      ),
      wickets = case_when(
        grepl("W", event) ~ 1,
        TRUE ~ 0
      ),
      extras = case_when(
        grepl("nb", event) ~ 2,
        event %in% c("•", "W", "1", "2", "3", "4", "5", "6") ~ 0,
        event %in% c("1b", "1lb", "1w") ~ 1,
        event %in% c("2b", "2lb", "2w", "2n-l") ~ 2,
        event %in% c("3b", "3lb", "3w", "3n-l") ~ 3,
        event %in% c("4b", "4lb", "4w", "4n-l") ~ 4,
        event %in% c("5b", "5lb", "5w", "5n-l") ~ 5,
        event %in% c("6b", "6lb", "6w", "6n-l") ~ 6,
        TRUE ~ -9
      )) %>%
      group_by(innings) %>%
      mutate(row = seq(1:n()))
    
    
    partnerships <- match %>%
      group_by(innings, batsman) %>%
      summarise(start_row = min(row),
                end_row = max(row)) %>%
      group_by(innings) %>% 
      arrange(innings, start_row) %>%
      mutate(end_row2 = cummax(end_row)) %>%
      mutate(start_row = ifelse(start_row > lag(end_row) & !is.na(lag(end_row)), lag(end_row) + 1, start_row),
             start_row = ifelse(start_row > lag(end_row2, 2) & !is.na(lag(end_row2, 2)), lag(end_row2, 2) + 1, start_row)) %>%
      arrange(innings, desc(end_row)) %>%
      mutate(end_row = ifelse(seq(1:n()) == 2, lag(end_row),end_row)) %>%
      arrange(innings, start_row) %>%
      mutate(start_row = ifelse(seq(1:n()) == 2, lag(start_row),start_row))
    
    partnerships <- partnerships %>%
      left_join(partnerships, by=c("innings")) %>%
      filter(batsman.x != batsman.y & end_row.x >= start_row.y & start_row.x <= start_row.y) %>%
      group_by(innings, batsman.x, batsman.y) %>%
      mutate(start_row = max(start_row.x, start_row.y),
             end_row = min(end_row.x, end_row.y)) %>%
      select(innings, batsman1 = batsman.x, batsman2 = batsman.y, start_row, end_row) %>%
      group_by(innings) %>%
      arrange(innings, start_row) %>%
      filter(seq(1:n()) != 2)
    
    match2 <- match %>%
      left_join(partnerships, by=c("innings")) %>%
      filter(row >= start_row & row <= end_row) %>%
      mutate(non_striker = ifelse(batsman == batsman1, batsman2, batsman1))
  } else {
    summary <- NA
    match2 <- NA
  }
  return(list(summary, match2))
}

# t2 <- t %>%
#   slice(1:3)

test <- mapply(scrapeTheHundred, t$., seq(1:nrow(t)))


#Overall innings scores (runs/wickets)
inningsSummary <- test[[2]] %>%
  group_by(innings) %>%
  summarise(runs = sum(runs),
            wickets = sum(wickets))


#Batting summary by innings (runs, balls, 4s, 6s, strike rate, not-out)
battingSummary <- test[[2]] %>%
  group_by(innings, batsman) %>%
  summarise(Runs = sum(batting_runs),
            Balls = sum(!grepl("w", event)),
            `4s` = sum(batting_runs==4),
            `6s` = sum(batting_runs==6),
            SR = round(Runs/Balls*100, 2),
            notOut = 1 - sum(wickets))

#Now we have the stats for each batsman we want to order them correctly
battingSummary <- test[[2]] %>%
  group_by(innings, batsman) %>%
  summarise(start_row = min(row)) %>%
  arrange(innings, start_row) %>%
  select(innings, batsman) %>%
  left_join(battingSummary, by=c("innings", "batsman"))


#Bowling summary by innings (balls, dots, runs, wickets, economy, 4s,6s, wides, no balls)
bowlingSummary <- test[[2]] %>%
  group_by(innings, bowler) %>%
  summarise(Balls = sum(!grepl("w", event) & !grepl("nb", event) & !grepl("n-l", event)),
            Dots = sum(runs == 0),
            Runs = sum(bowling_runs),
            # Overs = paste0(floor(Balls/6), ".", Balls - floor(Balls/6)*6),
            # Maidens = floor(sum(over_runs == 0)/6),
            Wickets = sum(grepl("W", event)),
            RPB = round(Runs / Balls, 2),
            `4s` = sum(batting_runs == 4),
            `6s` = sum(batting_runs == 6),
            Wides = sum(grepl("w", event)),
            NoBalls = sum(grepl("nb", event)))


#Now we have the stats for each bowler we want to order them correctly
bowlingSummary <- test[[2]] %>%
  group_by(innings, bowler) %>%
  summarise(start_row = min(row)) %>%
  arrange(innings, start_row) %>%
  select(innings, bowler) %>%
  left_join(bowlingSummary, by=c("innings", "bowler"))

remDr$close()
