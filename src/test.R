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
p <- as.integer(4010)
driver <- rsDriver(browser=c("chrome"), port = p, geckover = NULL, chromever = "92.0.4515.43")
remDr <- driver[["client"]]
# remDr$open()


url <- "https://www.espncricinfo.com/series/the-hundred-women-s-competition-2021-1252659/match-results"

#Read the raw html data
raw <- try(xml2::read_html(url), silent = TRUE)
if ("try-error" %in% class(raw)) 
  stop("Error in URL")

#Extract tables
t <- raw %>% 
  rvest::html_nodes("a") %>% 
  html_attr("href") %>%
  data_frame(.) %>%
  filter(grepl("full-scorecard", .) & grepl("the-hundred-women", .) & !duplicated(.))

x <- t %>% slice(12) %>% as.character(.)
index <- 1


scrapeTheHundred <- function(x, index) {
  print(paste0("Index: ", index))
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
  #
  #This assumes that both innings had at least one ball bowled.
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
      
      Sys.sleep(1)
      
      #Find the element referring to the innings we want
      #We need to use the xpath for this as it has no other identifying tags
      inningsSelection <- remDr$findElement(using = 'xpath', 
                                            paste0('//*[@id="main-container"]/div[1]/div/div/div[2]/div[2]/div[1]/div[1]/div/div/div/ul/li[', inn, ']'))
      
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
        
        Sys.sleep(5)
        
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
        if (pass > 10) {
          print(pass)
          print(earliest)
          print(prev)
          if(earliest == prev) {
            message("Refreshing...")
            #If the page stops adding new deliveries then refresh and reselect the innings
            remDr$refresh()
            Sys.sleep(5)
            inningsDropdown <- remDr$findElement(using = 'xpath',
                                                 '//*[@id="main-container"]/div[1]/div/div/div[2]/div[2]/div[1]/div[1]/div')
            
            inningsDropdown$clickElement()
            
            Sys.sleep(2)
            inningsSelection <- remDr$findElement(using = 'xpath', 
                                                  paste0('//*[@id="main-container"]/div[1]/div/div/div[2]/div[2]/div[1]/div[1]/div/div/div/ul/li[', inn, ']'))
            
            
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
      
      #We can also extract the events that occurred on each ball
      #This only captures the 'main' event (i.e. wickets take priority over runs)
      events <- pageSource %>%
        rvest::html_nodes(".match-comment-run-container") %>%
        rvest::html_text() %>%
        dplyr::data_frame(event = .) 
      
      #We can extract the short descrption for each delivery
      shortDesc <- pageSource %>%
        rvest::html_nodes(".match-comment-short-text") %>%
        rvest::html_text() %>%
        dplyr::data_frame(shortDesc = .)
      
      shortRuns <- pageSource %>%
        rvest::html_nodes(".comment-short-run") %>%
        rvest::html_text() %>%
        dplyr::data_frame(shortRuns = .)
      
      #Finally a full text description of the outcome of the delivery
      fullDesc <- pageSource %>%
        rvest::html_nodes(".match-comment-wrapper") %>%
        rvest::html_text() %>%
        dplyr::data_frame(fullDesc = .)
      
      #Also extract wicket descriptions so that we can accurately determine which batsman was out
      #In run-outs we don't know if it was the striker or non-striker
      wicketDesc <- pageSource %>%
        rvest::html_nodes(".match-comment-wicket-no-icon") %>%
        rvest::html_text() %>%
        dplyr::data_frame(wicketDesc = .) 
      
      if(nrow(wicketDesc) > 0) { 
        wicketDesc <- wicketDesc %>%
        mutate(wicket = seq(1:n()),
               innings = inn)
        
        #Our innings data is the combination of these four dataframes
        innings <- cbind(balls, events, shortDesc, shortRuns, fullDesc) %>%
          dplyr::mutate(ball = as.numeric(ball)) %>%
          dplyr::arrange(ball) %>%
          dplyr::mutate(innings = inn,
                        w = ifelse(event == "W", 1, 0),
                        wicket = ifelse(event == "W", sum(w) - cumsum(w) + 1, 0)) %>%
          dplyr::select(-w) %>%
          dplyr::left_join(wicketDesc, by=c("innings", "wicket"))
      } else {
        #Our innings data is the combination of these four dataframes
        innings <- cbind(balls, events, shortDesc, shortRuns, fullDesc) %>%
          dplyr::mutate(ball = as.numeric(ball)) %>%
          dplyr::arrange(ball) %>%
          dplyr::mutate(innings = inn,
                        wicket= 0,
                        wicketDesc = NA) 
      }
      
      #Bind to the match data
      match <- rbind(match, innings)
    }
    
    match <- match %>%
      group_by(ball, fullDesc) %>%
      mutate(fullDesc = gsub(shortDesc, paste0(shortDesc, "\n"), fullDesc),
             bowler = gsub("([A-z \\-]+) to.*", "\\1", shortDesc),
             batsman = gsub("([A-z \\-]+),.*", "\\1", sub("[A-z \\-]+ to ([A-z ,]+)", "\\1", shortDesc)),
             runsCheck = case_when(
               grepl("no run", shortDesc) ~ 0,
               grepl("1", shortDesc) ~ 1,
               grepl("2", shortDesc) ~ 2,
               grepl("3", shortDesc) ~ 3,
               grepl("FOUR", shortDesc) ~ 4,
               grepl("SIX", shortDesc) ~ 6,
               TRUE ~ 0),
             extrasCheck = case_when(
               grepl("wide|no ball", shortRuns) ~ 1,
               TRUE ~ 0
             ),
             ball = as.numeric(ball)) %>%
      arrange(innings, ball, desc(extrasCheck)) %>%
      mutate(runs = case_when(
        event %in% c("•") ~ 0,
        event %in% c("W") ~ runsCheck,
        event %in% c("1", "1b", "1lb", "1nb", "1w") ~ 1,
        event %in% c("2", "2b", "2lb", "2nb", "2n-l", "2w", "2n-b") ~ 2,
        event %in% c("3", "3b", "3lb", "3nb", "3n-l", "3w", "3n-b") ~ 3,
        event %in% c("4", "4b", "4lb", "4nb", "4n-l", "4w", "4n-b") ~ 4,
        event %in% c("5", "5b", "5lb", "5nb", "5n-l", "5w", "5n-b") ~ 5,
        event %in% c("6", "6b", "6lb", "6nb", "6n-l", "6w", "6n-b") ~ 6,
        event %in% c("7", "7b", "7lb", "7nb", "7n-l", "7w", "7n-b") ~ 7,
        event %in% c("8", "8b", "8lb", "8nb", "8n-l", "8w", "8n-b") ~ 8,
        TRUE ~ -9
      ),
      batting_runs = case_when(
        event %in% c("•", #"W",
                     "1b", "1lb", "1nb", "1w",
                     "2b", "2lb", "2nb", "2n-l", "2n-b", "2w",
                     "3b", "3lb", "3w", "3n-l", "3n-b", 
                     "4b", "4lb", "4w", "4n-l", "4n-b",
                     "5b", "5lb", "5w", "5n-l", "5n-b",
                     "6b", "6lb", "6w", "6n-l", "6n-b") ~ 0,
        event %in% c("W") ~ runsCheck,
        event %in% c("1", "3nb") ~ 1,
        event %in% c("2", "4nb") ~ 2,
        event %in% c("3", "5nb") ~ 3,
        event %in% c("4", "6nb") ~ 4,
        event %in% c("5", "7nb") ~ 5,
        event %in% c("6", "8nb") ~ 6,
        TRUE ~ -9
      ),
      bowling_runs = case_when(
        event %in% c("•", #"W", 
                     "1b", "1lb",
                     "2b", "2lb", 
                     "3b", "3lb",
                     "4b", "4lb",
                     "5b", "5lb",
                     "6b", "6lb") ~ 0,
        event %in% c("W") ~ runsCheck,
        event %in% c("1", "1nb", "1w") ~ 1,
        event %in% c("2", "2w", "2nb", "2w", "2n-l", "2n-b", 
                     "3n-l", "4n-l", "5n-l", "6n-l") ~ 2,
        event %in% c("3", "3w", "3nb", "3w", "3n-b") ~ 3,
        event %in% c("4", "4w", "4nb", "4w", "4n-b") ~ 4,
        event %in% c("5", "5w", "5nb", "5w", "5n-b") ~ 5,
        event %in% c("6", "6w", "6nb", "6w", "6n-b") ~ 6,
        event %in% c("7", "7w", "7nb", "7w", "7n-b") ~ 7,
        event %in% c("8", "8w", "8nb", "8w", "8n-b") ~ 8,
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
        event %in% c("2b", "2lb", "2w", "2n-l", "2n-b") ~ 2,
        event %in% c("3b", "3lb", "3w", "3n-l", "3n-b") ~ 3,
        event %in% c("4b", "4lb", "4w", "4n-l", "4n-b") ~ 4,
        event %in% c("5b", "5lb", "5w", "5n-l", "5n-b") ~ 5,
        event %in% c("6b", "6lb", "6w", "6n-l", "6n-b") ~ 6,
        TRUE ~ -9
      )) %>%
      group_by(innings) %>%
      mutate(row = seq(1:n())) 
      
    
    partnerships <- match %>%
      group_by(innings, batsman) %>%
      summarise(start_row = min(row),
                end_row = max(row)) %>%
      left_join(match %>% select(innings, batsman, wickets, row), by=c("innings", "batsman", "end_row"="row")) %>%
      group_by(innings) %>% 
      arrange(innings, start_row) %>%
      mutate(end_row2 = cummax(end_row),
             wickets2 = ifelse(cummax(end_row)==end_row, wickets, 0)) %>%
      #Need some logic in here to detect when a wicket falls that is of the non-striker.
      #Currently this logic indirectly assumes that the last ball you faced is when you got out!
      mutate(start_row = ifelse(start_row > lag(end_row) & !is.na(lag(end_row)) & lag(wickets) == 1, lag(end_row) + 1, start_row),
             start_row = ifelse(start_row > lag(end_row2, 2) & !is.na(lag(end_row2, 2)) & lag(wickets2, 2) == 1, lag(end_row2, 2) + 1, start_row)) %>%
      arrange(innings, desc(end_row)) %>%
      mutate(end_row = ifelse(seq(1:n()) == 2, lag(end_row), end_row),
             end_row = ifelse(end_row < lag(start_row) - 1 & !is.na(lag(start_row)), lag(start_row) - 1, end_row)) %>%
      arrange(innings, start_row) %>%
      mutate(start_row = ifelse(seq(1:n()) == 2, lag(start_row),start_row))
    
    #New logic for when there is a gap between end_row for a batsman and start_row for their replacement
    
    w <- match %>%
      filter(event == "W" & grepl("run out", wicketDesc)) %>%
      select(innings, row)
    
    partnerships2 <- merge(partnerships, partnerships %>% select(innings, start_row_comp=start_row, end_row_comp=end_row), by="innings") %>%
      mutate(diff = start_row - end_row_comp) %>%
      group_by(innings) %>%
      filter(diff > 1 & end_row!=max(end_row) | wickets == 0 & start_row==start_row_comp & end_row==end_row_comp & end_row!=max(end_row)) %>%
      arrange(innings, batsman, diff) %>%
      group_by(innings, batsman) %>%
      slice(1) %>%
      ungroup() %>%
      left_join(w, by="innings") %>%
      group_by(innings, batsman) %>%
      mutate(diff2 = row - start_row,
             test1 = cumsum(end_row < row),
             test2 = cumsum(start_row > row)) %>%
      group_by(row) %>%
      filter(!min(abs(diff2)) <= 1 & (abs(diff2)==min(abs(diff2)) | diff2 > 0)) %>%
      ungroup() %>%
      filter(test1 == 1 | test2 == max(test2)) %>%
      slice(1) %>%
      mutate(start_row = ifelse(start_row > row, row + 1, start_row),
             end_row = ifelse(end_row < row, row, end_row)) %>%
      select(names(partnerships))
    
    partnerships <- rbind(partnerships %>% anti_join(partnerships2, by=c("innings", "batsman")), partnerships2) %>%
      arrange(innings, start_row)
    
    partnerships <- partnerships %>%
      left_join(partnerships, by=c("innings")) %>%
      filter(batsman.x != batsman.y & end_row.x >= start_row.y & start_row.x <= start_row.y) %>%
      group_by(innings, batsman.x, batsman.y) %>%
      mutate(start_row = max(start_row.x, start_row.y, na.rm=T),
             end_row = min(end_row.x, end_row.y, na.rm=T)) %>%
      select(innings, batsman1 = batsman.x, batsman2 = batsman.y, start_row, end_row) %>%
      group_by(innings) %>%
      arrange(innings, start_row) %>%
      filter(seq(1:n()) != 2)
    
    match2 <- match %>%
      left_join(partnerships, by=c("innings")) %>%
      filter(row >= start_row & row <= end_row) %>%
      mutate(non_striker = ifelse(batsman == batsman1, batsman2, batsman1)) %>%
      rowwise() %>%
      mutate(dismissed = case_when(
        is.na(wicketDesc) ~ "",
        grepl(batsman1, wicketDesc) ~ batsman1, 
        grepl(batsman2, wicketDesc) ~ batsman2,
        TRUE ~ "Check"))
  } else {
    summary <- NA
    match2 <- NA
  }
  return(list(summary, match2))
}

# test <- scrapeTheHundred(x, index)

test <- mapply(scrapeTheHundred, t$., seq(1:nrow(t)))

saveRDS(test, "data/All Balls (Women).RDS")

index <- NULL
for (x in c(seq(2, length(test), 2))) {
  if(!is.na(test[[x]])) {
    index <- c(index, x)
    test[[x]]$match <- x/2
  }
}


allBalls <- rbindlist(test[index])  



Scorecard <- test[[23]]
Check <- test[[24]] %>% ungroup()

partnerships <- Check %>%
  group_by(innings, batsman) %>%
  summarise(start_row = min(row),
            end_row = max(row)) %>%
  left_join(Check %>% select(innings, batsman, wickets, row), by=c("innings", "batsman", "end_row"="row")) %>%
  group_by(innings) %>% 
  arrange(innings, start_row) %>%
  mutate(end_row2 = cummax(end_row),
         wickets2 = ifelse(cummax(end_row)==end_row, wickets, 0)) %>%
  #Need some logic in here to detect when a wicket falls that is of the non-striker.
  #Currently this logic indirectly assumes that the last ball you faced is when you got out!
  mutate(start_row = ifelse(start_row > lag(end_row) & !is.na(lag(end_row)) & lag(wickets) == 1, lag(end_row) + 1, start_row),
         start_row = ifelse(start_row > lag(end_row2, 2) & !is.na(lag(end_row2, 2)) & lag(wickets2, 2) == 1, lag(end_row2, 2) + 1, start_row)) %>%
  arrange(innings, desc(end_row)) %>%
  mutate(end_row = ifelse(seq(1:n()) == 2, lag(end_row), end_row),
         end_row = ifelse(end_row < lag(start_row) - 1, lag(start_row) - 1, end_row)) %>%
  arrange(innings, start_row) %>%
  mutate(start_row = ifelse(seq(1:n()) == 2, lag(start_row),start_row))


wickets <- Check %>%
  filter(event == "W" & grepl("run out", wicketDesc)) %>%
  select(innings, row)

partnerships2 <- merge(partnerships, partnerships %>% select(innings, start_row_comp=start_row, end_row_comp=end_row), by="innings") %>%
  mutate(diff = start_row - end_row_comp) %>%
  filter(diff > 1 & !is.na(end_row) | wickets == 0 & start_row==start_row_comp & end_row==end_row_comp) %>%
  arrange(innings, batsman, diff) %>%
  group_by(innings, batsman) %>%
  slice(1) %>%
  ungroup() %>%
  left_join(wickets, by="innings") %>%
  mutate(test1 = cumsum(end_row < row),
         test2 = cumsum(start_row > row)) %>%
  filter(test1 == 1 | test2 == max(test2)) %>%
  group_by(innings, batsman) %>%
  slice(1) %>%
  mutate(start_row = ifelse(start_row > row, row + 1, start_row),
         end_row = ifelse(end_row < row, row, end_row)) %>%
  select(names(partnerships))

partnerships <- rbind(partnerships %>% anti_join(partnerships2, by=c("innings", "batsman")), partnerships2) %>%
  arrange(innings, start_row)

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



match2 <- Check %>%
  select(-c("start_row", "end_row", "batsman1", "batsman2", "non_striker", "dismissed")) %>%
  left_join(partnerships2, by=c("innings")) %>%
  filter(row >= start_row & row <= end_row) %>%
  mutate(non_striker = ifelse(batsman == batsman1, batsman2, batsman1)) %>%
  group_by(innings, ball) %>%
  mutate(dismissed = case_when(
    is.na(wicketDesc) ~ "",
    grepl(batsman1, wicketDesc) ~ batsman1, 
    TRUE ~ batsman2))

grepl(Cleary$batsman2, Cleary$wicketDesc)

testRunOut <- allBalls %>%
  filter(dismissed != batsman & dismissed != "")

summaryTheHundred <- function(x, type=c("batting", "bowling", "innings")) {
  if (!is.na(x)) {
    if (type == "batting") {
      #Batting summary by innings (runs, balls, 4s, 6s, strike rate, not-out)
      summary <- x %>%
        group_by(innings, batsman) %>%
        summarise(Runs = sum(batting_runs),
                  Balls = sum(!grepl("w", event)),
                  `4s` = sum(batting_runs==4),
                  `6s` = sum(batting_runs==6),
                  SR = round(Runs/Balls*100, 2))
      
      wickets <- x %>%
        group_by(innings, dismissed) %>%
        summarise() %>%
        mutate(out = 1)
      
      summary <- summary %>%
        left_join(wickets, by=c("innings", "batsman"="dismissed")) %>%
        mutate(out = ifelse(is.na(out), 0, 1),
               NotOut = 1 - out) %>%
        select(-out)
      
      #Now we have the stats for each batsman we want to order them correctly
      summary <- x %>%
        group_by(innings, batsman) %>%
        summarise(start_row = min(row)) %>%
        arrange(innings, start_row) %>%
        select(innings, batsman) %>%
        left_join(summary, by=c("innings", "batsman"))
    } else if (type == "bowling") {
      #Bowling summary by innings (balls, dots, runs, wickets, economy, 4s,6s, wides, no balls)
      summary <- x %>%
        group_by(innings, bowler) %>%
        summarise(Balls = sum(!grepl("w", event) & !grepl("nb", event) & !grepl("n-l", event)),
                  Dots = sum(runs == 0),
                  Runs = sum(bowling_runs),
                  Wickets = sum(grepl("W", event)),
                  RPB = round(Runs / Balls, 2),
                  `4s` = sum(batting_runs == 4),
                  `6s` = sum(batting_runs == 6),
                  Wides = sum(grepl("w", event)),
                  NoBalls = sum(grepl("nb", event)))
      
      
      #Now we have the stats for each bowler we want to order them correctly
      summary <- x %>%
        group_by(innings, bowler) %>%
        summarise(start_row = min(row)) %>%
        arrange(innings, start_row) %>%
        select(innings, bowler) %>%
        left_join(summary, by=c("innings", "bowler"))
    } else {
      #Overall innings scores (runs/wickets)
      summary <- x %>%
        group_by(innings) %>%
        summarise(runs = sum(runs),
                  wickets = sum(wickets))
    }
  } else {
    summary <- NA
  }
  return(summary)
}

test2 <- summaryTheHundred(test[[24]] %>% ungroup(), "batting")


