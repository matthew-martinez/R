library(rvest)
library(httr)
library(stringr)
library(tidyverse)

# This scraper creates a fight database using sherdog's event pages
# Sherdog's event URLs are standardized (x-x-#) and can easily be looped over
# Fights for event URLs that exist are pulled into a df
# and then the loop moves on to test the next URL in the loop

# initializing some empty objects for the loop
i <- 1
fullRecords <- data.frame()
startTime <- Sys.time()
loopCounter <- 0
logDf <- data.frame()

# Random event number error testing
random <- floor(runif(5000, min=1, max=88000))

# loop through as many iterations as the above counter specifies
for (i in c(1,2,3,4,43,3432,347,3424)){
  # creating the sherdog URL
  site <- NULL
  sessionLogger <- NULL
  urlLogger <- NULL
  sitePaste <- paste("https://www.sherdog.com/events/x-x-", (i), sep="")
  print(sitePaste)
  loopCounter = loopCounter+1
  print(loopCounter)
  
  # checking to see if URL 404
  urlCheck <- url_ok(sitePaste)
  
  # If URL is 404, then skip scraping
  if (urlCheck == TRUE){
  
    site <- tryCatch(html_session(sitePaste), error=function(e){NA})
    
    # creating a log to see if any attempts do not become sessions
    sessionLogger <- c(sessionLogger, is.session(site))
    urlLogger <- c(urlLogger, sitePaste)
    logDfTemp <- data.frame(sessionLogger, urlLogger)
    logDf <- rbind(logDf, logDfTemp)
    
    # reading in date
    eventDate <- site %>% html_nodes(".date") %>% html_text()
    event <- site %>% html_nodes(".section_title") %>% html_text(trim=TRUE)
    event <- strsplit(event, "\\\n")
    
    # reading in main event fighters and cleaning
    fightDivLeft <- site %>% html_nodes(".fighter.left_side") %>% html_text(trim=TRUE)
    fightDivRight <- site %>% html_nodes(".fighter.right_side") %>% html_text(trim=TRUE)
    
    if (length(fightDivLeft) != 0){
      mainEventFighter <- strsplit(fightDivLeft, "\\\n")
      mainEventFighter <- mainEventFighter[[1]][1]
      
      mainEventOpponent <-strsplit(fightDivRight, "\\\n")
      mainEventOpponent <- mainEventOpponent[[1]][1]
      
      # reading table 1 (.[1]) and turning it into a list of data frames - stats from main event
      mainEventTablesParsed <- site %>%
        html_nodes("table") %>%
        .[1] %>%
        html_table(fill = TRUE)
    
      # selecting only the first element of the list of data frames and creating an individual fight record
      mainEvent <- mainEventTablesParsed[[1]]
      mainEvent$Date <- eventDate[2]
      
      # haphazard method for obtaining the fighter URLs to use as IDs
      fightDivLeftDIV <- site %>% html_nodes(".fighter.left_side")
      fightDivRightDIV <- site %>% html_nodes(".fighter.right_side")
      
      # where the URLs are located once the html_node is obtained
      mainEventFighterURL <- xml_attrs(xml_child(fightDivLeftDIV[[1]], 1))[["href"]]
      mainEventOpponentURL <- xml_attrs(xml_child(fightDivRightDIV[[1]], 1))[["href"]]
      
      # making some additional columns so that I can rbind later + reorganizing and string cleanup
      mainEvent$Result <- "Win"
      mainEvent$Fighter <- mainEventFighter
      mainEvent$Opponent <- mainEventOpponent
      mainEvent$FighterID <- mainEventFighterURL
      mainEvent$OpponentID <- mainEventOpponentURL
      
      mainEvent <- mainEvent %>%
        select(X1, Fighter, FighterID, Result, Opponent, OpponentID, X2, X4, X5, Date)
      
      colnames(mainEvent) <- c("Match", "Fighter", "FighterID", "Result", "Opponent", "OpponentID","Method", "Round", "Time", "Date")
      
      mainEvent$Match <- str_sub(mainEvent$Match, start=7)
      mainEvent$Round <- str_sub( mainEvent$Round, start=7)
      mainEvent$Time <- str_sub(mainEvent$Time, start=6)
      
      # removing "/fighter/" from URL so we only have a stable ID for each fighter
      mainEvent$FighterID <- str_sub(mainEvent$FighterID, start=10)
      mainEvent$OpponentID  <- str_sub(mainEvent$OpponentID, start=10)
      
      # grabbing all tables from the website
      tables <- tryCatch(html_nodes(site, "table") %>% .[2], error=function(e){NA})
      if (is.na(tables) == FALSE) {
        # reading table 1 (.[1]) and turning it into a list of data frames
        tablesParsed <- site %>%
          html_nodes("table") %>%
          .[2] %>%
          html_table(fill = TRUE)
        
        # obtaining the section of the site with hrefs for fighter URLs
        tableLinksParsed <- site %>%
          html_nodes(".fighter_result_data")
        
        # a loop that takes the fighter URLs from the obtained xml and puts them into a vector
        tableURL = NULL
        j = 1
        for (j in 1:length(tableLinksParsed)){
        tableURL <- c(tableURL, xml_attrs(xml_child(tableLinksParsed[[j]], 1))[["href"]])
        }
        
        # reading every other fighter URL (even and odd) to place into fighter or opponent vectors
        fURL <- tableURL[c(seq(from=1, to=length(tableURL), by=2))]
        oURL <- tableURL[c(seq(from=0, to=length(tableURL), by=2))]
        
        # selecting only the first element of the list of data frames and creating an individual fight record
        fightRecord <- tablesParsed[[1]]
        fightRecord$Date <- eventDate[2]
        fightRecord <- fightRecord %>%
          mutate(Result = if_else(str_detect(X5, "DrawN/A"), "Draw", "Win"))
        
        colnames(fightRecord) <- c("Match", "Fighter", "vs.", "Opponent", "Method", "Round", "Time", "Date", "Result")
        
        fightRecord <- fightRecord[2:nrow(fightRecord),]
  
        # creating columns for fighterID and opponentID using the fURL and oURL vectors
        fightRecord$FighterID <- all_of(fURL)
        fightRecord$OpponentID <- all_of(oURL)
        
        fightRecord <- fightRecord %>%
          select(Match, Fighter, FighterID, Result, Opponent, OpponentID, Method, Round, Time, Date)
        
        fightRecord$FighterID <- str_sub(fightRecord$FighterID, start=10)
        fightRecord$OpponentID  <- str_sub(fightRecord$OpponentID, start=10)
        
        fightRecord$Fighter[fightRecord$Result == "Draw"] <- str_sub(fightRecord$Fighter[fightRecord$Result == "Draw"], end=-5)
        fightRecord$Opponent[fightRecord$Result == "Draw"]  <- str_sub(fightRecord$Opponent[fightRecord$Result == "Draw"], end=-5)
        
        fightRecord$Fighter[fightRecord$Result == "Win"] <- str_sub(fightRecord$Fighter[fightRecord$Result == "Win"], end=-4)
        fightRecord$Opponent[fightRecord$Result == "Win"] <- str_sub(fightRecord$Opponent[fightRecord$Result == "Win"], end=-5)
        
        fightRecord <- rbind(mainEvent, fightRecord)
        fightRecord$Event <- event[[1]][1]
        fightRecord$EventID <- i
        fightRecord$Promotion <- str_split(event[[1]][2], "\\t")[[1]][3]
        #fightRecord$Promotion <- str_sub(fightRecord$Promotion, start=5)
        fightRecord <- fightRecord %>%
          select(Promotion, Event, EventID, Match, Fighter, FighterID, Result, Opponent, OpponentID, Method, Round, Time, Date)
  
        fullRecords <- rbind(fullRecords, fightRecord)
      }
        if (is.na(tables) == TRUE) {
          fightRecord <- mainEvent
          fightRecord$Event <- event[[1]][1]
          fightRecord$EventID <- i
          fightRecord$Promotion <- str_split(event[[1]][2], "\\t")[[1]][3]
          #fightRecord$Promotion <- str_sub(fightRecord$Promotion, start=5)
          fightRecord <- fightRecord %>%
            select(Promotion, Event, EventID, Match, Fighter, FighterID, Result, Opponent, OpponentID, Method, Round, Time, Date)
          
          fullRecords <- rbind(fullRecords, fightRecord)
        }
    }
  }
}

fullRecords$Date <- as.Date(fullRecords$Date, "%b %d, %Y")

endTime <- Sys.time()
endTime - startTime

write.csv(logDf, "/home/m/Documents/R/MMA/urlLog1to5000.csv")
write.csv(fullRecords, "/home/m/Documents/R/MMA/eventRecords1to5000.csv")