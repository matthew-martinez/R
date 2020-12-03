library(rvest)
library(httr)
library(stringr)
library(tidyverse)

############# EVENT RECORDS ############

# initializing some empty objects for the loop
# counter can be set for where to start and where to end w/ sherdog URL
counter <- c(1:500)
i <- 0
fullRecords <- data.frame()[1:8,]

startTime <- Sys.time()

# loop through as many interations as the above counter specifies
for (i in counter) {
  
  # creating the sherdog URL
  site <- NULL
  sitePaste <- paste("https://www.sherdog.com/events/x-x-", (counter[i]), sep="")
  print(sitePaste)
  
  # checking to see if URL 404
  urlCheck <- url_ok(sitePaste)
  
  # If URL is 404, then skip scraping
  if (urlCheck == TRUE){
  
    site <- html_session(sitePaste)
    
    # reading in date
    eventDate <- site %>% html_nodes(".date") %>% html_text()
    
    # reading in main event fighters and cleaning
    fightDivLeft <- site %>% html_nodes(".fighter.left_side") %>% html_text(trim=TRUE)
    fightDivRight <- site %>% html_nodes(".fighter.right_side") %>% html_text(trim=TRUE)
    
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
    
    # making some additional columns so that I can rbind later + reorganizing and string cleanup
    mainEvent$Result <- "Win"
    mainEvent$Fighter <- mainEventFighter
    mainEvent$Opponent <- mainEventOpponent
    
    mainEvent <- mainEvent %>%
      select(X1, Fighter, Result, Opponent, X2, X4, X5, Date)
    
    colnames(mainEvent) <- c("Match", "Fighter", "Result", "Opponent", "Method", "Round", "Time", "Date")
    
    mainEvent$Match <- str_sub(mainEvent$Match, start=7)
    mainEvent$Round <- str_sub( mainEvent$Round, start=7)
    mainEvent$Time <- str_sub(mainEvent$Time, start=6)
    
    # grabbing all tables from the website
    tables <- tryCatch(html_nodes(site, "table") %>% .[2], error=function(e){NA})
    if (is.na(tables) == FALSE) {
      # reading table 1 (.[1]) and turning it into a list of data frames
      tablesParsed <- site %>%
        html_nodes("table") %>%
        .[2] %>%
        html_table(fill = TRUE)
      
      # selecting only the first element of the list of data frames and creating an individual fight record
      fightRecord <- tablesParsed[[1]]
      fightRecord$Date <- eventDate[2]
      fightRecord$Result <- "Win"
        
      colnames(fightRecord) <- c("Match", "Fighter", "vs.", "Opponent", "Method", "Round", "Time", "Date", "Result")
      
      fightRecord <- fightRecord %>%
        select(Match, Fighter, Result, Opponent, Method, Round, Time, Date)
      
      fightRecord <- fightRecord[2:nrow(fightRecord),]
      
      fightRecord <- rbind(mainEvent, fightRecord)
      
      fullRecords <- rbind(fullRecords, fightRecord)
    }
  }
}

fullRecords$Date <- as.Date(fullRecords$Date, "%b %d, %Y")
      
endTime <- Sys.time()
endTime - startTime

write.csv(fullRecords, "/home/m/Documents/R/MMA/eventRecordsFirst500.csv")