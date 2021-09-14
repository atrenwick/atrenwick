library(tidyr) ; library(lubridate) ; library(rjson) ; library(jsonlite) ; library(tibble) ; library(beepr); library(tm) ; library(textclean) ; library(dplyr) ; library(stringr)
theCount <- 0
FileNames <- ""
TweetCount <- 0
Step5 <- ""
ImportDay <- as.integer(format(Sys.Date(), "%d")) -1 ### this number = day number in json folder to open
if ((ImportDay <10)){
  ImportDay <- paste(0,ImportDay, sep = "")}
RealMonth <- as.integer(format(Sys.Date(), "%m"))
if ((RealMonth <10)){
  RealMonth <- paste(0,RealMonth, sep = "")}

myFolder <- "" ## add path here before starting…

thisPath <- paste("/TwitterStream/2021-", RealMonth, "-", ImportDay, "-FR-json/", sep="")
files <- list.files(path=thisPath, pattern="*.json", full.names=TRUE, recursive=FALSE)
{
  for (i in 1:length(files)){
    Step1 <- as_tibble(stream_in(file(files[i]))) ##Stream in
    col_order <- c("created_at", "id", "text", "lang")
    Step2 <- Step1[, col_order]
    Step2 <-Step2[Step2$lang == "fr", ] ##Filter to leave only FR tweets
    try((Step2$new <- parse_date_time(Step2$created_at, c("a b d  HMS z Y"))), outFile=Step2$new)
    ## divide up according to results of this
    Step3A <- Step2[which(!is.na(Step2$new) ),]
    Step3B <- Step2[which(is.na(Step2$new) ),]
    if (dim(Step2)[1] != dim(Step3A)[1] + dim(Step3B)[1]) {beep(2) ; break}
    try((Step3B$new <- ymd_hms(Step3B$created_at)), outFile=Step3B$new)
    zzParsed <- rbind(Step3A, Step3B)
    Step4 <- separate(zzParsed, new, c("Date", "Time"), sep=" ")
    Step4 <- separate(Step4, Date, c("YYYY", "MM", "DD"), sep="-")
    Step4 <- separate(Step4, Time, c("hh", "mm", "ss"), sep=":")
    Step4 <- Step4[-c(1)]
    Step5 <- rbind(Step5, Step4)
    cat("Completed", i, "of", length(files), "Name", files[i], "\n")
    thisCount <- dim(Step1)[1]
    TweetCount <- rbind(thisCount, TweetCount)
    theCount <- thisCount + theCount
    cat("+", thisCount, "->", theCount, "\n")
  }
  Step5 <- unique.data.frame(Step5)
  Step6 <- Step5[!duplicated(Step5$id),]
  
  Step6$text <- gsub("http.+", "", Step6$text)
  Step6$text <- gsub("#.+? ", "", Step6$text)
  Step6$text <- gsub("@.+? ", "", Step6$text)
  Step6$text <- gsub("\'|’", "\' ", Step6$text) ## space elisions
  Step6$text <- gsub("_", "", Step6$text) ## remove double spaces
  Step6$text <- gsub("-|–", "-", Step6$text) ## remove double spaces
  Step6$text <- gsub("[\"“‘«]", "\"", Step6$text) # replace opening quote variations with std double "
  Step6$text <- gsub("[”’»]", "\"", Step6$text) # idem for closing
  Step6$text <- gsub("[0-9]+", "", Step6$text)
  Step6$text <- gsub(",", "", Step6$text)
  Step6$text <- gsub("\\.", "", Step6$text)
  Step6$text <- gsub("tco.*ing.", "", Step6$text)
  Step6$text <- gsub("(f|ht)t(ps|p):.*(\\s|$)", "", Step6$text, ignore.case = TRUE)
  Step6$text <- gsub("@[a-z,0-9,:punct:,_]*?\\s", "", Step6$text, ignore.case = TRUE)
  Step6$text <- add_comma_space(Step6$text)
  Step6$text <- replace_non_ascii(Step6$text)
  Step6$text <- replace_hash(Step6$text)
  Step6$text <- replace_html(Step6$text)
  Step6$text <- replace_emoji(Step6$text)
  Step6$text <- replace_white(Step6$text)
  
  beep(1)}
TweetStreamName <- paste("/TwitterStream/2021-", RealMonth, "-", ImportDay, "-FR-TweetStreamV2.csv", sep = "")
KeyListName <- paste("/TwitterStream/2021-", RealMonth, "-", ImportDay, "-Keylist.csv", sep = "")
write.csv((Step6[c(1,4,5,6,7,8,9)]), KeyListName)
write.csv((Step6[c(1,2)]), TweetStreamName)

