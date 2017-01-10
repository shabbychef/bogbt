set.seed(7)

library(zoo); library(xts); library(quantmod); library(ggplot2); library(gridExtra)
library(scales);  library(tseries); library(highfrequency); library(PerformanceAnalytics); 
library(xlsxjars); library(xlsx); library(fBasics); library(quantmod); library(beepr)
library(robustbase); library(rjson); library(stringr); library(tcltk); library(gsubfn)


setwd("./Google Drive/ME/Microeconometrics/Replication/RawData/UnzippedRenamedTweetsFromFebruary2008ToOctober2008")  ## Tunnel in to folder 

## setwd("..") ## Tunnel out of folder 
## getwd()

Files.February2008toOctober2008 = c('february.json', 'march.json', 'april.json', 'may.json',
                                    'june.json', 'july.json', 'august.json', 'september.json','october.json')

# remove output csv file if script has not finished running
## file.remove("gnip_tweets.csv") 

# read every json file from the time frame of interest

for(i in Files.February2008toOctober2008){
  mat = NULL
  # read each file, Tweet, and skip blank Tweets
  tweets = readLines(i); for(j in tweets){ if(j == "") {next}
    
    
    tweet = unlist(fromJSON(j))
    
    # IRI for the twitter user; isolate the numeric part and skip blank id's.
    id = tweet['actor.id']; if(is.na(id)){next}; id = sub(".*?com:(.*?)","\\1", id); names(id) = NULL
    
    # unique IRI for the tweet
    tweet_id = tweet['id']; tweet_id = sub(".*?com,2005:(.*?)","\\1", tweet_id); names(tweet_id) = NULL
    
    # user defined location
    location = tweet['actor.location.displayName']; names(location) = NULL
    location = gsubfn(".", list(","=" ", "\n"=" ", "\t"=" ","\r"=" ","\"" = "", "'" = ""), location)
    
    # date and time
    time_stamp = tweet['postedTime']; time_stamp = as.POSIXct(time_stamp, tz="UTC", format="%Y-%m-%dT%H:%M:%S")
    
    # EST time
    attributes(time_stamp)$tzone="EST" 
    # time_stamp = as.Date(time_stamp, tz="EST", format="%Y-%m-%dT%H:%M:%S")
    time_stamp = as.character(time_stamp)
    
    # text 
    text = tweet['body']; names(text)=NULL
    text = gsubfn(".", list(","=" ", "\n"=" ", "\t"=" ",
                           "\r"=" ","\"" = ""), text)  
    #text = tolower(text);  text = str_pad(text, 140, "right")    
    
    mat = rbind(mat, c(text, tweet_id, id, location, time_stamp))
  }
  
  colnames(mat) = c("text", "tweetid", "userid", "location",
                    "timestamp")
  
  # print json file's tweets 
  out = gsub(".json",".csv",i)
  setwd("../.."); setwd("./ProcessedData/CleanedUnzippedRenamedTweetsFromFebruary2008toOctober2008/")  ## Tunnel in to folder 
  write.csv(mat, out, quote = FALSE, row.names = FALSE)
  setwd(".."); setwd("./RawData/UnzippedRenamedTweetsFromFebruary2008ToOctober2008")
}

setwd(".."); setwd("./UnzippedTweetsFromJanuary2007toFebruary2008")

Files.January2007toFebruary2008 = list.files()

for(i in Files.January2007toFebruary2008){
        mat = NULL
        # read each file, Tweet, and skip blank Tweets
        tweets = readLines(i); for(j in tweets){ if(j == "") {next}
                
                
                tweet = unlist(fromJSON(j))
                
                # IRI for the twitter user; isolate the numeric part and skip blank id's.
                id = tweet['actor.id']; if(is.na(id)){next}; id = sub(".*?com:(.*?)","\\1", id); names(id) = NULL
                
                # unique IRI for the tweet
                tweet_id = tweet['id']; tweet_id = sub(".*?com,2005:(.*?)","\\1", tweet_id); names(tweet_id) = NULL
                
                # user defined location
                location = tweet['actor.location.displayName']; names(location) = NULL
                location = gsubfn(".", list(","=" ", "\n"=" ", "\t"=" ","\r"=" ","\"" = "", "'" = ""), location)
                
                # date and time
                time_stamp = tweet['postedTime']; time_stamp = as.POSIXct(time_stamp, tz="UTC", format="%Y-%m-%dT%H:%M:%S")
                
                # EST time
                attributes(time_stamp)$tzone="EST" 
                # time_stamp = as.Date(time_stamp, tz="EST", format="%Y-%m-%dT%H:%M:%S")
                time_stamp = as.character(time_stamp)
                
                # text 
                text = tweet['body']; names(text)=NULL
                text = gsubfn(".", list(","=" ", "\n"=" ", "\t"=" ",
                                        "\r"=" ","\"" = ""), text)  
                #text = tolower(text);  text = str_pad(text, 140, "right")    
                
                mat = rbind(mat, c(text, tweet_id, id, location, time_stamp))
        }
        
        colnames(mat) = c("text", "tweetid", "userid", "location",
                          "timestamp")
        
        # print json file's tweets 
        out = gsub(".json.gz",".csv",i)
        setwd("../.."); setwd("./ProcessedData/CleanedUnzippedTweetsFromJanuary2007toFebruary2008")  ## Tunnel in to folder 
        write.csv(mat, out, quote = FALSE, row.names = FALSE)
        setwd("../.."); setwd("./RawData/UnzippedTweetsFromJanuary2007toFebruary2008")
}

setwd(".."); setwd("./RawData/UnzippedRenamedTweetsFromNovember2008toDecember2008")

Files.November2008toDecember2008 = list.files()

for(i in Files.November2008toDecember2008) {
        tweets = readLines(i)
        
        for(j in tweets) { 
          
          if(j == ""){next} # blank lines
          
          tweet=unlist(fromJSON(j))
          
          # user id
          userid = tweet['user.id_str']; if(is.na(userid)){next} # non-tweets
          names(userid) = NULL
          
          # user defined location
          location = tweet['user.location']; names(location)=NULL
          location = gsubfn(".", list(","=" ", "\n"=" ", "\t"=" ",
                                      "\r"=" ","\"" = "", "'" = ""), location)
          
          # time stamp
          time_stamp = tweet['created_at']; 
          time_stamp = as.POSIXct(time_stamp, format = "%a %b %d %H:%M:%S %z %Y")
          attributes(time_stamp)$tzone="EST"
          time_stamp = as.character(time_stamp) 
          
          # text
          text = tweet['text']; names(text)=NULL; 
          text = gsubfn(".", list(","=" ", "\n"=" ", "\t"=" ",
                                  "\r"=" ","\"" = "", "'" = ""), text)    
          text = tolower(text);  text = str_pad(text, 140, "right")
          
          # output to file; first line of file November2008toDecember2008.csv is:
          # "text, userid, location, timestamp" with one line break afterwards
          setwd("../.."); setwd("./ProcessedData/CleanedUnzippedRenamedTweetsFromNovember2008toDecember2008")
          write(c(text, userid, location, time_stamp), file="November2008toDecember2008.csv",
                ncolumns=4, append=TRUE, sep=",")
          setwd("../.."); setwd("./RawData/UnzippedRenamedTweetsFromNovember2008toDecember2008")
          }
  }