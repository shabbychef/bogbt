#It is assumed that you have downloaded the following libraries and their dependencies.

library(proto); library(TTR); library(zoo); library(xts); library(quantmod); 
library(gridExtra); library(scales);  library(tseries); library(highfrequency); 
library(PerformanceAnalytics); library(xlsxjars); library(xlsx); library(fBasics); library(quantmod); 
library(beepr); library(robustbase); library(rjson); library(stringr); library(gsubfn)
library(slam); library(quanteda)

setwd("./Google Drive/Publications/2017/EJW/")  ## Tunnel in to folder 
November2008toDecember2008.CSV = textfile("./ProcessedData/CleanedUnzippedRenamedTweetsFromNovember2008toDecember2008/*.csv", 
                                          textField = 1,
                                          cache=F,
                                          docvarnames = 
                                            c("text", "userid","location", 
                                              "timestamp"))

Corpus.November2008toDecember2008.CSV = corpus(November2008toDecember2008.CSV); rm(November2008toDecember2008.CSV)

February2008toOctober2008.CSV = textfile("./ProcessedData/CleanedUnzippedRenamedTweetsFromFebruary2008toOctober2008/*.csv", 
                                         textField = 1, cache=F,
                                         docvarnames = c("text", "tweetid", "userid", "location", "timestamp"))

Corpus.February2008toOctober2008.CSV = corpus(February2008toOctober2008.CSV); rm(February2008toOctober2008.CSV)

Sys.setlocale("LC_ALL", "C") #needed thsi for Japanese mac mini

July2007toFebruary2008.CSV = textfile("./ProcessedData/CleanedUnzippedTweetsFromJuly2007toFebruary2008/*.csv", 
                                         textField = 1, cache=F,
                                         docvarnames = c("text", "tweetid", "userid", "location", "timestamp"))

Corpus.JulytoFebruary2007.CSV = corpus(July2007toFebruary2008.CSV); rm(July2007toFebruary2008.CSV)

Corpus.July2007toDecember2008 = Corpus.JulytoFebruary2007.CSV + 
  Corpus.February2008toOctober2008.CSV + 
  Corpus.November2008toDecember2008.CSV

rm(Corpus.February2008toOctober2008.CSV, Corpus.JulytoFebruary2007.CSV, Corpus.November2008toDecember2008.CSV); gc()

## MAKING MOOD TIME SERIES


#First, make our dictionaries

composed = readLines("./ProcessedData/wordlists/composed.txt"); anxious = readLines("./ProcessedData/wordlists/anxious.txt")

DATES = as.Date(Corpus.July2007toDecember2008[["timestamp"]])

library(sentimentr)

posnegwt = c(1,-1)
mood.dict = data.frame(c(composed, anxious), 
                       c(rep(posnegwt[1], length(composed)),
                         rep(posnegwt[2], length(anxious))))
mood.dict.key = as_key(mood.dict)

# comp.key = update_key(mood.dict.key, drop = anxious)
# anxi.key = update_key(mood.dict.key, drop = composed)

scores = NULL
counts = NULL

#Start with sophisticated sentiment analysis

for (i in seq(from = 1, to = dim(Corpus.July2007toDecember2008$documents)[1],by = 400000)){
  print(i)
  tweet.scores = sentiment_by(Corpus.July2007toDecember2008$documents$texts[i:(i+399999)], 
                              polarity_dt = mood.dict.key)
  counts = c(counts, tweet.scores$word_count)
  scores = c(scores, tweet.scores$ave_sentiment)
  gc()
}

dated.scores = scores[1:length(DATES)]
dated.counts = counts[1:length(DATES)]

save(Corpus.July2007toDecember2008, file = "Corpus.July2007toDecember2008")

#Next, use unsophisticated raw word count-based "tone" scores
library(SnowballC)

#We make "stemmed dictionaries"
stem.composed = wordStem(composed)
stem.anxious = wordStem(anxious)

#But stemmed dictionaries have duplicates!
rd.stem.composed = stem.composed[!duplicated(stem.composed)]
rd.stem.anxious = stem.anxious[!duplicated(stem.anxious)]


# We clean our Tweet text following the procedure of BMZ
library(tm)

nopunc.twts = removePunctuation(Corpus.July2007toDecember2008$documents$texts)

library(qdap)
negWords = wordStem(unique(removePunctuation(negation.words)));

# This will be our list of removed stop words

Top98Words = Top100Words[-which(Top100Words %in% negWords)]; length(Top98Words) # It's 98 words long

clean.twts = rm_stopwords(nopunc.twts, 
                          stopwords = Top98Words,
                          unlist=TRUE,
                          sep = FALSE)

#We use the Porter stemmer

require(parallel)  # parallel processing
require(tau)       # tokenise function

stem_text = function(text, language = 'porter', mc.cores = 1) {
  # stem each word in a block of text
  stem_string =  function(str, language) {
    str = tokenize(x = str)
    str = wordStem(str, language = language)
    str = paste(str, collapse = "")
    return(str)
  }
  
  # stem each text block in turn
  x = mclapply(X = text, FUN = stem_string, language, mc.cores = mc.cores)
  
  # return stemed text blocks
  return(unlist(x))
}

cleanest.twts = stem_text(clean.twts)

library(stringi)

clean.twts.composed.score = stri_count_regex(cleanest.twts,
                                             paste(rd.stem.composed, 
                                                   collapse = '|'))

clean.twts.anxious.score = stri_count_regex(cleanest.twts,
                                             paste(rd.stem.anxious, 
                                                   collapse = '|'))

#make Tweet tone
clean.tone.num = clean.twts.composed.score - clean.twts.anxious.score
clean.tone.denom = clean.twts.composed.score + clean.twts.anxious.score
subjectivity = clean.tone.denom
clean.tone.denom[clean.tone.denom == 0] = 1 #many 0's in denominator of tweet text
tone = clean.tone.num/clean.tone.denom


# tone.denom = composed.score.raw + anxious.score.raw
# Word count methods applied to "raw" Tweet text may be of interest; results do not differ from those in paper
# The only additional series satisfying the visual selection criterion is the one with stop words and punctuation
# removed, suggesting that data cleaning procedures provide the researcher with significant degrees of freedom for 
# this text data set

# rawtwt.composed.score = stri_count_regex(Corpus.July2007toDecember2008$documents$texts, 
#                                         paste(composed, collapse = '|'))
# rawtwt.anxious.score = stri_count_regex(Corpus.July2007toDecember2008$documents$texts, 
#                                        paste(anxious, collapse = '|'))

# rawtwt.stem.composed.score = stri_count_regex(Corpus.July2007toDecember2008$documents$texts, 
#                                              paste(stem.composed, collapse = '|'))
# rawtwt.stem.anxious.score = stri_count_regex(Corpus.July2007toDecember2008$documents$texts, 
#                                             paste(stem.anxious, collapse = '|'))

# rawtwt.rd.stem.composed.score = stri_count_regex(Corpus.July2007toDecember2008$documents$texts, 
#                                                 paste(rd.stem.composed, collapse = '|'))
# rawtwt.rd.stem.anxious.score = stri_count_regex(Corpus.July2007toDecember2008$documents$texts, 
#                                                paste(rd.stem.anxious, collapse = '|'))

# cleantwt.rd.stem.composed.score = stri_count_regex(clean.twts, 
#                                                 paste(rd.stem.composed, 
#                                                         collapse = '|'))
# cleantwt.rd.stem.anxious.score = stri_count_regex(clean.twts, 
#                                                paste(rd.stem.anxious, 
#                                                       collapse = '|'))


# tone.num = composed.score.raw - anxious.score.raw
# tone.denom = composed.score.raw + anxious.score.raw
# stem.tone.num = stem.composed.score.raw - stem.anxious.score.raw
# stem.tone.denom = stem.composed.score.raw + stem.anxious.score.raw
# rd.stem.tone.num = rd.stem.composed.score.raw - rd.stem.anxious.score.raw
# rd.stem.tone.denom = rd.stem.composed.score.raw + rd.stem.anxious.score.raw
# rd.stem.tone.clean.num = cleantwt.rd.stem.composed.score - cleantwt.rd.stem.anxious.score
# rd.stem.tone.clean.denom = cleantwt.rd.stem.composed.score + cleantwt.rd.stem.anxious.score

# tone.denom[tone.denom == 0] = 1
# stem.tone.denom[stem.tone.denom == 0] = 1
# rd.stem.tone.denom[rd.stem.tone.denom == 0] = 1
# rd.stem.tone.clean.denom[rd.stem.tone.clean.denom == 0] = 1

# raw.tone = tone.num/tone.denom
# stem.tone = stem.tone.num/stem.tone.denom
# cor(tone, stem.tone) = 0.77; relatively small!
# rd.stem.tone = rd.stem.tone.num/rd.stem.tone.denom
# rd.stem.tone.clean = rd.stem.tone.clean.num/rd.stem.tone.clean.denom
# cor(rd.stem.tone.clean, tone) = 0.75, again, arbitrary choice of cleaning procedure has large effect

library(data.table)

mood = data.table(dates = DATES,
                             tone = tone,
                             calm = dated.scores)

setkey(mood, dates)
daily.mood = mood.sentimentr[,.(tone = mean(tone), calm = mean(calm)), by = dates]

# make our data matrix

## Visual Comparison

library(ggplot2)

p = ggplot(daily.mood[400:532], aes(dates, tone)) + 
  geom_line() + 
  geom_vline(xintercept = as.numeric(c(as.Date("2008-09-16"), as.Date("2008-10-11"), as.Date("2008-11-04"))))

p

q = ggplot(daily.mood[300:500], aes(dates, tone)) + 
  geom_line() + 
  geom_vline(xintercept = as.numeric(c(as.Date("2008-08-01"), as.Date("2008-09-27"), as.Date("2008-11-05"))))

q

#make normalized time series

normalize = function(x,k){ #define local normalization function
  T = length(x); n.ts = x
  for (t in 1:T){
    if (t - k < 1){
      #first demean, then divide by standard deviation
      n.ts[t] = (x[t] - mean(x[1:(t+k)]))/sd(x[1:(t+k)])
    }
    else if(t + k > T){
      n.ts[t] = (x[t] - mean(x[(t-k):T]))/sd(x[(t-k):T])
    }
    else{
      n.ts[t] = (x[t] - mean(x[(t-k):(t+k)]))/sd(x[(t-k):(t+k)])}
    }
  return(n.ts)
}

daily.mood[, Z10:=normalize(tone, k=10)]
daily.mood[, Z1:=normalize(tone, k=1)]

basicStats(daily.mood[,"calm"])
basicStats(daily.mood[,"tone"])
basicStats(daily.mood[,"Z1"])
basicStats(daily.mood[,"Z10"])

p1 = ggplot(daily.mood[400:500], aes(dates, Z)) + geom_line() + 
  geom_vline(xintercept = as.numeric(c(as.Date("2008-09-16"), 
                                       as.Date("2008-10-11"), 
                                       as.Date("2008-11-04"))))

p1

q1 = ggplot(daily.mood[350:500], aes(dates, Z)) + geom_line() + 
  geom_vline(xintercept = as.numeric(c(as.Date("2008-08-01"), as.Date("2008-09-27"), as.Date("2008-11-05"))))

q1 # normalized tone series doesn't recover BMZ's time series but retains

library(zoo); library(quantmod)

#full sample tests

#read in an extra day for differencing
getSymbols("DJIA", from = "2007-07-18", to = "2008-12-31") #DJIA data

#merge will not work on DJIA because it is both a zoo and xts object
#xts "overrules" the zoo class on merge, which causes problems throughout
#thus, we recast the relevant DJIA time series as a zoo object

class(DJIA); DJIA.ts = zoo(Cl(DJIA)); class(DJIA.ts)

#for our full sample, we will create create two mood time series
full.sample = merge(DJIA.ts,
                    diff(DJIA.ts),
                    read.zoo(daily.mood))

full.sample = full.sample[2:dim(full.sample)[1]]
colnames(full.sample)[1:2] = c("DJIA","d.DJIA")

full.sample.wknds = merge(full.sample,
                          full.sample[,c("tone","calm","Z10","Z1")])

colnames(full.sample.wknds) = c(colnames(full.sample), 
                                c("tone.min",
                                  "calm.min",
                                  "Z10.min",
                                  "Z1.min"))
                                
for (i in rev(index(full.sample))){
  if(as.Date(i)==("2007-07-19")){next} #throw exception for first day
  if(is.na(full.sample[as.Date(i), "DJIA"])){
    full.sample.wknds[as.Date(i-1),"tone.min"] = min(full.sample.wknds[as.Date(i),"tone.min"],
                                                     full.sample.wknds[as.Date(i-1),"tone.min"],
                                                     na.rm=TRUE)
    full.sample.wknds[as.Date(i-1),"calm.min"] = min(full.sample.wknds[as.Date(i),"calm.min"],
                                                     full.sample.wknds[as.Date(i-1),"calm.min"],
                                                     na.rm=TRUE)
    full.sample.wknds[as.Date(i-1),"Z1.min"] = min(full.sample.wknds[as.Date(i),"Z1.min"],
                                                  full.sample.wknds[as.Date(i-1),"Z1.min"],
                                                  na.rm=TRUE)
    full.sample.wknds[as.Date(i-1),"Z10.min"] = min(full.sample.wknds[as.Date(i),"Z10.min"],
                                                 full.sample.wknds[as.Date(i-1),"Z10.min"],
                                                 na.rm=TRUE)}

  }

#now, we simply drop all weekends and useless columns

full.sample.ts = full.sample.wknds[complete.cases(full.sample.wknds)]
lintest.ts = full.sample.ts[1:328] #this is the time series from 2007/07/19 to 2008/11/03
bmz.lintest.ts = full.sample.ts[155:328] #this is the time series from 2008/02/28 to 2008/11/03
test.ts = full.sample.ts[329:dim(full.sample.ts)[1]]

basicStats(full.sample.ts[,"DJIA"]); adf.test(full.sample.ts[,"DJIA"])      
basicStats(full.sample.ts[,"d.DJIA"]); adf.test(full.sample.ts[,"d.DJIA"])
basicStats(lintest.ts[,"DJIA"]); adf.test(lintest.ts[,"DJIA"])
basicStats(lintest.ts[,"d.DJIA"]); adf.test(lintest.ts[,"d.DJIA"])
basicStats(bmz.lintest.ts[,"DJIA"]); adf.test(bmz.lintest.ts[,"DJIA"])
basicStats(bmz.lintest.ts[,"d.DJIA"]); adf.test(bmz.lintest.ts[,"d.DJIA"])
basicStats(test.ts[,"DJIA"]); adf.test(test.ts[,"DJIA"])
basicStats(test.ts[,"d.DJIA"]); adf.test(test.ts[,"d.DJIA"])

par(mfrow=c(2,1))
pacf(bmz.lintest.ts[,"d.DJIA"], 
     na.action = na.pass, 
     main = "TMP First DJIA Difference Partial Autocorrelation")
pacf(lintest.ts[,"d.DJIA"], 
     na.action = na.pass,
     main = "Full Sample Training Set First DJIA Difference Partial Autocorrelation")


## Twitter mood and stock market comovements

library(dynlm); library(lmtest)

# Does normalized Twitter mood predict the stock market?

pvalues = NULL
for (i in 1:5){
  L1 = dynlm(bmz.lintest.ts[,"d.DJIA"] ~ 
               L(bmz.lintest.ts[,"d.DJIA"], 1:i))
  L2 = dynlm(bmz.lintest.ts[,"d.DJIA"] ~
               L(bmz.lintest.ts[,"d.DJIA"], 1:i) + L(bmz.lintest.ts[,"Z1"], 1:i))
  pvalues = c(pvalues, waldtest(L1,L2)["Pr(>F)"][2,])
}
pvalues

pvalues = NULL
for (i in 1:5){
  L1 = dynlm(bmz.lintest.ts[,"d.DJIA"] ~ 
               L(bmz.lintest.ts[,"d.DJIA"], 1:i))
  L2 = dynlm(bmz.lintest.ts[,"d.DJIA"] ~
               L(bmz.lintest.ts[,"d.DJIA"], 1:i) + L(bmz.lintest.ts[,"Z1.min"], 1:i))
  pvalues = c(pvalues, waldtest(L1,L2)["Pr(>F)"][2,])
}
pvalues

pvalues = NULL
for (i in 1:5){
  L1 = dynlm(bmz.lintest.ts[,"d.DJIA"] ~ 
               L(bmz.lintest.ts[,"d.DJIA"], 1:i))
  L2 = dynlm(bmz.lintest.ts[,"d.DJIA"] ~
               L(bmz.lintest.ts[,"d.DJIA"], 1:i) + L(bmz.lintest.ts[,"Z10"], 1:i))
  pvalues = c(pvalues, waldtest(L1,L2)["Pr(>F)"][2,])
}
pvalues

pvalues = NULL
for (i in 1:5){
  L1 = dynlm(bmz.lintest.ts[,"d.DJIA"] ~ 
               L(bmz.lintest.ts[,"d.DJIA"], 1:i))
  L2 = dynlm(bmz.lintest.ts[,"d.DJIA"] ~
               L(bmz.lintest.ts[,"d.DJIA"], 1:i) + L(bmz.lintest.ts[,"Z10.min"], 1:i))
  pvalues = c(pvalues, waldtest(L1,L2)["Pr(>F)"][2,])
}
pvalues

pvalues = NULL
for (i in 1:5){
  L1 = dynlm(lintest.ts[,"d.DJIA"] ~ 
               L(lintest.ts[,"d.DJIA"], 1:i))
  L2 = dynlm(lintest.ts[,"d.DJIA"] ~
               L(lintest.ts[,"d.DJIA"], 1:i) + L(lintest.ts[,"Z1"], 1:i))
  pvalues = c(pvalues, waldtest(L1,L2)["Pr(>F)"][2,])
}
pvalues

pvalues = NULL
for (i in 1:5){
  L1 = dynlm(lintest.ts[,"d.DJIA"] ~ 
               L(lintest.ts[,"d.DJIA"], 1:i))
  L2 = dynlm(lintest.ts[,"d.DJIA"] ~
               L(lintest.ts[,"d.DJIA"], 1:i) + L(lintest.ts[,"Z1.min"], 1:i))
  pvalues = c(pvalues, waldtest(L1,L2)["Pr(>F)"][2,])
}
pvalues

pvalues = NULL
for (i in 1:5){
  L1 = dynlm(lintest.ts[,"d.DJIA"] ~ 
               L(lintest.ts[,"d.DJIA"], 1:i))
  L2 = dynlm(lintest.ts[,"d.DJIA"] ~
               L(lintest.ts[,"d.DJIA"], 1:i) + L(lintest.ts[,"Z10"], 1:i))
  pvalues = c(pvalues, waldtest(L1,L2)["Pr(>F)"][2,])
}
pvalues

pvalues = NULL
for (i in 1:5){
  L1 = dynlm(lintest.ts[,"d.DJIA"] ~ 
               L(lintest.ts[,"d.DJIA"], 1:i))
  L2 = dynlm(lintest.ts[,"d.DJIA"] ~
               L(lintest.ts[,"d.DJIA"], 1:i) + L(lintest.ts[,"Z10.min"], 1:i))
  pvalues = c(pvalues, waldtest(L1,L2)["Pr(>F)"][2,])
}
pvalues

#probably not

## Did Twitter mood predict the stock market?

pvalues = NULL
for (i in 1:5){
  L1 = dynlm(bmz.lintest.ts[,"d.DJIA"] ~ 
               L(bmz.lintest.ts[,"d.DJIA"], 1:i))
  L2 = dynlm(bmz.lintest.ts[,"d.DJIA"] ~
               L(bmz.lintest.ts[,"d.DJIA"], 1:i) + L(bmz.lintest.ts[,"tone"], 1:i))
  pvalues = c(pvalues, waldtest(L1,L2)["Pr(>F)"][2,])
}
pvalues #probably not

pvalues = NULL;
for (i in 1:5){
  L1 = dynlm(bmz.lintest.ts[,"d.DJIA"] ~ 
               L(bmz.lintest.ts[,"d.DJIA"], 1:i))
  L2 = dynlm(bmz.lintest.ts[,"d.DJIA"] ~
               L(bmz.lintest.ts[,"d.DJIA"], 1:i) + L(bmz.lintest.ts[,"tone.min"], 1:i))
  pvalues = c(pvalues, waldtest(L1,L2)["Pr(>F)"][2,])
}
pvalues #evidence is strongly suggestive that in the linear tests

# Check full sample
pvalues = NULL
for (i in 1:5){
  L1 = dynlm(lintest.ts[,"d.DJIA"] ~ 
               L(lintest.ts[,"d.DJIA"], 1:i))
  L2 = dynlm(lintest.ts[,"d.DJIA"] ~
               L(lintest.ts[,"d.DJIA"], 1:i) + L(lintest.ts[,"tone"], 1:i))
  pvalues = c(pvalues, waldtest(L1,L2)["Pr(>F)"][2,])
}
pvalues #probably not

pvalues = NULL
for (i in 1:5){
  L1 = dynlm(lintest.ts[,"d.DJIA"] ~ 
               L(lintest.ts[,"d.DJIA"], 1:i))
  L2 = dynlm(lintest.ts[,"d.DJIA"] ~
               L(lintest.ts[,"d.DJIA"], 1:i) + L(lintest.ts[,"tone.min"], 1:i))
  pvalues = c(pvalues, waldtest(L1,L2)["Pr(>F)"][2,])
}
pvalues #probably not

pvalues = NULL
for (i in 1:5){
  L1 = dynlm(bmz.lintest.ts[,"d.DJIA"] ~ 
               L(bmz.lintest.ts[,"d.DJIA"], 1:i))
  L2 = dynlm(bmz.lintest.ts[,"d.DJIA"] ~
               L(bmz.lintest.ts[,"d.DJIA"], 1:i) + L(bmz.lintest.ts[,"calm"], 1:i))
  pvalues = c(pvalues, waldtest(L1,L2)["Pr(>F)"][2,])
}
pvalues #probably not

pvalues = NULL
for (i in 1:5){
  L1 = dynlm(bmz.lintest.ts[,"d.DJIA"] ~ 
               L(bmz.lintest.ts[,"d.DJIA"], 1:i))
  L2 = dynlm(bmz.lintest.ts[,"d.DJIA"] ~
               L(bmz.lintest.ts[,"d.DJIA"], 1:i) + L(bmz.lintest.ts[,"calm.min"], 1:i))
  pvalues = c(pvalues, waldtest(L1,L2)["Pr(>F)"][2,])
}
pvalues #evidence is strongly suggestive that in the linear tests

# Check full sample
pvalues = NULL;
for (i in 1:5){
  L1 = dynlm(lintest.ts[,"d.DJIA"] ~ 
               L(lintest.ts[,"d.DJIA"], 1:i))
  L2 = dynlm(lintest.ts[,"d.DJIA"] ~
               L(lintest.ts[,"d.DJIA"], 1:i) + L(lintest.ts[,"calm"], 1:i))
  pvalues = c(pvalues, waldtest(L1,L2)["Pr(>F)"][2,])
}
pvalues #probably not

pvalues = NULL;
for (i in 1:5){
  L1 = dynlm(lintest.ts[,"d.DJIA"] ~ 
               L(lintest.ts[,"d.DJIA"], 1:i))
  L2 = dynlm(lintest.ts[,"d.DJIA"] ~
               L(lintest.ts[,"d.DJIA"], 1:i) + L(lintest.ts[,"calm.min"], 1:i))
  pvalues = c(pvalues, waldtest(L1,L2)["Pr(>F)"][2,])
}
pvalues #probably not

## HORSERACE

## Does the stock market predict Twitter mood?

pvalues = NULL; 
for (i in 1:5){
  M1 = dynlm(bmz.lintest.ts[,"tone"] ~ 
               L(bmz.lintest.ts[,"tone"], 1:i))
  M2 = dynlm(bmz.lintest.ts[,"tone"] ~
               L(bmz.lintest.ts[,"tone"], 1:i) + L(bmz.lintest.ts[,"d.DJIA"], 1:i))
  pvalues = c(pvalues, waldtest(M1,M2)["Pr(>F)"][2,])
}
pvalues #probably not

pvalues = NULL;
for (i in 1:5){
  M1 = dynlm(bmz.lintest.ts[,"tone.min"] ~ 
               L(bmz.lintest.ts[,"tone.min"], 1:i))
  M2 = dynlm(bmz.lintest.ts[,"tone.min"] ~
               L(bmz.lintest.ts[,"tone.min"], 1:i) + L(bmz.lintest.ts[,"d.DJIA"], 1:i))
  pvalues = c(pvalues, waldtest(M1,M2)["Pr(>F)"][2,])
}
pvalues #probably not

pvalues = NULL; 
for (i in 1:5){
  M1 = dynlm(lintest.ts[,"tone"] ~ 
               L(lintest.ts[,"tone"], 1:i))
  M2 = dynlm(lintest.ts[,"tone"] ~
               L(lintest.ts[,"tone"], 1:i) + L(lintest.ts[,"d.DJIA"], 1:i))
  pvalues = c(pvalues, waldtest(M1,M2)["Pr(>F)"][2,])
}
pvalues #probably not

pvalues = NULL;
for (i in 1:5){
  M1 = dynlm(lintest.ts[,"tone.min"] ~ 
               L(lintest.ts[,"tone.min"], 1:i))
  M2 = dynlm(lintest.ts[,"tone.min"] ~
               L(lintest.ts[,"tone.min"], 1:i) + L(lintest.ts[,"d.DJIA"], 1:i))
  pvalues = c(pvalues, waldtest(M1,M2)["Pr(>F)"][2,])
}
pvalues #probably not

A1 = dynlm(lintest.ts[,"tone"] ~ L(lintest.ts[,"d.DJIA"], 1:5)); summary(A1)
A2 = dynlm(bmz.lintest.ts[,"tone"] ~ L(bmz.lintest.ts[,"d.DJIA"], 1:5)); summary(A2)
A3 = dynlm(lintest.ts[,"tone.min"] ~ L(lintest.ts[,"d.DJIA"], 1:5)); summary(A3)
A4 = dynlm(bmz.lintest.ts[,"tone.min"] ~ L(bmz.lintest.ts[,"d.DJIA"], 1:5)); summary(A4)




library(caret)

# none of the p-values are significant, 
# but we need to pick a model for a linear versus non-linear horserace
# thus, we will be using this time series with k = 5 in the caret package
# on the full time series

# first, we must construct lagged mood variables

DJIA.lags = lag(full.sample.ts[,"d.DJIA"], k=0:(-5))
mood.lags = lag(full.sample.ts[,"tone.min"], k=(-1:-5))
full.data = merge(DJIA.lags, mood.lags)
complete.full.data = full.data[complete.cases(full.data)]

train.data = complete.full.data[1:323]
bmz.train.data = complete.full.data[150:323]
test.data = complete.full.data[324:dim(complete.full.data)[1]]

colnames(train.data) = c("d.DJIA",
                         "L1.DJIA",
                         "L2.DJIA",
                         "L3.DJIA",
                         "L4.DJIA",
                         "L5.DJIA",
                         "L1.mood",
                         "L2.mood",
                         "L3.mood",
                         "L4.mood",
                         "L5.mood")

colnames(bmz.train.data) = colnames(train.data)
colnames(test.data) = colnames(train.data)

library(caret)
lin.formula.1 = d.DJIA ~ L1.DJIA + L2.DJIA + L3.DJIA + L4.DJIA + L5.DJIA
lin.formula.2 = d.DJIA ~ L1.DJIA + L2.DJIA + L3.DJIA + L4.DJIA + L5.DJIA + 
  L1.mood + L2.mood + L3.mood + L4.mood + L5.mood

lin.model.base = train(form = lin.formula.1, data = train.data,
  method = "glm") #estimated by least squares

bmz.lin.model.base = train(form = lin.formula.1, data = bmz.train.data,
                          method = "glm") #estimated by least squares

lin.model.mood = train(form = lin.formula.2, data = train.data,
                           method = "glm") #estimated by least squares

bmz.lin.model.mood = train(form = lin.formula.2, data = bmz.train.data,
                       method = "glm") #estimated by least squares

summary(lin.model.base); summary(lin.model.mood)
summary(bmz.linmodel.base); summary(bmz.lin.model.mood)
#set maximum number of terms
maxt = 20
data.ppr = ppr(train.data[,2:11], train.data[,1],nterms=1,max.terms=maxt);
bmz.data.ppr = ppr(bmz.train.data[,2:11], bmz.train.data[,1],nterms=1,max.terms=maxt);
gofntest = data.frame(Hyperparameter = 1:maxt, gofn = data.ppr$gofn)
bmz.gofntest = data.frame(Hyperparameter = 1:maxt, gofn = bmz.data.ppr$gofn)
goftest.plot = ggplot(gofntest, aes(Hyperparameter, gofn)) + geom_point()
bmz.gofntest.plot = ggplot(bmz.gofntest, aes(Hyperparameter, gofn)) + geom_point()
goftest.plot + ylab("Relative Sum of Squares")
bmz.gofntest.plot

# data suggests 5, 7, 9, 10, and 14 are reasonable parameter choices for full data
#               5, 6, 7, and 11 are reasonable parameter choices for BMZ's data
# in the senior thesis, 9 was chosen (using only BMZ's data)
# We follow Carmona (2014) and err on the side of parsimony and choose 10 for full-sample
# We choose 7 
# Since we're using mood to choose
# hyperparameter for the both mood and non-mood non-linear models, this induces
# a bias against the non-linear model not containing mood.

nonlin.model.base = ppr(train.data[,2:6], train.data[,1], nterms=10);
nonlin.model.mood = ppr(train.data[,2:11], train.data[,1], nterms=10);
bmz.nonlin.model.base = ppr(bmz.train.data[,2:6], bmz.train.data[,1], nterms=7);
bmz.nonlin.model.mood = ppr(bmz.train.data[,2:11], bmz.train.data[,1], nterms=7);


# Make predictions using linear models

lin.pred.D = predict(lin.model.base,test.data)
lin.pred.DX = predict(lin.model.mood,test.data)

bmz.lin.pred.D = predict(bmz.lin.model.base, test.data)
bmz.lin.pred.DX = predict(bmz.lin.model.mood, test.data)

nonlin.pred.D = predict(nonlin.model.base, test.data[,2:6])
nonlin.pred.DX = predict(nonlin.model.mood, test.data[,2:11])
bmz.nonlin.pred.D = predict(bmz.nonlin.model.base, test.data[,2:6])
bmz.nonlin.pred.DX = predict(bmz.nonlin.model.mood, test.data[,2:11])

#calculate up-down accuracy

test = test.data[,"d.DJIA"] 

#linear models over full sample - with and without Twitter mood
sum(lin.pred.D*test> 0)/length(lin.pred.D)
sum(lin.pred.DX*test > 0)/length(lin.pred.DX)

#linear models over BMZ subsample - with and without Twitter mood
sum(bmz.lin.pred.D*test > 0)/length(lin.pred.D)
sum(bmz.lin.pred.DX*test > 0)/length(lin.pred.DX)

#nonlinear models over full sample - with and without Twitter mood
sum(nonlin.pred.D*test > 0)/length(nonlin.pred.D)
sum(nonlin.pred.DX*test > 0)/length(nonlin.pred.DX)

#nonlinear models over BMZ subsample - with and without Twitter mood
sum(bmz.nonlin.pred.D*test > 0)/length(nonlin.pred.D)
sum(bmz.nonlin.pred.DX*test > 0)/length(nonlin.pred.DX)

#calculate hypetets
succ = c(sum(lin.pred.D*test > 0), 
         sum(lin.pred.DX*test > 0),
         sum(bmz.lin.pred.D*test > 0),
         sum(bmz.lin.pred.DX*test > 0),
         sum(nonlin.pred.D*test > 0),
         sum(nonlin.pred.DX*test > 0),
         sum(bmz.nonlin.pred.D*test > 0),
         sum(bmz.nonlin.pred.DX*test > 0))
         
prop.test(x = succ, n = rep(40,8))
prop.test(x = succ[1:2], n = c(40,40), correct = T)
prop.test(x = succ[3:4], n = c(40,40))
prop.test(x = succ[5:6], n = c(40,40))
prop.test(x = succ[7:8], n = c(40,40))
prop.test(x = succ[1], n = 40, p=0.5, correct = T)
prop.test(x = succ[2], n = 40, p=0.5, correct = T)
prop.test(x = succ[3], n = 40, p=0.5, correct = T)
prop.test(x = succ[4], n = 40, p=0.5, correct = T)
prop.test(x = succ[5], n = 40, p=0.5, correct = T)
prop.test(x = succ[6], n = 40, p=0.5, correct = T)

#linear models over full sample - with and without Twitter mood
sum(abs((lin.pred.D-test)/test))/length(lin.pred.D)
sum(abs((lin.pred.DX-test)/test))/length(lin.pred.DX)

#linear models over BMZ subsample - with and without Twitter mood
sum(abs((bmz.lin.pred.D-test)/test))/length(lin.pred.D)
sum(abs((bmz.lin.pred.DX-test)/test))/length(lin.pred.DX)

#nonlinear models over full sample - with and without Twitter mood
sum(abs((nonlin.pred.D-test)/test))/length(nonlin.pred.D)
sum(abs((nonlin.pred.DX-test)/test))/length(nonlin.pred.DX)

#nonlinear models over BMZ sample - with and without Twitter mood
sum(abs((bmz.nonlin.pred.D-test)/test))/length(nonlin.pred.D)
sum(abs((bmz.nonlin.pred.DX-test)/test))/length(nonlin.pred.DX)

#calculate RMSE

#linear models over full sample - with and without Twitter mood
sqrt(sum((lin.pred.D-test)^2)/length(test))
sqrt(sum((lin.pred.DX-test)^2)/length(test))

#linear models over BMZ subsample - with and without Twitter mood
sqrt(sum((bmz.lin.pred.D-test)^2)/length(test))
sqrt(sum((bmz.lin.pred.DX-test)^2)/length(test))

#nonlinear models over full sample - with and without Twitter mood
sqrt(sum((nonlin.pred.D-test)^2)/length(test));
sqrt(sum((nonlin.pred.DX-test)^2)/length(test))

#nonlinear models over BMZ sample - with and without Twitter mood
sqrt(sum((bmz.nonlin.pred.D-test)^2)/length(test));
sqrt(sum((bmz.nonlin.pred.DX-test)^2)/length(test))


# Visualize our horserace
horserace = data.frame(test, 
                       lin.pred.D, 
                       lin.pred.DX, 
                       nonlin.pred.D, 
                       nonlin.pred.DX)
library(gridExtra)

plot.lin.D = ggplot(data = horserace, aes(x = as.Date(rownames(horserace)),y = test)) + 
  geom_line(aes(color = "Actual")) +
  scale_y_continuous(name = "DJIA Differences") +
  geom_line(aes(y = lin.pred.D, color = "Predicted")) +
  scale_color_manual(values = c("black","red")) +
  ggtitle("ARDL Model without Twitter Mood") + theme(legend.position = "bottom") +
  xlab("")

plot.lin.DX = ggplot(data = horserace, aes(x = as.Date(rownames(horserace)),y = test)) + 
  geom_line(aes(color = "Actual")) +
  scale_y_continuous(name = "DJIA Differences") +
  geom_line(aes(y = lin.pred.DX, color = "Predicted")) +
  scale_color_manual(values = c("black","red")) +
  ggtitle("ARDL Model with Twitter Mood") +
  xlab("") + guides(color = F)

grid.arrange(plot.lin.D, plot.lin.DX, ncol = 1)

plot.nonlin.D = ggplot(data = horserace, aes(x = as.Date(rownames(horserace)),y = test)) + 
  geom_line(aes(color = "Actual")) +
  scale_y_continuous(name = "DJIA Differences") +
  geom_line(aes(y = nonlin.pred.D, color = "Predicted")) +
  scale_color_manual(values = c("black","red")) + theme(legend.position = "bottom") +
  ggtitle("Projection Pursuit Regression without Twitter Mood") +
  xlab("")

plot.nonlin.DX = ggplot(data = horserace, aes(x = as.Date(rownames(horserace)),y = test)) + 
  geom_line(aes(color = "Actual")) +
  scale_y_continuous(name = "DJIA Differences") +
  geom_line(aes(y = nonlin.pred.DX, color = "Predicted")) +
  scale_color_manual(values = c("black","red")) +
  ggtitle("Projection Pursuit Regression with Twitter Mood") +
  xlab("") + guides(color = F)

grid.arrange(plot.nonlin.D, plot.nonlin.DX, ncol = 1)
daily.mood.df = as.data.frame(daily.mood)

daily.mood.tone.plot = ggplot(data = daily.mood.df, aes(x = dates, y = tone)) +
  scale_y_continuous(name = "Collective Mood Measure: Tone") + 
  geom_line(aes(color = "")) +
  xlab("") + guides(color = F)

daily.mood.calm.plot = ggplot(data = daily.mood.df, aes(x = dates, y = calm)) +
  scale_y_continuous(name = "Collective Mood Measure: S") + 
  geom_line(aes(color = "")) +
  xlab("") + guides(color = F)

grid.arrange(daily.mood.tone.plot, daily.mood.calm.plot, ncol = 1)

DJIA.levels.plot = ggplot(data = full.sample.ts, 
                          aes(x = index(full.sample.ts), y = DJIA)) +
  scale_y_continuous(name = "DJIA Levels") + 
  scale_color_manual(values = c("black")) + 
  geom_line() + ggtitle("2007-07-19 to 2008-12-31") + 
  geom_vline(xintercept = as.numeric(as.Date("2008-11-04")), 
             color = "red", linetype = "longdash") +
  xlab("") + guides(color = F)

DJIA.differences.plot = ggplot(data = full.sample.ts, 
                               aes(x = index(full.sample.ts), y = d.DJIA)) +
  scale_y_continuous(name = "DJIA First Differences") + geom_line() +
  scale_color_manual(values = c("black", "red")) + 
  xlab("") + guides(color = F) + 
  geom_vline(xintercept = as.numeric(as.Date("2008-11-04")), 
             color = "red", linetype = "longdash") +
  geom_text(aes(x = as.Date("2008-11-07"), 
                label = "Beginning of test set", y = 750), angle = 90, size = 2.5)

grid.arrange(DJIA.levels.plot, DJIA.differences.plot, ncol = 1)

