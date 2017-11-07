---
title: "Sentiment Analysis"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Here I am demonstrating how to create a longitudinal data set of journal entries using a simple artificial example.  This example includes two journal entries over two different time points across two different people.  The unit of analysis is the journal entry.
```{r message=FALSE, warning=FALSE, echo=FALSE}
id = rep(1:2, 1, each = 2)
time = rep(1:2, 2)
journalEntry = rbind("I hate everything and dislike all people", "I'm happy with everything and I love everyone", "I am very negative and sad and unhappy", "I am very positive, happy, and upbeat") 
testDat = testDat = data.frame(cbind(id, time, journalEntry))
colnames(testDat) = c("id", "time", "journalEntry")
testDat$journalEntry = as.character(testDat$journalEntry)
testDat
```
The first step is to break down each journal entry by each word.  We can do this using the unnest tokens function that breaks down each word in each journal entry, but also keeps the person (i.e. id) and time point that each word comes from.  Then we need to gather the lexicon that contains the sentiments, which are valences of positive and negative on a scale of -5 (negative) to 5 (positive).  

Uses this dictionary e.g., Jockers (2017) dictionary.  Keeps the sentence structure and then tags the words with +1 or -1.  Then from those tagged words looks at the four words before and after those words.  Polarized context cluster has a list of neutral, negator, amplifier, and de-amplifier words.  These values are weigthed by the ampliyer and negators 

Figure out how accurate this method is.
What is the lexicon they are using?
What is the effect of neutrual words?

```{r message=FALSE, warning=FALSE}
library(tidyr)
library(tidytext)
library(dplyr)
library(sentimentr)
nrc = sentiment(testDat$journalEntry, lexicon::hash_sentiment_nrc)
nrc = data.frame(nrc$sentiment)
jockers = sentiment(testDat$journalEntry)
jockers= data.frame(jockers$sentiment)
huliu = sentiment(testDat$journalEntry, lexicon::hash_sentiment_huliu)
huliu = data.frame(huliu$sentiment)
testDat = cbind(testDat, nrc, jockers, huliu)
testDat

```


