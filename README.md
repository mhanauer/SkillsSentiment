---
title: "Sentiment Analysis"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Here I am demonstrating how to create a longitudinal data set of journal entries using a simple artificial example.  It may be of interest for researchers to evaluate a participant's sentiment over time on something like a journal entry.  This example includes two journal entries over two different time points across two different people.  The unit of analysis is the journal entry.
```{r message=FALSE, warning=FALSE, echo=FALSE}
id = rep(1:2, 1, each = 2)
time = rep(1:2, 2)
journalEntry = rbind("I hate everything and dislike all people", "I'm happy with everything and I love everyone", "I am very negative and sad and unhappy", "I am very positive, happy, and upbeat") 
testDat = testDat = data.frame(cbind(id, time, journalEntry))
colnames(testDat) = c("id", "time", "journalEntry")
testDat$journalEntry = as.character(testDat$journalEntry)
testDat
```
The sentimentr package keeps the sentence structure and then tags the words with +1 or -1 depending upon whether the tagged words are positive or negative based on lexicons (i.e. dictionaries of negative and positive words).  Then for those words showing some positive or negative valence, the package looks at the four words before and after the valance words to see if there are any negations (e.g. I don't like apples), amplifiers (I really love apples), neutral (I like green apples), and de-amplifier (I somewhat like apples).  For amplifiers, the package increases the valance, for de-amplifiers the package decreases the valance, for negations, the valance's sign is flipped, and no effect to the word's valence if the surrounding four words are all neutral.  
 
If the data are in a sentence structure, the analysis is pretty easy.  All you need to do is run the function sentiment on the data frame.  Here I want to see if there are any differences in three different lexicons so I run the analysis three times and then combine the results.  
 
One possible limitation is that the lexicons were not built for looking at journal entries of participants in some type of intervention over time.  Therefore, to add some level of validity, I ran the sentiment analysis using three different lexicons.  Given that the valances across the lexicons seem consistent, I have more confidence that the results are accurately capturing the participants sentiment.  Another limitation that researchers will want to take into account is that some words that usually have some positive or negative valence can have different meanings under different contexts.  For example, the word miss is often included in lexicons as demonstrating negative valence; however, in some contexts it can be a way of addressing a woman.  Therefore, researchers will need to think about possible negative words that may have different meaning in their context and review the those words in the data to ensure that the words are consistent with that valance that the sentiment function is assigning them. 

Now the data in a longitudinal format that can analyzed using analyses such as multilevel modeling.
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


