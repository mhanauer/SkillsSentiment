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
The first step is to break down each journal entry by each word.  We can do this using the unnest tokens function that breaks down each word in each journal entry, but also keeps the person (i.e. id) and time point that each word comes from.  Then we need to gather the lexicon that contains the sentiments, which are valences of positive and negative on a scale of -5 (negative) to 5 (positive).  Additionally, we get rid of stop words such as, the, is, and a.  More information about the lexicon can be found here: http://www2.imm.dtu.dk/pubdb/views/publication_details.php?id=6010

```{r message=FALSE, warning=FALSE}
library(tidyr)
library(tidytext)
library(dplyr)
testDat = testDat %>%
  unnest_tokens(word, journalEntry) %>%
  anti_join(stop_words)
testDat

AfinnFilter = get_sentiments("afinn")
```
Now we can create a longitudinal data set through several steps.  First, we can use piping to combine the testDat, which has each word per row and match (i.e. left join) with the words found in the afinn lexicon.  Because we are using a left join, some words in the original testDat don't have a match in the afinn data set so they are given the value NA.  For NA values that are not found in the afinn lexicon, we can remove them from the list.  Then I delete the word column, because for now the actual word is not relevant for this analysis.  However, for other analyses it may be useful to check the most common words used, possibly the top 10% of words used and make sure that those words are being used in a way that is consistent with the valence assigned to it.  For example, miss is assigned a negative valence; however, in some context miss could be referring to a woman.  If you do find a word like miss that could have a different meaning it would make sense to review how it is used in the text to see if it is being used in the way that the valence is assigned.  If there are some cases where the word does not match it valence you could code the use of the word as being either consistent or not consistent with the valence, the researcher could delete those instances of that word from the text.  
```{r message=FALSE, warning=FALSE}
longData = testDat %>%
  left_join(AfinnFilter) %>%
  group_by(id, time)
longData = na.omit(longData)
longData$word = NULL
longData
```
Next, we take the longitudinal data and group it by id and time, which allows us to summarize the valence scores by its mean.  Therefore, each person gets one score per time point that is the mean of the valence for all the words in the original journal entry for that person on that particular time point.  Now the data is in a long format that can be analyzed using statistical analyses such as multilevel modeling.
```{r message=FALSE, warning=FALSE}
longData = longData %>%
  group_by(id, time) %>%
  summarize(test = mean(score))
longData
```
