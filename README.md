---
title: "Sentiment Analysis"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Create a data set with notes from 100 students over three time points.  I want the first data set to have 75% negative and 25% positive, then 50% 50%, then 25% negative and 75% positive.  So what would this look if we actually took sentences and then broke them down by each word.  We would have a document id, which would be the time indicator, then we would have a word indicator.  I would need to assign a person id to each document somehow.  I am assuming that I could extract the person id from the document that they provide.  So here I have assumed that I.  So you would have many words for each person over time.  You would need to an average sentiment score for person over each period.  So that is the first step how do we do that.  Let's just use a small example, because creating all that text would be too much then just fill in the average sentiment later.

Ok so let's us assume that that we have two people with two notes over time that are in sentence strucutre.  Let us assume that we have provided an id and document indicator to identify each person and the document (which is the time indicator, because a journal entry is taking over time).
```{r}
id = rep(1:2, 1, each = 2); id
time = rep(1:2, 2); time
sent = rbind("I hate everything and dislike all people", "I'm happy with everything and I love everyone", "I am very negative and sad and unhappy", "I am very positive, happy, and upbeat") 
testDat = testDat = data.frame(cbind(id, time, sent)); testDat
colnames(testDat) = c("id", "time", "sent")
testDat$sent = as.character(testDat$sent)
testDat
```
Now I need to change the data set into a tidy format.  Now I need to grab the sentiment for each of them.  Get rid of stop words  
```{r}
# First create a data set that contains all the words across time and person, which are going to be similar to book and chapter, because chapters are nested within books, just like people are nesting within time.  
testDat = testDat %>%
  unnest_tokens(word, sent) %>%
  anti_join(stop_words)
testDat

# Just do a binary negative or not for the sake of simplicity
bingNegative = get_sentiments("bing") %>%
  filter(sentiment == "negative")
# This should get the total number of words per time point per person.  So I think we can get the sum of the number of negative words per time per person over the total number of words used.  May want to add a fixed effect for the person writing the notes if we are looking a social worker writing about different clients, because some social workers will different language. 

# Total word counts per id per time  Now I need to the sentiment
wordCounts = testDat %>%
  group_by(id, time) %>%
  summarize(words = n())
wordCounts

# Now get the number of negative words possible per id and time. With left join we can the number of negative words out of the total number of words.  We lost time two.  Need to think about keep data points when they have zero.  Probably something with the join.
wordCountIdTime = testDat %>%
  semi_join(bingNegative) %>%
  group_by(id, time) %>%
  summarize(negWords = n()) %>%
  left_join(wordCounts, by = c("id", "time")) %>%
  mutate(ratio = negWords / words) 
wordCountIdTime

```
