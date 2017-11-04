---
title: "Sentiment Analysis"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Here is my start to sentiment analysis.  This could be used for client notes Chapter 1
Need 1:4, because we have four lines here.

The piping has to be in the format otherwise it won't work.
```{r}
library(janeaustenr)
library(dplyr)
library(stringr)
library(dplyr)
library(tidytext)
library(ggplot2)
text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")
textDF = data_frame(line = 1:4, text = text); textDF

textDFPipe = textDF %>%
  unnest_tokens(word, text)

 
```
Next example.  Set the stop words, which are filler type words and get rid of those.  Anti join I am assuming means delete or remove.
```{r}
original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

original_books

tidyBooks = original_books %>%
  unnest_tokens(word, text)
head(tidyBooks)

data(stop_words)
tidyBooks = tidyBooks %>%
  anti_join(stop_words)
tidyBooks
```
Now we are doing analyses.  First is getting counts
```{r}
tidyBooksCount = tidyBooks %>%
  count(word, sort = TRUE)
tidyBooksCount

tidyBooks %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word,n)) +
  geom_col()+
  xlab("# of Words") +
  coord_flip()

```
Now we want to look 
