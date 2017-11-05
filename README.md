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
Now we want to look correlations bewteen the words used in different novels.  How does this work?
```{r}
#library(gutenbergr)
#hgwells <- gutenberg_download(c(35, 36, 5230, 159))

# This gets each word into a row and gets rid of stop words
tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
# This counts each word that is the same  
#tidy_hgwells = tidy_hgwells %>%
  #count(word, sort = TRUE)

#bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))
tidy_bronte <- bronte %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

#tidy_bronte = tidy_bronte %>%
  #count(word, sort = TRUE)

library(tidyr)
# This puts the rows together and also adds which author or source that they are from.
# Spread matches each word in the word column across the authors.  Then gather put them back into the author category and places the na when there is a word in one book but not the other. 
freq = bind_rows(mutate(tidy_bronte, author = "Brontë Sisters"), mutate(tidy_hgwells, author = "H.G. Wells")) %>%
    mutate(word = str_extract(word, "[a-z']+")) %>%
    count(author, word) %>%
    group_by(author) %>%
    # This get the proportion for each word 
    mutate(proportion = n /sum(n)) %>%
    # I am assuming this gets rid of the n column
    select(-n) %>%
    spread(author, proportion) #%>%
    #gather(author, proportion, `Brontë Sisters`:`H.G. Wells`)

freq %>%
  count(author)
cor.test(freq$`Brontë Sisters`, freq$`H.G. Wells`)
t.test(freq$`Brontë Sisters`, freq$`H.G. Wells`)
```
cor is looking at the differences in proportions of the words that are matching.  So if the proportions are different then of the same words that they are using there is correlation between the two.

So if there is no difference then there is no difference in the proportion of words used and if there is a difference then there is a difference in the proportion of words used.  Not sure what value this adds, but still kind of cool.

Figure out what spread does.  Cannot do correlation test when the row numbers differ. 
```{r}
library(dplyr)
stocks <- data.frame(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4))
stocks
# Ok this stacks, values in a dataset.   
stocksm <- stocks %>% gather(stock, price, time); stocksm
gather(stocks, key = price, value = time)

cor.test(data = freq[freq$author == "H.G. Wells",], 
         ~ proportion + 'Brontë Sisters')

```

