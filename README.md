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
Now we are moving onto sentiment analysis.  AFINN has scores for negative and positive
sentiment.
nrc has the senitment into different types such as fear, trust, anger, sadness

Example, what if I had an electronic journal entry and wantd to evaluate how sad people's journal's enteries were over time.  We can see if there journal enteries get happier or less sad over time.  Could be used on client notes about themselves as well.  If staff are speaking more positively about the person, then there is evidence that the person is doing better.

Need to research how these lexicons were validated.  Unigrams only and do not take into account negation text like the word so other words.  See in sentiment analysis paper's how people deal with lexicons if they are not from the same sample. 

You may need some type of indicator that a new Note is coming.  Maybe the date assoicated with each note or journal.
```{r}
library(tidytext)
library(dplyr)
library(tidytext)
library(janeaustenr)
sentiments
sentiments %>%
  count(sentiment)
get_sentiments("afinn")
get_sentiments("nrc")

# Broke it down by line with line number. cumsum I think this is saying take cummulative some going from 0 to 1 for each chapter that you identify.  
tidy_books = austen_books() %>%
  group_by(book)%>%
  mutate(linenumber = row_number(), chapter = cumsum(str_detect(text,    regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)
tidy_books

# This get all the sentiments from NRC lexicon and then filters so you only have the joy ones.
nrcJoy = get_sentiments("nrc") %>%
  filter(sentiment == "joy")

# This is finding the words that match in the Emma book and the nrc joy lexicon
tidy_booksNRCJoy = tidy_books %>% 
  filter(book == "Emma") %>%
  inner_join(nrcJoy) %>%
  count(word, sort=TRUE)
  
```
The index is for 80 lines of codes.  So we looking at for 100 lines of code the difference between the number of positive and negative words.  Spread puts the negative and positive words into seperate columns.

Count per book every 80 lines the number of positive and negative words bing has positive and negative words.

Produces two indexes per 80 lines one for the sum of positive words and one for the sum of negative words.
Fill says fill missing values with zero.  Not sure why there are missing values.
Then mutate creates a new variables called sentiment.  Now we could run statistical analysis on this difference, by person if we wanted.  So maybe each person has ten notes or journal enteries.  Then we look at the difference of those notes over time and see if there is an average change that is different from zero.

Do this with some negative emotion from the nrc database.  
```{r}
jansSent = tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80, sentiment) 
jansSent

# Spread says spread sentiment by n 
jansSent = tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
jansSent

# Ok saying plot your data, and the x and y axes are index and sentiment and create a plot for each book.  I think free_x scales means have a unique scale for each book.  So when you don't have free_x, everything scales to 200, which in this case doesn't make sense, because indexs are different.
ggplot(jansSent, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = TRUE)+
  facet_wrap(~book, ncol = 2, scales= "free_x")
```
Now looking how much each word weights to the sentiment using bing which is negative and positive.  One way to validate your finding is to see if they hold up across different lexicons.  Not sure what ungroup does.  

Now we can create a custom list of stop words.  So for example miss may actually talking about a person miss and not miss the bad.  The same could be said for object, which could be referring to object the object and not object.  So may want to take a sample of words that may have different meaning.  May need to figure out the percentage each word in contributing each word is playing if more than 10% and is a double meaning word, then you need to take a sample of 20% of that word and see if we need to discard it.  So we would code that word that has a double meaning code the wrong meaning and see if that number is statistically significantly different from zero.  If so, then we would add that word to the custom stop list.

Ok this is adding the word miss to the lexicon custom that is rbinded with the stop_words lexicon.
```{r}

customStopWords = bind_rows(data_frame(word = c("miss"), lexicon = c("custom")), stop_words)
bingWordCounts = tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  anti_join(customStopWords) %>%
  ungroup()
bingWordCounts
```
Now we are making word clouds in R. 
Says create a word cloud with the unigrams in the column word and make them bigger based upon their n and only put 100 words in the cloud.

The value var must mean sum by n for each word in the positive and negative I think.
```{r}
library(wordcloud)
library(reshape2)
tidy_books %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

tidyCloudCompar = tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"), max.words = 100)
head(tidyCloudCompar)

# We can run comparision cloud that looks the differences in positive and negative words


```
Now we are going to analyze the data at the sentence level
unnest = first is the name of the new column, then the column that you want to change and the token, which says take each sentence and break that up sentences, which I am assuming looking for puncutation.

The summarize functions creates a new variable that is the number of words, which is drawn by n for each chapter and book, which are negative
```{r}
PPSentence = data_frame(text = prideprejudice)


PPSentence = data_frame(text = prideprejudice) %>%
  unnest_tokens(sentence, text, token = "sentences")

bingNegative = get_sentiments("bing") %>%
  filter(sentiment == "negative")

# Total number of words per chapter per book
wordCounts = tidy_books %>%
  group_by(book, chapter) %>%
  summarize(words = n())

# Now filter the words in the wordCounts by negative.  Not quite sure how semi join is different from inner join.  I think it keeps all of the words in the negative lexicon. negWords says sum the number of words, which are already filterd by negative words for each chapter in each book.  I think left join is saying add all the words from the total number of words including any negative words that match, but don't duplicate any of the negative words and include any words not matched in the full lexicon not found in the negative words lexicon.  Looks like the filter is getting rid of the preface or the chapter before one
wordCountBook = tidy_books %>%
  semi_join(bingNegative) %>%
  group_by(book, chapter) %>%
  summarize(negWords = n()) %>%
  left_join(wordCounts, by = c("book", "chapter")) %>%
  mutate(ratio = negWords / words) %>%
  filter(chapter != 0) %>%
  top_n(1)
wordCountBook


```
Moving on to word and document analyses.  So instead of getting rid stop words, which may or not may not be important, we can downplay common words that are used in text alot and weight up not common words?  inverse document frequency (idf)
```{r}
bookWords = austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE) %>%
  ungroup()
bookWords

# Now get the total number of words per book?
totalWords = bookWords %>%
  group_by(book) %>%
  summarize(total = sum(n))
totalWords
# Ok this get the total number of words per book per word and also the number for word for each book.  Zip's law says that the frequency of the word is into inverse (so 1/ probablity that is shows up)
bookWords = left_join(bookWords, totalWords); bookWords

# So here let's get the inverse weight.  So the more common the word the higher the percentage.  So the higher the percentage when take 1/percentage the closer the actual value will be to one; however, smaller percentages mean those numbers will be weighted higher so less common words will be weighted higher than non-common words in the text.  This is all text based. The table is sorted by highest n values so the row number for each chapter in each book gives us that word's rank for each book.   

zifInver = bookWords %>%
  group_by(book) %>%
  mutate(rank = row_number(), 'termFreq' = n/total)
zifInver
```
Now get Zipf's laws.  I think we want a constant slope, because that is an assumptions of Zipf's laws.  May want to look into this. 
Alpha is the brightness I think.  I think with Zipf we can MLE instead of assuming some underlying distribution?  I don't thiks this is a good idea, because if word is in more than one document then it get zeroed out and I don't want.  When analyzing proprotions I don't think you need to worry about this, because you grab a lexicon dictoray and just use those words that match.
```{r}
# Plot to see if slope is constant
zifInver %>%
  ggplot(aes(rank, termFreq, color = book))+
  geom_line(size = 1.2, alpha = .8) +
  scale_x_log10()+
  scale_y_log10()

# Now do a regression to if a one unit increase in rank when logged is contant so is the slope constant to assess the assumption of Zifp's law/  Zipf's law is 1 / rank in the corpus using the assumption that there is a linear relationship between the ranks.
lm(formula = log10(termFreq) ~ log10(rank), data = zifInver)

```
This is a document term matrix that has each row representing one document, each column one term (so maybe a word) and then each value or data point often contains the number of times that term appeared in the document.  So for most documents a term is not present, because they are different documents
```{r}
#install.packages("http://cran.r-project.org/src/contrib/topicmodels_0.2-7.tar.gz", repos=NULL, type="source")
library(topicmodels)
library(modeltools)
library(tm)
data("AssociatedPress")
terms = Terms(AssociatedPress)
head(terms, 50)
# I think i is the document id v is the count and j must be the term?
# This converts non-tidy to tidy
ap_td = tidy(AssociatedPress); head(ap_td, 30)
# This converts tidy to document matrix term
library(methods)
ap_tdDTM = ap_td %>%
  cast_dtm(document, term, count)
# Document number, term identification, count for term
```
Now we are doing topic modeling: Latent Dirichlet allocation (LDA) 
Each document can have different percentages of topics and words can be used in different topics.
```{r}
library(topicmodels)
AssociatedPress
ap_lda = LDA(AssociatedPress, k=2, control = list(seed = 1234))
ap_lda
# This grabs the per topic per word probabilities.  The probabilites that each word in is each topic.  For two topics there are two topics per term.
ap_topics = tidy(ap_lda, matrix = "beta"); ap_topics

# Now we want to visualize the top ten words per topic.  Negative on the beta put them into largest prob first.
ap_top_terms = ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
ap_top_terms

# I am assuming that fill means fill the graph by topic.  Need mutate to add the new variable term.
ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()
# This part is important, because we can see the most common words and then name the topics.
# Could also look at the terms that had the biggest differences between topic 1 and topic 2.  So for percent we would look at the difference in probab for topic one and topic two and rank that difference bewteen all the other terms / words.  We take the log of that ratio so that we can make meaning comparision (1 means that b2 is twice as large and -1 means b1 is twice as large when b2/b1 is the formula and we are using log 2).  This can be compared with the other version to check the validity of the topics.

# Mutate here is creating a new variable topic.  Look like this will be used to spread to a new topic later, which is topic one and two.  Spread goes by the 1 and 2 in spread so it breaks up the probability for each into 1 or 2 and places them into two new variables topic1 and topic2 and gets rid of the original topic variable.  Filter is just saying to keep values that have probability of being included that are greater than .1%.
apSpread = ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2/topic1))
apSpread

# Figure how to filter and graph this at some point.

```

