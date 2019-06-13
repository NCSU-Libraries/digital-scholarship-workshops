## Diving Deeper into Text Analysis with R
## Markus Wust, Alison Blaine, and Erica Hayes
## Activity: Analyze Presidential State of the Union Addresses


# Section 1. Load data from multiple text files, create a corpus, and create a wordcloud.

# 1. Import libraries
install.packages("tm") # Text mining
install.packages("readtext") # Reading text data from various filetypes
install.packages("dplyr") # Data manipulation (from tidyverse)
install.packages("magrittr") # Allows you to chain functions together (from tidyverse)
install.packages("ggplot2") # Data visualization (from tidyverse)
install.packages("stringr") # String manipulation (from tidyverse)
install.packages("forcats") # Factor reordering (from tidyverse)
install.packages("wordcloud") # Creating wordclouds 
install.packages("RColorBrewer") # Creating color palettes
install.packages("topicmodels") # Topic modeling
install.packages("tidytext") # Tidying text data for text analysis
install.packages("SentimentAnalysis") # Sentiment analyzer

# 2. Load libraries
library(tm)
library(readtext)
library(dplyr)
library(magrittr)
library(ggplot2)
library(stringr)
library(forcats)
library(wordcloud)
library(RColorBrewer)
library(topicmodels)
library(tidytext)
library(SentimentAnalysis)

# 3. Get the text from the files in your working directory into R as a dataframe.
# NOTE: We got the data files from the internet using the sotu package and the following 2 lines of code:
# directory <- getwd()
# sotu_dir(dir = directory) # load in all of the SOTU address files into your working directory.

docs <- readtext("*.txt",
                      docvarsfrom = "filenames", 
                      docvarnames = c("filename"),
                      encoding = "utf8")


head(names(docs)) # see column names

# 4. Create new fields for year and name in your dataset.
docs <- docs %>% 
  mutate(year= str_sub(docs$filename, -5)) %>% # create a year column from the last 5 characters of the filename
  mutate(name= str_sub(docs$filename, 1, -6)) # create a name column

# 5. Clean the fields by stripping out unwanted characters and removing the filename column.
docs$year <- docs$year %>%
  str_replace("[-ab]", "")  # remove unwanted characters from the year column

docs$name <- docs$name %>%
  str_replace_all("-", " ") # remove unwanted characters from the name column

docs <- select(docs, -filename)

# 6. Turn your data frame into a corpus object for text analysis.
docs_source <- DataframeSource(docs) # interprets each row of docs as a document
docs_corpus <- SimpleCorpus(docs_source)  # creates a corpus of documents


# 7. Now do some text processing.

# A. Change all words to lower case
docs_corpus <- tm_map(docs_corpus, content_transformer(tolower)) #tolower() is a base R function

# B. Remove common English stopwords (e.g., "and", "the", etc).
# If you want to define your own list of words, you can pass those words in a character vector, e.g.
# docs_corpus <- tm_map(docs_corpus, removeWords, c("dogs", "cats"))

docs_corpus <- tm_map(docs_corpus, removeWords, stopwords("english")) # removeWords is a tm function

# C. Remove punctuation
docs_corpus <- tm_map(docs_corpus, removePunctuation) #removePunctuation is a tm function

# D. Remove numbers
docs_corpus <- tm_map(docs_corpus, removeNumbers) #removeNumbers is a tm function

# E. Remove white space
docs_corpus <- tm_map(docs_corpus, stripWhitespace) #stripWhitespace is a tm function

# see an example document
content(docs_corpus[[100]])

# 8. Create a term-document matrix (shows frequency of terms in a document collection)
termdocmatrix <- TermDocumentMatrix(docs_corpus)
matrix <- as.matrix(termdocmatrix)

# 9. Get counts across all documents
counts <- rowSums(matrix)
sorted_counts <- sort(counts, decreasing = TRUE)

# 10. Convert matrix to dataframe
df <- data.frame(word = names(sorted_counts),freq=sorted_counts, row.names=NULL)

# see the top 20 terms
head(df, 20)

# Uncomment next line if you want the same wordcloud on every run
#set.seed(1234)

# 11. Generate the word cloud
wordcloud(words = df$word, freq = df$freq, min.freq = 1, max.words=500, random.order=TRUE, rot.per=0.35, colors=brewer.pal(8, "Set2"))

# Section 2. Graphing & More with Term Frequencies

# 12. Create a bar plot of the top 20 terms.
barplot(sorted_counts[1:20], col="blue", las=2, ylim=range(pretty(c(0, sorted_counts)))) #bars will be in descending order because matrix is sorted

# The previous bar plot was created with base R. This code creates the bar plot with the package ggplot2.
df$word <- as.factor(df$word) #first, turn "word" into a factor so you can order the bars by count

freq_plot <- df %>%
  top_n(15) %>% # select top 15 terms
  ggplot(., aes(x=fct_reorder(word, freq), y=freq)) + 
  geom_col() +
  coord_flip() +
  labs(title="Most Frequent Terms", x=NULL, y="frequency") + 
  theme_classic()

freq_plot  

# 13. Some more term frequency functions. 

# Find frequent terms in a term-document-matrix. You can set a range.
findFreqTerms(termdocmatrix, lowfreq = 1000, highfreq = Inf)

# See the most frequent terms by document.
findMostFreqTerms(termdocmatrix)


# Section 3. Term Frequency - Inverse Document Frequency (tf-idf) and corpus filtering

# 14. Construct a tf-idf on a term-document matrix to determine a document's most distinctive words
tf_idf <- weightTfIdf(termdocmatrix, normalize = TRUE)
tf_idf_mat <- as.matrix(tf_idf)

# See most distinctive words by document.
findMostFreqTerms(tf_idf)

# 15. Use meta() and tm_filter() to filter out documents from the tdm. Create a set of documents from 1989-2016. 

find_docs_8916 <- meta(docs_corpus, "year") >= 1989 & meta(docs_corpus, "year") <= 2016
docs_8916 <- docs_corpus[find_docs_8916]

docs_8916


# 16. Create a tdm of the 1989-2016 corpus. Find term frequencies by document using that tdm.

tdm_8916 <- TermDocumentMatrix(docs_8916)

findMostFreqTerms(tdm_8916)


# 17. Practice for Sections 2 & 3. 

# A. Create a new subsetted corpus of documents based on President name and/or year. Use step # 15 as a guide.
# Example filtering on name: 
# nixon <- meta(docs_corpus, "name") == "richard m nixon"
# nixon_docs <- docs_corpus[nixon]


# B. Create a term document matrix from that subset.
# Example solution: nixon_tdm <- TermDocumentMatrix(nixon_docs)

# C. Find the most frequent terms in that matrix. 
# Example solution: nixon_mostfreq <- findMostFreqTerms(nixon_tdm)
 
# D. Create tf-idf weighting for that matrix. See step #14 as a guide.
# Example solution: 
# nixon_tf_idf <- weightTfIdf(nixon_tdm, normalize = TRUE)
# nixon_tf_idf_mat <- as.matrix(nixon_tf_idf) # see it as a matrix

# E. See the most distinctive terms by document in your tf-idf matrix. See #14 as a guide. 
# findMostFreqTerms(nixon_tf_idf)
 
# Section 4. Topic Modeling using Latent Dirichlet Allocation (LDA)

# 18. Create a Document Term Matrix (as opposed to tdm).
# from subset of speeches from 1989-2016 (GHW Bush to Obama years)
dtm_8916 <- DocumentTermMatrix(docs_8916)

# 19. Run the LDA model on the dtm.
docs_8916_lda <- LDA(dtm_8916, k=10, control=list(seed=1234))
docs_8916_lda

docs8916_topics <- tidy(docs_8916_lda, matrix = "beta") #tidy() comes from the tidytext library
docs8916_topics

# 20. Sort topics by group in descending order. 
topics_8916 <- docs8916_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# 21. Graph the topics using ggplot2. 
topics_8916 %>%
  mutate(term=reorder(term, beta)) %>% #order terms by beta
  ggplot(aes(term, beta, fill=factor(topic))) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ topic, scales="free") +    # break into subplots based on topic
  coord_flip() #flip x and y coordinates for horizontal bar chart

# 22. Filter out common words from the topics and graph again.

common_words <- c("will", "people", "america")

topics_8916 %>%
  subset(., !term %in% common_words) %>%   # remove common words before graphing
  mutate(term=reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill=factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales="free") +
  coord_flip()

# 23. Practice. Repeat LDA model steps using the obama_docs dataset.

# A. Create a Document Term Matrix for obama_docs named dtm_obama_docs

# B. Run the LDA model on the dtm_obama_docs.

# C. Sort topics by group in descending order. 

# D. Graph the topics. 


# Section 5. Sentiment Analysis

# 24. Use analyzeSentiment on a corpus to get a sentiment rating per document.
sentiment_8916 <- analyzeSentiment(tdm_8916)

sentiment_8916

plotSentiment(sentiment_8916)

# 25. Try a tidytext approach to determining sentiment within documents. 

# A. Create a data subset of docs data set (which is a dataframe, not a corpus) for George W. Bush docs.

gw_docs <- docs %>%
  filter(name == "george w bush")

#. B. Tokenize the text into sentences using the unnest_tokens() function.

gw_docs <- gw_docs %>%
  unnest_tokens(word, text)

head(gw_docs, 4)

# 26. Tokenize the text into one word per line.

gw_docs_sent <- gw_docs %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(year) %>%
  summarise(sentiment= sum(score, na.rm=TRUE)) %>%
  ggplot(aes(year, sentiment)) + geom_line(group=1)

gw_docs_sent

# 27. Let's look at the 2003 speech more closely. It got a pretty negative score overall.

gw_docs_2003 <- gw_docs %>%
  filter(year==2003) %>%
  inner_join(get_sentiments("afinn")) %>% # calculates a sentiment score for each word
  arrange(score) # sort by most negative to most positive term

gw_docs_2003
