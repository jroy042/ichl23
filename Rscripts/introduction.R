packs = c("tm", "tidytext","SnowballC", "servr", "dplyr","tidyr","purrr","readr","stringr","topicmodels","mgcv","ggplot2","visreg","stm","stmBrowser", "LDAvis")

#text mining packages
library(tm)
library(tidytext)
library(SnowballC)

# Tools for data/string maniuplation
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(stringr)
library(servr)

# Model Building
library(topicmodels)
library(mgcv)

#Visualization
library(ggplot2)
library(visreg)
library(LDAvis)

# Read data in and make a tidyText object
Pamplets = data_frame(file = dir("/data/data_ichl23/2021", full.names = TRUE, 
                      pattern = "\\.(txt)$")) %>%
  mutate(text = map(file, read_file)) %>%
  transmute(id = basename(file), text) %>%
  unnest(text)


#Remove comments or items not part of the text.
# The pamplets have a header in each file that 
# The \\$ means $, but $ is a special character. 
Pamplets = Pamplets %>%
mutate(text = gsub("\\$.*?\\$", "", text)) %>%
mutate(text = gsub("<.*?>", "", text))  %>%
mutate(text = gsub("[\r\n]", " ", text))


pamWords = Pamplets %>% unnest_tokens(word, text)  %>%
  anti_join(stop_words) %>%
  mutate(word = wordStem(word, language="english"))  



#to see modern languages supported by wordStem
#getStemLanguages()

#Create DTM
word_counts = Pamplets %>%
  count(id, word, sort = TRUE) %>%
  ungroup()

pam_dtm = word_counts %>%
  cast_dtm(id, word, n)


pamTopics <- LDA(pam_dtm, k = 3, control = list(seed = 1234))

tidyPam <- tidytext:::tidy.LDA(pamTopics)

phi <- posterior(pamTopics)$terms
theta <- posterior(pamTopics)$topics

# vocabulary names (in the same order as phi)
vocab <- colnames(phi)

# count the number of occurences for each word
term_frequency <- colSums( as.matrix(pam_dtm) )

# obtain the number of tokens per document
# match the document order the theta
doc_length <- word_counts %>% count(id)
row_order  <- match( rownames(theta), doc_length$id )
doc_length <- doc_length[ row_order, ][['nn']]



json <- createJSON(
  phi = phi,
  theta = theta,
  vocab = vocab,
  doc.length = doc_length,
  term.frequency = term_frequency
  
)

#Opens browser window
serVis(json)



# A little more traditional way
# Corpus of Early English Dialogues

# Read data in and make a tidyText object
dialogues = data_frame(file = dir("/data/data_ichl23/2507/dia text/", full.names = TRUE)) %>%
  mutate(text = map(file, read_file)) %>%
  transmute(id = basename(file), text) %>%
  unnest(text)


#Demonstrate what happens without cleaning. 

dialogues = dialogues %>%
mutate(text = gsub("\\$.*?\\$", "", text)) %>%
  mutate(text = gsub("<.*?>", "", text))  %>%
  mutate(text = gsub("[\r\n]", " ", text))


diaWords = dialogues %>% unnest_tokens(word, text)  %>%
  anti_join(stop_words) %>%
  mutate(word = wordStem(word, language="english"))  



#to see modern languages supported by wordStem
#getStemLanguages()

#Create DTM
word_counts = diaWords %>%
  count(id, word, sort = TRUE) %>%
  ungroup()

dia_dtm = word_counts %>%
  cast_dtm(id, word, n)


# diaTopics <- LDA(dia_dtm, k = 15, control = list(seed = 1234))
load("/data/data_ichl23/diaTopics.rda")

tidyDia <- tidytext:::tidy.LDA(diaTopics)


topicWeights = as.data.frame(diaTopics@gamma)

rownames(topicWeights) = diaTopics@documents

write.csv(diaTopics@gamma,file="topicWeights.csv")

# Lets see 

top_terms <- tidyDia %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free")
