#text mining packages
library(tm)
library(tidytext)
library(stm)
library(stmBrowser)
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

#Read in a xls(x) spreadsheet
library(readxl)

negatives = read_excel("/data/data_ichl23/negativeDeclaratives_1700-1913.xlsx")

negatives = negatives %>%
  mutate(id = paste0(document,address))  


# ?textProcessor for description
# Defaults: removes SMART stopwords, removes punctuation, numbers, language = "english"
# supports SnowballC current languages. 
# striphtml parameter

processed <- textProcessor(documents=negatives$token, metadata =
                             negatives,verbose = TRUE)

out <- prepDocuments(processed$documents, processed$
                       vocab, processed$meta, lower.thresh = 1)

load("/data/data_ichl23/stmStuff.rda")

#stmFit <- stm(documents = out$documents, vocab = out$vocab, 
#                       K =30, prevalence =~ variant+date, data = out$meta, init.type = "Spectral")



#prep <- estimateEffect(1:30 ~ variant+date, stmFit, meta = out$meta, uncertainty = "Global")


plot(prep, covariate = "variant", topics = c(21,16,13,25))

plot(stmFit,covariate = "variant", type = "perspectives", topics =16)

# FREX words weight words by their overall frequency and how exclusive they are to a topic
# Lift words weight words by dividing by their frequency in other topics, therefore giving higher weight to words that appear less frequently in other topics.
# score divides the log frequency of the word in the topic by the log frequency of the word in other topics.


labelTopics(stmFit,topic=c(6,12,13),n=10)


setwd(tempdir())

stmBrowser(stmFit, data=out$meta,c("variant","date"),text="token")

unlink("stm-visualization", recursive=TRUE)

removed = c(processed$docs.removed,out$docs.removed)

keptTokens = negatives[-removed,]$token


# 13, 16, 17

plot(prep, "date", method = "continuous", topics = 7)

plot(prep, "date", method = "continuous", topics =3,
     moderator = "variant",moderator.value = "Do Support", linecol = "blue", ylim = c(0, .12),
     printlegend = FALSE)

plot(prep, "date", method = "continuous", topics =3,
     moderator = "variant",moderator.value = "Inversion", linecol = "red",add=TRUE, 
     printlegend = FALSE)
plot(prep, "date", method = "continuous", topics =3,
     moderator = "variant",moderator.value = "Have Perf", linecol = "green",add=TRUE, 
     printlegend = FALSE)
legend(1705, .105, c("Do Support", "Inversion","Have Perfect"),lwd = 2, col = c("blue", "red", "green"))

labelTopics(stmFit,topic=16,n=10)

thoughts <- findThoughts(stmFit,texts=keptTokens,
                         topics=16, n=2)$docs[[1]]

plotQuote(thoughts)


