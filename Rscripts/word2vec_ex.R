#See the github documentation
#https://github.com/bmschmidt/wordVectors/blob/master/vignettes/introduction.Rmd

# If you haven't installed devtools or wordVectors
  if (!(require(devtools))) {
    install.packages("devtools")
  }
  devtools::install_github("bmschmidt/wordVectors")
# 

library(wordVectors)
library(magrittr)


download.file("http://archive.lib.msu.edu/dinfo/feedingamerica/cookbook_text.zip","cookbooks.zip")
unzip("cookbooks.zip",exdir="cookbooks")

#bundling joins word chuncks e.g. red_pepper, into one word. 
# prep_word2vec(origin="cookbooks",destination="cookbooks.txt",lowercase=T,
#              bundle_ngrams=2)

# 10 million word file

#model = train_word2vec("cookbooks.txt","cookbook_vectors.bin",
#                       vectors=200,threads=4,window=12,iter=5,negative_samples=0)


model = read.vectors("/data/data_ichl23/cookbook_vectors.bin")

model %>% closest_to("fish")

some_fish = closest_to(model,model[[c("fish","salmon","trout","shad","flounder","carp","roe","eels")]],150)
fishy = model[[some_fish$word,average=F]]
plot(fishy,method="pca")

tastes = model[[c("sweet","salty"),average=F]]

# model[1:3000,] here restricts to the 3000 most common words in the set.
sweet_and_saltiness = model[1:3000,] %>% cosineSimilarity(tastes)

# Filter to the top 20 sweet or salty.
sweet_and_saltiness = sweet_and_saltiness[
  rank(-sweet_and_saltiness[,1])<20 |
    rank(-sweet_and_saltiness[,2])<20,
  ]

plot(sweet_and_saltiness,type='n')
text(sweet_and_saltiness,labels=rownames(sweet_and_saltiness))



tastes = model[[c("sweet","salty","savory","bitter","sour"),average=F]]


common_similarities_tastes = model[1:3000,] %>% cosineSimilarity(tastes)

common_similarities_tastes[20:30,]

high_similarities_to_tastes = common_similarities_tastes[rank(-apply(common_similarities_tastes,1,max)) < 75,]

high_similarities_to_tastes %>% 
  prcomp %>% 
  biplot(main="Fifty words in a\nprojection of flavor space")


