library(tidyverse)
library(quanteda)
library(stm)
library(LDAvis)


csv_files <- list.files(pattern = "csv")


df <- read_csv(csv_files[2])

# a couple of summaries just to make sure their isn't some kind of form or 
# repeated responses. 
#hist of number of characteres
hist(nchar(df$reason), 120)

#hist of number words
hist(lengths(quanteda::tokenize(x = df$reason, what = "word")), 120)

#build a corpus 
reason_corpus <- corpus(df$reason)
docnames(reason_corpus) <- df$dsa_id
docvars(reason_corpus, field = "new_member") <- df$new_member


#build document term matrix 
reason_dfm <-  dfm(
  x = reason_corpus,
  stem = T,
  remove = stopwords(kind = "english"),
  removePunct = T
)



#what are the top words?
topfeatures(reason_dfm, n = 100)

#to save a little on memore conver to native stm format
reason_stm <- convert(reason_dfm, to = "stm")

#this results in the loss of a few documents because they are too short 

df <- df[df$dsa_id %in% names(reason_stm$documents), ]


k_select <- stm::searchK(
  documents = reason_stm$documents,
  vocab = reason_stm$vocab,
  K = seq(from = 4 ,to = 16, by = 2),
  # allowing prior for topic prevalance to take into account new_member status
#  prevalence = ~ new_member,
  data = df )


plot(k_select$results$exclus)

k <- 10

reason_model_10 <- stm(
  documents = reason_stm$documents,
  vocab = reason_stm$vocab,
  K = k,
  # allowing prior for topic prevalance to take into account new_member status
  # prevalence = ~ new_member,
  data = df )

topic_labels <- labelTopics(reason_model_10, n = 20)

frequent_words <- t(topic_labels$freq)
prob_words <- t(topic_labels$prob)

write.csv(frequent_words, file = "frequent_words.csv")
write.csv(frequent_words, file = "frequent_words.csv")

final_df <- cbind(df, reason_model_10$theta)
names(final_df) <- c(names(df), paste("topic", 1:k, sep = "_"))


#this returns the colname of the col number that is returned by a 
# function that is applied rowise 
get_column_names <- function(x = NULL, .f  = NULL){
  temp <- apply(x, 1,  .f)
  names(x)[temp]
  
}

final_df$top_topic <- get_column_names(x = final_df[, 4:13], .f = which.max)

reason_LDAvis <- stm::toLDAvis(mod = reason_model_10, docs = reason_stm$documents , R = 50, as.gist = T)




