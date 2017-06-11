
library(quanteda)
library(data.table)
library(dtplyr)
library(reshape2)

blogs <- readLines('en_US.blogs.txt', encoding = 'UTF-8')
news <- readLines('en_US.news.txt', encoding = 'UTF-8')
twitter <- readLines('en_US.twitter.txt', encoding = 'UTF-8')

set.seed(11)
blog_sam <- sample(blogs, blog_lines / 100)
news_sam <- sample(news, news_lines / 100)
twitter_sam <- sample(twitter, twitter_lines / 100)
total_sam <- c(blog_sam, news_sam, twitter_sam)

qcor <- corpus(total_sam)
generate_n_gram <- function(cor, n){
        dfm_gram <- dfm(cor, removePunct = TRUE, removeNumbers = TRUE, ngram = n)
        top <- topfeatures(dfm_gram, nfeature(dfm_gram))
        dt <- as.data.table(top, keep.rownames=T)
        setkey(dt, rn)
        colnames(dt) <- c("variable", "value")
        dt
}

words_1_gram <- generate_n_gram(qcor, 1)
words_2_gram <- generate_n_gram(qcor, 2)
words_3_gram <- generate_n_gram(qcor, 3)
words_4_gram <- generate_n_gram(qcor, 4)
words_5_gram <- generate_n_gram(qcor, 5)

prep <- function(str){
        temp <- tolower(str)
        temp <- gsub("'", ".", temp)
        temp
}

pred <- "happy"




katz <- function(pred, k = 3){
        pred <- prep(pred)
        n <- pred_length(pred)
        if(n == 1){
                Obs_bigrams <- get_Obs_bigrams(pred, words_2_gram)
                Obs_bigram_freq <- get_Obs_bigram_prob(pred, words_1_gram, Obs_bigrams)
                Unobs_bigtails <- get_Unobs_bigtails(Obs_bigram_freq, words_1_gram)
                unig <- words_1_gram[words_1_gram$variable == pred,]
                alpha_bigram_1 <- get_alpha_bigram(unig, Obs_bigrams)
                Unobs_prob <- get_Unobs_prob(Unobs_bigtails, alpha_bigram_1)
                
                
                final <- rbind(Obs_bigram_freq, Unobs_prob)
                setorder(final, -prob)
                final[1:k]
        }
        else if(n == 2){
                Obs_trigrams <- get_Obs_trigrams(pred, words_3_gram)
                Obs_trigram_freq <- get_Obs_trigram_prob(pred, words_2_gram, Obs_trigrams)
                Unobs_trigtails <- get_Unobs_trigtails(Obs_trigram_freq, words_1_gram)
                unig <- strsplit(pred, " ")[[1]][2]
                unig <- words_1_gram[words_1_gram$variable == unig,]
                alpha_bigram <- get_alpha_bigram(unig, words_2_gram)
                ObsBo_bigrams <- get_ObsBo_bigrams(pred, Unobs_trigtails, words_2_gram)
                UnobsBo_bigrams <- get_unobsBo_bigrams(pred, Unobs_trigtails, ObsBo_bigrams)
                Obs_bigrams_prob <- get_Obs_bigrams_prob(ObsBo_bigrams, words_1_gram)
                Unobs_bigrams_prob <- get_Unobs_bigrams_prob(UnobsBo_bigrams, words_1_gram, alpha_bigram)
                qbo_bigrams <- rbind(Obs_bigrams_prob, Unobs_bigrams_prob)
                
                temp <- strsplit(pred, " ")[[1]]
                temp <- paste(temp[1], "_", temp[2], sep = "")
                bigram <- words_2_gram[words_2_gram$variable %in% temp]
                alpha_trigram <- get_alpha_trigram(Obs_trigrams, bigram)
                Unobs_trigrams_prob <- get_Unobs_trigrams_prob(pred, Obs_bigrams_prob, Unobs_bigrams_prob, alpha_trigram)
                final_prob <- rbind(Obs_trigram_freq, Unobs_trigrams_prob)
                best_pred <- final_prob[,.(ngram = mapply(function(x) x[3], strsplit(ngram, "_")), prob = prob)]
                setorder(best_pred, -prob)
                best_pred[1:k]
        }
}




#################################################


get_Obs_bigrams <- function(pred, bigrams){
        input <- paste("^", prep(pred), "_", sep = "")
        bigrams[grep(input, bigrams[,variable], value = TRUE)]
}

Obs_bigrams <- get_Obs_bigrams(pred, words_2_gram)

get_Obs_bigram_prob <- function(pred, unigrams, Obs_bigrams, gamma1 = 0.5){
        Obs_bigrams[,.(ngram = sapply(strsplit(variable, "_"), function(x) x[2]), prob = (value - gamma1)/unigrams[pred][,value])]
}

Obs_bigram_freq <- get_Obs_bigram_prob(pred, words_1_gram, Obs_bigrams)

get_Unobs_bigtails <- function(freq, unigs, gamma1 = 0.5){
        if(is.null(freq[1]$ngram[[1]])){
                unigs[,variable]
        }
        else{
                temp <- strsplit(freq[,ngram],"_")
                temp <- mapply(function(x) x[2], temp)
                unigs[!(variable %in% temp)][,variable]
        }
}

Unobs_bigtails <- get_Unobs_bigtails(Obs_bigram_freq, words_1_gram)


unig <- words_1_gram[words_1_gram$variable == pred,]

get_alpha_bigram <- function(unig, Obs_bigrams, gamma1 = 0.5){
        if(is.null(Obs_bigrams[1]$ngram[[1]])){
                1
        }
        else{
                1 - sum((Obs_bigrams[,value] - gamma1)) / unig[,value]
        }
}

alpha_bigram_1 <- get_alpha_bigram(unig, Obs_bigrams)

get_Unobs_prob <- function(Unobs_bigtails, alpha){
        words_1_gram[,.(ngram = variable, prob = value * alpha / sum(words_1_gram[Unobs_bigtails][,value]))]
}

Unobs_prob <- get_Unobs_prob(Unobs_bigtails, alpha_bigram_1)


cond <- is.null(Obs_bigram_freq[1]$ngram[[1]])
if(cond){
        final <- Unobs_prob
}else{
        final <- rbind(Obs_bigram_freq, Unobs_prob)
}

setorder(final, -prob)
final[1:3]



##########################################################






get_Obs_trigrams <- function(pred ,trigrams){
        temp <- strsplit(pred, " ")[[1]]
        input <- paste("^", prep(temp[1]), "_", prep(temp[2]), "_", sep = "")
        trigrams[grep(input, trigrams[,variable], value = TRUE)]
}

Obs_trigrams <- get_Obs_trigrams(pred, words_3_gram)

get_Obs_trigram_prob <- function(pred, bigrams, Obs_trigrams, gamma1 = 0.5){
        temp <- strsplit(pred, " ")[[1]]
        temp <- paste(temp[1], "_", temp[2], sep = "")
        Obs_trigrams[,.(ngram = variable, prob = (value - gamma1)/bigrams[temp][,value])]
}

Obs_trigram_freq <- get_Obs_trigram_prob(pred, words_2_gram, Obs_trigrams)

get_Unobs_trigtails <- function(freq, unigs, gamma1 = 0.5){
        temp <- strsplit(freq[,ngram],"_")
        temp <- mapply(function(x) x[3], temp)
        unigs[!(variable %in% temp)][,variable]
}

Unobs_trigtails <- get_Unobs_trigtails(Obs_trigram_freq, words_1_gram)

unig <- strsplit(pred, " ")[[1]][2]
unig <- words_1_gram[words_1_gram$variable == unig,]

get_alpha_bigram <- function(unig, bigrams, gamma1 = 0.5){
        input <- paste("^", prep(unig[,variable]), "_", sep = "")
        temp <- bigrams[grep(input, bigrams[,variable], value = TRUE)]
        1 - sum((temp[,value] - gamma1) / unig[,value])
}

alpha_bigram <- get_alpha_bigram(unig, words_2_gram)

get_ObsBo_bigrams <- function(pred, unobs_trigtails, bigrams){
        temp <- strsplit(pred, " ")[[1]][2]
        total <- paste(temp, "_", unobs_trigtails, sep = "")
        bigrams[total][!is.na(value)]
}

ObsBo_bigrams <- get_ObsBo_bigrams(pred, Unobs_trigtails, words_2_gram)

get_unobsBo_bigrams <- function(pred, Unobs_trigtails, ObsBo_bigrams){
        temp <- strsplit(pred, " ")[[1]][2]
        total <- paste(temp, "_", Unobs_trigtails, sep = "")
        total[!(total %in% ObsBo_bigrams[,variable])]
}

UnobsBo_bigrams <- get_unobsBo_bigrams(pred, Unobs_trigtails, ObsBo_bigrams)

get_Obs_bigrams_prob <- function(ObsBo_bigrams, unigs, gamma2 = 0.5){
        temp <- ObsBo_bigrams[,variable]
        if(is.na(temp[1])){
                ObsBo_bigrams[,.(ngram = variable, prob = value)]
        }
        else{
                temp <- strsplit(temp, "_")[[1]][1]
                ObsBo_bigrams[,.(ngram = variable, prob = (value - gamma2)/unigs[temp][,value])]
        }
}

Obs_bigrams_prob <- get_Obs_bigrams_prob(ObsBo_bigrams, words_1_gram)

get_Unobs_bigrams_prob <- function(UnobsBo_bigrams, unigs, alpha_bigram){
        temp <- strsplit(UnobsBo_bigrams, "_")
        temp <- mapply(function(x) x[2], temp)
        dt <- unigs[temp][,.(ngram = variable, prob = alpha_bigram * value / sum(value, na.rm = TRUE))] 
        dt$ngram <- UnobsBo_bigrams
        dt[!is.na(prob)]
}

Unobs_bigrams_prob <- get_Unobs_bigrams_prob(UnobsBo_bigrams, words_1_gram, alpha_bigram)

qbo_bigrams <- rbind(Obs_bigrams_prob, Unobs_bigrams_prob)

temp <- strsplit(pred, " ")[[1]]
temp <- paste(temp[1], "_", temp[2], sep = "")
bigram <- words_2_gram[words_2_gram$variable %in% temp]

get_alpha_trigram <- function(Obs_trigrams, bigram, gamma2 = 0.5){
        1 - sum((Obs_trigrams[,value] - gamma2) / bigram[,value])
}

alpha_trigram <- get_alpha_trigram(Obs_trigrams, bigram)

get_Unobs_trigrams_prob <- function(pred, Obs_bigrams_prob, Unobs_bigrams_prob, alpha_trigram){
        first_word <- strsplit(pred, " ")[[1]][1]
        dt <- rbind(Obs_bigrams_prob, Unobs_bigrams_prob)
        dt[,.(ngram = paste(first_word, "_", ngram, sep = ""), prob = alpha_trigram * prob / sum(prob))]
}

Unobs_trigrams_prob <- get_Unobs_trigrams_prob(pred, Obs_bigrams_prob, Unobs_bigrams_prob, alpha_trigram)
final_prob <- rbind(Obs_trigram_freq, Unobs_trigrams_prob)
best_pred <- final_prob[,.(prediction = mapply(function(x) x[3], strsplit(ngram, "_")), prob = prob)]
setorder(best_pred, -prob)
best_pred[1:10]




