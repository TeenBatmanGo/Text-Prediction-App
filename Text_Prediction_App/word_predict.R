
library(quanteda)
library(tm)
library(data.table)
library(dtplyr)
library(reshape2)
library(stringi)

words_1_gram <- fread("words_1_gram.txt", sep = " ", data.table = TRUE)
words_2_gram <- fread("words_2_gram.txt", sep = " ", data.table = TRUE)
words_3_gram <- fread("words_3_gram.txt", sep = " ", data.table = TRUE)
words_4_gram <- fread("words_4_gram.txt", sep = " ", data.table = TRUE)
words_5_gram <- fread("words_5_gram.txt", sep = " ", data.table = TRUE)
setkey(words_1_gram, "variable")
setkey(words_2_gram, "variable")
setkey(words_3_gram, "variable")
setkey(words_4_gram, "variable")
setkey(words_5_gram, "variable")

prep <- function(pred){
        temp <- tolower(pred)
        temp <- removePunctuation(temp)
        temp
}

pred_length <- function(pred){
        temp <- prep(pred)
        temp <- strsplit(temp, " ")[[1]]
        length(temp)
}



input1gram_count <- function(pred){
        temp <- paste("^", prep(pred), "_", sep = "")
        words_2_gram[grep(temp, words_2_gram$variable, value = TRUE)]
}

input1gram_score <- function(pred, k = 3){
        
        pred <- prep(pred)
        dt <- input1gram_count(pred)[,.(ngram = variable, score = value / words_1_gram[pred][,value])]
        words <- mapply(function(x) x[2], strsplit(dt$ngram, "_"))
        dt$ngram <- words
        
        other_dt <- words_1_gram[,.(ngram = variable, score = 0.4 * value / length(words_1_gram$variable))]
        if(is.null(dt[1]$ngram[[1]])){
                total_dt <- other_dt
        }
        else{
                total_dt <- unique(rbind(dt, other_dt), by = "ngram")
        }
        total_dt <- total_dt[,.(ngram = ngram, score = round(score, 4))]
        setorder(total_dt, -score)
        total_dt[1:k]
}




input2gram_count <- function(pred){
        temp <- strsplit(prep(pred), " ")[[1]]
        temp <- paste("^", temp[1], "_", temp[2], "_", sep = "")
        words_3_gram[grep(temp, words_3_gram$variable, value = TRUE)]
}

input2gram_score <- function(pred, k = 3){
        pred <- prep(pred)
        temp_split <- strsplit(pred, " ")[[1]]
        temp <- paste(temp_split[1], "_", temp_split[2], sep = "")
        dt <- input2gram_count(pred)[,.(ngram = variable, score = value / words_2_gram[temp][,value])]
        words <- mapply(function(x) x[3], strsplit(dt$ngram, "_"))
        dt$ngram <- words
        
        other_dt <- input1gram_score(temp_split[2], k)[,.(ngram, score = score * 0.4)]
        if(is.null(dt[1]$ngram[[1]])){
                total_dt <- other_dt
        }
        else{
                total_dt <- unique(rbind(dt, other_dt), by = "ngram")
        }
        total_dt <- total_dt[,.(ngram = ngram, score = round(score, 4))]
        setorder(total_dt, -score)
        total_dt[1:k]
}



input3gram_count <- function(pred){
        temp <- strsplit(prep(pred), " ")[[1]]
        temp <- paste("^", temp[1], "_", temp[2], "_", temp[3], "_", sep = "")
        words_4_gram[grep(temp, words_4_gram$variable, value = TRUE)]
}

input3gram_score <- function(pred, k = 3){
        pred <- prep(pred)
        temp_split <- strsplit(pred, " ")[[1]]
        temp <- paste(temp_split[1], "_", temp_split[2], "_", temp_split[3], sep = "")
        dt <- input3gram_count(pred)[,.(ngram = variable, score = value / words_3_gram[temp][,value])]
        words <- mapply(function(x) x[4], strsplit(dt$ngram, "_"))
        dt$ngram <- words
        
        temp_new <- paste(temp_split[2], temp_split[3])
        other_dt <- input2gram_score(temp_new, k)[,.(ngram, score = score * 0.4)]
        if(is.null(dt[1]$ngram[[1]])){
                total_dt <- other_dt
        }
        else{
                total_dt <- unique(rbind(dt, other_dt), by = "ngram")
        }
        total_dt <- total_dt[,.(ngram = ngram, score = round(score, 4))]
        setorder(total_dt, -score)
        total_dt[1:k]
}



input4gram_count <- function(pred){
        temp <- strsplit(prep(pred), " ")[[1]]
        temp <- paste("^", temp[1], "_", temp[2], "_", temp[3], "_", temp[4], "_", sep = "")
        words_5_gram[grep(temp, words_5_gram$variable, value = TRUE)]
}

input4gram_score <- function(pred, k = 3){
        pred <- prep(pred)
        temp_split <- strsplit(pred, " ")[[1]]
        temp <- paste(temp_split[1], "_", temp_split[2], "_", temp_split[3], "_", temp_split[4], sep = "")
        dt <- input4gram_count(pred)[,.(ngram = variable, score = value / words_4_gram[temp][,value])]
        words <- mapply(function(x) x[5], strsplit(dt$ngram, "_"))
        dt$ngram <- words
        
        temp_new <- paste(temp_split[2], temp_split[3], temp_split[4])
        other_dt <- input3gram_score(temp_new, k)[,.(ngram, score = score * 0.4)]
        if(is.null(dt[1]$ngram[[1]])){
                total_dt <- other_dt
        }
        else{
                total_dt <- unique(rbind(dt, other_dt), by = "ngram")
        }
        total_dt <- total_dt[,.(ngram = ngram, score = round(score, 4))]
        setorder(total_dt, -score)
        total_dt[1:k]
}


word_predict <- function(pred, k = 3){
        pred <- prep(pred)
        n <- pred_length(pred)
        pred_split <- strsplit(pred, " ")[[1]]
        if(n>4){
                pred <- stri_c(pred_split[(n-3):n], collapse = " ")
                input4gram_score(pred, k)
        }
        else if(n == 1){
                input1gram_score(pred, k)
        } 
        else if(n == 2){
                input2gram_score(pred, k)
        }
        else if(n == 3){
                input3gram_score(pred, k)
        }
        else if(n == 4){
                input4gram_score(pred, k)
        }        
        
}







katz <- function(pred, k = 3){
        pred <- prep(pred)
        n <- pred_length(pred)
        if(n>2){
                pred_split <- strsplit(pred, " ")[[1]]
                pred <- stri_c(pred_split[(n-1):n], collapse = " ")
                katz_two(pred, k)
        }
        else if(n == 1){
                katz_one(pred, k)
        }
        else if(n == 2){
                katz_two(pred, k)
        }
}



katz_one <- function(pred, k = 3){
        Obs_bigrams <- get_Obs_bigrams(pred, words_2_gram)
        Obs_bigram_freq <- get_Obs_bigram_prob(pred, words_1_gram, Obs_bigrams)
        Unobs_bigtails <- get_Unobs_bigtails(Obs_bigram_freq, words_1_gram)
        unig <- words_1_gram[words_1_gram$variable == pred,]
        alpha_bigram_1 <- get_alpha_bigram(unig, Obs_bigrams)
        Unobs_prob <- get_Unobs_prob(Unobs_bigtails, alpha_bigram_1)
        
        
        final <- rbind(Obs_bigram_freq, Unobs_prob)
        final <- final[,.(ngram = ngram, prob = round(prob, 4))]
        setorder(final, -prob)
        final[1:k]
}


katz_two <- function(pred, k = 3){
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
        best_pred <- final_prob[,.(ngram = mapply(function(x) x[3], strsplit(ngram, "_")), prob = round(prob, 4))]
        setorder(best_pred, -prob)
        best_pred[1:k]
}


#################################################


get_Obs_bigrams <- function(pred, bigrams){
        input <- paste("^", prep(pred), "_", sep = "")
        bigrams[grep(input, bigrams[,variable], value = TRUE)]
}


get_Obs_bigram_prob <- function(pred, unigrams, Obs_bigrams, gamma1 = 0.5){
        Obs_bigrams[,.(ngram = sapply(strsplit(variable, "_"), function(x) x[2]), prob = (value - gamma1)/unigrams[pred][,value])]
}


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


get_alpha_bigram <- function(unig, Obs_bigrams, gamma1 = 0.5){
        if(is.null(Obs_bigrams[1]$ngram[[1]])){
                1
        }
        else{
                1 - sum((Obs_bigrams[,value] - gamma1)) / unig[,value]
        }
}


get_Unobs_prob <- function(Unobs_bigtails, alpha){
        words_1_gram[,.(ngram = variable, prob = value * alpha / sum(words_1_gram[Unobs_bigtails][,value]))]
}




##########################################################




get_Obs_trigrams <- function(pred ,trigrams){
        temp <- strsplit(pred, " ")[[1]]
        input <- paste("^", prep(temp[1]), "_", prep(temp[2]), "_", sep = "")
        trigrams[grep(input, trigrams[,variable], value = TRUE)]
}


get_Obs_trigram_prob <- function(pred, bigrams, Obs_trigrams, gamma1 = 0.5){
        temp <- strsplit(pred, " ")[[1]]
        temp <- paste(temp[1], "_", temp[2], sep = "")
        Obs_trigrams[,.(ngram = variable, prob = (value - gamma1)/bigrams[temp][,value])]
}


get_Unobs_trigtails <- function(freq, unigs, gamma1 = 0.5){
        temp <- strsplit(freq[,ngram],"_")
        temp <- mapply(function(x) x[3], temp)
        unigs[!(variable %in% temp)][,variable]
}



get_alpha_bigram <- function(unig, bigrams, gamma1 = 0.5){
        input <- paste("^", prep(unig[,variable]), "_", sep = "")
        temp <- bigrams[grep(input, bigrams[,variable], value = TRUE)]
        1 - sum((temp[,value] - gamma1) / unig[,value])
}


get_ObsBo_bigrams <- function(pred, unobs_trigtails, bigrams){
        temp <- strsplit(pred, " ")[[1]][2]
        total <- paste(temp, "_", unobs_trigtails, sep = "")
        bigrams[total][!is.na(value)]
}


get_unobsBo_bigrams <- function(pred, Unobs_trigtails, ObsBo_bigrams){
        temp <- strsplit(pred, " ")[[1]][2]
        total <- paste(temp, "_", Unobs_trigtails, sep = "")
        total[!(total %in% ObsBo_bigrams[,variable])]
}


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


get_Unobs_bigrams_prob <- function(UnobsBo_bigrams, unigs, alpha_bigram){
        temp <- strsplit(UnobsBo_bigrams, "_")
        temp <- mapply(function(x) x[2], temp)
        dt <- unigs[temp][,.(ngram = variable, prob = alpha_bigram * value / sum(value, na.rm = TRUE))] 
        dt$ngram <- UnobsBo_bigrams
        dt[!is.na(prob)]
}


get_alpha_trigram <- function(Obs_trigrams, bigram, gamma2 = 0.5){
        1 - sum((Obs_trigrams[,value] - gamma2) / bigram[,value])
}


get_Unobs_trigrams_prob <- function(pred, Obs_bigrams_prob, Unobs_bigrams_prob, alpha_trigram){
        first_word <- strsplit(pred, " ")[[1]][1]
        dt <- rbind(Obs_bigrams_prob, Unobs_bigrams_prob)
        dt[,.(ngram = paste(first_word, "_", ngram, sep = ""), prob = alpha_trigram * prob / sum(prob))]
}


###############################################################


text_predict <- function(pred, method = "kbo", k = 3){
        sent <- tokenize(pred, what = "sentence")
        pred <- sent[[1]][length(sent[[1]])]
        if(method == "sbo"){
                word_predict(pred, k)
        }
        else if(method == "kbo"){
                katz(pred, k)
        }
}