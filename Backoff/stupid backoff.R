
library(quanteda)
library(tm)
library(data.table)
library(dtplyr)
library(reshape2)
library(stringi)

blogs <- readLines('en_US.blogs.txt', encoding = 'UTF-8')
news <- readLines('en_US.news.txt', encoding = 'UTF-8')
twitter <- readLines('en_US.twitter.txt', encoding = 'UTF-8')
total <- c(blogs, news, twitter)
total_lines <- length(total)
total <- sample(total, total_lines)

traindata <- total[1:(total_lines * 0.9)]
testdata <- total[(total_lines * 0.9 + 1):total_lines]
train_len <- length(traindata)
test_len <- length(testdata)


generate_n_gram <- function(cor, n){
        dfm_gram <- dfm(cor, removePunct = TRUE, removeNumbers = TRUE, ngram = n)
        
        top <- topfeatures(dfm_gram, nfeature(dfm_gram))
        dt <- as.data.table(top, keep.rownames=T)
        colnames(dt) <- c("variable", "value")
        setkey(dt, "variable")
        dt
}

set.seed(1111)
train_sam <- sample(traindata, train_len / 100)
train_sam_prep <- iconv(train_sam, "latin1", "ASCII", sub="")
qcor <- corpus(removePunctuation(train_sam_prep))



words_1_gram <- generate_n_gram(qcor, 1)
words_2_gram <- generate_n_gram(qcor, 2)
words_3_gram <- generate_n_gram(qcor, 3)
words_4_gram <- generate_n_gram(qcor, 4)
words_5_gram <- generate_n_gram(qcor, 5)


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
        setorder(total_dt, -score)
        total_dt[1:k]
}


word_predict <- function(pred, k = 3){
        pred <- prep(pred)
        n <- pred_length(pred)
        if(n == 1){
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



write.table(words_3_gram, "words_3_gram.txt", sep = " ", row.name = FALSE)
write.table(words_4_gram, "words_4_gram.txt", sep = " ", row.name = FALSE)
write.table(words_5_gram, "words_5_gram.txt", sep = " ", row.name = FALSE)
write.table(words_2_gram, "words_2_gram.txt", sep = " ", row.name = FALSE)
write.table(words_1_gram, "words_1_gram.txt", sep = " ", row.name = FALSE)
rm(list = ls())
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

source("benchmark.R")
benchmark(word_predict,
# additional parameters to be passed to the prediction function can be inserted here
sent.list = list('tweets' = tweets[1:50],
'blogs' = blogs[1:50]),
ext.output = T)



get_words <- function(num, k){
        data <- iconv(testdata[num], "latin1", "ASCII", sub="")
        sent <- tokenize(data, what = "sentence")[[1]]
        if(k == 1){
                sent <- paste0(sent, " ")
        }
        else if(k == 2){
                sent <- paste0(sent, " ", " ")
        }
        rand1 <- sample(1:length(sent), 1)
        temp <- strsplit(prep(sent[rand1]), " ")[[1]]
        len <- length(temp)
        if(len == k){
                temp[k + 1] <- " "
                result <- temp
        }
        else{
                rand <- sample(1:(len-k), 1)
                result <- temp[rand:(rand + k)]
        }
        result
}


oneword_test <- function(func, k){
        time <- Sys.time()
        temp <- lapply(seq(test_len/1000), get_words, k)
        pred <- sapply(temp, function(x) x[k+1])
        test <- sapply(temp, function(x) ifelse(k == 1, x[1], paste(x[1], x[k])))
        ind <- 1:length(pred) %in% grep("[a-z]+", pred)
        pred <- pred[ind]
        test <- test[ind]
        count_top3 = count_top1 = 0
        for(i in 1:length(test)){
                prediction = func(test[i])$ngram
                if(any(prediction %in% pred[i])){
                        count_top3 = count_top3 + 1
                        if(prediction[1] == pred[i]){
                                count_top1 = count_top1 + 1
                        }
                }
        }
        time <- Sys.time() - time
        cat("Top1 accuracy: ", round(count_top1 / length(test) * 100, 2), "%", "\n", 
            "Top3 accuracy: ", round(count_top3 / length(test) * 100, 2), "%", "\n", 
            "Time Spent: ", round(time, 2), " sec", "\n", 
            "Test data: ", length(pred), sep = "")
}

oneword_test(word_predict, 2)
oneword_test(katz, 2)

