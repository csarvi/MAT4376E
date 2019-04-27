### Twitter Multilingual Sentiment Analysis

## Context and data questions

The purpose of this document is to present an approach to classify tweets in English and Portuguese using machine learning techniques for sentiment analysis.  

We will be focusing on a topic that has been on the international news since the beginning of the year ["the Venezuelan presidential crisis"](https://en.wikipedia.org/wiki/2019_Venezuelan_presidential_crisis).

Venezuela is a democratic country and Venezuelans have officially re-elected  Nicolas Maduro as their president in 2018. However, the results of this election has been deemed as fraudulent by opposition parties and multiple countries. With the support of the Venezuelan National Assembly and the international community, Juan Guaidó was declared acting president of the nation since 10 January 2019. 

The main questions we want to answer is : Are the tweets related to this topic and posted in April 2019 by English and Portuguese speakers mostly positive or negative? Are these opinions different depending on the location and/or language? This is in machine learning classification problem using text mining to classify the opinion of each tweet. 

## Methodology

There are many examples of how to do sentiment analysis and for the purpose of this exercise we intend to take advantage of the work performed by other authors, mainly in the pre-classification of tweets by sentiment. 

In our research we encounter two main datasets with tweets pre-classified as positive or negative in English and Portuguese. The English dataset contains 1.6 million tweets and the Portuguese dataset has 500K tweets.

Since most of the work on twitter sentiment analysis has been done in English, we aim to replicate the steps of the feature extraction and classification process, and apply a similar approach to the Portuguese tweets.

The sentiment classification will be implemented through four steps: data extraction, exploration of labelled data, pre-processing & feature selection, and classification using the model trained with the labelled data.


1. Extracting data related to the Venezuelan crisis
2. Explore twitter labelled data
  + 2.1 English labelled Tweets 
  + 3.2 Portuguese labelled Tweets
3. Building Classificaion Models for Labelled Data
  + 2.1 English (Pre-processing, Feature selection and Classification Model)
  + 2.2 Portuguese (Pre-processing, Feature selection and Classification Model)
4. Predictions and analysis of results
  + 4.1 Classification of Tweets in English
  + 4.2 Classification of Tweets in Portuguese
5. Lessons learned and next steps



## 1. Extracting data related to the Venezuelan crisis


We start by loading the necessary packages for the project:

```{r message=FALSE, warning=FALSE, paged.print=FALSE}
#LOAD PACKAGES----------------
library(caret)
library(dplyr)
library(ggplot2)
library(magrittr)
library(tm)
library(twitteR)
library(purrr)
library(plyr)
library(wordcloud)
```


We proceed to extract tweets in both languages using the authentication credentials obtained from the Twitter API.  The `twitteR` package allows to use parameters to narrow the search for specific location and language. 


``` r
# AUTHENTICATION FOR THE TWITTER API----------------------------------------
#Credentials will not work because the API configuration has been changed after
#running the code
consumerKey = "FU2sd3IX4CJDHfq62HjnSn7PF"  
consumerSecret = "iQkD1QsyRK0uqhAGzdzOoBOyzUxEfzAE79InkvhF0tzRu6IX85"
accessToken = "110476495-PM2CXoqDMAzUJriW7ZdCC1Mc7DbuSNZAWpyh6tJU"
accessSecret = "Nu4op1YW4IWDQLtVTXSZtAlUmmInNmn1NtjKGLJLByDpG"
options(httr_oauth_cache=TRUE)
setup_twitter_oauth(consumer_key = consumerKey, consumer_secret = consumerSecret,
                    access_token = accessToken, access_secret = accessSecret)
````

We start with the tweets posted in English. Since  the account used for this project only allows to extract Tweets published in the past 7 days, we will look for tweets publised between April 12 and April 19, 2019.

After running initial test to extract data using the API, we identify there is a limitation to extract more than 10K tweets with a single request. There is also a 15 minute wait to allow us do another request. We created a loop to extract a sample of 9k tweets per day with a delay of 18 minutes.


``` r
# CAPTURING ENGLISH TWEETS --------------------------------------------------
nbrTwts <- 9000 #limit is 10K every 15 minutes
#selecting the keywords for the search
vzltwts <- c("#Venezuela", "#MaduroRegime", 
             "@NicolasMaduro", "@maduro_en",
             "#VenezuelaLibre", "@jguaido")
#Create a boolean search to look for results with all the keywords
vzltwts <- paste0(vzltwts,collapse = " OR ")
# make a string for the days
days <- as.character(seq(as.Date("2019-04-12"), Sys.Date(),by = 1)) # We can only go back 10 days with the type of account we have
#Extract Tweets in English posted in North America
tweetList_EN <- lapply(seq_along(days), function(x){
  
  res_en <- searchTwitter(vzltwts,
                n = nbrTwts ,
                lang ="en",
                since = days[[x]],
                until = days[[x]],
                geocode = '37.09024,-95.712891,3000km') #North America
  
  print("Tweets fetched successfully. Starging wait time...")
  Sys.sleep(time = 1100)
  print("Starting to fetch tweets again...")
  return(res_en)
})
#save the results in a RDS file
saveRDS (tweetList_EN, "../helpers/data_extraction/tweetList_EN.RDS" )
```

And we continue extracting the twweets in portugese.

``` r
# CAPTURING TWEETS IN PORTUGUEESE --------------------------------------------------
#Extract Tweets in Portuguese posted in South America
tweetList_PT <- searchTwitter(vzltwts,
                n = nbrTwts,
                lang ="pt",
                since = "2014-04-15",
                geocode = '-9.138173,-55.383780,2000km') #South America
  
saveRDS (tweetList_PT, "../helpers/data_extraction/tweetList_PT.RDS" )
```

In the previous process, the results were saved in list. We need to covert them to a data frame.

``` r
#Convert the English tweets as a data.frame
ven_en <-Reduce(`c`, tweetList_EN)
ven_en_df <- tbl_df(map_df(ven_en, as.data.frame))
saveRDS(ven_en_df, "../helpers/data_extraction/ven_en_df.RDS" )
#Convert the Portuguese tweets as a data.frame
ven_pt <-Reduce(`c`, tweetList_PT)
ven_pt_df <- tbl_df(map_df(ven_pt, as.data.frame))
saveRDS(ven_pt_df, "../helpers/data_extraction/ven_pt_df.RDS" )
```

After exploring the data, we found that there is duplication in the data extraction. The following code removes duplicates from each dataset.

``` r
ven_en_df <- readRDS("../helpers/data_extraction/ven_en_df.RDS" )
ven_pt_df <- readRDS("../helpers/data_extraction/ven_pt_df.RDS" )
############### English dataset ##############
#Select the variables we need to combine with the labelled data
ven_en_df <- ven_en_df %>% 
  select(id, text)
#Create and index for duplicated rows
dup <- duplicated(ven_en_df$text)
#Remove duplicates from the dataset
ven_en_df <- ven_en_df[!dup, ]
############### Portuguese dataset ##############
ven_pt_df <- ven_pt_df %>% 
  select(id, text)
#Create and index for duplicated rows
dup_pt <- duplicated(ven_pt_df$text)
#Remove duplicates from the dataset
ven_pt_df <- ven_pt_df[!dup_pt, ]
#Change the column name tweet_text to text
ven_pt_df <- ven_pt_df  %>% 
  dplyr::rename(tweet_text = text)
str(ven_pt_df)
``` 

##2.Explore classified data.


###  2.1 English labelled Tweets

The English dataset was created by Go, Bhayani & Huang [1] and it contains tweets extracted between August and September 2018. The tweets were labelled as positive and negative using a distant classification supervision method. The dataset includes the classification positive and negative, the tweet id, date, flag, user and the text of the tweet. 

``` r
#IMPORT ENGLISH LABELLED DATASET ----
TwitTerData_EN <- read.csv("../data/labeled_en/twitterSentiment.csv", stringsAsFactors = F, header = F)
#Set names of the variables
colnames <- c("target", "id", "date", "flag", "user", "text")
names(TwitTerData_EN) <-colnames
 
#Save data as RDS file
saveRDS(TwitTerData_EN,"../data/labeled_en/twtr_dataset_en.RDS") 
```

The dataset has 50% positive and 50% negative labels, 0 for negative tweets and 4 for positive tweets.

``` r
# Identify the frequency of classified sentiments 
TwitTerData_EN <- readRDS("../data/labeled_en/twtr_dataset_en.RDS")
sentfrequ <- TwitTerData_EN %>%
    dplyr::select(target) %>% 
    dplyr::count(target)
  
sentfrequ
```

We will convert this classification to 0 for negative and 1 for positive, and creating a new column call `sentiment` to save the classification.

``` r
#Convert sentiment to 0 negative and 1 positive
TwitTerData_EN <- TwitTerData_EN %>%
  dplyr::mutate(target = dplyr::if_else(target == 0, 0, 1)) %>% 
  dplyr::mutate(sentiment = as.factor(target))# change data type from character to factor
str(TwitTerData_EN)
```

#### 1.2 Portuguese labelled Tweets

The Portuguese dataset was created by de Santiago [2] using a similar approach than the one used by Go. et al. The dataset was collected using 100 political terms and it also includes the tweet id, the text, date and the labelled sentiment.

``` r
#IMPORT PORTUGUESE LABELLED DATASET ----
TwitTerData_PT <- read.csv("../data/labeled_pt/Train500.csv", stringsAsFactors = F)
#Convert sentiment to 0 negative and 1 positive
TwitTerData_PT <- TwitTerData_PT %>%
    dplyr::mutate(sentiment = as.factor(sentiment))# change data type from character to factor
str(TwitTerData_PT)
saveRDS(TwitTerData_PT, "../data/labeled_pt/TwitTerData_PT.RDS")
```


``` r
# Identify the frequency of classified sentiments 
TwitTerData_PT<- readRDS("../data/labeled_pt/twtr_dataset_pt.RDS")
sentfrequ <- TwitTerData_PT %>%
    dplyr::select(sentiment) %>% 
    dplyr::count(sentiment)
  
sentfrequ
```

The dataset has as well a distribution of 50% positive and 50% negative labels, 0 for negative tweets and 1 for positive tweets.


### 2. Building Classificaion Models for Labelled Data


#### 2.1 English (Pre-processing, Feature extraction and Classification Model)


The first step is to combine the extracted and labelled dataset to create a common corpus.

``` r
TwitTerData_EN <- plyr::rbind.fill(TwitTerData_EN, ven_en_df)
```


We start by creating a function to remove unnecessary information from the tweet and then apply it to the dataset, e.g. removing the stop words, white spaces, repeating words, emoticons and #hash tags.


``` r
ProcessTweets <- function (x) {
  
  # select the column that has the tweet text
  tweet <- x$text
  # remove @user tagged in the tweet
  tweet <- gsub("@\\S+", " ", tweet)
  # remove #hashtags in the text
  tweet <- gsub("#\\S+", " ", tweet)
  # remove links
  tweet <- gsub("http\\S+", " ", tweet)
  # remove punctuation 
  tweet <- gsub("[[:punct:]]", " ", tweet)
  # remove numbers e.g. times
  tweet <- gsub("[[:digit:]]", " ", tweet)
  # normalize words -> convert every word to lowercase
  tweet <- tolower(tweet)
  # remove stop words using the tm stopword lexicon
  tweet <- tm::removeWords(tweet, tm::stopwords("English"))
  # remove stop words using the tm stopword lexicon
  tweet <- tm::removeWords(tweet, tm::stopwords("English"))
  #Stem document
  tweet <- tm::stemDocument(tweet, language = "en")
  # replace letters that have more than 3 repetition
  # this deals with words like looooong and soooooooo and change them to long and so   
  regExp <- sapply(letters, function(x) paste0(paste0(rep(x, 3), collapse=""), "+"))
  for(i in seq_along(regExp)){
    tweet <- gsub(x = tweet, pattern = regExp[[i]], replacement = letters[[i]])
  }
  #deals with special characters in the text 
  tweet<- iconv(tweet, "latin1", "ASCII", "")
  # remove words less than 2 characters
  tweet <- gsub("(\\b)?\\w{1,2}(\\b)?", " ", tweet)
  #Remove leading and/or trailing whitespace and other white spaces
  tweet <-trimws(tweet)
  tweet <- gsub("[ \t]{2,}", " ", tweet)
  # bind processed tweets with the data frame
  x <- data.frame (cbind.data.frame(id = x$id,
                         text = x$text,
                         sentiment = x$sentiment,
                         text_sa = tweet))                             
  return(x)
}
```


Now we apply the function to the dataset to process the tweets. Since this is a large dataset and depending on the configuration of the computer it can take a considerable amount of time to process. Throughout the code we are saving the results as RDS files in case there is a need to skip some of the steps.


``` r
TwitTerData_EN_Processed <- ProcessTweets(TwitTerData_EN)
str(TwitTerData_EN_Processed)
````

The variable `text_sa` was converted as a factor and it needs to be a character vector

``` r
TwitTerData_EN_Processed$text_sa <- as.character(TwitTerData_EN_Processed$text_sa)
#Filter out tweets that have no words associated after the initial processing
TwitTerData_EN_Processed <- TwitTerData_EN_Processed %>% 
  filter(text_sa != "")
saveRDS(TwitTerData_EN_Processed, "../data/labeled_en/TwitTerData_EN_Processed.RDS")
  
str(TwitTerData_EN_Processed)
```

The next step is to extract the features of the tweets by converting the processed text to a document text matrix using the `tm` package.  We will also remove tweets that have no text after the pre-processing step. 

``` r
# Generate document text matrix
TwitTerData_EN_Processed <- readRDS("../data/labeled_en/TwitTerData_EN_Processed.RDS")
ENTweetCorpus <- tm::Corpus(tm::VectorSource(TwitTerData_EN_Processed$text_sa))
dtm_EN <- tm::DocumentTermMatrix(ENTweetCorpus)
saveRDS(dtm_EN, "../data/labeled_en/dtm_EN.RDS")
#remove sparse terms
dtm_EN_sparse <- tm::removeSparseTerms(dtm_EN, .993)
saveRDS(dtm_EN_sparse, "../data/labeled_en/dtm_EN_sparse.RDS")
```

We used the sparse term function of the caret package to filter out terms this will remove terms that are more sparse than 0.993. The following plot presents a view of the most frequent words in the English corpus for the word retained after applying the sparse function.  

``` r
# Frequency
memory.limit(size= 53000)    
freq_en <- sort(colSums(as.matrix(dtm_EN_sparse)), decreasing=TRUE)
summary(freq)
#Plot a wordcloud showing words with in the top quartile 
set.seed(123)
wordcloud(names(freq_en), freq_en, min.freq= quantile(freq_en, .75), colors=brewer.pal(6, "Dark2"))
```


``` r
#Need to increase memory size to complete this part because the configuration of my machine
memory.limit(size=53000)
#convert dtm as a matrix
ENdtm <- as.matrix(dtm_EN_sparse)
saveRDS(ENdtm, "../data/labeled_en/ENdtm.RDS")
#convert matrix as data frame
ENdtmDF <- as.data.frame(ENdtm)
#add sentiment target to the data frame
ENdtmDF_sa <- cbind(ENdtmDF, sentiment_target = TwitTerData_EN_Processed$sentiment)
#confirm there are not Na values in the dataset
anyNA(ENdtmDF_sa)
#save EN_dtm as an RDS
saveRDS (ENdtmDF_sa, "../data/labeled_en/ENdtmDF_sa.RDS")
```

As we have now converted the dataset to a document text matrix for the English tweets, the next step is to start building the classification model. The classification model used in this project is based on the Naive Bayes logistic algorithm.  Because of machine configuration, we will only use 25% of the data and split it in 80/20 for training and testing. We use the `caret` package to complete this section of the process.

``` r
ENdtmDF_sa <- readRDS("../data/labeled_en/ENdtmDF_sa.RDS") ## - > start here if you only want to run the model
#Filter out the dataset with the tweets extracted linked to the venezuelan conflict 
holdout_en <- is.na(ENdtmDF_sa$sentiment_target)
ENdtmDF_sa <- ENdtmDF_sa[!holdout_en, ]
# Extract sample of the dataset   
set.seed(9876)
EN_index <- caret::createDataPartition(ENdtmDF_sa$sentiment_target, p = 0.25, 
                                       list = FALSE, 
                                       times = 1)
ENdtmDF_sample <- ENdtmDF_sa[EN_index, ]
#confirm that the number of classification variables is distributed equally
table(ENdtmDF_sample$sentiment)
```

The result is a sample consisting of 198,741 negative tweets and 198,597 positive tweets. This dataset will be split in training and testing.


``` r
#Split dataset in training (80%) and testing (20%)
set.seed(2312)
EN_index_sample <- caret::createDataPartition(ENdtmDF_sample$sentiment, p = 0.8, 
                                             list = FALSE, 
                                             times = 1)      
EN_training <- ENdtmDF_sample[EN_index_sample, ]
EN_test  <- ENdtmDF_sample[-EN_index_sample,]
#verify the distribution of the dataset 
table(EN_training$sentiment_target)
    #save EN_dtm as an RDS
    saveRDS (EN_training, "../data/labeled_en/EN_training.RDS")
    saveRDS (EN_test, "../data/labeled_en/EN_test.RDS")
    
```

The final step is to run the classification model, using the Bayesian Generalized Linear Models for supervised machine learning. Please note that this process can take up to an hour depending on the configuration of your machine. We included a parallel computing function to be able to process the data in the computer used to run the following lines of code.

``` r
EN_training <- readRDS ("../data/labeled_en/EN_training.RDS")
#configure parallel processing
library(parallel)
library(doParallel)
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)
#Need to increase memory to run the process in my machine
memory.limit(size= 53000)    
##### Training the model with naive bayes 
StartTIme <- Sys.time()
fit_nb <- caret::train(sentiment_target ~ ., data = EN_training, method = 'bayesglm')
print(difftime(Sys.time(), StartTIme, units = 'mins'))
#Stop the parallel processing cluster
stopCluster(cluster)
registerDoSEQ()
#save the model
saveRDS(fit_nb, "../data/labeled_en/fit_nb.RDS")
#configure parallel processing
library(parallel)
library(doParallel)
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)
#Need to increase memory to run the process in my machine
memory.limit(size= 53000)    
###### Training the model with suport vector machine 
trctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
StartTIme <- Sys.time()
fit_svm <- caret::train(sentiment_target ~ ., data = EN_training, method = 'svmLinear',
                        trControl=trctrl)
print(difftime(Sys.time(), StartTIme, units = 'mins'))
#Stop the parallel processing cluster
stopCluster(cluster)
registerDoSEQ()
```

And test the model.

``` r
#Need to increase memory to run the process in my machine
memory.limit(size= 53000)    
EN_test <- readRDS ("../data/labeled_en/EN_test.RDS")
fit_nb <- readRDS("../data/labeled_en/fit_nb.RDS")
# Testing the model
predict_nb <- predict(fit_nb, newdata = EN_test)
saveRDS (predict_nb, "../data/labeled_en/predict_nb.RDS")
```



``` r
# Verify the results
fit_nb 
#confirm the results
expected_EN <- EN_test$sentiment_target
results_EN <- confusionMatrix(data = predict_nb, reference = expected_EN)
results_EN
```

The accuracy of the results of the training and testing model is low, only 69%. We estimate that using more data to train the model can increase the accuracy of the model. For the purpose of this project we will apply this model to the data extraction of English tweets and predict the outcome of the obtained level of accuracy.



### 2.2 Portuguese (Pre-processing, Feature extraction and Classification Model)


We'll combine the Portuguese extracted dataset with the labelled dataset to create a common corpus

``` r
TwitTerData_PT <- plyr::rbind.fill(TwitTerData_PT, ven_pt_df)
```


As we did with the English tweets, we will use a similar function to remove words from the tweets in Portuguese. In addition to the process of English tweets, we will also identify other stop words and special characters. 



``` r
ProcessTweets_PT <- function (x) {
  
  # select the column that has the tweet text
  tweet <- x$tweet_text
  # remove @user tagged in the tweet
  tweet <- gsub("@\\S+", " ", tweet)
  # remove #hashtags in the text
  tweet <- gsub("#\\S+", " ", tweet)
  # remove links
  tweet <- gsub("http\\S+", " ", tweet)
  # remove punctuation 
  tweet <- gsub("[[:punct:]]", " ", tweet)
  # remove numbers e.g. times
  tweet <- gsub("[[:digit:]]", " ", tweet)
  # normalize words -> convert every word to lowercase
  tweet <- tolower(tweet)
  # remove stop words using the tm stopword lexicon
  tweet <- tm::removeWords(tweet, tm::stopwords("portuguese"))
  # remove stop words using the tm stopword lexicon
  tweet <- tm::removeWords(tweet, c("pra", "vou" ))
  # remove other simbols in the text
  tweet <- gsub("-\\S+", " ", tweet)
  tweet <- gsub("_\\S+", " ", tweet)
  #Stem document
  # this deals with words like brasiiiiill or orgulhooooooooooo and change them to brasil and orgulho
   regExp <- sapply(letters, function(x) paste0(paste0(rep(x, 3), collapse=""), "+"))
   for(i in seq_along(regExp)){
       tweet <- gsub(x = tweet, pattern = regExp[[i]], replacement = letters[[i]])
    }
  #deals with special characters in the text 
  tweet<- iconv(tweet, "latin1", "ASCII", "")
  #Sten words
  tweet <- tm::stemDocument(tweet, language = "pt")
  # remove words less than 2 characters
  tweet <- gsub("(\\b)?\\w{1,2}(\\b)?", " ", tweet)
  #Remove leading and/or trailing whitespace and other white spaces
  tweet <-trimws(tweet)
  tweet <- gsub("[ \t]{2,}", " ", tweet)
  # bind processed tweets with the data frame
 x <- data.frame (cbind.data.frame(id = x$id,
                         tweet_text_ = x$tweet_text,
                         sentiment = x$sentiment,
                          text_sa = tweet))    
                           
  return(x)
}
 
```


Now we apply the function to the dataset to process the tweets in Portuguese. 

``` r
TwitTerData_PT_Processed <- ProcessTweets_PT(TwitTerData_PT)
str(TwitTerData_PT_Processed)
````

Remove tweets that have no text after the pre-processing step. The variable `text_sa` was converted as a factor and it needs to be a character vector.



``` r
TwitTerData_PT_Processed$text_sa <- as.character(TwitTerData_PT_Processed$text_sa)
#Filter out tweets that have no words associated after the initial processing
TwitTerData_PT_Processed <- TwitTerData_PT_Processed %>% 
  filter(text_sa != "")
saveRDS(TwitTerData_PT_Processed, "../data/labeled_pt/TwitTerData_PT_Processed.RDS")
  
str(TwitTerData_PT_Processed)
```


The next step is to extract the features of the tweets by converting the processed text to a document text matrix.


``` r
# Generate document text matrix
PT_TweetCorpus <- tm::Corpus(tm::VectorSource(TwitTerData_PT_Processed$text_sa))
dtm_PT <- tm::DocumentTermMatrix(PT_TweetCorpus)
saveRDS(dtm_PT, "../data/labeled_pt/dtm_PT.RDS")
#remove sparse terms
dtm_PT_sparse <- tm::removeSparseTerms(dtm_PT, .99)
saveRDS(dtm_PT_sparse, "../data/labeled_pt/dtm_PT_sparse.RDS")
```


``` {r include=TRUE, eval= FALSE}
# Frequency
memory.limit(size= 53000)    
freq_pt <- sort(colSums(as.matrix(dtm_PT_sparse)), decreasing=TRUE)
summary(freq_pt)
#Plot a wordcloud showing words with in the top quartile 
set.seed(123)
wordcloud(names(freq_pt), freq_pt, min.freq= quantile(freq_pt, .75), colors=brewer.pal(6, "Dark2"))
```

``` r
#Need to increase memory size to complete this part because the configuration of my machine
memory.limit(size=53000)
#convert dtm as a matrix
PTdtm <- as.matrix(dtm_PT_sparse)
saveRDS(PTdtm, "../data/labeled_pt/PTdtm.RDS")
#convert matrix as data frame
PTdtmDF <- as.data.frame(PTdtm)
#add sentiment target to the data frame
PTdtmDF_sa <- cbind(PTdtmDF, sentiment_target = TwitTerData_PT_Processed$sentiment)
#confirm there are not Na values in the dataset
anyNA(PTdtmDF_sa)
#save EN_dtm as a RDS
saveRDS (PTdtmDF_sa, "../data/labeled_pt/PTdtmDF_sa.RDS")
```

After converting the dataset to a document text matrix, the next step is to start building the classification model. The classification model used in this project is based on the naive Bayes logistic algorithm.  Because of machine configuration, we will only use 70% of the data and split it in 80/20 for training and testing. 

``` r
PTdtmDF_sa <- readRDS("../data/labeled_pt/PTdtmDF_sa.RDS") ## - > start here if you only want to run the model
#Filter out the dataset with the tweets extracted linked to the venezuelan conflict 
holdout_pt <- is.na(PTdtmDF_sa$sentiment_target)
PTdtmDF_sa <- PTdtmDF_sa[!holdout_pt, ]
# Extract sample of the dataset   
set.seed(9876)
PT_index <- caret::createDataPartition(PTdtmDF_sa$sentiment_target, p = 0.7, 
                                       list = FALSE, 
                                       times = 1)
PTdtmDF_sample <- PTdtmDF_sa[PT_index, ]
#confirm that the number of classification variables is distributed equally
table(PTdtmDF_sample$sentiment)
```

Now we have the sample consisting of 172,524 negative tweets and 173,320 positive tweets. This dataset will be split in training and testing.


``` r
#Split dataset in training (80%) and testing (20%)
set.seed(2312)
PT_index_sample <- caret::createDataPartition(PTdtmDF_sample$sentiment, p = 0.8, 
                                             list = FALSE, 
                                             times = 1)      
PT_training <- PTdtmDF_sample[PT_index_sample, ]
PT_test  <- PTdtmDF_sample[-PT_index_sample,]
#verify the distribution of the dataset 
table(PT_training$sentiment_target)
    #save EN_dtm as a RDS
    saveRDS (PT_training, "../data/labeled_pt/PT_training.RDS")
    saveRDS (PT_test, "../data/labeled_pt/PT_test.RDS")
    
```

The final step is to run the classification model, using the Bayesian Generalized Linear Models for supervised machine learning. 

``` r
#Need to increase memory to 
memory.limit(size= 53000)    
PT_training <- readRDS("../data/labeled_pt/PT_training.RDS")
# Training the model
StartTIme <- Sys.time()
fit_pt <- caret::train(sentiment_target ~ ., data = PT_training, method = 'bayesglm')
EndTime <- Sys.time()
EndTime - StartTIme
saveRDS (fit_pt, "../data/labeled_pt/fit_pt.RDS")
```

And test the model.

``` r
#Need to increase memory to run the process in my machine
memory.limit(size= 53000)    
PT_test <- readRDS ("../data/labeled_pt/PT_test.RDS")
fit_pt <- readRDS("../data/labeled_pt/fit_pt.RDS")
# Testing the model
predict_pt <- predict(fit_pt, newdata = PT_test)
```

``` r
# Verify the results
fit_pt 
#confirm the results
expected_PT <- PT_test$sentiment_target
results_PT <- confusionMatrix(data = predict_pt, reference = expected_PT)
results_PT
```


**** Insert here classification results *******


### 4. Predictions and analysis of results

With the classification models created in the previous phase of the project, we now apply the model to the data extraction for each language.

#### 4.1 Prediction for Tweets in English

``` r
ENdtmDF_sa <- readRDS ("../data/labeled_en/ENdtmDF_sa.RDS")
#get the extracted data to run the classiffier
en_vzla_holdout <- ENdtmDF_sa[holdout_en, ]
#Run the classification model
en_vzla_prediction <- predict(fit, newdata = en_vzla_holdout)
#Add the classification to the tweets
en_vzla_predictionDF <- cbind(en_vzla_holdout, en_vzla_prediction)
en_vzla_predictionDF <- en_vzla_predictionDF%>% 
  tibble::rownames_to_column(var = "rowid") %>% 
  dplyr::select(rowid, en_vzla_prediction)
TwitTerData_EN_Processed_vla <- TwitTerData_EN_Processed[holdout_en, ] %>% 
  tibble::rownames_to_column(var = "rowid")
en_vzla_predictionDF_tweets <- en_vzla_predictionDF %>% 
  left_join(TwitTerData_EN_Processed_vla, by = "rowid") %>% 
  select(id, text, text_sa, sentiment = en_vzla_prediction)
saveRDS (en_vzla_predictionDF_tweets, "../helpers/classification/en_vzla_predictionDF_tweets.RDS")
```


``` r
#Get results of the predictions and compare some results
table(en_vzla_predictionDF_tweets$sentiment)
#See some results of the negative classification
negative_en <- en_vzla_predictionDF_tweets %>% 
  dplyr::filter(sentiment == 0) %>% 
  select(text, text_sa)
tail(negative_en, n=20)
#Test results with some of the words
negative_en_words <- en_vzla_predictionDF_tweets %>% 
  dplyr::filter(grepl('bad|harm', text)) %>% 
  select(text, text_sa, sentiment)
#see how tweets with these words were classified
table(negative_en_words$sentiment)
negative_en_words_test <- negative_en_words %>% 
  dplyr::filter(sentiment == 1)
```



#### 4.2 Prediction for Tweets in Portuguese


``` r
PTdtmDF_sa <- readRDS("../data/labeled_pt/PTdtmDF_sa.RDS")
#Filter out the dataset with the tweets extracted linked to the venezuelan conflict 
holdout_pt <- is.na(PTdtmDF_sa$sentiment_target)
#get the extracted data to run the classiffier
pt_vzla_holdout <- PTdtmDF_sa[holdout_pt, ]
#Run the classification model
pt_vzla_prediction <- predict(fit_pt, newdata = pt_vzla_holdout)
#Add the classification to the tweets
pt_vzla_predictionDF <- cbind(pt_vzla_holdout, pt_vzla_prediction)
pt_vzla_predictionDF <- pt_vzla_predictionDF%>% 
  tibble::rownames_to_column(var = "rowid") %>% 
  dplyr::select(rowid, pt_vzla_prediction)
TwitTerData_PT_Processed_vla <- TwitTerData_PT_Processed[holdout_pt, ] %>% 
  tibble::rownames_to_column(var = "rowid")
str(TwitTerData_PT_Processed_vla)
pt_vzla_predictionDF_tweets <- pt_vzla_predictionDF %>% 
  left_join(TwitTerData_PT_Processed_vla, by = "rowid") %>% 
  select(id, tweet_text_, text_sa, sentiment = pt_vzla_prediction)
saveRDS (pt_vzla_predictionDF_tweets, "../helpers/classification/pt_vzla_predictionDF_tweets.RDS")
```


``` r
#Get results of the predictions and compare some results
table(pt_vzla_predictionDF_tweets$sentiment)
#See some results of the negative classification
negative_pt <- pt_vzla_predictionDF_tweets %>% 
  dplyr::filter(sentiment == 0) %>% 
  select(tweet_text_, text_sa) %>% collect
head(negative_pt, n=20)
#Test results with some of the words
negative_pt_words <- pt_vzla_predictionDF_tweets %>% 
  dplyr::filter(grepl('miserável|ditador', tweet_text_)) %>% 
  select(tweet_text_, text_sa, sentiment) %>% collect
#see how tweets with these words were classified
table(negative_pt_words$sentiment)
positive_pt <-   pt_vzla_predictionDF_tweets %>% 
  dplyr::filter(sentiment == 1)
  
```


### 5. Lessons learned and next steps








### References

[1] Go, A., Bhayani, R. and Huang, L., 2009. Twitter sentiment classification using distant supervision. CS224N Project Report, Stanford, 1(2009), p.12. Retrieved from https://www.kaggle.com/kazanova/sentiment140" 

[2] de Santiago, R.  Portuguese tweets for sentiment analysis. Retrieved from
https://www.kaggle.com/augustop/portuguese-tweets-for-sentiment-analysis
