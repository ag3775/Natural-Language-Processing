ls()
pjct <- read.csv("C:/Users/Dell/Desktop/Desktop/spotify3.csv")
pjct <- read.csv("C:/Users/Dell/Desktop/Desktop/amazon.csv")
pjct <- read.csv("C:/Users/Dell/Desktop/Desktop/Pandora.csv")
head(pjct1)
View(pjct1)
# how to instal with lib location install.packages("dplyr", lib="C:/Users/Dell/Documents/R/Rpackages")
# how to load with lib location library("dplyr", lib.loc="C:/Users/Dell/Documents/R/Rpackages")
library(dplyr)
install.packages(c('tidytext', 'dplyr', 'ggplot2'))
install.packages("dplyr")
library(ggplot2)
library(tidytext)
library(dplyr)
getwd()
setwd("C:/Users/Dell/Documents/R")
getwd()
install.packages("rJava")
install.packages("qdap")
install.packages("tm")
library(qdap)
library(tm)
install.packages("wordcloud")
library(wordcloud)
reviews <- pjct$Rev
reviews_az <- pjct1$review
head(reviews)
head(reviews_az)
reviews_11 <- VectorSource(reviews)
RCorpus <- VCorpus(reviews_11) 

reviews_az_11 <- VectorSource(reviews_az)
RCorpus_az <- VCorpus(reviews_az_11) 


###reviews1 <- as.vector(reviews)
str(RCorpus)

reviews11<- tolower(RCorpus)
reviews2 <- removePunctuation(reviews11)
reviews3 <- removeNumbers(reviews2)
reviews4 <- stripWhitespace(reviews3)
reviews5 <- replace_number(reviews4)
reviews6 <- replace_abbreviation(reviews5)
reviews7 <- replace_contraction(reviews6)
reviews8 <- replace_symbol(reviews7)
reviews9<- stripWhitespace(reviews8)



clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c("app","selection","can","work","get","ads","couple","devices","amazon","pandora","spotify","dollars","prime","use","download","way","stopwords"("en")))
  return(corpus)
}

clean_corp <- clean_corpus(RCorpus)
clean_corp_az <- clean_corpus(RCorpus_az)
clean_corp[[20]][1]
clean_corp_az[[20]][1]

clean_corp

Review_tdm <- TermDocumentMatrix(clean_corp)
Review_az_tdm <- TermDocumentMatrix(clean_corp_az)

Review_m <- as.matrix(Review_tdm)
dim(Review_m)

Review_az_m <- as.matrix(Review_az_tdm)
dim(Review_az_m)

term_frequency <- rowSums(Review_m)
term_frequency <- sort(term_frequency,decreasing = T)
term_frequency[1:20]
barplot(term_frequency[1:10], col = "tan", las = 2)


barplot(music_freqs[1:10,]$num, las = 2, names.arg = music_freqs[1:10,]$term,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

wordcloud2(music_freqs, color = "random-light", backgroundColor = "grey")
wordcloud2(music_freqs, minRotation = -pi/6, maxRotation = -pi/6, minSize = 10,
           rotateRatio = 1)

letterCloud(music_freqs, word = "R", size =100)
letterCloud(music_freqs, word = "WORDCLOUD2", wordSize = 1)

figPath = system.file("examples/t.png",package = "wordcloud2")
wordcloud2(demoFreq, figPath = figPath, size = 1.5,color = "skyblue")



term_frequency_words <- sort(term_frequency, decreasing =  T)
head(term_frequency_words)

music_freqs <- data.frame(term = names(term_frequency_words), num = term_frequency_words)

wordcloud(music_freqs$term,music_freqs$num,
          max.words = 100, colors = "red")

head(colors(),50)

wordcloud(music_freqs$term,
          music_freqs$num,
          max.words = 50, 
          colors = c("grey80","darkgoldenrod1", "tomato"))

wordcloud(words = music_freqs$term, freq = music_freqs$num
          , min.freq = 1,
          max.words=50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
View(music_freqs)

wordcd <- music_freqs[1:50,]
View(wordcd)

install.packages("wordcloud2")
library(wordcloud2)
wordcloud2(wordcd,size = 2)
letterCloud(music_freqs,word = "A", color="random light",size = 1, backgroundColor = "black")

letterCloud(music_freqs,word ='a',size=1)

purple_orange <- brewer.pal(10, "PuOr")

wordcloud(music_freqs$term,
          music_freqs$num,
          max.words = 50, 
          colors = purple_orange)


##################### AMAZON WORD CLOUD ######
term_frequency_az <- rowSums(Review_az_m)
term_frequency_az <- sort(term_frequency_az,decreasing = T)
term_frequency_az[1:20]
barplot(term_frequency_az[1:10], col = "tan", las = 2)
term_frequency_az

term_frequency_az_words <- sort(term_frequency_az, decreasing =  T)
head(term_frequency_az_words)

music_freqs_az <- data.frame(term = names(term_frequency_az_words), num = term_frequency_az_words)

wordcloud(music_freqs_az$term,music_freqs_az$num,
          max.words = 100, colors = "red")

head(colors(),50)

wordcloud(music_freqs_az$term,
          music_freqs_az$num,
          max.words = 50, 
          colors = c("grey80","darkgoldenrod1", "tomato"))

purple_orange <- brewer.pal(10, "PuOr")

wordcloud(music_freqs_az$term,
          music_freqs_az$num,
          max.words = 50, 
          colors = purple_orange)

#### TO COMBINE ALL THE DATA FOR WORDCLOUD 

all_spotify <- paste(reviews, collapse = "")
all_amazon <- paste(reviews_az, collapse = "")
all_Reviews <- c(all_spotify, all_amazon)

head(all_Reviews)

# clean all_tweets
all_Reviews <- VectorSource(all_Reviews)
all_corpus <- VCorpus(all_Reviews)
# Add new stop words to clean_corpus()

all_clean <- clean_corpus(all_corpus)
all_tdm <- TermDocumentMatrix(all_clean)
all_m <- as.matrix(all_tdm)

# Make commonalitiy cloud
commonality.cloud(all_m, 
                  colors = "steelblue1",
                  max.words = 100)


# Clean the corpus
all_clean <- clean_corpus(all_corpus)

# Create all_tdm
all_tdm <- TermDocumentMatrix(all_clean)

# Give the columns distinct names
colnames(all_tdm) <- c("Spotify", "Amazon")

# Create all_m
all_m <- as.matrix(all_tdm)

# Create comparison cloud
comparison.cloud(all_m,
                 colors = c("orange", "blue"),
                 max.words = 50)

#####################################################################

common_words <- subset(
  all_m,
  all_m[, 1] > 0 & all_m[, 2] > 0
)
head(common_words)

sort(common_words, decreasing =  T)

difference <- abs(common_words[, 1] - common_words[, 2])
common_words <- cbind(common_words, difference)
common_words <- common_words[order(common_words[, 3],
                                   decreasing = T), ]
head(common_words)

top25_df <- data.frame(x = common_words[1:25, 1],
                       y = common_words[1:25, 2],
                       labels = rownames(common_words[1:25, ]))

# The plotrix package has been loaded
install.packages("plotrix")
library(plotrix)
# Make pyramid plot
pyramid.plot(top25_df$x, top25_df$y,
             labels = top25_df$labels, 
             main = "Words in Common",
             gap = 500,
             laxlab = NULL  ,
             raxlab = NULL, 
             unit = 500,
             top.labels = c("Spotify",
                            "Words",
                            "Amazon")
)



head(clean_corp)


reviews10 <- bracketX(reviews9)
newstops <- c("app","selection","couple","devices","dollars","way",stopwords("en"))
review11 <- removeWords(reviews10,newstops)
View(review11)
review11


# From previous step
positive <- "DataCamp courses are good awsome bad wrong for learning"

pos_score <- polarity(positive)
neg_score <- polarity(positive)
pos_score
# Get counts
(pos_counts <- counts(pos_score))
(neg_counts <- counts(neg_score))
pos_counts
# Number of positive words
n_good <- length(pos_counts$pos.words[[1]])
n_neg <- length(neg_counts$neg.words[[1]])
n_good
n_neg
# Total number of words
n_words <- pos_counts$wc
n_words
# Verify polarity score
n_good / sqrt(n_words)


# Examine conversation
conversation

# Polarity - All
polarity(conversation$text)

# Polarity - Grouped
student_pol <- conversation %$%
  polarity(text, student)

# Student results
scores(student_pol)

# Sentence by sentence
counts(student_pol)

# qdap plot
plot(student_pol)

# Examine the key.pol
key.pol

# Negators
qdapDictionaries::negation.words

# Amplifiers
qdapDictionaries::amplification.words

# De-amplifiers
qdapDictionaries::deamplification.words

# Examine
text

# Complete the polarity parameters
polarity(
  text.var       = text$words,
  grouping.var   = text$speaker,
  polarity.frame = key.pol,
  negators       = negation.words,
  amplifiers     = amplification.words,
  deamplifiers   = deamplification.words 
)

# stressed_out has been pre-defined
head(stressed_out)

# Basic lexicon score
polarity(stressed_out)

# Check the subjectivity lexicon
key.pol[grep("stress", x)]

# New lexicon
custom_pol <- sentiment_frame(positive.words, c(negative.words, "stressed", "turn back"))

# Compare new score
pos_score <- polarity(stressed_out, polarity.frame = custom_pol)
(pos_counts <- counts(pos_score))
pos_counts
n_good <- length(pos_counts$pos.words[[1]])
n_good






#---------------------------------------------------------------------------------------------------------------#
pjct <- read.csv("C:/Users/Dell/Desktop/Desktop/Pandora.csv")
head(pjct)
str(pjct)

pjct$ï..date<- as.Date(pjct$ï..Date, "%B %d, %Y")

names(pjct)[names(pjct) == 'ï..date'] <- 'Date'
names(pjct)[names(pjct) == 'Rev'] <- 'Review'


head(pjct)

pjct$Month_Yr <- format(as.Date(pjct$Date), "%Y-%m")

pjct_sorted <- pjct[order(pjct$Date),]
View(pjct_sorted)

library(data.table)
library(tm)

pjct_sorted <- pjct_sorted[,1:3]
View(pjct_sorted)


pjct_sorted$update[pjct_sorted$Date %between% c('2011-08-01', '2013-07-30')] <-"Update-1"
pjct_sorted$update[pjct_sorted$Date %between% c('2013-08-01', '2015-07-30')] <-"Update-2"
pjct_sorted$update[pjct_sorted$Date %between% c('2015-08-01', '2016-08-31')] <-"Update-3"
pjct_sorted$update[pjct_sorted$Date > '2016-09-01'] <-"Update-4"

 unique(pjct_sorted$update)
 
 
View(pjct_sorted)

pjct_sorted$Review <- tolower(pjct_sorted$Review)
pjct_sorted$Review<- stripWhitespace(pjct_sorted$Review)
pjct_sorted$Review<- stripWhitespace(pjct_sorted$Review)
pjct_sorted$Review <- removeWords(pjct_sorted$Review, stopwords("en"))
pjct_sorted$Review<- removeNumbers(pjct_sorted$Review)
pjct_sorted$Review<- replace_abbreviation(pjct_sorted$Review)
pjct_sorted$Review<- replace_contraction(pjct_sorted$Review)
pjct_sorted$Review<- replace_symbol(pjct_sorted$Review)
pjct_sorted$Review <- gsub(pattern="\\W", replace=" ", pjct_sorted$Review)
pjct_sorted$Review<-gsub(pattern="\\d", replace=" ", pjct_sorted$Review)
pjct_sorted$Review<- gsub(pattern="\\b[A-z]\\b{1}", replace="", pjct_sorted$Review)
View(pjct_sorted)


pjct_sorted_1 <- pjct_sorted


positive <- pjct_sorted$Review


pos_score <- polarity(positive)
(pos_counts <- counts(pos_score))

all_1 <- cbind(pjct_sorted,pos_counts)



View(all_1)

View(pos_counts)

library(stringr)

str1 <- pos_counts$pos.words
str2 <- as.character(str1)
neg_str1 <- pos_counts$neg.words
neg_str2 <- as.character(neg_str1)


str3 <- sub('c',' ',str2)
neg_str3 <- sub('c', '' ,neg_str2)

View(str3)
strsplit(str3,' ')

str5 <- str_count(str3, '\\w+')
neg_str5 <- str_count(neg_str3, '\\w+')



str(str2)
View(str5)
str55 <- as.numeric(str5)
str6 <- cbind(str2,str55)
neg_str55 <- as.numeric(neg_str5)
View(str6)
neg_str6 <- cbind(neg_str2,neg_str55)
View(pjct_sorted)

final_5 <- cbind(pjct_sorted,str6,neg_str6)

View(final_5)
library(dplyr)

final_9 <- final_5 %>% select(update,str55,neg_str55)

View(final_9)
str(final_9)

final_9[, 2] <- as.numeric(as.character( final_9[, 2]))
final_9[, 3] <- as.numeric(as.character( final_9[, 3]))


final_9$str55[final_9$str55 >1] <-1
final_9$neg_str55[final_9$neg_str55 >1] <-1

final_10 <- final_9 %>% group_by(update) %>% summarise(Post_freq = n()) %>% arrange(desc(Post_freq))



final_10 <- final_9 %>%
  group_by(update) %>%
  summarize(pos_freq = sum(str55, na.rm = TRUE),
            neg_freq = sum(neg_str55, na.rm = TRUE))

View(final_10)

final_11 <- as.matrix(final_10[1:4,])

View(final_11)
write.csv(final_11, "Pandora.csv")
###########amazon#####################

################################
get_wd()
################################################################################################################
barplot(final_11, col = colors()[c(23,89,12)],border = "white", space=0.04,font.axis=2,xlab = "group",beside = TRUE)
barplot(final_11, beside = TRUE,legend = row.names(final_11), ylab = "no of freq", col = c("light grey"))
 library(ggplot2)
library(reshape2)
library(dplyr)

df <- read.table(text=
                   "tea                coke            beer             water           gender
                 14.55              26.50793651     22.53968254      40              1
                 24.92997199        24.50980392     26.05042017      24.50980393     2
                 23.03732304        30.63063063     25.41827542      20.91377091     1   
                 225.51781276       24.6064623      24.85501243      50.80645161     1
                 24.53662842        26.03706973     25.24271845      24.18358341     2", header=TRUE)

df.melt <- melt(df, id="gender")
bar <- group_by(df.melt, variable, gender)%.%summarise(mean=mean(value))

pjct_S <- read.csv("C:/Users/Dell/Desktop/Desktop/try.csv")

View(pjct_S)

bar2table = table(pjct_S$Update.1,pjct_S$Update.2)

barplot(bar2table, beside = TRUE, main = "FacVar1=level2", legend = levels(unique(final_11$update)))




final_10 <- aggregate(final_9$str5, by=list(Category=final_9$update), FUN=sum)




str2 <- gsub(' {2,}',' ',str1)
str2
strsplit(str2,' ')
strsplit(str2,' ')[[1]]
str2$b <- length(strsplit(str2,' '))

str(pos_counts)
check1 <- as.data.frame(pos_counts[,2:6])
check2 <- pos_counts$pos.words

View(check2)


write.table(check1, file="mydata.csv",sep=",",row.names=F)

check1$new <- toString(check1$pos.words)

head(check2)
View(check1)
str(check1)
check3 <- as.data.frame(check2)

sent_anal <- group_by(data, tailnum)  
  
library(stringr)
check2 %>% group_by(VicRace_label) %>% count()

check1$Count_p <- str_count(check1$pos.words)

View(check1)






pos_score
str(pos_score)
# Get counts
library(stringr)
df$freq<-str_count(df$word,'\\w+')
wordcount(x, sep = " ", count.function = sum)

(pos_counts <- counts(pos_score))

# Number of positive words
pos_c <- str_count(pos_score$pos.words,'\\w+')
View(pos_c)
n_neg <- length(neg_counts$neg.words[[1]])
View(pos_counts)
n_good
n_neg
# Total number of words
n_words <- pos_counts$wc
n_words
# Verify polarity score
n_good / sqrt(n_words)


#














#---------------------------------------------------------------------------------------------------------------#
install.packages("devtools")
library(ggplot2)
install.packages("Ggplot2")
library(easyGgplot2)

ggplot2













