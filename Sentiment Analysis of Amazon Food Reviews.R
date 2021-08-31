require(dplyr)
require(ggplot2)
require(RColorBrewer)
require(rpart)
require(tidyverse)
require(Rgraphviz)
require(RSQLite)

## Importing Data ####

conn <- dbConnect(SQLite(), "C:\\Users\\likhi\\Desktop\\MSDA\\Data Mining 2\\Dataset\\database.sqlite")

data_amazon <- dbGetQuery(conn, "SELECT * FROM Reviews")

head(data_amazon)

str(data_amazon)

dbGetQuery(conn, "SELECT Text
                  FROM Reviews
                  WHERE Text IS NULL")



distinct_products <- dbGetQuery(conn, "SELECT DISTINCT ProductId
                  FROM Reviews ")
dim(distinct_products)


popular_products <- dbGetQuery(conn, "SELECT ProductId
                  FROM Reviews
                  GROUP BY ProductId
                  HAVING COUNT(Text)> 3
                  ORDER BY COUNT(Text) DESC")

dim(popular_products)
head(popular_products)


most_popluar_product <- dbGetQuery(conn, "SELECT Text
                  FROM Reviews
                  WHERE ProductId = 'B007JFMH8M'")

dim(most_popluar_product)


data_retrieved <- dbGetQuery(conn,"SELECT *
          		      FROM Reviews
          		      WHERE Score != 3 
          		      GROUP BY ProductId
                    HAVING COUNT(Text)> 3")
dim(data_retrieved)

dbDisconnect(conn)

## Exploratory Data Analysis####

duplicate_check1 <- filter(data_retrieved, Id == 171104)
duplicate_check2 <- filter(data_retrieved, Id == 217335)
duplicate_check1$Text
duplicate_check2$Text

#Removing duplicate values

data <-  data_retrieved[!duplicated(data_retrieved[c("UserId","Time","Text")]),] 
dim(data)

head(data)

#Converting the UNIX time to real date format

data$Time <- lubridate::as_datetime(data$Time)
data$Year <- lubridate::year(data$Time)

unique(data$Year)

#Defining Target 

data$Negative_Review <- as.factor(data$Score < 3)

head(data[1:4,12])

## Data Visualization ####

user_count <-  data %>% 
  count(UserId)
par(mar=c(9, 9, 2, 1))
user_count <- user_count[order(-user_count$n),]
x <- user_count[1:5,]
cols <- brewer.pal(5, "Set2") 
barplot(height=x$n, names=x$UserId, col=cols, las = 2, main="Top 5 Frequent Reviewers")


x$UserId

top_user_review <- data[data$UserId %in% c("A1YUL9PCJR3JTY","A3OXHLG6DIBRW8", "A281NPSIMI1C2R","A1LZJZIHUPLDV4","A1WX42M589VAMQ"),]

top_user_score <-  top_user_review %>% 
  group_by(UserId) %>% 
  count(Score)

top_user_score$UserId <- factor(top_user_score$UserId)
top_user_score$Score <- factor(top_user_score$Score)     

ggplot(data=top_user_score, aes(x=" ", y=n, group=Score, colour=Score, fill=Score)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  facet_grid(.~ UserId) +theme_void()

review_year <- data %>%
  select(Text, Year) %>%
  group_by(Year) %>%
  summarise(review_count = n())
plot(review_year$Year, review_year$review_count,type = "b", pch = 19, col = "blue",
     main="Total Reviews Received on popular products over year")


review_neg <- data %>%
  group_by(Year) %>%
  count(Negative_Review)
pos_check1 <- as.data.frame(filter(review_neg, Negative_Review == "FALSE"))
neg_check2 <- as.data.frame(filter(review_neg, Negative_Review == "TRUE"))

neg_check2 <- neg_check2 %>% add_row(Year = 2002, n = 0)
neg_check2 <- neg_check2[order(neg_check2$Year),]
review_year <- as.data.frame(review_year)
review_year$Negative_review <- neg_check2$n
review_year$Positive_review <- pos_check1$n
reshape_data <- reshape2::melt(review_year,id.vars="Year")
barchart_data <- reshape_data[12:33,]


ggplot(barchart_data) +  geom_bar(aes(x=Year,y=value,fill=variable), stat="identity",position="dodge")+
  ggtitle("Total Reviews Received on Popular Products Over Years") 


## Data Preprocessing ####

#Convert all letters to lowercase

data$Text <- stringr::str_to_lower(data$Text)

#Remove special character strings such as websites and email

data$Text <- qdapRegex::rm_url(
  data$Text,
  replacement = " ",
  clean = TRUE
)

data$Text <- qdapRegex::rm_hash(
  data$Text,
  replacement = " ",
  clean = TRUE
)
data$Text <- qdapRegex::rm_tag(
  data$Text,
  replacement = " ",
  clean = TRUE
)
data$Text <- qdapRegex::rm_emoticon(
  data$Text,
  replacement = " ",
  clean = TRUE
)
data$Text <- qdapRegex::rm_email(
  data$Text,
  replacement = " ",
  clean = TRUE
)

#Remove stop words

data$Text <- tm::removeWords(
  x = data$Text,
  words = tm::stopwords(kind = "SMART")
)
data$Text <- tm::removeWords(
  x = data$Text,
  words = tm::stopwords(kind = "english")
)
data$Text <- tm::removeWords(
  x = data$Text,
  words = qdapDictionaries::Top200Words
)

data$Text <- trimws(stringr::str_replace_all(
  string = data$Text,
  pattern = "\\s+",
  replacement = " "
))


#Removing Punctuation and Numbers

data$Text <- tm::removePunctuation(
  x = data$Text
) 

data$Text <- tm::removeNumbers(
  x = data$Text
)

#Create Corpus

Corpus_reviews <- iconv(data$Text)
Corpus_reviews <- tm::VCorpus(tm::VectorSource(data$Text))
tm::inspect(Corpus_reviews[[15]])

Corpus_reviews <- tm::tm_map(Corpus_reviews, tm::stripWhitespace)

#Creating Document Term Matrix of the review corpus

DocumentTermMatrix_reviews <- tm::DocumentTermMatrix(Corpus_reviews)

#Remove sparse terms.

DocumentTermMatrix_reviews <- tm::removeSparseTerms(
  DocumentTermMatrix_reviews,
  0.995
)

#Create a integer matrix equivalent to the term document matrix

M <- as.matrix(DocumentTermMatrix_reviews)
M[1:5,1:5]
dim(M)

term_frequency <- data.frame(
  Term = colnames(M),
  Frequency = colSums(M),
  stringsAsFactors = FALSE
)
term_frequency <- term_frequency[order(term_frequency$Frequency),]

term_frequency


wordcloud::wordcloud(
  words = term_frequency$Term,
  freq = term_frequency$Frequency,
  max.words = 25,
  random.order = FALSE,
  colors = viridis::viridis(100)
)


#Removing some uninformative words

Corpus_reviews <- tm::tm_map(Corpus_reviews,tm::removeWords, c("amazon", "order","buy","food","product","bought","add","knowing","common"))


DocumentTermMatrix_reviews <- tm::DocumentTermMatrix(Corpus_reviews)
DocumentTermMatrix_reviews <- tm::removeSparseTerms(
  DocumentTermMatrix_reviews,
  0.995
)
M <- as.matrix(DocumentTermMatrix_reviews)
term_frequency <- data.frame(
  Term = colnames(M),
  Frequency = colSums(M),
  stringsAsFactors = FALSE
)
term_frequency <- term_frequency[order(term_frequency$Frequency),]

wordcloud::wordcloud(
  words = term_frequency$Term,
  freq = term_frequency$Frequency,
  max.words = 25,
  random.order = FALSE,
  colors = viridis::viridis(100)
)


dim(term_frequency)

barplot(term_frequency[1174:1170,]$Frequency, las = 2, names.arg = term_frequency[1174:1170,]$Term,
        col ="purple", main ="Top 5 most frequent words",
        ylab = "Word frequencies")


term_frequency[1174:1165,]$Term

Network <- as.data.frame(M)
Bn <- Network[,c("tea", "taste", "flavor", "coffee","love" ,"price","sugar",    "chocolate" ,"eat", "bag"  )]


model_amazon <- bnlearn::hc(Bn)
bnlearn::graphviz.plot(model_amazon,
                       shape = "ellipse")


## Using syuzhet Lexicon for sentiment analysis ####

sentiment <- syuzhet::get_nrc_sentiment(data$Text)
head(sentiment)

sentiment_M <- as.matrix(sentiment)
barplot(sentiment_M, border = "dark blue",las = 2)

## Building a model for sentiment prediction ####

review_df <- as.data.frame(M)
colnames(review_df) <- make.names(colnames(review_df))
review_df$Negative_reviews <- data$Negative_Review

set.seed(813)
split <- caTools::sample.split(review_df$Negative_reviews, SplitRatio=0.7)
train_reviews <- subset(review_df, split == TRUE)
test_reviews <- subset(review_df, split == FALSE)

#Baseline Model

table(test_reviews$Negative_reviews)

#Then the accuracy of out baseline model is 5377/(5377+711) = 0.883

#CART Model

review_model <- rpart::rpart(Negative_reviews ~ ., data = train_reviews, method ="class")
predict_m <- predict(review_model, newdata = test_reviews, type="class")

cm <- caret::confusionMatrix(test_reviews$Negative_reviews,predict_m)
fourfoldplot(cm$table)

cm

#SVM Model

svm_model <- e1071::svm(Negative_reviews ~ ., data = train_reviews)
predict_svm <- predict(svm_model, test_reviews)

cm2 <- caret::confusionMatrix(test_reviews$Negative_reviews,predict_svm)
fourfoldplot(cm2$table)

cm2 






















