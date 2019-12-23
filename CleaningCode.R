setwd('ST309/yelp-dataset')
#---------------------------------------load libraries
library(cld3)
library(tidytext) 
library(dplyr) 
library(stringr)
library(tidyr)
library(ggplot2)
library(wordcloud)
library(reshape2)
library(igraph)
library(ggraph)
library(ROCR)
library(randomForest)
library(maptree)
library(tree)
library(leaflet)
library(DT)
library(tidyverse) 
library(lubridate)
library(data.table)
#---------------------------------------import dataset chunks
r = readr::read_csv('yelp_review.csv')
b = readr::read_csv('yelp_business.csv')
ba = readr::read_csv('yelp_business_attributes.csv')
bh = readr::read_csv('yelp_business_hours.csv')
#--------------------------------------------------------------
setDT(r)[,c("review_id","user_id"):=NULL] # remove review_id and user_id as we will not need it when we merge the data
colSums(sapply(r, is.na)) #we can see r has no missing values
colSums(sapply(b, is.na)) #we can see that neighorhood is missing a lot of values, so we will remove it as it will not help with our analysis
setDT(b)[,c("neighborhood"):=NULL]
colSums(sapply(b, is.na)) #we can see that there are missing values in 5 categories: longitude,latitude,state,city, postal code
#we will remove the corresponding values once we merge the datasets
attach(r)
ur =length(unique(business_id))
detach(r)
attach(b)
ub =length(unique(business_id))
detach(b)
ur == ub #we can see that the amount of unique business ids in r and b datasets is the same, hence we can assume that all businesses are represented in teh reviews
rm(ur,ub)

#hence we can merge the datasets
merged=merge(b,r, by='business_id', all=TRUE) #no rows or unique columns lost, successful merging
merged=merge(merged,bh, by='business_id', all=TRUE) #no rows or unique columns lost, successful merging

colSums(sapply(ba, is.na)) # no missing values in the ba dataset
#ba has a shorter lenght than b, implying that some businesses don't have reported attributes, however they do have reviews
#therefore we will ommit businesses without the business attributes

merged=merge(merged,ba, by='business_id', all=FALSE)
nrow(r)-nrow(merged)#  197895 reviews were omitted as a result of the merging, this is fine since we still have over 5 million observations
rm(bh,r)
#check for missing values in the merged dataset
colSums(sapply(merged, is.na)) #missing values in postal code, longitude, latitude and city
attach(merged)
lo =apply(is.na(t(longitude)),c(1), which) #25 missing values in longitude
la =apply(is.na(t(latitude)),c(1), which) #25 missing values in latitude
length(lo)==length(la) # same length
pc = apply(is.na(t(postal_code)),c(1), which) #4402 missing values in postal code
c =apply(is.na(t(city)),c(1), which) #5 missing values in city
mean(lo %in% la)# missing values in longitude and latitude are in the same rows
mean(lo %in% pc)# no overlap between postal code and longitude/latitude missing values
mean(lo %in% c)# no overlap between city and longitude/latitude missing values
mean(c %in% pc)# no overlap between postal code and city missing values
emv = length(lo)+length(pc)+length(c) # expected missing values total  = 4432
rm(c,la,lo,pc,emv)
detach(merged)
cleaned = na.omit(merged)
nrow(merged)-nrow(cleaned) #4432 reviews were omitted due to na values in key geographical characteristics
rm(merged)
#we can see that the amount of omitted values is equal to our emv hence we can deduce all the missing values have been removed
#additional check for missing values
colSums(sapply(cleaned, is.na))
#we can deduce that the cleaned dataframe is complete

#The datset is too large as is
#Generate subsets to make the process smoother
#Using categories variable to decide on a category subset
categories = strsplit(cleaned$categories,";")
categories = as.data.frame(unlist(categories))
colnames(categories) = c("Name")
categories %>%
  group_by(Name) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(Name = reorder(Name,Count)) %>%
  head(10) %>%
  
  
  ggplot(aes(x = Name,y = Count)) +
  geom_bar(stat='identity',colour="white", fill =c(cm.colors(10))) +
  geom_text(aes(x = Name, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Name of Category', y = 'Count', 
       title = 'Top 10 Categories of Business Reviews') +
  coord_flip() + 
  theme_bw()
#cft has allowed us to deduce the most frequent categories
#we will generate subset for Restaurants
Rs = subset(cleaned, grepl("Restaurants", cleaned$categories))
city = strsplit(Rs$city,";")
city = as.data.frame(unlist(city))
colnames(city) = c("Name")
city %>%
  group_by(Name) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(Name = reorder(Name,Count)) %>%
  head(10) %>%
  
  
  ggplot(aes(x = Name,y = Count)) +
  geom_bar(stat='identity',colour="white", fill =c(heat.colors(10))) +
  geom_text(aes(x = Name, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Name of Category', y = 'Count', 
       title = 'Top 10 Most Restaurant Reviews per City') +
  coord_flip() + 
  theme_bw()

#still very large, preferences vary by region, so we will use one city for analysis
#We can see that Las Vegas is most frequent, however the city is very unique
#So we will use Phoenix instead which is the second largest
Phoenix = subset(Rs, grepl("Phoenix", Rs$city))
rm(cleaned,Rs)

#Regression analysis
rb = subset(b, grepl("Restaurants", b$categories))
Phoe = subset(rb, grepl("Phoenix", rb$city))
Ph=merge(Phoe,ba, by='business_id', all=FALSE) # loss of 36 observations after merge
colSums(sapply(ba, is.na))
Ph = na.omit(Ph) #loss of 4 observations

for (i in 13:93){
  t =table(Ph[,..i])
  print("-------------------")
  print(colnames(Ph[,..i]))
  print(i)
  print(t)
}
Ph = Ph[, -c(4, 5,6,12:16, 21:32, 33:93)]

attach(Ph)
barplot(table(Ph$is_open), main = 'Open vs Closed Restaurants in Phoenix', xlab = 'Open', col = c('red', 'light green'))
library(leaflet)
PhO = Ph[Ph$is_open==1]
dim(PhO)
PhC = Ph[Ph$is_open==0]
dim(PhC)
base =1063/3608 #base misclassification rate
basemap = addProviderTiles(leaflet(),'OpenStreetMap.DE', group = 'OSM(default)')
pmap = addCircleMarkers(setView(basemap, lng=-112.0728, lat=33.44277,zoom = 11), lng = PhO$longitude, lat = PhO$latitude, radius = 1.5, fillOpacity = 0.1, col = c('purple'))
pmap
hist(Ph$stars, main = 'Distribution of Stars for Restaurants in Phoenix', xlab = 'Stars', col = rainbow(length(unique(Ph$stars))))


# convert is_open into a factor
Open =ifelse(is_open==0, 'No', 'Yes')
Open = as.factor(Open)
Ph1 = data.frame(Ph,Open)
#convert business factors to binary variables, NAs treated as 0
BPs =ifelse(BusinessParking_street=='True', 1, 0)
Ph1 = data.frame(Ph1,BPs)
BPv =ifelse(BusinessParking_validated=='True', 1, 0)
Ph1 = data.frame(Ph1,BPv)
BPva =ifelse(BusinessParking_valet=='True', 1, 0)
Ph1 = data.frame(Ph1,BPva)
BPl =ifelse(BusinessParking_lot=='True', 1, 0)
Ph1 = data.frame(Ph1,BPl)
detach(Ph)
Ph1 = Ph1[,-c(1:3,8:12)] # removed business_id, name, address, the original variables
#convert location to logs since subset raw values will be very similar, more accurate representation of variation
Ph1$latitude = log(Ph1$latitude)
Ph1$longitude = log(abs(Ph1$longitude))
#convert review count to logs to get a clearer understanding how reviews contribute to success
hist(Ph$review_count) #heavily skewed
Ph1$review_count = log(Ph1$review_count)
hist(Ph1$review_count) #makes the ditribtuion less skewed

set.seed(12)
train = sample(1:nrow(Ph1),1000)
TrD = Ph1[train,]
TsD = Ph1[-train,]
treet =tree(Open~., TrD)
treep =predict(treet, TsD, type='class')
Ots = Open[-train]
t = data.frame(table(treep,Ots))
draw.tree(treet, cex=0.8,digits = 3)
mclassrate1 = (t$Freq[2]+t$Freq[3])/(t$Freq[1]+t$Freq[2]+t$Freq[3]+t$Freq[4])
p = 'Misclassification Error Rate:'
print(paste(p,mclassrate1,sep = ''))
cv.treet = cv.tree(treet,FUN = prune.misclass)
cv.treet$size
cv.treet$dev
prune.treet = prune.misclass(treet, best = 6)
treep =predict(prune.treet, TsD, type='class')
t = data.frame(table(treep,Ots))
mclassrate3 = (t$Freq[2]+t$Freq[3])/(t$Freq[1]+t$Freq[2]+t$Freq[3]+t$Freq[4])
p = 'Misclassification Error Rate:'
print(paste(p,mclassrate3,sep = ''))
draw.tree(prune.treet, cex=0.8,digits = 5)
#random forest analysis
bag.Ph1 = randomForest(Open ~., data = Ph1, subset = train, mtry = 3, importance=T)
bag.Ph1
s = glm(Open~., data = TrD, family = binomial)
summary(s) # we will use step regression to remove insignificant variables from the model
StepS = step(s, direction = 'backward', test ='F')
#remove remaining variables systematically to find the most significant variables
ts = glm(Open~+BPva+BPs+BPv+longitude+latitude+review_count, data = TrD, family = binomial) #initial model after StepS
ts = glm(Open~+BPva+BPs+BPv+longitude+review_count, data = TrD, family = binomial)#removed latitude
ts = glm(Open~+BPva+BPv+longitude++review_count, data = TrD, family = binomial)#removed BPs since it was only significant at the 10% level
summary(ts) #We can see that all variables are at least significnat at the 1% level
predT=predict(prune.treet, TsD, type ='vector')
predTest.tree = predT[,2]
predTs = plogis(predict(ts,TsD))
PtreeT = prediction(predTest.tree, TsD$Open)
PregsT = prediction(predTs, TsD$Open)

rtree = performance(PtreeT, measure = "tpr", x.measure = "fpr")
rregs = performance(PregsT, measure = "tpr", x.measure = "fpr")
plot(rtree, colorkey = T, colorize = T, main = "ROC Curve on Classification Tree")
abline(0,1)
auct=performance(PtreeT, measure='auc')@y.values
plot(rregs, colorkey = T, colorize = T, main = "ROC Curve on Step Regression")
abline(0,1)
aucl=performance(PregsT, measure='auc')@y.values

#Text analysis

language = detect_language(Phoenix$text)
Lang =head(sort(table(language), decreasing = TRUE)) #look at language distribution of reviews
Lang_table =data.table(language)
table(Lang_table)
PH = subset(Phoenix, language =='en') #we only keep the reviews in English

PH2 = Phoenix[, c(2, 9, 11, 13:15)] #remove all the variables but name, stars.x, is_open, stars.y, date, and text

counts.open <- table(PH2$is_open) #looking at distribution of reviews by closed vs open restaurants
counts.open
co <- data.frame(counts.open)
pie <- pie(co$Freq[1:2], labels = c("Closed - 14%","Open - 86%"), radius = 1.5, col=c("red", "light green"), main = "Distribution of Reviews for Restaurants
           in Phoenix by Restaurant Status ")

#looking at correlation between Yelp stars and restaurant opening status
cor(PH2$is_open, PH2$stars.x) #0.1182327

attach(PH2)
stars=vector(length = 329408) #Creating a new variable where stars will be combined by .5
stars[stars.x==5]=5
stars[stars.x==4.5 | stars.x==4]=4
stars[stars.x==3.5 | stars.x==3]=3
stars[stars.x==2.5 | stars.x==2]=2
stars[stars.x==1.5 | stars.x==1]=1
detach(PH2)

PH2=data.frame(PH2,stars) #adding the new stars variable to the dataframe 

#comparing the offial stars of restaurants with those given in the reviews
Restaurants = as.data.frame(table(unlist(PH2$stars))) 
Reviews = as.data.frame(table(unlist(PH2$stars.y)))
SS <- read.table(
  header=TRUE, text='Category        Stars Frequency
  1  Restaurant    1      1551
  2  Reviews       1      35342
  3  Restaurant    2      17330
  4  Reviews       2      29369
  5  Restaurant    3      105192
  6  Reviews       3      39175
  7  Restaurant    4      202344
  8  Reviews       4      86064
  9  Restaurant    5      2991
  10 Reviews       5      139458')
ggplot(SS, aes(Stars, Frequency, fill = Category)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Star Frequency by Restraurants and Reviews")

#text analysis - cleaning the text and making it 'tidy' + separating stop and non-stop words
reviews <- group_by(PH2, name)%>%mutate(reviewer = row_number()) #created the variable reviewer so that no information about words belonging to the same review would be lost
PH_tidy <- reviews %>% unnest_tokens(word, text)
PH_tidy
data(stop_words)
PH_noS <- anti_join(PH_tidy, stop_words)
PH_stop <- semi_join(PH_tidy, stop_words)

#sentiment analysis - net sentiment
sentiments
get_sentiments("bing")
reviews_sentiment <- PH_tidy %>%
  inner_join(get_sentiments("bing")) %>%
  count(stars, index = reviewer, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(reviews_sentiment, aes(index, sentiment, fill = stars)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~stars, ncol = 2, scales = "free_x")+
  ggtitle("Sentiment Scores by Star Through Reviews") #graph showing net sentiment by stars

# common positive and negative words
bing_word_counts <- PH_tidy %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(50) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip() +
  scale_fill_brewer(palette = "Set1")+
  ggtitle("Most Common Negative and Positive Words") #graph showing most common positive and negative words

#wordclouds
PH_noS %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 150)) #word cloud by frequency

PH_noS %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 150) #word cloud by frequency positive vs negative

#frequency
PH_noS %>%
  count(word, sort = TRUE) %>%
  filter(n > 1000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()+
  ggtitle("Most Frequent Words") #plot of most frequent words

## Frequency analysis and term importance
review_words <- unnest_tokens(PH2, word, text) %>%
  count(stars, word, sort = TRUE) %>%
  ungroup()

total_words <- review_words %>% 
  group_by(stars) %>% 
  summarize(total = sum(n))

review_words <- left_join(review_words, total_words)
review_words

ggplot(review_words, aes(n/total, fill = stars)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~stars, ncol = 2, scales = "free_y")+
  ggtitle("Term Frequency Distribution") #distribution of term frequencies

freq_by_rank <- review_words %>% 
  group_by(stars) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total)
freq_by_rank

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = stars)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()+
  ggtitle("Zipf’s Law for the Reviews") #Zipf’s law
#We see that the reviews for all types of restaurants are similar to each other, and that the relationship between rank and frequency does have negative slope.

rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)
#slope of line close to -1

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = stars)) + 
  geom_abline(intercept = -0.62, slope = -1.1, color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()+
  ggtitle("Fitting an Exponent for Zipf’s Law on the Reviews") #Fitting an exponent for Zipf’s law
# This result is similar to the findings of Zipf’s law. We have deviations at high rank, which are expected given the nature of the text.

#tf_idf
review_words <- review_words %>%
  bind_tf_idf(word, stars, n)
review_words

review_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

review_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(stars) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = stars)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~stars, ncol = 2, scales = "free") +
  coord_flip()+
  ggtitle("Highest Tf-idf Words in Reviews by Star") #plot of tf_idf

#bigrams analysis

count_bigrams <- function(PH2) {
  PH2 %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}

visualize_bigrams <- function(bigrams) {
  set.seed(2019)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "kk") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}

PH_bigrams <- PH2 %>%
  count_bigrams()

PH_bigrams %>%
  filter(n > 2500,
         !str_detect(word1, "\\d"),
         !str_detect(word2, "\\d")) %>%
  visualize_bigrams()

