#project precpocess
#cleaning and description statistics

#load the data
dataset <- read.csv("C:/Users/daisy/OneDrive/Study/DM/Project/dataset/fake.csv", header = T, sep = ",")
#check the data
dataset[1:3,]

#delete the useless attributs
dataset = dataset[-1]

#check the missing data in dataset
sum(is.na(dataset))
#there are 4223 missing data, but we are not sure where are they and what is the missing patrern
#next we can check the missing data pattern
library(mice)
md.pattern(dataset)
#The output shows us that 8776 sample are complete, 4423 sample miss only the domain rank
#we can igonre the domain_rank for some time and using the whole sample at first.

library(VIM)
aggr_plot <- aggr(dataset, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
                  labels=names(dataset), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))

#Next we figure out the numerice and categorical data in the dataset and 
#make different plots for two kinds of data

#numeric: order_in_thread, domain_rank, spam_score, replies_count, partic_count, likes, comments, shares
#categorical: language, siteurl, conutry, maining_url, type
#text: author, title, text, thread_title, 

#make a summary table for numeric attributes
ord_in_thread = c(summary(dataset$ord_in_thread), sd(dataset$ord_in_thread))
domain_rank = c(summary(dataset$domain_rank), sd(dataset$domain_rank))
spam_score = c(summary(dataset$spam_score), sd(dataset$spam_score))
replies_count = c(summary(dataset$replies_count), sd(dataset$replies_count))
parcp_count = c(summary(dataset$participants_count), sd(dataset$participants_count))
likes = c(summary(dataset$likes), sd(dataset$likes))
comments = c(summary(dataset$comments), sd(dataset$comments))
shares = c(summary(dataset$shares), sd(dataset$shares))
result = rbind(ord_in_thread, domain_rank, spam_score, replies_count, parcp_count,
               likes, comments, shares)
result = as.data.frame(result)
colnames(result)[7] = c("sd")
library(knitr)
kable(result, caption = 'Table 1: Summary of attributes')


#density plot for numeric variable
library(ggplot2)
theme_set(theme_bw())
ggplot(data = dataset, aes(x = ord_in_thread)) + geom_density()
ggplot(data = dataset, aes(x = spam_score)) + geom_density()
ggplot(data = dataset, aes(x = replies_count)) + geom_density()
ggplot(data = dataset, aes(x = participants_count)) + geom_density()
ggplot(data = dataset, aes(x = likes)) + geom_density()
ggplot(data = dataset, aes(x = comments)) + geom_density()
ggplot(data = dataset, aes(x = shares)) + geom_density()
#nearly all the numeric variables are left skewed
#and they all show a climax at around 0

#normal distribution check
library(e1071)
skewness(dataset$ord_in_thread)
skewness(dataset$spam_score)
skewness(dataset$replies_count)
skewness(dataset$participants_count)
skewness(dataset$likes)
skewness(dataset$comments)#48
skewness(dataset$shares)

#Draw a normal probability plot (q-q plot), 
#and check if the distribution is approximately forms a straight line.
qqnorm(dataset$spam_score)
qqline(dataset$spam_score)

qqnorm(dataset$comments)
qqline(dataset$comments)

#check the relationship with each numeric predictor and response variable through correlation and scatterplot
#correlation
library(car)
dt = dataset[,c('ord_in_thread','spam_score','replies_count','participants_count','likes','comments','shares')]
cor(dt)

#scatterplot
scatterplotMatrix(dt, spread = FALSE, lty.smooth = 2, main = 'Scatter Plot Matrix')

#for categorical predictor, generate conditional density plot of response varibles
#convert the some of the categorical variables to factor so that we can make a plot
dataset$language = as.factor(dataset$language)
dataset$site_url = as.factor(dataset$site_url)
dataset$country = as.factor(dataset$country)
dataset$main_img_url = as.factor(dataset$main_img_url)
dataset$type = as.factor(dataset$type)

#check the levels for categorical data
levels(dataset$language)
levels(dataset$country)
levels(dataset$type)

#histogram plot for categorical variables
ggplot(dataset, aes(x=spam_score)) + geom_histogram(aes(group=language, fill=language), alpha=.5)
ggplot(dataset, aes(x=spam_score)) + geom_density(aes(group=language, fill=language), alpha=0.05)

#ggplot(dataset, aes(x=spam_score)) + geom_histogram(aes(group=site_url, fill=site_url), alpha=.5)

ggplot(dataset, aes(x=spam_score)) + geom_histogram(aes(group=country, fill=country), alpha=.5)

ggplot(dataset, aes(x=spam_score)) + geom_histogram(aes(group=type, fill=type), alpha=.5)

#Check the conditional density plot for spam-score over 0.5
dataset2 = dataset[which(dataset$spam_score>=0.5),]
ggplot(dataset2, aes(x=spam_score)) + geom_histogram(aes(group=language, fill=language), alpha=.5)
ggplot(dataset2, aes(x=spam_score)) + geom_density(aes(group=language, fill=language), alpha=0.05)
#ggplot(dataset, aes(x=spam_score)) + geom_histogram(aes(group=site_url, fill=site_url), alpha=.5)
ggplot(dataset2, aes(x=spam_score)) + geom_density(aes(group=country, fill=country), alpha=.5)
ggplot(dataset2, aes(x=spam_score)) + geom_density(aes(group=type, fill=type), alpha=.5)

#For each categorical predictor, compare and 
#describe whether the categories have significantly different means.
language_comp = aov(spam_score ~ language, data = dataset) 
summary(language_comp)
#By studying the above outputs of ANOVA table, we see that F-statistic is 25.69 with a p-value less than 2e-16.
#We clearly reject the null hypothesis of equal means for all language groups.

country_comp = aov(spam_score ~ country, data = dataset) 
summary(country_comp)
#By studying the above outputs of ANOVA table, we see that F-statistic is 17.86 with a p-value less than 2e-16.
#We clearly reject the null hypothesis of equal means for all country groups.

type_comp = aov(spam_score ~ type, data = dataset) 
summary(type_comp)
#By studying the above outputs of ANOVA table, we see that F-statistic is 3.321 with a p-value is 0.00155.
#We nearly can accept the null hypothesis of equal means for all type groups.

#computes the pair-wise t-test between each pair of groups for spam_score and type
pairwise.t.test(dataset$spam_score, dataset$type)
#
group1 = subset(dataset, type == "fake", select = spam_score)
group2 = subset(dataset, type == "bias", select = spam_score)
t.test(group1, group2)
#The outputs inform us that the spam_score mean for type == "fake" is 0.08447368 and 
#for type == "bias" is 0.02917156. They are not significantly different (p-value = 0.2335). 
#The 95% confidence interval of the difference in mean AmountSpent is between 0.149 and 0.0388. 

#########################################################################
#text preprocess
Sys.setenv(NOAWT= "true") 
library(tm)
library(lsa)

text_data = dataset[c(5)]

topic = factor(rep(c("big data", "global warming", "gov shutdown"), each = 4333))
topic = as.data.frame(topic)

df = cbind(text_data, topic)
df1 = df[1:4333,]
df2 = df[4334:8666,]
df3 = df[8667:12999,]
df[1:3,]

## prepare corpus
corpus_prepare <- function(dataset){
  corpus = Corpus(VectorSource(dataset$text))
  corpus = tm_map(corpus, tolower) ## convert text to lower case
  #inspect(corpus[1:3])  
  
  corpus = tm_map(corpus, removePunctuation) ## remove punctuations
  #inspect(corpus[1:3]) 
  
  corpus = tm_map(corpus, removeNumbers) ## remove numbers
  #inspect(corpus[1:3])
  
  corpus = tm_map(corpus, function(x) removeWords(x, stopwords("english"))) ## remove stopwords
  #inspect(corpus[1:3]) 
  
  corpus = tm_map(corpus, stemDocument, language = "english") ## stemming
  #inspect(corpus[1:3])
  return(corpus)
}

corpus = corpus_prepare(df1)
corpus2 = corpus_prepare(df2)
corpus3 = corpus_prepare(df3)

td_mat1<- as.matrix(TermDocumentMatrix(corpus, control=list(bounds = list(global = c(5,Inf)))))
td_mat2<- as.matrix(TermDocumentMatrix(corpus2, control=list(bounds = list(global = c(5,Inf)))))
td_mat3<- as.matrix(TermDocumentMatrix(corpus3, control=list(bounds = list(global = c(5,Inf)))))
td_mat1[1:3,]

library(wordcloud)

cloud <- function(td.mat){
  m = as.matrix(td.mat)
  ## calculate the frequency of words
  v = sort(rowSums(m), decreasing=TRUE) 
  words = names(v)
  wc = data.frame(word=words, freq=v)
  wc[1:3,]
  wordcloud(wc$word, wc$freq, scale = c(1, 0.2), min.freq=500)
  return(wc)
}


wordfreq1 = cloud(td_mat1)
wordfreq1[1:3,]

wordfreq2 = cloud(td_mat2)
wordfreq3 = could(td_mat3)


