# clean data script

data<-"/home/rw13742/Documents/datashield/amased/data/bl_csv_1_book/txt/data.txt"
data.txt<-read.table(data)
class(data.txt)

library(tm)
docs<-Corpus(VectorSource(data.txt))
inspect(docs)

#output
#<<PlainTextDocument>>
#Metadata:  7
#Content:  chars: 236304


######################################################################
# DATA CLEANING
########################################################################
# remove numbers
# remove punctuation
# remove english stop words (common words e.g. the, and, it)
# remove custom stop words
# replace symbols with white space
# remove whitespace
# conversion to lowercase
# stemming > removes common english word endings e.g. -es. -ed -s to provide the word stem

#####################################################################
#function to transform content into spaces
#####################################################################

#toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))

#convert "X" to space
#docs <- tm_map(docs, toSpace, "/")
#docs <- tm_map(docs, toSpace, "@")
#docs <- tm_map(docs, toSpace, "\\|")

######################################################################
# functions to remove things to create pure text
######################################################################

#remove numbers
docs <- tm_map(docs, removeNumbers)
inspect(docs)

#output
#<<PlainTextDocument>>
#Metadata:  7
#Content:  chars: 236274

#remove punctuation
docs <- tm_map(docs, removePunctuation)
inspect(docs)

#output
#<<PlainTextDocument>>
#Metadata:  7
#Content:  chars: 227016

#remove english stop words (common words  e.g. the)
docs <- tm_map(docs, removeWords, stopwords("english"))
inspect(docs)
#output
#<<PlainTextDocument>>
#Metadata:  7
#Content:  chars: 175749


#list stop words
stopwords("english")

#remove own stopwords
#docs <- tm_map(docs, removeWords, c("xxxxx", "xxxx"))

#remove whitespace
docs <- tm_map(docs, stripWhitespace)
inspect(docs)
#output
#<<PlainTextDocument>>
#Metadata:  7
#Content:  chars: 156178


#################################################################
# Stemming
#################################################################

library(SnowballC)
docs <- tm_map(docs, stemDocument)
inspect(docs)
#output
#<<PlainTextDocument>>
#Metadata:  7
#Content:  chars: 138365


#################################################################
# Document Term Matrix
#################################################################

dtm <- DocumentTermMatrix(docs)
dtm

#output
#<<DocumentTermMatrix (documents: 1, terms: 4358)>>
#Non-/sparse entries: 4358/0
#Sparsity           : 0%
#Maximal term length: 18
#Weighting          : term frequency (tf)


#dimensions 1 row (frequency of word) and 6632 columns (words)
class(dtm)
dim(dtm)
#[1]    1 4358

#print to screen the dtm 1 columns 1:1000)
inspect(dtm[1, 1:1000])

#transpose the dtm so columns become rows
tdm <- TermDocumentMatrix(docs)
tdm

#output
#<<TermDocumentMatrix (terms: 4358, documents: 1)>>
#Non-/sparse entries: 4358/0
#Sparsity           : 0%
#Maximal term length: 18
#Weighting          : term frequency (tf)

#################################################################
# Exploring dtm
#################################################################
#number of rows/ individual words
freq <- colSums(as.matrix(dtm))
length(freq)

#order the words
ord <- order(freq)

# Least frequent terms
freq[head(ord)]

# Most frequent terms
freq[tail(ord)]

#freq[head(ord)]
#  aain  aback  abash   abil abject abomin 
#     1      1      1      1      1      1 
#> 
#> # Most frequent terms
#> freq[tail(ord)]
#    delachain           the          said         aggie        bertha 
#          137           155           161           191           302 
#â\u0080\u0094 
#          338 


#write to csv file
#m <- as.matrix(dtm)
#dim(m)
#write.csv(m, file="/home/rw13742/Documents/datashield/amased/data/bl_csv_1_book/txt/dtm.csv")

###################################################################################
#remove sparse terms
############################################################################################
dtms <- removeSparseTerms(dtm, 0.8) #NOT WORKING!
dim(dtms)

inspect(dtms)
freq <- colSums(as.matrix(dtms))
freq
table(freq)

#####################################################################
#EXPLORATORY STATS
#####################################################################
#find frequent terms words with 100 or more frequency in the text.
findFreqTerms(dtm, lowfreq=50)

#associations between a specific word 
findAssocs(dtm, "ask", corlimit=0.1) #NOT WORKING!

#plot associations graphically - NOT WORKING!
plot(dtm,
terms=findFreqTerms(dtm, lowfreq=100)[1:50],
corThreshold=0.5)

#word frequency bar graph. sort frequencies in descending order
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
#check
#head(freq, 14)

wf <- data.frame(word=names(freq), freq=freq)
#check
#head(wf)

#plot words by frequency if frequency is >100
library(ggplot2)
subset(wf, freq>100) %>%
ggplot(aes(word, freq)) +
geom_bar(stat="identity") +
theme(axis.text.x=element_text(angle=45, hjust=1))

#print to file
#png("/home/rw13742/Documents/datashield/amased/data/bl_csv_1_book/txt/bar.png", width=12, #height=8, units="in", res=300)
#subset(wf, freq>100) %>%
#ggplot(aes(word, freq)) +
#geom_bar(stat="identity") +
#theme(axis.text.x=element_text(angle=45, hjust=1))
#dev.off()

#wordcloud
library(wordcloud)
set.seed(123)
wordcloud(names(freq), freq, min.freq=40)
#use of set.seed() only so that we can obtain smae layout each time
#see wordcloud library on beautifying the output!
set.seed(123)
wordcloud(names(freq), freq, min.freq=40, scale=c(8, 0.3), colors=brewer.pal(6, "Dark2"))

#to print to file
#png("/home/rw13742/Documents/datashield/amased/data/bl_csv_1_book/txt/cloud.png", width=12, #height=8, units="in", res=300)
#set.seed(123)
#wordcloud(names(freq), freq, min.freq=40, scale=c(8, 0.3), colors=brewer.pal(6, "Dark2"))
#dev.off()

#convert tdm into a matrix, extract the column
#names (the terms) and retain those shorter than 20 character
words <- dtm %>%
as.matrix %>%
colnames %>%
(function(x) x[nchar(x) < 20])
length(words)



#first 15 words
head(words, 15)
dist_tab(nchar(words))
summary(nchar(words))
#output
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  3.000   5.000   6.000   5.966   7.000  18.000 

table(nchar(words))
#  3   4   5   6   7   8   9  10  11  12  13  14  15  18 
# 262 784 946 897 665 363 203 138  47  22  22   6   2   1 

dist_tab(nchar(words))
#   interval freq cum.freq percent cum.percent
#1         3  262      262    6.01        6.01
#2         4  784     1046   17.99       24.00
#3         5  946     1992   21.71       45.71
#4         6  897     2889   20.58       66.29
#5         7  665     3554   15.26       81.55
#6         8  363     3917    8.33       89.88
#7         9  203     4120    4.66       94.54
#8        10  138     4258    3.17       97.71
#9        11   47     4305    1.08       98.78
#10       12   22     4327    0.50       99.29
#11       13   22     4349    0.50       99.79
#12       14    6     4355    0.14       99.93
#13       15    2     4357    0.05       99.98
#14       18    1     4358    0.02      100.00

#word length counts/distribution

#png("/home/rw13742/Documents/datashield/amased/data/bl_csv_1_book/txt/character_bar.png", width=12, height=8, units="in", res=300)
data.frame(nletters=nchar(words)) %>%
ggplot(aes(x=nletters)) +
geom_histogram(binwidth=1) +
geom_vline(xintercept=mean(nchar(words)),
colour="green", size=1, alpha=.5) +
labs(x="Number of Letters", y="Number of Words")
#dev.off()

#review the frequency of letters across all of the words in the discourse. Some
#data preparation will transform the vector of words into a list of letters, which we then construct
#a frequency count for, and pass this on to be plotted

library(dplyr)
library(stringr)

#png("/home/rw13742/Documents/datashield/amased/data/bl_csv_1_book/txt/letters.png", width=12, height=8, units="in", res=300)
words %>%
str_split("") %>%
sapply(function(x) x[-1]) %>%
unlist %>%
dist_tab %>%
mutate(Letter=factor(toupper(interval),
levels=toupper(interval[order(freq)]))) %>%
ggplot(aes(Letter, weight=percent)) +
geom_bar() +
coord_flip() +
ylab("Proportion") +
scale_y_continuous(breaks=seq(0, 12, 2),
label=function(x) paste0(x, "%"),
expand=c(0,0), limits=c(0,12))
#dev.off()

#letter-posiotion heat map

#png("/home/rw13742/Documents/datashield/amased/data/bl_csv_1_book/txt/letter_position.png", width=12, height=8, units="in", res=300)
words %>%
lapply(function(x) sapply(letters, gregexpr, x, fixed=TRUE)) %>%
unlist %>%
(function(x) x[x!=-1]) %>%
(function(x) setNames(x, gsub("nnd", "", names(x)))) %>%
(function(x) apply(table(data.frame(letter=toupper(names(x)),
position=unname(x))),
1, function(y) y/length(x))) %>%
qheat(high="green", low="yellow", by.column=NULL,
values=TRUE, digits=3, plot=FALSE) +
ylab("Letter") +
xlab("Position") +
theme(axis.text.x=element_text(angle=0)) +
guides(fill=guide_legend(title="Proportion"))
#dev.off()

