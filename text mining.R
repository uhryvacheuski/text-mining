#LAB 2
library(tm)
library(snowballc)
library(ggplot2)
library(wordcloud)
library(cluster)
library(fpc)
library(topicmodels)
library(igraph)
library(tidyverse) # data manipulation
library(cluster) # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(purrr)
library(fpc)

wd <- "/Users/vladgrivacevskij/TM/stem"
setwd(wd)
getwd() #get current working directory
dir(wd)

#Creating corpus
docs <- Corpus(DirSource(wd))

#Length = 1883 documents
length(docs)

#Removing punctuation and numbers
docs <- tm_map(docs, removePunctuation) 
docs <- tm_map(docs, removeNumbers) 
writeLines(as.character(docs[[1]])) #let's see what we have
#docs <- tm_map(docs, PlainTextDocument)

#remove special chars
for (j in seq(docs)){
  docs[[j]] <- gsub("/", " ", docs[[j]])
  docs[[j]] <- gsub("@", " ", docs[[j]])
  docs[[j]] <- gsub("–", " ", docs[[j]])
  docs[[j]] <- gsub("’", " ", docs[[j]])
  docs[[j]] <- gsub("“", " ", docs[[j]])
  docs[[j]] <- gsub("‘", " ", docs[[j]])
  docs[[j]] <- gsub(")", " ", docs[[j]])
  docs[[j]] <- gsub("”", " ", docs[[j]])
  docs[[j]] <- gsub("show", " ", docs[[j]])
  docs[[j]] <- gsub("more", " ", docs[[j]])
}

docs <- tm_map(docs, tolower) # to lower case

#Remove polish stop words
length(stopwords("pl", source = "stopwords-iso"))
stopwords::stopwords("pl", source = "stopwords-iso")
docs <- tm_map(docs, removeWords, stopwords::stopwords("pl", source = "stopwords-iso"))

#writeLines(as.character(docs[[1]])) #let's see what we have [2]

#Stripping  white spaces, i.e. removing unnecesary white space
docs <- tm_map(docs, stripWhitespace)

#extract lines to stem it with Python
setwd('/Users/vladgrivacevskij/TM/no_show_more')
for (j in seq(docs)){
  file_name <-paste('output',j,'.txt',sep='')
  writeLines(as.character(docs[[j]]), file_name)
}

setwd(wd)
stemmed_dir <- '/Users/vladgrivacevskij/TM/no_show_more'

# Re-load corpus with stemmed documents and without show and more words
docs <- Corpus(DirSource(stemmed_dir)) 
writeLines(as.character(docs[[15]]))

#Documnet Term matrix / Term Document Matrix
dtm <- DocumentTermMatrix(docs)
inspect(dtm)
tdm <- t(dtm) #transpose
inspect(tdm)

#Let's check our sparsity
dtmr <-DocumentTermMatrix(docs, control=list(wordLengths=c(3, 20),bounds = list(global = c(2,Inf))))

dtmr1 = removeSparseTerms(dtmr, 0.70) #Reduce sparcity, after inspection, we can see it's 48%.


#Let's check length of our documents and other discriptive statistics 
doc_length <- as.data.frame(rowSums(as.matrix(dtm)))
#max_length<-max(doc_length)
#max_length
#min_length<-min(doc_length)
#min_length 
#aver_length<-mean(rowSums(as.matrix(dtm)))
#aver_length
summary(doc_length)
#We can see, that average length of document is 218.6, median is 213, which means that distribution is close to normal

nn<-rowSums(as.matrix(dtm))
nn
dtm_Norm<-dtm/nn

#backuping data
m0 <- as.matrix(dtm)
write.csv(m0, file="/Users/vladgrivacevskij/TM/files/DocumentTermMatrix.csv")
m1<-as.data.frame(as.matrix(dtm_Norm))
write.csv(m1, file="/Users/vladgrivacevskij/TM/files/DocumentTermMatrixNorm_B.csv")
m2 <- as.matrix(dtmr)
write.csv(m2, file="/Users/vladgrivacevskij/TM/files/DocumentTermMatrix_1_B.csv")
m3 <- as.matrix(dtmr1)
write.csv(m3, file="/Users/vladgrivacevskij/TM/files/SparseDocumentTermMatrix_B.csv")

#Calculate the cumulative frequencies of words across documents and sort as before.
freqr <- colSums(as.matrix(dtm)) #Here we can see how many times certain word appears 
length(freqr)
freq <- sort(freqr, decreasing=TRUE) #after sorting we can see most frequent words
head(freq, 14) #top 14 by frequency
findFreqTerms(dtmr,lowfreq=80) 

#findAssocs(dtmr,"polecać",0.6)

#Graphics
#Cumulative frequencies of words with dtmr (document term matrix after sparcirty)
freqr <- colSums(as.matrix(dtmr)) #check both dtmr1 and dtmr
length(freqr) #8592
freq <- sort(freqr, decreasing=TRUE)
head(freq, 14) #top 14 by frequency

mk<-min(head(freq, 30))
mk 
wf=data.frame(word=names(freq),freq=freq)

#Plotting Zipfs law
p <- ggplot(subset(wf, freq>mk), aes(x = reorder(word, -freq), y = freq)) 
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

#Create word cloud
#set.seed(42)
#wordcloud(names(freq),freq, min.freq=70)
#set.seed(142)
#wordcloud(names(freq), freq, max.words=100)
#wordcloud(names(freq), freq, min.freq=70,colors=brewer.pal(6, "Dark2"))
#dev.new(width = 100, height = 100, unit = "px") #could be useful
set.seed(142)
dark2 <- brewer.pal(6, "Dark2")
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2)

#N Grams
docs_1 <- VCorpus(DirSource(stemmed_dir))
docs_1

NgramTokenizer = function(x) {
  unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
}

dtm_n <- DocumentTermMatrix(docs_1, control = list(tokenize = NgramTokenizer))
dtm_n

#___________Calculating the Zipfs law of bigrams. Most significant bigrams__________________________
freq_n <- sort(colSums(as.matrix(dtm_n)), decreasing=TRUE) 
head(freq_n, 15)
mk <- min(head(freq_n, 15))
tail(freq_n, 15)
m<-as.matrix(dtm_n)

#backuping N-gramm
write.csv(m, file="/Users/vladgrivacevskij/TM/files/N_DocumentTermMatrix.csv")

#Zipf's law of bigram
wf=data.frame(word=names(freq_n),freq=freq_n)
wf
p <- ggplot(subset(wf, freq>=mk), aes(x = reorder(word, -freq), y = freq))
p <- p + geom_bar(stat="identity")+ ggtitle("Histogram of Bigrams for Opinions") +labs(x="Bi-grams",y="Frequency")
p <- p + theme(axis.text.x=element_text(angle=90, hjust=1, size=16))
p

#word cloud of bigrams
set.seed(142)
dark2 <- brewer.pal(6, "Dark2")
wordcloud(names(freq_n), freq_n, max.words=100, rot.per=0.2, colors=dark2)

#--------

dtm1 = as.data.frame.matrix(m0)
dtm1 [1:10,1:10]
dtm<-dtm1[,-1]

filenames <- list.files("/Users/vladgrivacevskij/TM/no_show_more",pattern="*.txt")
filenames <-c(filenames)
filenames
rownames(dtm)
rownames(dtm)<-filenames
dtm [1:10,1:10]

freq <- sort(colSums(dtm), decreasing=TRUE)
freq

freq1 <- sort(rowSums(dtm), decreasing=TRUE)
freq1

wf=data.frame(word=names(freq),freq=freq)
wf
p <- ggplot(subset(wf, freq>150), aes(x = reorder(word, -freq), y = freq)) 
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

#calculating TF, IDF and TF-IDF

dtm <- read.csv("/Users/vladgrivacevskij/TM/files/DocumentTermMatrix.csv")
#dtm1 = as.data.frame.matrix(dtm)

tdm<- t(dtm) # t(dtm) – transpose matrix DTM into TDM 
tf <- as.matrix(tdm) #
idf <- log(ncol(tf) / (rowSums(tf != 0)))  
tf[1:5,1:3]
idf[1:5] #IDF

# words with highest and lowerst IDF
idf_sort <- sort(idf, decreasing=FALSE) 
head(idf_sort, 15)
tail(idf_sort, 15)

# building tf-idf
idf1 <- diag(idf) #put IDFs on diagonal, idf is just a vector
idf1[1:5,1:5]
tf_idf <- crossprod(tf, idf1) #calculating cross product of TF and IDF
#tf_idf
colnames(tf_idf) <- rownames(tf)
colnames(tf_idf)
tf_idf

#tf idf backup
#write.csv(m, file="/Users/vladgrivacevskij/TM/files/tf_idf.csv")
#tf_idf[1:5,1:5]

setwd("/Users/vladgrivacevskij/TM")
tf_idf <- read.csv("/Users/vladgrivacevskij/TM/files/tf_idf.csv")

# TF-IDF wordcloud
freq <- colSums(as.matrix(tf_idf), na.rm = FALSE)
dev.new(width = 100, height = 100, unit = "px") # if you need
set.seed(42)
wordcloud(names(freq),freq, max.words=100,colors=dark2)

# Second view of wordcloud
#
#dev.new(width = 100, height = 100, unit = "px") # if you need
#set.seed(142)
#wordcloud(words = names(freq), freq = freq, min.freq = 1,
#          max.words=100, random.order=FALSE, rot.per=0.35,
#          colors=brewer.pal(8, "Dark2"))

#clustring
dtm[1:15,1:15]

d1 <- dist(dtm, method="euclidian") #choose method of distance calculation 
# make the clustering
fit <- hclust(d=d1, method="complete") #hierarchical clustering 
fit
plot.new()
plot(fit, hang=-1, cex=0.5)
groups <- cutree(fit, k=4) # "k" defines the number of clusters you are using
#rect.hclust(fit, k=4, border="red")

#OPTIMAL NUMBER OF CLUSTERS

set.seed(123)
# function to compute total within-cluster sum of square
wss <- function(k) {
  kmeans(d, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 20
k.values <- 1:20
# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)
wss_values
# optimal number of clusters should appear to be the bend in the knee (or elbow)
plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


#k-Mean
#DTM = 5 cluster

#TF-IDF = 7 K-means clustering with 7 clusters of sizes 61, 153, 42, 644, 379, 310, 294
#Within cluster sum of squares by cluster:81.5 %

dtm_tfidf <-as.DocumentTermMatrix(tf_idf,weighting = weightTf)
dtm_tfidf<-removeSparseTerms(dtm_tfidf, 0.35)
dtm_tfidf
d_tfidf <- dist(dtm_tfidf, method="euclidian")
kfit_tfidf <- kmeans(d_tfidf, 7)
kfit_tfidf
clusplot(as.matrix(d_tfidf), kfit_tfidf$cluster, color=T, shade=T, lines=0,alpha=0.3)

c1_tftdf <- names(kfit_tfidf$cluster[kfit_tfidf$cluster == 1])
c2_tftdf <- names(kfit_tfidf$cluster[kfit_tfidf$cluster == 2])
c3_tftdf <- names(kfit_tfidf$cluster[kfit_tfidf$cluster == 3])
c4_tftdf <- names(kfit_tfidf$cluster[kfit_tfidf$cluster == 4])
c5_tftdf <- names(kfit_tfidf$cluster[kfit_tfidf$cluster == 5])
c6_tftdf <- names(kfit_tfidf$cluster[kfit_tfidf$cluster == 6])
c7_tftdf <- names(kfit_tfidf$cluster[kfit_tfidf$cluster == 7])

#DTM Clustering 
#K-means clustering with 5 clusters of sizes 461, 60, 807, 311, 244
#Within cluster sum of squares by cluster:75.5 %
dtm <-as.DocumentTermMatrix(dtm1,weighting = weightTf)
dtmr<-removeSparseTerms(dtm, 0.35)
dtmr
d <- dist(tf, method="euclidian")
kfit_dtm <- kmeans(d, 5)
kfit_dtm
clusplot(as.matrix(d), kfit_dtm$cluster, color=T, shade=T, lines=0)

c1_dtm <- names(kfit_tfidf$cluster[kfit_tfidf$cluster == 1])
c2_dtm <- names(kfit_tfidf$cluster[kfit_tfidf$cluster == 2])
c3_dtm <- names(kfit_tfidf$cluster[kfit_tfidf$cluster == 3])
c4_dtm <- names(kfit_tfidf$cluster[kfit_tfidf$cluster == 4])
c5_dtm <- names(kfit_tfidf$cluster[kfit_tfidf$cluster == 5])

#End of DTM Clustering

for (j in 1:length(c1)){
  conct_path <- paste('/Users/vladgrivacevskij/TM/clust_tfidf',c1[j],sep = '')
  my_data <- read.delim(conct_path)
  print(my_data)
}
### END OF CLUSTRING


c1_dir <- "/Users/vladgrivacevskij/TM/no_show_more"
docs <- Corpus(DirSource(stemmed_dir)) # Corpus

a <- as.matrix(dtm)
c1_dtm_m <- subset(a, rownames(a) %in% c1_dtm)
c2_dtm_m <- subset(a, rownames(a) %in% c2_dtm)
c3_dtm_m <- subset(a, rownames(a) %in% c3_dtm)
c4_dtm_m <- subset(a, rownames(a) %in% c4_dtm)
c5_dtm_m <- subset(a, rownames(a) %in% c5_dtm)

b <- as.matrix(tf_idf)

c1_tfidf_m <- subset(b, rownames(a) %in% c1_tftdf)
c2_tfidf_m <- subset(b, rownames(a) %in% c2_tftdf)
c3_tfidf_m <- subset(b, rownames(a) %in% c3_tftdf)
c4_tfidf_m <- subset(b, rownames(a) %in% c4_tftdf)
c5_tfidf_m <- subset(b, rownames(a) %in% c5_tftdf)
c6_tfidf_m <- subset(b, rownames(a) %in% c6_tftdf)
c7_tfidf_m <- subset(b, rownames(a) %in% c7_tftdf)

##Word cloud DTM clustering
#Cluster 1. Top 10 words =  strzyżenie,polecać,włos, super, zadowolony, atmosfera,miły,manicure, DAMSKI, koloryzacja 
#Cluster 2. Top 10 words = strzyżenie, MĘSKI, polecać, super, włos, fryzjer, salon, miły,zadowolony, atmosfera 
#Cluster 3. Top 10 words = włos, strzyżenie, polecać, wizyta, długi, damski, salon, atmosfera, średnia, zadowolony 
#Cluster 4. Top 10 words =  strzyżenie, polecać,włos,męski, damski, super, zadowolony, miły, atmosfera,koloryzacja 
#Cluster 5. Top 10 words = strzyżenie, męski, polecać, włos, super, miły, zadowolony, damski,atmosfera, obsługa 

#Word cloud TF-IDF clustering
#Cluster 1. Top 10 words =  manicure,hybrydowy, comba, paznokieć, brew, haircuta, klasyczny, agnieszka, rzęsa, kamil 
#Cluster 2. Top 10 words =  robert, męski, szymoać, jamorski, tinativus, łukać, makowski, czuszyć, latyński 
#Cluster 3. Top 10 words =  średnia, natalia, włos, długi, sylwi,kucharski, kręcić, ewa, prostować, kasi
#Cluster 4. Top 10 words =  damski,manicure, natalia,moniki, hybrydowy, męski, modelować, koloryzacja,paulin,średnia 
#Cluster 5. Top 10 words =  męski, klasyczny, aleksandra, comba, nożyczki, bród, gotówka, maszynka, krzysztof, tatsiaum 
#Cluster 6. Top 10 words =  męski, damski, agnieszka,  olativus,moniki, modelować, sylwi, klasyczny, średnia,  maszynka 
#Cluster 7. Top 10 words =  włos, średnia, karolina, bród, haircuta, comba, długi,kasi, damski, koloryzacja 


CLUSTER <- c7_tfidf_m # c1_dtm_m and #c1_tfidf_m
freqr1_dtm <- colSums(CLUSTER)
freq_dtm <- sort(freqr1_dtm, decreasing=TRUE)
head(freq_dtm, 10)
findFreqTerms(CLUSTER,lowfreq=80)
dev.new(width = 100, height = 100, unit = "px") #could be useful
set.seed(142)
dark2 <- brewer.pal(6, "Dark2")
wordcloud(names(freq_dtm), freq_dtm, max.words=100, rot.per=0.2, colors=dark2)

#graphs and topic modeling
MyData <-read.csv("/Users/vladgrivacevskij/TM/files/DocumentTermMatrix.csv")
dtm1 = as.data.frame.matrix(MyData)
dtm1 [1:10,1:10]
dtm<-dtm1[,-1]

mm_s = as.matrix(dtm)
mm<-as.matrix(mm_s)

#function cosineSim compute cosine similarity between document vectors
#converting to distance matrix sets diagonal elements to 0
#Defining cosine similarity function
cosineSim <- function(x){
  as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}

#compute cosine similarity between document vectors
cs <- cosineSim(mm)
cs[1]
#backup
write.csv(as.matrix(cs),file="/Users/vladgrivacevskij/TM/files/DocumentCosine.csv")

#Setting threshold for minimal value of similarity 
min_cos<-0.2 
cs[cs < min_cos] <- 0
cs <- round(cs,3)

write.csv(as.matrix(cs),file="/Users/vladgrivacevskij/TM/files/DocumentAdjacencyMatrix.csv")
cs

dat<-read.csv("/Users/vladgrivacevskij/TM/files/DocumentAdjacencyMatrix.csv",
              header = TRUE,
              sep = ",",
              colClasses = NA,
              na.string = "NA",
              skip = 0,
              strip.white = TRUE,
              fill = TRUE,
              comment.char = "#",
              stringsAsFactors = FALSE
)
mm1 = as.data.frame.matrix(dat)
mm1=mm1[,-1]

#Setting file names for mm1
filenames <- list.files("/Users/vladgrivacevskij/TM/no_show_more/",pattern="*.txt")
filenames <-c(filenames)
filenames
#converting mm1 into matrix format
rownames(mm1)<-filenames
cs<-as.matrix(mm1)
cs

#Creating undirected weighted graph
g=graph.adjacency(cs,mode="undirected",weighted=TRUE)
g
#Checking the undirected weighted graph attributes
list.vertex.attributes(g)
list.edge.attributes(g)
V(g)$name
E(g)$weight

#Calculate the degree of each Vertices and assign it’s to the Vertices size feature.
deg <- graph.strength(g, mode="all")
deg
DEG_CONST <- 0.1
V(g)$size <- deg * DEG_CONST

#Build the Vertices color
hc5 <- terrain.colors(5)
g.max <- max(deg)
vcolors <- 5 - round(4 *(deg / g.max))
vcolors <- hc5[vcolors]
vcolors

#. Build the Graph Plot using different Layouts.
DEG_CONST_2 <- 0.0085
VERTEX_SIZE_CONST <- 0.03

#lay_1 <- layout.fruchterman.reingold(g)
#lay_2 <- layout_in_circle
#lay_3 <- layout_with_kk(g)
lay_4 <- layout_randomly(g)
#lay_5 <- layout_on_sphere(g)

plot.igraph(g,layout= lay_4, edge.arrow.size=0.00001,edge.width=E(g)$weight*DEG_CONST_2,
            vertex.color=vcolors,vertex.size=V(g)$size*VERTEX_SIZE_CONST, vertex.label=NA)#, vertex.label.cex=1) vertex.label=V(g)$name

#Community detection algorithms try to find the relationships between the Documents, presented in
#the Graph view, on the bases of the data about cosine similarity.
cfg <- cluster_fast_greedy(as.undirected(g))
plot(cfg, as.undirected(g))
membership(cfg)

# building Term Document Matrix as a transformed Document Term Matrix
dtm <- read.csv("/Users/vladgrivacevskij/TM/files/DocumentTermMatrix.csv")
tdm <-as.TermDocumentMatrix(t(dtm),weighting = weightTf)
tdm
# transform Term Document Matrix into the matrix with sparsity is not higher then (for example) 0.2
tdm = removeSparseTerms(tdm, 0.20)
tdm
# completing the matrix column names
filenames <- list.files("/Users/vladgrivacevskij/TM/no_show_more/",pattern="*.txt")
filenames <-c(filenames)
filenames
colnames(tdm)<-filenames
tdm 

#TOPIC MODELLING USING LDA
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE
k <- 9

dtm <- read.csv("/Users/vladgrivacevskij/TM/files/DocumentTermMatrix.csv")
dtm1 = as.data.frame.matrix(dtm)
dtm<-dtm1[,-1]
filenames <- list.files("/Users/vladgrivacevskij/TM/no_show_more",pattern="*.txt")
filenames <-c(filenames)
filenames
rownames(dtm)
rownames(dtm)<-filenames
dtm [1:10,1:10]

ldaOut <-LDA(dtm, k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
str(ldaOut)
#Check the most significant 6 terms in each topic, transform them into the matrix format and save to the File. 
ldaOut.terms <- as.matrix(terms(ldaOut,10))
ldaOut.terms
write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicsToTerms_k_9.csv"))

#Transform the probabilities associated with each topic assignment into the matrix format 
#and save to the File. Check and analyze the results of the topics assignment.
topicProbabilities <- as.data.frame(ldaOut@gamma)
topicProbabilities
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilities_k_9.csv"))

#Transform the Topics into the matrix format and save to the File.
ldaOut.topics <- as.matrix(topics(ldaOut))
ldaOut.topics
write.csv(ldaOut.topics,file=paste("LDAGibbs",k, "DocsToTopics_k_9.csv"))


