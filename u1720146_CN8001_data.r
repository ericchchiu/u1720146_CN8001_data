#R code for text mining of the three transcripts of oral evidence
#given to the parliamentary inquiry
#'Social media data and real time analytics' (HC245)
#Word counts, word associations, word clouds and
#tf-idf text analysis 

#----------------------------------------
#code 01: 
#set working directory
setwd(dirname(file.choose()))
getwd()

#load packages
library(textstem)
library(qdap)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(RSQLite)
library(textstem)
library(tm)
library(wordcloud)
#----------------------------------------
#code 02:
#import the three oral evidence data files and 
#convert them to dataframes
#combine the three dataframes to one
#add an index column(id) to the combined file
#import u1720146_CN8001_persons.csv and convert it to a dataframe

oral0618 <- read.csv('u1720146_CN8001_oral_20140618.csv', comment.char = '#', sep = '|', stringsAsFactors = FALSE)
oral0623 <- read.csv('u1720146_CN8001_oral_20140623.csv', comment.char = '#', sep = '|', stringsAsFactors = FALSE)
oral0708 <- read.csv('u1720146_CN8001_oral_20140708.csv', comment.char = '#', sep = '|', stringsAsFactors = FALSE)
oralAll <- rbind(oral0618, oral0623, oral0708)
oralAll$id <- seq.int(nrow(oralAll))
persons <- read.csv('u1720146_CN8001_persons.csv', comment.char = '#', sep = '|', stringsAsFactors = FALSE)

#----------------------------------------
#code 03:
#create an sqlite database and copy the two dataframes to it

conndb1 <- dbConnect(RSQLite::SQLite(), 'db1')
dbWriteTable(conndb1, 'oralAll', oralAll)
dbWriteTable(conndb1, 'persons', persons)
dbListTables(conndb1) #inspect the database db1

#----------------------------------------
#code 04:
#use sqlite code to create a dataframe of witnesses and the utterances
#of each and every of them ranked by their names and then question #numbers

witnssUttrnces <- dbGetQuery(conndb1, 'WITH witnssUttrnces AS (SELECT persons.person AS person, id, question, oralEvidence FROM oralAll INNER JOIN persons ON oralAll.person = persons.person WHERE persons.sector <> "member" ORDER BY person, id) SELECT person, question, oralEvidence FROM witnssUttrnces')

#----------------------------------------
#code 05:
#commonality and comparison clouds for utterances of 
#Emma Carr and Timo Hannay

#create a dataframe of Timo Hannay and Emma Carr's utterances(concated)

dbWriteTable(conndb1, 'witnssUttrnces', witnssUttrnces)
THAndECUttrnces <- dbGetQuery(conndb1, 'SELECT person, GROUP_CONCAT(oralEvidence," ") AS oralEvidence FROM witnssUttrnces WHERE person IN ("Emma Carr", "Timo Hannay") GROUP BY person ORDER BY person')

#clear data and produce tdm for Emma Carr and Timo Hannay

THAndECUttrnces_cor_cl <- Corpus(VectorSource(THAndECUttrnces$oralEvidence))
THAndECUttrnces_cor_cl <- tm_map(THAndECUttrnces_cor_cl, tolower)
stop.word <- unlist(read.table("stop_word.txt", stringsAsFactors=FALSE))
THAndECUttrnces_cor_cl <- tm_map(THAndECUttrnces_cor_cl, removeWords, stop.word)
THAndECUttrnces_cor_cl <- tm_map(THAndECUttrnces_cor_cl, removeNumbers)
THAndECUttrnces_cor_cl <- tm_map(THAndECUttrnces_cor_cl, removeWords, stopwords())
THAndECUttrnces_cor_cl <- tm_map(THAndECUttrnces_cor_cl, removePunctuation)
stop.char <- unlist(read.table("stop_char.txt", stringsAsFactors=FALSE))
THAndECUttrnces_cor_cl <- tm_map(THAndECUttrnces_cor_cl, removeWords, stop.char)
THAndECUttrnces_cor_cl <- tm_map(THAndECUttrnces_cor_cl, stripWhitespace)
THAndECUttrnces_cor_cl <- tm_map(THAndECUttrnces_cor_cl, lemmatize_strings)
#THAndECUttrnces_cor_cl <- tm_map(THAndECUttrnces_cor_cl, PlainTextDocument)
THAndECUttrnces_tdm_cl <- TermDocumentMatrix(THAndECUttrnces_cor_cl)
colnames(THAndECUttrnces_tdm_cl) <- c("Emma Carr", "Timo Hannay")
THAndECUttrnces_tdm_cl

# coerce as a matrix
THAndECUttrnces_tdm_cl <- as.matrix(THAndECUttrnces_tdm_cl)
colnames(THAndECUttrnces_tdm_cl) <- c("Emma Carr", "Timo Hannay")

# assign a palette
pal <- brewer.pal(5, "Accent")
# plot wordclouds
set.seed(12345)
comparison.cloud(THAndECUttrnces_tdm_cl, scale=c(2,0.5), max.words = 50, rot.per = 0.3, random.order=FALSE, color=pal, title.size=2)
set.seed(12345)
commonality.cloud(THAndECUttrnces_tdm_cl, scale=c(5,0.5), max.words = 50, rot.per = 0.3, random.order=FALSE, color=pal, title.size=2)

#----------------------------------------
#code 06:
#use functions to reduce repetition of code

#6.1 function for forming cleaned and lemmatised corpus from sector utterances
getFrmSctorUttrnces_cleanNLemmatsdCrpus <- function(sctorUttrnces){
sctorUttrnces_corpus <- Corpus(VectorSource(sctorUttrnces$oralEvidence))
sctorUttrnces_corpus <- tm_map(sctorUttrnces_corpus, tolower)
stop.word <- unlist(read.table("stop_word.txt", stringsAsFactors=FALSE))
sctorUttrnces_corpus <- tm_map(sctorUttrnces_corpus, removeWords, stop.word)
sctorUttrnces_corpus <- tm_map(sctorUttrnces_corpus, removeNumbers)
sctorUttrnces_corpus <- tm_map(sctorUttrnces_corpus, removeWords, stopwords())
sctorUttrnces_corpus <- tm_map(sctorUttrnces_corpus, removePunctuation)
stop.char <- unlist(read.table("stop_char.txt", stringsAsFactors=FALSE))
sctorUttrnces_corpus <- tm_map(sctorUttrnces_corpus, removeWords, stop.char)
sctorUttrnces_corpus <- tm_map(sctorUttrnces_corpus, lemmatize_strings)
sctorUttrnces_corpus <- tm_map(sctorUttrnces_corpus, stripWhitespace)
return(sctorUttrnces_corpus)
}

#6.2 function for producing a word cloud from a corpus
prduceFrmSctorUttrnces_corpus_wordCloud <- function(sctorUttrnces_corpus){
set.seed(12345)
wordcloud(sctorUttrnces_corpus, scale=c(3,0.5), max.words = 100, rot.per = 0.3, random.order=FALSE, colors="black")
}

#6.3 function for producing a tdm from a corpus
prduceFrmSctorUttrnces_corpus_tdm <- function(sctorUttrnces_corpus) return(TermDocumentMatrix(sctorUttrnces_corpus))

#6.4 function for producing a csv file of word count list from a tdm
#the list shall be sorted first according to scores (descending) and
#then according to alphabetical order of words (ascending)
prduceFrmSctorUttrnces_tdm_WordCntCsvFile <- function(sctorUttrnces_tdm, fileNameIncldCsvSuffix){
sctorUttrnces_wordCount <- as.data.frame(rowSums(as.matrix(sctorUttrnces_tdm)))
sctorUttrnces_wordCount <- cbind(rownames(sctorUttrnces_wordCount), data.frame(sctorUttrnces_wordCount, row.names = NULL))
colnames(sctorUttrnces_wordCount) <- c('word', 'count')
sctorUttrnces_wordCount <- sctorUttrnces_wordCount[order(-sctorUttrnces_wordCount[,2], sctorUttrnces_wordCount[,1]),]
sctorUttrnces_wordCount$rank <- seq.int(nrow(sctorUttrnces_wordCount))
sctorUttrnces_wordCount <- sctorUttrnces_wordCount[, c('rank', 'word', 'count')]
write.table(sctorUttrnces_wordCount, fileNameIncldCsvSuffix, quote = FALSE, sep = ',', row.name = FALSE)
}

#6.5 function for finding word frequencies directly from a corpus by 
#using the freq_terms function of the qdap package
getFrmCorpus_frequenciesOfWords <- function(corpus, topHowMany) return(freq_terms(as.data.frame(corpus), top = topHowMany))

#6.6 function for producing an association word list from a 
#dtm or a tdm
prduceFromDtmTdmAssocsWordsList <- function(dtmTdm, charVectWords, corLimitVect) return(findAssocs(dtmTdm, charVectWords, corLimitVect))

#6.7 function for producing a df from an association word list
produceFromAssocsWordsList_df <- function(tdmDtm_AssocsWordsList){
return(list_vect2df(tdmDtm_AssocsWordsList, col1 = 'word', col2 = 'assocsWords', col3 = 'corrScore'))
}

#----------------------------------------
#code 07:
#divide witnesses into the following four sectors:
#academic(Professor Yates, Dr McPherson, Professor Preston, Sir Nigel
#Shadbolt, Professor McAuley, Professor van Zoonen, Dr d'Aquin,
#Professor Robertson, Dr Macnish, Dr Elliot), business(Sureyya Cansoy,
#Timo Hannay), government(Steve Wood, Mr Vaizey) and ngo(Carl Miller,
#Emma Carr)
#group utterances by sectors and treat replies to a question from a
#sector a document
#create a dataframe for each sector's utterances

academicUttrnces  <- dbGetQuery(conndb1, "SELECT question, GROUP_CONCAT(oralEvidence,' ') AS oralEvidence FROM witnssUttrnces JOIN persons ON witnssUttrnces.person = persons.person WHERE sector =  'academic' GROUP BY question ORDER BY question")

businessUttrnces  <- dbGetQuery(conndb1, "SELECT question, GROUP_CONCAT(oralEvidence,' ') AS oralEvidence FROM witnssUttrnces JOIN persons ON witnssUttrnces.person = persons.person WHERE sector =  'business' GROUP BY question ORDER BY question")

govtUttrnces <- dbGetQuery(conndb1, "SELECT question, GROUP_CONCAT(oralEvidence,' ') AS oralEvidence FROM witnssUttrnces JOIN persons ON witnssUttrnces.person = persons.person WHERE sector =  'government' GROUP BY question ORDER BY question")

ngoUttrnces  <- dbGetQuery(conndb1, "SELECT question, GROUP_CONCAT(oralEvidence,' ') AS oralEvidence FROM witnssUttrnces JOIN persons ON witnssUttrnces.person = persons.person WHERE sector =  'ngo' GROUP BY question ORDER BY question")

#----------------------------------------
#code 08:
#code for applying the above functions to perform text mining

#8.1 code for producing a clean and lemmatised corpus with the
#utterances of witnesseses of each and every sector
#academic
academicUttrnces_corpus <- getFrmSctorUttrnces_cleanNLemmatsdCrpus(academicUttrnces)
#business
businessUttrnces_corpus <- getFrmSctorUttrnces_cleanNLemmatsdCrpus(businessUttrnces)
#government
govtUttrnces_corpus <- getFrmSctorUttrnces_cleanNLemmatsdCrpus(govtUttrnces)
#ngo
ngoUttrnces_corpus <- getFrmSctorUttrnces_cleanNLemmatsdCrpus(ngoUttrnces)

#8.2 code for producing a word cloud with each and every sector uttereance corpus
#academic
prduceFrmSctorUttrnces_corpus_wordCloud(academicUttrnces_corpus)
#business
prduceFrmSctorUttrnces_corpus_wordCloud(businessUttrnces_corpus)
#government
prduceFrmSctorUttrnces_corpus_wordCloud(govtUttrnces_corpus)
#ngo
prduceFrmSctorUttrnces_corpus_wordCloud(ngoUttrnces_corpus)

#8.3 code for producing a tdm from each and every sector corpus
#academic
academicUttrnces_tdm <- prduceFrmSctorUttrnces_corpus_tdm(academicUttrnces_corpus)
#business
businessUttrnces_tdm <- prduceFrmSctorUttrnces_corpus_tdm(businessUttrnces_corpus)
#government
govtUttrnces_tdm <- prduceFrmSctorUttrnces_corpus_tdm(govtUttrnces_corpus)
#ngo
ngoUttrnces_tdm <- prduceFrmSctorUttrnces_corpus_tdm(ngoUttrnces_corpus)

#8.4 code for producing a csv file of word freqency list with each and every tdm
#academic
prduceFrmSctorUttrnces_tdm_WordCntCsvFile(academicUttrnces_tdm, 'academicUttrncesWordCount.csv')
#business
prduceFrmSctorUttrnces_tdm_WordCntCsvFile(businessUttrnces_tdm, 'businessUttrncesWordCount.csv')
#government
prduceFrmSctorUttrnces_tdm_WordCntCsvFile(govtUttrnces_tdm, 'govtUttrncesWordCount.csv')
#ngo
prduceFrmSctorUttrnces_tdm_WordCntCsvFile(ngoUttrnces_tdm, 'ngoUttrncesWordCount.csv')

#8.5 code for producing a word frequency list from each and every corpus instead of from tdm or dtm (results should be the same as those contained in the abovementioned csv files) 
#academic
academic_wordFrequency <- getFrmCorpus_frequenciesOfWords(academicUttrnces_corpus, 20)
#business
business_wordFrequency <- getFrmCorpus_frequenciesOfWords(businessUttrnces_corpus, 20)
#government
govt_wordFrequency <- getFrmCorpus_frequenciesOfWords(govtUttrnces_corpus, 20)
#ngo
ngo_wordFrequency <- getFrmCorpus_frequenciesOfWords(ngoUttrnces_corpus, 20)
#forming a dataframe of the top 20 most frequent words of all sectors

#8.6 code for producing a dataframe which showing the top 20 most frequent words of each and every sector
word_frequency_top20_df <- as.data.frame(cbind(academic_wordFrequency[1:20,1], business_wordFrequency[1:20,1], govt_wordFrequency[1:20,1], ngo_wordFrequency[1:20,1]))
names(word_frequency_top20_df) <- c('academicSector', 'businessSector', 'govtSector', 'ngoSector')
word_frequency_top20_df

#8.7 code for finding words in each and every corpus which are most correlated with datum, government, legislation and research 

charVectWords <- c('datum', 'government', 'legislation', 'research')
corLimitVect <- c(0.4, 0.5, 0.5, 0.6)
#academic
academicUttrnces_tdm_AssocsWordsList <- prduceFromDtmTdmAssocsWordsList(academicUttrnces_tdm, charVectWords, corLimitVect)
#business
businessUttrnces_tdm_AssocsWordsList <- prduceFromDtmTdmAssocsWordsList(businessUttrnces_tdm, charVectWords, corLimitVect)
#government
govtUttrnces_tdm_AssocsWordsList <- prduceFromDtmTdmAssocsWordsList(govtUttrnces_tdm, charVectWords, corLimitVect)
#ngo
ngoUttrnces_tdm_AssocsWordsList <- prduceFromDtmTdmAssocsWordsList(ngoUttrnces_tdm, charVectWords, corLimitVect)

#8.8 code for producing a dataframe from the association word lists in respect the government sector corpus

govtUttrnces_AssocsWords_df <- produceFromAssocsWordsList_df(govtUttrnces_tdm_AssocsWordsList)

govtUttrnces_AssocsWords_df[58:105,]

----------------------------------------
code 09:
#code for plotting a graph of those words associated with the word 'government' with the correlation value equal or higher than 0.5 in the government sector utterances corpus

govtUttrnces_AssocsWords_df[is.element(govtUttrnces_AssocsWords_df$word, 'government'),]

#use the list vector obtained from code 8.7 above
govtUttrnces_tdm_AssocsWords_df_government <- list_vect2df(govtUttrnces_tdm_AssocsWordsList[2, drop = FALSE], col2 = "assocsWords", col3 = "corrScore")

#use the df obtained from code 8.8 above
govtUttrnces_tdm_AssocsWords_df_government <- govtUttrnces_AssocsWords_df[is.element(govtUttrnces_AssocsWords_df$word, 'government'),][,2:3]

ggplot(govtUttrnces_tdm_AssocsWords_df_government, aes(corrScore, assocsWords)) + 
  geom_point(size = 3) +   ggtitle('govtWtnsses_OralEvidence_WordsAssocsWth_government') +
  theme_gdocs()

----------------------------------------
code 10
#attempt to use if-tdf to find out which query or queires of the terms of reference each question is belonged to

#input, clean and lemmatise the terms of reference  
termsOfReference_df = read.csv('termsOfReference.csv', sep = '|', comment.char = '#', stringsAsFactors = FALSE)
termsOfReference_list = as.list(termsOfReference_df[,2])
termsOfReferenceCorpus = VectorSource(termsOfReference_list)
termsOfReferenceCorpus_preproc = Corpus(termsOfReferenceCorpus)
termsOfReferenceCorpus_preproc = tm_map(termsOfReferenceCorpus_preproc,stripWhitespace)
termsOfReferenceCorpus_preproc = tm_map(termsOfReferenceCorpus_preproc,removePunctuation)
termsOfReferenceCorpus_preproc = tm_map(termsOfReferenceCorpus_preproc,content_transformer(tolower))
termsOfReferenceCorpus_preproc = tm_map(termsOfReferenceCorpus_preproc,removeWords,stopwords())
termsOfReferenceCorpus_preproc = tm_map(termsOfReferenceCorpus_preproc, lemmatize_strings)
inspect(termsOfReferenceCorpus_preproc)
hc245_tors = as.data.frame(termsOfReferenceCorpus_preproc)

#input the 3 oral evidence files and combine them
oral0618 <- read.csv('..\\..\\RQDAWorkingFolder\\u1720146_CN8001_oral_20140618.csv', comment.char = '#', sep = '|', stringsAsFactors = FALSE)
oral0623 <- read.csv('..\\..\\RQDAWorkingFolder\\u1720146_CN8001_oral_20140623.csv', comment.char = '#', sep = '|', stringsAsFactors = FALSE)
oral0708 <- read.csv('..\\..\\RQDAWorkingFolder\\u1720146_CN8001_oral_20140708.csv', comment.char = '#', sep = '|', stringsAsFactors = FALSE)
oralAll <- rbind(oral0618, oral0623, oral0708)

#form a dataframe of all questions
#answers to each question are concatened to form a document
conndb1 <- dbConnect(RSQLite::SQLite(), 'db1')
dbWriteTable(conndb1, 'oralAll', oralAll)
#dbWriteTable(conndb1, 'persons', persons)
evidQsConcated <- dbGetQuery(conndb1, 'SELECT question, GROUP_CONCAT(oralEvidence, " ") AS oralEvidence FROM oralAll GROUP BY question')

#form named list of the questions
evidQsConcated_list = as.list(evidQsConcated[,2])
evidQsConcated_N.docs = length(evidQsConcated_list)
names(evidQsConcated_list) = evidQsConcated[,1]

#form named list of the tors (cleaned and lemmatised version)
hc245_tors = read.csv('termsOfReference.txt', comment.char = '#', sep = '|', stringsAsFactors = FALSE)
hc245_tors_list = unlist(hc245_tors[,2])
hc245_tors_N.query = length(hc245_tors_list)
names(hc245_tors_list) = paste0("query", c(1:hc245_tors_N.query))

#move all to a corpus
evidQsConcated_corpus = VectorSource(c(evidQsConcated_list, hc245_tors_list))
evidQsConcated_corpus$Names = c(names(evidQsConcated_list),names(hc245_tors_list))
evidQsConcated_corpus_preproc = Corpus(evidQsConcated_corpus)

#clean, trim and lemmatise the coprus
evidQsConcated_corpus_preproc = tm_map(evidQsConcated_corpus_preproc,stripWhitespace)
evidQsConcated_corpus_preproc = tm_map(evidQsConcated_corpus_preproc,removePunctuation)
evidQsConcated_corpus_preproc = tm_map(evidQsConcated_corpus_preproc,content_transformer(tolower))
evidQsConcated_corpus_preproc = tm_map(evidQsConcated_corpus_preproc,removeWords,stopwords())
evidQsConcated_corpus_preproc = tm_map(evidQsConcated_corpus_preproc, lemmatize_strings)

#form term document matrix
evidQsConcated_tdm = TermDocumentMatrix(evidQsConcated_corpus_preproc,control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
evidQsConcated_tdm_mat = as.matrix(evidQsConcated_tdm)
colnames(evidQsConcated_tdm_mat) = c(names(evidQsConcated_list),names(hc245_tors_list))

#normalising the tdm
evidQsConcated_tfidf_mat <- scale(evidQsConcated_tdm_mat, center = FALSE,scale = sqrt(colSums(evidQsConcated_tdm_mat^2)))

#split qsAndEvidence and tors
hc245_tors.vectors <- evidQsConcated_tfidf_mat[, (evidQsConcated_N.docs + 1):(evidQsConcated_N.docs+hc245_tors_N.query)]
evidQsConcated_tfidf_mat <- evidQsConcated_tfidf_mat[, 1:evidQsConcated_N.docs]

#calculate the similarity scores
evidQsConcated_doc.scores <- t(hc245_tors.vectors) %*% evidQsConcated_tfidf_mat

#change the tors list back to original questions
hc245_tors_list = unlist(termsOfReference_df[,2])
names(hc245_tors_list) = paste0("query", c(1:hc245_tors_N.query))

#form a dataframe of scores queries and scores
evidQsConcated_results.df <- data.frame(querylist = hc245_tors_list,evidQsConcated_doc.scores)

evidQsConcated_showTopresults <- function(query, noOfDocs){
  x = evidQsConcated_results.df[which(evidQsConcated_results.df$querylist == query),]
  yy =  data.frame(t(x),rownames(t(x)),row.names = NULL)[-1,]
  names(yy) = c("score","docs")
  yy$score = as.numeric(as.character(yy$score))
  yyy = yy[order(yy$score,decreasing = T),]
  
  return(yyy[which(yyy$score > 0),][1:noOfDocs,])
}

evidQsConcated_showTopresults("What are the ethical concerns of using personal data and how is this data anonymised for research?", 20)
