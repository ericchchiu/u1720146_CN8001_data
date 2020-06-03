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
dbListTables(conndb1) #inspect the database db1

#if tables oralAll or persons are not in db1
#create them with the below two lines of code
dbWriteTable(conndb1, 'oralAll', oralAll, overwrite = TRUE) #optional
dbWriteTable(conndb1, 'persons', persons, overwrite = TRUE) #optional

#----------------------------------------
#code 04:
#form a df of word count and top six most freequently used words
#for every witnesses
#check whether table oralALL is already in the database db1
dbListTables(conndb1)

#if tables oralAll and persons are already in db1
#(if not, see above to produce them)
#form a dataframe of utterances uttered by all witnesses 
#ranked first by sectors then by witnesses (by alphabetical order)
#evidence uttered by each witness are concatened to form a document
evidWitnssConcated <- dbGetQuery(conndb1, "WITH evidWitnssConcated AS (SELECT sector, persons.person AS person, GROUP_CONCAT(oralEvidence, ' ') AS oralEvidence FROM oralALL JOIN persons ON oralALL.person = persons.person WHERE sector <> 'member' GROUP BY persons.person ORDER BY sector, persons.person) SELECT person, oralEvidence FROM evidWitnssConcated")

#use qdap to form a df of word counts for all witnesses
wordCount <- word_count(evidWitnssConcated$oralEvidence)
evidWitnssConcated$wordCount <- wordCount
evidWitnssConcated_wordCount <- evidWitnssConcated[,c(1,3)]
evidWitnssConcated_wordCount #inspect the wordCount df

#a function for cleaning and lemmatising texts (tm used)
getFrmPersUttrnces_cleanNLemmatsdCrpus <- function(evidWitnssConcated_cell){
corpus <- Corpus(VectorSource(evidWitnssConcated_cell))
corpus <- tm_map(corpus, tolower)
stop.word <- unlist(read.table("stop_word.txt", stringsAsFactors=FALSE))
corpus <- tm_map(corpus, removeWords, stop.word)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords())
corpus <- tm_map(corpus, removePunctuation)
stop.char <- unlist(read.table("stop_char.txt", stringsAsFactors=FALSE))
corpus <- tm_map(corpus, removeWords, stop.char)
corpus <- tm_map(corpus, lemmatize_strings)
corpus <- tm_map(corpus, stripWhitespace)
return(corpus)
}

#a function for picking up the top six most frequently used lexicon words 
top6Words <- function(x) return(freq_terms(getFrmPersUttrnces_cleanNLemmatsdCrpus(x), 6, extend= FALSE)$WORD)

#form list of lists of six words for all witnesses
listwitnss6TopWrds <- lapply(evidWitnssConcated[,2], top6Words)

#convert the list of lists to df 
top6WordsDf <- as.data.frame(t(matrix(unlist(listwitnss6TopWrds), nrow=length(unlist(listwitnss6TopWrds[1])))))

#add column names
colnames(top6WordsDf) <- c('mostFreq', '2nd', '3rd', '4th', '5th', '6th')

#combine the wordCount df and the top six words df
allWitnsWrdCuntNTop6Wrds <- cbind(evidWitnssConcated_wordCount, top6WordsDf)
allWitnsWrdCuntNTop6Wrds <- allWitnsWrdCuntNTop6Wrds[order(allWitnsWrdCuntNTop6Wrds$person),]
allWitnsWrdCuntNTop6Wrds #inspect the df of word counts and top six words

#----------------------------------------
#code 05:
#use sqlite code to create a dataframe of witnesses and the utterances
#of each and every of them ranked by their names and then question
#numbers
witnssUttrnces <- dbGetQuery(conndb1, 'WITH witnssUttrnces AS (SELECT persons.person AS person, id, question, oralEvidence FROM oralAll INNER JOIN persons ON oralAll.person = persons.person WHERE persons.sector <> "member" ORDER BY person, id) SELECT person, question, oralEvidence FROM witnssUttrnces')
dbWriteTable(conndb1, 'witnssUttrnces', witnssUttrnces, overwrite = TRUE)

#----------------------------------------
#code 06:
#commonality and comparison clouds for utterances of 
#Carl Miller vs. Mr Vaizey, Carl Miller vs. Sir Nigel Shadbolt, and Carl Miller vs. Timo Hannay 
#create a dataframe of Carl Miller and Timo Hannay's utterances(concated)...
MVAndCMUttrnces <- dbGetQuery(conndb1, 'SELECT person, GROUP_CONCAT(oralEvidence," ") AS oralEvidence FROM witnssUttrnces WHERE person IN ("Carl Miller", "Mr Vaizey") GROUP BY person ORDER BY person')
SNSAndCMUttrnces <- dbGetQuery(conndb1, 'SELECT person, GROUP_CONCAT(oralEvidence," ") AS oralEvidence FROM witnssUttrnces WHERE person IN ("Carl Miller", "Sir Nigel Shadbolt") GROUP BY person ORDER BY person')
THAndCMUttrnces <- dbGetQuery(conndb1, 'SELECT person, GROUP_CONCAT(oralEvidence," ") AS oralEvidence FROM witnssUttrnces WHERE person IN ("Carl Miller", "Timo Hannay") GROUP BY person ORDER BY person')

prdceComparNCmmonClouds <- function(THAndCMUttrnces, witness1, witness2) {
#clear data and produce tdm for Carl Miller and Timo Hannay
THAndCMUttrnces_cor_cl <- Corpus(VectorSource(THAndCMUttrnces$oralEvidence))
THAndCMUttrnces_cor_cl <- tm_map(THAndCMUttrnces_cor_cl, tolower)
stop.word <- unlist(read.table("stop_word.txt", stringsAsFactors=FALSE))
THAndCMUttrnces_cor_cl <- tm_map(THAndCMUttrnces_cor_cl, removeWords, stop.word)
THAndCMUttrnces_cor_cl <- tm_map(THAndCMUttrnces_cor_cl, removeNumbers)
THAndCMUttrnces_cor_cl <- tm_map(THAndCMUttrnces_cor_cl, removeWords, stopwords())
THAndCMUttrnces_cor_cl <- tm_map(THAndCMUttrnces_cor_cl, removePunctuation)
stop.char <- unlist(read.table("stop_char.txt", stringsAsFactors=FALSE))
THAndCMUttrnces_cor_cl <- tm_map(THAndCMUttrnces_cor_cl, removeWords, stop.char)
THAndCMUttrnces_cor_cl <- tm_map(THAndCMUttrnces_cor_cl, stripWhitespace)
THAndCMUttrnces_cor_cl <- tm_map(THAndCMUttrnces_cor_cl, lemmatize_strings)
THAndCMUttrnces_tdm_cl <- TermDocumentMatrix(THAndCMUttrnces_cor_cl)
colnames(THAndCMUttrnces_tdm_cl) <- c(witness1, witness2)
THAndCMUttrnces_tdm_cl

# coerce as a matrix
THAndCMUttrnces_tdm_cl <- as.matrix(THAndCMUttrnces_tdm_cl)
colnames(THAndCMUttrnces_tdm_cl) <- c(witness1, witness2)

# assign a palette
pal <- brewer.pal(5, "Accent")

# plot wordclouds
set.seed(12345)
comparison.cloud(THAndCMUttrnces_tdm_cl, scale=c(2,0.5), max.words = 50, rot.per = 0.3, random.order=FALSE, color=pal, title.size=2)
set.seed(12345)
commonality.cloud(THAndCMUttrnces_tdm_cl, scale=c(5,0.5), max.words = 50, rot.per = 0.3, random.order=FALSE, color=pal, title.size=2)
}

prdceComparNCmmonClouds(MVAndCMUttrnces, "Carl Miller", "Mr Vaizey")
prdceComparNCmmonClouds(SNSAndCMUttrnces, "Carl Miller", "Sir Nigel Shadbolt")
prdceComparNCmmonClouds(THAndCMUttrnces, "Carl Miller", "Timo Hannay")

#----------------------------------------
#code 07:
#use functions to reduce repetition of code

#7.1 function for forming cleaned and lemmatised corpus from sector utterances
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

#7.2 function for producing a word cloud from a corpus
prduceFrmSctorUttrnces_corpus_wordCloud <- function(sctorUttrnces_corpus){
set.seed(12345)
wordcloud(sctorUttrnces_corpus, scale=c(3,0.5), max.words = 100, rot.per = 0.3, random.order=FALSE, colors="black")
}

#7.3 function for producing a tdm from a corpus
prduceFrmSctorUttrnces_corpus_tdm <- function(sctorUttrnces_corpus) return(TermDocumentMatrix(sctorUttrnces_corpus))

#7.4 function for producing a csv file of word count list from a tdm
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

#7.5 function for finding word frequencies directly from a corpus by 
#using the freq_terms function of the qdap package
getFrmCorpus_frequenciesOfWords <- function(corpus, topHowMany) return(freq_terms(as.data.frame(corpus), top = topHowMany))

#7.6 function for producing an association word list from a 
#dtm or a tdm
prduceFromDtmTdmAssocsWordsList <- function(dtmTdm, charVectWords, corLimitVect) return(findAssocs(dtmTdm, charVectWords, corLimitVect))

#7.7 function for producing a df from an association word list
produceFromAssocsWordsList_df <- function(tdmDtm_AssocsWordsList){
return(list_vect2df(tdmDtm_AssocsWordsList, col1 = 'word', col2 = 'assocsWords', col3 = 'corrScore'))
}

#----------------------------------------
#code 08:
#divide witnesses into the following four sectors:
#academic(Professor Yates, Dr McPherson, Professor Preston, Sir Nigel
#Shadbolt, Professor McAuley, Professor van Zoonen, Dr d'Aquin,
#Professor Robertson, Dr Macnish, Dr Elliot), business(Sureyya Cansoy,
#Timo Hannay), government(Steve Wood, Mr Vaizey) and 
#ngo(Carl Miller, Emma Carr)
#group utterances by sectors and treat replies to a question from a
#sector a document
#create a dataframe for each sector's utterances
academicUttrnces  <- dbGetQuery(conndb1, "SELECT question, GROUP_CONCAT(oralEvidence,' ') AS oralEvidence FROM witnssUttrnces JOIN persons ON witnssUttrnces.person = persons.person WHERE sector =  'academic' GROUP BY question ORDER BY question")

businessUttrnces  <- dbGetQuery(conndb1, "SELECT question, GROUP_CONCAT(oralEvidence,' ') AS oralEvidence FROM witnssUttrnces JOIN persons ON witnssUttrnces.person = persons.person WHERE sector =  'business' GROUP BY question ORDER BY question")

govtUttrnces <- dbGetQuery(conndb1, "SELECT question, GROUP_CONCAT(oralEvidence,' ') AS oralEvidence FROM witnssUttrnces JOIN persons ON witnssUttrnces.person = persons.person WHERE sector =  'government' GROUP BY question ORDER BY question")

ngoUttrnces  <- dbGetQuery(conndb1, "SELECT question, GROUP_CONCAT(oralEvidence,' ') AS oralEvidence FROM witnssUttrnces JOIN persons ON witnssUttrnces.person = persons.person WHERE sector =  'ngo' GROUP BY question ORDER BY question")

#code 08_1 create a dataframe for all witnesses' utterances concatenated
allWitnessesUttrnces  <- dbGetQuery(conndb1, "SELECT question, GROUP_CONCAT(oralEvidence,' ') AS oralEvidence FROM witnssUttrnces JOIN persons ON witnssUttrnces.person = persons.person WHERE sector <>  'member' GROUP BY question ORDER BY question")

#code 08_2 create a dataframe for final report the summary part concatenated
responsibleUseOfData <- read.csv('u1720146_CN8001_Responsible_Use_of_Data.csv', comment.char = '#', quote = '^"\'', sep = '|', stringsAsFactors = FALSE)
dbWriteTable(conndb1, 'responsibleUseOfData', responsibleUseOfData, overwrite = TRUE)
responsibleUseOfDataConcatd  <- dbGetQuery(conndb1, "SELECT paraNo, GROUP_CONCAT(content,' ') AS oralEvidence FROM responsibleUseOfData")

#----------------------------------------
#code 09:
#code for applying the above functions to perform text mining

#9.1 code for producing a clean and lemmatised corpus with the
#utterances of witnesseses of each and every sector
#academic
academicUttrnces_corpus <- getFrmSctorUttrnces_cleanNLemmatsdCrpus(academicUttrnces)
#business
businessUttrnces_corpus <- getFrmSctorUttrnces_cleanNLemmatsdCrpus(businessUttrnces) #7.1
#government
govtUttrnces_corpus <- getFrmSctorUttrnces_cleanNLemmatsdCrpus(govtUttrnces)
#ngo
ngoUttrnces_corpus <- getFrmSctorUttrnces_cleanNLemmatsdCrpus(ngoUttrnces)

#9.1_1 #all witnesses
allWitnessesUttrnces_corpus <- getFrmSctorUttrnces_cleanNLemmatsdCrpus(allWitnessesUttrnces)

#9.1_2 #final report the summary part
responsibleUseOfDataConcatd_corpus <- getFrmSctorUttrnces_cleanNLemmatsdCrpus(responsibleUseOfDataConcatd)


#9.2 code for producing a word cloud with each and every 
#sector utterance corpus
#academic
prduceFrmSctorUttrnces_corpus_wordCloud(academicUttrnces_corpus) #7.2
#business
prduceFrmSctorUttrnces_corpus_wordCloud(businessUttrnces_corpus)
#government
prduceFrmSctorUttrnces_corpus_wordCloud(govtUttrnces_corpus)
#ngo
prduceFrmSctorUttrnces_corpus_wordCloud(ngoUttrnces_corpus)

#9.2_1 #all witnesses
prduceFrmSctorUttrnces_corpus_wordCloud(allWitnessesUttrnces_corpus)

#9.2_2 #final report the summary part
prduceFrmSctorUttrnces_corpus_wordCloud(responsibleUseOfDataConcatd_corpus)

#9.3 code for producing a tdm from each and every sector corpus
#academic
academicUttrnces_tdm <- prduceFrmSctorUttrnces_corpus_tdm(academicUttrnces_corpus) #7.3
#business
businessUttrnces_tdm <- prduceFrmSctorUttrnces_corpus_tdm(businessUttrnces_corpus)
#government
govtUttrnces_tdm <- prduceFrmSctorUttrnces_corpus_tdm(govtUttrnces_corpus)
#ngo
ngoUttrnces_tdm <- prduceFrmSctorUttrnces_corpus_tdm(ngoUttrnces_corpus)

#9.4 code for producing a csv file of word freqency list
#with each and every tdm
#academic
prduceFrmSctorUttrnces_tdm_WordCntCsvFile(academicUttrnces_tdm, 'academicUttrncesWordCount.csv') #7.4
#business
prduceFrmSctorUttrnces_tdm_WordCntCsvFile(businessUttrnces_tdm, 'businessUttrncesWordCount.csv')
#government
prduceFrmSctorUttrnces_tdm_WordCntCsvFile(govtUttrnces_tdm, 'govtUttrncesWordCount.csv')
#ngo
prduceFrmSctorUttrnces_tdm_WordCntCsvFile(ngoUttrnces_tdm, 'ngoUttrncesWordCount.csv')

#9.5 code for producing a word frequency list from each and every 
#corpus instead of from tdm or dtm (results should be the same 
#as those contained in the abovementioned csv files) 
#academic
academic_wordFrequency <- getFrmCorpus_frequenciesOfWords(academicUttrnces_corpus, 20) #7.5
#business
business_wordFrequency <- getFrmCorpus_frequenciesOfWords(businessUttrnces_corpus, 20)
#government
govt_wordFrequency <- getFrmCorpus_frequenciesOfWords(govtUttrnces_corpus, 20)
#ngo
ngo_wordFrequency <- getFrmCorpus_frequenciesOfWords(ngoUttrnces_corpus, 20)

#forming a dataframe of the top 20 most frequent words of all sectors

#9.6 code for producing a dataframe which showing the 
#top 20 most frequent words of each and every sector
word_frequency_top20_df <- as.data.frame(cbind(academic_wordFrequency[1:20,1], business_wordFrequency[1:20,1], govt_wordFrequency[1:20,1], ngo_wordFrequency[1:20,1]))
names(word_frequency_top20_df) <- c('academicSector', 'businessSector', 'govtSector', 'ngoSector')
word_frequency_top20_df #show the df

#9.7 code for finding words in each and every corpus which are 
#most correlated with datum, government, legislation and research 
charVectWords <- c('datum', 'government', 'legislation', 'research')
corLimitVect <- c(0.4, 0.5, 0.5, 0.6)
#academic
academicUttrnces_tdm_AssocsWordsList <- prduceFromDtmTdmAssocsWordsList(academicUttrnces_tdm, charVectWords, corLimitVect)
#business
businessUttrnces_tdm_AssocsWordsList <- prduceFromDtmTdmAssocsWordsList(businessUttrnces_tdm, charVectWords, corLimitVect) #7.6
#government
govtUttrnces_tdm_AssocsWordsList <- prduceFromDtmTdmAssocsWordsList(govtUttrnces_tdm, charVectWords, corLimitVect)
#ngo
ngoUttrnces_tdm_AssocsWordsList <- prduceFromDtmTdmAssocsWordsList(ngoUttrnces_tdm, charVectWords, corLimitVect)

#9.7_1 allWitnessesUttrnces
allWitnessesUttrnces_tdm <- prduceFrmSctorUttrnces_corpus_tdm(allWitnessesUttrnces_corpus)
charVectWords2 <- c('legislation', 'research', 'people', 'social')
corLimitVect2 <- c(0.5, 0.5, 0.5, 0.5)
allWitnessesUttrnces_tdm_AssocsWordsList <- prduceFromDtmTdmAssocsWordsList(allWitnessesUttrnces_tdm, charVectWords2, corLimitVect2)


#9.8 code for producing a dataframe from the association word lists in respect the government sector corpus
govtUttrnces_AssocsWords_df <- produceFromAssocsWordsList_df(govtUttrnces_tdm_AssocsWordsList) #7.7
#show part of the df: words associated with 'government'. corLimit = 0.5
govtUttrnces_AssocsWords_df[58:105,]

#9.8_1 allWitnessesUttrnces
allWitnessesUttrnces_df <- produceFromAssocsWordsList_df(allWitnessesUttrnces_tdm_AssocsWordsList) #7.7
#show the df: words associated with 'legislation'. corLimit = 0.5
allWitnessesUttrnces_df

#----------------------------------------
#code 10:
#code for plotting a graph of those words associated with the word
#'government' with the correlation value equal or higher than 0.5 in the
#government sector utterances corpus

#use the list vector obtained from code 9.7 above
govtUttrnces_tdm_AssocsWords_df_government <- list_vect2df(govtUttrnces_tdm_AssocsWordsList[2, drop = FALSE], col2 = "assocsWords", col3 = "corrScore")

#use the df obtained from code 9.8 above to plot the graph
govtUttrnces_tdm_AssocsWords_df_government <- govtUttrnces_AssocsWords_df[is.element(govtUttrnces_AssocsWords_df$word, 'government'),][,2:3]
ggplot(govtUttrnces_tdm_AssocsWords_df_government, aes(corrScore, assocsWords)) + 
  geom_point(size = 3) +   ggtitle('govtWtnsses_OralEvidence_WordsAssocsWth_government') +
  theme_gdocs()
  
#code 10_1: allWitnessesUttrnces
allWitnessesUttrnces_tdm_AssocsWords_df_legislation <- list_vect2df(allWitnessesUttrnces_tdm_AssocsWordsList[1, drop = FALSE], col2 = "assocsWords", col3 = "corrScore")
allWitnessesUttrnces_tdm_AssocsWords_df_legislation <- allWitnessesUttrnces_df[is.element(allWitnessesUttrnces_df$word, 'legislation'),][,2:3]
ggplot(allWitnessesUttrnces_tdm_AssocsWords_df_legislation, aes(corrScore, assocsWords)) + 
  geom_point(size = 3) +   ggtitle('allWitnesses_OralEvidence_WordsAssocsWth_legislation') +
  theme_gdocs()

#----------------------------------------
#code 11
#attempt to use if-tdf to find out which query or queires of 
#the terms of reference each question is belonged to
#(see termsOfReference.csv for the six terms of reference)

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

#optional: connect and inspect the database db1
#if table oralAll is not there, create one with code 02
#and code 03
#conndb1 <- dbConnect(RSQLite::SQLite(), 'db1')
#dbWriteTable(conndb1, 'oralAll', oralAll)

#create df of questions. Content of each question and its replies
#form a document
evidQsConcated <- dbGetQuery(conndb1, 'SELECT question, GROUP_CONCAT(oralEvidence, " ") AS oralEvidence FROM oralAll GROUP BY question')

#form a named list of the questions
evidQsConcated_list = as.list(evidQsConcated[,2])
evidQsConcated_N.docs = length(evidQsConcated_list)
names(evidQsConcated_list) = evidQsConcated[,1]

#form a named list of the tors (cleaned and lemmatised version)
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

#change the tors wording to original wording
hc245_tors_list = unlist(termsOfReference_df[,2])
names(hc245_tors_list) = paste0("query", c(1:hc245_tors_N.query))

evidQsConcated_results.df <- data.frame(querylist = hc245_tors_list,evidQsConcated_doc.scores)

evidQsConcated_showTopresults <- function(query, noOfDocs){
  x = evidQsConcated_results.df[which(evidQsConcated_results.df$querylist == query),]
  yy =  data.frame(t(x),rownames(t(x)),row.names = NULL)[-1,]
  names(yy) = c("score","docs")
  yy$score = as.numeric(as.character(yy$score))
  yyy = yy[order(yy$score,decreasing = T),]
  
  return(yyy[which(yyy$score > 0),][1:noOfDocs,])
}

#find the top 20 questions which are, 
#according to tf-idf, most related to terms of reference 1
evidQsConcated_showTopresults("How can real-time analysis of social media data benefit the UK? What should the Government be doing to maximise these benefits?", 20)

#according to tf-idf, the top 20 questions which are
#most related to terms of reference 4
evidQsConcated_showTopresults("What are the ethical concerns of using personal data and how is this data anonymised for research?", 20)

#please see termsOfReference.csv for the six terms of reference

#----------------------------------------
#code 12
#finding the p-value by simulation

zero1200One800 = append(rep(0, 1200), rep(1, 800))
countAtLeast <- function(smplPop, pickup, rept, threshold){
cnt2 = 0
for(i in 1: 100) {
cnt <- 0
for(i in 1: rept){
if(sum(sample(smplPop, pickup, replace = FALSE)) <= threshold) cnt <- cnt + 1
}
cnt2 = cnt2 + cnt
}
return(cnt2/100)
}
zero1200One800 = append(rep(0, 1200), rep(1, 800))
p.value <- countAtLeast(zero1200One800, 17, 100, 4)
p.value

#----------------------------------------
#code 13
#Chi-squared test (independence test, by simulation and by the fisher method)

atttdeToEULegsntnInput =("
Sector,Positive,Neutral,Negative
academic,1,6,3
business,0,0,2
government,2,0,0
ngo,1,1,1
")
atttdeToEULegsntnMtrx = as.matrix(read.table(textConnection(atttdeToEULegsntnInput), sep = ',', header=TRUE, row.names=1))
atttdeToEULegsntnMtrx
atttdeToEULegsntnChisq <- chisq.test(atttdeToEULegsntnMtrx)
atttdeToEULegsntnChisq
atttdeToEULegsntnChisqSimu <- chisq.test(atttdeToEULegsntnMtrx, simulate.p.value = TRUE)
atttdeToEULegsntnChisqSimu
atttdeToEULegsntnFisher <- fisher.test(atttdeToEULegsntnMtrx)
atttdeToEULegsntnFisher

#----------------------------------------
#code 14
#Chi-squared test (one homogeneity test and two independence tests)

theme_evidencePara <- read.csv('theme_evidencePara.csv', stringsAsFactors = FALSE)
theme_reportPara <- read.csv('theme_reportPara.csv', stringsAsFactors = FALSE)
responsibleUseOfData <- read.csv('u1720146_CN8001_Responsible_Use_of_Data.csv', comment.char = '#', quote = '^"\'', sep = '|', stringsAsFactors = FALSE)
dbWriteTable(conndb1, 'theme_evidencePara', theme_evidencePara, overwrite = TRUE)
dbWriteTable(conndb1, 'theme_reportPara', theme_reportPara, overwrite = TRUE)
dbWriteTable(conndb1, 'responsibleUseOfData', responsibleUseOfData, overwrite = TRUE)
dbListTables(conndb1) #inspect db1

themeEvidenceNoOfOwrds <- dbGetQuery(conndb1, "SELECT theme, ROUND(SUM(wholeOrHalf * (LENGTH(TRIM(oralEvidence)) - LENGTH(TRIM(REPLACE(oralEvidence, ' ', ''))) + 1))) AS 'totalNoOfWords(Evidence)' FROM oralAll JOIN theme_evidencePara ON id = paraNo GROUP BY theme ORDER BY CASE WHEN theme = 'benefits' THEN 1 ELSE theme END") #41914 correct!

themeReportNoOfWords <- dbGetQuery(conndb1, "SELECT theme, SUM(LENGTH(TRIM(content)) - LENGTH(TRIM(REPLACE(content, ' ', ''))) + 1) AS 'totalNoOfWords(Report)' FROM theme_reportPara JOIN responsibleUseOfData ON theme_reportPara.paraNo = responsibleUseOfData.paraNo GROUP BY theme ORDER BY CASE WHEN theme = 'benefits' THEN 1 ELSE theme END") #6604 correct!

#produce an observed count table of distribution of words
themeEvidenceReportWordsDf <- cbind(themeEvidenceNoOfOwrds, themeReportNoOfWords)
rownames(themeEvidenceReportWordsDf) <- themeEvidenceReportWordsDf[,1]
themeEvidenceReportWordsDf <- themeEvidenceReportWordsDf[,c(2,4)]
evidenceReportThemeWordsDf <- t(themeEvidenceReportWordsDf)
evidenceReportThemeWordsDf

#chi-squared test (homogeneity test)
#null hypothesis: difference in distributions of words across the themes are not significant between the witnesses' evidence and the report
chisq.test(evidenceReportThemeWordsDf)

sectorThemeNoOfWords <- dbGetQuery(conndb1, "WITH oralAllWithSector AS (SELECT id, sector, LENGTH(TRIM(oralEvidence)) - LENGTH(TRIM(REPLACE(oralEvidence, ' ', ''))) + 1 AS noOfWords FROM oralAll JOIN persons WHERE oralAll.person = persons.person) SELECT sector, theme, ROUND(sum(noOfWords * wholeOrHalf)) AS noOfWords FROM oralAllWithSector JOIN theme_evidencePara WHERE id = paraNo GROUP BY sector, theme") #41913 correct!
sectorThemeNoOfWords

sectorThemeNoOfWordsTable <- xtabs(noOfWords~sector+theme,data = sectorThemeNoOfWords)
sectorThemeNoOfWordsTable
#chi-squared test (independence test)
#null hypothesis: career sector persons belong to and the length of their utterances allocated to the four themes are independent 
chisq.test(sectorThemeNoOfWordsTable)

panelThemeNoOfWords <- dbGetQuery(conndb1, "WITH oralAllWithPanel AS (SELECT id, panel, (LENGTH(TRIM(oralEvidence)) - LENGTH(TRIM(REPLACE(oralEvidence, ' ', ''))) + 1) AS noOfWords FROM oralAll JOIN persons WHERE oralAll.person = persons.person) SELECT panel, theme, ROUND(SUM(noOfWords * wholeOrHalf)) AS noOfWords FROM oralAllWithpanel JOIN theme_evidencePara WHERE id = paraNo GROUP BY panel, theme")
panelThemeNoOfWords

panelThemeNoOfWordsTable <- xtabs(noOfWords~panel+theme,data = panelThemeNoOfWords)
panelThemeNoOfWordsTable
#chi-squared test (independence test)
#null hypothesis: lengths of utterances allocated to the four themes were independent to the panels  
chisq.test(panelThemeNoOfWordsTable)
