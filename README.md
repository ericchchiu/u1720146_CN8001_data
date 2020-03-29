This folder 'u1720146_CN8001_data' contains the following eight files:
1/ README.TXT (this file)
2/ u1720146_oral_20140618.csv (Note 1)
3/ u1720146_oral_20140623.csv (Note 1)
4/ u1720146_oral_20140708.csv (Note 1)
5/ u1720146_oral_all.csv (Note 2)
6/ u1720146_persons.csv (Note 3)
7/ u1720146_sessions.csv (Note 4)
8/ u1720146_Responsible_Use_of_Data.txt (Converted from 'Responsible Use of Data.pdf', only the main text was preserved)

Notes:
1. These three files are produced by converting the three pdf files that contain oral evidence given to the social media data and real-time analytics inquiry of the Science and Technology Select Committee in 2014. The pdf files are obtainable from https://www.parliament.uk/business/committees/committees-a-z/commons-select/science-and-technology-committee/inquiries/parliament-2010/social-media-data-and-real-time-analytics/?type=Oral#pnlPublicationFilter. All words in the pdf format files were preserved after conversion. However, those lines that are not part of the questions or answers were commented out with a hash sign ('#') (using the multi-editing technique of Notepad++ would make this task less cumbersome). The pipe sign ('|') is used as the delimiter. Content was divided into three columns: "question|person|oralEvidence". A sample R code line for converting 'Q143 Dr d'Aquin: There are mechanisms...' to 'Q143|Dr d'Aquin|There are mechanisms...' is shown below:

tx2  <- gsub(pattern = '(^Q\\d+)(\\s)([A-Z,a-z,’]+\\s*[A-Z,a-z,']*\\s*[A-Z,a-z,']*\\s*)(:)(\\s)', replace = '\\1|\\3|', tx)

2. This file is formed by merging files 2/, 3/ and 4/; deleting all commented-out lines; and adding an index column called 'id' which allocated each line of the file a unique integer in ascending order starting from one. A sample R code line for merging two dataframes is shown below:

df12 <- merge(df, df2, all = TRUE, sort = FALSE)

Two sample R code lines for adding a line index column 'id' to a dataframe are shown below:

df123$id <- seq.int(nrow(df123))
df123_indexed <- df123[,c(ncol(df123),1:(ncol(df123)-1))]

3. The persons.csv file consists of three columns: 'person|sector|description'. Fields of the 'description' column contain those brief descriptions of the committee members and witnesses contained in the pdf format document entitled 'Responsible Use of Data'. The 'sector' column attributed to each witness a sector which includes 'academic', 'business', 'government' and 'ngo' (fields for committee members marked 'member').

4. The sessions.csv file consists of five columns: 'session|start_id|end_id|start_qn|end_qn'. The oral evidence were given in six sessions (two sessions per day). They are labelled as 06181, 06182, 06231, 06232, 07081, 07082. The two columns 'start_id' and 'end_id' recorded the index('id' number of the lines of the oral_all.csv file) of the first line and last line of each session. The columns 'start_qn' and 'end_qn' recorded numbers of the first and last question of each section.

Example R/SQLite code for producing a csv file (ngoEvidence.csv) from oral_all.csv and persons.csv, which contain all oral evidence given by witnesses who were in the NGO sector:
 
oralAll <- read.csv('oral_all.csv', comment.char = '#', sep = '|', stringsAsFactors = FALSE)
persons <- read.csv('persons.csv', comment.char = '#', sep = '|', stringsAsFactors = FALSE)
library(RSQLite)
conndb1 <- dbConnect(RSQLite::SQLite(), 'db1')
dbWriteTable(conndb1, 'oralAll', oralAll)
dbWriteTable(conndb1, 'persons', persons)
dbListTables(conndb1)
ngoEvidence <- dbGetQuery(conndb1, 'WITH ngoEvidence AS (SELECT id, question, persons.person, oralEvidence FROM oralAll INNER JOIN persons ON oralAll.person = persons.person WHERE persons.sector = "ngo" ORDER BY id) SELECT question, person, oralEvidence FROM ngoEvidence')
dbDisconnect(conndb1)
write.table(ngoEvidence, 'ngoEvidence.csv', quote = FALSE, sep = '|', row.names = FALSE)

The csv file (ngoEvidence.csv) can be used for, for example, producing a word cloud, performing close reading, coding, etc.

8 March 2020


