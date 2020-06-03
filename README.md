This folder 'u1720146_CN8001_data' contains, among others, the following eight files:
1/ README.TXT (this file)
2/ u1720146_oral_20140618.csv (Note 1)
3/ u1720146_oral_20140623.csv (Note 1)
4/ u1720146_oral_20140708.csv (Note 1)
5/ u1720146_persons.csv (Note 2)
6/ u1720146_Responsible_Use_of_Data.txt (Converted from 'Responsible Use of Data.pdf', only the main text was preserved)
7/ theme_evidencePara.csv
8/ theme_reportPara.csv
9/ stop_char.txt
10/ stop_word.txt
11/ csvText.txt
12/ u1729146_CN8001_data.r (the coursework, file 14/ is based on this R file)
13/ u1729146_CN8001_data_withAdditions.r
14/ u1720146_CN8001_final.docx (the coursework)

Notes:
1. These three files are produced by converting the three pdf files that contain oral evidence given to the social media data and real-time analytics inquiry of the Science and Technology Select Committee in 2014. The pdf files are obtainable from https://www.parliament.uk/business/committees/committees-a-z/commons-select/science-and-technology-committee/inquiries/parliament-2010/social-media-data-and-real-time-analytics/?type=Oral#pnlPublicationFilter. All words in the pdf format files were preserved after conversion. However, those lines that are not part of the questions or answers were commented out with a hash sign ('#') (using the multi-editing technique of Notepad++ would make this task less cumbersome). The pipe sign ('|') is used as the delimiter. Content was divided into three columns: "question|person|oralEvidence". A sample R code line for converting 'Q143 Dr d'Aquin: There are mechanisms...' to 'Q143|Dr d'Aquin|There are mechanisms...' is shown below:

tx2  <- gsub(pattern = '(^Q\\d+)(\\s)([A-Z,a-z,’]+\\s*[A-Z,a-z,']*\\s*[A-Z,a-z,']*\\s*)(:)(\\s)', replace = '\\1|\\3|', tx)

(The convertion can also be performed by using the regular express function of notepad++:
Find what: (^Q\d+)(\s)([A-Z,a-z,’]+\s*[A-Z,a-z,']*\s*[A-Z,a-z,']*\s*)(:)(\s)
Replace with: \1|\3|
(Replace All) )

2. The persons.csv file consists of four columns: 'person|panel|sector|description'. Fields of the 'description' column contain those brief descriptions of the committee members and witnesses contained in the pdf format document entitled 'Responsible Use of Data'. There were six panels. The 'sector' column attributed to each witness a sector which includes 'academic', 'business', 'government' and 'ngo' (fields for committee members marked 'member').

For further details, please see the coursework entitled 'Text mining and qualitative research on transcripts of oral evidence given to the parliamentary inquiry entitled "Social media data and real time analytics"' which can be found in this repository (14/ u1720146_CN8001_final.docx). 

Eric Chiu
u1720146@uel.ac.uk
3 June 2020


