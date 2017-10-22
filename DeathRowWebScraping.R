require(rvest); library(tidyverse);library(stringr);library(stringi);library(XML);library(purrr);library(lubridate);

setwd("G:/Personal/R coding/Rmarkdown/TexasDeathRowInmates")


#################################################################### Web Scraping and Data Export
url <- "http://www.tdcj.state.tx.us/death_row/dr_executed_offenders.html"

#Scrape main table information 
firsttable<- url %>%
    read_html() %>%
    html_nodes("table")%>%
    .[[1]] %>%
    html_table()

#Scrape links off of page.  Used selectorgadget (chrome add-in) to get html_nodes
offenderInfoLinks <- url %>%
  read_html() %>%
  html_nodes("td:nth-child(2) a")

lastStatementLinks <- url %>%
  read_html() %>%
  html_nodes("td~ td+ td a")

#Grab links only
offenderLinks <- sapply(offenderInfoLinks,str_extract_all,'dr_info/.*\\.[a-z]{1,4}')
statementLinks <- sapply(lastStatementLinks,str_extract_all,'dr_info/.*\\.[a-z]{1,4}')

#Convert links to dataframes and concatentate full url path
#Stringi to convert list to dataframe
linkDataFrameSta <- as.data.frame(stri_list2matrix(statementLinks,byrow = TRUE))
linkDataFrameSta$finalLink <- paste0("https://www.tdcj.state.tx.us/death_row/",linkDataFrameSta$V1,sep = "")
linkDataFrameSta$V1 <- NULL

#Scrape last statements
pages <- linkDataFrameSta$finalLink %>%
  map(read_html)

statements <- pages %>%
  map_if(is_empty, ~ NA_character_) %>%
  map(. %>%
          html_nodes("p:nth-child(10)") %>%
            html_text()
          ) 

#Turn statements list into a data frame
statements <- stri_list2matrix(statements, byrow = TRUE)

#Combine both sets of data
cleandata <- cbind(firsttable,statements,stringsAsFactors = FALSE)
cleandata$Date <- as_date(cleandata$Date,"%m/%d/%Y")
cleandata$statements <- iconv(inmateData$statements,"UTF-8","WINDOWS-1252")

#Write data to csv
write.csv(cleandata,"inmateData.csv")

#Scrape Table of number of executions
numOfExecutionsbyYear<- "http://www.tdcj.texas.gov/death_row/dr_executions_by_year.html" %>%
  read_html() %>%
  html_nodes("table")%>%
  .[[1]] %>%
  html_table()
names(numOfExecutionsbyYear) <- c('year','white_count','white_per','black_count','black_per','hispanic_count','hispanic_per',
                                  'other_count','other_per','total_count','total_per')
numOfExecutionsbyYear$year <- as.numeric(numOfExecutionsbyYear$year)
numOfExecutionsbyYear <- numOfExecutionsbyYear[1:35,]
write.csv(numOfExecutionsbyYear,"executionsPerYear.csv")

#Accessed sept 23, 2017
offendersOnDeathRow<- "http://www.tdcj.texas.gov/death_row/dr_offenders_on_dr.html" %>%
  read_html() %>%
  html_nodes("table")%>%
  .[[1]] %>%
  html_table()

offendersOnDeathRow$`Date of Birth` <- as_date(offendersOnDeathRow$`Date of Birth`,"%m/%d/%Y")
offendersOnDeathRow$`Date Received` <- as_date(offendersOnDeathRow$`Date Received`,"%m/%d/%Y")
offendersOnDeathRow$`Date of Offense` <- as_date(offendersOnDeathRow$`Date of Offense`,"%m/%d/%Y")
write.csv(offendersOnDeathRow,"offendersOnDeathRow.csv")

#################################################################### Load Data
library(ggplot2);library(tidyverse);library(scales);library(forcats)

setwd("G:/Personal/R coding/Rmarkdown/TexasDeathRowInmates")

inmateData <- read.csv2("inmateData.csv", sep = ",", stringsAsFactors = FALSE,na.strings = c("Last Statement:","NA"))
execPerYearData <- read.csv2("executionsPerYear.csv", sep = ",", stringsAsFactors = FALSE)
deathRowInmates <- read.csv2("offendersOnDeathRow.csv", sep = ",", stringsAsFactors = FALSE)

#################################################################### Data Processing

### Executed Inmate Data
inmateData <- inmateData[,c(2,5:12)]

### Executions per year
execPerYearData <- execPerYearData[,2:11]

### Death Row Inmates
deathRowInmates <-deathRowInmates[,c(2,4:11)]

#################################################################### Exploratory Data Analysis - Basic Info
inmateData$statements <- iconv(inmateData$statements,"UTF-8","WINDOWS-1252")

#Executions per year 
execuPerYearGraph <- execPerYearData %>%
  select(year,total_count) %>%
  ggplot(mapping = aes(x = year, y= total_count))+
  geom_line()+
  geom_point()+
  labs(x = "Year", y = "Execution Count") + 
  ggtitle("Executions Per Year")+
  theme_classic()
execuPerYearGraph

#Executions by race
execByRaceGraph <- inmateData %>%
  select(Race) %>%
  ggplot(mapping = aes(x = fct_infreq(Race)))+ #fct_infreq from forcats pkg
  geom_bar(stat = "count")+
  labs(x = "Race", y = "Count",title = "Execution Count by Race")+
  theme_classic()
execByRaceGraph

#Histogram of Ages at time of execution
execAgeGraph <- inmateData %>%
  select(Age) %>%
  ggplot(mapping= aes(Age)) +
  geom_histogram(binwidth = 5, colour = "black")+
  labs(x = "Age", y = "Count")+
  theme_classic()
execAgeGraph

#Most Frequent Counties
execCountyGraph <- inmateData %>%
  select(County) %>%
  group_by(County) %>%
  summarize(
    count = n()
  ) %>%
  top_n(10) %>%
  ggplot(mapping = aes(x = County, y = count))+
  geom_bar(stat = "identity")+
  labs(x = "County","Count", title = "Top 10 Counties Inmates Came From")+
  coord_flip()+
  theme_classic()
execCountyGraph

#################################################################### Exploratory Data Analysis - Statements
require(tidytext)

#Tokenize the dataset
lastStatementsData <- inmateData %>%
  select(Execution, statements) %>%
  unnest_tokens(word,statements)

#Remove stop words
data("stop_words")
lastStatementStop <- lastStatementsData %>%
  anti_join(stop_words)

#Visualize most common words
lastStatementStopGraph <- lastStatementStop %>%
  na.omit() %>%
  count(word,sort = TRUE) %>%
  filter(n>50)%>%
  mutate(word = reorder(word,n))%>%
  ggplot(aes(word,n))+
  geom_col()+
  xlab(NULL)+ylab("Word Count")+
  ggtitle("Most Frequent Words") +
  coord_flip()+
  theme_classic()
lastStatementStopGraph

#Bigrams
bigramsData <- inmateData %>%
  select(Execution, statements) %>%
  unnest_tokens(bigram,statements,token = "ngrams", n=2) #%>%
  # count(bigram, sort = TRUE)

#Remove stop words from bigrams
bigrams_counts <- bigramsData %>%
  separate(bigram,c("word1","word2"),sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  count(word1,word2, sort = TRUE)

#Bigrams Separated, looking for "not" to see sentiment
bigrams_separated <- bigramsData %>%
  separate(bigram,c("word1","word2"),sep = " ") %>%
  filter(word1 == "not") %>%
  count(word1,word2, sort = TRUE)

#Trigrams
trigramsData <- inmateData %>%
  select(Execution,statements) %>%
  unnest_tokens(trigram,statements, token="ngrams",n=3) #%>%
  # count(trigram,sort = TRUE)

trigram_counts <- trigramsData %>%
  separate(trigram,c("word1","word2","word3"),sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  count(word1,word2,word3, sort = TRUE) 

#Trigrams Separated, looking for "not" to see sentiment
trigrams_separated <- trigramsData %>%
  separate(trigram,c("word1","word2","word3"),sep = " ") %>%
  filter(word1 == "not") %>%
  count(word1,word2,word3, sort = TRUE)

#Correlations
require(widyr);require(ggraph);require(igraph)

#Most common pairs of words that co-appear within the same section
word_pairs <- lastStatementStop %>%
  pairwise_count(word,Execution, sort = TRUE)

#correlations
word_cor <- lastStatementStop %>%
  group_by(word)%>%
  filter(n()>20) %>%
  pairwise_cor(word,Execution,sort = TRUE)

set.seed(5)
corNetworkPlot <- word_cor %>%
  filter(correlation >= .50) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation),show.legend = FALSE) + 
  geom_node_point(color = "darkblue",size = 2)+
  geom_node_text(aes(label = name),repel = TRUE)+
  theme_void()
corNetworkPlot

#################################################################### Topic Modelling
require(topicmodels)

#Create a document term matrix to use in the modelling
statements_dtm <- inmateData %>%
  select(Execution, statements) %>%
  unnest_tokens(word,statements) %>%
  anti_join(stop_words) %>%
  count(Execution,word) %>%
  cast_dtm(Execution,word,n)

#Creating LDA model, using 4 topics.  Chosen from the Network Plot in the exploratory analysis
statementsLDA <- LDA(statements_dtm, k = 4,control = list(seed = 1234))

#Extract beta(per-topic-per-word probabilities) from model
statements_topics <- tidy(statementsLDA, matrix = "beta")

#Plot 10 top terms for each topic
topTermsPlot <- statements_topics %>%
  group_by(topic) %>%
  top_n(10,beta) %>%
  ungroup() %>%
  arrange(topic,-beta) %>%
  mutate(term = reorder(term,beta)) %>%
  ggplot(aes(term,beta,fill = factor(topic))) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~topic, scales= "free")+
  coord_flip()+
  theme_classic()
topTermsPlot
