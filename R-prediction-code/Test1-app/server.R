# ----------------------------------------------------------------------------
# Author  : Team Blodgic
# Purpose : DMBA Assingment 7
# Note1   : Creates a data frame with high frequency words and their counts
# Note2   : Rows correspond to document/presentation
# Note3   : Columns correspond to high frequency words, Cells represents counts
#
#____________________________________________________________________________
# This one reads the security presentations from a given directory
# Directory hard coded for now
# Extract the 'description' 'title' field alone in to a separate document using 'cut'
# Jay's code will populate the presentations with only 'title' and 'decription' fields
# and drop it in a directory

library('tm')
library('plyr')
library('class')
library('SnowballC')
library('memoise')

# Set appropriate options.



library(shiny)
shinyServer(
  function(input,output) {
    
    
    output$word.cloud1 <- renderPlot({
      options(stringsAsFactors=FALSE)
      
      # Change the path below to include your own directory
      # This will also be the integration point to integrate the document cleaning
      # The Presentations_Mildred_North_America_Events csv is located within the Test1-App folder
      SecDocs <- Corpus (DirSource('~/Desktop/Years_capstone/1992.csv'))
      
      #Please change above DirSource^ 
      
      # Pre process the data
      
      SecDocs <- tm_map(SecDocs, stripWhitespace)
      
      SecDocs <- tm_map(SecDocs, tolower)
      
      SecDocs <- tm_map(SecDocs, removePunctuation)
      
      #require(parallel)
      #library(multicore)
      #mclapply(1:3, mc.cores=2, mc.preschedule= FALSE)
      SecDocs <- tm_map(SecDocs, removeWords, c(stopwords("english"),"even", "like", "first", "get", "discuss", "hat", "just", "many", "mr.", "new", "number", "talk", "time", "used", "use", "using", "well","prior", "team","one","work", "since", "group", "work", "now", "several"))
      
      # Brennan - Create your own stopwords for common words such as 'also' , 'use'
      # Remove them using  tm_map(SecDocs, removewords, MyStopWords)
      # Refer to the TM Infrastructure documentation
      
      
      SecDocs <- tm_map(SecDocs, stemDocument)
      
      
      SecDoc.clean <- tm_map(SecDocs, PlainTextDocument)
      
      # Code to calculate term document matrix
      
      SecDoc.tdm <- TermDocumentMatrix(SecDoc.clean)
      
      # Find high frequency terms in the term document matrix
      
      StdmHighFreq <- findFreqTerms(SecDoc.tdm, 500, 2000)
      
      # get the tfidf 
      
      tfidf_doc <- DocumentTermMatrix(SecDoc.clean, control = list(weighting = weightTfIdf))
      
      # transpose this matrix to reverse the col and row
      # THis contains the term frequencys.df
      
      StfidfHF <- findFreqTerms(tfidf_doc, 1000)
      
      s.tfidf.mat <- data.matrix(tfidf_doc)
      
      s.tfidf.df <- as.data.frame(s.tfidf.mat, stringsAsFactors=FALSE)
      
      
      s.t.mat <- t(data.matrix(SecDoc.tdm))
      
      
      s.t.df <- as.data.frame(s.t.mat, stringsAsFactors=FALSE)
      
      # This data frame below contains only High Freq terms
      # There will be as many rows as the number of presentations
      
      Hfdf1 <- s.t.df [,c(StdmHighFreq)]
      
    
      
      #use wordcloud package 
      #use Presentations_Mildred_North_America_Events as document 
      View(Hfdf1)
      
      library(wordcloud)
      
      #Remove the first 3 columns <d0> 2010,,,,,,,english 2012,,,,,,,english
      Hfdf1[1:2] <- list(NULL)
      
      #Final Word Count
      View(Hfdf1)
      
      
      #wordcloud code 
      #also seen here http://beyondvalence.blogspot.com/2014/01/text-mining-4-performing-term.html
      
      #stanndard wordcloud code
      
      set.seed(1234)
      displayWC <- wordcloud(names(Hfdf1), Hfdf1, min.freq=300)
      
      #Make the wordcloud look pretty
      grayLevels <- gray( (Hfdf1 + 30) / (max(Hfdf1) + 30 ))
      word.cloud <- wordcloud(words=names(Hfdf1), freq=Hfdf1,
                              min.freq=300, random.order=F, colors=grayLevels)
      
    })
  })

