# Jay P Kesavan
# Date : 8/25/2014
# This is a set of utilities to manage files.
# CSV Row Counts : Gives a count of rows in a spreadsheet
# CSV File Add: Adds all CSVs into one combined file.
######################################################

CSVRowCount <- function(folderNameToReadFrom,fileDelim) {
  
  setwd(folderNameToReadFrom)
  file_list <- list.files()
  #DataFrame to hold MetaData
  metaDataDF <- data.frame("FileName"= character(),"NumRows"= numeric()) 
    
  #Read List of all Files
  for (file in file_list){
    # if the merged dataset doesn't exist, create it
    dataset <- read.delim(file, header=TRUE, sep=fileDelim)
    #Some Logging
    print(paste0(file,":",dim(dataset)))
    newRow <- data.frame(FileName=file, NumRows=nrow(dataset))
    metaDataDF <- rbind(metaDataDF, newRow)
    
  }
  return(list(metaDataDF))
}

CSVFileAdd <- function(folderNameToReadFrom,fileDelim) {
 
  setwd(folderNameToReadFrom)
  file_list <- list.files()
  #DataFrame to hold MetaData
  metaDataDF <- data.frame("FileName"= character(),"NumRows"= numeric()) 
  #DataFrame to hold all excels
  containerDF = data.frame()
  
  #Read List of all Files
  for (file in file_list){
    # if the merged dataset doesn't exist, create it
    dataset <- read.delim(file, header=TRUE, sep=fileDelim)
    #Some Logging
    print(paste0(file,":",dim(dataset)))
    newRow <- data.frame(FileName=file, NumRows=nrow(dataset))
    metaDataDF <- rbind(metaDataDF, newRow)
    containerDF <- rbind(containerDF, dataset)
    
  }
  return(list(metaDataDF,containerDF))
}

#This function is work in progress
SplitFiles <- function(folderNameToReadFrom,fileDelim,as.integer(numberOfFiles)) {
  
  setwd(folderNameToReadFrom)
  file_list <- list.files()
  #DataFrame to hold MetaData
  metaDataDF <- data.frame("FileName"= character(),"NumRows"= numeric()) 
 
  listDF = as.list()
 
  #Read List of all Files
  for (file in file_list){
  
    # if the merged dataset doesn't exist, create it
    dataset <- read.delim(file, header=TRUE, sep=fileDelim)
    numberOfRowsInDataSet = nrow(dataset)
    numberofRowsinsubset = numberOfRowsInDataSet/numberOfFiles
    
    count = 1
    #numberofFiles = 0
    while(count<numberofFiles+1){
  
          subset = head(dataset,numberofRowsinsubset)
          #Some Logging
          print(paste0(file,":",dim(dataset)))
          fileName = cat(file, "-Part-",count)
          
          newRow <- data.frame(FileName=fileName, NumRows=nrow(subset))
          metaDataDF <- rbind(metaDataDF, newRow)
          listDF[count] = subset 
          count = count + 1
    }
  }

  
  listDF[length(listDF)+1] = metaDataDF
  return(listDF)

}

#Examples of invocation
#Ideally you could use Source and invoke in another file.
#Eventually - it would be great if we write wrappers around more
#core functionality which we can invoke from a Main program.
#We need DB Utils, Model Utils, List Utils to store Security related terms.
#We need Plot Utils etc. All of this would be customized wrappers around R packages
#specific to our project.

listDF <- CSVFileAdd("C:/Lora/NYU/09302014",",")
listDF <- CSVRowCount("C:/Lora/09222014/History/DataAnalysis",",")

#Write results to a file.
metaDataResultDF = as.data.frame(listDF[2])
write.table(metaDataResultDF, 
            file = "C:/Lora/NYU/09302014/FullSet.csv",
            row.names=FALSE, 
            na="",
            col.names=TRUE, 
            sep=",")


