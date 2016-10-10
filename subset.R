originalNames <- names(input) 
remove_names <- originalNames[c(grep("^date", originalNames), grep("^_", originalNames))] 
remove_names <- c(remove_names, "splunk_server", "splunk_server_group", "Label", "linecount", "punct", "eventtype") 
subsetNames <- c("_time", originalNames[!(originalNames %in% remove_names)]) 
output <- subset(input, select=subsetNames)