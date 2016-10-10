#ET Translate @Blodgic 07/24/2016

#R libraries to include
library(stringr)
library(ggplot2)

#set your working directory here and enter the directory in between the quotes
setwd("~/Downloads/")
#translate a .tgz to .txt and select your ET rules file here
#https://rules.emergingthreats.net/open/snort-2.9.0/
untar(file.choose(),list = TRUE)

#set directory within the ET rules folder directory
setwd(dir = "rules/")

# rename all .rules files to rules.txt files
#http://www.r-bloggers.com/programatically-rename-files-or-do-other-stuff-to-them-in-r/
rules_files <- list.files(path = ".", pattern = "*.rules")
sapply(rules_files, FUN=function(eachPath) {
  file.rename(from=eachPath, to=sub(pattern=".rules", replacement = ".rules.txt", eachPath))
})
#list.files(path = ".")
rules_files.txt <- list.files(path=".", pattern = "*.rules.txt")

#combine all .txt files to one
outFile <- file("combined.txt", "w")
for (i in rules_files.txt){
  x <- readLines(i)
  writeLines(x[2:(length(x)-1)], outFile)
}

rulefile.etpro <- readLines("combined.txt")

signatures <- grep("^alert.+\\;\\s?\\)$", rulefile.etpro, value=TRUE)

et.pro.df <- as.data.frame(signatures)


et.pro.df$msg <- str_extract(et.pro.df$signatures,'msg\\:\\s?.+?\\;')
et.pro.df$sid <- str_extract(et.pro.df$signatures, 'sid\\:\\d+') 
et.pro.df$classtype <- str_extract(et.pro.df$signatures,'classtype\\:\\w+\\-\\w+')

et.pro.df_proto <- str_extract(et.pro.df$signatures, '^(?:\\S+\\s){1}(\\S+)')
et.pro.df_proto <- gsub('alert\\s+',"", et.pro.df_proto)
et.pro.df_proto.df <- as.data.frame(table(et.pro.df_proto))
et.pro.df_proto.df <- et.pro.df_proto.df [order(et.pro.df_proto.df$Freq),]
et.pro.df$proto <- et.pro.df_proto

et.pro.df_sip <- str_extract(et.pro.df$signatures, '^(?:\\S+\\s){2}(\\S+)')
et.pro.df_sip <- gsub('alert\\s+\\w+\\s+',"", et.pro.df_sip)
et.pro.df$sip <- et.pro.df_sip

et.pro.df_sport <- str_extract(et.pro.df$signatures, '^[^-]*')
et.pro.df_sport <- str_extract(et.pro.df_sport, '(\\w+|\\W+|\\S+)(\\s+)$')
et.pro.df$sport <- et.pro.df_sport

et.pro.df_dip <- str_extract(et.pro.df$signatures, perl('(?<=\\-\\>)\\s+\\S+'))
et.pro.df_dip <- gsub('^\\s+',"", et.pro.df_dip)
et.pro.df$dip <- et.pro.df_dip
et.pro.df_dip.tb <- as.data.frame(table(et.pro.df$dip))
et.pro.df_dip.tb <- et.pro.df_dip.tb [order(-et.pro.df_dip.tb$Freq),]


et.pro.df_dport <- str_extract(et.pro.df$signatures, perl('(?<=\\-\\>)\\s+\\S+\\s+\\S+'))
et.pro.df_dport <- gsub('^\\s+\\S+',"", et.pro.df_dport)
et.pro.df_dport <- gsub('^\\s+',"",et.pro.df_dport)
et.pro.df$dport <- et.pro.df_dport


et.pro.df$sid <- gsub("sid\\:","", et.pro.df$sid)
et.pro.df$msg <- gsub("msg\\:\\s?","",et.pro.df$msg)
et.pro.df$msg <- gsub('^\\"',"", et.pro.df$msg)
et.pro.df$msg <- gsub('\\"\\;$',"", et.pro.df$msg)

et.pro.df$classtype <- gsub('classtype\\:',"", et.pro.df$classtype)
et.pro.df$msg_type <- str_extract(et.pro.df$msg, "^\\s?\\w+\\s+\\w+")

et.pro.df$rule_who <- str_extract(et.pro.df$msg, "^\\w+")

et.pro.df$CVE <- str_extract(et.pro.df$signatures, 'reference\\:cve.+?\\;')
et.pro.df$CVE<- gsub('reference\\:',"", et.pro.df$CVE)
et.pro.df$CVE<- gsub('\\;',"", et.pro.df$CVE)
et.pro.df_CVE <- as.data.frame(table(et.pro.df$CVE))
et.pro.df_CVE  <- et.pro.df_CVE  [order(-et.pro.df_CVE$Freq),]

et.pro.msg_type <- et.pro.df$msg_type
et.pro.msg_type <- as.data.frame(table(et.pro.msg_type))
colnames(et.pro.msg_type)[1] <- "Var1"
et.pro.df_classtype <- as.data.frame(table(et.pro.df$classtype))



#ET rule list count
table(et.pro.df$rule_who)
et.pro_rule_who <- as.data.frame(table(et.pro.df$rule_who))
et.pro_rule_who_count <- as.data.frame(sum(et.pro_rule_who$Freq))
names(et.pro_rule_who_count)[1] <- "Suricata_Rule_Count"


#extract year from CVE
et.pro.df$cveyear3 <- str_extract(et.pro.df$CVE, perl("cve\\s*,\\s*(CVE-|CAN-|cve|CVE_)?([0-9]{4})-"))
et.pro.df$cveyear3 <- str_extract(et.pro.df$cveyear3, perl("[0-9]{4}"))
CVE_year_et.pro <- (as.data.frame(table(et.pro.df$cveyear3)))


#Bloomberg Signatures
Bloomberg_SIGS <- et.pro.msg_type[grep("BLOOM", et.pro.msg_type$Var1),]
Bloomberg_SIGS <- as.data.frame(Bloomberg_SIGS)
Bloomberg_SIGS  <- Bloomberg_SIGS  [order(-Bloomberg_SIGS$Freq),]



#Non Bloomberg Signatures
Non_Bloomberg_SIGS <- et.pro.msg_type[grep("^(?!BLOOM.*$)", et.pro.msg_type$Var1, perl = TRUE),]
Non_Bloomberg_SIGS  <- Non_Bloomberg_SIGS  [order(-Non_Bloomberg_SIGS$Freq),]


#Non Bloomberg Count of ET Pro / GPL / idef / Suricata / Volexity Rules
Non_Bloomberg_SIGS_gr8_200 <- subset(Non_Bloomberg_SIGS, Non_Bloomberg_SIGS$Freq >= 200)
Non_Bloomberg_SIGS_200_20 <- subset(Non_Bloomberg_SIGS, Non_Bloomberg_SIGS$Freq <= 200 & Non_Bloomberg_SIGS$Freq >= 20)
Non_Bloomberg_SIGS_less_20 <- subset(Non_Bloomberg_SIGS, Non_Bloomberg_SIGS$Freq <= 20)
Non_Bloomberg_SIGS_gr8_200 <- transform(Non_Bloomberg_SIGS_gr8_200, Var1 = reorder(Var1, Freq))

#plots
ggplot(data = Non_Bloomberg_SIGS_gr8_200, aes(x = Var1, y = Freq, fill=Var1)) + geom_bar(stat="identity") + geom_text(aes(label=Freq), size=5) + ggtitle("Non_Bloomberg_SIGS_gr8_200") +
  coord_flip()

Non_Bloomberg_SIGS_200_20 <- transform(Non_Bloomberg_SIGS_200_20, Var1 = reorder(Var1, Freq))
ggplot(data = Non_Bloomberg_SIGS_200_20, aes(x = Var1, y = Freq, fill=Var1)) + geom_bar(stat="identity") + geom_text(aes(label=Freq), size=5) + ggtitle("Non_Bloomberg_SIGS_200_20") +
  coord_flip()

Non_Bloomberg_SIGS_less_20 <- transform(Non_Bloomberg_SIGS_less_20, Var1 = reorder(Var1, Freq))
ggplot(data = Non_Bloomberg_SIGS_less_20, aes(x = Var1, y = Freq, fill=Var1)) + geom_bar(stat="identity") + geom_text(aes(label=Freq), size=5) + ggtitle("Non_Bloomberg_SIGS_less_20") +
  coord_flip()


#ET PRO count of class types
et.pro.df_classtype  <- et.pro.df_classtype  [order(-et.pro.df_classtype$Freq),]
et.pro.df_classtype <- transform(et.pro.df_classtype, Var1 = reorder(Var1, Freq))

#class plot
ggplot(data = et.pro.df_classtype, aes(x = Var1, y = Freq, fill=Var1)) + geom_bar(stat="identity") + geom_text(aes(label=Freq), size=5) + ggtitle("et.pro.df_classtype") +
  coord_flip()

# ET PRO plot of CVE signatures
ggplot(data = CVE_year_et.pro, aes(x = Var1, y = Freq)) + ggtitle("ET PRO CVE Distribution per year") + geom_bar(stat="identity") + geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.25)

###END OF Plots###


###Start of the BLOOM Categorization###
et.pro.modified.df <- et.pro.df  

#multiple rule messages 
rule_messages <- as.data.frame(table(et.pro.modified.df$msg))
rule_messages_gr8than1 <- subset(rule_messages, Freq>1)
rule_messages_gr8than1  <- rule_messages_gr8than1  [order(-rule_messages_gr8than1$Freq),]


rule_messages_morethan1 <- transform(rule_messages_gr8than1, Var1 = reorder(Var1, Freq))
ggplot(data = rule_messages_gr8than1, aes(x = Var1, y = Freq, fill=Var1)) + geom_bar(stat="identity") + geom_text(aes(label=Freq), size=5) + ggtitle("Rule message with multiple rules") +
  coord_flip()

#THE BLOOM CATEGORY
et.pro.modified.df$SEIM_Category <- ""


#phishing category
et.pro.modified.df$SEIM_Category <- ifelse(grepl('(phish)', et.pro.modified.df$msg, ignore.case = TRUE), "PHISHING", "")

#remove NA's 
et.pro.modified.df$SEIM_Category <- sapply(et.pro.modified.df$SEIM_Category, as.character)
et.pro.modified.df$SEIM_Category[is.na(et.pro.modified.df$SEIM_Category)] <- ""  

#number of bloom sigs categorized after Phishing
as.data.frame(table(et.pro.modified.df$SEIM_Category))

#Infrastructure/Service Attacks 
et.pro.modified.df$SEIM_Category <- ifelse(et.pro.modified.df$SEIM_Category!="PHISHING", 
                                            ifelse(et.pro.modified.df$classtype== "attempted-recon", "Infrastructure/Service Attacks",
                                                   ifelse(et.pro.modified.df$category== "etpro-web_server.rules", "Infrastructure/Service Attacks",       
                                                          ifelse(et.pro.modified.df$category== "etpro-attack_response.rules", "Infrastructure/Service Attacks",
                                                                 ifelse(et.pro.modified.df$category== "etpro-dos.rules", "Infrastructure/Service Attacks",
                                                                        ifelse(et.pro.modified.df$category== "etpro-pop3.rules", "Infrastructure/Service Attacks",       
                                                                               ifelse(et.pro.modified.df$classtype== "system-call", "Infrastructure/Service Attacks", "")))))),"PHISHING")

et.pro.modified.df$SEIM_Category[grepl("server",et.pro.modified.df$msg_type, ignore.case=TRUE)] <- "Infrastructure/Service Attacks"
et.pro.modified.df$SEIM_Category[grepl("denial\\-*",et.pro.modified.df$classtype, perl=TRUE, ignore.case = TRUE)] <- "Infrastructure/Service Attacks"       
et.pro.modified.df$SEIM_Category[grepl("successful-recon",et.pro.modified.df$classtype, perl=TRUE,ignore.case = TRUE)] <-  "Infrastructure/Service Attacks"
et.pro.modified.df$SEIM_Category[grepl("network-scan",et.pro.modified.df$classtype, perl=TRUE,ignore.case = TRUE)] <- "Infrastructure/Service Attacks"
et.pro.modified.df$SEIM_Category[grepl("scan",et.pro.modified.df$msg_type, perl=TRUE,ignore.case = TRUE)] <- "Infrastructure/Service Attacks"
et.pro.modified.df$SEIM_Category[grepl("sql",et.pro.modified.df$msg_type, perl=TRUE,ignore.case = TRUE)] <- "Infrastructure/Service Attacks"

#remove NA's 
et.pro.modified.df$SEIM_Category <- sapply(et.pro.modified.df$SEIM_Category, as.character)
et.pro.modified.df$SEIM_Category[is.na(et.pro.modified.df$SEIM_Category)] <- ""  


#number of bloom sigs categorized after Infrastructure/Service Attacks
as.data.frame(table(et.pro.modified.df$SEIM_Category))



#Spyware/Adware/PUPs
#spy <- c("spware","adware", "PUP")
#et.pro.modified.df$SEIM_Category <- ifelse((et.pro.modified.df$SEIM_Category!="PHISHING",
#                                           ifelse(et.pro.modified.df$SEIM_Category!="Infrastructure/Service Attacks",""),"PHISHING"))
et.pro.modified.df$SEIM_Category[grepl("spware|adware|PUP)", et.pro.modified.df$msg, ignore.case=TRUE)] <- "Spyware/Adware/PUPs"



#remove NA's 
et.pro.modified.df$SEIM_Category <- sapply(et.pro.modified.df$SEIM_Category, as.character)
et.pro.modified.df$SEIM_Category[is.na(et.pro.modified.df$SEIM_Category)] <- "" 

#number of bloom sigs categorized after Spyware/Adware/PUPs
as.data.frame(table(et.pro.modified.df$SEIM_Category))


#Browswer Exploits
et.pro.modified.df$SEIM_Category <- ifelse(et.pro.modified.df$SEIM_Category!="PHISHING", 
                                            ifelse(et.pro.modified.df$SEIM_Category!="Infrastructure/Service Attacks", 
                                                   ifelse(et.pro.modified.df$SEIM_Category!="Spyware/Adware/PUPs", 
                                                          ifelse(et.pro.modified.df$category=="etpro-tor.rules","Browsing Exploits",
                                                                 ifelse(et.pro.modified.df$category=="etpro-shellcode.rules","Browsing Exploits")),
                                                          "Spyware/Adware/PUPs"), "Infrastructure/Service Attacks"), "PHISHING")
et.pro.modified.df$SEIM_Category[grepl("browser|plugin|flash|silverlight|java|php|internet explorer", et.pro.modified.df$msg, ignore.case=TRUE)] <- "Browsing Exploits"     



#remove NA's 
et.pro.modified.df$SEIM_Category <- sapply(et.pro.modified.df$SEIM_Category, as.character)
et.pro.modified.df$SEIM_Category[is.na(et.pro.modified.df$SEIM_Category)] <- "" 

#number of bloom sigs categorized after Browser Exploits
as.data.frame(table(et.pro.modified.df$SEIM_Category))

#"Corp Policy Violations"
et.pro.modified.df$SEIM_Category <- ifelse(et.pro.modified.df$SEIM_Category!="PHISHING", 
                                            ifelse(et.pro.modified.df$SEIM_Category!="Infrastructure/Service Attacks", 
                                                   ifelse(et.pro.modified.df$SEIM_Category!="Spyware/Adware/PUPs", 
                                                          ifelse(et.pro.modified.df$SEIM_Category!="Browsing Exploits", 
                                                                 ifelse(et.pro.modified.df$category== "etpro-policy.rules", "Corp Policy Violations",
                                                                        ifelse(et.pro.modified.df$category== "etpro-inappropriate.rules", "Corp Policy Violations",
                                                                               ifelse(et.pro.modified.df$category== "etpro-games.rules", "Corp Policy Violations",
                                                                                      ifelse(et.pro.modified.df$category== "etpro-p2p.rules", "Corp Policy Violations",
                                                                                             ifelse(et.pro.modified.df$classtype== "policy-violation", "Corp Policy Violations",
                                                                                                    ifelse(et.pro.modified.df$category== "etpro-tor.rules", "Corp Policy Violations",       
                                                                                                           ifelse(et.pro.modified.df$category== "etpro-chat.rules", "Corp Policy Violations", ""))))))),
                                                                 "Browsing Exploits"), "Spyware/Adware/PUPs"), "Infrastructure/Service Attacks"), "PHISHING")  
et.pro.modified.df$SEIM_Category[grepl("policy", et.pro.modified.df$signatures, ignore.case=TRUE)] <- "Corp Policy Violations"

#number of bloom sigs categorized after Corp Policy Violations
as.data.frame(table(et.pro.modified.df$SEIM_Category))


#remove NA's 
et.pro.modified.df$SEIM_Category <- sapply(et.pro.modified.df$SEIM_Category, as.character)
et.pro.modified.df$SEIM_Category[is.na(et.pro.modified.df$SEIM_Category)] <- "" 


#Malware
et.pro.modified.df$SEIM_Category <- ifelse(et.pro.modified.df$SEIM_Category!="PHISHING", 
                                            ifelse(et.pro.modified.df$SEIM_Category!="Infrastructure/Service Attacks", 
                                                   ifelse(et.pro.modified.df$SEIM_Category!="Spyware/Adware/PUPs", 
                                                          ifelse(et.pro.modified.df$SEIM_Category!="Browsing Exploits", 
                                                                 ifelse(et.pro.modified.df$SEIM_Category!= "Corp Policy Violations",
                                                                        ifelse(et.pro.modified.df$classtype== "trojan-activity", "Malware",
                                                                               ifelse(et.pro.modified.df$category== "etpro-compromised.rules", "Malware",
                                                                                      ifelse(et.pro.modified.df$category== "etpro-owned.rules", "Malware",
                                                                                             ifelse(et.pro.modified.df$category== "etpro-malware.rules", "Malware")))),
                                                                        "Corp Policy Violations"),"Browsing Exploits"), "Spyware/Adware/PUPs"), "Infrastructure/Service Attacks"), "PHISHING") 

et.pro.modified.df$SEIM_Category[grepl("(worm|exploit|activex|ciarmy|trojan|botcc|dshield|owned)", et.pro.modified.df$msg_type, ignore.case=TRUE)] <- "Malware"


#remove NA's 
et.pro.modified.df$SEIM_Category <- sapply(et.pro.modified.df$SEIM_Category, as.character)
et.pro.modified.df$SEIM_Category[is.na(et.pro.modified.df$SEIM_Category)] <- "" 

#number of bloom sigs categorized after Malware
as.data.frame(table(et.pro.modified.df$SEIM_Category))

#DLP
et.pro.modified.df$SEIM_Category <- ifelse(et.pro.modified.df$SEIM_Category!="PHISHING", 
                                            ifelse(et.pro.modified.df$SEIM_Category!="Infrastructure/Service Attacks", 
                                                   ifelse(et.pro.modified.df$SEIM_Category!="Spyware/Adware/PUPs", 
                                                          ifelse(et.pro.modified.df$SEIM_Category!="Browsing Exploits", 
                                                                 ifelse(et.pro.modified.df$SEIM_Category!= "Corp Policy Violations",
                                                                        ifelse(et.pro.modified.df$SEIM_Category!= "Malware",
                                                                               ifelse(grepl("(ftp)", et.pro.modified.df$category,perl =TRUE),'DLP',''),  
                                                                               "Malware"),"Corp Policy Violations"),"Browsing Exploits"), "Spyware/Adware/PUPs"), "Infrastructure/Service Attacks"), "PHISHING") 
et.pro.modified.df$SEIM_Category[grepl("(ftp)", et.pro.modified.df$signatures, ignore.case=TRUE)] <- "DLP"

#remove NA's 
et.pro.modified.df$SEIM_Category <- sapply(et.pro.modified.df$SEIM_Category, as.character)
et.pro.modified.df$SEIM_Category[is.na(et.pro.modified.df$SEIM_Category)] <- "" 

#number of bloom sigs categorized after DLP
as.data.frame(table(et.pro.modified.df$SEIM_Category))





#Activity Reviews 
et.pro.modified.df$SEIM_Category <- ifelse(et.pro.modified.df$SEIM_Category!="PHISHING", 
                                            ifelse(et.pro.modified.df$SEIM_Category!="Infrastructure/Service Attacks", 
                                                   ifelse(et.pro.modified.df$SEIM_Category!="Spyware/Adware/PUPs", 
                                                          ifelse(et.pro.modified.df$SEIM_Category!="Browsing Exploits", 
                                                                 ifelse(et.pro.modified.df$SEIM_Category!= "Corp Policy Violations",
                                                                        ifelse(et.pro.modified.df$SEIM_Category!= "Malware",
                                                                               ifelse(et.pro.modified.df$SEIM_Category!= "DLP",
                                                                                      ifelse(et.pro.modified.df$SEIM_Category=="","Activity Reviews", ""),
                                                                                      "DLP"),"Malware"),"Corp Policy Violations"),"Browsing Exploits"), "Spyware/Adware/PUPs"), "Infrastructure/Service Attacks"), "PHISHING")


#remove NA's 
et.pro.modified.df$SEIM_Category <- sapply(et.pro.modified.df$SEIM_Category, as.character)
et.pro.modified.df$SEIM_Category[is.na(et.pro.modified.df$SEIM_Category)] <- "Activity Reviews" 

#no category yet
table(et.pro.modified.df$SEIM_Category, useNA="ifany")
Activity_Review <- et.pro.modified.df[grep("^$", et.pro.modified.df$SEIM_Category),]
#View(Activity_Review)
table(Activity_Review$category)

#number of SEIM_Cateogry sigs categorized after Activity Reviews
as.data.frame(table(et.pro.modified.df$SEIM_Category))



et.pro.modified_categories <- et.pro.modified.df$SEIM_Category
et.pro.modified_categories <- as.data.frame(table(et.pro.modified_categories))

#Sum of Rules per category
sum(et.pro.modified_categories$Freq)


et.pro.modified_categories <- et.pro.modified_categories [order(-et.pro.modified_categories$Freq),]
names(et.pro.modified_categories)
sum(et.pro.modified_categories$Freq)


#ggplot ET Pro Categories 
et.pro.modified_categories <- transform(et.pro.modified_categories, et.pro.modified_categories = reorder(et.pro.modified_categories, Freq))
ggplot(data = et.pro.modified_categories , aes(x = et.pro.modified_categories, y = Freq, fill=et.pro.modified_categories)) +  geom_bar(stat="identity") + geom_text(aes(label=Freq), size=5) + ggtitle("ET Pro New Categories") +
  coord_flip()



#remove duplicates
et.pro.modified.df_uq <- et.pro.modified.df[!duplicated(et.pro.modified.df),]
View(et.pro.modified.df_uq)


#wordcloud
#http://www.r-bloggers.com/joy-to-the-world-and-also-anticipation-disgust-surprise/
library(wordcloud)
library(tm)

msg <- et.pro.modified.df_uq$msg
#msg <- data.matrix(msg, rownames.force = NA)
msg.corpus <- Corpus(VectorSource(as.character(msg))) 
msg.corpus <- tm_map(msg.corpus, removePunctuation)
msg.corpus <- tm_map(msg.corpus, removeWords, c('charact', 'mday','yday','datetimestamp', 'head','wday','hour', stopwords('english')))
msg.corpus <- tm_map(msg.corpus, removeNumbers)
msg.corpus <- tm_map(msg.corpus, stemDocument)
wordcloud(msg.corpus, max.words = 400, random.order = FALSE)

#date
write.csv(et.pro.modified.df, file = paste(Sys.Date(), "csv", sep = "."))

