#Updated 02/23/2016

library(stringr)
library(ggplot2)

setwd(dir = "~/R_code/shiny dashboard/")
## Rconfigure_default.R
library(utils)
## Using Internet Explorer proxy settings is
## often helpful in an IT controlled environment
#.libPaths(c("C:\\Users\\blodge9\\Downloads", .libPaths()))


#ORIGINAL RULES FILE 
#missing.rules<- readLines("C://Users//blodge9//Downloads//perl//suricata_rules/combined_modified.txt")

#translate a .tgz to .txt
untar(file.choose(),list = TRUE)
#untar('2015-10-14-suricata-rules.tgz')
#setwd(dir = "rules/")

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
Bloomberg_SIGS_all <- et.pro.msg_type[grep("BLOOM", et.pro.msg_type$Var1),]
Bloomberg_SIGS <- as.data.frame(Bloomberg_SIGS_all)
Bloomberg_SIGS  <- Bloomberg_SIGS  [order(-Bloomberg_SIGS$Freq),]




#Non Bloomberg Signatures
Non_Bloomberg_SIGS <- et.pro.msg_type[grep("^(?!BLOOM.*$)", et.pro.msg_type$Var1, perl = TRUE),]
Non_Bloomberg_SIGS  <- Non_Bloomberg_SIGS  [order(-Non_Bloomberg_SIGS$Freq),]



#Non Bloomberg Count of ET Pro / GPL / idef / Suricata / Volexity Rules
Non_Bloomberg_SIGS_gr8_200 <- subset(Non_Bloomberg_SIGS, Non_Bloomberg_SIGS$Freq >= 200)
Non_Bloomberg_SIGS_200_20 <- subset(Non_Bloomberg_SIGS, Non_Bloomberg_SIGS$Freq <= 200 & Non_Bloomberg_SIGS$Freq >= 20)
Non_Bloomberg_SIGS_less_20 <- subset(Non_Bloomberg_SIGS, Non_Bloomberg_SIGS$Freq <= 20)
Non_Bloomberg_SIGS_gr8_200 <- transform(Non_Bloomberg_SIGS_gr8_200, Var1 = reorder(Var1, Freq))




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
et.pro.modified.df$BLOOM_Category <- ""

#Recon & Scanning category
et.pro.modified.df$BLOOM_Category <- ifelse(grepl('(attempted-dos|attempted-recon|network-scan|rpc-portmapsuccessful-dos|successful-recon)', et.pro.modified.df$classtype), "RECON_SCAN", "")


#remove NA's 
et.pro.modified.df$BLOOM_Category <- sapply(et.pro.modified.df$BLOOM_Category, as.character)
et.pro.modified.df$BLOOM_Category[is.na(et.pro.modified.df$BLOOM_Category)] <- ""  



#phishing category
et.pro.modified.df$BLOOM_Category <- ifelse(et.pro.modified.df$BLOOM_Category!="RECON_SCAN",
                                            ifelse(grepl('(phish)', et.pro.modified.df$msg, ignore.case = TRUE), "PHISHING", ""), "RECON_SCAN")

#remove NA's 
et.pro.modified.df$BLOOM_Category <- sapply(et.pro.modified.df$BLOOM_Category, as.character)
et.pro.modified.df$BLOOM_Category[is.na(et.pro.modified.df$BLOOM_Category)] <- ""  


#number of bloom sigs categorized after Phishing
as.data.frame(table(et.pro.modified.df$BLOOM_Category))



#Infrastructure/Service Attacks 
et.pro.modified.df$BLOOM_Category <- ifelse(et.pro.modified.df$BLOOM_Category!="RECON_SCAN",
                                            ifelse(et.pro.modified.df$BLOOM_Category!="PHISHING",  
                                                   ifelse(et.pro.modified.df$category== "etpro-web_server.rules", "Infrastructure/Service Attacks",       
                                                          ifelse(et.pro.modified.df$category== "etpro-attack_response.rules", "Infrastructure/Service Attacks",
                                                                 ifelse(et.pro.modified.df$category== "etpro-dos.rules", "Infrastructure/Service Attacks",
                                                                        ifelse(et.pro.modified.df$category== "etpro-pop3.rules", "Infrastructure/Service Attacks",       
                                                                               ifelse(et.pro.modified.df$classtype== "system-call", "Infrastructure/Service Attacks", ""))))), "PHISHING"), "RECON_SCAN")

et.pro.modified.df$BLOOM_Category[grepl("server",et.pro.modified.df$msg_type, ignore.case=TRUE)] <- "Infrastructure/Service Attacks"
et.pro.modified.df$BLOOM_Category[grepl("sql",et.pro.modified.df$msg_type, perl=TRUE,ignore.case = TRUE)] <- "Infrastructure/Service Attacks"

#remove NA's 
et.pro.modified.df$BLOOM_Category <- sapply(et.pro.modified.df$BLOOM_Category, as.character)
et.pro.modified.df$BLOOM_Category[is.na(et.pro.modified.df$BLOOM_Category)] <- ""  


#number of bloom sigs categorized after Infrastructure/Service Attacks
as.data.frame(table(et.pro.modified.df$BLOOM_Category))



#Spyware/Adware/PUPs
#spy <- c("spware","adware", "PUP")
#et.pro.modified.df$BLOOM_Category <- ifelse((et.pro.modified.df$BLOOM_Category!="PHISHING",
#                                           ifelse(et.pro.modified.df$BLOOM_Category!="Infrastructure/Service Attacks",""),"PHISHING"))
et.pro.modified.df$BLOOM_Category[grepl("spware|adware|PUP)", et.pro.modified.df$msg, ignore.case=TRUE)] <- "Spyware/Adware/PUPs"



#remove NA's 
et.pro.modified.df$BLOOM_Category <- sapply(et.pro.modified.df$BLOOM_Category, as.character)
et.pro.modified.df$BLOOM_Category[is.na(et.pro.modified.df$BLOOM_Category)] <- "" 

#number of bloom sigs categorized after Spyware/Adware/PUPs
as.data.frame(table(et.pro.modified.df$BLOOM_Category))


#Browswer Exploits
et.pro.modified.df$BLOOM_Category <- ifelse(et.pro.modified.df$BLOOM_Category!="RECON_SCAN",
                                            ifelse(et.pro.modified.df$BLOOM_Category!="PHISHING", 
                                                   ifelse(et.pro.modified.df$BLOOM_Category!="Infrastructure/Service Attacks", 
                                                          ifelse(et.pro.modified.df$BLOOM_Category!="Spyware/Adware/PUPs", 
                                                                 ifelse(et.pro.modified.df$category=="etpro-tor.rules","Browsing Exploits",
                                                                        ifelse(et.pro.modified.df$category=="etpro-shellcode.rules","Browsing Exploits", "")),
                                                                 "Spyware/Adware/PUPs"), "Infrastructure/Service Attacks"), "PHISHING"), "RECON_SCAN")
et.pro.modified.df$BLOOM_Category[grepl("browswer|plugin|flash|silverlight|java|php|internet explorer", et.pro.modified.df$msg, ignore.case=TRUE)] <- "Browsing Exploits"     



#remove NA's 
et.pro.modified.df$BLOOM_Category <- sapply(et.pro.modified.df$BLOOM_Category, as.character)
et.pro.modified.df$BLOOM_Category[is.na(et.pro.modified.df$BLOOM_Category)] <- "" 

#number of bloom sigs categorized after Browser Exploits
as.data.frame(table(et.pro.modified.df$BLOOM_Category))

#"Corp Policy Violations"
et.pro.modified.df$BLOOM_Category <- ifelse(et.pro.modified.df$BLOOM_Category!="RECON_SCAN",
                                            ifelse(et.pro.modified.df$BLOOM_Category!="PHISHING", 
                                                   ifelse(et.pro.modified.df$BLOOM_Category!="Infrastructure/Service Attacks", 
                                                          ifelse(et.pro.modified.df$BLOOM_Category!="Spyware/Adware/PUPs", 
                                                                 ifelse(et.pro.modified.df$BLOOM_Category!="Browsing Exploits", 
                                                                        ifelse(et.pro.modified.df$category== "etpro-policy.rules", "Corp Policy Violations",
                                                                               ifelse(et.pro.modified.df$category== "etpro-inappropriate.rules", "Corp Policy Violations",
                                                                                      ifelse(et.pro.modified.df$category== "etpro-games.rules", "Corp Policy Violations",
                                                                                             ifelse(et.pro.modified.df$category== "etpro-p2p.rules", "Corp Policy Violations",
                                                                                                    ifelse(et.pro.modified.df$classtype== "policy-violation", "Corp Policy Violations",
                                                                                                           ifelse(et.pro.modified.df$category== "etpro-tor.rules", "Corp Policy Violations",       
                                                                                                                  ifelse(et.pro.modified.df$category== "etpro-chat.rules", "Corp Policy Violations", ""))))))),
                                                                        "Browsing Exploits"), "Spyware/Adware/PUPs"), "Infrastructure/Service Attacks"), "PHISHING"), "RECON_SCAN")
et.pro.modified.df$BLOOM_Category[grepl("policy", et.pro.modified.df$signatures, ignore.case=TRUE)] <- "Corp Policy Violations"

#number of bloom sigs categorized after Corp Policy Violations
as.data.frame(table(et.pro.modified.df$BLOOM_Category))


#remove NA's 
et.pro.modified.df$BLOOM_Category <- sapply(et.pro.modified.df$BLOOM_Category, as.character)
et.pro.modified.df$BLOOM_Category[is.na(et.pro.modified.df$BLOOM_Category)] <- "" 


#Malware
et.pro.modified.df$BLOOM_Category <- ifelse(et.pro.modified.df$BLOOM_Category!="RECON_SCAN",
                                            ifelse(et.pro.modified.df$BLOOM_Category!="PHISHING", 
                                                   ifelse(et.pro.modified.df$BLOOM_Category!="Infrastructure/Service Attacks", 
                                                          ifelse(et.pro.modified.df$BLOOM_Category!="Spyware/Adware/PUPs", 
                                                                 ifelse(et.pro.modified.df$BLOOM_Category!="Browsing Exploits", 
                                                                        ifelse(et.pro.modified.df$BLOOM_Category!= "Corp Policy Violations",
                                                                               ifelse(et.pro.modified.df$classtype== "trojan-activity", "Malware",
                                                                                      ifelse(et.pro.modified.df$category== "etpro-compromised.rules", "Malware",
                                                                                             ifelse(et.pro.modified.df$category== "etpro-owned.rules", "Malware",
                                                                                                    ifelse(et.pro.modified.df$category== "etpro-malware.rules", "Malware")))),
                                                                               "Corp Policy Violations"),"Browsing Exploits"), "Spyware/Adware/PUPs"), "Infrastructure/Service Attacks"), "PHISHING"), "RECON_SCAN") 

et.pro.modified.df$BLOOM_Category[grepl("(worm|exploit|activex|ciarmy|trojan|botcc|dshield|owned)", et.pro.modified.df$msg_type, ignore.case=TRUE)] <- "Malware"


#remove NA's 
et.pro.modified.df$BLOOM_Category <- sapply(et.pro.modified.df$BLOOM_Category, as.character)
et.pro.modified.df$BLOOM_Category[is.na(et.pro.modified.df$BLOOM_Category)] <- "" 

#number of bloom sigs categorized after Malware
as.data.frame(table(et.pro.modified.df$BLOOM_Category))

#DLP
et.pro.modified.df$BLOOM_Category <- ifelse(et.pro.modified.df$BLOOM_Category!="RECON_SCAN",
                                            ifelse(et.pro.modified.df$BLOOM_Category!="PHISHING", 
                                                   ifelse(et.pro.modified.df$BLOOM_Category!="Infrastructure/Service Attacks", 
                                                          ifelse(et.pro.modified.df$BLOOM_Category!="Spyware/Adware/PUPs", 
                                                                 ifelse(et.pro.modified.df$BLOOM_Category!="Browsing Exploits", 
                                                                        ifelse(et.pro.modified.df$BLOOM_Category!= "Corp Policy Violations",
                                                                               ifelse(et.pro.modified.df$BLOOM_Category!= "Malware",
                                                                                      ifelse(grepl("(ftp)", et.pro.modified.df$category,perl =TRUE),'DLP',''),  
                                                                                      "Malware"),"Corp Policy Violations"),"Browsing Exploits"), "Spyware/Adware/PUPs"), "Infrastructure/Service Attacks"), "PHISHING"), "RECON_SCAN")
et.pro.modified.df$BLOOM_Category[grepl("(ftp)", et.pro.modified.df$signatures, ignore.case=TRUE)] <- "DLP"

#remove NA's 
et.pro.modified.df$BLOOM_Category <- sapply(et.pro.modified.df$BLOOM_Category, as.character)
et.pro.modified.df$BLOOM_Category[is.na(et.pro.modified.df$BLOOM_Category)] <- "" 

#number of bloom sigs categorized after DLP
as.data.frame(table(et.pro.modified.df$BLOOM_Category))





#Activity Reviews 
et.pro.modified.df$BLOOM_Category <- ifelse(et.pro.modified.df$BLOOM_Category!="RECON_SCAN",
                                            ifelse(et.pro.modified.df$BLOOM_Category!="PHISHING", 
                                                   ifelse(et.pro.modified.df$BLOOM_Category!="Infrastructure/Service Attacks", 
                                                          ifelse(et.pro.modified.df$BLOOM_Category!="Spyware/Adware/PUPs", 
                                                                 ifelse(et.pro.modified.df$BLOOM_Category!="Browsing Exploits", 
                                                                        ifelse(et.pro.modified.df$BLOOM_Category!= "Corp Policy Violations",
                                                                               ifelse(et.pro.modified.df$BLOOM_Category!= "Malware",
                                                                                      ifelse(et.pro.modified.df$BLOOM_Category!= "DLP",
                                                                                             ifelse(et.pro.modified.df$BLOOM_Category=="","Activity Reviews", ""),
                                                                                             "DLP"),"Malware"),"Corp Policy Violations"),"Browsing Exploits"), "Spyware/Adware/PUPs"), "Infrastructure/Service Attacks"), "PHISHING"), "RECON_SCAN")


#remove NA's 
et.pro.modified.df$BLOOM_Category <- sapply(et.pro.modified.df$BLOOM_Category, as.character)
et.pro.modified.df$BLOOM_Category[is.na(et.pro.modified.df$BLOOM_Category)] <- "Activity Reviews" 

#no category yet
table(et.pro.modified.df$BLOOM_Category, useNA="ifany")
Activity_Review <- et.pro.modified.df[grep("^$", et.pro.modified.df$BLOOM_Category),]
#View(Activity_Review)
table(Activity_Review$category)

#number of bloom sigs categorized after Activity Reviews
as.data.frame(table(et.pro.modified.df$BLOOM_Category))


et.pro.modified_categories <- et.pro.modified.df$BLOOM_Category
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
nrow(table(et.pro.modified.df_uq$msg_type))

table(et.pro.modified.df_uq$msg_type)

#grab host information from ADDM
ADDM_host <- read.csv("C://Users//blodge9//Downloads//host_addm", sep = ",")
#counts <- table(ADDM_host$Discovered.OS)
ADDM_host_count <- as.data.frame(table(ADDM_host$Discovered.OS))
ADDM_host_count <- ADDM_host_count [order(-ADDM_host_count$Freq),]


#splunk 90 day results
splunk <- read.csv("90daySplunkSuricata.csv", sep=",")
splunk <- merge(splunk, et.pro.modified.df_uq, by="sid")

#splunk msg type count
splunk_msg_type <- as.data.frame(table(splunk$msg_type))
nrow(splunk_msg_type)

#Splunk Bloom Category
splunk_BLOOM_Category <- as.data.frame(table(splunk$BLOOM_Category))

# Diff between fired msg_type's and not fired msg_type's 
library(plyr)

splunk_msg_type <- rename(splunk_msg_type , c("Var1"="splunk_msg"))
Non_Bloomberg_SIGS <- rename(Non_Bloomberg_SIGS , c("Var1"="Non_Bloomberg_msg"))

no_match <- splunk_msg_type$splunk_msg[ !Non_Bloomberg_SIGS$Non_Bloomberg_msg]
no_match <- subset(Non_Bloomberg_SIGS, !(Non_Bloomberg_msg %in% splunk_msg_type$splunk_msg))


#Splunk 90 day msg_type plot
ggplot(data = splunk_msg_type , aes(x = reorder(splunk_msg , +Freq), y = Freq, fill= +Freq)) +   
  geom_bar(stat="identity") +  geom_text(aes(label=Freq, colour = factor(splunk_msg)),  colour = "red", fontface = "bold", size=4.5) + 
  ggtitle("SPLUNK 90 Day Msg Type") + labs(x="Rule MSG Type",y="90 Day Alert Count") +   
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20, hjust=.5)) +  
  theme(axis.text = element_text(size= 10)) +  
  coord_flip() 

names(splunk_msg_type)

#Splunk 90 day msg_type plot
ggplot(data = splunk_BLOOM_Category , aes(x = reorder(Var1 , +Freq), y = Freq, fill= +Freq)) +   
  geom_bar(stat="identity") +  geom_text(aes(label=Freq, colour = factor(Var1)),  colour = "red", fontface = "bold", size=4.5) + 
  ggtitle("SPLUNK 90 Day BLoomberg Category") + labs(x="Rule MSG Type",y="90 Day Alert Count") +   
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20, hjust=.5)) +  
  theme(axis.text = element_text(size= 10)) +  
  coord_flip() 



#setwd("~/R_code/shiny dashboard/")
#top 20 splunk 90 day 
#splunk20 <- read.csv("NIDS sigs/splunk20.csv", sep=",")
#splunk20 <- merge(splunk20, et.pro.modified.df_uq, by="sid")

#nexpose report
nexpose <- read.csv("report (1).csv")

#nexpose Mcafee antivirus not running
mcafee_disabled <- nexpose[grep("^McAfee", nexpose$Vulnerability.Title), ]
non_dups_mcafee_disabled <- mcafee_disabled[!duplicated(mcafee_disabled[,2]),]
mcafee_disabled <- non_dups_mcafee_disabled



#strsplit(mcafee_disabled$Site.Name, ",")

library(stringr)
library("plyr")


Locations <- str_split_fixed(mcafee_disabled$Site.Name, ",", 2)
Locations <- as.data.frame(Locations)
Locations_table <- table(Locations$V1)
Locations_table <- as.data.frame(Locations_table)

Locations_table$percentage <- (Locations_table$Freq/sum(Locations_table$Freq))*100
Locations_table <- Locations_table [order(-Locations_table$Freq),]
colnames(Locations_table) <- c("Location","Count","Percentage")

write.csv(mcafee_disabled, file = paste("mcafee_disabled_machines", "csv", sep = "."))

#mcafee disabled missing 
#View(mcafee_disabled$Asset.IP.Address)


#IP to username (removed -a)
ip_to_username <- read.csv("~/R_code/shiny dashboard/McafeeDisabled_03012016.csv", sep=",")

#matches of mcafee ips to username
mcafee_usernames_ip <- read.csv("mcafee_disabled.csv")

#merge of mcafee disabled and mcafee disabled username + IP
mcafee_merge_names <- merge(mcafee_disabled,mcafee_usernames_ip,by.x=c("Asset.IP.Address"),by.y=c("IP"), all.x = TRUE, all.y = TRUE, incomparables = NULL)

#View(mcafee_merge_names)

#Risky list 2
risky2 <- read.csv("UUUID_to_CORP.csv", sep=",")
#View(risky2)


###TRIM###
# returns string w/o leading whitespace
trim.leading <- function (x)  sub("^\\s+", "", x)

# returns string w/o trailing whitespace
trim.trailing <- function (x) sub("\\s+$", "", x)

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)


#risky2 uuid to samname
risky2_uuid_samname <- read.csv("risky_uuid_samname.csv", sep=",")
risky2_uuid_samname$samname <- trim(risky2_uuid_samname$samname)

mcafee_merge_names$samname <- trim(mcafee_merge_names$samname)


#merge risky2 to mcafee disabled
risky2_merge_with_mcafee <- merge(risky2_uuid_samname,mcafee_merge_names,by=c("samname"))

write.csv(mcafee_merge_names, file = paste("mcafee_merge_names", "csv", sep = "."))
write.csv(risky2_merge_with_mcafee, file = paste("mcafee_merge_names_risky", "csv", sep = "."))

risky2_merge <- merge(risky2,risky2_uuid_samname,by=c("UUID"), all.x = TRUE, all.y = TRUE, incomparables = NULL)


#merge
user_details <- merge(ip_to_username,username_to_UUID,by=c("username"), all.x = TRUE, all.y = TRUE, incomparables = NULL)


#Username to UUID (removed -a)
username_to_UUID <- read.csv("~/R_code/shiny dashboard/UUID_to_Username.csv", sep=",")

user_details <- merge(ip_to_username,username_to_UUID,by=c("username"), all.x = TRUE, all.y = TRUE, incomparables = NULL)


#highrisk
high_risk <- read.csv("~/R_code/shiny dashboard/bfm31D1.csv", sep=",")


#mcafee disabled report with user name
mcafee_disabled_2 <- merge(mcafee_disabled,user_details,by=c("Asset.IP.Address"), all.x = TRUE, all.y = TRUE, incomparables = NULL)

#View(mcafee_disabled_2)

#high risk to uuid
matches_av_disabled <- merge(mcafee_disabled_2,high_risk,by=c("UUID"), all = FALSE)
#View(matches_av_disabled)
write.csv(matches_av_disabled, file = paste("McAfee_disabled_highrisk", "csv", sep = "."))


# windows firewall disabled
fw_disabled <- nexpose[grep("^Windows Firewall is Disabled", nexpose$Vulnerability.Title), ]
non_dups_fw_disabled <- fw_disabled[!duplicated(fw_disabled[,2]),]
non_dups_fw_disabled

#Windows autologin enabled
autologin_disabled <- nexpose[grep("^Windows autologin enabled", nexpose$Vulnerability.Title), ]
non_dups_autologin_disabled <- autologin_disabled[!duplicated(autologin_disabled[,2]),]
nrow(non_dups_autologin_disabled)


#Vuln Count 
nexpose_vuln <- as.data.frame(table(nexpose$Vulnerability.Title))
nexpose_vuln <- nexpose_vuln [order(-nexpose_vuln$Freq),]

#Asset Family
nexpose_OS <- as.data.frame(table(nexpose$Asset.OS.Family))
nexpose_OS <- nexpose_OS [order(-nexpose_OS$Freq),]


#Asset OS Name
nexpose_OS_name <- as.data.frame(table(nexpose$Asset.OS.Name))
nexpose_OS_name <- nexpose_OS_name [order(-nexpose_OS_name$Freq),]
nexpose_OS_name

#location count
nexpose_loc <- as.data.frame(table(nexpose$Site.Name))
nexpose_loc <- nexpose_loc [order(-nexpose_loc$Freq),]
nexpose_loc

#plots
require(ggplot2)
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

#bubble chart
ggplot(splunk, aes(x = classtype , y = msg_type, label = sid )) +
  geom_point(aes(size = request_count, colour = percent_of_hits  , alpha=.80)) + 
  geom_text(hjust = 1, size = 2) +
  scale_size(range = c(1,15)) +
  theme_bw()


ggplot(data = et.pro.modified_categories , aes(x = et.pro.modified_categories, y = Freq, fill=et.pro.modified_categories)) +  geom_bar(stat="identity") + geom_text(aes(label=Freq), size=5) + ggtitle("ET Pro New Categories") +
  coord_flip()


ggplot(data = splunk , aes(x = reorder(classtype , +request_count), y = request_count, fill= +request_count)) +  geom_bar(position = "dodge", stat="identity") + geom_text(aes(label=request_count), size=5) + ggtitle("SPLUNK") +
  coord_flip()


#sum of class types
sum(et.pro.df_classtype$Freq)

#chart of OS hosts
ggplot(data = ADDM_host_count , aes(x = reorder(Var1 , +Freq), y = Freq, fill= -Freq)) +  geom_bar(position = "dodge", stat="identity") + geom_text(aes(label=Freq), size=5) + ggtitle("ADDM Host Types") +
  coord_flip()

#grab software instances
ADDM_software <- read.csv("C://Users//blodge9//Downloads//software_addm", sep = ",")

#grab software packages
ADDM_software_packages <- read.csv("C://Users//blodge9//Downloads//software_packages", sep = ",")


ADDM_software_count <- as.data.frame(table(ADDM_software$Type))
ADDM_software_count <- ADDM_software_count [order(-ADDM_host_count$Freq),]

#Rule Types
nrow(Non_Bloomberg_SIGS)
require(ggplot2)

ggplot(data = Non_Bloomberg_SIGS , aes(x = reorder(Var1 , +Freq), y = Freq, fill= -Freq)) +  geom_bar(position = "dodge", stat="identity") + geom_text(aes(label=Freq), size=5) + ggtitle("Rule Types") +
  coord_flip()



ggplot(diamonds, aes(carat)) +
  geom_histogram()

#date
write.csv(et.pro.modified.df, file = paste(Sys.Date(), "csv", sep = "."))


