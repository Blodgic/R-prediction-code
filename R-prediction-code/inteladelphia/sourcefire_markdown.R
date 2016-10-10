--------------------
  title: NIDS Breakdown
Brennan Lodoge
June 1, 2015
--------------------
  ```{r eval=TRUE, echo=FALSE, warning=FALSE}
#NIDS SIGS consolidation
library(stringr)
library(plyr)
library(ggplot2)
library(labeling)
```

```{r eval=TRUE, echo=FALSE}
#SourceFire
setwd("c:/Users/blodge9/")
SourceFire <- read.csv("Desktop/NIDS sigs/SourcefireIDSrules.csv")
SourceFire <- as.data.frame(SourceFire)
```

```{r eval=TRUE, echo=FALSE}
#show count of rule types
SourceFire_msg_type <- str_extract(SourceFire$msg, perl('(?=[A-Z])\\w+(\\s+|\\-)\\w+'))
SourceFire$msg_type <- SourceFire_msg_type
SourceFire_msg_type.df <- as.data.frame(table(SourceFire_msg_type))
SourceFire_msg_type.df <- SourceFire_msg_type.df [order(-SourceFire_msg_type.df$Freq),]
SourceFire_msg_type.df_deleted <- SourceFire_msg_type.df[grep("DELETED*", SourceFire_msg_type.df$SourceFire_msg_type), ]
SourceFire_msg_type.df_not_deleted <- SourceFire_msg_type.df[grep("^((?!DELETED).)*$", SourceFire_msg_type.df$SourceFire_msg_type,perl = TRUE), ]
SourceFire$classtype <- str_extract(SourceFire$rule_text,'classtype\\:\\w+\\-\\w+')
SourceFire$classtype <- gsub('classtype\\:',"", SourceFire$classtype)
SourceFire$CVE <- str_extract(SourceFire$rule_text, 'reference\\:cve.+?\\;')
SourceFire$CVE<- gsub('reference\\:',"", SourceFire$CVE)
SourceFire$CVE<- gsub('\\;',"", SourceFire$CVE)
SourceFire_classtype <- as.data.frame(table(SourceFire$classtype, useNA = "ifany"))
SourceFire_classtype <- SourceFire_classtype [order(-SourceFire_classtype$Freq),]
SourceFire_CVEs <- as.data.frame(table(SourceFire$CVE))
SourceFire_CVEs <- SourceFire_CVEs [order(-SourceFire_CVEs$Freq),]
SourceFire$CVEyear <- str_extract(SourceFire$CVE, perl("(?<=\\,)\\d+(?=\\-)"))
CVE_year <- as.data.frame(table(SourceFire$CVEyear))
Class_type <- as.data.frame(table(SourceFire$classtype))
Class_type <- Class_type [order(-Class_type$Freq),]
SourceFire_category <- as.data.frame(table(SourceFire$category))
SourceFire_category <- as.data.frame(table(SourceFire$category))
SourceFire_category <- SourceFire_category [order(-SourceFire_category$Freq),]
SourceFire_category_gr8_300 <- subset(SourceFire_category, Freq >= 300)
SourceFire_category_300_100 <- subset(SourceFire_category, Freq < 300 & Freq >= 100)
SourceFire_category_less_100 <- subset(SourceFire_category, Freq < 100)
SourceFire_non_deleted <- SourceFire[ which(SourceFire$category!='deleted.rules'),]
SourceFire_proto.df <- as.data.frame(table(SourceFire_non_deleted$proto))
SourceFire_proto.df <- SourceFire_proto.df [order(-SourceFire_proto.df$Freq),]


SourceFire_DeletedRules <- as.data.frame(sum(SourceFire_msg_type.df_deleted$Freq))
names(SourceFire_DeletedRules)[1] <- "SourceFire_Deleted_Rules_Count"
SourceFire_ActiveRules <- as.data.frame(sum(SourceFire_msg_type.df_not_deleted$Freq))
names(SourceFire_ActiveRules)[1] <- "SourceFire_Active_Rule_Count"
```
---------
  SourceFire Active Rules Count
---------
  ```{r eval=TRUE, echo=FALSE}
SourceFire_ActiveRules
```

----------
  SourceFire Rule protocol breakdown
----------
  ```{r eval=TRUE, echo=FALSE}
SourceFire_proto.df
```

----------
  SourceFire Rule breakdown message count
----------
  ```{r eval=TRUE, echo=FALSE}
SourceFire_msg_type.df_not_deleted 
```

-------
  BarChart of SourceFire Count of Signatures per ClassType
-------
  ```{r eval=TRUE, echo=FALSE, warning=FALSE}


Class_type <- transform(Class_type, Var1 = reorder(Var1, Freq))
ggplot(data = Class_type, aes(x = Var1, y = Freq, fill=Var1)) +  geom_bar(stat="identity") + geom_text(aes(label=Freq), size=5) +
  coord_flip()

```

-------
  BarChart of SourceFire  Count of Rule Category
-------
  ```{r eval=TRUE, echo=FALSE}
SourceFire_category_gr8_300 <- transform(SourceFire_category_gr8_300, Var1 = reorder(Var1, Freq))
ggplot(data = SourceFire_category_gr8_300, aes(x = Var1, y = Freq, fill=Var1)) + geom_bar(stat="identity") + geom_text(aes(label=Freq), size=5) +
  coord_flip()

SourceFire_category_300_100 <- transform(SourceFire_category_300_100, Var1 = reorder(Var1, Freq))
ggplot(data = SourceFire_category_300_100, aes(x = Var1, y = Freq, fill=Var1)) + geom_bar(stat="identity") + geom_text(aes(label=Freq), size=5) +
  coord_flip()

SourceFire_category_less_100 <- transform(SourceFire_category_less_100, Var1 = reorder(Var1, Freq))
ggplot(data = SourceFire_category_less_100, aes(x = Var1, y = Freq, fill=Var1)) + geom_bar(stat="identity") + geom_text(aes(label=Freq), size=5) +
  coord_flip()
```

-------
  Distribution of SourceFire Rules related to CVE year
-------
  ```{r eval=TRUE, echo=FALSE, warning=FALSE}

ggplot(data = CVE_year, aes(x = Var1, y = Freq, ymax=max(Freq)*1.05)) + geom_bar(stat="identity") + geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.25)
```

```{r eval=TRUE, echo=FALSE}
#pull in one rules file
#ep rules and cat the .rules files 
#for %f in (etpro*.rules) do type "%f" >> combined.txt
# combined all ET PRO rules files into combined.txt
library(stringr)
rulefile.etpro <- readLines("C://Users//blodge9//Desktop//eprules/combined.txt")

signatures <- grep("^alert.+\\;\\s?\\)$", rulefile.etpro, value=TRUE)

et.pro.df <- as.data.frame(signatures)

et.pro.df$msg <- str_extract(et.pro.df$signatures,'msg\\:\\s?.+?\\;')
et.pro.df$sid <- str_extract(et.pro.df$signatures, 'sid\\:\\d+') 
et.pro.df$classtype <- str_extract(et.pro.df$signatures,'classtype\\:\\w+\\-\\w+')

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
et.pro.df_proto <- str_extract(et.pro.df$signatures, '^(?:\\S+\\s){1}(\\S+)')
et.pro.df_proto <- gsub('alert\\s+',"", et.pro.df_proto)
et.pro.df_proto.df <- as.data.frame(table(et.pro.df_proto))
et.pro.df_proto.df <- et.pro.df_proto.df [order(-et.pro.df_proto.df$Freq),]
et.pro.df$proto <- et.pro.df_proto
```

-------
  SourceFire New Category Breakdown
-------
  ```{r eval=TRUE, echo=FALSE}
#summarizing the NIDS sigs by Categories


SourceFire_non_deleted$BLOOM_Category <- ""
SourceFire_non_deleted$BLOOM_Category[is.na(SourceFire_non_deleted$BLOOM_Category)] <- " "

#phishing category
SourceFire_non_deleted$BLOOM_Category <- ifelse(SourceFire_non_deleted$category == "policy-spam.rules", 
                                                "PHISHING", ifelse(grepl('(phish)', SourceFire_non_deleted$msg, ignore.case = TRUE), "PHISHING", ""))
#Infrastructure/Service Attacks 
SourceFire_non_deleted$BLOOM_Category <- ifelse(SourceFire_non_deleted$BLOOM_Category!="PHISHING", 
                                                ifelse(SourceFire_non_deleted$classtype== "attempted-recon", "Infrastructure/Service Attacks",
                                                       ifelse(SourceFire_non_deleted$category== "server-other.rules", "Infrastructure/Service Attacks",       
                                                              ifelse(grepl("denial\\-*",SourceFire_non_deleted$classtype, perl=TRUE), "Infrastructure/Service Attacks",       
                                                                     ifelse(grepl("(server)",SourceFire_non_deleted$category, perl=TRUE),  "Infrastructure/Service Attacks",
                                                                            ifelse(grepl("successful-recon*",SourceFire_non_deleted$classtype, perl=TRUE),  "Infrastructure/Service Attacks",
                                                                                   ifelse(grepl("network-scan*",SourceFire_non_deleted$classtype, perl=TRUE),  "Infrastructure/Service Attacks",
                                                                                          ifelse(grepl("(scan)",SourceFire_non_deleted$category, perl=TRUE),  "Infrastructure/Service Attacks",
                                                                                                 ifelse(grepl("(sql)",SourceFire_non_deleted$category, perl=TRUE),  "Infrastructure/Service Attacks",
                                                                                                        ifelse(SourceFire_non_deleted$classtype== "system-call", 
                                                                                                               "Infrastructure/Service Attacks", ""))))))))),"PHISHING")
#remove NA's 
SourceFire_non_deleted$BLOOM_Category <- sapply(SourceFire_non_deleted$BLOOM_Category, as.character)
SourceFire_non_deleted$BLOOM_Category[is.na(SourceFire_non_deleted$BLOOM_Category)] <- " "  

#Spyware/Adware/PUPs
SourceFire_non_deleted$BLOOM_Category <- ifelse(SourceFire_non_deleted$BLOOM_Category!="PHISHING", 
                                                ifelse(SourceFire_non_deleted$BLOOM_Category!="Infrastructure/Service Attacks", 
                                                       ifelse(SourceFire_non_deleted$category== "pua-adware.rules","Spyware/Adware/PUPs", ""), 
                                                       "Infrastructure/Service Attacks"), "PHISHING")
#remove NA's 
SourceFire_non_deleted$BLOOM_Category <- sapply(SourceFire_non_deleted$BLOOM_Category, as.character)
SourceFire_non_deleted$BLOOM_Category[is.na(SourceFire_non_deleted$BLOOM_Category)] <- " " 

#Browswer Exploits
SourceFire_non_deleted$BLOOM_Category <- ifelse(SourceFire_non_deleted$BLOOM_Category!="PHISHING", 
                                                ifelse(SourceFire_non_deleted$BLOOM_Category!="Infrastructure/Service Attacks", 
                                                       ifelse(SourceFire_non_deleted$BLOOM_Category!="Spyware/Adware/PUPs", 
                                                              ifelse(grepl("(browser)", SourceFire_non_deleted$category),"Browsing Exploits", ""), 
                                                              "Spyware/Adware/PUPs"), "Infrastructure/Service Attacks"), "PHISHING")

#remove NA's 
SourceFire_non_deleted$BLOOM_Category <- sapply(SourceFire_non_deleted$BLOOM_Category, as.character)
SourceFire_non_deleted$BLOOM_Category[is.na(SourceFire_non_deleted$BLOOM_Category)] <- " " 

#"Corp Policy Violations"
SourceFire_non_deleted$BLOOM_Category <- ifelse(SourceFire_non_deleted$BLOOM_Category!="PHISHING", 
                                                ifelse(SourceFire_non_deleted$BLOOM_Category!="Infrastructure/Service Attacks", 
                                                       ifelse(SourceFire_non_deleted$BLOOM_Category!="Spyware/Adware/PUPs", 
                                                              ifelse(SourceFire_non_deleted$BLOOM_Category!="Browsing Exploits", 
                                                                     ifelse(SourceFire_non_deleted$category== "policy-other.rules", "Corp Policy Violations",
                                                                            ifelse(SourceFire_non_deleted$category== "policy-social.rules", "Corp Policy Violations",
                                                                                   ifelse(SourceFire_non_deleted$category== "policy-multimedia.rules", "Corp Policy Violations",
                                                                                          ifelse(SourceFire_non_deleted$classtype== "policy-violation", "Corp Policy Violations", "")))),
                                                                     "Browsing Exploits"), "Spyware/Adware/PUPs"), "Infrastructure/Service Attacks"), "PHISHING")  

#remove NA's 
SourceFire_non_deleted$BLOOM_Category <- sapply(SourceFire_non_deleted$BLOOM_Category, as.character)
SourceFire_non_deleted$BLOOM_Category[is.na(SourceFire_non_deleted$BLOOM_Category)] <- " " 


#Malware
SourceFire_non_deleted$BLOOM_Category <- ifelse(SourceFire_non_deleted$BLOOM_Category!="PHISHING", 
                                                ifelse(SourceFire_non_deleted$BLOOM_Category!="Infrastructure/Service Attacks", 
                                                       ifelse(SourceFire_non_deleted$BLOOM_Category!="Spyware/Adware/PUPs", 
                                                              ifelse(SourceFire_non_deleted$BLOOM_Category!="Browsing Exploits", 
                                                                     ifelse(SourceFire_non_deleted$BLOOM_Category!= "Corp Policy Violations",
                                                                            ifelse(grepl("malware*", SourceFire_non_deleted$category),"Malware", 
                                                                                   ifelse(grepl("blacklist*", SourceFire_non_deleted$category),"Malware",
                                                                                          ifelse(grepl("trojan*", SourceFire_non_deleted$classtype),"Malware",
                                                                                                 ifelse(grepl("(indicator)", SourceFire_non_deleted$category),"Malware",
                                                                                                        ifelse(grepl("(exploit)", SourceFire_non_deleted$category),"Malware",
                                                                                                               ifelse(grepl("malware*", SourceFire_non_deleted$classtype),"Malware", "")))))),
                                                                            "Corp Policy Violations"),"Browsing Exploits"), "Spyware/Adware/PUPs"), "Infrastructure/Service Attacks"), "PHISHING") 

#remove NA's 
SourceFire_non_deleted$BLOOM_Category <- sapply(SourceFire_non_deleted$BLOOM_Category, as.character)
SourceFire_non_deleted$BLOOM_Category[is.na(SourceFire_non_deleted$BLOOM_Category)] <- " " 

#DLP
SourceFire_non_deleted$BLOOM_Category <- ifelse(SourceFire_non_deleted$BLOOM_Category!="PHISHING", 
                                                ifelse(SourceFire_non_deleted$BLOOM_Category!="Infrastructure/Service Attacks", 
                                                       ifelse(SourceFire_non_deleted$BLOOM_Category!="Spyware/Adware/PUPs", 
                                                              ifelse(SourceFire_non_deleted$BLOOM_Category!="Browsing Exploits", 
                                                                     ifelse(SourceFire_non_deleted$BLOOM_Category!= "Corp Policy Violations",
                                                                            ifelse(SourceFire_non_deleted$BLOOM_Category!= "Malware",
                                                                                   ifelse(grepl("sensitive\\-data\\.rules", SourceFire_non_deleted$category,perl =TRUE),'DLP',
                                                                                          ifelse(grepl("protocol\\-ftp\\.rules", SourceFire_non_deleted$category), "DLP", '')),  
                                                                                   "Malware"),"Corp Policy Violations"),"Browsing Exploits"), "Spyware/Adware/PUPs"), "Infrastructure/Service Attacks"), "PHISHING") 

#remove NA's 
SourceFire_non_deleted$BLOOM_Category <- sapply(SourceFire_non_deleted$BLOOM_Category, as.character)
SourceFire_non_deleted$BLOOM_Category[is.na(SourceFire_non_deleted$BLOOM_Category)] <- " " 



#Activity Reviews 
SourceFire_non_deleted$BLOOM_Category <- ifelse(SourceFire_non_deleted$BLOOM_Category!="PHISHING", 
                                                ifelse(SourceFire_non_deleted$BLOOM_Category!="Infrastructure/Service Attacks", 
                                                       ifelse(SourceFire_non_deleted$BLOOM_Category!="Spyware/Adware/PUPs", 
                                                              ifelse(SourceFire_non_deleted$BLOOM_Category!="Browsing Exploits", 
                                                                     ifelse(SourceFire_non_deleted$BLOOM_Category!= "Corp Policy Violations",
                                                                            ifelse(SourceFire_non_deleted$BLOOM_Category!= "Malware",
                                                                                   ifelse(SourceFire_non_deleted$BLOOM_Category!= "DLP",
                                                                                          ifelse(SourceFire_non_deleted$classtype=="misc-activity","Activity Reviews",
                                                                                                 ifelse(SourceFire_non_deleted$classtype=="bad-unknown","Activity Reviews",
                                                                                                        ifelse(SourceFire_non_deleted$classtype=="protocol-command", "Activity Reviews",
                                                                                                               ifelse(SourceFire_non_deleted$classtype=="misc-attack","Activity Reviews",
                                                                                                                      ifelse(grepl("attempted*",SourceFire_non_deleted$classtype, perl=TRUE), "Activity Reviews",
                                                                                                                             ifelse(SourceFire_non_deleted$BLOOM_Category=="","Activity Reviews","")))))),
                                                                                          "DLP"),"Malware"),"Corp Policy Violations"),"Browsing Exploits"), "Spyware/Adware/PUPs"), "Infrastructure/Service Attacks"), "PHISHING")

#remove NA's 
SourceFire_non_deleted$BLOOM_Category <- sapply(SourceFire_non_deleted$BLOOM_Category, as.character)
SourceFire_non_deleted$BLOOM_Category[is.na(SourceFire_non_deleted$BLOOM_Category)] <- "Activity Reviews" 


```
-------
  SourceFire New Category Breakdown Count
-------
  ```{r eval=TRUE, echo=FALSE}
SourceFire_cateogories <- as.data.frame(table(SourceFire_non_deleted$BLOOM_Category))
SourceFire_cateogories <- SourceFire_cateogories [order(-SourceFire_cateogories$Freq),]


#ggplot SourceFire_cateogories
SourceFire_cateogories <- transform(SourceFire_cateogories, Var1 = reorder(Var1, Freq))
ggplot(data = SourceFire_cateogories , aes(x = Var1, y = Freq, fill=Var1)) +  geom_bar(stat="identity") + geom_text(aes(label=Freq), size=5) + ggtitle("SourceFire New Categories") +
  coord_flip()
```

-------
  Suricata RULES
-------
  
  ```{r eval=TRUE, echo=FALSE}

table(et.pro.df$rule_who)
et.pro_rule_who <- as.data.frame(table(et.pro.df$rule_who))
et.pro_rule_who_count <- as.data.frame(sum(et.pro_rule_who$Freq))
names(et.pro_rule_who_count)[1] <- "Suricata_Rule_Count"
et.pro_rule_who_count

```

```{r eval=TRUE, echo=FALSE}
#extract year from CVE
et.pro.df$cveyear3 <- str_extract(et.pro.df$CVE, perl("cve\\s*,\\s*(CVE-|CAN-|cve|CVE_)?([0-9]{4})-"))
et.pro.df$cveyear3 <- str_extract(et.pro.df$cveyear3, perl("[0-9]{4}"))
CVE_year_et.pro <- (as.data.frame(table(et.pro.df$cveyear3)))
```

```{r eval=TRUE, echo=FALSE}

Bloomberg_SIGS <- et.pro.msg_type[grep("BLOOM", et.pro.msg_type$Var1),]
Bloomberg_SIGS <- as.data.frame(Bloomberg_SIGS)
Bloomberg_SIGS  <- Bloomberg_SIGS[order(-Bloomberg_SIGS$Freq),]
#Bloomberg_SIGS
```

-------
  Breakdown of Suricata RULES
-------
  ```{r eval=TRUE, echo=FALSE}

Non_Bloomberg_SIGS <- et.pro.msg_type[grep("^(?!BLOOM.*$)", et.pro.msg_type$Var1, perl = TRUE),]
Non_Bloomberg_SIGS  <- Non_Bloomberg_SIGS  [order(-Non_Bloomberg_SIGS$Freq),]
Non_Bloomberg_SIGS
```

-----
  Suricata protocol Breakdown
-----
  ```{r eval=TRUE, echo=FALSE}
et.pro.df_proto.df
```

-------
  Non Bloomberg Count of ET Pro / GPL / idef / Suricata / Volexity Rules
-------
  ```{r eval=TRUE, echo=FALSE}

Non_Bloomberg_SIGS_gr8_200 <- subset(Non_Bloomberg_SIGS, Non_Bloomberg_SIGS$Freq >= 200)
Non_Bloomberg_SIGS_200_20 <- subset(Non_Bloomberg_SIGS, Non_Bloomberg_SIGS$Freq <= 200 & Non_Bloomberg_SIGS$Freq >= 20)
Non_Bloomberg_SIGS_less_20 <- subset(Non_Bloomberg_SIGS, Non_Bloomberg_SIGS$Freq <= 20)

Non_Bloomberg_SIGS_gr8_200 <- transform(Non_Bloomberg_SIGS_gr8_200, Var1 = reorder(Var1, Freq))
ggplot(data = Non_Bloomberg_SIGS_gr8_200, aes(x = Var1, y = Freq, fill=Var1)) + geom_bar(stat="identity") + geom_text(aes(label=Freq), size=5) +
  coord_flip()

Non_Bloomberg_SIGS_200_20 <- transform(Non_Bloomberg_SIGS_200_20, Var1 = reorder(Var1, Freq))
ggplot(data = Non_Bloomberg_SIGS_200_20, aes(x = Var1, y = Freq, fill=Var1)) + geom_bar(stat="identity") + geom_text(aes(label=Freq), size=5) +
  coord_flip()

Non_Bloomberg_SIGS_less_20 <- transform(Non_Bloomberg_SIGS_less_20, Var1 = reorder(Var1, Freq))
ggplot(data = Non_Bloomberg_SIGS_less_20, aes(x = Var1, y = Freq, fill=Var1)) + geom_bar(stat="identity") + geom_text(aes(label=Freq), size=5) +
  coord_flip()
```

-----
  Suricata count of class types
-----
  ```{r eval=TRUE, echo=FALSE}
et.pro.df_classtype  <- et.pro.df_classtype  [order(-et.pro.df_classtype$Freq),]
et.pro.df_classtype <- transform(et.pro.df_classtype, Var1 = reorder(Var1, Freq))
ggplot(data = et.pro.df_classtype, aes(x = Var1, y = Freq, fill=Var1)) + geom_bar(stat="identity") + geom_text(aes(label=Freq), size=5) +
  coord_flip()
```

-------
  Distribution of Suricata Rules related to CVE year
-------
  
  ```{r eval=TRUE, echo=FALSE, warning=FALSE}

ET.PRO.CVE <- ggplot(data = CVE_year_et.pro, aes(x = Var1, y = Freq, ymax=max(Freq)*1.05)) + geom_bar(stat="identity") + geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.25)
```

```{r eval=TRUE, echo=FALSE}
plot(ET.PRO.CVE)
```

-------
  ET Pro New Categories 
-------
  ```{r eval=TRUE, echo=FALSE}
library(stringr)

rulefile_modified.etpro <- readLines("C://Users//blodge9//Downloads//perl//suricata_rules/combined_modified.txt")

signatures.modified <- grep("^\\w+\\-\\w+\\.rules\\s+alert.+\\;\\s?\\)", rulefile_modified.etpro, value=TRUE)


et.pro.modified.df <- as.data.frame(signatures.modified)
et.pro.modified.df$category <- str_extract(et.pro.modified.df$signatures.modified,'^\\w+\\-\\w+\\.rules')
et.pro.modified.df$signatures.modified <- gsub('^\\w+\\-\\w+\\.rules',"",et.pro.modified.df$signatures.modified)
et.pro.modified.df$signatures.modified <- gsub('^\\s+',"",et.pro.modified.df$signatures.modified)



et.pro.modified.df$msg <- str_extract(et.pro.modified.df$signatures,'msg\\:\\s?.+?\\;')
et.pro.modified.df$sid <- str_extract(et.pro.modified.df$signatures, 'sid\\:\\d+') 
et.pro.modified.df$classtype <- str_extract(et.pro.modified.df$signatures,'classtype\\:\\w+\\-\\w+')



et.pro.modified.df_proto <- str_extract(et.pro.modified.df$signatures, '^(?:\\S+\\s){1}(\\S+)')
et.pro.modified.df_proto <- gsub('alert\\s+',"", et.pro.modified.df_proto)
et.pro.modified.df_proto.df <- as.data.frame(table(et.pro.modified.df_proto))
et.pro.modified.df_proto.df <- et.pro.modified.df_proto.df [order(et.pro.modified.df_proto.df$Freq),]
et.pro.modified.df$proto <- et.pro.modified.df_proto



et.pro.modified.df_sip <- str_extract(et.pro.modified.df$signatures, '^(?:\\S+\\s){2}(\\S+)')
et.pro.modified.df_sip <- gsub('alert\\s+\\w+\\s+',"", et.pro.modified.df_sip)
et.pro.modified.df$sip <- et.pro.modified.df_sip

et.pro.modified.df_sport <- str_extract(et.pro.modified.df$signatures, '^[^-]*')
et.pro.modified.df_sport <- str_extract(et.pro.modified.df_sport, '(\\w+|\\W+|\\S+)(\\s+)$')
et.pro.modified.df$sport <- et.pro.modified.df_sport

et.pro.modified.df_dip <- str_extract(et.pro.modified.df$signatures, perl('(?<=\\-\\>)\\s+\\S+'))
et.pro.modified.df_dip <- gsub('^\\s+',"", et.pro.modified.df_dip)
et.pro.modified.df$dip <- et.pro.modified.df_dip


et.pro.modified.df_dport <- str_extract(et.pro.modified.df$signatures, perl('(?<=\\-\\>)\\s+\\S+\\s+\\S+'))
et.pro.modified.df_dport <- gsub('^\\s+\\S+',"", et.pro.modified.df_dport)
et.pro.modified.df_dport <- gsub('^\\s+',"",et.pro.modified.df_dport)
et.pro.modified.df$dport <- et.pro.modified.df_dport


et.pro.modified.df$sid <- gsub("sid\\:","", et.pro.modified.df$sid)
et.pro.modified.df$msg <- gsub("msg\\:\\s?","",et.pro.modified.df$msg)
et.pro.modified.df$msg <- gsub('^\\"',"", et.pro.modified.df$msg)
et.pro.modified.df$msg <- gsub('\\"\\;$',"", et.pro.modified.df$msg)

et.pro.modified.df$classtype <- gsub('classtype\\:',"", et.pro.modified.df$classtype)
et.pro.modified.df$msg_type <- str_extract(et.pro.modified.df$msg, "^\\s?\\w+\\s+\\w+")

et.pro.modified.df$rule_who <- str_extract(et.pro.modified.df$msg, "^\\w+")

et.pro.modified.df$CVE <- str_extract(et.pro.modified.df$signatures, 'reference\\:cve.+?\\;')
et.pro.modified.df$CVE<- gsub('reference\\:',"", et.pro.modified.df$CVE)
et.pro.modified.df$CVE<- gsub('\\;',"", et.pro.modified.df$CVE)
et.pro.modified.df_CVE <- as.data.frame(table(et.pro.modified.df$CVE))
et.pro.modified.df_CVE  <- et.pro.modified.df_CVE  [order(-et.pro.modified.df_CVE$Freq),]

et.pro.modified.df$cveyear3 <- str_extract(et.pro.modified.df$CVE, perl("cve\\s*,\\s*(CVE-|CAN-|cve|CVE_)?([0-9]{4})-"))
et.pro.modified.df$cveyear3 <- str_extract(et.pro.modified.df$cveyear3, perl("[0-9]{4}"))

et.pro.modified.df_category <- as.data.frame(table(et.pro.modified.df$category))
et.pro.modified.df_category <- et.pro.modified.df_category [order(et.pro.modified.df_category$Freq),]

et.pro.modified.df_classtype <- as.data.frame(table(et.pro.modified.df$classtype))
et.pro.modified.df_classtype <- et.pro.modified.df_classtype [order(et.pro.modified.df_classtype$Freq),]

#TOAL NUMBER OF ET PRO SIGNATURES
sum(et.pro.modified.df_category$Freq)

#GGPLOT OF ET PRO Categories
et.pro.modified.df_category <- transform(et.pro.modified.df_category, Var1 = reorder(Var1, Freq))
ggplot(data = et.pro.modified.df_category, aes(x = Var1, y = Freq, fill=Var1)) +  geom_bar(stat="identity") + geom_text(aes(label=Freq), size=5) + ggtitle("ET PRO Categories") +
  coord_flip()

#GGPLOT OF ET PRO classtypes
et.pro.modified.df_classtype <- transform(et.pro.modified.df_classtype, Var1 = reorder(Var1, Freq))
ggplot(data = et.pro.modified.df_classtype, aes(x = Var1, y = Freq, fill=Var1)) +  geom_bar(stat="identity") + geom_text(aes(label=Freq), size=5) + ggtitle("ET PRO Classtypes") +
  coord_flip()



#THE BLOOM CATEGORY
et.pro.modified.df$BLOOM_Category <- ""
View(et.pro.modified.df)

#phishing category
et.pro.modified.df$BLOOM_Category <- ifelse(grepl('(phish)', et.pro.modified.df$msg, ignore.case = TRUE), "PHISHING", "")


#Infrastructure/Service Attacks 
et.pro.modified.df$BLOOM_Category <- ifelse(et.pro.modified.df$BLOOM_Category!="PHISHING", 
                                            ifelse(et.pro.modified.df$classtype== "attempted-recon", "Infrastructure/Service Attacks",
                                                   ifelse(et.pro.modified.df$category== "etpro-web_server.rules", "Infrastructure/Service Attacks",       
                                                          ifelse(et.pro.modified.df$category== "etpro-attack_response.rules", "Infrastructure/Service Attacks",
                                                                 ifelse(et.pro.modified.df$category== "etpro-dos.rules", "Infrastructure/Service Attacks",
                                                                        ifelse(et.pro.modified.df$category== "etpro-pop3.rules", "Infrastructure/Service Attacks",       
                                                                               ifelse(grepl("denial\\-*",et.pro.modified.df$classtype, perl=TRUE), "Infrastructure/Service Attacks",       
                                                                                      ifelse(grepl("(server)",et.pro.modified.df$category, perl=TRUE),  "Infrastructure/Service Attacks",
                                                                                             ifelse(grepl("successful-recon*",et.pro.modified.df$classtype, perl=TRUE),  "Infrastructure/Service Attacks",
                                                                                                    ifelse(grepl("network-scan*",et.pro.modified.df$classtype, perl=TRUE),  "Infrastructure/Service Attacks",
                                                                                                           ifelse(grepl("(scan)",et.pro.modified.df$category, perl=TRUE),  "Infrastructure/Service Attacks",
                                                                                                                  ifelse(grepl("(sql)",et.pro.modified.df$category, perl=TRUE),  "Infrastructure/Service Attacks",
                                                                                                                         ifelse(et.pro.modified.df$classtype== "system-call", 
                                                                                                                                "Infrastructure/Service Attacks", "")))))))))))),"PHISHING")
#remove NA's 
et.pro.modified.df$BLOOM_Category <- sapply(et.pro.modified.df$BLOOM_Category, as.character)
et.pro.modified.df$BLOOM_Category[is.na(et.pro.modified.df$BLOOM_Category)] <- " "  



#Spyware/Adware/PUPs
et.pro.modified.df$BLOOM_Category <- ifelse(et.pro.modified.df$BLOOM_Category!="PHISHING", 
                                            ifelse(et.pro.modified.df$BLOOM_Category!="Infrastructure/Service Attacks", 
                                                   ifelse(grepl("(spware)",et.pro.modified.df$signatures.modified, perl=TRUE, ignore.case = TRUE), "Spyware/Adware/PUPs",
                                                          ifelse(grepl("(adware)",et.pro.modified.df$signatures.modified, perl=TRUE, ignore.case = TRUE), "Spyware/Adware/PUPs",
                                                                 ifelse(grepl("(PUP)",et.pro.modified.df$msg, perl=TRUE, ignore.case = TRUE), "Spyware/Adware/PUPs", ""))),
                                                   "Infrastructure/Service Attacks"), "PHISHING")
#remove NA's 
et.pro.modified.df$BLOOM_Category <- sapply(et.pro.modified.df$BLOOM_Category, as.character)
et.pro.modified.df$BLOOM_Category[is.na(et.pro.modified.df$BLOOM_Category)] <- " " 


#Browswer Exploits
et.pro.modified.df$BLOOM_Category <- ifelse(et.pro.modified.df$BLOOM_Category!="PHISHING", 
                                            ifelse(et.pro.modified.df$BLOOM_Category!="Infrastructure/Service Attacks", 
                                                   ifelse(et.pro.modified.df$BLOOM_Category!="Spyware/Adware/PUPs", 
                                                          ifelse(et.pro.modified.df$category=="etpro-tor.rules","Browsing Exploits",
                                                                 ifelse(et.pro.modified.df$category=="etpro-shellcode.rules","Browsing Exploits",
                                                                        ifelse(grepl("(browswer)", et.pro.modified.df$signatures.modified),"Browsing Exploits",
                                                                               ifelse(grepl("(plugin)", et.pro.modified.df$signatures.modified),"Browsing Exploits",
                                                                                      ifelse(grepl("(flash)", et.pro.modified.df$signatures.modified),"Browsing Exploits",
                                                                                             ifelse(grepl("(silverlight)", et.pro.modified.df$signatures.modified),"Browsing Exploits",
                                                                                                    ifelse(grepl("(java)", et.pro.modified.df$signatures.modified),"Browsing Exploits",
                                                                                                           ifelse(grepl("(php)", et.pro.modified.df$signatures.modified),"Browsing Exploits", "")))))))),
                                                          "Spyware/Adware/PUPs"), "Infrastructure/Service Attacks"), "PHISHING")

#remove NA's 
et.pro.modified.df$BLOOM_Category <- sapply(et.pro.modified.df$BLOOM_Category, as.character)
et.pro.modified.df$BLOOM_Category[is.na(et.pro.modified.df$BLOOM_Category)] <- " " 



#"Corp Policy Violations"
et.pro.modified.df$BLOOM_Category <- ifelse(et.pro.modified.df$BLOOM_Category!="PHISHING", 
                                            ifelse(et.pro.modified.df$BLOOM_Category!="Infrastructure/Service Attacks", 
                                                   ifelse(et.pro.modified.df$BLOOM_Category!="Spyware/Adware/PUPs", 
                                                          ifelse(et.pro.modified.df$BLOOM_Category!="Browsing Exploits", 
                                                                 ifelse(et.pro.modified.df$category== "etpro-policy.rules", "Corp Policy Violations",
                                                                        ifelse(et.pro.modified.df$category== "etpro-inappropriate.rules", "Corp Policy Violations",
                                                                               ifelse(et.pro.modified.df$category== "etpro-games.rules", "Corp Policy Violations",
                                                                                      ifelse(et.pro.modified.df$category== "etpro-p2p.rules", "Corp Policy Violations",
                                                                                             ifelse(et.pro.modified.df$category== "policy-violation", "Corp Policy Violations",
                                                                                                    ifelse(et.pro.modified.df$category== "etpro-tor.rules", "Corp Policy Violations",       
                                                                                                           ifelse(et.pro.modified.df$category== "etpro-chat.rules", "Corp Policy Violations", ""))))))),
                                                                 "Browsing Exploits"), "Spyware/Adware/PUPs"), "Infrastructure/Service Attacks"), "PHISHING")  

#remove NA's 
et.pro.modified.df$BLOOM_Category <- sapply(et.pro.modified.df$BLOOM_Category, as.character)
et.pro.modified.df$BLOOM_Category[is.na(et.pro.modified.df$BLOOM_Category)] <- " " 


#Malware
et.pro.modified.df$BLOOM_Category <- ifelse(et.pro.modified.df$BLOOM_Category!="PHISHING", 
                                            ifelse(et.pro.modified.df$BLOOM_Category!="Infrastructure/Service Attacks", 
                                                   ifelse(et.pro.modified.df$BLOOM_Category!="Spyware/Adware/PUPs", 
                                                          ifelse(et.pro.modified.df$BLOOM_Category!="Browsing Exploits", 
                                                                 ifelse(et.pro.modified.df$BLOOM_Category!= "Corp Policy Violations",
                                                                        ifelse(et.pro.modified.df$classtype== "trojan-activity", "Malware",
                                                                               ifelse(et.pro.modified.df$category== "etpro-compromised.rules", "Malware",
                                                                                      ifelse(et.pro.modified.df$category== "etpro-owned.rules", "Malware",
                                                                                             ifelse(grepl("(activex)", et.pro.modified.df$category),"Malware", 
                                                                                                    ifelse(grepl("(worm)", et.pro.modified.df$category),"Malware",
                                                                                                           ifelse(et.pro.modified.df$category== "etpro-malware.rules", "Malware",
                                                                                                                  ifelse(grepl("(exploit)", et.pro.modified.df$category),"Malware",
                                                                                                                         ifelse(grepl("(activex)", et.pro.modified.df$category),"Malware",
                                                                                                                                ifelse(grepl("(ciarmy)", et.pro.modified.df$category),"Malware",
                                                                                                                                       ifelse(grepl("(trojan)", et.pro.modified.df$category),"Malware",
                                                                                                                                              ifelse(grepl("(botcc)", et.pro.modified.df$category),"Malware",
                                                                                                                                                     ifelse(grepl("(dshield)", et.pro.modified.df$category),"Malware",
                                                                                                                                                            ifelse(grepl("(owned)", et.pro.modified.df$classtype),"Malware", ""))))))))))))),
                                                                        "Corp Policy Violations"),"Browsing Exploits"), "Spyware/Adware/PUPs"), "Infrastructure/Service Attacks"), "PHISHING") 

#remove NA's 
et.pro.modified.df$BLOOM_Category <- sapply(et.pro.modified.df$BLOOM_Category, as.character)
et.pro.modified.df$BLOOM_Category[is.na(et.pro.modified.df$BLOOM_Category)] <- " " 



#DLP
et.pro.modified.df$BLOOM_Category <- ifelse(et.pro.modified.df$BLOOM_Category!="PHISHING", 
                                            ifelse(et.pro.modified.df$BLOOM_Category!="Infrastructure/Service Attacks", 
                                                   ifelse(et.pro.modified.df$BLOOM_Category!="Spyware/Adware/PUPs", 
                                                          ifelse(et.pro.modified.df$BLOOM_Category!="Browsing Exploits", 
                                                                 ifelse(et.pro.modified.df$BLOOM_Category!= "Corp Policy Violations",
                                                                        ifelse(et.pro.modified.df$BLOOM_Category!= "Malware",
                                                                               ifelse(grepl("(ftp)", et.pro.modified.df$category,perl =TRUE),'DLP',''),  
                                                                               "Malware"),"Corp Policy Violations"),"Browsing Exploits"), "Spyware/Adware/PUPs"), "Infrastructure/Service Attacks"), "PHISHING") 

#remove NA's 
et.pro.modified.df$BLOOM_Category <- sapply(et.pro.modified.df$BLOOM_Category, as.character)
et.pro.modified.df$BLOOM_Category[is.na(et.pro.modified.df$BLOOM_Category)] <- " " 


table(et.pro.modified.df$BLOOM_Category)

#Activity Reviews 
et.pro.modified.df$BLOOM_Category <- ifelse(et.pro.modified.df$BLOOM_Category!="PHISHING", 
                                            ifelse(et.pro.modified.df$BLOOM_Category!="Infrastructure/Service Attacks", 
                                                   ifelse(et.pro.modified.df$BLOOM_Category!="Spyware/Adware/PUPs", 
                                                          ifelse(et.pro.modified.df$BLOOM_Category!="Browsing Exploits", 
                                                                 ifelse(et.pro.modified.df$BLOOM_Category!= "Corp Policy Violations",
                                                                        ifelse(et.pro.modified.df$BLOOM_Category!= "Malware",
                                                                               ifelse(et.pro.modified.df$BLOOM_Category!= "DLP",
                                                                                      ifelse(et.pro.modified.df$BLOOM_Category=="","Activity Reviews", ""),
                                                                                      "DLP"),"Malware"),"Corp Policy Violations"),"Browsing Exploits"), "Spyware/Adware/PUPs"), "Infrastructure/Service Attacks"), "PHISHING")

#remove NA's 
et.pro.modified.df$BLOOM_Category <- sapply(et.pro.modified.df$BLOOM_Category, as.character)
et.pro.modified.df$BLOOM_Category[is.na(et.pro.modified.df$BLOOM_Category)] <- "Activity Reviews"

et.pro.modified_categories <- et.pro.modified.df$BLOOM_Category
et.pro.modified_categories <- as.data.frame(table(et.pro.modified_categories))

et.pro.modified_categories <- et.pro.modified_categories [order(-et.pro.modified_categories$Freq),]
names(et.pro.modified_categories)
sum(et.pro.modified_categories$Freq)

et.pro.modified_categories <- transform(et.pro.modified_categories, et.pro.modified_categories = reorder(et.pro.modified_categories, Freq))
ggplot(data = et.pro.modified_categories , aes(x = et.pro.modified_categories, y = Freq, fill=et.pro.modified_categories)) +  geom_bar(stat="identity") + geom_text(aes(label=Freq), size=5) + ggtitle("ET Pro New Categories") +
  coord_flip()
```


