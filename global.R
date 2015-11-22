
#Read in the data
library(data.table)
library(lubridate)
library(dplyr)
library(tidyr)
library(leaflet)
library(datasets)


breaches <- read.csv("./data/onc_breach_report.csv" ,na.strings = c("","\\N"), encoding = "UTF-8")
#Clean Data
breaches$Breach.Submission.Date <- mdy(breaches$Breach.Submission.Date)
breaches$Breach.Year <- year(breaches$Breach.Submission.Date)
breaches$Web.Description <- as.character(breaches$Web.Description)
breachesWDesc<- breaches%>%filter(!is.na(Web.Description))
breachType <- c("Theft","Loss","Other ","Hacking/IT Incident","Improper Disposal","Unauthorized Acess/Disclosure","Unknown") 
breaches$breach.types <- sapply(as.character(breaches$Type.of.Breach), strsplit,split=",")
maxCols <-max(sapply(breaches$breach.types,length) )
breachTypes <- lapply(breaches$breach.types,function(r) { l <- length(r); lastCols <- rep(NA,maxCols - l); as.list(c(r,lastCols)) })
df <- data.frame(matrix(unlist(breachTypes),length(breachTypes),byrow=TRUE))
col.count <- ncol(df)
names(df) <-   paste("breach.count",1:col.count,sep=".") 
#Clean up the levels
mlevels <- c()
for(i in 1:ncol(df)) {
        mlevels <- c(levels(df[,i]),mlevels)
        
}
for(i in 1:ncol(df)) {
        levels(df[,i]) <-  mlevels
        
}
breaches<-cbind(breaches,df)
breaches <- breaches%>%select(-breach.types)%>%distinct()
breachTypesByYear <- breaches[,c("Breach.Year","Covered.Entity.Type","Individuals.Affected",names(df))]

breach.types <- breachTypesByYear%>%gather(Breach.X,Breach.Type,-Breach.Year,-Covered.Entity.Type,-Individuals.Affected,na.rm=TRUE)
breach.types <- breach.types%>%select(-Breach.X)
 
breaches <- breaches%>%separate(Location.of.Breached.Information,into=c("location.of.breached.info.1","location.of.breached.info.2") ,sep = ",",extra="drop")

minYear <- min(breaches$Breach.Year)
maxYear <- max(breaches$Breach.Year)
midYear <- minYear + round((maxYear - minYear) / 2)

statePop <- read.csv("http://www.census.gov/popest/data/state/asrh/2014/files/SCPRC-EST2014-18+POP-RES.csv")
sfips <- read.csv("http://www2.census.gov/geo/docs/reference/state.txt",sep = "|")
statePop <- left_join(statePop,sfips)%>%na.exclude()
statePop$STATE <- as.character(statePop$STATE)
