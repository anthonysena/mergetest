
library(xlsx)
library(lubridate)
library(reshape)
library(reshape2)
library(forecast)
library(tsoutliers)
library(strucchange)
library(ggplot2)
library(jpeg)
library(DatabaseConnector)
library(fma)


connectionDetails <- createConnectionDetails(dbms="pdw", server="XXX", user="XXX", password= "XXX", schema="XXX.dbo",port=17001)

connection <- connect(connectionDetails)
#dqdata <- read.xlsx("C:/Documents/OHDSI/DQCDM/dqcdm_temporal_summary.xlsx",1)





#dqdatam <- melt(dqdata,id.vars=c("source_name","concept_id","concept_name","domain_id","time_period_year","measure_id","time_period","time_period_month"))
#dqdatat <- dcast(dqdatam, source_name + concept_id + concept_name + time_period_year ~ time_period_month)

concepts <- as.data.frame(table(dqdata$concept_id))
sources <- as.data.frame(table(dqdata$source_name))

#analyses <- unique(dqdata[c("concept_id","source_name")])
analyses <- querySql(connection,"select distinct source_name, concept_id from dqcdm_temporal_summary where concept_id>0 order by source_name, concept_id")

# loop through all databases, all concepts
for(i in 1:100)#nrow(analyses))
{
  
  #i <- 1
  sourceFolder <-  paste("C:/Documents/OHDSI/DQCDM/",analyses[i,]$SOURCE_NAME,sep="")
  if (!file.exists(sourceFolder))
  {
    
    dir.create(sourceFolder)
  }
  
  resultsFolder <- paste("C:/Documents/OHDSI/DQCDM/",analyses[i,]$SOURCE_NAME,"/",analyses[i,]$CONCEPT_ID,sep="")
  if (!file.exists(resultsFolder))
  {
    
    dir.create(resultsFolder)
  }
  
  
  getDataSql <- paste("select prevalence, time_period from dqcdm_temporal_summary where concept_id=",analyses[i,]$CONCEPT_ID, " and source_name='",analyses[i,]$SOURCE_NAME,"' order by time_period",sep="")
  dqdata1 <- querySql(connection,getDataSql)
  
  dqdata1$time_period_year <- substr(dqdata1$TIME_PERIOD, 1, nchar(dqdata1$TIME_PERIOD)-2)
  dqdata1$time_period_month <- substr(dqdata1$TIME_PERIOD, 5, nchar(dqdata1$TIME_PERIOD))
  dqdata1$time_period_date <- ymd(paste0(dqdata1$TIME_PERIOD,"01"))
  nrow(dqdata1)
  
  #dqdata1 <- subset(dqdata, source_name == analyses[i,]$source_name & concept_id==, select=c(prevalence,time_period_year,time_period_month,time_period, time_period_date))
  #    dqdata1 <- dqdata1[order(dqdata1$time_period),]
  
  first_year <- as.numeric(dqdata1[1,]$time_period_year)
  first_month <- as.numeric(dqdata1[1,]$time_period_month)
  last_year <- as.numeric(dqdata1[nrow(dqdata1),]$time_period_year)
  last_month <- as.numeric(dqdata1[nrow(dqdata1),]$time_period_month)
  
  fulldateseq <- seq(from=ymd(paste0(dqdata1[1,]$TIME_PERIOD,"01")), to=ymd(paste0(dqdata1[nrow(dqdata1),]$TIME_PERIOD,"01")), by='month')
  fullprev <- data.frame(Date=fulldateseq, value=with(dqdata1, PREVALENCE[match(fulldateseq, dqdata1$time_period_date)]))
  fullprev[is.na(fullprev)] <- 0
  
  #figure out how this handles nulls and maybe put the 0s in
  ts <- ts(fullprev[,2], start=c(first_year, first_month), end=c(last_year, last_month), frequency=12)
  
  if(length(ts)>24)#not enough data to make temporal pattern
  {
    stl<- stl(ts,s.window="periodic")
    jpeg(file=paste(resultsFolder,"/stl.jpg",sep=""))
    plot(stl)
    dev.off() 
    
    
    #data quality checks
    #fit lr to trend data,  is abs(beta) large AND significant?
    
    lmfit <- tslm(ts ~ trend + season)
    lmfitsum <-summary(lmfit)
    analyses[i,"temporalslope"] <- lmfit$coef[2]
    analyses[i,"temporalslopesig"] <- lmfitsum$coef[2,4]
    
    
    
    seasonalfit <- ets(ts)
    seasonalfit2 <- ets(ts,model="ANN")
    
    deviance <- 2*c(logLik(seasonalfit) - logLik(seasonalfit2))
    df <- attributes(logLik(seasonalfit))$df - attributes(logLik(seasonalfit2))$df 
    #P value for seasonality test
    analyses[i,"seasonalsig"] <-1-pchisq(deviance,df)
    
    
    
    n <- length(ts)
    jpeg(file=paste(resultsFolder,"/lineartrend.jpg",sep=""))
    plot(ts)
    lines(ts(lmfit$coef[1]+lmfit$coef[2]*(1:n)+mean(lmfit$coef[-(1:2)]),
             start=start(ts),f=12),col="red")
    dev.off()    
    
    #look for any outlier values in remainder...
    
    # this is the right way to do it, but its just too damn slow....
    #     dqoutliers <- tso(ts)
    #     jpeg(file=paste(resultsFolder,"/dqoutliers.jpg",sep=""))
    #     plot(dqoutliers)
    #     dev.off()
    #     analyses[i,"numoutliers"] <- nrow(dqoutliers$outliers)
    
    stl2<- stl(ts,s.window="periodic",robust="TRUE")
    analyses[i,"numoutliers"] <- sum(stl2$weights < 1e-8)
    
    struc<-breakpoints(ts~1)
    analyses[i,"numbreakpoints"] <- length(struc$breakpoints)
    if(length(struc$breakpoints)>=1)
    {
      vlines <- data.frame(xint=as.numeric(dqdata1[struc$breakpoints,"time_period_date"]))
      p<-ggplot(data=dqdata1,aes(x=time_period_date, y=PREVALENCE)) +geom_point()+geom_vline(data=vlines, aes(xintercept=xint,colour="red"), linetype = "dashed")
      ggsave(filename = paste(resultsFolder,"/breakpoints.jpg",sep=""), plot=p) 
    }
    
  }
  
  
}

dummy <- dbDisconnect(connection)
