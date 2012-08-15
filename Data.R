## @knitr data

#	Tell R to read character data as strings, not factors
options(stringsAsFactors = FALSE)

#	Read in the csv data (which I exported from SAS)
#rm(to20)
#to20.sas <- read.csv(file.choose())
#save( to20.sas , file="to20.sas.rda")
load( file="to20.sas.rda")

#	Exclude 
to20<- subset(to20.sas, Age>14 ) 

# Define status
to20$TB<-factor( to20$Status,  
                     level= c(0,1,2), 
                     labels= c("Normal-CXR", "TB-CXR", "Culture-Confirmed TB" ) 
                     )
                     
#	Define NTM
to20$NTM <- as.numeric( ifelse(to20$NumberSputumCollected>0, ifelse(to20$NumberNTM>0 ,1,0) , NA ))
                     
to20$NTM<-factor( to20$NTM, 
                  level= c(0,1), 
                  labels= c("not NTM" , "NTM")
                  )
                     
#	Define TST groups
to20$TST.group<-cut( to20$TSTmm, 
                    breaks = c(-1,4,9,14, 100), right=TRUE , 
                    labels=c("0-4mm","5-9mm","10-14mm","15mm+"), ordered_result = TRUE 
                    )
                     
#	Define age groups
to20$age.group<-cut( to20$Age, 
                    breaks = c(14,34,44,54, 100), right=TRUE , 
                    labels=c("15-34","35-44","45-54","55+"), ordered_result = TRUE
                    )
                     
to20$age.5yr<- cut( to20$Age, 
                    breaks = c(14,19,24,29,34,39,44,49,54,59,64, 100), right=TRUE , 
                    labels=c("15-19","20-24","25-29","30-34", "35-39", "40-44", 
                            "45-49", "50-54", "55-59", "60-64", "65+"), ordered_result = TRUE
                            )
                     
#	Define QFT response >10 as equal to 10
to20$qft.response <- as.numeric( ifelse(to20$Tb_Nil1 %in% ">10", 10, as.character(to20$Tb_Nil1) ) )
                     
#	Correct value of Tb-Nil1 (50.5) to 5.05 for VN521446
to20[to20$StudyID=="VN521446",]$qft.response <- 5.05
                     
#	Group QFT response into 3 categories
to20$qft.response.cat <- cut(to20$qft.response, breaks = c(-10,0.35, 0.75, 11), right=FALSE ,
                        labels= c("<0.35","0.35-0.74", "0.75 +"), ordered_result = TRUE)

#	Save data
save( to20 , file="to20.rda")

## @knitr TB-groups
df <- ddply(to20, .(TB), .fun=function(x){data.frame(Count=nrow(x))} ) 
library(xtable)
xtable(df)

## @knitr Ages
# median ages
  # with(to20, aggregate(Age, list(TB), FUN = median))
# median confidence intervals
  median1 = wilcox.test(x=to20[as.numeric(to20$TB)==1,]$Age, conf.int=TRUE)
  median2 =wilcox.test(x=to20[as.numeric(to20$TB)==2,]$Age, conf.int=TRUE)
  median3 =wilcox.test(x=to20[as.numeric(to20$TB)==3,]$Age, conf.int=TRUE)
  median.est = rbind(median1$estimate, median2$estimate, median3$estimate)
  median.ci = rbind(median1$conf.int, median2$conf.int, median3$conf.int)
  median.summary = data.frame(est=median.est, lowerCI=median.ci[,1], upperCI=median.ci[,2])
  names(median.summary)[1]="Median"
  row.names(median.summary)=levels(to20$TB)
xtable(median.summary)