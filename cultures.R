# Culture results
library(sqldf)

# Read data
  cultures<-read.csv("C:/Users/bzp3/Desktop/Analysis/TO20/R/data/routinesmear.csv")
  # number of patients
  length(table(cultures$DatabaseId))

#table of smear result by sputum specimen
Smear.result<-table(cultures$Smear, cultures$SputumNumber)
Smear.result
  
#table of culture result by sputum specimen
  Culture1<-table(cultures$Culture, cultures$SputumNumber)
  Culture1

# new cases identified on first culture
  Culture1[2,1]

# new cases identified on SECOND culture
  NotCulture1<-sqldf ("select * from cultures where 
                      DatabaseId Not in (Select DatabaseId from cultures where Culture=1 and SputumNumber=1)")
  
  #remove factor
  NotCulture1$DatabaseId <- factor(NotCulture1$DatabaseId)
  length(table(NotCulture1$DatabaseId))

  #Results for those whose first culture was not positive
  Culture2<-table(NotCulture1$Culture, NotCulture1$SputumNumber)
  Culture2[2,2]

# new cases identified on THIRD culture
  NotCulture1or2<-sqldf ("select * from NotCulture1 where 
                          DatabaseId Not in 
                         (Select DatabaseId from NotCulture1 
                         where Culture=1 and SputumNumber=2)" )
  
  #remove factor
  NotCulture1or2$DatabaseId <- factor(NotCulture1or2$DatabaseId)
  length(table(NotCulture1or2$DatabaseId))

  #Results for those whose first culture was not positive
  Culture3<- table(NotCulture1or2$Culture, NotCulture1or2$SputumNumber)
  Culture3[2,3] 

  #Number and percent new cases identified by culture
  NewCases<-c(Culture1[2,1], Culture2[2,2], Culture3[2,3])
  NewCases
  NewCases.pct = 100 * NewCases/132
  NewCases.pct

# NTM
# table of NTM result by sputum specimen
NTM<-table(cultures$NTM, cultures$SputumNumber)
NTM

# table of NTM result by Culture, sputum
NTM.culture<-data.frame( table(cultures$NTM, cultures$Culture))
NTM.culture

# table of NTM number 
# number of patients
table(cultures$DatabaseId,cultures$NTM)

# Dataset of number of culture pos per patient
library(ggplot2)
num.cultures<- ddply(cultures, .(DatabaseId), .fun=function(x){data.frame(
                      n=nrow(x),
                      culture=sum(x$Culture),
                      ntm=sum(x$NTM), 
                      smear=sum(x$Smear)
                      )} )
num.cultures
table(num.cultures$culture, num.cultures$ntm)
table(num.cultures$smear, num.cultures$ntm)

#list patients with 3 NTM
num.cultures[which(num.cultures$ntm==3),]

  #   list culture data for those patients
  cultures[which(cultures$NTM==1),]

  #List patient with simultaneous NTM and smear pos
  cultures[which(cultures$NTM==1 & cultures$Smear==1),]

    # show complete history for these 3
    cultures[which(cultures$DatabaseId %in% c("VN76427", "VN86116")),]
