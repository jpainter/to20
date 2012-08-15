# ChoRay Exam data for population totals of applicants with abnormal CXR, and Cx+ TB

#  Import data from SAS file (CRDEC2008JAN2010)
options(stringsAsFactors = FALSE)
choray.sas <- read.csv(file.choose())

# Exclude records with missing status
choray<-subset(choray.sas,, c("Age", "Sex", "CanSuggestAvtiveTB", "NumberCulturePos", "Smear" ) )

# Define TB Status (akin to TO20 'status' variable)
choray$TB<-factor(ifelse(choray$CanSuggestAvtiveTB==0, 0 , 
                         ifelse(choray$NumberCulturePos>0, 2, 1)
                         ),
                  level= c(0,1,2), 
                  labels= c("Normal-CXR", "TB-CXR", "Culture Confirmed TB" )
                  )

# Exclude missing TB value
choray<- subset(choray, TB %in% c("Normal-CXR", "TB-CXR", "Culture Confirmed TB" ))
               
choray$age.5yr<- cut( choray$Age, 
                    breaks = c(14,19,24,29,34,39,44,49,54,59,64, 100), right=TRUE , 
                    labels=c("15-19","20-24","25-29","30-34", "35-39", "40-44", 
                          "45-49", "50-54", "55-59", "60-64", "65+"), ordered_result = TRUE
                    )              
               
# Exclude children
choray.adults<-subset(choray, Age> 14 )

save(choray.adults, file="choray.adults.rda")

# Smear positive
table(choray.adults$Smear)

