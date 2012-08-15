## @knitr SASSurveyFreq

# Tell R to read character data as strings, not factors
options(stringsAsFactors = FALSE)

# Read in the csv data (which I exported from SAS SurveyFreq procedure; 
#   data set called 'joinpoint' because it was initially designed to be used with Joinpoint program)
rm(to20.surveyfreq)
  to20.surveyfreq <- read.csv(file.choose())# Save dataset

rm(to20.surveyfreq.5yr)
to20.surveyfreq.5yr <- read.csv(file.choose())# Save dataset

# Define factors
to20.surveyfreq$test<-factor(to20.surveyfreq$Test, 
                             level= c(0, 1,2,3), 
                             labels= c("QFT", "TST5" , "TST10", "TST15" ))

to20.surveyfreq.5yr$test<-factor(to20.surveyfreq$Test, 
                             level= c(0, 1,2,3), 
                             labels= c("QFT", "TST5" , "TST10", "TST15" ))
# to20.surveyfreq$age.5yr<- factor( to20.surveyfreq$Age5yr , c(4:14), 
#                  labels=c("15-19","20-24","25-29","30-34", "35-39", "40-44", 
#                           "45-49", "50-54", "55-59", "60-64", "65+") )

#Save file with changes
save(to20.surveyfreq, file="to20_surveyfreq.rda")
save(to20.surveyfreq.5yr, file="to20_surveyfreq_5yr.rda")

#Reload previous dataset 
load( file="to20_surveyfreq.rda")
load( file="to20_surveyfreq_5yr.rda")
