# Combine TO20 data with weighted population survey estimate (ChoRay)

#Reload previous datasets
load("to20.5yr.rda")
load("choray.summary.rda")

#Alter columns to match
str(to20.5yr)
str(choray.summary)

# For population extimate data, define as status=3
choray.adults$Status<- 3
to20.surveyfreq$Status <- factor( c(3) , 
                                  level= c(0,1,2,3), 
                                  labels= c("Normal CXR" , 
                                            "TB-CXR, Culture Negative", 
                                            "TB-CXR, Culture Positive", 
                                            "Population Estimate")
                                  )

to20.surveyfreq$n <- to20.surveyfreq$N
to20.surveyfreq$variable <- to20.surveyfreq$test
to20.surveyfreq$value <- to20.surveyfreq$pct
keep<-c("Age", "Sex", "CanSuggestAvtiveTB", "NumberCulturePos", "TB", "age.5yr", "Status")

#Join datasets
to20.choray <- rbind(to20.tests.age5yr , to20.surveyfreq[,keep])
to20.choray$percent<-to20.combined$value * 100
