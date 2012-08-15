## @knitr SurveyFreq

# Script replicates process previously donw with SAS proc SurveyFreq
library(survey)
library(ggplot2)
library(plyr)

  load("choray.adults.rda")
  load("to20.rda")

# Weights for TB status
  pop.tb<- ddply(choray.adults, c("TB","age.5yr"), "nrow")
    pop.tb$pop = pop.tb$nrow
  to20.tb<- ddply(to20, c("TB","age.5yr"), "nrow")
    to20.tb$study = to20.tb$nrow
  prob.tb = cbind(pop.tb, to20.tb)
  prob.tb$wt = with(prob.tb, study/nrow)
#   wts = prob.tb[,c("TB", "age.5yr", "wt")]



# Add weights and variabale for finite correction (number indiv in each group) to to20.5yr dataset
to20$surv.prob = ddply(to20, .(PanelID), .fun=function(x) { 
                  subset( prob.tb, as.numeric(TB)==as.numeric(x$TB) & as.numeric(age.5yr)==as.numeric(x$age.5yr) , wt)
                  })$wt
#Finite population correction--provide for each stage.  fpc1=TB; fpc2=age.5yr
to20$fpc1 = ddply(to20, .(PanelID), .fun=function(x) { 
            sum(subset( prob.tb, as.numeric(TB)==as.numeric(x$TB),)$pop)
            })$V1
to20$fpc2 = ddply(to20, .(PanelID), .fun=function(x) { 
  subset( prob.tb, as.numeric(TB)==as.numeric(x$TB) & as.numeric(age.5yr)==as.numeric(x$age.5yr) , pop)
})$pop

##OLD
#   to20$surv.prob<- ifelse(as.numeric(to20$TB)==1, prob.tb[1], 
#                           ifelse(as.numeric(to20$TB)==2,  prob.tb[2], prob.tb[3])
#                           )
#   to20$fpc<- ifelse(as.numeric(to20$TB)==1, pop.tb[1], 
#                     ifelse(as.numeric(to20$TB)==2,  pop.tb[2], pop.tb[3])
#                     )


# Survey design
  popdesign<-svydesign(ids=~TB + age.5yr, strata=~TB + age.5yr, probs=~surv.prob, fpc=~fpc1+fpc2, data=to20 )

# Weighted results
  qft.pop<-svytable(~to20$age.5yr + to20$QFT, popdesign)
  qft.pop.pct = qft.pop[,2] / margin.table(qft.pop,1)
  qft.pop = cbind(qft.pop, qft.pop.pct )
  qft.pop

  tst10.pop<-svytable(~to20$age.5yr + to20$TST10, popdesign)
  tst10.pop.pct = tst10.pop[,2] / margin.table(tst10.pop,1)
  tst10.pop = cbind(tst10.pop, tst10.pop.pct )
  tst10.pop

