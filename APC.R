## @knitr APC  
# Annual Percent Change

library(survey)
library(ggplot2)

#### get data from Survey Freq.R

# Survey GLM:  Regress test on age with log link
# QFT
pop.qft.glm<- svyglm(QFT~as.numeric(age.5yr), popdesign, family=quasibinomial(link="log"))
  summary(pop.qft.glm)
  # Calculate annual percent change
  qft.apc<- data.frame( test="qft" ,
                      apc= (exp(coef(pop.qft.glm)[2]/5)-1 ) *100 ,
                      apc.ll= (exp(confint(pop.qft.glm)[2,1]/5)-1 ) *100 ,
                      apc.ul= (exp(confint(pop.qft.glm)[2,2]/5)-1 ) *100 ,
                      p= summary(pop.qft.glm)$coefficients[2,4]
                    )
  qft.apc

# TST5
pop.tst5.glm<- svyglm(TST5~as.numeric(age.5yr), popdesign, family=quasibinomial(link="log"))
  summary(pop.tst5.glm)
  # Calculate annual percent change
  tst5.apc<- data.frame( test="tst5" ,
                        apc= (exp(coef(pop.tst5.glm)[2]/5)-1 ) *100 ,
                        apc.ll= (exp(confint(pop.tst5.glm)[2,1]/5)-1 ) *100 ,
                        apc.ul= (exp(confint(pop.tst5.glm)[2,2]/5)-1 ) *100 ,
                        p= summary(pop.tst5.glm)$coefficients[2,4]
                       )
  tst5.apc

# TST10
pop.tst10.glm<- svyglm(TST10~as.numeric(age.5yr), popdesign, family=quasibinomial(link="log"))
  summary(pop.tst10.glm)
  # Calculate annual percent change
  tst10.apc<- data.frame( test="tst10" ,
                       apc= (exp(coef(pop.tst10.glm)[2]/5)-1 ) *100 ,
                       apc.ll= (exp(confint(pop.tst10.glm)[2,1]/5)-1 ) *100 ,
                       apc.ul= (exp(confint(pop.tst10.glm)[2,2]/5)-1 ) *100 , 
                        p= summary(pop.tst10.glm)$coefficients[2,4]
                       )
tst10.apc

# TST15
  pop.tst15.glm<- svyglm(TST15~as.numeric(age.5yr), popdesign, family=quasibinomial(link="log"))
    summary(pop.tst15.glm)
    # Calculate annual percent change
    tst15.apc<- data.frame( test="tst15" ,
                            apc= (exp(coef(pop.tst15.glm)[2]/5)-1 ) *100 ,
                            apc.ll= (exp(confint(pop.tst15.glm)[2,1]/5)-1 ) *100 ,
                            apc.ul= (exp(confint(pop.tst15.glm)[2,2]/5)-1 ) *100 , 
                            p= summary(pop.tst15.glm)$coefficients[2,4]
                          )
  tst15.apc

#summary table
  apc.summary <- rbind(qft.apc, tst5.apc, tst10.apc, tst15.apc)
  apc.summary

# Fitted values
  ages<- data.frame(age.5yr=seq(1:11))
  levels(ages$age.5yr)<- c("15-19","20-24","25-29","30-34", "35-39", "40-44", 
                   "45-49", "50-54", "55-59", "60-64", "65+")
  pop.qft.pred<- data.frame(ages, 
                      predict.glm(pop.qft.glm, newdata=ages, se=TRUE, type='response'))
  pop.tst5.pred<- data.frame(ages, 
                            predict.glm(pop.tst5.glm, newdata=ages, se=TRUE, type='response'))
  pop.tst10.pred<- data.frame(ages, 
                      predict.glm(pop.tst10.glm, newdata=ages, se=TRUE, type='response'))
  pop.tst15.pred<- data.frame(ages, 
                            predict.glm(pop.tst15.glm, newdata=ages, se=TRUE, type='response'))

# Mean predicted
  mean<- data.frame( age.5yr= ( mean(choray.adults$Age)-10)/5 )
    mean.qft<- data.frame(test="QFT", mean.age=mean(choray.adults$Age), predict.glm(pop.qft.glm, newdata=mean, se=TRUE, type='response'))
    mean.tst5<- data.frame(test="tst5", mean.age=mean(choray.adults$Age), predict.glm(pop.tst5.glm, newdata=mean, se=TRUE, type='response'))
    mean.tst10<- data.frame(test="tst10", mean.age=mean(choray.adults$Age), predict.glm(pop.tst10.glm, newdata=mean, se=TRUE, type='response'))
    mean.tst15<- data.frame(test="tst15", mean.age=mean(choray.adults$Age), predict.glm(pop.tst15.glm, newdata=mean, se=TRUE, type='response'))
    pop.mean<-  rbind(mean.qft, mean.tst5, mean.tst10, mean.tst15)
    pop.mean$ll <- pop.mean$fit-1.96*pop.mean$se.fit
    pop.mean$ul <- pop.mean$fit+1.96*pop.mean$se.fit
  pop.mean


#Relabel variables
to20.5yr.m.new<-to20.5yr.m
to20.5yr.m.new$Variable.new <- ifelse(to20.5yr.m$variable=="TST5", "TST-5",
                                      ifelse(to20.5yr.m$variable=="TST10", "TST-10",
                                             ifelse(to20.5yr.m$variable=="TST15", "TST-15", "QFT") ))
                                                    
# Plot population estimates for test positive
apc<- ggplot() +
  # QFT
    geom_ribbon( aes( x=ages$age.5yr ,
                    ymin= fit-1.96*se.fit, ymax=fit+1.96*se.fit), 
                    data= pop.qft.pred, fill='grey' , alpha=0.5) +
    geom_line( aes(x=as.integer(choray.summary$age.5yr),y=fit), 
              lty=1, data= pop.qft.pred ) +
  # TST5
    geom_ribbon( aes( x=as.integer(choray.summary$age.5yr) , 
                    ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), 
                    data= pop.tst5.pred, fill='grey', alpha=.5 ) +
    geom_line( aes(x=as.integer(choray.summary$age.5yr), y=fit), 
              lty=3, data= pop.tst5.pred ) +
  # TST10
    geom_ribbon( aes( x=as.integer(choray.summary$age.5yr) , 
                    ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), 
                    data= pop.tst10.pred, fill='grey', alpha=.5 ) +
    geom_line( aes(x=as.integer(choray.summary$age.5yr), y=fit), 
              lty=4, data= pop.tst10.pred ) +
  # TST15
    geom_ribbon( aes( x=as.integer(choray.summary$age.5yr) , 
                    ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), 
                    data= pop.tst15.pred, fill='grey', alpha=.5 ) +
    geom_line( aes(x=as.integer(choray.summary$age.5yr), y=fit), 
              lty=2, data= pop.tst15.pred ) +  
  # labels
    #geom_text( aes(3,.625, label="TST5-APC=0.40%/yr") , size=2.5) +
    geom_text( aes(3,.55, label="TST10-APC=0.73% per year"), size=2.5) +
    #geom_text( aes(3,.15, label="TST15-APC=0.83%/yr"), size=2.5) +
    geom_text( aes(8.25,.38, label="QFT-APC=2.05% per year"), size=2.5) +
    
  # Dummy line to provide legend for the lines above
    geom_line(aes(x=as.numeric(age.5yr) , y=10 , lty=to20.5yr.m.new$Variable.new), data=to20.5yr.m.new) +
    scale_linetype_manual("", values=c("TST-5"=3, "TST-10"=4, "TST-15"=2, "QFT"=1)) +
    theme_bw() +
    scale_fill_grey() +
    scale_x_continuous(breaks=seq(1:11), 
                     labels=c("15-19","20-24","25-29","30-34", "35-39", "40-44", 
                              "45-49", "50-54", "55-59", "60-64", "65+") ) +
    scale_y_continuous(labels=percent, limits=c(0,1)) +
      labs(x = "\nAge Group (Years)", y = "\n") +
  opts( legend.align="horizontal",legend.position="bottom", 
        strip.text.y = theme_text(size=10), 
        strip.background =  theme_rect(fill = "grey", colour = NA), 
        plot.title=theme_text(size=10),
        title =       "Estimated Applicant Population (N=20,100)" )
apc