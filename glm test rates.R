# GLM Test Rates

qft.glm<- glm(value ~ as.numeric(age.5yr) , 
              data=subset(to20.5yr.m, to20.5yr.m$TB %in% "Normal Chest Radiograph" & to20.5yr.m$variable %in% "QFT"), 
              weight=n, family=quasibinomial(link="log"))
  summary(qft.glm)

# predict values
  ages<- data.frame(age.5yr=seq(1:11))
  levels(ages$age.5yr)<- c("15-19","20-24","25-29","30-34", "35-39", "40-44", 
                         "45-49", "50-54", "55-59", "60-64", "65+")

  qft.pred<- data.frame(ages, 
                      predict.glm(qft.glm, newdata=ages, se=TRUE, type='response'))

  tst10.pred<- data.frame(ages, 
                        predict.glm(tst10.glm, newdata=ages, se=TRUE, type='response'))

# QFT

library(ggplot2)
ggplot() +
  geom_ribbon( aes( x=ages$age.5yr ,
                  ymin= fit-1.96*se.fit, ymax=fit+1.96*se.fit), 
             data= qft.pred, fill='grey' , alpha=0.5) +
               geom_line( aes(x=as.integer(choray.summary$age.5yr),y=fit), 
                          lty=1, data= qft.pred ) +
                            scale_linetype_manual("Proportion\nTest\nPositive", 
                                                  c("TST10", "QFT") ) +
#TST10
#   geom_ribbon( aes( x=as.integer(choray.summary$age.5yr) , 
#                     ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), 
#                     data= tst10.pred, fill='grey', alpha=.5 ) +
# 
#   geom_line( aes(x=as.integer(choray.summary$age.5yr), y=fit), 
#                   lty=4, data= tst10.pred ) +                                                     
  theme_bw() +
  scale_fill_grey() +
  scale_x_continuous(breaks=seq(1:11), 
                      labels=c("15-19","20-24","25-29","30-34", "35-39", "40-44", 
                                "45-49", "50-54", "55-59", "60-64", "65+") ) +
  scale_y_continuous(formatter = 'percent', limits=c(0,1)) +
  labs(x = "\nAge Group (Years)", y = "\n") +
  opts( legend.align="horizontal",legend.position="bottom", 
        strip.text.y = theme_text(size=10), 
        strip.background =  theme_rect(fill = "grey", colour = NA), 
        title =       "Test Rates" )