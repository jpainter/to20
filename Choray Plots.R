## @knitr figure1

load("choray.summary.rda")
library(ggplot2)
library(scales)
library(gridBase)
library(xtable)

##### Regression and estimation of annual percent change (APC)

# abnormal CXR on age with log link 
glm.cxrtb<-glm(data=choray.summary, cxrtb~as.numeric(age.5yr) , weight=N, family="binomial"(link="log"))
  xtable(summary(glm.cxrtb))
  # Calculate annual percent change
  cxrtb.apc=( exp(coef(glm.cxrtb)[2]/5)-1 ) *100
  cxrtb.apc
  cxrtb.fit<- data.frame( age.5yr=1:11, fit=fitted(glm.cxrtb)  )
  cxrtb.pred<- data.frame(choray.summary$age.5yr, predict.glm(glm.cxrtb, se=TRUE, type='response'))

# culture confirmed TB (mtb) on age with log link
glm.mtb<-glm(data=choray.summary, mtb~as.numeric(age.5yr) , weight=N, family="binomial"(link="log"))
  summary(glm.mtb)
  # Calculate annual percent change
  mtb.apc=( exp(coef(glm.mtb)[2]/5)-1 ) *100
  mtb.apc
  mtb.pred<- data.frame(choray.summary$age.5yr, predict.glm(glm.mtb, se=TRUE, type='response'))

# Plot percent abnormal CXR by age
apc.label=paste("APC=",mtb.apc,"% per year", sep="")
cxr<- ggplot() +
  geom_ribbon( aes(x=as.integer(choray.summary.age.5yr), ymin=fit-se.fit, ymax=fit+se.fit), 
               data=cxrtb.pred, fill='lightgrey') +
  geom_point( aes(x=as.integer(age.5yr), y=cxrtb), data=choray.summary ) + 
  geom_line( aes(x=as.integer(choray.summary.age.5yr),y=fit), data=cxrtb.pred ) +
  theme_bw() +
  scale_fill_grey() +
  scale_y_continuous(labels=percent) +
  labs(x = "\nAge Group (Years)", y = "Percentage CXR-TB\n") + 
  geom_text( aes(3,.0125, label=apc.label), size=3.5 ) +
  opts(legend.align="horizontal",legend.position="bottom")
cxr

# Plot percent culture confirmed TB by age  
mtb<- ggplot() +
  geom_ribbon( aes( x=as.integer(choray.summary.age.5yr), 
                    ymin=fit-se.fit, ymax=fit+se.fit), 
                    data=mtb.pred, fill='lightgrey' ) +
  geom_point( aes(x=as.integer(age.5yr), y=mtb), data=choray.summary ) + 
  geom_line( aes(x=as.integer(choray.summary.age.5yr),y=fit), data=mtb.pred ) +
  geom_text( aes(3,.0125, label="APC=2.9% per year"), size=3.5 ) +
  # include CXR-TB
#   geom_ribbon( aes(x=as.integer(choray.summary.age.5yr), ymin=fit-se.fit, ymax=fit+se.fit), 
#                data=cxrtb.pred, fill='lightgrey') +
#                  geom_point( aes(x=as.integer(age.5yr), y=cxrtb), data=choray.summary ) + 
#                  geom_line( aes(x=as.integer(choray.summary.age.5yr),y=fit), data=cxrtb.pred ) +
  # Themes
  theme_bw() +
  scale_x_continuous(breaks=seq(1:11), 
                     labels=c("15-19","20-24","25-29","30-34", "35-39", "40-44", 
                              "45-49", "50-54", "55-59", "60-64", "65+") ) +
  scale_y_continuous(labels=percent) +
  labs(x = "\nAge Group (Years)", y = "Percentage Culture-\nConfirmed TB\n") +
  opts(legend.align="horizontal",legend.position="bottom")
mtb

# Combine plots with viewport function
png("choray.pop.png", width=9, height=5.9, units="in", res=600, )
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(6,2)))
  vplayout<- function(x,y)
  viewport(layout.pos.row=x, layout.pos.col=y)
  print(cxr, vp= vplayout(1:3, 1:2))
  print(mtb, vp= vplayout(4:6, 1:2))
dev.off()
