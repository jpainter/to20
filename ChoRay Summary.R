## @knitr ChoRaySummary
library(xtable)

# Load data
load("choray.adults.rda")

## @knitr CR-Ages
# median confidence intervals
median1 = wilcox.test(x=choray.adults[as.numeric(choray.adults$TB)==1,]$Age, conf.int=TRUE)
median2 =wilcox.test(x=choray.adults[as.numeric(choray.adults$TB)==2,]$Age, conf.int=TRUE)
median3 =wilcox.test(x=choray.adults[as.numeric(choray.adults$TB)==3,]$Age, conf.int=TRUE)
median.est = rbind(median1$estimate, median2$estimate, median3$estimate)
median.ci = rbind(median1$conf.int, median2$conf.int, median3$conf.int)
median.n = as.data.frame(table(choray.adults$TB))[2]
median.summary = data.frame(N=median.n , est=median.est, lowerCI=median.ci[,1], upperCI=median.ci[,2])
names(median.summary)[2]="Median"
names(median.summary)[1]="Count"
row.names(median.summary)=levels(choray.adults$TB)
xtable(median.summary)

## @knitr CR-histogram
# Plot histogram of applicants by age
library(ggplot2)
library(scales) # comma format

# reverse order of TB classification so that histogram base has culture confirmed on bottom
choray.adults$TB<- factor( choray.adults$TB, rev(levels(choray.adults$TB)) )

choray.adults$TB.label<- factor(choray.adults$TB, label= c("Culture-Confirmed TB(N=211)", 
                                                           "TB-CXR(N=2,087)", 
                                                           "Normal-CXR(N=17,802)") )
#   levels(choray.adults$TB.label)<- ("CC-TB(N=", "TB-CXR(N=)", "Nomral-CXR(N=")
cxr.histo<-ggplot( choray.adults , aes(x=age.5yr, fill=TB.label )) +
    theme_bw() +
    scale_y_continuous(labels=comma) +
#     scale_fill_grey() +
    scale_fill_manual(values=c( "black", "dark gray", "light gray")) +
    geom_bar( ) + 
    labs(fill="", x = NULL, y = "\nVisa Applicants\n") +
    opts(legend.align="horizontal",legend.position="top") 
cxr.histo

# subset abnormal cxr to view culture result
choray.tbcxr<-subset(choray.adults, TB %in% c("TB-CXR", "Culture Confirmed TB" ) )

## @knitr CR-percents
# Calcultate Percent cxr-tb, and percent MTB
library(plyr)
choray.summary<-ddply(choray.adults , .(age.5yr), .fun = function(x){
            data.frame(
              N = nrow(x),
              # calc percent
              cxrtb = nrow( subset( x, TB %in% c("TB-CXR", "Culture Confirmed TB" ))) / nrow(x),
              mtb = nrow( subset( x, TB %in% c("Culture Confirmed TB"))) / nrow(x)
              )
            })
xtable(choray.summary)

## @knitr CR-CXR

load("choray.summary.rda")
library(ggplot2)
library(scales)
library(gridBase)
library(xtable)

# Regression and estimation of annual percent change (APC)

# abnormal CXR on age with log link 
glm.cxrtb<-glm(data=choray.summary, cxrtb~as.numeric(age.5yr) , weight=N, family="binomial"(link="log"))
glm.cxrtb$call
xtable(summary(glm.cxrtb))

# Calculate annual percent change
cxrtb.apc=( exp(coef(glm.cxrtb)[2]/5)-1 ) *100
cxrtb.apc.l=( exp( (coef(glm.cxrtb)[2]-1.96 * summary(glm.cxrtb)$coefficients[2,2]) /5)-1 ) *100
cxrtb.apc.u=( exp( (coef(glm.cxrtb)[2]+1.96 * summary(glm.cxrtb)$coefficients[2,2]) /5)-1 ) *100
cxrtb.apc.ci = paste(format(cxrtb.apc, digits=2), "[",format(cxrtb.apc.l, digits=2),"-", format(cxrtb.apc.u, digits=2),"]", sep="")

cxrtb.fit<- data.frame( age.5yr=1:11, fit=fitted(glm.cxrtb)  )
cxrtb.pred<- data.frame(choray.summary$age.5yr, predict.glm(glm.cxrtb, se=TRUE, type='response'))

# Plot percent abnormal CXR by age
apc.label=paste("APC=", cxrtb.apc.ci, "% per year", sep="")
cxr<- ggplot() +
  geom_ribbon( aes(x=as.integer(choray.summary.age.5yr), ymin=fit-se.fit, ymax=fit+se.fit), 
               data=cxrtb.pred, fill='lightgrey') +
                 geom_point( aes(x=as.integer(age.5yr), y=cxrtb), data=choray.summary ) + 
                 geom_line( aes(x=as.integer(choray.summary.age.5yr),y=fit), data=cxrtb.pred ) +
                 theme_bw() +
                 scale_fill_grey() +
                 scale_y_continuous(labels=percent) +
                 scale_x_continuous(breaks=seq(1:11), 
                                    labels=c("15-19","20-24","25-29","30-34", "35-39", "40-44", 
                                             "45-49", "50-54", "55-59", "60-64", "65+") ) +
                 labs(x = "\nAge Group (Years)", y = "Percentage CXR-TB\n") + 
                 geom_text( aes(3,.0125, label=apc.label), size=3.5 ) +
                 opts(legend.align="horizontal",legend.position="bottom")
cxr

## @knitr CR-MTB

# culture confirmed TB (mtb) on age with log link
glm.mtb<-glm(data=choray.summary, mtb~as.numeric(age.5yr) , weight=N, family="binomial"(link="log"))
glm.mtb$call
xtable(summary(glm.mtb))

# Calculate annual percent change
mtb.apc=( exp(coef(glm.mtb)[2]/5)-1 ) *100
mtb.apc.l=( exp( (coef(glm.mtb)[2]-1.96 * summary(glm.mtb)$coefficients[2,2]) /5)-1 ) *100
mtb.apc.u=( exp( (coef(glm.mtb)[2]+1.96 * summary(glm.mtb)$coefficients[2,2]) /5)-1 ) *100
mtb.apc.ci = paste(format(mtb.apc, digits=2), "[",format(mtb.apc.l, digits=2),"-", format(mtb.apc.u, digits=2),"]", sep="")

mtb.pred<- data.frame(choray.summary$age.5yr, predict.glm(glm.mtb, se=TRUE, type='response'))

# Plot percent culture confirmed TB by age  
apc.label=paste("APC=", mtb.apc.ci,"% per year", sep="")
mtb<- ggplot() +
  geom_ribbon( aes( x=as.integer(choray.summary.age.5yr), 
                    ymin=fit-se.fit, ymax=fit+se.fit), 
               data=mtb.pred, fill='lightgrey' ) +
                 geom_point( aes(x=as.integer(age.5yr), y=mtb), data=choray.summary ) + 
                 geom_line( aes(x=as.integer(choray.summary.age.5yr),y=fit), data=mtb.pred ) +
                 geom_text( aes(3,.0125, label=apc.label), size=3.5 ) +
                 theme_bw() +
                 scale_x_continuous(breaks=seq(1:11), 
                                    labels=c("15-19","20-24","25-29","30-34", "35-39", "40-44", 
                                             "45-49", "50-54", "55-59", "60-64", "65+") ) +
                scale_y_continuous(labels=percent) +
                labs(x = "\nAge Group (Years)", y = "Percentage Culture-\nConfirmed TB\n") +
                opts(legend.align="horizontal",legend.position="bottom")
mtb

## @ knitr
# Combine plots with viewport function
png("choray.pop.png", width=9, height=5.9, units="in", res=600, )
grid.newpage()
pushViewport(viewport(layout=grid.layout(6,2)))
vplayout<- function(x,y)
  viewport(layout.pos.row=x, layout.pos.col=y)
print(cxr, vp= vplayout(1:3, 1:2))
print(mtb, vp= vplayout(4:6, 1:2))
dev.off()

