## ROC

library(pROC)

roc.tst5<- roc( Culture ~ TST5, subset(to20,Culture %in% c('Neg','Pos'), c(Culture, TST5)) ,
                percent=TRUE,
                # arguments for auc
                partial.auc=c(100, 90), partial.auc.correct=TRUE,
                partial.auc.focus="sens",
                # arguments for ci
                ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE)
roc.tst10<- roc( Culture ~ TST10, subset(to20,Culture %in% c('Neg','Pos'), c(Culture, TST10)) ,
                 plot=TRUE, add=TRUE, percent=roc1$percent)
roc.tst15<- roc( Culture ~ TST15, subset(to20,Culture %in% c('Neg','Pos'), c(Culture, TST15)) ,
                 plot=TRUE, add=TRUE, percent=roc1$percent)

## Define QFT cutpoints
  to20$QFT.35<-ifelse(to20$qft.response>=.35,1,0)
  to20$QFT.30<-ifelse(to20$qft.response>=.30,1,0)
  to20$QFT.25<-ifelse(to20$qft.response>=.25,1,0)
  to20$QFT.20<-ifelse(to20$qft.response>=.20,1,0)
  to20$QFT.15<-ifelse(to20$qft.response>=.15,1,0)
  to20$QFT.10<-ifelse(to20$qft.response>=.10,1,0)


roc.qft.35<- roc( Culture ~ QFT.35, subset(to20,Culture %in% c('Neg','Pos'),) , 
                percent=TRUE ,
                # arguments for auc
                partial.auc=c(100, 90), partial.auc.correct=TRUE,
                partial.auc.focus="sens",
                # arguments for ci
                ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE ,
                print.auc=TRUE, show.thres=TRUE)

roc.qft.10<- roc( Culture ~ QFT.10, subset(to20,Culture %in% c('Neg','Pos'), ) ,
                  percent= TRUE,
                  # arguments for auc
                  partial.auc=c(100, 90), partial.auc.correct=TRUE,
                  partial.auc.focus="sens",
                  # arguments for ci
                  ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
                  # arguments for plot
                  plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                  print.auc=TRUE, show.thres=TRUE )
                 
   

