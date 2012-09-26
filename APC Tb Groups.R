
# Calculate apc for each test in each TB group

# TB1: normal CXR
tb1.tst5.apc<-apc(group=1, test="TST5", start=c(0,0) )
tb1.tst10.apc<-apc(group=1, test="TST10")
tb1.tst15.apc<-apc(group=1, test="TST15")
tb1.qft.apc<-apc(group=1, test="QFT")

# TB2: normal CXR
tb2.tst5.apc<-apc(group=2, test="TST5")
tb2.tst10.apc<-apc(group=2, test="TST10")
tb2.tst15.apc<-apc(group=2, test="TST15")
tb2.qft.apc<-apc(group=2, test="QFT")

# TB1: normal CXR
tb3.tst5.apc<-apc(group=3, test="TST5") 
tb3.tst10.apc<-apc(group=3, test="TST10")
tb3.tst15.apc<-apc(group=3, test="TST15")
tb3.qft.apc<-apc(group=3, test="QFT")

tb.apc<- rbind(  tb1.tst10.apc, tb1.tst15.apc, tb1.qft.apc,
                tb2.tst5.apc, tb2.tst10.apc, tb2.tst15.apc, tb2.qft.apc ) #tb1.tst5.apc model did not converge
row.names(tb.apc) <- NULL    
tb.apc

###NB.  Because pvalue for slope was not sig (p=.0599), the apc for group=1 tst5 was not automatically calculated.
#  values were calculated by hand using estimated slope 0.02917 and std.err 0.01547
# apc was 0.6 (0.0-1.2)

library(ggplot2)
tb.means<-ddply(to20, .(TB), .fun=function(x){
    data.frame(
      n = nrow(x),
      qft=mean(na.omit(subset(x)$QFT)),
      tst5=mean(na.omit(subset(x)$TST5)),
      tst10=mean(na.omit(subset(x)$TST10)),
      tst15=mean(na.omit(subset(x)$TST15))
    )})
tb.means

 # compare means from model with direct mean:
 tb1.qft.mean<- mean(na.omit(subset(to20, as.numeric(TB)==1)$QFT))
 tb1.tst5.mean<- mean(na.omit(subset(to20, as.numeric(TB)==1)$TST5))
 tb1.tst10.mean<- mean(na.omit(subset(to20, as.numeric(TB)==1)$TST10))
 tb1.tst15.mean<- mean(na.omit(subset(to20, as.numeric(TB)==1)$TST15))
