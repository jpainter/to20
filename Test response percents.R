load("to20.rda")

#median values for each group
  qft.medians <- with(to20, aggregate(qft.response, list(Status), FUN = median) )
  tst.medians <- with(to20, aggregate(TSTmm, list(Status), FUN = median) )
  qft.medians
  tst.medians

# Correlations--Pearson's r
  tb1.cor<- cor.test(to20[which(as.numeric(to20$TB)==1),]$qft.response, 
                     to20[which(as.numeric(to20$TB)==1),]$TSTmm )
  tb2.cor<- cor.test(to20[which(as.numeric(to20$TB)==2),]$qft.response, 
                     to20[which(as.numeric(to20$TB)==2),]$TSTmm )
  tb3.cor<- cor.test(to20[which(as.numeric(to20$TB)==3),]$qft.response, 
                     to20[which(as.numeric(to20$TB)==3),]$TSTmm )

# Percent positive in subgroups
#   %TST10 when QFT Pos/
## TB=1
  tst.qft.tb1=data.frame(
    tb=1, rate="TST given QFT" ,
    tst5 = mean(subset(to20, as.numeric(to20$TB)==1 & QFT==1)$TST5 ),
    tst10 = mean(subset(to20, as.numeric(to20$TB)==1 & QFT==1)$TST10 ),
    tst15 = mean(subset(to20, as.numeric(to20$TB)==1 & QFT==1)$TST15 ) )
  tst.qft.tb1
## TB=2
  tst.qft.tb2=data.frame(
    tb=2, rate="TST given QFT" ,
    tst5 = mean(subset(to20, as.numeric(to20$TB)==2 & QFT==1)$TST5 ),
    tst10 = mean(subset(to20, as.numeric(to20$TB)==2 & QFT==1)$TST10 ),
    tst15 = mean(subset(to20, as.numeric(to20$TB)==2 & QFT==1)$TST15 ) )
  tst.qft.tb2
## TB=3
  tst.qft.tb3=data.frame(
    tb=2, rate="TST given QFT" ,
    tst5 = mean(subset(to20, as.numeric(to20$TB)==3 & QFT==1)$TST5 ),
    tst10 = mean(subset(to20, as.numeric(to20$TB)==3 & QFT==1)$TST10 ),
    tst15 = mean(subset(to20, as.numeric(to20$TB)==3 & QFT==1)$TST15 ) )
  tst.qft.tb3

#   %QFT when for TST
## TB=1
  qft.tst.tb1=data.frame(
    tb=1, rate="QFT given TST" ,
    tst04 = mean(subset(to20, as.numeric(to20$TB)==1 & TSTmm>=0 & TSTmm<5)$QFT ),
    tst59 = mean(subset(to20, as.numeric(to20$TB)==1 & TSTmm>=5 & TSTmm<10 & !is.na(QFT))$QFT ),
    tst1014 = mean(subset(to20, as.numeric(to20$TB)==1 & TSTmm>=10 & TSTmm<15)$QFT ),
    tst15up = mean(subset(to20, as.numeric(to20$TB)==1 & TSTmm>=15 & !is.na(QFT))$QFT ) , 
    tst10 = mean(subset(to20, as.numeric(to20$TB)==1 & TSTmm>=10 & !is.na(QFT))$QFT ) )
  qft.tst.tb1
## TB=2
  qft.tst.tb2=data.frame(
    tb=2, rate="QFT given TST" , 
    tst04 = mean(subset(to20, as.numeric(to20$TB)==2 & TSTmm>=0 & TSTmm<5)$QFT ),
    tst59 = mean(subset(to20, as.numeric(to20$TB)==2 & TSTmm>=5 & TSTmm<10)$QFT ),
    tst1014 = mean(subset(to20, as.numeric(to20$TB)==2 & TSTmm>=10 & TSTmm<15)$QFT ),
    tst15up = mean(subset(to20, as.numeric(to20$TB)==2 & TSTmm>=15 & !is.na(QFT))$QFT ),
    tst10 = mean(subset(to20, as.numeric(to20$TB)==2 & TSTmm>=10 & !is.na(QFT))$QFT ) )
  qft.tst.tb2

rbind(tst.qft.tb1,tst.qft.tb2)
rbind(qft.tst.tb1,qft.tst.tb2)