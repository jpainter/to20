load("to20.rda")
library(ggplot2)

# Calculate crude Proportion test positive
test.pos<-ddply( to20, .(TB), .fun = function(x){
  data.frame(
    n = nrow(x),
    TST0 = nrow(subset(x, TSTmm >=0 )) / nrow(x),
    TST5 = nrow(subset(x, TSTmm >=5 )) / nrow(x),
    TST10 = nrow(subset(x, TSTmm >=10 )) / nrow(x),
    TST15 = nrow(subset(x, TSTmm >=15 )) / nrow(x),
    QFT= nrow(subset(x, Result1 %in% c("Positive"))) /
      nrow(x),
    QFT.35= nrow(subset(x, qft.response>=0.35)) /
      nrow(x),
    QFT.25= nrow(subset(x, qft.response>=0.25)) /
      nrow(x),
    QFT.20= nrow(subset(x, qft.response>=0.20)) /
      nrow(x),   
    QFT.15= nrow(subset(x, qft.response>=0.15)) /
      nrow(x),
    QFT.10= nrow(subset(x, qft.response>=0.10)) /
      nrow(x),
    QFT.05= nrow(subset(x, qft.response>=0.05)) /
      nrow(x)
    )})
test.pos

write.table(test.pos, "clipboard", sep="\t", , row.names=FALSE)

# Summary data set by 5yr age 
to20.5yr<-ddply( to20, .(TB, age.5yr), .fun = function(x){
  data.frame(
    n = nrow(x),
    TST5 = nrow(subset(x, TSTmm >=5 )) / nrow(x),
    TST10 = nrow(subset(x, TSTmm >=10 )) / nrow(x),
    TST15 = nrow(subset(x, TSTmm >=15 )) / nrow(x),
    QFT= nrow(subset(x, Result1 %in% c("Positive"))) /
      nrow(x),
    QFT.35= nrow(subset(x, qft.response>=0.35)) /
      nrow(x),
    QFT.25= nrow(subset(x, qft.response>=0.25)) /
      nrow(x),
    QFT.15= nrow(subset(x, qft.response>=0.15)) /
      nrow(x)
    )})
to20.5yr
save(to20.5yr, file="to20.5yr.rda")


