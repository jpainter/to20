## @knitr cultures
library(xtable)

load("to20.rda")


table(to20$Status, to20$Culture)
# One participant had culture, but had normal CXR
# Limit table to those who had abnormal cxr
to20.abnormal<- subset(to20, Status !="Normal CXR")

print("Table of Culure vs. NTM")
xtable(table(to20.abnormal$Culture, to20.abnormal$NTM))

print("Table of NumberNTM vs. NumberCulturePos")
xtable(table(to20.abnormal$NumberNTM, to20.abnormal$NumberCulturePos ))

# QFT TST response among MTB
to20.mtb<-subset(to20, NumberCulturePos>0)

# Relationship of number MTB (1-3 of 3 cultures) to magnitude of test result
MTB.QFT<-table(to20.mtb$QFT, to20.mtb$NumberCulturePos)
print("Table of QFT vs. NumberCulturePos (culture pos only)")
xtable(MTB.QFT)

#chisq.test(MTB.QFT)
  # Confirm significant finding with exact test
  fisher.test(MTB.QFT)

MTB.TST10<-table(to20.mtb$TST10, to20.mtb$NumberCulturePos)
print("Table of TST10 vs. NumberCulturePos (culture pos only)")
xtable(MTB.TST10)
chisq.test(MTB.TST10)

# Relationship between TST10 and QFT positivity, crude 
MTB.QftVTst.crude<-table(to20.mtb$QFT, to20.mtb$TST10)
print("Table of QFT vs. TST10 (culture pos only)")
xtable(MTB.QftVTst.crude)
mcnemar.test(MTB.QftVTst.crude)
    # and stratified by number positive cultures
    MTB.QftVTst.numberMTB<-table(to20.mtb$QFT, to20.mtb$TST10, to20.mtb$NumberCulturePos)

# TST continuous, QFT log--need to set negative QFT to near zero.
to20.mtb$qft.response.adj <- ifelse(to20.mtb$qft.response<=0, .01, to20.mtb$qft.response)

## @knitr MTB
# TST and QFT response by number of cultures yielding MTB
ggplot( to20.mtb ,
    aes( x = TSTmm ,  y = qft.response.adj , shape=Result1)) +
    scale_y_log10() +
    theme_bw() + 
  # shade rectangle; select gray values from: gray.colors(3, start = 0.7, end = .9, gamma = 1)
    geom_rect( aes(xmin=5, xmax=10, ymin=0.005, ymax=50), fill="#E6E6E6" ) +
    geom_rect( aes(xmin=10, xmax=15, ymin=0.005, ymax=50), fill="#CCCCCC" ) +
    geom_rect( aes(xmin=15, xmax=30, ymin=0.005, ymax=50), fill="#B2B2B2" ) +
    geom_point(position ="jitter") +
    scale_shape_manual("QFT Result", values=c("Positive"=19, "Negative"=1, "Indetermin"=0)) +
    xlim(0,30) +
  # Reference lines
    geom_segment(y = log10(0.35) , yend=log10(0.35), x=0, xend=30, size=I(0.1) ) +
    labs(x = "TST Response (mm Induration)", y = "Log10 of QFT Response (IU/ml)") +
    opts(legend.align="horizontal",legend.position="bottom") +
    facet_grid(NumberCulturePos ~ .)

# Relationship of number MTB (1-3 of 3 cultures) to magnitude of test result
MTB.QFT<-table(to20.mtb$QFT, to20.mtb$NumberCulturePos)
xtable(MTB.QFT)
chisq.test(MTB.QFT)
MTB.TST10<-table(to20.mtb$TST10, to20.mtb$NumberCulturePos)
xtable(MTB.TST10)
chisq.test(MTB.TST10)

## @knitr NTM
# QFT TST response among NTM
to20.ntm<-subset(to20, NumberNTM>0)
table(to20.ntm$QFT, to20.ntm$TST10)

# TST continuous, QFT log--need to set negative QFT to near zero.
to20.ntm$qft.response.adj <- ifelse(to20.ntm$qft.response<=0, .01, to20.ntm$qft.response)

ggplot( to20.ntm ,
    aes( x = TSTmm ,  y = qft.response.adj , shape=Result1)) +
    scale_y_log10() +
    theme_bw() + 
  # shade rectangle; select gray values from: gray.colors(3, start = 0.7, end = .9, gamma = 1)
    geom_rect( aes(xmin=5, xmax=10, ymin=0.005, ymax=50), fill="#E6E6E6" ) +
    geom_rect( aes(xmin=10, xmax=15, ymin=0.005, ymax=50), fill="#CCCCCC" ) +
    geom_rect( aes(xmin=15, xmax=30, ymin=0.005, ymax=50), fill="#B2B2B2" ) +
    geom_point(position ="jitter") +
    scale_shape_manual("QFT Result", values= c("Positive"=19, "Negative"=1, "Indetermin"=0)) +
    xlim(0,30) +
  # Reference lines
    geom_segment(y = log10(0.35) , yend=log10(0.35), x=0, xend=30, size=I(0.1) ) +
    labs(x = "TST Response (mm Induration)", y = "Log10 of QFT Response (IU/ml)") +
    opts(legend.align="horizontal",legend.position="bottom") +
    facet_grid(NumberCulturePos ~ .)

# Relationship of number MTB (1-3 of 3 cultures) to magnitude of test result
NTM.QFT<-table(to20.ntm$QFT, to20.ntm$NumberNTM)
xtable(NTM.QFT)
chisq.test(NTM.QFT)
NTM.TST10<-table(to20.ntm$TST10, to20.ntm$NumberNTM)
xtable(NTM.TST10)
chisq.test(NTM.TST10)