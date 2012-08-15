## @knitr SensitivityTest 

with(to20, table(QFT,TB))
with(to20, table(TST10,TB))
with(to20, table(TST15,TB))
with(to20, table(TST5,TB))

#QFT vs TST10
M <- as.table(rbind(c(117, 15), c(107,25)))
dimnames(M) <- list(test=c("QFT","TST10"), result=c("Pos","Neg"))
M
(Xsq <- chisq.test(M))  # Prints test summary

#QFT vs TST15
M <- as.table(rbind(c(117, 15), c(63,69)))
dimnames(M) <- list(test=c("QFT","TST15"), result=c("Pos","Neg"))
M
(Xsq <- chisq.test(M))  # Prints test summary

#QFT vs TST5
M <- as.table(rbind(c(117, 15), c(118,14)))
dimnames(M) <- list(test=c("QFT","TST15"), result=c("Pos","Neg"))
M
(Xsq <- chisq.test(M))  # Prints test summary