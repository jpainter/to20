## @knitr venn

load("C:/Users/bzp3/Desktop/Analysis/TO20/R/TO20/to20.rda")
require(plyr)

venn <- ddply( to20, .(TB), .fun=function(x) { 
  data.frame(
    qpos = sum(x$QFT, na.rm=TRUE),
    qt5pos = sum(x$QFT * x$TST5, na.rm=TRUE),
    t5pos = sum(x$TST5, na.rm=TRUE),
    qt10pos = sum(x$QFT * x$TST10, na.rm=TRUE),
    t10pos = sum(x$TST10, na.rm=TRUE),
    qt15pos = sum(x$QFT * x$TST15, na.rm=TRUE),
    t15pos = sum(x$TST15, na.rm=TRUE)
)})
venn 

require(VennDiagram)

vennf <- function (x, level="5" ) {
    tpos= paste("t",level,"pos", sep="") ;
    TSTlabel = paste("TST",level, sep="") ;
    qtpos= paste("qt",level,"pos", sep="") ;
    if(x[,tpos] >= x$qpos ){ 
                        v =draw.pairwise.venn(x[,tpos], x$qpos, x[,qtpos], 
                        category = c( TSTlabel, "QFT") ,
                        fill=c("grey", "white"), alpha=.75, 
                        cex=2, cat.cex=1.5, ext.text=FALSE, cat.dist=c(.025, .025),
                        inverted=(FALSE) 
                       ) ;
    } ;
    if(x[,tpos] < x$qpos){  # Reverse default positioning so that TST always on left.
                          v= draw.pairwise.venn(x[,tpos], x$qpos, x[,qtpos], 
                          category = c( "QFT", TSTlabel) ,
                          fill=c("white","grey"), alpha=.75, 
                          cex=2, cat.cex=1.5, ext.text=FALSE, cat.dist=c(.025, .025),
                          inverted=(TRUE) 
      ) ;
    } ;
    return(v) 
  }

# test
v=vennf(subset(venn, as.numeric(TB)==3), level="15")
v
tiff(filename="venn-test.tif", compression = "lzw");grid.draw(v);dev.off();

# Create 9 plots
status= c("1","2","3")
tb= c("5","10","15")
for(i in 1:3){
  for( j in 1:3){
      v=vennf( subset(venn, as.numeric(TB)==as.numeric(status[i])), level=tb[j] )
      tiff(filename=paste("venn", status[i], "-", tb[j], ".tif", sep=""), compression = "lzw");
      grid.draw(v);
      dev.off();
  }
}

