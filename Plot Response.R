load("to20.rda")

# TST continuous, QFT log--need to set neg QFT to near zero.
  to20$qft.response.adj <- ifelse(to20$qft.response<=0, .01, to20$qft.response)

#   Modify labels for facets and legend
  levels(to20$TB)<- c("Normal\nCXR\n(N=479)", "TB CXR\n(N=864)", "Culture\nConfirmed TB\n(N=132)")
  to20$R1<- ifelse(to20$Result1 %in% "Indetermin", "Indeterminate", to20$Result1 )

library(ggplot2)
fig2<- ggplot( to20 ,
              aes( x = TSTmm ,  y = qft.response.adj , shape=to20$R1)) +
              scale_y_log10( breaks = c(.1, .35, 1, 10), labels=c(".1", "0.35","1","10") ) +
              theme_bw() + 
              facet_grid(TB ~ . ) +
              
              # shade rectangle; select gray values from: gray.colors(3, start = 0.7, end = .9, gamma = 1)
                 geom_rect( aes(xmin=5, xmax=10, ymin=0.005, ymax=50), fill="#E6E6E6" , alpha=.5) +
                 geom_rect( aes(xmin=10, xmax=15, ymin=0.005, ymax=50), fill="#CCCCCC", alpha=.5 ) +
                 geom_rect( aes(xmin=15, xmax=30, ymin=0.005, ymax=50), fill="#B2B2B2", alpha=.5 ) +
                 
              geom_point(position ="jitter", size=1.5) +
              
              # Format shapes 
              scale_shape_manual("QFT Result", c("Positive"=1, "Negative"=20, "Indeterminate"=0)) +
              
              scale_x_continuous(breaks=c(0,5,10,15,30), 
                                 labels=c("0mm", "5mm", "10mm", "15mm", "30mm"),
                                 limits=c(0,30)) +
                                   
              geom_segment(y = log10(0.35) , yend=log10(0.35), x=0, xend=30, size=I(0.1) ) +
              geom_segment(y = 1.75 , yend=1.75, x=0, xend=30, size=I(0.1) ) +
              geom_segment(y = -2.25 , yend=-2.25, x=0, xend=30, size=I(0.1) ) +
              geom_segment(y = -2.5 , yend=1.75, x=10, xend=10, size=I(0.1) ) +
              geom_segment(y = -2.25 , yend=1.75, x=30, xend=30, size=I(0.1) ) +
              geom_segment(y = -2.25  , yend=1.75, x=0, xend=0, size=I(0.1) ) +
              
              # labels
#               geom_text(aes(x, y, label=lab), 
#                         data=data.frame(
#                           x=c(5, 15),
#                           y=c(2,2),
#                           lab=c("this", "is"), 
#                           yy=to20$TB
#                           )) +
                            
              labs(x = "\nTST Response (mm Induration)", y = "QFT Response (log10[IU/ml])\n") +
              opts(legend.align="horizontal",legend.position="top",
                   strip.text.y = theme_text(size=12), 
                   strip.background = theme_rect(fill = NA , colour = NA)
                   ) 
fig2

ggsave(fig2,file="QfttTst response.png", width=6.5, height=8, dpi=600 )

# png("QfttTst response.png", width=6.5, height=8, units="in", res=600 )
#   grid.newpage()
#   pushViewport(viewport(layout=grid.layout(3,1)))
#   vplayout<- function(x,y)
#   viewport(layout.pos.row=x, layout.pos.col=y)
#   print(fig2, vp=vplayout(1:3, 1))
# dev.off()

