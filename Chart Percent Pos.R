## @knitr figure3

library(reshape)
library(ggplot2) #Load library for graphs 
load("to20.5yr.rda")


#Combined AGE GROUP chart
  # data from Test+.R 
# transform data as matrix with one column for test--suitable for graphing by groups
  to20.5yr.m<- melt(to20.5yr, id.vars =c("TB" , "age.5yr", "n"),
                        measure.vars = c( "TST5" , "TST10", "TST15", "QFT") )
  #   Modify labels for facets
  levels(to20.5yr.m$TB)<- c("Normal Chest Radiograph", 
                            "TB-Chest Radiograph (Sputum Culture Negative)",
                            "Culture Confirmed TB")

TB.1<-
  ggplot( to20.5yr.m[as.numeric(to20.5yr.m$TB)=="1",],
        aes(x = age.5yr, y = value, group=variable , lty = factor(variable) )) +
          stat_smooth(aes(weight=n), method="glm" , family="binomial" , color="black", fill='grey', alpha=.5 ) +
          scale_linetype_manual(name="", 
                                values=c("TST5"=3, "TST10"=4, "TST15"=2, "QFT"=1) ) +
          #   geom_point(aes(shape = factor(variable))) +          
          theme_bw() +
          scale_y_continuous(labels = percent, limits=c(0,1)) + 
          labs(x= NULL , y = "\n") +
          opts( legend.position="none", 
                strip.text.y = theme_text(size=10),
                plot.title=theme_text(size=10),
                title="Normal Chest Radiograph (N=479)") 
TB.1

# Get theme elements from above chart
  theme_get()

TB.2<-
  ggplot( to20.5yr.m[as.numeric(to20.5yr.m$TB)=="2",],
          aes(x = age.5yr, y = value, group=variable , lty = factor(variable) )) +
            stat_smooth(aes(weight=n), method="glm" , family="binomial" , color="black", fill='grey', alpha=.5 ) +
            scale_linetype_manual(name="Proportion\nTest\nPositive", 
                                  values=c("TST5"=3, "TST10"=4, "TST15"=2, "QFT"=1) ) +
            #   geom_point(aes(shape = factor(variable))) +          
            theme_bw() +
            scale_y_continuous(labels = percent, limits=c(0,1)) + 
            labs(x= NULL , y = "Percentage Test Positive\n") +
            opts( legend.position="none" ,
                strip.text.y = theme_text(size=10),
                plot.title=theme_text(size=10),
                title="TB-Chest Radiograph-Sputum Culture Negative (N=864)") 
TB.2
TB.3<-
  ggplot( to20.5yr.m[as.numeric(to20.5yr.m$TB)=="3",],
          aes(x = age.5yr, y = value, group=variable , lty = factor(variable) )) +
            stat_smooth(aes(weight=n), method="glm" , family="binomial" , color="black", fill='grey', alpha=.5 ) +
            scale_linetype_manual(name="", 
                                  values=c("TST5"=3, "TST10"=4, "TST15"=2, "QFT"=1) ) +
            #   geom_point(aes(shape = factor(variable))) +          
            theme_bw() +
            scale_y_continuous(labels = percent, limits=c(0,1)) + 
            labs( x = NULL , y = "\n") +
            opts( legend.position="none",
                strip.text.y = theme_text(size=10),
                plot.title=theme_text(size=10),
                title="Culture Confirmed TB (N=132)") 
TB.3

# pct.pos<-
#   ggplot( to20.5yr.m,
#           aes(x = age.5yr, y = value, group=variable , lty = factor(variable) )) +
#             stat_smooth(aes(weight=n), method="glm" , family="binomial" , color="black" ) +
#             scale_linetype_manual("Proportion Test Positive", 
#                                   c("TST5"=3, "TST10"=4, "TST15"=2, "QFT"=1) ) +
#                                     #   geom_point(aes(shape = factor(variable))) +          
#                                     theme_bw() +
#                                     scale_y_continuous(formatter = "percent", limits=c(0,1)) + 
#                                     labs(x = "\nAge (Years)", y = "Proportion Test Positive\n") +
#                                     opts(legend.align="horizontal",legend.position="bottom", 
#                                          strip.text.y = theme_text(size=10)) +
#                                            facet_grid(TB ~ .)
# 
# 

png("figure3.png", width=6.5, height=8.5, units="in", res=600, )
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(47,1)))
  vplayout<- function(x,y)
  viewport(layout.pos.row=x, layout.pos.col=y)
  print(TB.3, vp=vplayout(22:32, 1))  
  print(TB.2, vp=vplayout(12:22, 1))
  print(TB.1, vp=vplayout(1:12, 1))
  print(apc, vp=vplayout(32:47, 1))
dev.off()


