# from search for how to add geom_text to facet plot
# http://groups.google.com/group/ggplot2/browse_thread/thread/44bf0ced4b791295

library(ggplot2) 
x <-runif(9, 0, 125) 
data <- as.data.frame(x) 
data$y <--runif(9, 0, 125) 
data$yy <- factor(c("a","b","c")) 
pg <- ggplot(data, aes(x, y)) + geom_point(shape = 2) + 
  facet_grid(~yy) + geom_text(aes(80, 30, label="Same for each facet")) 
pg


ggplot(data, aes(x, y)) + geom_point(shape = 2) + 
  facet_grid(~yy) + 
  geom_text(aes(x, y, label=lab), 
              data=data.frame(
                x=60,
                y=Inf,
                lab=c("this","is","the way"), 
                yy=c("a","b","d")
                ), vjust=1)