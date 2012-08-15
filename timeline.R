## @knitr timeline

events = data.frame(v1=c(1900,1910,2002,2009),
                    v2=c("BCG", "TST", "IGRA", "Gene\nExpert"))
events$v1=as.Date(as.character(events$v1), format="%Y")

require(ggplot2)
dislocations <- c(-1,1,-1,1)

ggplot( events ) +
  geom_text( aes(x = v1, y=1.3*dislocations, label = v2), size=4) +
  geom_hline( yintercept=0, size=1, scale="date" ) +
  geom_point(aes(x = v1, y = dislocations), data=events) +
  geom_segment(  aes(x = v1, y=dislocations, xend=v1, yend=c(0,0,0,0)), 
                 alpha=.7) +
  scale_y_continuous(breaks = NULL, limits=c(-2,2)) +
                   theme_bw() +
                   opts(axis.text.x = theme_text(size = 16,angle = 45),
                        axis.text.y = theme_blank(),
                        axis.ticks = theme_blank(),
                        axis.title.x = theme_blank(),
                        axis.title.y = theme_blank()
                        )

# setting x limits to show more of future generates error: supplying discrete value to continuous scale
# scale_x_continuous(breaks = as.numeric(events$v1), labels = as.character(events$v1), limits=c(1875,2020)) +