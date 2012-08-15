## Sensitivity versus additional tests

test.pos  # from 'Test+.R'

# Chart percent pos among normal versus pct pos among culture pos
# For qft
  qft.normal.pct <- c(test.pos[1,"QFT.05"], test.pos[1,"QFT.10"],test.pos[1,"QFT.20"],test.pos[1,"QFT.35"])
  qft.tb.pct <- c(test.pos[3,"QFT.05"], test.pos[3,"QFT.10"],test.pos[3,"QFT.20"],test.pos[3,"QFT.35"])
  
# For TST
  tst.normal.pct <- c(test.pos[1,"TST0"],test.pos[1,"TST5"],test.pos[1,"TST10"],test.pos[1,"TST15"])
  tst.tb.pct <- c(test.pos[3,"TST0"],test.pos[3,"TST5"],test.pos[3,"TST10"],test.pos[3,"TST15"])

ggplot() + 
  geom_point(aes(x=qft.normal.pct, y=qft.tb.pct)) +  
  geom_text(aes(x=qft.normal.pct+.07, y=qft.tb.pct, label=c("QFT.05","QFT.10","QFT.20","QFT.35"))) +
  geom_point(aes(x=tst.normal.pct, y=tst.tb.pct),  color="red") +  
  geom_text(aes(x=tst.normal.pct+.07, y=tst.tb.pct, label=c("TST0","TST-5","TST-10","TST-15")),  color="red") +
  scale_y_continuous(formatter = "percent", limits=c(0,1)) +
  scale_x_continuous(formatter = "percent", limits=c(0,1)) +
  labs(x="normal", y="TB")+
  theme_bw()

# ratio additional normal test pos to additional tb test pos
tst15to10=(test.pos[1,"TST10"]-test.pos[1,"TST15"])/(test.pos[3,"TST10"]-test.pos[3,"TST15"])
tst10to05=(test.pos[1,"TST5"]-test.pos[1,"TST10"])/(test.pos[3,"TST5"]-test.pos[3,"TST10"])
tst5to0=(test.pos[1,"TST5"]-test.pos[1,"TST0"])/(test.pos[3,"TST5"]-test.pos[3,"TST0"])

## Sensitivity
