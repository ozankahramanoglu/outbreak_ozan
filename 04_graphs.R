library(ggplot2)
library(gtable)
library(grid)

  p1 <- ggplot(cusum_result)
  p1 <- p1 + geom_point(data = cusum_result, aes(x = Day, y = Observed, color=1),
                        size = 1, shape=20)
  p1 <- p1 +geom_point(data=cusum_result, aes(x = Day, y = Cp2, color = 2),
                       size = 2, shape=1)
  # p1 <- p1 +scale_colour_manual(name  ="",values = c("red","red"),labels=c("Daily counts"))
  p1 <- p1 + ylab('Daily counts')
  p1 <- p1 + facet_wrap(~Year)
  p1 <- p1 + theme(legend.position = "bottom",plot.margin = unit(c(1, 2, 0.5, 0.5), 'cm'),
                   panel.grid.major = element_blank(), 
                   #panel.grid.minor = element_blank(), 
                   panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                   panel.border = element_rect(colour = "black", fill=NA, size=0.2),
                   #axis.text.x=element_text(angle=0, family="serif", size=16, vjust = 0.0), 
                   axis.text.y=element_text(size=10),
                   axis.title=element_text(size=12,face="bold"),
                   legend.text=element_text(size=10), legend.title=element_text(colour="blue", size=10,
                                                                                face="bold"),
                   strip.text.x = element_text(size=10, face="bold"))
  
  p1
  