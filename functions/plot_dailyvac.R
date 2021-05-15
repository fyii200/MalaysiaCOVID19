#### function to plot daily doses administered ####

plot_dailyvac <- function(x, y1, y2, ylim, xaxis.lab, xaxis.lab.at){

  
  plot(x, y1, bty='n', type='l', col=col[1], cex.axis=0.8,  xaxt='n',          # plot daily 1st dose
       yaxt='n', ylab='', xlab='', ylim=ylim, xlim=c(min(x), max(x)), 
       main='How many doses are given per day?', lwd=4, cex.main=2, 
       col.main='gray56')
  
  abline(h=seq(0, 1e5, 2e4), col='gray88', lwd=0.5)                            # add H reference lines
  
  lines(x, y2, col=col[2], lwd=6)                                              # plot daily 2nd dose
  
  par(las=2)                                                                   # configure y axis
  yaxis.lab <- seq(0, ylim[2], 2e4)                                          
  axis(side=2, labels=format(yaxis.lab, scientific=F, big.mark=','), 
       at=yaxis.lab, cex.axis=0.8, tick=F, col.axis='gray56', cex.axis=1.1)
  
  par(las=0)                                                                   # configure x axis
  axis(side=1, labels=xaxis.lab, at=xaxis.lab.at, 
       cex.axis=0.8, tick=F, col.axis='gray56', cex.axis=1.2)
  
  legend('topleft', lty=1, col=col, lwd=7,                                     # add legend to show which dose
         c('1st dose', '2nd dose'), bty='n',cex=1.5, text.col=col)             # is represented by which line
  
  
  today <- length(y1)                                                          # add coloured points to latest data
  points(c(max(x),max(x)), c(y1[today], y2[today]), 
         pch=19, col=col, cex=2)
  
}








