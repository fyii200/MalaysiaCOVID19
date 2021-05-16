#### function to plot daily doses administered ####

plot_dailyvac <- function(x, y1, y2, ylim, xaxis.lab, 
                          xaxis.lab.at, yaxis.lab ,leg_ypos){
  
  # define margin on b, l, t & r (leave more space at the bottom)
  par(las=0, mar=c(8,4,4,2))        
  
  plot(x, y1, bty='n', type='l', col=col[1], cex.axis=0.8,  xaxt='n',          # plot daily 1st dose
       yaxt='n', ylab='', xlab='', ylim=ylim, xlim=c(min(x), max(x)), 
       main='', lwd=4, cex.main=2, 
       col.main='gray56')
  
  par(las=2)                                                                   # configure y axis
  axis(side=2, labels=format(yaxis.lab, scientific=F, big.mark=','), 
       at=yaxis.lab, cex.axis=0.8, tick=F, col.axis='gray56', cex.axis=1.1)
  
  par(las=0)                                                                   # configure x axis
  axis(side=1, labels=xaxis.lab, at=xaxis.lab.at, 
       cex.axis=0.8, tick=F, col.axis='gray56', cex.axis=1.2)
  
  abline(h=seq(0, max(yaxis.lab), 1e4), col='gray88', lwd=0.5)                 # add H reference lines
  
  lines(x, y2, col=col[2], lwd=4)                                              # plot daily 2nd dose
  
  par(xpd = TRUE)                                                              # xpd=true so legend not constrined within plot
  legend(min(x), leg_ypos, lty=1, col=col, lwd=6, horiz=T,                     # add legend to show which dose
         c('1st dose', '2nd dose'), bty='n',cex=1.2, text.col=col)             # is represented by which line
  
  
  today <- length(y1)                                                          # add coloured points to latest data
  points(c(max(x),max(x)), c(y1[today], y2[today]), 
         pch=19, col=col, cex=2)
  
}








