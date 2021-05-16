plot_case <- function(x, y, xlab, ylab,
                      xlim=xlim, ylim=ylim, col,
                      xtlab, xtlab.at, ticksize,
                      ytlab, ytlab.at, auc=T, yaxis='on', ...){
  
  plot(x, y, xlab=xlab, ylab=ylab, bty='n', xlim=xlim, ylim=ylim, 
       xaxt='n', yaxt='n', col=col, type='l', cex.main=2, col.main='gray56', ...)                 # framework
  
  par(las=0)
  axis(side=1, labels=xtlab, at=xtlab.at, cex=ticksize, 
       tick=F, col.axis='gray56', cex.axis=1.2)                                                   # x axis
  
  if(yaxis=='on'){
  par(las=2)
  axis(side=2, labels=ytlab, at=ytlab.at, cex=ticksize, 
       tick=F, col.axis='gray56', cex.axis=1.1)                                                   # y axis
  } else{}
  
  if(auc==T){
  polygon(c(min(x, na.rm=T),x,max(x, na.rm=T)), c(0,y,0), border=NA, col=col)       # shade area under the curve
  }
  
}







