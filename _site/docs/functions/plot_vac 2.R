#### plot cumulative number of doses ####

# argument 'col' requires two characters: one for 1st dose, while the other for 2nd dose

plot_vac <- function(x, y1, y2, title, xlim, ylim, tailor.xaxis=T,
                     xaxis.lab, xaxis.lab.at, col, tailor.yaxis=F, 
                     yaxt='s', yaxis.lab.at, yaxis.lab, refline=T,
                     leg_ypos, ...){
  
  par(las=0, mar=c(8,4,4,2))                                         # define margin on b, l, t & r (leave more space at the bottom)
  plot(x, y1, bty='n', type='l', col=col[1], lwd=3,                  # plot: at least 1 dose
       xlab='', ylab='', main=title, yaxt=yaxt, xaxt='n', 
       xlim=xlim, ylim=ylim, cex.main=2, col.main='gray56', ...)
  
  abline(h=yaxis.lab.at, lwd=0.5, col='gray88')                      # add H ref lines
  
  lines(x, y2, col=col[2])                                           # plot: fully vaccinated
  
  if (tailor.yaxis == T){                                            # configure y axis
    par(las=2)                                                         
    axis(side=2, labels=yaxis.lab, at=yaxis.lab.at, 
         cex.axis=1.1, tick=F, col.axis='gray56')
  } else{}
  
  if(tailor.xaxis==T){
    par(las=0)                                                        # configure x axis
    axis(side=1, labels=xaxis.lab, at=xaxis.lab.at, 
         cex.axis=1.2, tick=F, col.axis='gray56')
  } else{}
  
  polygon(c(min(x), x, max(x)), c(0, y1, 0), col=col[1], border=NA)  # shade area under the curve for at least 1 dose     
  polygon(c(min(x), x, max(x)), c(0, y2, 0), col=col[2], border=NA)  # and at least 2 doses
  
  par(xpd = TRUE)
  legend(min(x), leg_ypos, bty='n', lty=1, lwd=6, col=col, horiz=TRUE,     # legend to show light and dark blue lines   
         legend=c('1st dose', '2nd dose'), cex=1.2, text.col=col[3:2])     # == at least 1 and 2 doses, respectively
  
  if(refline==T){
    abline(h=c(max(y1),max(y2)), lty=2, col=col)                           # horizontal lines to show latest 1 & 2 doses
  }
  
}



# par(xpd = TRUE)
# plot_vac(x, y1, y2, col=col, title='', xlim=xlim,
#          ylim=ylim, tailor.xaxis=T, xaxis.lab, xaxis.lab.at, tailor.yaxis=T,
#          yaxt='n', yaxis.lab.at, yaxis.lab, refline=F, leg_ypos=-2)



