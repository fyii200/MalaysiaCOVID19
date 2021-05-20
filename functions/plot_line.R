# function to plot line and x-axis only #

plot_line <- function(x, y){
             par(mar = c(1, 0.3, 0, 0.3), las = 0)
              plot(x, y, 
              bty = 'n', 
              xaxt = 'n', 
              yaxt = 'n', 
              type = 'n', 
              xlab = '', 
              ylab = '', 
              ylim = c(0, max(y, na.rm = T) ) 
                  )
  
             xtlab <- seq(min(x), max(x), length.out=6)
             xtlab_format <- format(xtlab, '%d %b %Y')
  
             axis(side = 1, 
                   labels = format(xtlab, '%b'), 
                   at = xtlab, 
                   tick=F, 
                   col.axis='gray56', 
                   cex.axis=0.6, 
                   padj=-3)
  
             lines(x, y, col = col[2], lwd = 2)
             }






