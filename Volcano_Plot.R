require(limma)
require(gdata)
require(gplots)
require(gplot2)
require(RColorBrewer)
require(geneplotter)

VolcanoPlot <- function(res,title)
{
  b1<- res
  plot(b1$logFC, 
       -log10(b1$P.Value),
       xlim=c(-17,17), ylim=c(0,33),
       pch=16,
       cex=0.7,
       xlab="log2(FC)", 
       ylab="-log10(P-Value)",
       main= title,
       cex.axis=1,
       cex.lab=1.2,
       cex.main=1.3
  )
  
  abline(h=-log10(0.05),lty=2)
  abline(v=log2(1),lty=2)
  
  points(b1[b1$logFC > 0 & b1$P.Value<0.05,"logFC"],
         -log10(b1[b1$logFC > 0 & b1$P.Value<0.05,"P.Value"]),
         pch=16,
         cex=0.7,
         col=2)
  
  points(b1[b1$logFC < 0 & b1$P.Value<0.05,"logFC"],
         -log10(b1[b1$logFC < 0 & b1$P.Value<0.05,"P.Value"]),
         pch=16,
         cex=0.7,
         col=3)
  
  text(4,15,labels=paste0("n=",sum(b1$logFC > 0 & b1$P.Value<0.05)), cex=1.2,col=2)
  text(-4,15,labels=paste0("n=",sum(b1$logFC < 0 & b1$P.Value<0.05)), cex=1.2,col=3)
  
}

##esempio
pdf("ClassComparison/VolcanoPlot.pdf")
par(mfrow=c(2,2))
VolcanoPlot(res,title="NeuT 8w Vs BALB/c 8w")
VolcanoPlot(res1,title ="NeuT 12w Vs BALB/c 12w")
VolcanoPlot(res2,title ="NeuT 16w Vs BALB/c 16w")
VolcanoPlot(res3,title="NeuT 20w Vs BALB/c 20w")
dev.off()
