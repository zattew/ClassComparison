###  t test per una coppia di confronti

load() #collapsed eset

subset <- eset[,eset$Class%in%c("...","control")]
subset <- subset[,order(subset$Class)]

res<-data.frame(mirna=fData(subset)$OriginalMatureName,
                Accession=fData(subset)$SEQUENCE,
                avg.cancer=0,
                avg.control=0,
                logFC=0,
                p=0,
                stringsAsFactors = F)


for(i in 1:nrow(subset)){
  model1<-t.test(exprs(subset)[i,]~subset$Class)
  res$avg.cancer[i] <- model1$estimate[1]
  res$avg.control[i] <- model1$estimate[2]
  res$logFC[i]<-model1$estimate[1]-model1$estimate[2]
  res$p[i]<-model1$p.value
  
}

res<-res[order(res$p),]
res$fdr<-p.adjust(res$p)

write.table(res,file="...",sep="\t",row.names=F,quote=F)
