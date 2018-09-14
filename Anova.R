library(limma)
library(affy)
library(sva)
library(gdata)

#### ANOVA per test variabile multiple

setwd()
load() #eset collapsed


res<-data.frame(mirna=fData(eset)$OriginalMatureName,
                Sequence=fData(eset)$SEQUENCE,
                p_anova=0,
                pre_res_breast_cancer_vs_control=0,post_res_breast_cancervs_control=0,stringsAsFactors = F)

for(i in 1:nrow(eset)){
  model1<-aov(exprs(eset)[i,]~eset$Class)
  model2<-summary(model1)
  res$p_anova[i]<-model2[[1]][[5]][[1]]
  posthoc<-TukeyHSD(x=model1, "eset$Class", conf.level=0.95)
  res[i,4:ncol(res)]<-posthoc$`eset$Class`[,4]
}

res<-res[order(res$p_anova),]
write.table(res,file="",sep="\t",row.names=F,quote=F)
