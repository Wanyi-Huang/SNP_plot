library(ggplot2)
top.mar=0.2
right.mar=0.2
bottom.mar=0.2
left.mar=0.2
mytheme<-theme(panel.grid.major =element_blank(),
               panel.grid.minor = element_blank(),
               panel.background = element_blank(),
               panel.border = element_blank(),
               axis.line.y = element_line(color = "black"),
               axis.line.x = element_line(color = "black"),
               #axis.title.x = element_text(size = rel(1.2),color = "white"),
               axis.title.y = element_text(size = rel(1.2)),
               axis.text.y = element_text(size=rel(1.2),color="black"),
               #axis.text.x = element_text(size=rel(1.2),color="white"),
               plot.margin=unit(x=c(top.mar,right.mar,bottom.mar,left.mar),units="inches"))

dt <- read.table("data.txt",sep="\t", header = T)
LChr <- c(866743,982620,1066810,1105361,1087727,1295560,1315942,1306564)

dfm<-data.frame()
for (i in 1:8){
  Max<-LChr[i]
  ChrLoop<-seq(1, Max , by=500)
  df<-assign(paste("Chr",i,sep = ""),data.frame(rep(i),ChrLoop))
  df$ID<-paste(df[,1],df[,2],sep = "_")
  dfm<-rbind(dfm,df)
}
dfm$pos<-seq(1, length(dfm[,1])*500, by=500)
dt$ID<-paste(dt[,1],dt[,2],sep = "_")

vari<-merge(dfm,dt,by = "ID", sort=F, all=T)
vari[is.na(vari)] <- 0
vari<-vari[order(vari$pos),]
vari<-vari[,c(1,2,4,8,9)]
names(vari)<-c("ID","Chr","pos","variation","pi")

#variation plot
p1<-ggplot(vari, aes(x=pos))+geom_line(aes(y=variation,color=Chr))#+scale_color_manual(labels= col$Chr, values = col$Color) 
br<-c(436501,1363501,2403001,3492501,4573501,5769001,7073001,8388001)
la<-c("Chr1","Chr2","Chr3","Chr4","Chr5","Chr6","Chr7","Chr8")
p1<-p1+scale_x_continuous(limits = c(-100, 9154001),breaks = br,labels = la)
p1<-p1+scale_y_continuous(limits = c(-1,25),breaks = c(0,10,20),labels = c("0","10","20"))
p1<-p1+labs(y="No.of SNPs per 1,000 nucleotides",x="Chromosome")
p1<-p1 + mytheme + geom_hline(aes(yintercept=4.266), colour="black", linetype="dashed")
p1
