  #Fig.S1
  library("ggplot2")
  library("reshape2")
  
  GPP=read.csv("ECO2-ACO2-GPP.csv",header = T)
  GPP.1=t(GPP[,-1]);colnames(GPP.1)=c("Larix","Shrub")
  GPP.m=melt(GPP.1)
     
  tiff(paste0("GPP_ECO2_ACO2.tiff"),width=1400,height=900, res = 300)
     ggplot(GPP.m,aes(x=Var1,y=value,fill=Var2))+
       geom_bar(stat="identity",position="dodge" ,alpha=0.87)+
       scale_fill_manual(values=c("orange","blue"))+
       theme_bw()+  
       theme(panel.grid.major = element_blank(),
             axis.text = element_text(size=12),
             panel.grid.minor = element_blank())+
       labs(y="ECO2-ACO2 GPP (gC/m2/yr)")
       
   dev.off()