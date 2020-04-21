  #Fig.5 NPP

  library("ggplot2")
  library("reshape2")
  library(ggpubr)
 
  l = list.files("Data/")
  
  #ECO2----------------
  l_eco2 = grep("eco2",l)#position id
  l.flie = l[l_eco2]#eco2
  p=list()

  for(i in 1:length(l.flie)){
    fl=read.table(paste0("Data/",l.flie[i]))
    colnames(fl)=c("year","TAMB","T0.00","T2.25","T4.50","T6.75","T9.00")
    fl.m=melt(fl,id.vars = "year")
    line=c(rep("s",11),rep("d",11*5))#define linetype(dashed or solid)
    fl.m=cbind(fl.m,line)
    #plant functional type
    type = strsplit(l.flie[i],"_")[[1]][3]
    if(i>4) type=paste0(type,"_",strsplit(l.flie[i],"_")[[1]][4])
    
    p[[i]]=ggplot(fl.m,aes(x=year,y=value,color=variable,linetype=line))+
      geom_line()+theme_bw()+  
      theme(panel.grid.major = element_blank(),
            axis.text = element_text(size=12),
            legend.position = "none",
            axis.title.y=element_blank(),
            panel.grid.minor = element_blank())+
      labs(title=type,x="Year")+
      scale_x_continuous(breaks = seq(2015,2024,3))+
      scale_linetype_manual(values=c("twodash","solid"))+
      scale_color_manual(values = c("black","blue","turquoise3","green3","red","orange"))
  }
  
  tiff(paste0("NPP_ECO2.tiff"),width=2800,height=1800, res = 300)
  p.all = ggarrange(p[[3]],p[[2]],p[[4]],p[[6]],p[[5]],p[[1]],labels = c("g", "h", "i","j","k","l"),
                    label.x = 0.17,label.y = 0.89,ncol = 3, nrow = 2)
  annotate_figure(p.all,left = text_grob(expression(paste("NPP(gC/",m^2,"/yr)")), rot = 90))
  dev.off()
  
  #ACO2----------------
  l_eco2 = grep("aco2",l)#position id
  l.flie = l[l_eco2]
  p=list()
  
  for(i in 1:length(l.flie)){
    fl=read.table(paste0("Data/",l.flie[i]))
    colnames(fl)=c("year","TAMB","T0.00","T2.25","T4.50","T6.75","T9.00")
    fl.m=melt(fl,id.vars = "year")
    #plant functional type
    type = strsplit(l.flie[i],"_")[[1]][3]
    if(i>3) type=paste0(type,"_",strsplit(l.flie[i],"_")[[1]][4])
    
    p[[i]]=ggplot(fl.m,aes(x=year,y=value,color=variable))+
      geom_line()+theme_bw()+  
      theme(panel.grid.major = element_blank(),
            axis.text = element_text(size=12),
            legend.position = "none",
            axis.title.y=element_blank(),
            panel.grid.minor = element_blank())+
      labs(title=type,x="Year")+
      scale_x_continuous(breaks = seq(2015,2024,3))+
      scale_color_manual(values = c("black","blue","turquoise3","green3","red","orange"))
  } 
    
  tiff(paste0("NPP_ACO2.tiff"),width=2800,height=1800, res = 300)
  p.all = ggarrange(p[[3]],p[[2]],p[[4]],p[[6]],p[[5]],p[[1]],labels = c("a", "b", "c","d","e","f"),
                    label.x = 0.17,label.y = 0.89,
                    ncol = 3, nrow = 2)
  annotate_figure(p.all,left = text_grob(expression(paste("NPP(gC/",m^2,"/yr)")), rot = 90))
  dev.off()
  
  