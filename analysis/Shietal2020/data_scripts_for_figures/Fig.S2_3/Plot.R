  #Fig.S2-3
  for (vari in c("MR","TLAI")){
  
    l = list.files("Data/")
    #eco2
    l_fpsn1 = grep(vari,l);l_fpsn2 = grep("eco2",l)
    FPSN.eco2 = l[intersect(l_fpsn1,l_fpsn2)]#eco2
    p=list()
    
    for(i in 1:2){#plant functional type
      fl=read.table(paste0("Data/",FPSN.eco2[i]))
      colnames(fl)=c("year","TAMB","T0.00","T2.25","T4.50","T6.75","T9.00")
      fl.m=melt(fl,id.vars = "year")
      line=c(rep("s",11),rep("d",11*5))
      fl.m=cbind(fl.m,line)
  
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
    tiff(paste0(vari,"_eco2.tiff"),width=1800,height=900, res = 300)
    p.all = ggarrange(plotlist = p,ncol = 2, nrow = 1)
    annotate_figure(p.all,left = text_grob(vari, rot = 90))
    dev.off()
    
    #aco2
    l_fpsn1 = grep(vari,l);l_fpsn2 = grep("aco2",l)
    FPSN.eco2 = l[l_fpsn1 [! l_fpsn1 %in% l_fpsn2]]
    p=list()
    
    for(i in 1:2){
      fl=read.table(paste0("Data/",FPSN.eco2[i]))
      colnames(fl)=c("year","TAMB","T0.00","T2.25","T4.50","T6.75","T9.00")
      fl.m=melt(fl,id.vars = "year")
        
      p[[i]]=ggplot(fl.m,aes(x=year,y=value,color=variable))+
        geom_line() + theme_bw()+  
        theme(panel.grid.major = element_blank(),
              axis.text = element_text(size=12),
              legend.position = "none",
              axis.title.y=element_blank(),
              panel.grid.minor = element_blank())+
        labs(title=type,x="Year")+
        scale_color_manual(values = c("black","blue","turquoise3","green3","red","orange"))+
        scale_x_continuous(breaks = seq(2015,2024,3))
    }
    tiff(paste0(vari,"_aco2.tiff"),width=1800,height=900, res = 300)
    p.all = ggarrange(plotlist = p,ncol = 2, nrow = 1)
    annotate_figure(p.all,left = text_grob(vari, rot = 90))
    dev.off()
  } 