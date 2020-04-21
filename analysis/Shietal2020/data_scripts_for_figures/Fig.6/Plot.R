  #Fig6 relative change from control

  library("ggplot2")
  library("reshape2")

  fl=read.table("data_SPRUCEplot1.txt")
  colnames(fl)=c("year","Picea","Larix","Shrub","Sphagnum_hum","Sphagnum_hol","Water table height")
  fl$year=fl$year+2015
  fl.m=melt(fl,id.vars = "year")
      
  tiff(paste0("Fig6.tiff"),width=1400,height=900, res = 300)
  ggplot(fl.m,aes(x=year,y=value,color=variable,linetype=variable))+
        geom_line()+theme_bw()+  
        theme(panel.grid.major = element_blank(),
              axis.text = element_text(size=12),
              panel.grid.minor = element_blank())+
        labs(x="Year",y="Relative change from control")+
        scale_x_continuous(breaks = seq(2015,2024,3))+
        scale_y_continuous(breaks = seq(-1,2,0.5))+
        scale_color_manual(values=c("#F8766D","#B79F00","#00BA38","#619CFF","#619CFF","black"))+
        scale_linetype_manual(values=c("solid","solid","solid","solid","dashed","solid"))
  dev.off()
      
      
      