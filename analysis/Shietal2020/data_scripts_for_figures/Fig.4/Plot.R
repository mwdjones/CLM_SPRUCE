  library("ggplot2")
  library("reshape2")
  require(zoo)
  library(ggpubr)
  
  p=list()
  #Sphagnum_NPP--------
  data = data.frame(read.csv("Data/Sphagnum_NPP.csv"))
  data.m = melt(data[,1:3],id.vars="year")
  data.m$UNC = c(gpp.all[,4],rep(NA,5))
  
  p[[1]] = ggplot(data.m,aes(x=as.factor(year),y=value,fill=variable))+
    geom_bar(stat="identity",position="dodge" ,alpha=0.87)+
    theme_bw()+  scale_fill_manual(values=c("gray30","indianred1"))+
    theme(panel.grid.major = element_blank(),
          axis.text = element_text(size=12),
          panel.grid.minor = element_blank())+
    geom_errorbar(aes(ymin=value-UNC, ymax=value+UNC), width=.2,position=position_dodge(.9))+
    labs(y=expression(paste("Sphagnum NPP (gC/",m^2,"/yr)")),x="Year")
  
  #TREE_AG--------
  data=data.frame(read.csv("Data/Trees_above_ground_biomass.csv"))
  data.m=melt(data[,1:3],id.vars="year")
  data.m$unc=c(data.m$uncertainty,rep(NA,4))
  
  p[[2]] = ggplot(data.m,aes(x=year,y=value,fill=variable))+
    geom_bar(stat="identity",position="dodge",alpha=0.87)+
    theme_bw()+  scale_fill_manual(values=c("gray30","indianred1"))+
    theme(panel.grid.major = element_blank(),
          axis.text = element_text(size=12),
          panel.grid.minor = element_blank())+
    labs(y=expression(paste("Trees above ground biomass (gC/",m^2,")")),x="Year")+
    geom_errorbar(aes(ymin=value-unc, ymax=value+unc), width=.2,position=position_dodge(.9))+
    scale_y_continuous(breaks = seq(0,1600,400))
  
  #SHRUB------------------
  data=data.frame(read.csv("Data/Shrub_stem_carbon.csv"))
  data.m=melt(data[,1:3],id.vars="year")
  data.m$unc=c(data$uncertainty,rep(NA,4))
  
  p[[3]] = ggplot(data.m,aes(x=year,y=value,fill=variable))+
    geom_bar(stat="identity",position="dodge" ,alpha=0.87)+
  theme_bw()+  scale_fill_manual(values=c("gray30","indianred1"))+
    theme(panel.grid.major = element_blank(),
          axis.text = element_text(size=12),
          panel.grid.minor = element_blank())+
    labs(y=expression(paste("Shrub stem carbon (gC/",m^2,")")),x="Year")+
    geom_errorbar(aes(ymin=value-unc, ymax=value+unc), width=.2,position=position_dodge(.9))

  tiff("NPP.tiff",width=2100,height=950, res = 300)
  ggarrange(plotlist=p,ncol = 3, nrow = 1)
  dev.off()
  

  
