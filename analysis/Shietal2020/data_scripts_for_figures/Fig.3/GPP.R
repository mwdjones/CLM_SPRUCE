  #Fig3. GPP comparison
  library("ggplot2")
  require(zoo)
  library("scales")
  #read in gpp data
  gpp=data.frame(read.csv("GPP.csv",sep=",",header=TRUE))
  gpp$Date=as.Date(gpp[,1]-1)#convert to Date format
  
  #10d moving window
  ap.fl=function(x) rollapply(c(rep(x[1],4),x,rep(x[365],5)),width=10,FUN=mean,na.rm=T)
  gpp[,2:5] = apply(gpp[,2:5],2,ap.fl)

  gpp.m = reshape2::melt(gpp[,c(1,5,3,2)],id.vars="Date")
  
  #shadow: uncertainty for observed.gpp
  ymin.u = c(ap.fl(gpp$Observed.GPP -gpp$Uncertainty),rep(NA,730))
  ymax.u = c(ap.fl(gpp$Observed.GPP +gpp$Uncertainty),rep(NA,730))
  
  #plot GPP
  tiff("GPP.tiff",width=1900,height=900, res = 300)
  ggplot(gpp.m,aes(x=Date,y=value,color=variable))+
    geom_line()+labs(y=expression(paste("GPP (gC/",m^2,"/day)")))+
    geom_ribbon(aes(ymin=ymin.u, ymax=ymax.u ), alpha=0.35,fill="gray50",linetype=0)+ 
    theme_bw()+  scale_color_manual(values=c("cornflowerblue","black","indianred1"))+
    theme(panel.grid.major = element_blank(),
          axis.text = element_text(size=12),
          panel.grid.minor = element_blank())+
    scale_x_date(labels=date_format("%b"),date_breaks = "1 month")+
    scale_y_continuous(limits = c(0,7))
  dev.off()
 

  #add water depth hollow
  gpp.m = reshape2::melt(gpp[,c(1,6,7)],id.vars="Date")
  tiff("WTH.tiff",width=1900,height=900, res = 300)
  ggplot(gpp.m,aes(x=Date,y=value,color=variable))+
    geom_line(linetype="dashed")+labs(y="WTH(m)")+
    theme_bw()+  scale_color_manual(values=c("indianred1","black"))+
    theme(panel.grid.major = element_blank(),
          axis.text = element_text(size=12),
          panel.grid.minor = element_blank())+
    scale_x_date(labels=date_format("%b"),date_breaks = "1 month")+
  scale_y_continuous(position = "right")
  dev.off()