  
 #Fig.2 Fraction of variance
 #Each column is order by value in the figure. This is an illustration of the first column: GPP

  library("ggplot2")
  mycolors = c("black",'gray35','gray60',"coral4",'brown3','red','darkorange3','orange','yellow','bisque',
    'darkblue','blue','#3288BD','cyan3','cyan','darkgreen','green3','#99D6A4','green','purple4','purple',
    'hotpink','violetred3','violetred1','plum1')
  
  #read in data
  dat = read.csv("FIg2_SPRUCEsens_R.csv")
  ordera = order(dat$GPP,decreasing = T)
  dat.order = dat[ordera,]
  dat.order$id=25:1
  df.m=reshape2::melt(dat.order[,-c(1,10)],id.vars="id")
  
  tiff(paste0("Fig2_GPP.tiff"),width=2700,height = 1300, res = 300)
 
  ggplot(df.m,aes(x=variable,y=value,fill=factor(id)))+
    geom_col() + theme_bw() + 
    scale_fill_manual(values = mycolors[match(rev(dat.order$id.raw),rev(dat.order$id))]) +
    theme(panel.grid.major = element_blank(),
          axis.title.x = element_blank(),
          axis.text = element_text(size=12),
          panel.grid.minor = element_blank()) 
    
  dev.off() 