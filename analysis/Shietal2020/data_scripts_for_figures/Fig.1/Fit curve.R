  #Fig.1 fit sigmoid curve for water content

  #read in data
  r = read.csv("Sphagnum water content.csv")
  df = data.frame(soil.water=r[,3],surface.water=r[,5])
  df = df[complete.cases(df),]
  
  #sigmoid function
  sigmoidal = function(x){
    0.3933+7.6227/(1+exp(-(x-0.1571)/0.018))
  }
  
  #plot
  tiff("Sigmoid.tiff",width=1300,height=1000, res = 240)
  ggplot(r,aes(x=soil.water,y=surface.water))+geom_point()+theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    xlab("Soil water content at 10cm (cm3/cm3)")+
    ylab("water content of surface spagnum (g h2o/g tissue)")+
    stat_function(fun = sigmoidal,col="blue")+
    annotate("text", x = 0.0131, y = 8.1, label = "Sigmoid")+
    annotate("text", x = 0.1, y = 7.5, label = "y=0.3933+7.6227/(1+exp(-(x-0.1571)/0.018))")
  dev.off()
  
 