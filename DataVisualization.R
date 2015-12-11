
# -- --------------------------------------------------------------------------------- #
# -- Initial Developer: FranciscoME -------------------------------------------------- #
# -- GitHub Repossitory: https://github.com/IFFranciscoME/DataVisualization ---------- #
# -- License: GNU General Public License --------------------------------------------- #
# -- --------------------------------------------------------------------------------- #

# -- --------------------------------------------------------------------------------- #
# -- Yearly TimeSeries + 4 Vertical Lines + 4 Dots ----------------------------------- #
# -- --------------------------------------------------------------------------------- #

ggYT4V4P <- function(DatosYT4V4Pxts, FechasYT4V4P, ColorSerie, ColorTrim,
                     TextSize, LineSize, PointSize)
{
  DatosYT4V4P <<- DatosYT4V4Pxts
  DatosYT4V4P[,1] <<- as.POSIXct.Date(DatosYT4V4P[,1])
  YT4V4P1 <- ggplot(DatosYT4V4P,  aes(DatosYT4V4P[,1])) + 
    geom_line(aes(y = DatosYT4V4P[,2]), colour = ColorSerie, size = LineSize)       +
      labs(title = NULL, x = NULL, y = NULL)                                     + 
      theme(panel.background = element_rect(fill="white"),
      panel.grid.minor.y = element_line(size = .25, color = "dark grey"),
      panel.grid.major.y = element_line(size = .25, color = "dark grey"),
      panel.grid.major.x = NULL,
      panel.grid.major.x = element_line(size = .75, color = "black"),
      axis.text.x =element_text(colour = "black",size = TextSize, hjust =.5,vjust = 0),
      axis.text.y =element_text(colour = "black",size = TextSize, hjust =.5,vjust = 0),
      axis.title.x=element_text(colour = "black",size = TextSize, hjust =.5,vjust = 0),
      axis.title.y=element_text(colour = "black",size = TextSize, hjust =.5,vjust = 1),
      panel.border = element_rect(linetype = 1, colour = "dark grey", fill = NA))      +
  
  scale_x_datetime(breaks = FechasYT4V4P,labels = date_format("%d/%m/%y"))             + 
  
  scale_y_continuous(breaks = round(seq(
      round(min(DatosYT4V4Pxts[,2]),6),
      round(max(DatosYT4V4Pxts[,2]),6),
      (round(max(DatosYT4V4Pxts[,2]),6) - round(min(DatosYT4V4Pxts[,2]),6))/10),2),
      labels = comma) 

  Numeros <<- c(which(DatosYT4V4Pxts[,1] == as.Date(FechasYT4V4P[1])),
                which(DatosYT4V4Pxts[,1] == as.Date(FechasYT4V4P[2])),
                which(DatosYT4V4Pxts[,1] == as.Date(FechasYT4V4P[3])),
                which(DatosYT4V4Pxts[,1] == as.Date(FechasYT4V4P[4])),
                which(DatosYT4V4Pxts[,1] == as.Date(FechasYT4V4P[5])),
                which(DatosYT4V4Pxts[,1] == as.Date(FechasYT4V4P[6])),
                which(DatosYT4V4Pxts[,1] == as.Date(FechasYT4V4P[7])))
  
  YT4V4P <- YT4V4P1 +  
  geom_segment(x = as.numeric(FechasYT4V4P[1]), xend = as.numeric(FechasYT4V4P[1]),
  y = 0, yend = DatosYT4V4Pxts[which(DatosYT4V4Pxts[,1] == as.Date(FechasYT4V4P[1])),2],
  linetype= 5, size=.4, colour=ColorTrim) +
    
  geom_segment(x = as.numeric(FechasYT4V4P[2]), xend = as.numeric(FechasYT4V4P[2]),
  y = 0, yend = DatosYT4V4Pxts[which(DatosYT4V4Pxts[,1] == as.Date(FechasYT4V4P[2])),2],
  linetype= 5, size=.4, colour=ColorTrim) +
    
  geom_segment(x = as.numeric(FechasYT4V4P[3]), xend = as.numeric(FechasYT4V4P[3]),
  y = 0, yend = DatosYT4V4Pxts[which(DatosYT4V4Pxts[,1] == as.Date(FechasYT4V4P[3])),2],
  linetype= 5, size=.4, colour=ColorTrim) +
  
  geom_segment(x = as.numeric(FechasYT4V4P[4]), xend = as.numeric(FechasYT4V4P[4]),
  y = 0, yend = DatosYT4V4Pxts[which(DatosYT4V4Pxts[,1] == as.Date(FechasYT4V4P[4])),2],
  linetype= 5, size=.4, colour=ColorTrim) +
    
  geom_segment(x = as.numeric(FechasYT4V4P[5]), xend = as.numeric(FechasYT4V4P[5]),
  y = 0, yend = DatosYT4V4Pxts[which(DatosYT4V4Pxts[,1] == as.Date(FechasYT4V4P[5])),2],
  linetype= 5, size=.4, colour=ColorTrim) +
  
  geom_segment(x = as.numeric(FechasYT4V4P[6]), xend = as.numeric(FechasYT4V4P[6]),
  y = 0, yend = DatosYT4V4Pxts[which(DatosYT4V4Pxts[,1] == as.Date(FechasYT4V4P[6])),2],
  linetype= 5, size=.4, colour=ColorTrim) +
    
  geom_segment(x = as.numeric(FechasYT4V4P[7]), xend = as.numeric(FechasYT4V4P[7]),
  y = 0, yend = DatosYT4V4Pxts[which(DatosYT4V4Pxts[,1] == as.Date(FechasYT4V4P[7])),2],
  linetype= 5, size=.4, colour=ColorTrim) +

    geom_point(aes(x = DatosYT4V4P[Numeros[1],1], y=DatosYT4V4P[Numeros[1],2] ),
               size=PointSize, colour="red")    + 
    geom_point(aes(x = DatosYT4V4P[Numeros[2],1], y=DatosYT4V4P[Numeros[2],2] ),
               size=PointSize, colour="red")    +  
    geom_point(aes(x = DatosYT4V4P[Numeros[3],1], y=DatosYT4V4P[Numeros[3],2] ),
               size=PointSize, colour="red")    + 
    geom_point(aes(x = DatosYT4V4P[Numeros[4],1], y=DatosYT4V4P[Numeros[4],2] ),
               size=PointSize, colour="red")    +
    geom_point(aes(x = DatosYT4V4P[Numeros[5],1], y=DatosYT4V4P[Numeros[5],2] ),
               size=PointSize, colour="red")    + 
    geom_point(aes(x = DatosYT4V4P[Numeros[6],1], y=DatosYT4V4P[Numeros[6],2] ),
               size=PointSize, colour="red")    +
    geom_point(aes(x = DatosYT4V4P[Numeros[7],1], y=DatosYT4V4P[Numeros[7],2] ),
               size=PointSize, colour="red")    +
    geom_point(aes(x = DatosYT4V4P[Numeros[1],1], y=DatosYT4V4P[Numeros[1],2] ),
               size=PointSize, colour="white")  + 
    geom_point(aes(x = DatosYT4V4P[Numeros[2],1], y=DatosYT4V4P[Numeros[2],2] ),
               size=PointSize, colour="white")  +  
    geom_point(aes(x = DatosYT4V4P[Numeros[3],1], y=DatosYT4V4P[Numeros[3],2] ),
               size=PointSize, colour="white")  + 
    geom_point(aes(x = DatosYT4V4P[Numeros[4],1], y=DatosYT4V4P[Numeros[4],2] ),
               size=PointSize, colour="white")  +
    geom_point(aes(x = DatosYT4V4P[Numeros[5],1], y=DatosYT4V4P[Numeros[5],2] ),
               size=PointSize, colour="white")  + 
    geom_point(aes(x = DatosYT4V4P[Numeros[6],1], y=DatosYT4V4P[Numeros[6],2] ),
               size=PointSize, colour="white")  +
    geom_point(aes(x = DatosYT4V4P[Numeros[7],1], y=DatosYT4V4P[Numeros[7],2] ),
               size=PointSize, colour="white")
return(YT4V4P)
}

# -- --------------------------------------------------------------------------------- #
# -- Forecast Single TimeSeries with Standard errors --------------------------------- #
# -- --------------------------------------------------------------------------------- #

ggForecastSerie  <- function(activoP, titulo)
{
  activoP  <<- PredTable
  ggForec1 <<- ggplot(activoP,  aes(activoP[,2])) + 
  geom_line(aes(y = activoP[,3]), colour = "red", size = .75)                   + 
  geom_line(aes(y = activoP[,3] + 3*activoP[,4]), colour = "dark red",
  size = .5, linetype = "longdash")                                             +
  geom_line(aes(y = activoP[,3] - 3*activoP[,4]), colour = "dark red",
  size = .5, linetype = "longdash")                                             +
  labs(title = "pred", x = "TimeStamp", y = "Value")
  
  ggForec2 <- ggForec1 + theme(panel.background = element_rect(fill="white"),
  panel.grid.minor.y = element_line(size = .25, color = "dark grey"),
  panel.grid.major.y = element_line(size = .25, color = "dark grey"),
  panel.grid.major.x = element_line(size = .25, color = "dark grey"),
  panel.grid.major.x = element_line(size = .25, color = "dark grey"),
  axis.text.x =element_text(colour = "black",size = 10, hjust =.5,vjust = 0),
  axis.text.y =element_text(colour = "black",size = 10, hjust =.5,vjust = 0),
  axis.title.x=element_text(colour = "black",size = 12, hjust =.5,vjust = 0),
  axis.title.y=element_text(colour = "black",size = 12, hjust =.5,vjust = 1),
  title = element_text(colour = "black", size = 14, hjust = 1, vjust = 0.8), 
  panel.border = element_rect(linetype = 1, colour = "dark grey", fill = NA))   +
  scale_y_continuous(breaks = round(seq(
  (min(activoP[,3] - 3*activoP[,4])),
  (max(activoP[,3] + 3*activoP[,4])),
  (max(activoP[,3] + 3*activoP[,4]) - min(activoP[,3] - 3*activoP[,4]))/4),4)) +  
  scale_x_datetime(labels = date_format("%d/%m %H:%M")) + 
  geom_vline(xintercept = as.numeric(activoP$TimeStamp[1]), 
  color = "red", size = 1, linetype="longdash", alpha = .5)
return(ggForec2)
}

# -- --------------------------------------------------------------------------------- #
# -- FAC and FACP Correlogram -------------------------------------------------------- #
# -- --------------------------------------------------------------------------------- #

ggACF <- function(x,LagMax, Type, Yaxis, Xaxis)
{
  x <<- x
  ciline <<- 2/sqrt(length(x))
  bacf   <<- acf(x, plot = FALSE, lag.max = LagMax, type = Type)
  bacfdf <<- with(bacf, data.frame(lag, acf))
  bacfdf <<- bacfdf[-seq(1, 0),]
  Sig_nc <<- (abs(bacfdf[,2]) > abs(ciline))^2
  bacfdf <<- cbind(bacfdf, Sig_nc)
  fac_gg <<- qplot(lag,acf,data=bacfdf,geom="bar",width=.75, stat = "identity",
  position = "identity", ylab = Yaxis, xlab = Xaxis,  fill = factor(Sig_nc))   +
  geom_hline(yintercept = -ciline, color = "dark grey", size = 0.35,
  linetype = "dashed")                                                     +
  geom_hline(yintercept = ciline,color = "dark grey", size = 0.35,
  linetype = "dashed")                                                     +
  geom_hline(yintercept = 0, color = "dark grey",size = 0.25)              +
  scale_fill_hue(breaks = 0:1, labels=c("No Significative"," Significative"),
                 h=c(260, 235))                                            +
  scale_x_continuous(breaks = c(2,4,6,8,10,12,14,16,18,20,22,24,26,28)) +
  theme(panel.background = element_rect(size = .75, colour = "dark grey",
  fill = "white"), 
  axis.title.x = element_text(size = 8.5),
  axis.title.y = element_text(size = 8.5),
  plot.title = element_text(size = 5,vjust = 1),
  legend.position = "bottom", legend.title = element_blank(),
  legend.text = element_text(colour="blue",size = 8))
return(fac_gg)
}

# -- --------------------------------------------------------------------------------- #
# -- Single TimeSeries Plot ---------------------------------------------------------- #
# -- --------------------------------------------------------------------------------- #

ggSerie  <- function(activo, titulo, TimeBreak,LSize,TSize)
{
  activo <<- activo
  gg_sp  <- ggplot(activo,  aes(activo[,1], activo[,2]))
  gg_sp1 <- gg_sp + geom_line(colour = "steel blue", size = LSize)                     + 
  labs(title = titulo, x = "Dato", y = "Valor")

  gg_ser <- gg_sp1 + theme(panel.background = element_rect(fill="white"),
  panel.grid.minor.y = element_line(size = .25, color = "dark grey"),
  panel.grid.major.y = element_line(size = .25, color = "dark grey"),
  panel.grid.major.x = element_line(size = .25, color = "dark grey"),
  panel.grid.major.x = element_line(size = .25, color = "dark grey"),
  axis.text.x =element_text(colour = "black",size = TSize, hjust =.5,vjust = 0),
  axis.text.y =element_text(colour = "black",size = TSize, hjust =.5,vjust = 0),
  axis.title.x=element_text(colour = "black",size = TSize+2, hjust =.5,vjust = 0),
  axis.title.y=element_text(colour = "black",size = TSize+2, hjust =.5,vjust = 1),
  title = element_text(colour = "black", size = TSize+4, hjust = 1, vjust = 0.8), 
  panel.border = element_rect(linetype = 1, colour = "dark grey", fill = NA))        +
  scale_y_continuous(breaks = seq(round(min(activo[,2]),4),
  round(max(activo[,2]),4), (round(max(activo[,2]),4)-round(min(activo[,2]),4))/10)) +  
  scale_x_datetime(labels = date_format("%d/%m/%y %H:%M"), breaks = TimeBreak)
return(gg_ser)
}

# -- --------------------------------------------------------------------------------- #
# -- Single TimeSeries Plot + Vertical Signals --------------------------------------- #
# -- --------------------------------------------------------------------------------- #

ggSerieM1 <- function(DataIN,Color1,Color2,ValueSerieM1,maintitle,datebreaks,signalcolor1,signalcolor2,LSize,TSize)
{
  DataSerieM1  <<- DataIN
  ValueSerieM1 <<- ValueSerieM1
  gg_sp  <- ggplot(DataSerieM1,  aes(DataSerieM1$TimeStamp))
  gg_sp1 <- gg_sp + geom_line(colour = Color1, size = LSize, aes(y = DataSerieM1[,ValueSerieM1]))

  VisualSell <- as.numeric(DataSerieM1$TimeStamp[which(DataSerieM1$Signal == -1)])
  VisualBuy  <- as.numeric(DataSerieM1$TimeStamp[which(DataSerieM1$Signal == +1)])

  gg_ser <- gg_sp1 + theme(panel.background = element_rect(fill="white"),
  panel.grid.minor.y = NULL,
  panel.grid.major.y = element_line(size = .35, color = Color2, linetype = 2),
  panel.grid.minor.x = NULL,
  panel.grid.major.x = element_line(size = .35, color = Color2, linetype = 2),
  axis.text.x =element_text(colour = Color2,size = TSize, hjust =.5,vjust = 0),
  axis.text.y =element_text(colour = Color2,size = TSize, hjust =.5,vjust = 0),
  axis.title.x=element_text(colour = Color2,size = TSize+2, hjust =.5,vjust = 0),
  axis.title.y=element_text(colour = Color2,size = TSize, hjust =.5,vjust = 1),
  title = element_text(colour = Color1, size = TSize+4, hjust = 1, vjust = 0.8), 
  panel.border = element_rect(linetype = 1, colour = Color2, fill = NA))                   +
  labs(title = maintitle, x = NULL, y = NULL)                                              +
  scale_y_continuous(breaks = round(seq(
  round(min(DataSerieM1[,ValueSerieM1]),6),
  round(max(DataSerieM1[,ValueSerieM1]),6),
  (round(max(DataSerieM1[,ValueSerieM1]),6) -  round(min(DataSerieM1[,ValueSerieM1]),6))/10
  ),2),labels = comma)  +
  scale_x_datetime(breaks = datebreaks,labels = date_format("%d/%m/%y %H:%M"))             +
  geom_vline(xintercept=VisualSell, linetype = 5,size=.5,colour=signalcolor1,alpha = 0.8)  +
  geom_vline(xintercept=VisualBuy, linetype = 5,size=.5,colour=signalcolor2,alpha = 0.8)
return(gg_ser)
}

# -- --------------------------------------------------------------------------------- #
# -- Trading Singal Time Series ------------------------------------------------------ #
# -- --------------------------------------------------------------------------------- #

ggTradingSignal <- function(DataIN,Color1,Color2,Value,ColName,datebreaks,signalcolor1,signalcolor2,TSize)
{
  DataTrading   <<- data.frame(DataIN)
  ValueTrading  <<- Value
  gg_sp  <- ggplot(DataTrading,  aes(DataTrading$TimeStamp))
  gg_sp1 <- gg_sp + geom_line(colour = Color1, size = .75, aes(y = DataTrading[,ValueTrading]))
  
  yminTrading <- round(min(DataTrading[,ValueTrading])*0.90,7)
  ymaxTrading <- round(max(DataTrading[,ValueTrading]),7)
  ynumTrading <- (ymaxTrading-yminTrading)/8
  
  VisualSell <- as.numeric(DataTrading$TimeStamp[which(DataTrading$Signal == -1)])
  VisualBuy  <- as.numeric(DataTrading$TimeStamp[which(DataTrading$Signal == +1)])

  gg_ser1 <- gg_sp1 + theme(panel.background = element_rect(fill="white"),
  panel.grid.minor.y = NULL,
  panel.grid.major.y = element_line(size = .35, color = Color2, linetype = 2),
  panel.grid.major.x = element_line(size = .35, color = Color2, linetype = 2),
  panel.grid.minor.x = NULL,
  axis.text.x =element_text(colour = Color2,size = TSize, hjust =.5,vjust = 0),
  axis.text.y =element_text(colour = Color2,size = TSize, hjust =.5,vjust = 0),
  axis.title.x=element_text(colour = Color2,size = TSize+2, hjust =.5,vjust = 0),
  axis.title.y=element_text(colour = Color2,size = TSize+2, hjust =.5,vjust = 1),
  title = element_text(colour = Color1, size = TSize+4, hjust = 1, vjust = 0.8), 
  panel.border = element_rect(linetype = 1, colour = Color2, fill = NA))                  +
  labs(title = paste(ColName," Indicator ValueTrading"), x = NULL, y = NULL)              +
  scale_y_continuous(breaks = round(seq(yminTrading,ymaxTrading,ynumTrading),2),labels = comma)  +
  scale_x_datetime(breaks = datebreaks,labels = date_format("%d/%m/%y %H:%M"))            +
  geom_hline(yintercept=SellSignal, linetype = 5,size=.75,colour=signalcolor1,alpha = 1)  +
  geom_hline(yintercept=BuySignal, linetype = 5,size =.75,colour=signalcolor2,alpha = 1)
return(gg_ser1)
}

# -- --------------------------------------------------------------------------------- #
# -- Equity Time Series -------------------------------------------------------------- #
# -- --------------------------------------------------------------------------------- #

ggEquity <- function(DataIN,LineSize,Color1,Color2,SizeText,ValueEquity,ColName,
                     datebreaks,signalcolor1,signalcolor2)
{
  DataEquity  <<- data.frame(DataIN)
  ValueEquity <<- ValueEquity
  gg_sp   <- ggplot(DataEquity,  aes(DataEquity$TimeStamp))
  gg_sp1  <- gg_sp + geom_line(colour = Color1, size = LineSize, aes(y = DataEquity[,ValueEquity]))

  yminEquity <- round(min(DataEquity[,ValueEquity]),2)
  ymaxEquity <- round(max(DataEquity[,ValueEquity]),2)
  ynumEquity <- (ymaxEquity-yminEquity)/10
  
  VisualSell <<- as.numeric(DataEquity$TimeStamp[which(DataEquity$Signal == -1)])
  VisualBuy  <<- as.numeric(DataEquity$TimeStamp[which(DataEquity$Signal == +1)])
 
  gg_ser2 <- gg_sp1 + theme(panel.background = element_rect(fill="white"),
  panel.grid.minor.y = element_line(size = .35, color = Color2, linetype = 2),
  panel.grid.major.y = element_line(size = .35, color = Color2, linetype = 2),
  panel.grid.major.x = element_line(size = .35, color = Color2, linetype = 2),
  panel.grid.minor.x = NULL,
  axis.text.x =element_text(colour = Color2,size = SizeText, hjust =.5,vjust = 0),
  axis.text.y =element_text(colour = Color2,size = SizeText, hjust =.5,vjust = 0),
  axis.title.x=element_text(colour = Color2,size = SizeText+2, hjust =.5,vjust = 0),
  axis.title.y=element_text(colour = Color2,size = SizeText+2, hjust =.5,vjust = 1),
  title = element_text(colour = Color1, size = SizeText+4, hjust = 1, vjust = 0.8), 
  panel.border = element_rect(linetype = 1, colour = Color2, fill = NA))                   +
  labs(title = NULL, x = NULL, y = NULL)                                +
  scale_y_continuous(breaks= round(seq(yminEquity,ymaxEquity,ynumEquity),2)) +
  scale_x_datetime(breaks = datebreaks,labels = date_format("%d/%m/%y %H:%M"))             +
  geom_vline(xintercept=VisualSell, linetype = 5,size=LineSize/8,colour=signalcolor1,alpha = 0.8) +
  geom_vline(xintercept=VisualBuy, linetype = 5,size=LineSize/8,colour=signalcolor2,alpha = 0.8)
return(gg_ser2)
}

# -- --------------------------------------------------------------------------------- #
# -- 4 time series plot -------------------------------------------------------------- #
# -- --------------------------------------------------------------------------------- #

ggSerieM4 <- function(Assets)
{
 MultiData <- data.frame(Assets[,1],
 Assets[,2]/max(Assets[,2]),Assets[,3]/max(Assets[,3]),
 Assets[,4]/max(Assets[,4]),Assets[,5]/max(Assets[,5]),Assets[,6]/max(Assets[,6]))

 colnames(MultiData) <<- colnames(Assets)
 MultiData <<- melt(MultiData, id = "TimeStamp", 
 variable.name = "Assets", value.name = "NormalizedPrice")

ggsm  <- ggplot(MultiData, aes(x=TimeStamp,y=NormalizedPrice),group=Assets)    +
geom_line(aes(colour = Assets), linetype = "longdash", size = 1.5)             +
labs(title = "Stocks", x = "TimeStamp", y = "Normalized Price")                +
scale_color_manual(values=c("blue","dark grey", "dark blue"))

y_min <- round(min(MultiData[,3]),1)
y_max <- round(max(MultiData[,3]),1)
y_num <- (y_max-y_min)/10
   
ggsm1 <<- ggsm + theme(panel.background = element_rect(fill="white"),
panel.grid.minor.y = element_line(size = .25, color = "dark grey", linetype = "solid"),
panel.grid.major.y = element_line(size = .25, color = "dark grey", linetype = "solid"),
panel.grid.major.x = element_line(size = .25, color = "dark grey", linetype = "dashed"),
panel.grid.minor.x = element_line(size = .25, color = "dark grey", linetype = "dashed"),
axis.text.x  = element_text(colour = "black",size = 12, hjust =.5,vjust = 0),
axis.text.y  = element_text(colour = "black",size = 12, hjust =.5,vjust = 0),
axis.title.x = element_text(colour = "black",size = 16,hjust  =.5,vjust = 0),
axis.title.y = element_text(colour = "black",size = 16,hjust  =.5,vjust = 1),
title = element_text(colour = "blue", size = 10, hjust = 1, vjust = 0.8), 
legend.position = "right", legend.margin = unit(.25, "cm"), 
legend.background = element_rect(colour = "white", fill = NA),
legend.text  = element_text(size = 11.5), 
panel.border = element_rect(linetype = 1, colour = "white", fill = NA),
legend.key   = element_rect(colour = "white", fill = "white", size = 0.5),
legend.title = element_blank())  + 
scale_x_datetime(breaks = datebreaks,labels = date_format("%d/%m/%y %H:%M")) +
scale_y_continuous(breaks = seq(y_min, y_max, y_num),labels = comma)
return(ggsm1)
}

# -- --------------------------------------------------------------------------------- #
# -- OrderBook plot ------------------------------------------------------------------ #
# -- --------------------------------------------------------------------------------- #

ggOrderBookPlot <- function(Data, Color1, Color2)
{
OBData <- data.frame(Data)
OBPlot <- ggplot(OBData, aes(x = factor(OBData$Price),
y =  factor(OBData$Amount)), group=Side) + geom_bar(aes(fill = Side),
stat="identity") + scale_fill_manual(values = c(Color1,Color2)) +
labs(title = NULL, x = "Price", y = "Amount(BTC)") +
theme(panel.background = element_rect(fill="white"),
panel.grid.minor.y = element_line(size = .25, color = "dark grey", linetype = "solid"),
panel.grid.major.y = element_line(size = .25, color = "dark grey", linetype = "solid"),
panel.grid.major.x = element_line(size = .25, color = "dark grey", linetype = "dashed"),
panel.grid.minor.x = element_line(size = .25, color = "dark grey", linetype = "dashed"),
axis.text.x  = element_text(colour = "black",size = 12, hjust =.5,vjust = 0),
axis.text.y  = element_text(colour = "black",size = 12, hjust =.5,vjust = 0),
axis.title.x = element_text(colour = "black",size = 16,hjust  =.5,vjust = 0),
axis.title.y = element_text(colour = "black",size = 16,hjust  =.5,vjust = 1),
title = element_text(colour = "blue", size = 10, hjust = 1, vjust = 0.8), 
legend.position = "right", legend.margin = unit(.25, "cm"), 
legend.background = element_rect(colour = "white", fill = NA),
legend.text  = element_text(size = 11.5), 
panel.border = element_rect(linetype = 1, colour = "dark grey", fill = NA),
legend.key   = element_rect(colour = "white", fill = "white", size = 0.5),
legend.title = element_blank())
return(OBPlot)
}
