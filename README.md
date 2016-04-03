# DataVisualization
Data Visualization for Dinamic Report

```{r}
library(reshape2)
library(ggplot2)
library(quantmod)

IniDate <- "2016-01-01"
EndDate <- "2016-03-31"

Activos <- c("AC.MX","ALFAA.MX","ALPEKA.MX","ALSEA.MX","AMXL.MX","ASURB.MX","BIMBOA.MX",
             "BOLSAA.MX","CEMEXCPO.MX","COMERCIUBC.MX","ELEKTRA.MX","GAPB.MX",
             "GENTERA.MX","GFINBURO.MX","GFNORTEO.MX","GFREGIOO.MX","GMEXICOB.MX",
             "GRUMAB.MX","GSANBORB-1.MX","ICA.MX","ICHB.MX","IENOVA.MX","KIMBERA.MX",
             "KOFL.MX","LABB.MX","LALAB.MX","LIVEPOLC-1.MX","MEXCHEM.MX","OHLMEX.MX",
             "PINFRA.MX","SANMEXB.MX","TLEVISACPO.MX","WALMEX.MX")

getSymbols.yahoo(Symbols=Activos, env=.GlobalEnv, from=IniDate, to=EndDate)

Tickers <- c("AC","ALFA.A","ALPEK.A","ALSEA","AMX.L","ASUR.B","BIMBO.A",
             "BOLSA.A","CEMEX.CPO","COMERCI.UBC","ELEKTRA","GAP.B",
             "GENTERA","GFINBUR.O","GFNORTE.O","GFREGIO.O","GMEXICO.B",
             "GRUMA.B","GSANBOR.B-1","ICA","ICH.B","IENOVA","KIMBER.A",
             "KOFL","LAB.B","LALA.B","LIVEPOL.C-1","MEXCHEM","OHLMEX",
             "PINFRA","SANMEX.B","TLEVISA.CPO","WALMEX")

AssetAdPrices <- do.call(merge, lapply(Activos, function(x) Ad(get(x))))
AssetAdPrices <- na.omit(AssetAdPrices)
rm(list = Activos)

LogReturns <- round(diff(log(AssetAdPrices))[-1],4)
LogReturns <- na.omit(LogReturns)
colnames(LogReturns) <- Tickers

# -- --------------------------------------------------------------------------------- #

ggCorHM(Data = LogReturns, Nombres = Activos, OrdType =  "Ordenado", ColorLow = "sky blue", 
        ColorHigh = "blue", ColorMid = "white", TamTxtCor = 3, RndTxtCor = 1, 
        ColTxtCor = "black")

LogReturns <- LogReturns[,-c(seq(1,30,1))]

ggCorHM(Data = LogReturns, Nombres = Activos[-c(seq(1,30,1))], OrdType =  "Ordenado",
        ColorLow = "sky blue", ColorHigh = "blue", ColorMid = "white", TamTxtCor = 7,
        RndTxtCor = 1, ColTxtCor = "black")
```
