
### Modelo Arcelus 


shift.arcelus <- function(region_t, region_t1, nation_t, nation_t1) {
  
  industries <- length(region_t)
  
  sum_region_t <- sum(region_t)
  sum_region_t1 <- sum(region_t1)
  sum_nation_t <- sum(nation_t)
  sum_nation_t1 <- sum(nation_t1)
  # National Growth Effect on industry i in the rth region between (t-1) and t.
  
  i <- 0
  # novos componentes de taxas de crescimento e homotetico
  h <- vector()
  G <- vector()
  g <- vector()
  gi <- vector()
  Gi <- vector()
  
  
  # NS_tir <- vector()
  NSE <- vector()
  NSD <- vector()
  
  # IM_tir <- vector()
  IME <- vector()
  IMD <- vector()
  
  # RS_tir <- vector()
  RGE <- vector()
  RGD <- vector()
  RIE <- vector()
  RID <- vector()
  
  VarTot <- vector()
  conferencia <- vector()
  
  for (i in 1:industries) {
    h[i] <- region_t[i] * (nation_t[i]/sum_nation_t)
    
    G[i] <- (sum_nation_t1 - sum_nation_t)/sum_nation_t
    
    g[i] <- (sum_region_t1 - sum_region_t)/sum_region_t
    
    gi[i] <- (region_t1[i] - region_t[i])/region_t[i]
    
    Gi[i] <- (nation_t1[i] - nation_t[i])/nation_t[i]
    
    # componentes
    
    NSE[i] <- h[i] * G[i]
    
    NSD[i] <- (region_t[i] - h[i]) * G[i]
    
    IME[i] <- h[i] * (Gi[i] - G[i])
    
    IMD[i] <- (region_t[i] - h[i]) * (Gi[i] - G[i])
    
    
    RGE[i] <- h[i] * (g[i] - G[i])
    
    RGD[i] <- (region_t[i] - h[i]) * (g[i] - G[i])
    
    RIE[i] <- h[i] * (gi[i] - g[i] - Gi[i] + G[i])
    
    RID[i] <- (region_t[i] - h[i]) * (gi[i] - g[i] - Gi[i] + G[i])
    
    VarTot[i] <- NSE[i] + NSD[i] + IME[i] + IMD[i] + RGE[i] + RGD[i] + RIE[i] + 
      RID[i]
    
    conferencia[i] <- region_t1[i] - region_t[i]
    
  }
  shifts <- list(NSE = NSE, NSD = NSD, IME = IME, IMD = IMD, RGE = RGE, RGD = RGD, 
                 RIE = RIE, RID = RID, VarTot = VarTot, conferencia = conferencia)
  
  return(shifts)
}


## 
options(max.print = 1000, digits = 2, scipen = 100)
resultado.arcelus <- shift.arcelus(impera$IMP_2006, impera$IMP_2017, impera$BR_2006, 
                                   impera$BR_2017)


tabela.arcelus <- cbind(impera$industry, as.data.frame(resultado.arcelus))

print(tabela.arcelus)


somaNSa <- tabela.arcelus$NSE + tabela.arcelus$NSD
somaIMa <- tabela.arcelus$IME + tabela.arcelus$IMD
somaRSa <- tabela.arcelus$RGE + tabela.arcelus$RGD + tabela.arcelus$RIE + tabela.arcelus$RID
comp.classic <- cbind(somaNSa, somaIMa, somaRSa)
print(comp.classic)

tabela.arcelus$conferencia<-NULL

write.csv2(tabela.arcelus, "tabela.arcelus2.csv")

getwd()

portfolio(impera$IMP_2006, impera$IMP_2017,
          impera$BR_2006, impera$BR_2017,
          industry.names = impera$industry,
          impera$BR_2017, psize.factor = 30,
          pmx = "Imperatriz", pmy = "Brasil",
          pmtitle = "Brasil e Imperatriz", 
          leg = TRUE, leg.fsize = 0.5, bgrid = TRUE, leg.y = -0.10)

library(REAT)

shift(impera$IMP_2006, impera$IMP_2017, impera$BR_2006, impera$BR_2017, industry.names = impera$industry,shift.method = "Dunn",
      print.results = TRUE, plot.results = FALSE, plot.colours = NULL,
      plot.title = NULL, plot.portfolio = FALSE)

shift.growth(impera$IMP_2006, impera$IMP_2017, impera$BR_2006, impera$BR_2017, time.periods = NULL,
             industry.names = impera$industry)


RESULTA<-shifti(impera$IMP_2006, impera$IMP_2017, impera$BR_2006, impera$BR_2017, industry.names = impera$industry,
                shift.method = "Dunn", print.results = TRUE,
                plot.results = FALSE,plot.colours = NULL,
                plot.title = NULL, plot.portfolio = FALSE)

