#title: "Explorar o pacote Caret"
#: "Lucas G Nachtigall"
#date: "09/2022"
 


rm(list=ls())
setwd("~/GitHub/R/Computação Aplicada I")
dados = read.csv("prep-dados.txt",header=T)
 
#================================================================================================================================================#
#1. Demonstre o uso de um histograma (função hist) usando a variável Occupancy com os rótulos "Ocupado" e "Não Ocupado"                          #
#================================================================================================================================================#
m<-hist(dados$Occupancy, xlab = "Estados", ylab ="Frequencia", 
        col = "darkmagenta", border = "black", xlim = c(0,1), ylim = c(0, 8000), breaks = 2)  

text(m$mids, m$counts, labels = c("Ocupado","Não Ocupado"), adj = c(0.5, -0.5))  
 
 
dados$Occupancy = factor(dados$Occupancy,
                         labels=c("Não Ocupado","Ocupado"))
 
#================================================================================================================================================#
#2.  Demonstre o uso de um Boxplot (função boxplot ou ggplot ou featurePlot) usando a variável Occupancy com os rótulos "Ocupado" e "Não Ocupado"#
#================================================================================================================================================#
library(ggplot2)
df <- data.frame(
  Estados = factor(dados$Occupancy),
  Occupancy = rnorm(8143)
)
ds <- do.call(rbind, lapply(split(df, df$Estados), function(d) {
  data.frame(mean = mean(d$Occupancy), sd = sd(d$Occupancy), Estados = d$Estados)
}))


ggplot(df) + geom_point(aes(Estados, Occupancy)) +
  geom_point(data = ds, aes(Estados, mean), colour = 'red', size = 2)

#================================================================================================================================================#
#3. Demonstre o uso do gráfico de Dispersão (função pairs ou ggpairs) para as variáveis Temperature, Humidity, Light, CO2.                       #
#================================================================================================================================================# 
library(GGally)
ggpairs(dados[2:5], mapping = aes(color=dados$Occupancy, alpha = 0.5), 
        title = "Avaliação das Variáveis ggpairs", cardinality_threshold=8143) 

#================================================================================================================================================#
#4. Demonstre o uso da Correlação (função corrplot)                                                                                              #
#================================================================================================================================================#
library(corrplot)
#summary(dados[1:7])
corrplot(cor(dados[4:7]))  
 
