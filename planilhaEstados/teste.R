dados <- read.csv(file=
"D:/Documentos (D)/UFJF/6-PERIODO/TCCI/DADOS IBGE/ProjIBGE/planilhaEstados/Dados_Estados_Brasileiros.csv")

par(ps=20)
plot(dados[,-c(1,3)],cex.axis=0.5)
boxplot(dados$MortalidadeInfantil/100)