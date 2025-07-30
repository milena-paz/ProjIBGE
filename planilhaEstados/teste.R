dados <- read.csv(file=
"D:/Documentos (D)/UFJF/6-PERIODO/TCCI/DADOS IBGE/ProjIBGE/planilhaEstados/Dados_Estados_Brasileiros.csv")

png(filename=file.path(getwd(),"dispersao.png"),width=1200,height=1200,pointsize=30)
plot(dados[,-c(1,3)],cex.axis=0.5)
dev.off()
