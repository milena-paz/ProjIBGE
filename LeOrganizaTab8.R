##lendo o arquivo, pulando as linhas com NAs (as que indicam as classes)
hab.ate5k <- readxl::read_xls("Brasil/tab8.xls",range="A12:G1312",col_names = F)
hab.5k_10k <- readxl::read_xls("Brasil/tab8.xls",range="A1314:G2525",col_names = F)
hab.10k_20k <- readxl::read_xls("Brasil/tab8.xls",range="A2527:G3927",col_names = F)
hab.20k_50k <- readxl::read_xls("Brasil/tab8.xls",range="A3929:G4971",col_names = F)
hab.50k_100k <- readxl::read_xls("Brasil/tab8.xls",range="A4973:G5297",col_names = F)
hab.100k_500k <- readxl::read_xls("Brasil/tab8.xls",range="A5299:G5543",col_names = F)
hab.acimaDe500k <- readxl::read_xls("Brasil/tab8.xls",range="A5545:G5582",col_names = F)
##
##juntando as subtabelas numa lista
tabelas<-mget( ls(pattern="hab.") )
##
##adiciona um fator (das classes de tamanho da populacao) às tabelas
classes<-c("De 100 a 500 mil","De 10 a 20 mil","De 20 a 50 mil",
           "De 50 a 100 mil","De 5 a 10 mil","Acima de 500 mil",
           "Até 5 mil")
for(i in 1:7){
  tabelas[[i]][1:nrow(tabelas[[i]]), 8] <- as.factor(classes[i])
}
##juntando todas as tabelas em uma so
Tab<-NULL
for(i in c(7,5,2,3,4,1,6)){
  Tab <- rbind(Tab,tabelas[[i]])
}
##
##dando nomes às colunas
colnames(Tab) <- c("Codigo", "UF", "Cidade", "Val.Medio", "Q1", "Mediana", "Q3","TamanhoPop")
##Convertendo Unidade Federativa em fator
Tab$UF <- as.factor(Tab$UF)
write.csv(Tab,file="Tabela8.csv",row.names = F)