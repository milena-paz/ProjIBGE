library(dplyr)

lista <- list()
#ordenando para que fique facil juntar as tabelas
for (i in 1:17){
  lista[[paste0("Tab",i)]]<- read.csv2(paste0("dados/Brasil(csv)/tab",i,".csv")) |>
    arrange(Codigo)
}

### RETIRANDO INFORMACOES REPETIDAS ----

#deixando so uma tabela com cidades, codigos e UF
for (i in (2:17)[-5]){
  lista[[i]] <- subset(lista[[i]], select= -c( 1:3, ncol(lista[[i]])) )
}
lista[[6]] <- lista[[6]][,-c(1:3,11)]

#TOTAL DE UNIDADES DOMESTICAS E DISTRIBUICAO PERCENTUAL DE 1 OU MAIS RESPONSAVEIS
lista[[5]] <- lista[[5]][,-(1:3)]
lista[[6]] <- lista[[6]][,-1]
colnames(lista[[4]])[1] <- "TotUnidDom"

#POPULACAO RESIDENTE TOTAL
lista[[2]]<- lista[[2]][,-1]


##distinguindo nomes de colunas para evitar repetição
colnames(lista[[7]])[1] <- "TotalDomPartPerm"
colnames(lista[[5]])[1] <- "Taxa15"
colnames(lista[[16]])[3:4] <- c("Indq_2000","Indq_2010")
colnames(lista[[17]]) <- c("Total10", "Taxa10")
colnames(lista[[12]])[1] <- "PopResDPP"
colnames(lista[[13]]) <- paste0(colnames(lista[[13]]),"Indq")

#TABELAO - juntando os burros ----

tabelao <- lista[[1]]
for (i in 2:17)
  tabelao <- append(tabelao, lista[[i]])
tabelao<-as.data.frame(tabelao)

# TABELAO - Adicionando Regioes Geograficas ----
Regioes<- readxl::read_excel("dados/regioes_geograficas_composicao_por_municipios_2017_20180911.xls") |>
  subset(select=c(1,2,4,6), subset=CD_GEOCODI %in% tabelao$Codigo)
#convertendo codigo de municipio para numerico para nao ter erro na hora de ordenar
Regioes$CD_GEOCODI <- as.numeric(Regioes$CD_GEOCODI)
Regioes<- arrange(Regioes,CD_GEOCODI)

tabelao$RGI <- Regioes$nome_rgi
tabelao$RGint <- Regioes$nome_rgint

# TABELAO - Adicionando mesorregioes e microrregioes

#micromeso<- readxl::read_excel("dados/RG2017_micromeso89.xlsx") |>
#  subset(select=c(3,6,8)) |> arrange(cod_mun)
##ESSA DAQUI ESTA FALTANDO ALGUNS MUNICIPIOS...

#fonte da tabela: https://workmappro.com.br/importacao-pelo-codigo-do-municipio-do-ibge/
micromeso<- readxl::read_excel("dados/Tabela-de-Codigos-Geograficos.xlsx") |> 
  subset(select=c(8,10,13), subset= Cod_Munc %in% tabelao$Codigo) |> arrange(Cod_Munc)

tabelao$Mesorregiao <- micromeso$Nome_da_meso
tabelao$Microrregiao <- micromeso$Nome_da_micro

# TABELAO - EXPORTANDO
write.csv2(tabelao,"dados/TabelaBrasil.csv",row.names=F)

#TABELAO - apenas o RJ ----

tabelaoRJ<- tabelao |> subset(UF=="RJ") |> arrange(Codigo)

#microrregioes e macrorregioes
#micromeso <- readxl::read_excel("dados/Microrregioes_Mesorregioes-RJ.xlsx") |> arrange(Município)
#tabelaoRJ$Microrregiao <- micromeso$Microrregião
#tabelaoRJ$Mesorregiao <- micromeso$Mesorregião
#RGI e RGInt
#Regioes<- readxl::read_excel("dados/regioes_geograficas_composicao_por_municipios_2017_20180911.xls") |>
#  subset(subset=substr(CD_GEOCODI, start=1,stop=2)=="33",select=c(1,4,6)) |> arrange(nome_mun)
#tabelaoRJ$RGI <- Regioes$nome_rgi
#tabelaoRJ$RGInt <- Regioes$nome_rgint

# EXPORTANDO
write.csv2(tabelaoRJ,file='dados/RJ/TabelaoRioDeJaneiro.csv', row.names=F)

#### TABELA SELECIONADA ----

# tab1 - Populacao residente total, proporcoes de genero e situacao
# tab3 - Taxas de analfabetismo
# tab7 - Domicilios particulares permanentes, por tipo de saneamento (%)
# tab8 - Rendimento mensal totaldomiciliar per capita nominal
# tab9 - Rendimento mensal total nominal médio e mediano por sexo
# tab10 - Rendimento mensal total nominal médio por cor ou raça
# tab13 - Classes de rendimento mensal total domiciliar per capita nominal de
#         pessoas residentes em domicilios com saneamento inadequado
# tab 17 - Taxa de analfabetismo de crianças de 10 anos de idade

#municipios com codigo que começa com "33" são pertencentes ao estado do RJ :-)
RegioesRJ<- Regioes |> subset(subset=substr(CD_GEOCODI, start=1,stop=2)=="33")
micromesoRJ<- micromeso |> subset(subset=substr(Cod_Munc, start=1,stop=2)=="33")

tabRJ <- lista[[1]]
for(i in c(3,7:10,13,17))
  tabRJ <- append(tabRJ, lista[[i]])
tabRJ <- as.data.frame(tabRJ) |> subset(UF=="RJ") |> arrange(Codigo)

tabRJ$Microrregiao <- micromesoRJ$Nome_da_micro
tabRJ$Mesorregiao <- micromesoRJ$Nome_da_meso
tabRJ$RGI <- RegioesRJ$nome_rgi
tabRJ$RGInt <- RegioesRJ$nome_rgint

# EXPORTANDO
write.csv2(tabRJ,file='dados/RJ/TabRJ.csv', row.names=F)

