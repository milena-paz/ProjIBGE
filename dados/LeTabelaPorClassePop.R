#INFORMACAO GLOBAL
nlinhas<-1301
nlinhas[2]<-1212
nlinhas[3]<-3927-2526
nlinhas[4]<-4971-3928
nlinhas[5]<-5297-4972
nlinhas[6]<-5543-5298
nlinhas[7]<-5582-5544
classes<-c("Até 5 mil","De 5 a 10 mil","De 10 a 20 mil","De 20 a 50 mil",
           "De 50 a 100 mil","De 100 a 500 mil","Acima de 500 mil")
colNamesGlobais <- c("Codigo","UF","Municipio")

#FUNCAO DE LER TABELA
leTabela<-function(offset,n,uCol,sheet=1){
  tab<-list()
  ranges<-numeric(0)
  for(i in 1:7){
    ranges[i]<-paste0("A",offset,":",LETTERS[uCol],offset+nlinhas[i]-1)
    tab[[i]]<-readxl::read_xls(paste0("dados/Brasil/tab",n,".xls"),range=ranges[i],
                               na=c("","-"),col_names = F,sheet=sheet)
    offset<-offset+nlinhas[i]+1
    tab[[i]][1:nrow(tab[[i]]), uCol+1]<- as.factor(classes[i])
  }
  return(tab)
}

organizaTabela <- function(offset,n,uCol,sheet){
  #-------------------------------------------------------#
  #offset=onde comecam os dados na tabela
  #n=id da tabela
  #uCol= numero de colunas da tabela, ou posicao da ultima
  #-------------------------------------------------------#
  t<-leTabela(offset,n,uCol,sheet)
  Tab <- NULL
  for(i in 1:7) Tab<-rbind(Tab,t[[i]])
  return(as.data.frame(Tab))
}

#TABELAS 8 A 13
TodasTabs<-list(Tab8= organizaTabela(12,8,7),Tab9= organizaTabela(11,9,9),
                Tab10= organizaTabela(9,10,8),Tab11= organizaTabela(9,11,8),
                Tab12= organizaTabela(11,12,8),Tab13= organizaTabela(11,13,8))

for(i in 1:6){
  colnames(TodasTabs[[i]])[1:3] <- colNamesGlobais
  TodasTabs[[i]]$UF <- as.factor(TodasTabs[[i]]$UF)
}
#NOMES DE COLUNAS
colnames(TodasTabs[[1]])[4:8]<- c("Media","Q1","Mediana","Q3","Classe")
colnames(TodasTabs[[2]])[4:10]<- c("Media.H","Media.M","Mediana.H",
                                  "Mediana.M", "Media.H/M","Mediana.H/M",
                                  "Classe")
colnames(TodasTabs[[3]])[4:9] <- c("Branca","Preta","Parda","Amarela",
                                   "Indígena","Classe")
colnames(TodasTabs[[4]])[4:9] <- c("Br/Pr","Br/Pa","Br/Am","Br/In","Pr/Pa",
                                   "Classe")
colnames(TodasTabs[[5]])[4:9] <- c("Total","Ate70Reais","Ate1/4","Ate1/2",
                                   "Ate170reais","Classe")
colnames(TodasTabs[[6]])[4:9] <- c("Total","Ate70Reais","Ate1/4","Ate1/2",
                                   "Ate225reais","Classe")
######

tab1<- organizaTabela(10,1,9)
colnames(tab1) <- c(colNamesGlobais, "PopTotal", "Urbana","Rural", "Homem","Mulher","RazaoSexo","Classe")
write.csv(tab1, row.names=F, file="dados/Brasil(utilizavel)/Tab1.csv")

tab<- organizaTabela(11,2,10,sheet=2)