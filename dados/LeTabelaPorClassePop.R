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

nomes<- read.csv("dados/Brasil(utilizavel)/vars.csv",sep=";",na="")

of<- c(10,11,14,12,12,10,10,10,12,12,11,9,9,11,11,11,11,11,10)
colu<-c(9,10,13,12,15,10,9,9,7,7,9,8,8,8,8,11,9,9,5)

#FUNCAO DE LER TABELA
leTabela<-function(offset,n,uCol,sheet=1){
  tab<-list()
  ranges<-numeric(0)
  for(i in 1:7){
    ranges[i]<-paste0("A",offset,":",LETTERS[uCol],offset+nlinhas[i]-1)
    tab[[i]]<-readxl::read_xls(paste0("dados/Brasil/tab",n,".xls"),range=ranges[i],
                               na=c("","-","."),col_names = F,sheet=sheet)
    offset<-offset+nlinhas[i]+1
    tab[[i]][1:nrow(tab[[i]]), uCol+1]<- as.factor(classes[i])
  }
  return(tab)
}

organizaTabela <- function(offset,n,uCol,sheet=1){
  #-------------------------------------------------------#
  #offset=onde comecam os dados na tabela
  #n=id da tabela
  #uCol= numero de colunas da tabela, ou posicao da ultima
  #sheet= qual subtabela (argumento de leTabela)
  #-------------------------------------------------------#
  t<-leTabela(offset,n,uCol,sheet)
  Tab <- NULL
  for(i in 1:7) Tab<-rbind(Tab,t[[i]])
  return(as.data.frame(Tab))
}


#### TABELAS 1 a 5 ----
TodasTabs <- list()
for(i in 1:5){
  TodasTabs[[paste0("Tab",i)]] <- organizaTabela(of[i],i,colu[i])
}
#NOMES DAS VARS
for(i in 1:5){
  nom <- nomes[!is.na(nomes[,i]),i]
  colnames(TodasTabs[[paste0("Tab",i)]])<- c(nom,"Classe")
}

#### TABELA 6 (CASO ESPECIAL) ----
Tab6<-  list(A=organizaTabela(10,6,10,1),B=organizaTabela(10,6,9,2),C=organizaTabela(10,6,9,3))
for(i in 6:8){
  nom <- nomes[!is.na(nomes[,i]),i]
  colnames(Tab6[[i-5]])<- c(nom,"Classe")
}
Tab6$B <- subset(Tab6$B,select=-c(1,2,3,10))
Tab6$C <- subset(Tab6$C,select=-c(1,2,3,10))
TodasTabs$Tab6 <- with(Tab6,cbind(A,B,C))

#### TABELAS 8 A 18 ----
for(i in 7:17){
  TodasTabs[[paste0("Tab",i)]] <- organizaTabela(of[i+2],i,colu[i+2])
}

for(i in 7:17){
  nom <- unlist(nomes[!is.na(nomes[,i+2]),i+2])
  colnames(TodasTabs[[paste0("Tab",i)]])<- c(nom,"Classe")
}
#### EXPORTANDO TABELAS ----
for(i in 1:17) write.csv2(TodasTabs[[i]],file=paste0("dados/Brasil(utilizavel)/tab",i,".csv"),row.names=F)