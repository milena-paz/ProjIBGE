tabela <- read.csv2("dados/RJ/TabRJ.csv")
niveis <-
  c("Acima de 500 mil", "De 10 a 20 mil", "De 100 a 500 mil", "De 20 a 50 mil", 
    "De 5 a 10 mil", "De 50 a 100 mil")
tabela$Classe <- factor(tabela$Classe,levels=niveis[c(5,2,4,6,3,1)])
tabela$Microrregiao <-as.factor(tabela$Microrregiao)
tabela$Mesorregiao <-as.factor(tabela$Mesorregiao)
levels(tabela$Mesorregiao) <- c('Baixadas', 'Centro', 'Metropolitana', 'Noroeste', 'Norte', 'Sul')
###

op <- par(no.readonly=T)
par(mar = c(4.1, 4.1, 1, 1))
bxp<-boxplot(ValMedio ~ Mesorregiao, data=tabela, ylim= c(338,1700),col="gray80",
        ylab="Renda Média(R$)",xlab="Mesorregião",outline=F,border='grey45')
stripchart(ValMedio ~ Mesorregiao,data=tabela,add=T,pch=19,
           cex=.6,vertical=T,method="jitter")
#title(main="Boxplot da renda média domiciliar per capita\ndos municípios fluminenses, por mesorregião")
fora <- numeric(7)
for(i in 1:7) fora[i] <- which(tabela$ValMedio == bxp$out[i])

text(x= c(1,1,2,3,3,5,5),y=tabela$ValMedio[fora],labels=tabela$Municipio[fora],
     cex= .75,pos=c(3,3,3,1,3,3,3))
text(x=0.65,y=c(700,649,600),labels=c("Q3","Mediana","Q1"),col="red",pos=2,cex=0.7)

#Selecionando Rio das ostras,Nova Friburgo, Rio de Janeiro, Niterói e Macaé:
sel <- fora[c(1,3,4,5,6)]
IIQ <- tabela$Q3[sel] - tabela$Q2[sel]
maxi<-tabela$Mediana[sel]+IIQ*1.5
lar <- 0.2
#BOXPLOTS MANUAIS
par(mar = c(3, 4.1, 1, 1))
plot(0, ylim = c(0, 3500), xlim = c(0.5, 5.5),type="n",xaxt="n",
     ylab="Renda Domiciliar per capita(R$)",xlab="")
rect(xleft=1:5-lar,ybottom=tabela$Q2[sel],xright=1:5+lar,ytop=tabela$Q3[sel],
     col="gray80",border="grey45")
segments(1:5-lar,tabela$Mediana[sel],1:5+lar,col="grey45",lwd=3,lend=1)
arrows(1:5,y0=tabela$Q3[sel],y1=maxi,col="grey45",angle=90)
arrows(1:5,y0=tabela$Q2[sel],y1=0,col="grey45",angle=90)
axis(1,labels = tabela$Municipio[sel],at=1:5)
text(x=2.8,y=c(tabela$Q3[sel[3]],tabela$Mediana[sel[3]],tabela$Q2[sel[3]]),
     labels=c("Q3","Mediana","Q1"),col="red",pos=2,cex=0.7)
#text()

###

bxp<-boxplot(list(Homens=tabela$MediaHomens, Mulheres=tabela$MediaMulheres),
             pch=19,ylab="Renda Média(R$)",xlab="",col="grey80",border="gray45") #c("#9B89C9","#C98C9E")
fora <- numeric(4)
for(i in 1:4) fora[i] <- which(tabela$MediaHomens == bxp$out[i])
fora <- append(fora,which(tabela$MediaMulheres == bxp$out[5]))
fora <- append(fora,which(tabela$MediaMulheres == bxp$out[6]))
text(x= 1,y=tabela$MediaHomens[fora[1:4]],labels=tabela$Municipio[fora[1:4]],
     cex= .75,pos=c(3,1,1,1))
text(x= 2,y=tabela$MediaMulheres[fora[5:6]],labels=tabela$Municipio[fora[5:6]],
     cex= .75,pos=1)

boxplot(list(Homens=tabela$MedianaHomens, Mulheres=tabela$MedianaMulheres),
             pch=19,ylab="Renda Mediana(R$)",xlab="",col=c("#9B89C9","#C98C9E"),border="gray5")

# boxplot classe

bxp<- boxplot(Inadequado ~ Classe,data=tabela,xlab="Número de habitantes",pch=19,
        ylab="",col="grey80",border="gray45")
title(ylab="Domicílios com saneamento inadequado (%)",cex.lab=1)
fora <- numeric(2)
for(i in 1:2) fora[i] <- which(tabela$Inadequado == bxp$out[i])
text(x=3, y=bxp$out, labels=tabela$Municipio[fora],cex= .75,pos=c(1,3))