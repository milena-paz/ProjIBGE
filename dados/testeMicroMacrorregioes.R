#c<- readxl::read_excel("dados/regioes_geograficas_composicao_por_municipios_2017_20180911.xls")
#c<-subset(c,substr(c$CD_GEOCODI, start=1,stop=2)=="33",select=c(1,4,6))
c<- readxl::read_excel("Microrregioes_Mesorregioes-RJ.xlsx")
tab<- read.csv("dados/limpos/Tabs/Tab8.csv")
tab<- subset(tab, UF=="RJ")

library(dplyr)
c<- arrange(c,Município)
tab <- arrange(tab, Municipio)
all.equal(tab$Municipio,c$Município)
tab$Microrregiao <- c$Microrregião
tab$Mesorregiao <- c$Mesorregião

boxplot(Media ~ Mesorregiao,data=tab)