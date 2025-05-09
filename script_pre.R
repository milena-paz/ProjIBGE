		# --------------------------------------------------------- #
		#							    #
		#		Modelo dos Campos Semânticos (MCS)	    #
		#		Campo: Medidas Resumo			    #
		#							    #
		# by: Prof. Victor Ferreira Junqueira			    #
		# Período: Abril/2025					    #
		# Local: Paracambi, RJ					    #
		# --------------------------------------------------------- #

getwd()
rm(list=ls())
	# Importação dos Dados
dados <- read.csv2("dados/RJ/TabRJ.csv", header = TRUE)

	# Insoeção do conjunto de dados

head(dados)
tail(dados)
dim(dados)

is.data.frame(dados)

  # quantidade de NAs
sapply(dados, FUN = function(x) sum(is.na(x)))

  # nome das variaveis
names(dados)

nomes.pop <- c("PopTot", "Pop15", "Pop15.24", "Pop25.39")
populacao <- dados[, c(4, 11, 13, 15, 17, 19)]
is.data.frame(populacao)
pop.tab <- sapply(populacao, FUN = function(x) tapply(x, dados$Mesorregiao, FUN = sum))

#apply(pop.tab[, 2:5], 1, 

table(dados$Mesorregiao)

pop.obj <- boxplot(Pop60. ~ Mesorregiao, data = dados[-62, ])
unique(dados$Mesorregiao)
head(populacao)

total <-  aggregate(PopTotal ~ Mesorregiao, data = dados, FUN = sum)
atipico <- numeric(0)

for(i in pop.obj$out)  atipico<- append(atipico,which(dados$Pop60. == i))



		# Análise da Renda 
variaveis.renda <- c("ValMedio", "Q2", "Mediana", "Q3", "MediaHomens", 
				"MediaMulheres", "MedianaHomens", "MedianaMulheres", 
				"mediaH.M", "medianaH.M", "Ate70R.Indq", "Ate1.4Indq",
				"Ate1.2Indq", "Ate60.Indq")

renda <- dados[, variaveis.renda]
is.data.frame(renda)
renda.tab <- sapply(renda[, c(1, 3)], FUN = function(x) tapply(x, dados$Mesorregiao, FUN = mean))
renda.tab
cbind(pop.tab[, 1], renda.tab)
pop.tab$PopTot
is.data.frame(renda.tab)

round(renda.tab/510, 2)
#apply(pop.tab[, 2:5], 1, 
mean(dados$ValMedio)

table(dados$Mesorregiao)

renda.obj <- boxplot(ValMedio ~ Mesorregiao, data = dados[-62, ])
renda.obj$out
str(renda.obj)

fora <- numeric(6)
for(i in 1:6){
	fora[i] <- which(dados$ValMedio == renda.obj$out[i])
}

dados$Municipio[fora]
## [1] "Rio das Ostras" "Silva Jardim"   "Nova Friburgo"  "Niterói"       
#[5] "Rio de Janeiro" "Macaé"

#"Q2", "Mediana", "Q3",

renda$Mediana[fora]

plot(renda$ValMedio[fora], ylim = c(200, 2500), xlim = c(0.5, 6.5))
points(1:6, renda$Mediana[fora], pch = 3, col = "red", lwd = 2)
text(1:6, renda$Q2[fora], label = "Q1", cex = 0.85, col = "blue", font =2)
text(1:6, renda$Q3[fora], label = "Q3", cex = 0.85, col = "blue", font =2)
text(1:6, 2300, label = out.abb, cex = 0.85, pos = 1)

rect(xleft = (1:6) - 0.25, ybottom = renda$Q2[fora], 
	xright = (1:6) + 0.25, ytop = renda$Q3[fora])

table(dados$Mesorregiao)



head(dados)
pdf("fig.final.pdf")

  # boxplot com pontos
par(mar = c(3, 3, 1, 1))
boxplot(ValMedio ~ Mesorregiao, data = dados[-62, ], border = "grey", 
		col = "light grey", boxwex = 0.5)

meso.num <- as.numeric(as.factor(dados$Mesorregiao))

  # pontos dos dados
with(dados,points(jitter(meso.num, 0.5),
	unlist(ValMedio, Mesorregiao), cex = 0.5, pch = 16)
)

#dev.off()
out.abb <- c("R.Ostras", "S.Jardim", "Friburgo", "Niterói", "R.Janeiro", "Macaé")

text(renda.obj$group, renda.obj$out, label = out.abb, cex = 0.85, pos = 1)